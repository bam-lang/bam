///! BAM! virtual machine.
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    io::{stdin, stdout, Write},
    rc::Rc,
};

use crate::{
    compiler::{FlatStream, Handle, Instr, Module, Unit},
    Builtin, Value,
};

use anyhow::{anyhow, bail, Result};
use tracing::trace;

/// Infinite value stream.
pub struct Loop {
    env: SharedEnv,
    fstream: FlatStream,
    factory: Factory,
}

impl Loop {
    /// Steps through the stream.
    pub fn step(&self) -> Result<Value> {
        self.factory
            .clone()
            .compile(self.fstream.clone())
            .next(&self.env)
    }
}

macro_rules! flow {
    ($env:ident, $body:expr) => {{
        #[allow(unused_imports)]
        use Value::*;
        SharedFlow::new(move |$env: SharedEnv| $body)
    }};
}

// SAFETY: if we can prove that BAM!'s semantics and/or
// the structure of the evaluator follow Rust's aliasing
// xor mutability principle, we can drop the `RefCell`s
// in favour of `UnsafeCell`.

/// Flow environment.
pub struct Env {
    outer: Option<SharedEnv>,
    // TODO: replace this with a vector.
    flows: HashMap<Handle, SharedFlow>,
}

/// Shared stream environment.
#[derive(Clone)]
pub struct SharedEnv(Rc<RefCell<Env>>);

impl SharedEnv {
    /// Creates an empty `SharedEnv` with no outer layer.
    pub fn new() -> Self {
        let env = Env {
            outer: None,
            flows: HashMap::new(),
        };
        Self(Rc::new(RefCell::new(env)))
    }

    /// Creates an empty `SharedEnv` with that shadows `outer`.
    pub fn shadow(outer: Self) -> Self {
        let env = Env {
            outer: Some(outer),
            flows: HashMap::new(),
        };
        Self(Rc::new(RefCell::new(env)))
    }

    /// Resolves a `Handle` in the environment.
    pub fn get(&self, handle: Handle) -> Result<SharedFlow> {
        let env = &*self.0.borrow();

        match env.flows.get(&handle) {
            Some(sf) => Ok(sf.clone()),
            None => match &env.outer {
                Some(e) => e.get(handle),
                None => bail!("Undefined stream handle {:?}", handle),
            },
        }
    }

    /// Resolves a `Handle` in the first environment.
    pub fn get_from_head(&self, handle: Handle) -> Option<SharedFlow> {
        let env = &*self.0.borrow();
        env.flows.get(&handle).cloned()
    }

    /// Resolves a `Handle` in the first environment.
    pub fn get_from_outer(&self, handle: Handle) -> Option<SharedFlow> {
        let env = &*self.0.borrow();
        match &env.outer {
            Some(e) => {
                let env = &*e.0.borrow();
                env.flows.get(&handle).cloned()
            }
            None => None,
        }
    }

    /// Inserts a `SharedEnv` at the specified `Handle`.
    pub fn set(&self, handle: Handle, sflow: SharedFlow) {
        let env = &mut *self.0.borrow_mut();
        env.flows.insert(handle, sflow);
    }
}

/// Compiled `FlatStream`.
pub trait Flow: FnMut(SharedEnv) -> Result<Value> + 'static {}

impl<F> Flow for F where F: FnMut(SharedEnv) -> Result<Value> + 'static {}

/// A reference-counted, mutable flow in memory.
#[derive(Clone)]
pub struct SharedFlow {
    flow: Rc<RefCell<dyn Flow>>,
    cache: Rc<Cell<Value>>,
}

impl SharedFlow {
    /// Creates a new `SharedFlow` from a `Closure`.
    pub fn new(inner: impl Flow) -> Self {
        Self {
            flow: Rc::new(RefCell::new(inner)),
            cache: Rc::new(Cell::new(Value::Null)),
        }
    }

    /// Consume a `Value` from this `SharedFlow`.
    pub fn next(&self, env: &SharedEnv) -> Result<Value> {
        let flow = &mut *self.flow.borrow_mut();

        match self.cache.take() {
            Value::Null => flow(env.clone()),
            other => Ok(other),
        }
    }

    /// Consume a `Value` from this `SharedFlow` whilst ignoring the cache.
    pub fn peek(&self, env: &SharedEnv) -> Result<Value> {
        let flow = &mut *self.flow.borrow_mut();

        let val = match self.cache.take() {
            Value::Null => flow(env.clone()),
            other => Ok(other),
        }?;

        self.cache.set(val.clone());
        Ok(val)
    }
}

/// BAM!'s execution engine.
#[derive(Clone)]
pub struct Factory {
    module: Rc<Module>,
    // TODO: this is the trickiest bit and needs documentation.
    offset: Cell<usize>,
}

impl Factory {
    /// Creates a `Factory` from a `Module`.
    pub fn new(module: Module) -> Self {
        Self {
            module: Rc::new(module),
            offset: Cell::new(0),
        }
    }

    /// Starts the factory in the Main unit.
    pub fn make_loop(&self, name: &str) -> Loop {
        let env = SharedEnv::new();
        let input = self.clone().compile(FlatStream::Stdin);
        env.set(Handle::input(), input);
        let fstream = FlatStream::Pipe(Handle::input(), String::from(name));

        Loop {
            env,
            fstream,
            factory: self.clone(),
        }
    }

    /// Resolves a unit from the factory.
    pub fn unit(&self, name: &str) -> Result<Unit> {
        self.module
            .0
            .get(name)
            .cloned()
            .ok_or(anyhow!("Undefined fantastic machine {name}"))
    }

    /// Creates a `Factory` with a new offset.
    fn bump(&self, amount: usize) {
        let offset = self.offset.get() + amount;
        self.offset.replace(offset);
    }

    /// Transform a `FlatStream` into a `SharedFlow`.
    pub fn compile(self, fstream: FlatStream) -> SharedFlow {
        // FIXME: this cache thing everywhere, macro?
        trace!("[EVAL] factory/compile {fstream}");
        match fstream {
            FlatStream::Stdin => {
                flow!(_env, {
                    // FIXME: decide on `stdin` vs `null -> Read`
                    let mut buf = String::new();
                    stdin().read_line(&mut buf)?;
                    Ok(Str(buf))
                })
            }
            FlatStream::Repeat(val) => {
                flow!(_env, Ok(val.clone()))
            }
            FlatStream::Zip(hdls) => {
                flow!(
                    env,
                    Ok(Tuple(
                        hdls.clone()
                            .into_iter()
                            .map(|hdl| env.get(hdl).and_then(|sflow| sflow.next(&env)))
                            .collect::<Result<_, _>>()?,
                    ))
                )
            }
            FlatStream::Copy(hdl) => {
                flow!(env, env.get(hdl)?.next(&env))
            }
            FlatStream::Take(hdl, rest) => {
                let mut state = Value::Num(rest as f64);
                flow!(env, {
                    let sflow = env.get(hdl)?;
                    let current = state.clone().to_num();

                    if current == 0f64 {
                        Ok(Null)
                    } else {
                        state = Num(current - 1f64);
                        sflow.next(&env)
                    }
                })
            }
            FlatStream::Peek(hdl) => {
                flow!(env, env.get(hdl)?.peek(&env))
            }
            FlatStream::Pipe(hdl, name) => flow!(env, {
                let unit = self.unit(&name)?.instantiate(self.offset.get());
                self.bump(unit.allocs() + 1);
                let input = env.get(hdl)?;
                let env = SharedEnv::shadow(env);
                env.set(unit.input(), input);

                for instr in unit.unpack() {
                    match instr {
                        Instr::Make(handle, fstream) => {
                            let sflow = self.clone().compile(fstream);
                            env.set(handle, sflow);
                        }
                        Instr::Next(handle) => {
                            let sflow = env.get(handle)?;
                            sflow.next(&env)?;
                        }
                        Instr::Exit(handle) => {
                            let sflow = env.get(handle)?;
                            return sflow.next(&env);
                        }
                    }
                }

                bail!("FatalError: a unit was missing an Exit instr.")
            }),
            FlatStream::Exec(hdl, builtin) => {
                flow!(env, {
                    let input = env.get(hdl)?;
                    let val = input.next(&env)?;
                    Self::exec(val, builtin.clone())
                })
            }
            FlatStream::Proj(hdl, idx) => {
                flow!(env, {
                    let input = env.get(hdl)?;
                    let val = input.next(&env)?;
                    if let Value::Tuple(values) = val {
                        match values.get(idx) {
                            Some(val) => Ok(val.clone()),
                            None => bail!("Bad index in unzip: {}", idx),
                        }
                    } else {
                        bail!("Proj on non-tuple value.")
                    }
                })
            }
            FlatStream::Cond(cond, then, otherwise) => {
                flow!(env, {
                    let cond = env.get(cond)?;
                    let val = cond.next(&env)?;
                    if let Value::Bool(cond) = val {
                        if cond {
                            let then = env.get(then)?;
                            then.next(&env)
                        } else {
                            let then = env.get(otherwise)?;
                            then.next(&env)
                        }
                    } else {
                        bail!("Non-bool value in conditional")
                    }
                })
            }
        }
    }

    /// Execute a builtin machine.
    pub fn exec(value: Value, builtin: Builtin) -> Result<Value> {
        match builtin {
            Builtin::Add => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_num();
                let rhs = rhs.to_num();
                Ok(Value::Num(lhs + rhs))
            }
            Builtin::Sub => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_num();
                let rhs = rhs.to_num();
                Ok(Value::Num(lhs - rhs))
            }
            Builtin::Mul => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_num();
                let rhs = rhs.to_num();
                Ok(Value::Num(lhs * rhs))
            }
            Builtin::Div => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_num();
                let rhs = rhs.to_num();
                if lhs == 0_f64 {
                    bail!("Division by zero")
                } else {
                    Ok(Value::Num(lhs / rhs))
                }
            }
            Builtin::Mod => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_num();
                let rhs = rhs.to_num();
                Ok(Value::Num(lhs % rhs))
            }
            Builtin::Pow => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_num();
                let rhs = rhs.to_num();
                Ok(Value::Num(lhs.powf(rhs)))
            }
            Builtin::Sqrt => {
                let num = value.to_num();
                Ok(Value::Num(f64::sqrt(num)))
            }
            Builtin::Gt => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_num();
                let rhs = rhs.to_num();
                Ok(Value::Bool(lhs > rhs))
            }
            Builtin::Lt => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_num();
                let rhs = rhs.to_num();
                Ok(Value::Bool(lhs < rhs))
            }
            Builtin::Eq => {
                let (lhs, rhs) = value.to_pair();
                Ok(Value::Bool(lhs == rhs))
            }
            Builtin::And => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_bool();
                let rhs = rhs.to_bool();
                Ok(Value::Bool(lhs && rhs))
            }
            Builtin::Or => {
                let (lhs, rhs) = value.to_pair();
                let lhs = lhs.to_bool();
                let rhs = rhs.to_bool();
                Ok(Value::Bool(lhs || rhs))
            }
            Builtin::Not => {
                let inv = !value.to_bool();
                Ok(Value::Bool(inv))
            }
            Builtin::Dup2 => {
                let pair = vec![value.clone(), value];
                Ok(Value::Tuple(pair))
            }
            Builtin::Dup3 => {
                let triple = vec![value.clone(), value.clone(), value];
                Ok(Value::Tuple(triple))
            }
            Builtin::Write => {
                write!(stdout(), "{}", &value)?;
                Ok(value)
            }
            Builtin::Read => {
                let mut buf = String::new();
                stdin().read_line(&mut buf)?;
                Ok(Value::Str(buf))
            }
        }
    }
}
