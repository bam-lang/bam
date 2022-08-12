//! BAM! bytecode compiler.

use std::collections::HashMap;
use std::fmt;

use crate::{syntax::Value, Builtin, Definition, Machine, Program, Statement, Stream};

/// Local handle to a [`Flow`].
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Handle(usize);

impl Handle {
    /// Get flow index.
    pub fn ix(self) -> usize {
        self.0
    }

    /// Input handle.
    pub fn input() -> Self {
        Self(0)
    }

    /// Offset the handle by some amount.
    pub fn add(mut self, offset: usize) -> Self {
        self.0 += offset;
        self
    }
}

impl fmt::Display for Handle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.ix())
    }
}

/// A stream without nesting and user-variables.
#[derive(Debug, Clone)]
pub enum FlatStream {
    /// Standard input.
    Stdin,
    /// Integer range.
    Integers,
    /// Always return the same value. Boring!
    Repeat(Value),
    /// Stream of tuples.
    Zip(Vec<Handle>),
    /// Just a handle.
    Copy(Handle),
    /// Consume from the underlying stream a limited number of times.
    Take(Handle, usize),
    /// Consume from the underlying stream without changing it.
    Peek(Handle),
    /// Run a unit with a stream.
    Pipe(Handle, String),
    /// Builtin operations.
    Exec(Handle, Builtin),
    /// Projection into a tuple value.
    Proj(Handle, usize), // Value -> Value
    /// Conditional stream.
    Cond(Handle, Handle, Handle),
}

impl fmt::Display for FlatStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatStream::Stdin => write!(f, "stdin"),
            FlatStream::Integers => write!(f, "integers"),
            FlatStream::Repeat(val) => write!(f, "{val}"),
            FlatStream::Zip(hdls) => write!(
                f,
                "({})",
                hdls.into_iter()
                    .map(|hdl| format!("{hdl}"))
                    .reduce(|acc, s| (acc + ", " + s.as_str()))
                    .unwrap_or_else(|| "".to_string())
            ),
            FlatStream::Copy(hdl) => write!(f, "{hdl}"),
            FlatStream::Take(hdl, rst) => write!(f, "{hdl}{{{rst}}}"),
            FlatStream::Peek(hdl) => write!(f, "!{hdl}"),
            FlatStream::Pipe(hdl, name) => write!(f, "{hdl} -> {name}"),
            FlatStream::Exec(hdl, builtin) => write!(f, "{hdl} -> {builtin:?}"),
            FlatStream::Proj(hdl, idx) => write!(f, "{hdl}[{idx}]"),
            FlatStream::Cond(hdl0, hdl1, hdl2) => write!(f, "{hdl0} ? {hdl1} : {hdl2}"),
        }
    }
}

impl FlatStream {
    /// Creates a new unique version of the flat stream by offsetting all the handles.
    pub fn instantiate(&self, offset: usize) -> Self {
        match self {
            FlatStream::Stdin => FlatStream::Stdin,
            FlatStream::Integers => FlatStream::Integers,
            FlatStream::Repeat(val) => FlatStream::Repeat(val.clone()),
            FlatStream::Zip(hdls) => {
                FlatStream::Zip(hdls.iter().cloned().map(|hdl| hdl.add(offset)).collect())
            }
            FlatStream::Copy(hdl) => FlatStream::Copy(hdl.add(offset)),
            FlatStream::Take(hdl, count) => FlatStream::Take(hdl.add(offset), *count),
            FlatStream::Peek(hdl) => FlatStream::Peek(hdl.add(offset)),
            FlatStream::Pipe(hdl, name) => FlatStream::Pipe(hdl.add(offset), name.clone()),
            FlatStream::Exec(hdl, builtin) => FlatStream::Exec(hdl.add(offset), builtin.clone()),
            FlatStream::Proj(hdl, index) => FlatStream::Proj(hdl.add(offset), *index),
            FlatStream::Cond(cond, then, otherwise) => {
                FlatStream::Cond(cond.add(offset), then.add(offset), otherwise.add(offset))
            }
        }
    }
}

/// Compiled version of a machine.
#[derive(Debug, Clone)]
pub struct Unit {
    input: Handle,
    instrs: Vec<Instr>,
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}: {{", self.input)?;
        for instr in &self.instrs {
            writeln!(f, "  {instr};")?;
        }
        writeln!(f, "}}")
    }
}

impl Unit {
    /// Creates a new unique version of the unit by offsetting all the handles.
    pub fn instantiate(&self, offset: usize) -> Self {
        Self {
            input: self.input.add(offset),
            instrs: self
                .instrs
                .iter()
                .map(|instr| instr.instantiate(offset))
                .collect(),
        }
    }

    /// Counts the number of created handles.
    pub fn allocs(&self) -> usize {
        self.instrs
            .iter()
            .filter(|instr| matches!(instr, Instr::Make(_, _)))
            .count()
    }
}

impl Unit {
    /// Creates an empty `Unit`.
    pub fn new(input: Handle) -> Self {
        Self {
            input,
            instrs: Vec::new(),
        }
    }

    /// Unpacks the `Unit` into instructions.
    pub fn unpack(self) -> Vec<Instr> {
        // TODO: replace with an Iterator impl?
        self.instrs
    }

    /// Returns the input handle.
    pub fn input(&self) -> Handle {
        self.input
    }
}

/// Machine instructions.
#[derive(Debug, Clone)]
pub enum Instr {
    /// Create a shared stream in the current task
    /// and bind it to the given symbol.
    Make(Handle, FlatStream), // () -> Stream
    /// Consume a value from this stream and drop it (?).
    Next(Handle), // Stream -> Value
    /// Consume a value from this stream and return it.
    Exit(Handle),
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Make(hdl, stream) => write!(f, "{hdl} = {stream}"),
            Instr::Next(hdl) => write!(f, "next {hdl}"),
            Instr::Exit(hdl) => write!(f, "exit {hdl}"),
        }
    }
}

impl Instr {
    /// Creates a new unique version of the instruction by offsetting all the handles
    pub fn instantiate(&self, offset: usize) -> Self {
        match self {
            Instr::Make(hdl, fs) => Instr::Make(hdl.add(offset), fs.instantiate(offset)),
            Instr::Next(hdl) => Instr::Next(hdl.add(offset)),
            Instr::Exit(hdl) => Instr::Exit(hdl.add(offset)),
        }
    }
}

/// Renaming utility.
/// Counting starts at zero.
#[derive(Debug)]
pub struct Renamer {
    map: HashMap<String, u32>,
    count: u32,
}

impl Renamer {
    /// Create a renamer.
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            count: 0,
        }
    }

    /// Bind a name in the renamer.
    fn bind(&mut self, name: String) -> u32 {
        match self.map.get(&name).cloned() {
            None => {
                let sym = self.fresh();
                self.map.insert(name, sym);
                sym
            }
            Some(sym) => sym,
        }
    }

    /// Get a fresh new symbol.
    fn fresh(&mut self) -> u32 {
        let sym = self.count;
        self.count += 1;
        sym
    }

    /// Resets the internal conter.
    pub fn reset(&mut self) {
        self.count = 0;
        self.map.clear();
    }
}

/// Resulting module.
#[derive(Debug)]
pub struct Module(pub HashMap<String, Unit>);

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, unit) in &self.0 {
            writeln!(f, "{name} = {unit}")?;
        }

        Ok(())
    }
}

/// Compiler context.
#[derive(Debug)]
pub struct Compiler {
    /// Collected units, correspond to machines.
    /// Alongside their respective name contexts.
    units: HashMap<String, Unit>,
    /// Currently compiled unit.
    current: String,
    /// Global handle renamer.
    handles: Renamer,
}

impl Compiler {
    /// Create and run the compiler.
    pub fn new() -> Self {
        Compiler {
            units: HashMap::new(),
            current: String::new(),
            handles: Renamer::new(),
        }
    }

    /// Transform a syntax tree into a mostly flat representation.
    pub fn transform(mut self, program: Program) -> Module {
        for machine in program.machines {
            self.transform_machine(machine);
            self.handles.reset();
        }

        Module(self.units)
    }

    /// Transform a machine into a unit.
    pub fn transform_machine(&mut self, machine: Definition) {
        // INVARIANT: the unit's symbol is the same as its index in `units`.
        let unit = Unit::new(self.fresh_handle());
        self.units.insert(machine.name.clone(), unit);
        self.current = machine.name;

        for stmt in machine.body {
            self.transform_statement(stmt);
        }

        let handle = self.transform_stream(machine.result);
        self.add_instr(Instr::Exit(handle))
    }

    /// Transform a stream to a flow, pipes get
    pub fn transform_statement(&mut self, stmt: Statement) {
        match stmt {
            // let x, y = stream;
            Statement::Let(mut names, stream) => {
                if names.len() == 1 {
                    let name = names.pop().unwrap();
                    self.transform_stream_with(stream, Some(name));
                } else {
                    let tuple = self.transform_stream(stream);
                    for (index, name) in names.into_iter().enumerate() {
                        let handle = self.bind_flow(name);
                        self.add_instr(Instr::Make(handle, FlatStream::Proj(tuple, index)));
                    }
                }
            }
            Statement::Consume(stream) => {
                let handle = self.transform_stream(stream);
                self.add_instr(Instr::Next(handle))
            }
        }
    }

    /// Transform a stream to a flow and give it the supplied handle.
    pub fn transform_stream_with(&mut self, stream: Stream, name: Option<String>) -> Handle {
        let handle = match name {
            Some(name) => self.bind_flow(name),
            None => self.fresh_handle(),
        };

        match stream {
            Stream::Var(var) => match var.as_str() {
                "integers" => {
                    self.add_instr(Instr::Make(handle, FlatStream::Integers));
                }
                "it" => {
                    return self.current().input;
                }
                _ => {
                    return self.bind_flow(var.clone());
                }
            },
            Stream::Const(val) => {
                self.add_instr(Instr::Make(handle, FlatStream::Repeat(val)));
            }
            Stream::Pipe(stream, machine) => {
                let input = self.transform_stream(*stream);
                let flow = match *machine {
                    Machine::Var(var) => FlatStream::Pipe(input, var.clone()),
                    Machine::Builtin(builtin) => FlatStream::Exec(input, builtin),
                };

                self.add_instr(Instr::Make(handle, flow));
            }
            Stream::Zip(streams) => {
                let zipped = streams
                    .into_iter()
                    .map(|s| self.transform_stream(s))
                    .collect::<Vec<_>>();

                self.add_instr(Instr::Make(handle, FlatStream::Zip(zipped)));
            }
            Stream::Cond(cond, then, otherwise) => {
                let cond = self.transform_stream(*cond);
                let then = self.transform_stream(*then);
                let otherwise = self.transform_stream(*otherwise);

                self.add_instr(Instr::Make(handle, FlatStream::Cond(cond, then, otherwise)))
            }
            Stream::Take(stream, count) => {
                let inner = self.transform_stream(*stream);

                self.add_instr(Instr::Make(handle, FlatStream::Take(inner, count)))
            }
            Stream::Peek(stream) => {
                let inner = self.transform_stream(*stream);

                self.add_instr(Instr::Make(handle, FlatStream::Peek(inner)))
            }
        };

        handle
    }

    /// Helper function for [`transform_stream_with`].
    pub fn transform_stream(&mut self, stream: Stream) -> Handle {
        self.transform_stream_with(stream, None)
    }

    /// Add an instruction in the currently compiled unit.
    pub fn add_instr(&mut self, instr: Instr) {
        // TODO(fuzzypixelz): handle errors.
        let unit = self.current.clone();
        self.units
            .get_mut(&unit)
            .expect("no units in `add_instr`")
            .instrs
            .push(instr)
    }

    /// Bind a name in the currently compiled unit.
    pub fn bind_flow(&mut self, name: String) -> Handle {
        Handle(self.handles.bind(name) as usize)
    }

    /// Get a fresh handle in the currently compiled unit.
    pub fn fresh_handle(&mut self) -> Handle {
        Handle(self.handles.fresh() as usize)
    }

    /// Returns the currently compiled unit.
    pub fn current(&self) -> &Unit {
        self.units.get(&self.current).unwrap()
    }

    /// Returns the currently compiled unit as mutable.
    pub fn current_mut(&mut self) -> &Unit {
        self.units.get_mut(&self.current).unwrap()
    }
}
