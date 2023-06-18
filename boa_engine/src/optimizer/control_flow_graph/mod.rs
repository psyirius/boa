//! TODO: doc

#![allow(dead_code)]
#![allow(missing_debug_implementations)]

mod basic_block;
mod instruction_iterator;

use std::{fmt::Debug, rc::Rc};

use indexmap::IndexSet;
use rustc_hash::FxHashMap;

use crate::vm::Opcode;

pub use self::basic_block::{BasicBlock, RcBasicBlock, WeakBasicBlock};
use self::{basic_block::BasicBlockFlags, instruction_iterator::InstructionIterator};

/// TODO: doc
#[derive(Default, Clone)]
pub enum Terminator {
    /// TODO: doc
    #[default]
    None,

    /// TODO: doc
    JumpUnconditional {
        /// TODO: doc
        opcode: Opcode,
        /// TODO: doc
        target: RcBasicBlock,
    },

    /// TODO: doc
    JumpConditional {
        /// TODO: doc
        opcode: Opcode,
        /// TODO: doc
        no: RcBasicBlock,
        /// TODO: doc
        yes: RcBasicBlock,
    },

    /// TODO: doc
    TemplateLookup {
        /// TODO: doc
        no: RcBasicBlock,

        /// TODO: doc
        yes: RcBasicBlock,

        /// TODO: doc
        site: u64,
    },

    /// TODO: doc
    Return {
        /// Finally block that the return should jump to, if exists.
        finally: Option<RcBasicBlock>,
    },
}

impl Terminator {
    /// Check if [`Terminator::None`].
    pub fn is_none(&self) -> bool {
        matches!(self, Terminator::None)
    }

    /// Check if [`Terminator::Jump`].
    pub fn is_jump(&self) -> bool {
        matches!(
            self,
            Terminator::JumpUnconditional { .. } | Terminator::JumpConditional { .. }
        )
    }

    /// Check if unconditional [`Terminator::Jump`].
    pub fn is_unconditional_jump(&self) -> bool {
        matches!(self, Terminator::JumpUnconditional { .. })
    }

    /// Check if conditional [`Terminator::Jump`].
    pub fn is_conditional_jump(&self) -> bool {
        matches!(self, Terminator::JumpConditional { .. })
    }
}

/// TODO: doc
pub struct ControlFlowGraph {
    basic_block_start: RcBasicBlock,
    basic_blocks: IndexSet<RcBasicBlock>,
}

impl Debug for ControlFlowGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BasicBlocks:")?;

        let mut seen = FxHashMap::default();
        let index_from_basic_block = |bb: &RcBasicBlock| {
            for (i, basic_block) in self.basic_blocks.iter().enumerate() {
                if basic_block == bb {
                    return i;
                }
            }

            unreachable!("There should be a basic block")
        };

        let mut index = 0;
        for basic_block in &self.basic_blocks {
            if seen.contains_key(&basic_block.as_ptr()) {
                continue;
            }
            seen.insert(basic_block.as_ptr(), index);

            let basic_block = basic_block.borrow();

            write!(
                f,
                "    B{index}: -- {}reachable",
                if basic_block.reachable() { "" } else { "not " }
            )?;

            if !basic_block.predecessors.is_empty() {
                write!(f, " -- predecessors ")?;
                for predecessor in &basic_block.predecessors {
                    if let Some(predecessor) = predecessor.upgrade() {
                        let index = index_from_basic_block(&predecessor);
                        write!(f, "B{index}, ")?;
                    }
                }
            }

            let successors = basic_block.successors();
            if !successors.is_empty() {
                write!(f, " -- successors ")?;
                for successor in &successors {
                    let index = index_from_basic_block(successor);
                    write!(f, "B{index}, ")?;
                }
            }

            writeln!(f, "")?;

            for result in InstructionIterator::new(&basic_block.bytecode) {
                writeln!(
                    f,
                    "        {:06}      {}",
                    result.current_opcode_pc,
                    result.opcode.as_str()
                )?;
            }

            let terminator = &basic_block.terminator;
            if !terminator.is_none() {
                write!(f, "        Terminator: ")?;
                match terminator {
                    Terminator::None => write!(f, "None")?,
                    Terminator::JumpUnconditional { opcode, target } => {
                        let target = index_from_basic_block(target);
                        write!(f, "{} B{target}", opcode.as_str())?;
                    }
                    Terminator::JumpConditional { opcode, no: _, yes } => {
                        let target = index_from_basic_block(yes);
                        write!(f, "{} B{target}", opcode.as_str())?;
                    }
                    Terminator::TemplateLookup {
                        no: _,
                        yes,
                        site: _,
                    } => {
                        let target = index_from_basic_block(yes);
                        write!(f, "TemplateLookup B{target}")?;
                    }
                    Terminator::Return { finally } => {
                        write!(f, "Return")?;
                        if let Some(finally) = finally {
                            let finally = index_from_basic_block(finally);
                            write!(f, " -- finally block B{finally}")?;
                        }
                    }
                }
                writeln!(f, "")?;
            }

            writeln!(f, "")?;

            index += 1;
        }

        Ok(())
    }
}

const fn is_jump_kind_opcode(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::Jump
            | Opcode::JumpIfTrue
            | Opcode::JumpIfFalse
            | Opcode::JumpIfNotUndefined
            | Opcode::JumpIfNullOrUndefined
            | Opcode::Case
            | Opcode::Default
            | Opcode::LogicalAnd
            | Opcode::LogicalOr
            | Opcode::Coalesce
            | Opcode::Break
            | Opcode::BreakLabel
            | Opcode::Continue
    )
}

impl ControlFlowGraph {
    /// Generate leaders for the [`BasicBlock`]s.
    fn leaders(bytecode: &[u8]) -> (Vec<u32>, bool) {
        let mut leaders: Vec<u32> = vec![];

        for result in InstructionIterator::new(bytecode) {
            match result.opcode {
                Opcode::Return => {
                    leaders.push(result.next_opcode_pc as u32);
                }
                Opcode::TryStart => {
                    let next_address = result.read::<u32>(0);
                    let finally_address = result.read::<u32>(4);
                    leaders.push(next_address);

                    if finally_address != u32::MAX {
                        leaders.push(finally_address);
                    }
                }
                Opcode::LoopStart => {
                    let start = result.read::<u32>(0);
                    let exit = result.read::<u32>(4);
                    leaders.push(start);
                    leaders.push(exit);
                }
                Opcode::LabelledStart => {
                    let exit = result.read::<u32>(0);
                    leaders.push(exit);
                }
                Opcode::TemplateLookup => {
                    let exit = result.read::<u32>(0);
                    leaders.push(exit);
                }
                opcode if is_jump_kind_opcode(opcode) => {
                    let target = result.read::<u32>(0);

                    leaders.push(result.next_opcode_pc as u32);
                    leaders.push(target);
                }
                _ => {}
            }
        }

        // If we have a leader at one byte after the last byte then we need an extra block for the reference to be valid.
        let mut need_extra_block = true;
        if leaders
            .iter()
            .position(|x| *x == bytecode.len() as u32)
            .is_none()
        {
            need_extra_block = false;
        }

        leaders.push(bytecode.len() as u32);
        leaders.sort_unstable();
        leaders.dedup();

        (leaders, need_extra_block)
    }

    /// TODO: doc
    pub fn generate(bytecode: &[u8]) -> Self {
        let (leaders, need_extra_block) = Self::leaders(bytecode);
        let block_count = leaders.len() + usize::from(need_extra_block);

        let mut basic_blocks = IndexSet::with_capacity(block_count);
        for _ in 0..block_count {
            basic_blocks.insert(RcBasicBlock::default());
        }

        let basic_block_from_bytecode_position = |address: u32| {
            let index = leaders
                .iter()
                .position(|x| *x == address)
                .expect("There should be a basic block")
                + 1;

            basic_blocks[index].clone()
        };

        let mut try_environment: Vec<(RcBasicBlock, Option<RcBasicBlock>)> = Vec::new();

        let mut iter = InstructionIterator::new(bytecode);
        for (i, leader) in leaders.iter().map(|x| *x as usize).enumerate() {
            let mut bytecode = Vec::new();
            let mut terminator = Terminator::None;
            while let Some(result) = iter.next() {
                let push_bytecode = match result.opcode {
                    Opcode::Return => {
                        let finally = try_environment
                            .iter()
                            .rev()
                            .filter_map(|(_, finally)| finally.clone())
                            .next();

                        if let Some(finally) = &finally {
                            finally
                                .borrow_mut()
                                .predecessors
                                .push(basic_blocks[i].downgrade());
                        }

                        terminator = Terminator::Return { finally };

                        false
                    }
                    opcode @ Opcode::Jump | opcode @ Opcode::Default => {
                        let address = result.read::<u32>(0);
                        let target = basic_block_from_bytecode_position(address);

                        target
                            .borrow_mut()
                            .predecessors
                            .push(basic_blocks[i].downgrade());

                        terminator = Terminator::JumpUnconditional { opcode, target };

                        false
                    }
                    Opcode::TemplateLookup => {
                        let address = result.read::<u32>(0);
                        let site = result.read::<u64>(4);
                        let yes = basic_block_from_bytecode_position(address);
                        let no = basic_blocks[i + 1].clone();

                        yes.borrow_mut()
                            .predecessors
                            .push(basic_blocks[i].downgrade());
                        no.borrow_mut()
                            .predecessors
                            .push(basic_blocks[i].downgrade());

                        terminator = Terminator::TemplateLookup { no, yes, site };

                        false
                    }
                    opcode if is_jump_kind_opcode(opcode) => {
                        let address = result.read::<u32>(0);
                        let yes = basic_block_from_bytecode_position(address);
                        let no = basic_blocks[i + 1].clone();

                        yes.borrow_mut()
                            .predecessors
                            .push(basic_blocks[i].downgrade());
                        no.borrow_mut()
                            .predecessors
                            .push(basic_blocks[i].downgrade());

                        terminator = Terminator::JumpConditional { opcode, no, yes };

                        false
                    }
                    Opcode::TryStart => {
                        let next_address = result.read::<u32>(0);
                        let finally_address = result.read::<u32>(4);

                        let mut finally = None;
                        if finally_address != u32::MAX {
                            finally = Some(basic_block_from_bytecode_position(finally_address));
                        }

                        try_environment
                            .push((basic_block_from_bytecode_position(next_address), finally));

                        true
                    }
                    Opcode::TryEnd => {
                        try_environment.pop();

                        true
                    }
                    _ => true,
                };

                if push_bytecode {
                    bytecode.extend_from_slice(result.full);
                }

                if leader == result.next_opcode_pc {
                    break;
                }
            }

            let mut basic_block = basic_blocks[i].borrow_mut();
            basic_block.bytecode = bytecode;
            basic_block.terminator = terminator;
        }

        assert!(try_environment.is_empty());

        Self {
            basic_block_start: basic_blocks[0].clone(),
            basic_blocks,
        }
    }

    /// Remove [`BasicBlock`].
    pub fn remove(&mut self, basic_block: &RcBasicBlock) {
        self.basic_blocks.shift_remove(basic_block);
    }

    /// Get [`BasicBlock`] index.
    pub fn get_index(&self, basic_block: &RcBasicBlock) -> usize {
        self.basic_blocks
            .get_index_of(basic_block)
            .expect("there should be a BasicBlock in CFG")
    }

    /// Finalize bytecode.
    pub fn finalize(self) -> Vec<u8> {
        let index_from_basic_block = |bb: &RcBasicBlock| {
            for (i, basic_block) in self.basic_blocks.iter().enumerate() {
                if Rc::ptr_eq(basic_block, bb) {
                    return i;
                }
            }

            unreachable!("There should be a basic block")
        };

        let mut results = Vec::new();
        let mut labels = Vec::new();
        let mut blocks = Vec::with_capacity(self.basic_blocks.len());

        for basic_block in &self.basic_blocks {
            let basic_block = basic_block.borrow();

            blocks.push(results.len() as u32);

            results.extend_from_slice(&basic_block.bytecode);
            match &basic_block.terminator {
                Terminator::None => {}
                Terminator::JumpUnconditional { opcode, target } => {
                    results.extend_from_slice(&[*opcode as u8]);
                    let start = results.len();
                    results.extend_from_slice(&[0, 0, 0, 0]);

                    let target = index_from_basic_block(target);
                    labels.push((start as u32, target));
                }
                Terminator::JumpConditional { opcode, no: _, yes } => {
                    results.extend_from_slice(&[*opcode as u8]);
                    let start = results.len();
                    results.extend_from_slice(&[0, 0, 0, 0]);

                    let target = index_from_basic_block(yes);
                    labels.push((start as u32, target));
                }
                Terminator::TemplateLookup { yes, site, .. } => {
                    results.extend_from_slice(&[Opcode::TemplateLookup as u8]);
                    let start = results.len();
                    results.extend_from_slice(&[0, 0, 0, 0]);
                    results.extend_from_slice(&site.to_ne_bytes());

                    let target = index_from_basic_block(yes);
                    labels.push((start as u32, target));
                }
                Terminator::Return { .. } => {
                    results.push(Opcode::Return as u8);
                }
            }
        }

        for (label, block_index) in labels {
            let address = blocks[block_index];

            let bytes = address.to_ne_bytes();
            results[label as usize] = bytes[0];
            results[label as usize + 1] = bytes[1];
            results[label as usize + 2] = bytes[2];
            results[label as usize + 3] = bytes[3];
        }

        results
    }
}

impl Drop for ControlFlowGraph {
    fn drop(&mut self) {
        // NOTE: Untie BasicBlock nodes, so they can be deallocated.
        for basic_block in &self.basic_blocks {
            *basic_block.borrow_mut() = BasicBlock::default();
        }
    }
}

/// Simplifies the [`ControlFlowGraph`].
///
/// # Operations
///
/// - Branch to same blocks -> jump
/// - Unrachable block elimination
#[derive(Clone, Copy)]
pub struct GraphSimplification;

impl GraphSimplification {
    /// TODO: doc
    pub fn perform(graph: &mut ControlFlowGraph) -> bool {
        let mut changed = false;
        for basic_block_ptr in &graph.basic_blocks {
            {
                let mut basic_block = basic_block_ptr.borrow_mut();

                match basic_block.terminator.clone() {
                    Terminator::None => {}
                    Terminator::Return { .. } => {}
                    Terminator::JumpConditional { no, yes, .. } => {
                        if no == yes {
                            basic_block.insert_last(&[Opcode::Pop as u8]);
                            basic_block.terminator = Terminator::JumpUnconditional {
                                opcode: Opcode::Jump,
                                target: yes,
                            };

                            changed |= true;
                        }
                    }
                    _ => {}
                }
            }
        }
        changed
    }
}

/// TODO: doc
#[derive(Clone, Copy)]
pub struct GraphEliminateUnreachableBasicBlocks;

impl GraphEliminateUnreachableBasicBlocks {
    /// TODO: doc
    pub fn perform(graph: &mut ControlFlowGraph) -> bool {
        let mut changed = false;

        let mut stack = vec![graph.basic_block_start.clone()];
        while let Some(basic_block_ptr) = stack.pop() {
            let mut basic_block = basic_block_ptr.borrow_mut();
            if basic_block.reachable() {
                break;
            }
            basic_block.flags |= BasicBlockFlags::REACHABLE;
            basic_block.next(&mut stack);

            // println!("{:p} -- {}", basic_block_ptr.as_ptr(), basic_block.reachable());
        }

        assert!(
            graph.basic_block_start.borrow().reachable(),
            "start basic block node should always be reachable"
        );

        let mut delete_list = Vec::new();
        for (i, basic_block) in graph.basic_blocks.iter().enumerate().rev() {
            if !basic_block.borrow().reachable() {
                delete_list.push(i);
            }
        }

        // println!("{delete_list:?}");

        for i in delete_list {
            let basic_block = graph
                .basic_blocks
                .shift_remove_index(i)
                .expect("there should be a BasicBlock in CFG");
            let mut basic_block = basic_block.borrow_mut();

            assert!(
                !basic_block.reachable(),
                "reachable basic blocks should not be eliminated"
            );

            basic_block.predecessors.clear();
            basic_block.terminator = Terminator::None;

            changed |= true;
        }

        changed
    }
}
