//! TODO: doc

#![allow(dead_code)]

use std::{fmt::Debug, iter::FusedIterator, mem::size_of};

use rustc_hash::FxHashMap;

use crate::vm::{code_block::Readable, Opcode};

struct BytecodeIteratorResult<'bytecode> {
    current_opcode_pc: usize,
    next_opcode_pc: usize,
    opcode: Opcode,
    operands: &'bytecode [u8],
}

impl BytecodeIteratorResult<'_> {
    /// Read type T from code.
    ///
    /// # Safety
    ///
    /// Does not check if read happens out-of-bounds.
    pub(crate) unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        // Safety:
        // The function caller must ensure that the read is in bounds.
        //
        // This has to be an unaligned read because we can't guarantee that
        // the types are aligned.
        unsafe {
            self.operands
                .as_ptr()
                .add(offset)
                .cast::<T>()
                .read_unaligned()
        }
    }

    /// Read type T from code.
    #[track_caller]
    pub(crate) fn read<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        assert!(offset + size_of::<T>() - 1 < self.operands.len());

        // Safety: We checked that it is not an out-of-bounds read,
        // so this is safe.
        unsafe { self.read_unchecked(offset) }
    }
}

struct BytecodeIterator<'bytecode> {
    bytecode: &'bytecode [u8],
    pc: usize,
}

impl<'bytecode> BytecodeIterator<'bytecode> {
    fn new(bytecode: &'bytecode [u8]) -> Self {
        Self { bytecode, pc: 0 }
    }

    /// Read type T from code.
    ///
    /// # Safety
    ///
    /// Does not check if read happens out-of-bounds.
    pub(crate) unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        // Safety:
        // The function caller must ensure that the read is in bounds.
        //
        // This has to be an unaligned read because we can't guarantee that
        // the types are aligned.
        unsafe {
            self.bytecode
                .as_ptr()
                .add(offset)
                .cast::<T>()
                .read_unaligned()
        }
    }

    /// Read type T from code.
    #[track_caller]
    pub(crate) fn read<T>(&self, offset: usize) -> T
    where
        T: Readable,
    {
        assert!(offset + size_of::<T>() - 1 < self.bytecode.len());

        // Safety: We checked that it is not an out-of-bounds read,
        // so this is safe.
        unsafe { self.read_unchecked(offset) }
    }
}

impl<'bytecode> Iterator for BytecodeIterator<'bytecode> {
    type Item = BytecodeIteratorResult<'bytecode>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.pc == self.bytecode.len() {
            return None;
        }

        let current_opcode_pc = self.pc;

        let opcode = self.bytecode[self.pc].into();
        self.pc += size_of::<Opcode>();
        let start_operand_byte = self.pc;
        match opcode {
            Opcode::SetFunctionName => {
                let _operand = self.read::<u8>(self.pc);
                self.pc += size_of::<u8>();
            }
            Opcode::RotateLeft | Opcode::RotateRight => {
                let _operand = self.read::<u8>(self.pc);
                self.pc += size_of::<u8>();
            }
            Opcode::PushInt8 => {
                let _operand = self.read::<i8>(self.pc);
                self.pc += size_of::<i8>();
            }
            Opcode::PushInt16 => {
                let _operand = self.read::<i16>(self.pc);
                self.pc += size_of::<i16>();
            }
            Opcode::PushInt32 => {
                let _operand = self.read::<i32>(self.pc);
                self.pc += size_of::<i32>();
            }
            Opcode::PushRational => {
                let _operand = self.read::<f64>(self.pc);
                self.pc += size_of::<f64>();
            }
            Opcode::PushLiteral
            | Opcode::ThrowNewTypeError
            | Opcode::Jump
            | Opcode::JumpIfTrue
            | Opcode::JumpIfFalse
            | Opcode::JumpIfNotUndefined
            | Opcode::JumpIfNullOrUndefined
            | Opcode::CatchStart
            | Opcode::FinallyStart
            | Opcode::LabelledStart
            | Opcode::Case
            | Opcode::Default
            | Opcode::LogicalAnd
            | Opcode::LogicalOr
            | Opcode::Coalesce
            | Opcode::CallEval
            | Opcode::Call
            | Opcode::New
            | Opcode::SuperCall
            | Opcode::ConcatToString => {
                let _operand = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
            }
            Opcode::PushDeclarativeEnvironment | Opcode::PushFunctionEnvironment => {
                let _operand = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
            }
            Opcode::CopyDataProperties
            | Opcode::Break
            | Opcode::BreakLabel
            | Opcode::Continue
            | Opcode::LoopStart
            | Opcode::IteratorLoopStart
            | Opcode::TryStart
            | Opcode::GeneratorDelegateNext
            | Opcode::GeneratorDelegateResume => {
                let _operand1 = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
                let _operand2 = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
            }
            Opcode::TemplateLookup | Opcode::TemplateCreate => {
                let _operand1 = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
                let _operand2 = self.read::<u64>(self.pc);
                self.pc += size_of::<u64>();
            }
            Opcode::GetArrowFunction
            | Opcode::GetAsyncArrowFunction
            | Opcode::GetFunction
            | Opcode::GetFunctionAsync => {
                let _operand = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>() + size_of::<u8>();
            }
            Opcode::GetGenerator | Opcode::GetGeneratorAsync => {
                let _operand = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
            }
            Opcode::DefVar
            | Opcode::DefInitVar
            | Opcode::PutLexicalValue
            | Opcode::GetName
            | Opcode::GetLocator
            | Opcode::GetNameAndLocator
            | Opcode::GetNameOrUndefined
            | Opcode::SetName
            | Opcode::DeleteName
            | Opcode::ThrowMutateImmutable => {
                let _operand = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
            }
            Opcode::GetPropertyByName
            | Opcode::GetMethod
            | Opcode::SetPropertyByName
            | Opcode::DefineOwnPropertyByName
            | Opcode::DefineClassStaticMethodByName
            | Opcode::DefineClassMethodByName
            | Opcode::SetPropertyGetterByName
            | Opcode::DefineClassStaticGetterByName
            | Opcode::DefineClassGetterByName
            | Opcode::SetPropertySetterByName
            | Opcode::DefineClassStaticSetterByName
            | Opcode::DefineClassSetterByName
            | Opcode::DeletePropertyByName
            | Opcode::SetPrivateField
            | Opcode::DefinePrivateField
            | Opcode::SetPrivateMethod
            | Opcode::SetPrivateSetter
            | Opcode::SetPrivateGetter
            | Opcode::GetPrivateField
            | Opcode::PushClassFieldPrivate
            | Opcode::PushClassPrivateGetter
            | Opcode::PushClassPrivateSetter
            | Opcode::PushClassPrivateMethod
            | Opcode::InPrivate => {
                let _operand = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
            }
            Opcode::PushPrivateEnvironment => {
                let _count = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>() * (_count as usize + 1);
            }
            Opcode::GeneratorJumpOnResumeKind => {
                let _normal = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
                let _throw = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
                let _return = self.read::<u32>(self.pc);
                self.pc += size_of::<u32>();
            }
            Opcode::CreateIteratorResult => {
                let _done = self.read::<u8>(self.pc) != 0;
                self.pc += size_of::<u8>();
            }
            Opcode::Pop
            | Opcode::PopIfThrown
            | Opcode::Dup
            | Opcode::Swap
            | Opcode::PushZero
            | Opcode::PushOne
            | Opcode::PushNaN
            | Opcode::PushPositiveInfinity
            | Opcode::PushNegativeInfinity
            | Opcode::PushNull
            | Opcode::PushTrue
            | Opcode::PushFalse
            | Opcode::PushUndefined
            | Opcode::PushEmptyObject
            | Opcode::PushClassPrototype
            | Opcode::SetClassPrototype
            | Opcode::SetHomeObject
            | Opcode::Add
            | Opcode::Sub
            | Opcode::Div
            | Opcode::Mul
            | Opcode::Mod
            | Opcode::Pow
            | Opcode::ShiftRight
            | Opcode::ShiftLeft
            | Opcode::UnsignedShiftRight
            | Opcode::BitOr
            | Opcode::BitAnd
            | Opcode::BitXor
            | Opcode::BitNot
            | Opcode::In
            | Opcode::Eq
            | Opcode::StrictEq
            | Opcode::NotEq
            | Opcode::StrictNotEq
            | Opcode::GreaterThan
            | Opcode::GreaterThanOrEq
            | Opcode::LessThan
            | Opcode::LessThanOrEq
            | Opcode::InstanceOf
            | Opcode::TypeOf
            | Opcode::Void
            | Opcode::LogicalNot
            | Opcode::Pos
            | Opcode::Neg
            | Opcode::Inc
            | Opcode::IncPost
            | Opcode::Dec
            | Opcode::DecPost
            | Opcode::GetPropertyByValue
            | Opcode::GetPropertyByValuePush
            | Opcode::SetPropertyByValue
            | Opcode::DefineOwnPropertyByValue
            | Opcode::DefineClassStaticMethodByValue
            | Opcode::DefineClassMethodByValue
            | Opcode::SetPropertyGetterByValue
            | Opcode::DefineClassStaticGetterByValue
            | Opcode::DefineClassGetterByValue
            | Opcode::SetPropertySetterByValue
            | Opcode::DefineClassStaticSetterByValue
            | Opcode::DefineClassSetterByValue
            | Opcode::DeletePropertyByValue
            | Opcode::DeleteSuperThrow
            | Opcode::ToPropertyKey
            | Opcode::ToBoolean
            | Opcode::Throw
            | Opcode::TryEnd
            | Opcode::CatchEnd
            | Opcode::CatchEnd2
            | Opcode::FinallyEnd
            | Opcode::This
            | Opcode::Super
            | Opcode::Return
            | Opcode::PopEnvironment
            | Opcode::LoopEnd
            | Opcode::LoopContinue
            | Opcode::LoopUpdateReturnValue
            | Opcode::LabelledEnd
            | Opcode::CreateForInIterator
            | Opcode::GetIterator
            | Opcode::GetAsyncIterator
            | Opcode::GeneratorResumeReturn
            | Opcode::IteratorNext
            | Opcode::IteratorFinishAsyncNext
            | Opcode::IteratorValue
            | Opcode::IteratorResult
            | Opcode::IteratorDone
            | Opcode::IteratorToArray
            | Opcode::IteratorPop
            | Opcode::IteratorReturn
            | Opcode::IteratorStackEmpty
            | Opcode::RequireObjectCoercible
            | Opcode::ValueNotNullOrUndefined
            | Opcode::RestParameterInit
            | Opcode::RestParameterPop
            | Opcode::PushValueToArray
            | Opcode::PushElisionToArray
            | Opcode::PushIteratorToArray
            | Opcode::PushNewArray
            | Opcode::PopOnReturnAdd
            | Opcode::PopOnReturnSub
            | Opcode::GeneratorYield
            | Opcode::AsyncGeneratorYield
            | Opcode::GeneratorNext
            | Opcode::GeneratorSetReturn
            | Opcode::PushClassField
            | Opcode::SuperCallDerived
            | Opcode::Await
            | Opcode::NewTarget
            | Opcode::ImportMeta
            | Opcode::SuperCallPrepare
            | Opcode::CallEvalSpread
            | Opcode::CallSpread
            | Opcode::NewSpread
            | Opcode::SuperCallSpread
            | Opcode::SetPrototype
            | Opcode::PushObjectEnvironment
            | Opcode::IsObject
            | Opcode::SetNameByLocator
            | Opcode::PopPrivateEnvironment
            | Opcode::ImportCall
            | Opcode::Nop => {}
            Opcode::Reserved1
            | Opcode::Reserved2
            | Opcode::Reserved3
            | Opcode::Reserved4
            | Opcode::Reserved5
            | Opcode::Reserved6
            | Opcode::Reserved7
            | Opcode::Reserved8
            | Opcode::Reserved9
            | Opcode::Reserved10
            | Opcode::Reserved11
            | Opcode::Reserved12
            | Opcode::Reserved13
            | Opcode::Reserved14
            | Opcode::Reserved15
            | Opcode::Reserved16
            | Opcode::Reserved17
            | Opcode::Reserved18
            | Opcode::Reserved19
            | Opcode::Reserved20
            | Opcode::Reserved21
            | Opcode::Reserved22
            | Opcode::Reserved23
            | Opcode::Reserved24
            | Opcode::Reserved25
            | Opcode::Reserved26
            | Opcode::Reserved27
            | Opcode::Reserved28
            | Opcode::Reserved29
            | Opcode::Reserved30
            | Opcode::Reserved31
            | Opcode::Reserved32
            | Opcode::Reserved33
            | Opcode::Reserved34
            | Opcode::Reserved35
            | Opcode::Reserved36
            | Opcode::Reserved37
            | Opcode::Reserved38
            | Opcode::Reserved39
            | Opcode::Reserved40
            | Opcode::Reserved41
            | Opcode::Reserved42
            | Opcode::Reserved43
            | Opcode::Reserved44
            | Opcode::Reserved45
            | Opcode::Reserved46
            | Opcode::Reserved47
            | Opcode::Reserved48
            | Opcode::Reserved49
            | Opcode::Reserved50
            | Opcode::Reserved51 => unreachable!("Reserved opcodes are unrechable"),
        }

        let end_operand_byte = self.pc;

        Some(BytecodeIteratorResult {
            current_opcode_pc,
            next_opcode_pc: self.pc,
            opcode,
            operands: &self.bytecode[start_operand_byte..end_operand_byte],
        })
    }
}

impl FusedIterator for BytecodeIterator<'_> {}

/// TODO: doc
#[derive(Clone, Copy)]
pub enum Terminator {
    /// TODO: doc
    None,
    /// TODO: doc
    JumpUnconditional(u32),
    /// TODO: doc
    JumpConditional(Opcode, u32),
}

impl Debug for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::None => write!(f, "None"),
            Terminator::JumpUnconditional(target) => write!(f, "Jump B{target}"),
            Terminator::JumpConditional(opcode, target) => {
                write!(f, "{} B{target}", opcode.as_str())
            }
        }
    }
}

impl Terminator {
    /// Check if [`Terminator::None`].
    pub fn is_none(&self) -> bool {
        matches!(self, Terminator::None)
    }
}

/// TODO: doc
pub struct BasicBlock {
    bytecode: Vec<u8>,
    terminator: Terminator,
}

impl Debug for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // writeln!(f, "BasicBlock")?;
        for result in BytecodeIterator::new(&self.bytecode) {
            writeln!(
                f,
                "    {:06}      {}",
                result.current_opcode_pc,
                result.opcode.as_str()
            )?;
        }
        if !self.terminator.is_none() {
            writeln!(f, "    Terminator: {:?}", self.terminator)?;
        }

        Ok(())
    }
}

impl BasicBlock {
    /// Get nth instruction in the [`BasicBlock`].
    fn get(&mut self, nth: usize) -> Option<BytecodeIteratorResult<'_>> {
        BytecodeIterator::new(&self.bytecode).nth(nth)
    }

    /// Insert nth instruction in the [`BasicBlock`].
    fn insert(&mut self, nth: usize, instruction: &[u8]) -> bool {
        let start = if let Some(value) = self.get(nth) {
            value.next_opcode_pc.saturating_sub(1)
        } else {
            0
        };

        for i in 0..instruction.len() {
            self.bytecode.insert(start + i, instruction[i]);
        }

        true
    }

    /// Remove nth instruction in the [`BasicBlock`].
    fn remove(&mut self, nth: usize) -> bool {
        let Some(value) = self.get(nth) else {
            return false;
        };

        let start = value.current_opcode_pc;
        let length = value.next_opcode_pc - value.current_opcode_pc;

        for i in 0..length {
            self.bytecode.remove(start + i);
        }

        true
    }
}

/// TODO: doc
///
// TODO: Figure out best layout for `BasicBlock`s and
pub struct ControlFlowGraph {
    basic_blocks: Vec<BasicBlock>,
    sparse_index: Vec<u32>,
    references: Vec<Vec<u32>>,
}

impl Debug for ControlFlowGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BasicBlocks:")?;

        for (i, basic_block) in self.basic_blocks.iter().enumerate() {
            write!(f, "  B{i}:")?;
            if !self.references[i].is_empty() {
                write!(f, " -- referenced by ")?;
                for r in &self.references[i] {
                    write!(f, "B{r}, ")?;
                }
            }

            writeln!(f, "")?;

            writeln!(f, "{basic_block:#?}")?;
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
    )
}

impl ControlFlowGraph {
    /// TODO: doc
    pub fn generate(bytecode: &[u8]) -> Self {
        let mut leaders: Vec<u32> = vec![];

        for result in BytecodeIterator::new(bytecode) {
            match result.opcode {
                opcode if is_jump_kind_opcode(opcode) => {
                    let target = result.read::<u32>(0);

                    leaders.push(result.next_opcode_pc as u32);
                    leaders.push(target);
                }
                _ => {}
            }
        }

        leaders.sort_unstable();
        if leaders.first() != Some(&0) {
            leaders.push(0);
        }
        leaders.sort_unstable();
        leaders.dedup();

        let mut basic_blocks = Vec::new();
        let mut inverse_cfg = FxHashMap::default();
        for i in 0..leaders.len() {
            let leader = leaders[i] as usize;
            inverse_cfg.insert(leader as u32, i as u32);
            if i + 1 == leaders.len() {
                let vec = bytecode[leader..].to_vec();
                basic_blocks.push(BasicBlock {
                    bytecode: vec,
                    terminator: Terminator::None,
                });
                break;
            }
            let next = leaders[i + 1] as usize;
            let vec = bytecode[leader..next].to_vec();
            basic_blocks.push(BasicBlock {
                bytecode: vec,
                terminator: Terminator::None,
            });
        }

        let mut references: Vec<Vec<u32>> = Vec::new();
        references.resize(basic_blocks.len(), Vec::new());

        for (i, basic_block) in basic_blocks.iter_mut().enumerate() {
            let len = basic_block.bytecode.len();
            if let Some(result) = BytecodeIterator::new(&basic_block.bytecode).last() {
                if is_jump_kind_opcode(result.opcode) {
                    let target = result.read::<u32>(0);
                    let index = inverse_cfg.get(&target).expect("");
                    basic_block.terminator = if result.opcode == Opcode::Jump {
                        references[*index as usize].push(i as u32);

                        Terminator::JumpUnconditional(*index)
                    } else {
                        references[*index as usize].push(i as u32);

                        if i + 1 != references.len() {
                            references[*index as usize].push(i as u32 + 1);
                        }

                        Terminator::JumpConditional(result.opcode, *index)
                    };

                    basic_block.bytecode.truncate(len - 5);
                }
            }
        }

        let mut sparse_index = Vec::with_capacity(basic_blocks.len());
        for i in 0..(basic_blocks.len() as u32) {
            sparse_index.push(i);
        }

        Self {
            basic_blocks,
            sparse_index,
            references,
        }
    }

    // /// Asserts that there is no other reference to this basic block.
    // ///
    // /// Use for `remove` method.
    // fn assert_no_reference_to_basic_block(&self, nth: usize) {
    //     for (i, basic_block) in self.basic_blocks.iter().enumerate() {
    //         // We do not check for the block we are asserting on.
    //         if i == nth {
    //             continue;
    //         }

    //         let nth = nth as u32;
    //         match &basic_block.terminator {
    //             Terminator::None => {},
    //             Terminator::JumpUnconditional(target) => assert_ne!(*target, nth),
    //             Terminator::JumpConditional(_opcode, target) => assert_ne!(*target, nth),
    //         }
    //     }
    // }

    // /// Remove nth [`BasicBlock`].
    // pub fn remove(&mut self, nth: usize) {
    // }

    /// Get the nth [`BasicBlock`].
    pub fn get(&self, nth: usize) -> &BasicBlock {
        let index = self.sparse_index[nth];
        &self.basic_blocks[index as usize]
    }

    /// Get the nth [`BasicBlock`].
    pub fn get_mut(&mut self, nth: usize) -> &mut BasicBlock {
        let index = self.sparse_index[nth];
        &mut self.basic_blocks[index as usize]
    }

    /// Get [`BasicBlock`]s count in the [`ControlFlowGraph`].
    pub fn basic_blocks_len(&self) -> u32 {
        self.basic_blocks.len() as u32
    }

    /// Finalize bytecode.
    pub fn finalize(self) -> Vec<u8> {
        let mut results = Vec::new();
        let mut labels = Vec::new();
        let mut blocks = Vec::with_capacity(self.basic_blocks.len());
        for basic_block in self.basic_blocks {
            blocks.push(results.len() as u32);

            results.extend(basic_block.bytecode);
            match basic_block.terminator {
                Terminator::None => {}
                Terminator::JumpUnconditional(target) => {
                    results.extend_from_slice(&[Opcode::Jump as u8]);
                    let start = results.len();
                    results.extend_from_slice(&[0, 0, 0, 0]);
                    labels.push((start as u32, target));
                }
                Terminator::JumpConditional(opcode, target) => {
                    results.extend_from_slice(&[opcode as u8]);
                    let start = results.len();
                    results.extend_from_slice(&[0, 0, 0, 0]);
                    labels.push((start as u32, target));
                }
            }
        }

        for (label, block) in labels {
            let block_index = blocks[block as usize];

            let bytes = block_index.to_ne_bytes();
            results[label as usize] = bytes[0];
            results[label as usize + 1] = bytes[1];
            results[label as usize + 2] = bytes[2];
            results[label as usize + 3] = bytes[3];
        }

        results
    }
}
