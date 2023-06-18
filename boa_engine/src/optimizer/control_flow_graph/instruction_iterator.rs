use std::{iter::FusedIterator, mem::size_of};

use crate::vm::{code_block::Readable, Opcode};

pub(crate) struct InstructionIteratorResult<'bytecode> {
    pub(crate) current_opcode_pc: usize,
    pub(crate) next_opcode_pc: usize,
    pub(crate) opcode: Opcode,
    pub(crate) operands: &'bytecode [u8],
    pub(crate) full: &'bytecode [u8],
}

impl InstructionIteratorResult<'_> {
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

pub(crate) struct InstructionIterator<'bytecode> {
    bytecode: &'bytecode [u8],
    pc: usize,
}

impl<'bytecode> InstructionIterator<'bytecode> {
    pub(crate) fn new(bytecode: &'bytecode [u8]) -> Self {
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

impl<'bytecode> Iterator for InstructionIterator<'bytecode> {
    type Item = InstructionIteratorResult<'bytecode>;
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

        Some(InstructionIteratorResult {
            current_opcode_pc,
            next_opcode_pc: self.pc,
            opcode,
            operands: &self.bytecode[start_operand_byte..end_operand_byte],
            full: &self.bytecode[current_opcode_pc..end_operand_byte],
        })
    }
}

impl FusedIterator for InstructionIterator<'_> {}
