use crate::{Fragment, Label, Register, BIT_12, LOWER_12_BITS, UPPER_20_BITS};

impl Fragment {
	/// Generate a subroutine with prologue and epilogue, and the indicates number of slots for local variables (4-byte sized)
	/// Call using Fragment::jal_label(returned label)
	pub fn subroutine(&mut self, local_slots: u32, block: impl Fn(&mut Self, Label)) -> Label {
		let sr_label = self.current_point();
		self.prologue(local_slots * 4);
		block(self, sr_label);
		self.epilogue(local_slots * 4);
		sr_label
	}

	/// Loop forever
	pub fn loop_forever(&mut self, block: impl Fn(&mut Self)) -> &mut Self {
		let start = self.current_point();
		block(self);
		self.jump(start)
	}

	pub fn loop_until(&mut self, block: impl Fn(&mut Self, Label)) -> &mut Self {
		let break_label = self.future_point();
		let start = self.current_point();
		block(self, break_label);
		self.jump(start);
		self.label(break_label);
		self
	}

	/// Append instruction to set the indicated register to zero
	pub fn zero(&mut self, rd: Register) -> &mut Self {
		self.add(rd, Register::Zero, Register::Zero);
		self
	}

	/// Save the indicated register to the stack. The 'slot' indicates the offset from the stack pointer in 4 byte increments
	/// (as registers are 4 bytes).
	pub fn save_to_stack(&mut self, slot: u32, rs: Register) -> &mut Self {
		self.sw(Register::StackPointer, 4 + slot * 4, rs);
		self
	}

	/// Load the indicated register from the stack. The 'slot' indicates the offset from the stack pointer in 4 byte increments
	/// (as registers are 4 bytes).
	pub fn load_from_stack(&mut self, slot: u32, rd: Register) -> &mut Self {
		self.lw(rd, Register::StackPointer, 4 + slot * 4);
		self
	}

	/// Generates function prologue
	fn prologue(&mut self, local_bytes: u32) -> &mut Self {
		// SP must be 128-bit aligned, so 16*4 = 128
		let stack_size = num::Integer::next_multiple_of(&(local_bytes + 4), &16u32);

		self.subi(Register::StackPointer, Register::StackPointer, stack_size);
		self.sw(Register::StackPointer, 0, Register::ReturnAddress);
		self
	}

	/// Generates function epilogue
	fn epilogue(&mut self, local_bytes: u32) -> &mut Self {
		// SP must be 128-bit aligned, so 16*4 = 128
		let stack_size = num::Integer::next_multiple_of(&(local_bytes + 4), &16u32);

		// self.sw(Register::StackPointer, 0, Register::ReturnAddress);
		self.lw(Register::ReturnAddress, Register::StackPointer, 0);
		self.addi(Register::StackPointer, Register::StackPointer, stack_size);
		self.ret();
		self
	}

	/// Subtract immediate (= shorthand for addi -imm)
	pub fn subi(&mut self, rd: Register, rs: Register, imm: u32) -> &mut Self {
		assert!(
			imm & (UPPER_20_BITS | BIT_12) == 0,
			"cannot subtract immediates larger than 0x7FF with subi"
		);
		self.addi(rd, rs, (u32::wrapping_sub(0, imm) & LOWER_12_BITS) | BIT_12) // Set bit 12 as the sign bit so the 12 bit number is negative
	}
}
