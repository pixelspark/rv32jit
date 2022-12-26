/*
I-type instructions contain 12-bit signed immediate values (e.g. addi a0, a0, 1337). Signed negative value have their upper
bits set to 1 and 'count back' from zero (e.g. -1 in 12 bit is 0b111111111111, -2 is 0b111111111110, etc.).

To use larger immediates than 12 bits, you can use the `lui` instruction to load 20 immediate bits to the upper 20 bits of
a register. (lui+addi can be used for a 'large add', lui+jalr for a 'far jump' to any 32 bit address).

Note that `addi` and `jalr` propagate the sign to the upper 20 bits. Basically if the 12th bit of their immediate is a 1,
the high value is subtracted by one. To fix this, the high value (immediate of preceding lui) needs to be pre-incremented by
1 (the `call` function does this).

Note that logical functions (andi, ori, xori) sign-extend the 12 bit immediate value, which means all bits 31...13 will
be set to the value of bit 12. For instance the 12 bit immediate 0xF0000 will become 0xFFFF0000, and 0x0F000 becomes 0x0000F000.
*/

use std::ops::AddAssign;

#[derive(Debug, Clone)]
pub struct Fragment {
	pub(crate) code: Code,
	pub(crate) labels: Vec<Option<Position>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Code {
	code: Vec<Provisional>,
	position: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position(u32);

/// Size of a single instruction
pub(crate) const LOWER_20_BITS: u32 = 0b00000000000011111111111111111111;
pub(crate) const LOWER_12_BITS: u32 = 0b00000000000000000000111111111111;
pub(crate) const UPPER_20_BITS: u32 = 0b11111111111111111111000000000000;
pub(crate) const UPPER_12_BITS: u32 = 0b11111111111100000000000000000000;
pub(crate) const BIT_12: u32 = 0b00000000000000000000100000000000;

#[allow(clippy::upper_case_acronyms, dead_code)]
#[derive(Clone, Copy, Debug)]
pub(crate) enum Opcode {
	Add,
	Addi,
	And,
	Andi,
	Auipc,
	Beq,
	Bge,
	Bgeu,
	Blt,
	Bltu,
	Bne,
	Ebreak,
	Ecall,
	Jal,
	Jalr,
	Lb,
	Lbu,
	Lh,
	Lhu,
	Lui,
	Lw,
	Mul,
	Mulh,
	Mulsu,
	Mulu,
	Div,
	Divu,
	Rem,
	Remu,
	Or,
	Ori,
	Sb,
	Sh,
	Sll,
	Slli,
	Slt,
	Slti,
	Sltiu,
	Sltu,
	Sra,
	Srai,
	Srl,
	Srli,
	Sub,
	Sw,
	Xor,
	Xori,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub enum Register {
	Zero,
	ReturnAddress,
	StackPointer,
	GlobalPointer,
	ThreadPointer,
	T0,
	T1,
	T2,
	S0,
	S1,
	A0,
	A1,
	A2,
	A3,
	A4,
	A5,
	A6,
	A7,
	S2,
	S3,
	S4,
	S5,
	S6,
	S7,
	S8,
	S9,
	S10,
	S11,
	T3,
	T4,
	T5,
	T6,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Instruction {
	I {
		op: Opcode,
		rs: Register,
		rd: Register,
		imm: u32,
	},

	R {
		op: Opcode,
		rs1: Register,
		rs2: Register,
		rd: Register,
	},

	J {
		op: Opcode,
		rd: Register,
		imm: u32,
	},

	U {
		op: Opcode,
		rd: Register,
		imm: u32,
	},

	S {
		op: Opcode,
		imm: u32,
		rs1: Register,
		rs2: Register,
	},

	B {
		op: Opcode,
		imm: u32,
		rs1: Register,
		rs2: Register,
	},
}

#[derive(Clone, Copy, Debug)]
pub struct Label(usize);

#[derive(Debug, Clone)]
pub(crate) enum Provisional {
	Instruction(Instruction),
	Jump(Instruction, Label),
}

impl AddAssign<u32> for Position {
	fn add_assign(&mut self, rhs: u32) {
		self.0 += rhs;
	}
}

impl Code {
	pub fn new() -> Code {
		Code {
			code: vec![],
			position: Position(0),
		}
	}

	pub fn append(&mut self, ins: Provisional) {
		self.position += ins.size();
		self.code.push(ins);
	}

	pub fn position(&self) -> Position {
		self.position
	}
}

impl Provisional {
	pub(crate) fn encode(&self, at_position: Position, labels: &[Option<Position>]) -> u32 {
		match self {
			Self::Instruction(i) => i.encode(),
			Self::Jump(i, lbl) => {
				// Resolve the jump to a relative address
				let label_address = labels[lbl.0].unwrap();
				let dest = (label_address.0).wrapping_sub(at_position.0);

				let translated = match *i {
					Instruction::I { op, rs, rd, imm: _ } => Instruction::I {
						op,
						rs,
						rd,
						imm: dest,
					},
					Instruction::J { op, rd, imm: _ } => Instruction::J { op, rd, imm: dest },
					Instruction::U { op, rd, imm: _ } => Instruction::U { op, rd, imm: dest },
					Instruction::B {
						op,
						imm: _,
						rs1,
						rs2,
					} => Instruction::B {
						op,
						imm: dest,
						rs1,
						rs2,
					},
					Instruction::R { .. } => panic!("cannot jumpify an R insn"),
					Instruction::S { .. } => panic!("cannot jumpify an R insn"),
				};
				translated.encode()
			}
		}
	}

	fn size(&self) -> u32 {
		match self {
			Self::Jump(i, _) => i.size_bytes(),
			Self::Instruction(i) => i.size_bytes(),
		}
	}
}

impl Opcode {
	fn opcode(&self) -> u32 {
		match self {
			Self::Addi
			| Self::Xori
			| Self::Ori
			| Self::Andi
			| Self::Slli
			| Self::Srli
			| Self::Srai
			| Self::Slti
			| Self::Sltiu => 0b0010011,

			Self::Mul
			| Self::Mulh
			| Self::Mulsu
			| Self::Mulu
			| Self::Div
			| Self::Divu
			| Self::Rem
			| Self::Remu => 0b0110011,

			Self::Add
			| Self::Xor
			| Self::Sub
			| Self::Or
			| Self::And
			| Self::Sll
			| Self::Srl
			| Self::Sra
			| Self::Slt
			| Self::Sltu => 0b0110011,

			Self::Lb | Self::Lh | Self::Lw | Self::Lbu | Self::Lhu => 0b000011,

			Self::Jal => 0b1101111,
			Self::Jalr => 0b1100111,
			Self::Ecall | Self::Ebreak => 0b1110011,

			Self::Auipc => 0b0010111,
			Self::Lui => 0b0110111,
			Self::Sb | Self::Sh | Self::Sw => 0b0100011,

			Self::Beq | Self::Bne | Self::Blt | Self::Bge | Self::Bltu | Self::Bgeu => 0b1100011,
		}
	}

	fn funct3(&self) -> u32 {
		match self {
			Self::Add | Self::Sub => 0x0,
			Self::Addi => 0x0,
			Self::And => 0x7,
			Self::Andi => 0x7,
			Self::Auipc | Self::Lui => panic!("not valid for U type"),
			Self::Beq => 0x0,
			Self::Bge => 0x5,
			Self::Bgeu => 0x7,
			Self::Blt => 0x4,
			Self::Bltu => 0x6,
			Self::Bne => 0x1,
			Self::Div => 0x4,
			Self::Divu => 0x5,
			Self::Ebreak | Self::Ecall => 0x0,
			Self::Jal => panic!("not valid for J type"),
			Self::Jalr => 0x0,
			Self::Lb => 0x0,
			Self::Lbu => 0x4,
			Self::Lh => 0x1,
			Self::Lhu => 0x5,
			Self::Lw => 0x2,
			Self::Mul => 0x0,
			Self::Mulh => 0x1,
			Self::Mulsu => 0x2,
			Self::Mulu => 0x3,
			Self::Or => 0x6,
			Self::Ori => 0x6,
			Self::Rem => 0x6,
			Self::Remu => 0x7,
			Self::Sb => 0x0,
			Self::Sh => 0x1,
			Self::Sll => 0x1,
			Self::Slli => 0x1,
			Self::Slt => 0x2,
			Self::Slti => 0x2,
			Self::Sltiu => 0x3,
			Self::Sltu => 0x3,
			Self::Sra => 0x5,
			Self::Srai => 0x5,
			Self::Srl => 0x5,
			Self::Srli => 0x5,
			Self::Sw => 0x2,
			Self::Xor => 0x4,
			Self::Xori => 0x4,
		}
	}

	fn funct7(&self) -> u32 {
		match self {
			Self::Slli | Self::Srli | Self::Srai => {
				panic!("funct7 should be wrapped in immediate")
			}

			Self::Add => 0x00,
			Self::Sub => 0x20,
			Self::Xor | Self::Or | Self::And | Self::Sll | Self::Srl | Self::Slt | Self::Sltu => {
				0x00
			}

			Self::Mul
			| Self::Mulh
			| Self::Mulu
			| Self::Mulsu
			| Self::Div
			| Self::Divu
			| Self::Rem
			| Self::Remu => 0x01,

			Self::Sra => 0x20,
			Self::Addi
			| Self::Xori
			| Self::Ori
			| Self::Andi
			| Self::Slti
			| Self::Sltiu
			| Self::Lb
			| Self::Lh
			| Self::Lw
			| Self::Lbu
			| Self::Lhu
			| Self::Jalr
			| Self::Ecall
			| Self::Ebreak => {
				panic!("immediate format instruction never has funct7")
			}
			Self::Jal => panic!("not valid for J-type"),
			Self::Auipc | Self::Lui => panic!("not valid for U type"),
			Self::Sw | Self::Sh | Self::Sb => panic!("not valid for S-type"),
			Self::Beq | Self::Bne | Self::Blt | Self::Bge | Self::Bltu | Self::Bgeu => {
				panic!("not valid for B-type")
			}
		}
	}
}

impl Register {
	pub fn number(&self) -> u32 {
		match self {
			Register::Zero => 0,

			Register::ReturnAddress => 1,
			Register::StackPointer => 2,
			Register::GlobalPointer => 3,
			Register::ThreadPointer => 4,
			Register::T0 => 5,
			Register::T1 => 6,
			Register::T2 => 7,
			Register::S0 => 8,
			Register::S1 => 9,
			Register::A0 => 10,
			Register::A1 => 11,
			Register::A2 => 12,
			Register::A3 => 13,
			Register::A4 => 14,
			Register::A5 => 15,
			Register::A6 => 16,
			Register::A7 => 17,
			Register::S2 => 18,
			Register::S3 => 19,
			Register::S4 => 20,
			Register::S5 => 21,
			Register::S6 => 22,
			Register::S7 => 23,
			Register::S8 => 24,
			Register::S9 => 25,
			Register::S10 => 26,
			Register::S11 => 27,
			Register::T3 => 28,
			Register::T4 => 29,
			Register::T5 => 30,
			Register::T6 => 32,
		}
	}
}

impl Instruction {
	fn encode(&self) -> u32 {
		match self {
			Instruction::I { op, rs, rd, imm } => {
				assert!(
					imm & UPPER_20_BITS == 0,
					"upper 20 bits of immediate must be zero for I type instruction, but the immediate is {imm:032b} for op {op:?} rs={rs:?} rd={rd:?}"
				);
				imm << 20 | rs.number() << 15 | op.funct3() << 12 | rd.number() << 7 | op.opcode()
			}

			Instruction::R { op, rs1, rs2, rd } => {
				op.funct7() << 25
					| rs2.number() << 20 | rs1.number() << 15
					| op.funct3() << 12 | rd.number() << 7
					| op.opcode()
			}

			Instruction::J { op, rd, imm } => {
				// https://github.com/michaelmelanson/riscy/blob/cfab625aeaf8ea402ea877aafd30ae84dfc22df0/isa/src/instruction.rs#L666
				(((imm >> 20) & 0b1) << 31)
					| (((imm >> 1) & 0b1111111111) << 21)
					| (((imm >> 11) & 0b1) << 20)
					| (((imm >> 12) & 0b11111111) << 12)
					| (rd.number() << 7) | op.opcode()
			}

			Instruction::U { op, rd, imm } => {
				assert!(
					imm & UPPER_12_BITS == 0,
					"upper 12 bits must be zero for U-type instruction"
				);
				rd.number() << 7 | op.opcode() | ((imm & LOWER_20_BITS) << 12)
			}

			Instruction::S { op, imm, rs1, rs2 } => {
				(((imm >> 5) & 0b1111111) << 25)
					| (rs2.number() << 20)
					| (rs1.number() << 15)
					| (op.funct3() << 12)
					| ((imm & 0b11111) << 7)
					| op.opcode()
			}

			Instruction::B { op, imm, rs1, rs2 } => {
				// Note, this corrects for the fact that destination offsets (in the immediate value) are multiples of 2 bytes
				(((imm >> 20) & 0b1) << 31)
					| (((imm >> 5) & 0b111111) << 25)
					| (rs2.number() << 20)
					| (rs1.number() << 15)
					| (op.funct3() << 12)
					| (((imm >> 1) & 0b1111) << 8)
					| (((imm >> 11) & 0b1) << 7)
					| op.opcode()
			}
		}
	}

	fn size_bytes(&self) -> u32 {
		4
	}
}

macro_rules! binary_op {
	($func:ident, $op:expr) => {
		/// Append a $func binary (R) instruction
		#[allow(dead_code)]
		pub fn $func(&mut self, rd: Register, rs1: Register, rs2: Register) -> &mut Self {
			self.code.append(Provisional::Instruction(Instruction::R {
				op: $op,
				rd,
				rs1,
				rs2,
			}));
			self
		}
	};
}

macro_rules! immediate_op {
	($func:ident, $op:expr) => {
		/// Append a $func immediate (I) instruction
		#[allow(dead_code)]
		pub fn $func(&mut self, rd: Register, rs: Register, imm: u32) -> &mut Self {
			assert!(
				imm & (UPPER_20_BITS) == 0,
				"cannot accept immediates larger than 0x800 for op {:?} rd={:?} rs={:?} imm={:08x}",
				$op,
				rd,
				rs,
				imm
			);

			self.code.append(Provisional::Instruction(Instruction::I {
				op: $op,
				rs,
				rd,
				imm,
			}));
			self
		}
	};
}

macro_rules! j_op {
	($func:ident, $op:expr) => {
		/// Append a $func jump instruction
		#[allow(dead_code)]
		pub fn $func(&mut self, rd: Register, imm: u32) -> &mut Self {
			self.code.append(Provisional::Instruction(Instruction::J {
				op: $op,
				rd,
				imm,
			}));
			self
		}
	};
}

macro_rules! store_op {
	($func:ident, $op:expr) => {
		/// Append a $func store instruction
		#[allow(dead_code)]
		pub fn $func(&mut self, rs1: Register, imm: u32, rs2: Register) -> &mut Self {
			self.code.append(Provisional::Instruction(Instruction::S {
				op: $op,
				rs1,
				rs2,
				imm,
			}));
			self
		}
	};
}

macro_rules! branch_op {
	($func:ident, $op:expr) => {
		/// Append a $func branch instruction
		#[allow(dead_code)]
		pub fn $func(&mut self, rs1: Register, rs2: Register, label: Label) -> &mut Self {
			self.code.append(Provisional::Jump(
				Instruction::B {
					op: $op,
					rs1,
					rs2,
					imm: 0,
				},
				label,
			));
			self
		}
	};
}

impl Fragment {
	pub fn new() -> Fragment {
		Fragment {
			code: Code::new(),
			labels: vec![],
		}
	}

	/// Return from subroutine
	pub fn ret(&mut self) -> &mut Self {
		// Equals jalr x0, x1, 0 / jalr zero, ra, 0
		self.jalr(Register::Zero, Register::ReturnAddress, 0);
		self
	}

	/// No operation
	pub fn nop(&mut self) -> &mut Self {
		self.addi(Register::Zero, Register::Zero, 0);
		self
	}

	/// Copy register
	pub fn mv(&mut self, rd: Register, rs: Register) -> &mut Self {
		self.addi(rd, rs, 0);
		self
	}

	/// Jump register
	pub fn jr(&mut self, rs: Register) -> &mut Self {
		self.jalr(Register::Zero, rs, 0);
		self
	}

	/// Two's complement / negate
	pub fn neg(&mut self, rd: Register, rs: Register) -> &mut Self {
		self.sub(rd, Register::Zero, rs);
		self
	}

	/// One's complement / not
	pub fn not(&mut self, rd: Register, rs: Register) -> &mut Self {
		self.xori(rd, rs, LOWER_12_BITS); // all ones in 12 bits represents -1 signed. This will be sign-extended before applying the XOR, so the register will be XOR'ed with 32 bit ones.
		self
	}

	/// Load immediate
	pub fn li(&mut self, rd: Register, imm: u32) -> &mut Self {
		//! TODO check size of immediate
		if (imm & 0xFFFFF800) != 0 {
			// The top twenty bits of the immediate are set, so we need to use two instructions
			let mut high = (imm & UPPER_20_BITS) >> 12;
			let low = imm & LOWER_12_BITS;
			if low & BIT_12 != 0 {
				// High bit of lower value set, need to pre-increment high value by one
				high += 1;
			}

			self.lui(rd, high);
			self.addi(rd, rd, low);
		} else {
			self.addi(rd, Register::Zero, imm);
		}
		self
	}

	/// Transfer control to OS
	pub fn ecall(&mut self) -> &mut Self {
		self.code.append(Provisional::Instruction(Instruction::I {
			op: Opcode::Ecall,
			rs: Register::Zero,
			rd: Register::Zero,
			imm: 0,
		}));
		self
	}

	/// Transfer control to debugger
	pub fn ebreak(&mut self) -> &mut Self {
		self.code.append(Provisional::Instruction(Instruction::I {
			op: Opcode::Ebreak,
			rs: Register::Zero,
			rd: Register::Zero,
			imm: 1,
		}));
		self
	}

	/// Adjust upper immediate PC
	pub fn auipc(&mut self, rd: Register, imm: u32) -> &mut Self {
		self.code.append(Provisional::Instruction(Instruction::U {
			op: Opcode::Auipc,
			rd,
			imm,
		}));
		self
	}

	/// Load upper immediate
	pub fn lui(&mut self, rd: Register, imm: u32) -> &mut Self {
		self.code.append(Provisional::Instruction(Instruction::U {
			op: Opcode::Lui,
			rd,
			imm,
		}));
		self
	}

	/// Call far function
	/// lui x1, offset[31:12]
	/// jalr x1, x1, offset[11:0]
	pub fn call(&mut self, address: u32) -> &mut Self {
		let mut high = (address >> 12) & UPPER_20_BITS;
		let low = address & LOWER_12_BITS;
		println!("call\t{address:032b}\nlui\t{high:032b}\njalr\t{low:032b}",);
		if low & BIT_12 != 0 {
			// high sign bit is set, need to increment high word with one
			// See: https://inst.eecs.berkeley.edu/~cs61c/resources/su18_lec/Lecture7.pdf, p 55
			high += 1;
		}
		self.lui(Register::ReturnAddress, high);
		self.jalr(Register::ReturnAddress, Register::ReturnAddress, low);
		self
	}

	// Immediate ops
	immediate_op!(addi, Opcode::Addi);
	immediate_op!(xori, Opcode::Xori);
	immediate_op!(ori, Opcode::Ori);
	immediate_op!(andi, Opcode::Andi);
	// immedate_op!(slli, Opcode::Slli);
	// immedate_op!(srli, Opcode::Srli);
	// immedate_op!(srai, Opcode::Srai);
	immediate_op!(slti, Opcode::Slti);
	immediate_op!(sltiu, Opcode::Sltiu);

	// Loads
	immediate_op!(lb, Opcode::Lb);
	immediate_op!(lh, Opcode::Lh);
	immediate_op!(lw, Opcode::Lw);
	immediate_op!(lbu, Opcode::Lbu);
	immediate_op!(lhu, Opcode::Lhu);

	// Binary operations
	binary_op!(add, Opcode::Add);
	binary_op!(sub, Opcode::Sub);
	binary_op!(xor, Opcode::Xor);
	binary_op!(and, Opcode::And);
	binary_op!(or, Opcode::Or);
	binary_op!(sll, Opcode::Sll);
	binary_op!(srl, Opcode::Srl);
	binary_op!(sra, Opcode::Sra);
	binary_op!(slt, Opcode::Slt);
	binary_op!(sltu, Opcode::Sltu);
	binary_op!(mul, Opcode::Mul);
	binary_op!(mulu, Opcode::Mulu);
	binary_op!(mulh, Opcode::Mulh);
	binary_op!(mulsu, Opcode::Mulsu);
	binary_op!(div, Opcode::Div);
	binary_op!(divu, Opcode::Divu);
	binary_op!(rem, Opcode::Rem);
	binary_op!(remu, Opcode::Remu);

	// Stores
	store_op!(sb, Opcode::Sb);
	store_op!(sh, Opcode::Sh);
	store_op!(sw, Opcode::Sw);

	// Branches
	branch_op!(beq, Opcode::Beq);
	branch_op!(bne, Opcode::Bne);
	branch_op!(blt, Opcode::Blt);
	branch_op!(bge, Opcode::Bge);

	// Jumps
	j_op!(jal, Opcode::Jal);
	immediate_op!(jalr, Opcode::Jalr);

	/// Generate an unconditional jumo to a specific label
	pub fn jump(&mut self, label: Label) -> &mut Self {
		self.code.append(Provisional::Jump(
			Instruction::J {
				op: Opcode::Jal,
				rd: Register::Zero,
				imm: 0,
			},
			label,
		));
		self
	}

	/// Branch if rs1 == 0
	pub fn beqz(&mut self, rs1: Register, label: Label) -> &mut Self {
		self.beq(rs1, Register::Zero, label)
	}

	/// Branch if rs1 != 0
	pub fn bnez(&mut self, rs1: Register, label: Label) -> &mut Self {
		self.bne(rs1, Register::Zero, label)
	}

	/// Branch if rs1 <= 0
	pub fn blez(&mut self, rs1: Register, label: Label) -> &mut Self {
		self.bge(Register::Zero, rs1, label)
	}

	/// Branch if rs1 >= 0
	pub fn bgez(&mut self, rs1: Register, label: Label) -> &mut Self {
		self.bge(rs1, Register::Zero, label)
	}

	/// Branch if rs1 < 0
	pub fn bltz(&mut self, rs1: Register, label: Label) -> &mut Self {
		self.blt(Register::Zero, rs1, label)
	}

	/// Branch if rs1 > 0
	pub fn bgtz(&mut self, rs1: Register, label: Label) -> &mut Self {
		self.blt(rs1, Register::Zero, label)
	}

	/// Branch if rs1 > rs2 (rs2 < rs1)
	pub fn bgt(&mut self, rs1: Register, rs2: Register, label: Label) -> &mut Self {
		self.blt(rs2, rs1, label)
	}

	/// Branch if rs1 <= rs2 (rs2 >= rs1)
	pub fn ble(&mut self, rs1: Register, rs2: Register, label: Label) -> &mut Self {
		self.bge(rs2, rs1, label)
	}

	/// Create a label that will refer to a point yet to be appended to this fragment. The label should be 'filled in' later
	/// by calling `label` at the appropriate point in the future.
	pub fn future_point(&mut self) -> Label {
		self.labels.push(None);
		println!("future point label: {}", self.labels.len() - 1);
		Label(self.labels.len() - 1)
	}

	/// Return a label that refers to the current end of the fragment
	pub fn current_point(&mut self) -> Label {
		self.labels.push(Some(self.code.position()));
		println!(
			"current point label {}={:?}",
			self.labels.len() - 1,
			self.code.position()
		);
		Label(self.labels.len() - 1)
	}

	/// Set the location of the given label to the current end of the fragment
	pub fn label(&mut self, label: Label) {
		assert_eq!(self.labels[label.0], None, "cannot re-assign label");
		self.labels[label.0] = Some(self.code.position());
	}

	// Return assembled fragment
	pub fn binary(&self) -> Vec<u8> {
		let mut position = Position(0);
		self.code
			.code
			.iter()
			.flat_map(|ins| {
				let res = ins.encode(position, &self.labels).to_le_bytes();
				position += ins.size();
				res
			})
			.collect()
	}

	pub fn write_to(&self, slice: &mut [u32]) {
		// Encode instructions
		let mut position = Position(0);
		for (idx, ins) in self.code.code.iter().enumerate() {
			slice[idx] = ins.encode(position, &self.labels);
			position += ins.size();
		}
	}

	pub fn size(&self) -> usize {
		self.code.position().0 as usize
	}
}

impl Default for Fragment {
	fn default() -> Self {
		Self::new()
	}
}
