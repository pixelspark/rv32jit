use rv32assembler::{Fragment, Register};

#[test]
fn test_basics() {
	let mut f = Fragment::new();
	f.add(Register::A0, Register::A1, Register::A0);
	let p = f.binary();
	assert_eq!(&p, &[0x33, 0x85, 0xa5, 0x00]);
}

mod emulated {
	use rv32assembler::{Fragment, Register};
	use rvemu::bus::DRAM_BASE;
	use rvemu::cpu::XRegisters;
	use rvemu::emulator::Emulator;

	type SetRegister = (Register, u32);

	fn run(program: &[u8], regs: &[SetRegister]) -> XRegisters {
		let mut emu = Emulator::new();
		emu.initialize_dram(program.to_vec());
		emu.initialize_pc(DRAM_BASE);

		for (reg, value) in regs {
			emu.cpu.xregs.write(reg.number() as u64, *value as u64);
		}

		loop {
			match emu.cpu.execute() {
				Ok(_ins) => {}
				Err(e) => match e {
					rvemu::exception::Exception::Breakpoint => break,
					_ => panic!("exception: {e:?}"),
				},
			}
		}

		emu.cpu.xregs
	}

	fn check(regs: &XRegisters, values: &[SetRegister]) {
		for (reg, value) in values {
			let actual = regs.read(reg.number() as u64) as u32;
			assert_eq!(
				actual, *value,
				"register {reg:?} expected to be {value:08x}, found {actual:08x}",
			);
		}
	}

	#[test]
	fn test_emulated_add() {
		let mut f = Fragment::new();
		f.add(Register::A0, Register::A1, Register::A0);
		f.ebreak();
		let p = f.binary();

		check(
			&run(&p, &[(Register::A0, 12), (Register::A1, 30)]),
			&[(Register::A0, 42)],
		);
	}

	#[test]
	fn test_emulated_loop() {
		let mut f = Fragment::new();
		f.mv(Register::T0, Register::A0);
		f.li(Register::A0, 0);
		f.loop_until(|f, break_label| {
			f.add(Register::A0, Register::A0, Register::A1);
			f.subi(Register::T0, Register::T0, 1);
			f.blez(Register::T0, break_label);
		});
		f.ebreak();

		check(
			&run(&f.binary(), &[(Register::A0, 11), (Register::A1, 3)]),
			&[(Register::A0, 33)],
		);
	}
}
