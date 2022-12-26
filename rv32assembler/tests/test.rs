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
	use rvemu::bus::MROM_BASE;
	use rvemu::cpu::XRegisters;
	use rvemu::emulator::Emulator;
	use rvemu::rom::Rom;

	type SetRegister = (Register, u32);

	fn run(program: &[u8], regs: &[SetRegister]) -> XRegisters {
		let mut emu = Emulator::new();
		let base = MROM_BASE;
		// The emulator emulates RV64i. This is fine as long as we use RV32i instructions and can ignore the upper half
		// of the 64-bit registers. This is usually the case but breaks down here because DRAM_BASE = 0x8000_0000. This means
		// all program addresses have their 32th bit set. Loading this into a 64-bit register causes sign extension, which
		// means bits 32 to 64 will be set, and making a jump fail. We load the program in the ROM as a workaround to ensure
		// 'low' PC values. The stack pointer still resides in DRAM for now (but we are only increment/decrementing it so that
		// is fine)
		//
		// emu.initialize_dram(program.to_vec());
		// emu.initialize_pc(DRAM_BASE);
		emu.is_debug = true;
		emu.cpu.bus.rom = Rom::new_with_data(program.to_vec());
		emu.initialize_pc(base);

		for (reg, value) in regs {
			emu.cpu.xregs.write(reg.number() as u64, *value as u64);
		}

		loop {
			match emu.cpu.execute() {
				Ok(_ins) => {
					// println!(
					// 	"{ins:08x} A0={:08x} RA={:08x} SP={:08x} PC={}",
					// 	emu.cpu.xregs.read(Register::A0.number() as u64),
					// 	emu.cpu.xregs.read(Register::ReturnAddress.number() as u64),
					// 	emu.cpu.xregs.read(Register::StackPointer.number() as u64)
					// 		- rvemu::bus::DRAM_BASE,
					// 	emu.cpu.pc - base
					// )
				}
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

	#[test]
	fn test_fib() {
		let mut f = Fragment::new();
		let end = f.future_point();
		f.jump(end);
		let fib = f.subroutine(2, |f, fib| {
			let ret_one = f.future_point();
			let fib_end = f.future_point();
			f.li(Register::T0, 1);
			f.ble(Register::A0, Register::T0, ret_one);

			// Save S0
			f.save_to_stack(0, Register::S0);
			f.save_to_stack(1, Register::S1);

			// Save A0 -> S0
			f.mv(Register::S0, Register::A0);

			// fib(n-1)
			f.subi(Register::A0, Register::S0, 1);
			f.jal_label(fib);
			f.mv(Register::S1, Register::A0); // Save return value in S1

			// fib(n-2)
			f.subi(Register::A0, Register::S0, 2);
			f.jal_label(fib);
			f.add(Register::A0, Register::S1, Register::A0); // fib(n-1)+fib(n-2) => A0

			// Restore S0, S1
			f.load_from_stack(0, Register::S0);
			f.load_from_stack(1, Register::S1);

			// Return
			f.jump(fib_end);

			f.label(ret_one);
			f.li(Register::A0, 1);
			f.label(fib_end);
		});
		f.label(end);
		f.jal_label(fib);
		f.ebreak();
		// println!("program: {:02x?}", f.binary());

		check(
			&run(&f.binary(), &[(Register::A0, 6)]),
			&[(Register::A0, 13)],
		);
	}
}
