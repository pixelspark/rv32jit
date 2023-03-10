use rv32assembler::Fragment;
use std::{env, io::Write};

pub fn main() {
	let mut f = Fragment::new();
	f.subroutine(0, |f, _| {
		f.loop_forever(|f| {
			f.add(
				rv32assembler::Register::A0,
				rv32assembler::Register::A1,
				rv32assembler::Register::A0,
			);
		});
	});

	let program = f.binary();

	let args: Vec<String> = env::args().collect();
	if args.len() > 1 {
		let mut f = std::fs::File::create(&args[1]).unwrap();
		f.write_all(&program).expect("failed to write bytes");
	}

	println!("Fragment: {f:?}");
	println!("Assembled: {program:02x?}");
}
