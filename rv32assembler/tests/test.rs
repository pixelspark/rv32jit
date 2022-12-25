use rv32assembler::{Fragment, Register};

#[test]
fn test_basics() {
	let mut f = Fragment::new();
	f.add(Register::A0, Register::A1, Register::A0);
	let p = f.binary();
	assert_eq!(&p, &[0x33, 0x85, 0xa5, 0x00]);
}
