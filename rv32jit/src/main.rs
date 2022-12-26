mod jit;

use jit::*;
use rv32assembler::*;
use std::ops::Not;

// If using the `binstart` feature of `esp-idf-sys`, always keep this module imported
use esp_idf_sys as _;

fn test_print(arg: u32) {
	println!("Hello from the JIT! arg={arg}");
}

#[cfg(target_arch = "riscv32")]
fn test_jit_loop() {
	let mut fragment = Fragment::new();
	fragment.subroutine(2, |fragment, _| {
		// The equivalent of:
		// let mut i = 10;
		// while i != 0 {
		//   a0 += 3;
		//   i -= 1;
		//  test_print(a0);
		// }
		fragment.li(Register::T1, 10);
		let label = fragment.current_point();
		fragment.subi(Register::T1, Register::T1, 1); // T1 -= 1
		fragment.addi(Register::A0, Register::A0, 3); // A0 += 3
		fragment.save_to_stack(0, Register::A0);
		fragment.save_to_stack(1, Register::T1);
		//fragment.sw(Register::StackPointer, 4, Register::A0); // Save register A0 to the stack
		//fragment.sw(Register::StackPointer, 8, Register::T1); // Save register T1 to the stack
		fragment.call(test_print as usize as u32);
		fragment.load_from_stack(0, Register::A0);
		fragment.load_from_stack(1, Register::T1);
		fragment.bne(Register::T1, Register::Zero, label); // if(T1!=0) jump label
	});

	println!("Fragment assembled: {fragment:?}");
	let program = JitFunction::from(&fragment);
	println!("Call fragment");
	let out = unsafe { program.call(13) };
	println!("Called");
	assert_eq!(out, 43);
	drop(program);
}

fn test_jit_call() {
	let mut f = Fragment::new();

	// Function prologue
	f.subroutine(0, |f, _| {
		let ptr = test_print as usize as u32; // test_print as *const fn() as u32;
		if (ptr & 3) != 0 {
			panic!("function pointer is not 4 byte aligned!");
		}
		f.call(ptr);
	});

	let program = JitFunction::from(&f);
	println!("Program is {program:?}");
	unsafe { program.call(0) };
	println!("Called");
}

fn test_jit_lui_large() {
	let mut f = Fragment::new();
	let magic: u32 = 0xCAFEBABE;
	let not_magic: u32 = magic.not();
	f.li(Register::A0, magic);
	f.not(Register::A0, Register::A0);
	f.ret();
	let program = JitFunction::from(&f);
	let s = unsafe { program.call(88) };
	assert_eq!(s, not_magic, "Result {s:032b} should be {not_magic:032b}");
}

fn test_jit_simple() {
	let mut f = Fragment::new();
	f.addi(Register::A0, Register::A0, 42);
	f.ret();
	let program = JitFunction::from(&f);
	println!("Add program is: {program:?}");
	let s = unsafe { program.call(88) };
	println!("Result: s= {} should be {}", s, 88 + 42);
}

fn main() {
	// Temporary. Will disappear once ESP-IDF 4.4 is released, but for now it is necessary to call this function once,
	// or else some patches to the runtime implemented by esp-idf-sys might not link properly.
	esp_idf_sys::link_patches();

	unsafe {
		let ret = esp_idf_sys::nvs_flash_init();
		if ret == esp_idf_sys::ESP_ERR_NVS_NO_FREE_PAGES
			|| ret == esp_idf_sys::ESP_ERR_NVS_NEW_VERSION_FOUND
		{
			esp_idf_sys::nvs_flash_erase();
			esp_idf_sys::nvs_flash_init();
		}
	}

	println!("Starting!");
	test_jit_simple();
	test_jit_lui_large();
	test_jit_call();
	test_jit_loop();
}
