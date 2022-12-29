use rv32assembler::Fragment;

#[derive(Debug)]
pub struct JitFunction(*mut c_void);

impl JitFunction {
	/// Call a JIT'ed function with a single argument.
	///
	/// # Safety
	/// JIT'ed function safety depends on input to assembler, and is not guaranteed in any way
	pub unsafe fn call(&self, arg: u32) -> u32 {
		unsafe {
			let ptr = self.0 as *const ();
			let code: extern "C" fn(u32) -> u32 = std::mem::transmute(ptr);
			(code)(arg)
		}
	}
}

use esp_idf_sys::{
	c_types::c_void, heap_caps_free, heap_caps_malloc, MALLOC_CAP_32BIT, MALLOC_CAP_EXEC,
};

impl Drop for JitFunction {
	fn drop(&mut self) {
		unsafe {
			// Release JIT code memory
			heap_caps_free(self.0);
		}
	}
}

impl From<&Fragment> for JitFunction {
	/// Make the fragment into a callable function
	fn from(fragment: &Fragment) -> JitFunction {
		unsafe {
			let buf = heap_caps_malloc(fragment.size() as u32, MALLOC_CAP_32BIT | MALLOC_CAP_EXEC);
			assert!(!buf.is_null(), "could not allocate code memory");

			let slice_u8: &mut [u8] =
				std::slice::from_raw_parts_mut(buf as *mut u8, fragment.size() * 4);

			fragment.write_to(slice_u8);

			println!(
				"ASM program {:?} -> {:02x?} @ {:?}",
				fragment,
				slice_u8,
				slice_u8.as_ptr()
			);
			JitFunction(buf)
		}
	}
}
