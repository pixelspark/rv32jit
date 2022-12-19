use crate::assembler::{Fragment, INSTRUCTION_BYTES};
use esp_idf_sys::{
	c_types::c_void, heap_caps_free, heap_caps_malloc, MALLOC_CAP_32BIT, MALLOC_CAP_EXEC,
};

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

impl Drop for JitFunction {
	fn drop(&mut self) {
		unsafe {
			// Release JIT code memory
			heap_caps_free(self.0);
		}
	}
}

impl Fragment {
	/// Make the fragment into a callable function
	pub fn jit(&self) -> JitFunction {
		unsafe {
			let buf = heap_caps_malloc(
				(self.code.len() as u32) * INSTRUCTION_BYTES,
				MALLOC_CAP_32BIT | MALLOC_CAP_EXEC,
			);
			assert!(!buf.is_null(), "could not allocate code memory");

			let slice: &mut [u32] =
				std::slice::from_raw_parts_mut(buf as *mut u32, self.code.len());

			// Encode instructions
			for (idx, ins) in self.code.iter().enumerate() {
				slice[idx] = ins.encode(idx as u32, &self.labels);
			}

			let slice_u8: &mut [u8] =
				std::slice::from_raw_parts_mut(buf as *mut u8, self.code.len() * 4);

			println!(
				"ASM program {:?} -> {:?} @ {:?}",
				self,
				slice_u8,
				slice.as_ptr()
			);
			JitFunction(buf)
		}
	}
}
