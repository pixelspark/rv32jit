# RISC-V 32-bit JIT

An assembler for JIT compilation for use on ESP32-C3 (rv32imc)

## Setting up the environment

```
rustup toolchain install nightly
rustup target add riscv32imc-unknown-none-elf
cargo install ldproxy
cargo install cargo-espflash
```

## Building

```bash
cargo build --release
```

To flash and run:

```bash
cargo espflash --release --monitor
```

If you get a certificate error on macOS:

```bash
curl "https://ccadb-public.secure.force.com/mozilla/IncludedRootsPEMTxt?TrustBitsInclude=Websites" > IncludedRootsPEM.txt
SSL_CERT_FILE=$(pwd)/IncludedRootsPEM.txt cargo build
```

To test locally (replace `aarch64-apple-darwin` with your system's target triple, use `rustup target list` to find installed targets):

```bash
cargo run --example assemble --target aarch64-apple-darwin -- ~/Desktop/test2.bin
```

The above will write a binary that you can upload in e.g. [this disassembler](https://jborza.com/riscvdasm/).

## Testing

```bash
cd rv32assembler
cargo test --target aarch64-apple-darwin
```

## Usage

```rust
fn test_jit_simple() {
	let mut f = Fragment::new();
	f.addi(Register::A0, Register::A0, 42);
	f.ret();
	let program = f.jit();
	let s = unsafe { program.call(88) };
	assert_eq!(s, 88 + 42);
}
```

If you want to use this crate in your own project, be sure to set the following option in `sdkconfig.defaults`:

```toml
# Required to allow JIT
CONFIG_ESP_SYSTEM_MEMPROT_FEATURE=n
```

## References

Useful reference material:

- [RISC-V reference card](https://github.com/jameslzhu/riscv-card/blob/master/riscv-card.pdf)
- [RV32I Base Integer Instruction Set, Version 2.1](https://five-embeddev.com/riscv-isa-manual/latest/rv32.html#)
- [Online RISC-V assembler](https://riscvasm.lucasteske.dev/#)
- [Online RISC-V disassembler](https://jborza.com/riscvdasm/)
- [Another RISC-V assembler in Rust](https://github.com/michaelmelanson/riscy)
- [Berkeley CS61C RISC-V instruction formats lecture](https://inst.eecs.berkeley.edu/~cs61c/resources/su18_lec/Lecture7.pdf)
- [Sign extension](https://en.wikipedia.org/wiki/Sign_extension)

To create a binary file from "[1, 2, 3]" debug output:

```js
const fs = require("fs");
fs.writeFileSync("foo.bin", Buffer.from([1, 2, 3]));
```

## License

[MIT](./LICENSE)
