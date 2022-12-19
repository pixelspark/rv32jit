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

## License

[MIT](./LICENSE)
