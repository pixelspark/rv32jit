#!/bin/sh
pushd .
cd rv32assembler
#cargo test --target aarch64-apple-darwin -- emulated::test_fib --exact --nocapture
cargo test --target aarch64-apple-darwin -- --nocapture
popd