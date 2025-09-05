# Dinoxor

## Re-implementing bitwise operations as abstractions in arm64 neon registers.

### Why

I was somehow bored and stressed at the same time.

![alt text](https://raw.githubusercontent.com/graves/dinoxor/main/DougieAndBipolarBear.png)

## What Is This?
This project demonstrates the use of **inline assembly in Rust** to implement and benchmark cryptographic algorithms, with a focus on confusing reverse engineers. It includes:
- A `dinoxor` module for **obfuscated XOR operations** using inline ARM64 assembly and NEON registers.
- A `chacha20` module for implementing the **ChaCha20 stream cipher**.
- Benchmarking of performance differences between plaintext XOR and inline assembly-based "dinoxor".

## Key Features

### Inline Assembly in Rust
The project showcases how to:
1. Use the `asm!` macro for inline assembly
2. Access and manipulate registers (e.g., `v0`, `w0`)
3. Call helper functions from within assembly
4. Manage input/output variables and register bindings
5. Manipulate NEON registers for personal gain

### ChaCha20 Implementation
- Implements the ChaCha20 stream cipher using Rust's `quickcheck` for testing.
- Demonstrates how to replace insecure algorithms like RC4 with modern ciphers.

### Benchmarks
Performs performance comparisons:
- `dinoxor(x, y)` vs regular **eor** `x ^ y`.
- `ChaCha20::process_with_dinoxor` vs standard ChaCha20 implementation.

## How to Use

### Add as a Crate
Add this to your `Cargo.toml`:
```toml
[dependencies]
dinoxor = "*"
```

### Example Usage
```rust
use dinoxor::dinoxor::dinoxor;

let result = dinoxor(0b11101011, 0b11111111);
assert_eq!(result, 0b10100);
```

## Benchmarks (from `cargo bench`)
- **Regular XOR**: `~6.29 ns/iter`
- **Dinoxor**: `~7,095.95 ns/iter`

## See Also
- [Dinoxor Blog Post](https://awfulsec.com/dinoxor.html)
- [ChaCha20 Implementation](https://github.com/graves/dinoxor/tree/main/src/chacha20.rs)

## Developer Notes
- This project is meant to demonstrate how inline assembly can be used for security through obscurity.
- The implementation of ChaCha20 is for educational purposes only.

## License
This project is released under the [MIT License](https://github.com/graves/dinoxor/blob/main/LICENSE).
