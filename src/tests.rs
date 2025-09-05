extern crate test;
use crate::dinoxor::*;
use std::arch::asm;
use test::Bencher;
use test::black_box;

/// Tests the `dinoxor` function by asserting that it behaves like XOR for two unsigned 8-bit integers.
///
/// Parameters:
/// - `x`: An unsigned 8-bit integer (u8).
/// - `y`: Another unsigned 8-bit integer (u8).
///
/// Returns:
/// - `()` as a unit type.
///
/// Errors:
/// - This function does not return an error, but the test may fail if `dinoxor` is not correctly implemented.
///
/// Safety:
/// - The function operates on unsigned integers and is safe to use within the bounds of `u8`.
///
/// Notes:
/// - This function is a simple test to verify that the `dinoxor` function behaves like XOR.
#[quickcheck]
fn test_dinoxor(x: u8, y: u8) {
    assert_eq!(dinoxor(x, y), x ^ y);
}

/// Handle a test for spreading bits to bytes in registers.
///
/// This function is designed to exercise the behavior of spreading `u8` values
/// into bytes using inline assembly, specifically calling two helper functions:
/// `_compress_bytes_to_bits` and `spread_bits_to_bytes`.
///
/// Parameters:
/// - x: A u8 value to be spread into bytes.
///
/// Returns:
/// - This function does not return a value; it performs an assertion to verify
///   that the input and output match.
///
/// Safety:
/// - The function uses `unsafe` inline assembly, which requires careful attention
///   to ensure correct usage of registers and memory.
///
/// Notes:
/// - The comment inside the inline `asm!` block warns that this test may become
///   unreliable and suggests using a debugger for verification.
#[quickcheck]
fn test_spread_bits_to_bytes_registers(x: u8) {
    let mut res: u8;

    unsafe {
        asm!(
            "eor v0.16b, v0.16b, v0.16b",
            "bl {spread_bits_to_bytes}",
            "bl {compress_bytes_to_bits}",  // This test looks like it will become a lie. Use the debugger.
            "mov {res:w}, w0",
            in("w0") x,
            res = out(reg) res,
            spread_bits_to_bytes = sym spread_bits_to_bytes,
            compress_bytes_to_bits = sym _compress_bytes_to_bits,
        );
    }

    assert_eq!(x, res);
}

/// Tests the `prepare_xor_truth_table` function by invoking it via inline assembly, then verifies that the expected values are returned.
///
/// This test function uses `asm!` to call a C-like subroutine (`prepare_xor_truth_table`) and checks that the lower and upper 64-bit values match the expected bitmask pattern.
///
/// Parameters:
/// - None
///
/// Returns:
/// - `()` (no return value)
///
/// Errors:
/// - None; the test is zero-failure.
///
/// Safety:
/// - This function uses unsafe inline assembly and should be used with care, especially in production code. It is intended for testing purposes only.
///
/// Notes:
/// - The function uses `asm!` to directly invoke a subroutine, which is not idiomatic in Rust and should be avoided unless necessary.
/// - The test assumes that the `prepare_xor_truth_table` function is implemented and returns two 64-bit values.
#[test]
fn test_prepare_xor_truth_table() {
    let expected: u64 = 0x01_01_00_00_01_01_00;
    let mut lower: u64;
    let mut upper: u64;

    unsafe {
        asm!(
            "bl {prepare_xor_truth_table}",
            "umov {lower:x}, v0.d[0]",
            "umov {upper:x}, v0.d[1]",
            lower = out(reg) lower,
            upper = out(reg) upper,
            prepare_xor_truth_table = sym prepare_xor_truth_table
        );
    }

    assert_eq!(lower, expected);
    assert_eq!(upper, expected);
}

/// Test function to verify the preparation of multiplication tables using inline assembly.
///
/// This function calls `prepare_multiplication_table` via indirect address binding
/// and verifies that the result matches expected values for lower and upper bounds.
///
/// Parameters:
/// - None
///
/// Returns:
/// - `Ok(())` on success, or error if inline assembly fails
///
/// Safety:
/// - This function is marked as `unsafe` due to use of inline assembly.
///
/// Notes:
/// - The test uses unsafe inline assembly with `asm!`, and assumes the correct
///   value is returned.
#[test]
fn test_prepare_multiplication_table() {
    let expected_lower: u64 = 0x02_02_02_02_02_02_02_02;
    let expected_upper: u64 = 0x01_01_01_01_01_01_01_01;
    let mut lower: u64;
    let mut upper: u64;

    unsafe {
        asm!(
            "bl {prepare_multiplication_table}",
            "umov {lower:x}, v1.d[0]",
            "umov {upper:x}, v1.d[1]",
            lower = out(reg) lower,
            upper = out(reg) upper,
            prepare_multiplication_table = sym prepare_multiplication_table
        );
    }

    assert_eq!(lower, expected_lower);
    assert_eq!(upper, expected_upper);
}

/// Handle the test for calculating XOR results using inline assembly.
///
/// This function tests the `calculate_xor_result` function by comparing its
/// output against a directly computed XOR value. It uses inline assembly to
/// perform bitwise operations on the input bytes and verifies that the result
/// matches the expected value.
///
/// Parameters:
/// - x: An 8-bit unsigned integer (u8) to be XORed with y.
/// - y: An 8-bit unsigned integer (u8) to be XORed with x.
///
/// Returns:
/// - `Ok(())` on successful test execution.
///
/// Safety:
/// - This function is unsafe because it uses inline assembly, which has no
///   inherent safety guarantees.
///
/// Notes:
/// - The function uses the `asm!` macro to perform low-level bitwise operations.
///   This approach is used for testing purposes and may not be suitable for general
///   use.
#[quickcheck]
fn test_calculate_xor_result(x: u8, y: u8) {
    let expected_result = x ^ y;

    unsafe {
        // Preparing v2 with x and y
        asm!(
            "eor v2.16b, v2.16b, v2.16b", // Clear v2
            "mov w0, {x_val:w}",          // Load x into w0
            "bl {spread_bits_to_bytes}",  // Spread x bits into bytes in v2
            "mov w0, {y_val:w}",          // Load y into w0
            "bl {spread_bits_to_bytes}",  // Spread y bits into bytes in v2

            x_val = in(reg) x,
            y_val = in(reg) y,
            spread_bits_to_bytes = sym spread_bits_to_bytes
        );

        // Prepare XOR truth table in v0
        asm!(
            "bl {prepare_xor_truth_table}",
            prepare_xor_truth_table = sym prepare_xor_truth_table
        );

        // Prepare multiplication table in v1
        asm!(
            "bl {prepare_multiplication_table}",
            prepare_multiplication_table = sym prepare_multiplication_table
        );

        // Calculate XOR result
        let mut result: u8;
        asm!(
            "bl {calculate_xor_result}",
            "mov {result_res:w}, w0",  // Move the result from w0 to the result variable

            calculate_xor_result = sym calculate_xor_result,
            result_res = out(reg) result
        );

        assert_eq!(result, expected_result);
    }
}

/// Benchmarks the XOR operation on a range of values.
///
/// This function benchmarks the performance of folding over a range with XOR, using a black-boxed value to ensure no compiler optimizations affect the result. It iterates over the range `(0x00..n)` and computes the XOR of all values, starting with an initial value of 0.
///
/// Parameters:
/// - `b`: A mutable reference to a Bencher instance used for benchmarking.
///
/// Returns:
/// - No return value; this function is intended to be used within a benchmarking context.
///
/// Errors:
/// - None; this function is designed to be used in a benchmarking scenario and does not return errors.
///
/// Safety:
/// - This function is safe to call as it only performs basic arithmetic operations and does not access external resources.
///
/// Notes:
/// - The `black_box` macro is used to prevent compiler optimizations from altering the benchmark results.
/// - The XOR operation (`^`) is performed between consecutive values in the range.
#[bench]
fn bench_xor(b: &mut Bencher) {
    b.iter(|| {
        let n = black_box(0xFF);

        (0x00..n).fold(0, |old: u8, new| old ^ new)
    });
}
/// Handle a benchmark for the `dinoxor` function.
///
/// This benchmarks how quickly and efficiently the `dinoxor` algorithm processes
/// a sequence of inputs. It measures performance by iterating over a range and
/// applying the `dinoxor` function to each pair of values.
///
/// Parameters:
/// - `b`: A mutable reference to a Bencher, used to configure and run the benchmark.
///
/// Returns:
/// - None. This function does not return a value; it performs the benchmarking.
///
/// Errors:
/// - None. This function does not return an error; it is designed for benchmarking purposes.
///
/// Safety:
/// - This function is safe to use in a multi-threaded context as it does not share
///   state between iterations.
///
/// Notes:
/// - The benchmark uses `black_box` to prevent compiler optimizations from
///   interfering with the measurement.
/// - The input range is defined as `0x00..n`, where `n` is a value determined by
///   the benchmark setup.
#[bench]
fn bench_dinoxor(b: &mut Bencher) {
    b.iter(|| {
        let n = black_box(0xFF);

        (0x00..n).fold(0, |old: u8, new| dinoxor(old, new))
    });
}

use crate::chacha20::*;

/// Test function for ChaCha20 encryption/decryption with known vectors.
///
/// This test initializes a ChaCha20 state with specific key, nonce, and counter values,
/// encrypts zeroed plaintext to ciphertext using `process()`, then decrypts the
/// ciphertext back to plaintext with the same state. It asserts that decryption
/// successfully reverts ciphertext to original plaintext.
///
/// Parameters:
/// - None; function is private and self-contained.
///
/// Returns:
/// - `Result<()>` on success, with no explicit return value in this context.
///
/// Errors:
/// - Any I/O or state management errors during process/reset operations would
///   propagate up, though not explicitly documented here.
///
/// Safety:
/// - The function uses unsafe blocks with raw pointers and memory operations,
///   so it should only be called in contexts where safe Rust guarantees are
///   not required.
///
/// Notes:
/// - This test uses zeroed plaintext for simplicity but could be extended
///   to test with arbitrary data.
#[test]
fn test_chacha_known_vector() {
    let key = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
        0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
        0x1e, 0x1f,
    ];
    let nonce = [
        0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x4a, 0x00, 0x00, 0x00, 0x00,
    ];

    let counter: u32 = 1;
    let mut state = ChaCha20State::new(&key, &nonce, counter);

    let plaintext = [0u8; 64]; // Use zeroed plaintext for simplicity.
    let mut ciphertext = [0u8; 64];
    let mut decrypted_text = [0u8; 64];

    unsafe {
        state.process(&plaintext, &mut ciphertext);
        state.reset(&key, &nonce, counter); // Reset state or reinitialize if needed.
        state.process(&ciphertext, &mut decrypted_text);
    }

    assert_eq!(
        plaintext, decrypted_text,
        "Decryption failed to revert ciphertext to original plaintext"
    );
}

/// Tests the properties of ChaCha20 encryption and decryption.
///
/// This function initializes a `ChaCha20State` with the provided key, nonce, and counter.
/// It then encrypts a plaintext block to ciphertext using `process`, resets the state,
/// and decrypts the ciphertext back to plaintext. Finally, it asserts that
/// decryption successfully reverts ciphertext to original plaintext.
///
/// Parameters:
/// - `key`: The ChaCha20 key (64 bytes).
/// - `nonce`: The ChaCha20 nonce (12 bytes).
/// - `counter`: A 32-bit unsigned integer used to initialize the state.
/// - `plaintext`: The plaintext block (16 bytes) to be encrypted and decrypted.
///
/// Returns:
/// - `()` on success, indicating the test passed.
///
/// Safety:
/// - This function uses raw pointers and may panic if invalid memory access occurs.
///
/// Notes:
/// - The plaintext is assumed to be zeroed for simplicity in testing.
/// - `unsafe` block ensures proper handling of memory and state.
#[quickcheck]
fn test_chacha_properties(key: Key, nonce: Nonce, counter: u32, plaintext: Block) {
    let Key(key) = key;
    let Nonce(nonce) = nonce;
    let Block(plaintext) = plaintext;

    let mut state = ChaCha20State::new(&key, &nonce, counter);

    //let plaintext = [0u8; 64]; // Use zeroed plaintext for simplicity.
    let mut ciphertext = [0u8; 64];
    let mut decrypted_text = [0u8; 64];

    unsafe {
        state.process(&plaintext, &mut ciphertext);
        state.reset(&key, &nonce, counter); // Reset state or reinitialize if needed.
        state.process(&ciphertext, &mut decrypted_text);
    }

    assert_eq!(
        plaintext, decrypted_text,
        "Decryption failed to revert ciphertext to original plaintext"
    );
}

use rand::RngCore;

#[bench]
fn bench_chacha_known_vector_eor(b: &mut Bencher) {
    let key = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
        0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
        0x1e, 0x1f,
    ];
    let nonce = [
        0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x4a, 0x00, 0x00, 0x00, 0x00,
    ];

    let counter: u32 = 1;
    let mut state = ChaCha20State::new(&key, &nonce, counter);

    let mut rng = rand::rng();

    let mut plaintext = [0u8; 64];

    rng.fill_bytes(&mut plaintext);

    let mut ciphertext = [0u8; 64];
    let mut decrypted_text = plaintext;

    b.iter(|| {
        unsafe {
            state.process(&plaintext, &mut ciphertext);
            state.reset(&key, &nonce, counter); // Reset state or reinitialize if needed.
            state.process(&ciphertext, &mut decrypted_text);
        }
    });
}
#[bench]
fn bench_chacha_known_vector_dinoxor(b: &mut Bencher) {
    let key = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
        0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
        0x1e, 0x1f,
    ];
    let nonce = [
        0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x4a, 0x00, 0x00, 0x00, 0x00,
    ];

    let counter: u32 = 1;
    let mut state = ChaCha20State::new(&key, &nonce, counter);

    let mut rng = rand::rng();

    let mut plaintext = [0u8; 64];

    rng.fill_bytes(&mut plaintext);

    let mut ciphertext = [0u8; 64];
    let mut decrypted_text = plaintext;

    b.iter(|| {
        unsafe {
            state.process_with_dinoxor(&plaintext, &mut ciphertext);
            state.reset(&key, &nonce, counter); // Reset state or reinitialize if needed.
            state.process_with_dinoxor(&ciphertext, &mut decrypted_text);
        }
    });
}
