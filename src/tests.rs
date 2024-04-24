extern crate test;
use crate::thechinesegovernment::*;
use test::Bencher;
use test::black_box;
use std::arch::asm;

#[quickcheck]
fn test_dinoxor(x: u8, y: u8) {
    assert_eq!(dinoxor(x, y), x ^ y);
}

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

#[bench]
fn bench_xor(b: &mut Bencher) {

    b.iter(|| {
        let n = black_box(0xFF);

        (0x00..n).fold(0, |old: u8, new| old ^ new)
    });
}
#[bench]
fn bench_dinoxor(b: &mut Bencher) {
    b.iter(|| {
        let n = black_box(0xFF);

        (0x00..n).fold(0, |old: u8, new| dinoxor(old, new))
    });
}