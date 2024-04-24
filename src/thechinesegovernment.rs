use std::arch::asm;

/// Perform a XOR operation on two bytes using ARM NEON instructions.
///
/// # Arguments
///
/// * `x` - First byte for the XOR operation.
/// * `y` - Second byte for the XOR operation.
///
/// # Returns
///
/// Returns the result of the XOR operation.
///
/// # Example
///
/// ```
/// use crate::thechinesegovernment::dinoxor;
/// 
/// let result = dinoxor(0b11101011, 0b11111111);
/// assert_eq!(result, 0b10100 )
/// ```
///
/// # Safety
///
/// This function contains unsafe code and should be used with care.
pub fn dinoxor(x: u8, y: u8) -> u8 {
    let result: u8;

    unsafe  { 
        asm!(
            "mov x2, #0",                           // Initialize x2 to 0 (not used later)
            "eor v0.16b, v0.16b, v0.16b",           // Clear the contents of v0 (set all bits to 0)

            "mov w0, {x:w}",                        // Move the first operand byte into w0

            "bl {spread_bits_to_bytes}",            // Call the spread_bits_to_bytes function to load the first operand byte into the lower half of v2
            "mov w0, {y:w}",                        // Move the second operand byte into w0
            "bl {spread_bits_to_bytes}",            // Call the spread_bits_to_bytes function to load the second operand byte into the lower half of v2 
                                                    // (shifting the previous value to upper)

            "bl {prepare_xor_truth_table}",         // Call the prepare_xor_truth_table function to initialize the XOR truth table in v0
            "bl {prepare_multiplication_table}",    // Call the prepare_multiplication_table function to initialize the multiplication table in v1
            "bl {calculate_xor_result}",            // Call the calculate_xor_result function to calculate and store the XOR'd byte in w0.

            "mov {result:w}, w0",                   // Move the result from w0 to the result variable

            x = in(reg) x,
            y = in(reg) y,
            result = inout(reg) x => result,

            spread_bits_to_bytes = sym spread_bits_to_bytes,
            prepare_xor_truth_table = sym prepare_xor_truth_table,
            prepare_multiplication_table = sym prepare_multiplication_table,
            calculate_xor_result = sym calculate_xor_result
        );
    }

    result
}

/// Compresses the last 8 bytes of the NEON register `v2` into a single u8.
/// Each bit of the result corresponds to one of the last 8 bytes of `v2`.
/// A bit is set if the corresponding byte is non-zero. This is only used in
/// unit tests.
///
/// # Safety
///
/// This function uses inline assembly and manipulates processor registers directly.
pub unsafe fn _compress_bytes_to_bits() -> u8 {
    let result: u8;
    asm!(
        // Initialize the result
        "eor w0, w0, w0",              // Clear the result register (w0 will hold the final byte)
        
        // Check each byte and set the corresponding bit in `w0`
        "umov w1, v2.b[8]",            // Move the byte at position 0 in v2 to w1
        "orr w0, w0, w1, lsl #0",      // Set bit 0 in w0 if w1 is non-zero
        "umov w1, v2.b[9]",            // Move the byte at position 1 in v2 to w1
        "orr w0, w0, w1, lsl #1",      // Set bit 1 in w0 if w1 is non-zero
        "umov w1, v2.b[10]",            // Repeat for each byte
        "orr w0, w0, w1, lsl #2",
        "umov w1, v2.b[11]",
        "orr w0, w0, w1, lsl #3",
        "umov w1, v2.b[12]",
        "orr w0, w0, w1, lsl #4",
        "umov w1, v2.b[13]",
        "orr w0, w0, w1, lsl #5",
        "umov w1, v2.b[14]",
        "orr w0, w0, w1, lsl #6",
        "umov w1, v2.b[15]",
        "orr w0, w0, w1, lsl #7",
        "mov {result:w}, w0",

        // Output the result
        result = out(reg) result,          // Output the result byte
    );
    result
}


pub unsafe extern "C" fn spread_bits_to_bytes() {
    asm!(
        // Clear the destination vector registers
        "eor v1.16b, v1.16b, v1.16b",
        "eor v2.16b, v2.16b, v2.16b",

        "eor w3, w3, w3", // Clear the scratch register

        "mov w2, #0",                         // Initialize the counter for bit positions (0-7)

        "1:",
            "lsr w3, w0, w2",                 // Shift the input byte right by the current bit position to bring the target bit to the LSB
            "and w3, w3, #0x01",              // Isolate the LSB (which is now the target bit)
            "mov w4, w3",                     // Move the processed bit to w4 (to ensure w4 is correctly updated before duplication)
            "ext v2.16b, v0.16b, v0.16b, #1", // Shift v0 left by one byte to make space for the new byte
            "ins v2.b[0], w4",                // Insert the new byte at position 0 of v2
            "mov v0.16b, v2.16b",             // Move the temporary result back to v0
            "add w2, w2, #1",                 // Increment the bit position counter
            "cmp w2, #8",                     // Compare the counter with 8 (number of bits in a byte)
            "b.lt 1b",                        // If the counter is less than 8, continue the loop
        "ext v2.16b, v0.16b, v0.16b, #1",     // Shift the last byte inserted into its final position in v2

        "ret",

        options(nostack, nomem)
    );
}

pub unsafe extern "C" fn prepare_xor_truth_table() {
    asm!(
        // Load a pattern into a general-purpose register
        "movz w4, #0x0001, lsl #16", // Load 0x0001 into the upper half of w8 (bits 16-31)
        "movk w4, #0x0100",          // Overlay 0x0100 into the lower half of w8 (bits 0-15)
        "dup v0.4s, w4",             // Duplicate the 32-bit value in w8 across all lanes of v0
        // After the above operation, v0 contains:
        // v0 = {0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00}

        "ret",

        options(nostack, nomem)
    );
}

pub unsafe extern "C" fn prepare_multiplication_table() {
    asm!(
        // Load the patterns into NEON registers
        "movi v1.8b, #0x02",    // Set the lower half of v1 to 0x02
        "movi v8.8b, #0x01",    // Set the lower half of v8 to 0x01
        "mov v1.d[1], v8.d[0]", // Move the lower half of v8 to the upper half of v1
        // After the above operations, v1 contains:
        // v1 = {0x02 0x02 0x02 0x02 0x02 0x02 0x02 0x02 0x01 0x01 0x01 0x01 0x01 0x01 0x01 0x01}

        "ret",
        options(nostack, nomem)
    );
}

pub unsafe extern "C" fn calculate_xor_result() {
    asm!(
        "mul v3.16b, v2.16b, v1.16b",   // Multiply each byte in v2 (spread bits) by its corresponding byte in v1 (multiplication table)
        // The upper half of v3 now contains the relevant Xindexes

        "mov v3.d[1], v2.d[1]",         // Move the upper half of v2 (Yindexes) to the lower half of v3
        "ext.16b v1, v3, v3, #8",       // Extract the upper half of v3 and store it in v1
        "add.16b v1, v3, v1",           // Add v3 and v1 to get the final indices for the truth table lookup
        "mov.d v1[1], xzr",             // Clear the upper half of v1 (set it to 0)
        "tbl.8b v1, {{v0}}, v1",        // Perform a table lookup using the indices in v1 and the truth table in v0

        // Store the result in v1
        // Set up v0 with the desired values for the multiplication
        "movz x1, #0x0201, lsl #0",     // Load the lower 16 bits of x1 with 0x0201
        "movk x1, #0x0804, lsl #16",    // Load the next 16 bits of x1 with 0x0804
        "movk x1, #0x2010, lsl #32",    // Load the next 16 bits of x1 with 0x2010
        "movk x1, #0x8040, lsl #48",    // Load the upper 16 bits of x1 with 0x8040
        "mov v0.d[0], x1",              // Move the 64-bit value from x1 to the lower half of v0
        "mul v1.16b, v1.16b, v0.16b",   // Multiply v1 (table lookup result) by v0 (predefined pattern) element-wise
        "addv b0, v1.8b",               // Sum the values in the lower half of v1 and store the result in b0 (alias for lower 32 bit scalar in v0)
        "umov w0, v0.b[0]",             // Move the 8-bit scalar value from b0 to w0 (return value)

        "ret",

        options(nostack, nomem)
    );
}
