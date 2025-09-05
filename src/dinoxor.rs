use std::arch::asm;

/// Perform a XOR operation on two bytes using ARM NEON instructions.
///
/// Parameters:
/// - `x`: First byte for the XOR operation.
/// - `y`: Second byte for the XOR operation.
///
/// Returns:
/// The result of the XOR operation, stored in `w0`.
///
/// Safety:
/// This function is unsafe because it uses low-level inline assembly and interacts with hardware registers directly.
///
/// Notes:
/// This function leverages ARM NEON instructions for obscure XOR operations. It internally calls several helper functions such as `spread_bits_to_bytes`, `prepare_xor_truth_table`, and others.
///
/// Examples:
/// ```no_run
/// use dinoxor::dinoxor::dinoxor;
///
/// let result = dinoxor(0b11101011, 0b11111111);
/// assert_eq!(result, 0b10100 );
///
/// ```
pub fn dinoxor(x: u8, y: u8) -> u8 {
    let result: u8;

    unsafe {
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

            x = in(reg) x,
            y = in(reg) y,
            lateout("w0") result,
            clobber_abi("C"),

            spread_bits_to_bytes = sym spread_bits_to_bytes,
            prepare_xor_truth_table = sym prepare_xor_truth_table,
            prepare_multiplication_table = sym prepare_multiplication_table,
            calculate_xor_result = sym calculate_xor_result
        );
    }

    result
}

/// Compresses the last 8 bytes of the NEON register `v2` into a single u8. Each bit of the result corresponds to one of the last 8 bytes of `v2`. A bit is set if the corresponding byte is non-zero. This function is only used in unit tests.
///
/// # Parameters:
/// - `v2`: A NEON register holding 16 bytes. The function processes the last 8 bytes of this register.
///
/// # Returns:
/// - A `u8` value where each bit represents whether the corresponding byte in `v2` is non-zero.
///
/// # Safety:
/// This function is marked as unsafe because it directly manipulates NEON registers and relies on specific hardware instructions. Use with care.
///
/// # Notes:
/// - This function is intended for use in unit tests only, as it leverages imperative operations for explanatory code.
/// - The assembly code is specific to ARM64 architecture.
///
/// # Examples:
/// ```no_run
/// use dinoxor::dinoxor::_compress_bytes_to_bits;
/// let v2 = unsafe { _compress_bytes_to_bits() };
/// assert_eq!(v2, 0); // Example: if all bytes in v2 are zero
///
/// ```
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

/// Expands the 8 bits of the input byte in `w0` into eight 0x00/0x01 bytes in a NEON vector.
///
/// This routine reads the caller-provided input byte from the **AArch64** integer
/// register `w0` and *spreads* each of its 8 bits into a separate byte lane of a
/// vector register. The result is placed in **`v2`** (low 8 bytes), where each lane
/// is either `0x00` (bit = 0) or `0x01` (bit = 1). The bits are processed least-
/// significant first (bit 0 → byte 0, …, bit 7 → byte 7).
///
/// > **Summary:**  
/// > Input: `w0 = b7 b6 b5 b4 b3 b2 b1 b0` (bits)  
/// > Output: `v2.16b[0..7] = {b0, b1, b2, b3, b4, b5, b6, b7}` as bytes 0x00/0x01
///
/// # How it works
/// - Clears working vectors (`v1`, `v2`) and a scratch scalar (`w3`).
/// - Loops `w2 = 0..7`:
///   1. `lsr w3, w0, w2` → move bit *w2* of the input to LSB of `w3`.
///   2. `and w3, w3, #1` → isolate that bit (0 or 1).
///   3. `ext v2, v0, v0, #1` → shift the current byte vector state left by one byte.
///   4. `ins v2.b[0], w3` → insert the new 0/1 byte at lane 0.
///   5. `mov v0, v2` → commit the updated vector into `v0` for the next iteration.
/// - Final `ext v2, v0, v0, #1` aligns the last inserted byte (so the low 8 bytes of
///   `v2` hold the eight bit-bytes in order).
///
/// # Registers (inputs/outputs/clobbers)
/// - **Input:** `w0` (caller supplies the byte to spread).
/// - **Output:** **`v2.16b`** (low 8 lanes contain 0x00/0x01 for bits 0..7).
/// - **Clobbers:** `v0`, `v1`, `v2`, `w2`, `w3`, `w4`.
///
/// # Preconditions
/// - Target must be **AArch64** with NEON/AdvSIMD.
/// - The caller must place the input byte in `w0`.
/// - This routine assumes it can freely clobber `v0`,`v1`,`v2` and scratch scalars
///   `w2`,`w3`,`w4`. If you need to preserve them, save/restore at the call site.
/// - No stack or memory is touched (`options(nostack, nomem)`), but this function is
///   still `unsafe` as it relies on target-specific inline assembly and register
///   conventions.
///
/// # Semantics (pseudocode)
/// ```text
/// v := [0u8; 16]
/// for i in 0..8 {
///   bit := ((w0 >> i) & 1) as u8
///   v = shift_left_by_1_byte(v)      // drop highest byte, open slot at index 0
///   v[0] = bit                       // write 0x00 or 0x01
/// }
/// v = shift_left_by_1_byte(v)        // align
/// // Result: v2 := v ; v2[0..7] = [b0,b1,b2,b3,b4,b5,b6,b7] as bytes
/// ```
///
/// # Example
/// ```rust,ignore
/// // Requires AArch64 + NEON + your symbol being linked:
/// // Put input in w0, call the function, then read v2.
/// use std::arch::asm;
///
/// let x: u8 = 0b1011_0010; // b7..b0 = 1 0 1 1 0 0 1 0
/// let out: [u8; 16];
/// unsafe {
///     asm!(
///         "mov w0, {x}",
///         "bl {spread_bits_to_bytes}",
///         // Store v2 to memory to inspect (example using st1 intrinsic would be cleaner):
///         "st1 {{v2.16b}}, [{out}]",
///         x = in(reg) x,
///         out = in(reg) out.as_ptr(), // for illustration; in real code, use proper NEON stores/binds
///         spread_bits_to_bytes = sym spread_bits_to_bytes,
///         options(nostack)
///     );
/// }
/// // Now out[0..8] are 0x00/0x01 for bits [b0..b7].
/// ```
///
/// # Notes
/// - This routine is typically paired with a companion that **compresses** the eight
///   0/1 bytes back into a single bitfield (e.g., `_compress_bytes_to_bits`), forming
///   a round-trip suitable for property tests (QuickCheck).
/// - If you intend to chain calls or reuse `v0`, remember this function clobbers
///   `v0` while constructing the shifted stream; the final result is in `v2`.
///
/// # Safety
/// `unsafe` because:
/// - Uses inline assembly with hard-coded registers and AArch64-specific instructions.
/// - No ABI-level guarantees are made beyond the documented clobbers and the input in `w0`.
/// - The caller must ensure the target supports the required features and that clobbered
///   registers are not needed across the call.
///
/// # Performance
/// - Pure register dataflow; no memory traffic (`nostack`, `nomem`).
/// - The loop runs a fixed 8 iterations; suitable for inlining in hot paths.
pub unsafe extern "C" fn spread_bits_to_bytes() {
    asm!(
        // Clear the destination vector registers
        "eor v1.16b, v1.16b, v1.16b",
        "eor v2.16b, v2.16b, v2.16b",
        "eor w3, w3, w3", // Clear the scratch register
        "mov w2, #0",     // Initialize the counter for bit positions (0-7)
        "1:",
        "lsr w3, w0, w2", // Shift the input byte right by the current bit position to bring the target bit to the LSB
        "and w3, w3, #0x01", // Isolate the LSB (which is now the target bit)
        "mov w4, w3", // Move the processed bit to w4 (to ensure w4 is correctly updated before duplication)
        "ext v2.16b, v0.16b, v0.16b, #1", // Shift v0 left by one byte to make space for the new byte
        "ins v2.b[0], w4",                // Insert the new byte at position 0 of v2
        "mov v0.16b, v2.16b",             // Move the temporary result back to v0
        "add w2, w2, #1",                 // Increment the bit position counter
        "cmp w2, #8",                     // Compare the counter with 8 (number of bits in a byte)
        "b.lt 1b",                        // If the counter is less than 8, continue the loop
        "ext v2.16b, v0.16b, v0.16b, #1", // Shift the last byte inserted into its final position in v2
        "ret",
        options(nostack, nomem)
    );
}

/// Prepares the XOR truth table using inline assembly.
///
/// This function uses inline ARMv8 assembly to generate a 16-bit XOR truth table
/// in vector register v0. Two patterns are combined to form the truth table:
/// - `0x0001`, placed in the upper half of the 32-bit word
/// - `0x0100`, overlaid into the lower half of the 32-bit word
///
/// The result is the 32-bit value `0x0001_0100`, which is then duplicated across
/// all lanes of the vector register v0. This produces a 16-byte sequence encoding
/// the XOR truth table pattern.
///
/// Parameters: None. This function does not take any parameters and is called
/// directly without arguments.
///
/// Returns: None. This function does not return a value and is used internally
/// for preparing the XOR truth table.
///
/// Safety: The function is marked as `unsafe` and uses inline assembly, which
/// may carry risks of undefined behavior if not used carefully. It is intended
/// for low-level operations and should be handled with care.
///
/// Notes: The assembly code first loads `0x0001` into the upper half of the
/// register, then overlays `0x0100` into the lower half, producing the combined
/// value `0x0001_0100`. This is duplicated into all lanes of v0, yielding:
/// `v0 = {0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00}`
///
/// Examples: None. This function is called internally and should not be used
/// directly in user code.
///
/// Options: The `nostack` and `nomem` options are used to prevent the function
/// from using the stack or memory, making it suitable for use in performance-critical sections.
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

/// Prepares byte constants for a multiplication table using Arm AdvSIMD (NEON).
///
/// This routine does **not** perform any multiplication by itself. Instead, it
/// constructs a 128-bit vector in `v1` whose **lower 64-bit lane** is filled
/// with `0x02` bytes and whose **upper 64-bit lane** is filled with `0x01`
/// bytes. These constants can then be used elsewhere to build a multiplication
/// table or as operands for vectorized multiply instructions.
///
/// How it works:
/// - `movi v1.8b, #0x02` writes `0x02` into each of the 8 bytes of the **lower
///   64-bit lane** of `v1` (the upper 64 bits are unchanged here).
/// - `movi v8.8b, #0x01` writes `0x01` into each of the 8 bytes of the **lower
///   64-bit lane** of `v8`.
/// - `mov v1.d[1], v8.d[0]` copies `v8`’s lower 64-bit lane into `v1`’s upper
///   64-bit lane, overlaying that lane with `0x01` bytes.
///
/// Final state:
/// `v1 = { 0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02, 0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01 }`
///
/// # Safety
/// This function is `unsafe` because it uses inline assembly. Although it is
/// marked with `options(nostack, nomem)` to declare that it neither touches the
/// stack nor arbitrary memory, the caller must still ensure the target CPU
/// supports the used AdvSIMD instructions and that the call site is sound.
///
/// # Parameters
/// None.
///
/// # Returns
/// `()`
///
/// # Errors
/// None.
///
/// # Notes
/// The inline assembly includes a `ret` instruction to return directly from the
/// function body.
///
/// # Examples
/// This function is intended to be called internally by code that consumes the
/// prepared constants; it is not typically invoked directly.
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

/// Computes the XOR of two 16-bit values using SIMD index formation, a table
/// lookup, and a mask-and-reduce step (Armv8-A AdvSIMD/NEON).
///
/// This routine builds per-bit indices for the pair `(A, B)`, looks up the XOR
/// result for each bit through a 4-entry truth table, then converts those
/// per-bit results into a single scalar by masking and horizontally adding.
///
/// # How it works
///
/// ## 1) Index formation (from spread bits)
/// The inputs are assumed to be **already expanded** into byte lanes:
/// - `v2.16b` carries the per-bit **X indexes** for `A`
/// - `v1.16b` carries the per-bit **Y indexes** for `B` (as a small table of
///   multipliers)
///
/// ```text
/// mul v3.16b, v2.16b, v1.16b   ; element-wise partial indices
/// mov v3.d[1], v2.d[1]         ; move upper (Y) half to v3's low half
/// ext v1.16b, v3.16b, v3.16b, #8
/// add v1.16b, v3.16b, v1.16b   ; v1 := final indices 0..3 per bit
/// mov v1.d[1], xzr             ; zero the upper half (only low 8 bytes used)
/// ```
///
/// After this, each **low-half** byte of `v1` is an index in `{0,1,2,3}` that
/// encodes the bit pair `(A_i, B_i)` for bit position `i`.
///
/// ### Index map for a single bit
/// ```text
/// Index | (A,B) | XOR(A,B)
/// ------+-------+---------
///   0   |  0 0  |    0
///   1   |  0 1  |    1
///   2   |  1 0  |    1
///   3   |  1 1  |    0
/// ```
///
/// ## 2) Truth-table lookup (branchless XOR per bit)
/// `v0` must **already contain** the XOR truth table (prepared earlier by a
/// helper like `prepare_xor_truth_table`). The table is organized so that
/// `tbl` returns **0x00** for XOR=0 and **0x01** for XOR=1.
///
/// ```text
/// tbl v1.8b, {v0}, v1.8b   ; for each lane i, v1[i] := T[v1[i]]
/// ```
///
/// After `tbl`, the **low 8 bytes** of `v1` are either `0x00` (if A_i XOR B_i = 0)
/// or `0x01` (if A_i XOR B_i = 1).
///
/// ## 3) Masking and reduction to a scalar
/// Immediately after the lookup, we **repurpose** `v0` as a weight/mask vector
/// for bit-packing the 8 boolean results into a single byte. The constant
/// written into `v0.d[0]` is:
///
/// ```text
/// 0x0201_0804_2010_8040   (little-endian byte order in v0.d[0])
/// bytes: [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80]
/// ```
///
/// That is, `v0.8b = [1,2,4,8,16,32,64,128]`, i.e., one power-of-two weight per
/// bit position (LSB→MSB).
///
/// ### Masking diagram (per byte lane in the low half)
/// Let `r_i ∈ {0,1}` be the table result for bit position `i`.
///
/// ```text
/// Before mul:
///   v1.8b = [r_0, r_1, r_2, r_3, r_4, r_5, r_6, r_7]
///   v0.8b = [  1,   2,   4,   8,  16,  32,  64, 128]
///
/// Element-wise multiply:
///   v1.8b := v1.8b * v0.8b
///         = [r_0*1, r_1*2, r_2*4, r_3*8, r_4*16, r_5*32, r_6*64, r_7*128]
///
/// Horizontal add (low half):
///   addv b0, v1.8b  =>  b0 = Σ_i (r_i * 2^i)
/// ```
///
/// Thus `b0` becomes the 8-bit packed XOR of the low 8 bit positions. The code
/// then copies that scalar byte into `w0` as the return value.
///
/// ### End-to-end bit-flow (one low-half example)
/// ```text
/// (A,B) bits → indices (0..3) → tbl → r_i (0/1) → weights → sum
///
///   A: 1 0 1 1 0 0 1 0
///   B: 0 1 0 1 1 0 1 0
/// XOR:1 1 1 0 1 0 0 0  → r_i
///
/// r_i: [1,1,1,0,1,0,0,0]
/// mul with weights [1,2,4,8,16,32,64,128]:
///       [1,2,4,0,16,0,0,0]
/// addv → 1+2+4+16 = 23 → 0b00010111
/// ```
///
/// # Safety
/// - `unsafe` due to inline assembly and assumptions about register contents.
/// - Valid only on AArch64 targets with AdvSIMD (NEON).
/// - Caller must ensure:
///   - `v2`/`v1` are initialized as described (spread/index sources).
///   - `v0` holds the XOR truth table **before** the `tbl` step.
///   - Clobbered registers are acceptable at the call site.
/// - `options(nostack, nomem)` declares no stack/memory effects; register state
///   remains the caller’s responsibility.
///
/// # Notes
/// - Assembler syntaxes vary slightly; e.g., some toolchains require
///   `tbl v1.8b, {v0.16b}, v1.8b` and `ext v1.16b, v3.16b, v3.16b, #8`.
/// - The function clears the upper 64 bits of `v1` and reduces over the **low
///   8 bytes** only (`addv b0, v1.8b`).
///
/// # Returns
/// The packed XOR of the low 8 bit positions, in `w0`’s low byte. (If your
/// calling convention expects the 16-bit XOR value, extend/accumulate across
/// two halves accordingly.)
///
/// # Example
/// ```no_run
/// use dinoxor::dinoxor::calculate_xor_result;
///
/// unsafe {
///     // ...prepare v2 (X indexes) and v1 (Y multipliers), and preload v0 with the XOR table...
///     calculate_xor_result();
/// }
/// ```
pub unsafe extern "C" fn calculate_xor_result() {
    asm!(
        "mul v3.16b, v2.16b, v1.16b", // Multiply each byte in v2 (spread bits) by its corresponding byte in v1 (multiplication table)
        // The upper half of v3 now contains the relevant Xindexes
        "mov v3.d[1], v2.d[1]", // Move the upper half of v2 (Yindexes) to the lower half of v3
        "ext.16b v1, v3, v3, #8", // Extract the upper half of v3 and store it in v1
        "add.16b v1, v3, v1",   // Add v3 and v1 to get the final indices for the truth table lookup
        "mov.d v1[1], xzr",     // Clear the upper half of v1 (set it to 0)
        "tbl.8b v1, {{v0}}, v1", // Perform a table lookup using the indices in v1 and the truth table in v0
        // Store the result in v1
        // Set up v0 with the desired values for the multiplication
        "movz x1, #0x0201, lsl #0", // Load the lower 16 bits of x1 with 0x0201
        "movk x1, #0x0804, lsl #16", // Load the next 16 bits of x1 with 0x0804
        "movk x1, #0x2010, lsl #32", // Load the next 16 bits of x1 with 0x2010
        "movk x1, #0x8040, lsl #48", // Load the upper 16 bits of x1 with 0x8040
        "mov v0.d[0], x1",          // Move the 64-bit value from x1 to the lower half of v0
        "mul v1.16b, v1.16b, v0.16b", // Multiply v1 (table lookup result) by v0 (predefined pattern) element-wise
        "addv b0, v1.8b", // Sum the values in the lower half of v1 and store the result in b0 (alias for lower 32 bit scalar in v0)
        "umov w0, v0.b[0]", // Move the 8-bit scalar value from b0 to w0 (return value)
        "ret",
        options(nostack, nomem)
    );
}
