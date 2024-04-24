.global _dinoxor
.text

// Function: dinoxor
// Description: Orchestrates a series of operations to replicate a bitwise XOR 
//              operation using NEON registers. This function initializes a XOR truth table,
//              calculates an index based on provided Xindex and Yindex, and creates a 
//              multiplication table to help calculate the index of each byte into the truth table.
// Arguments:
//   - x0: First operand byte
//   - x1: Second operand byte
// Returns: The result of the XOR operation between the two input bytes in w0.
_dinoxor:
    // Prologue: Prepare the stack and save callee-saved registers
    stp x29, x30, [sp, #-16]!  // Save the frame pointer and return address on the stack
    mov x29, sp                // Update the frame pointer to the current stack pointer

    mov x2, #0                 // Initialize x2 to 0 (not used later)
    eor v0.16b, v0.16b, v0.16b // Clear the contents of v0 (set all bits to 0)

    bl spread_bits_to_bytes    // Call the spread_bits_to_bytes function to load the first operand byte into the lower half of v2
    mov x0, x1                 // Move the second operand byte into x0
    bl spread_bits_to_bytes    // Call the spread_bits_to_bytes function to load the second operand byte into the lower half of v2 (shifting the previous value to upper)
    // After the above operations
    // For the inputs:
    //   x0 = 0b10101010
    //   x1 = 0b11111111
    // v2 contains:
    // v2 = {0x00 0x01 0x00 0x01 0x00 0x01 0x00 0x01 0x01 0x01 0x01 0x01 0x01 0x01 0x01 0x01}

    bl prepare_xor_truth_table // Call the prepare_xor_truth_table function to initialize the XOR truth table in v0
    // After the above operation, v0 contains:
    // v0 = {0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00}

    bl prepare_multiplication_table // Call the prepare_multiplication_table function to initialize the multiplication table in v1
    // After the above operation, v1 contains:
    // v1 = {0x02 0x02 0x02 0x02 0x02 0x02 0x02 0x02 0x01 0x01 0x01 0x01 0x01 0x01 0x01 0x01}

    bl calculate_xor_result // Call the calculate_xor_result function to calculate and store the XOR'd byte in w0.

    // Epilogue: Restore the stack and callee-saved registers
    ldp x29, x30, [sp], #16    // Restore the frame pointer and return address from the stack

    ret                        // Return to the caller

// Function: spread_bits_to_bytes
// Description: Spreads the bits of a byte into separate bytes in a NEON register.
// Arguments:
//   - x0: The input byte to be spread
// Returns: None (the result is stored in v2)
spread_bits_to_bytes:
    // Clear the destination vector registers
    eor v1.16b, v1.16b, v1.16b
    eor v2.16b, v2.16b, v2.16b

    mov w2, #0                 // Initialize the counter for bit positions (0-7)

spread_bit_loop:
    lsr w3, w0, w2             // Shift the input byte right by the current bit position to bring the target bit to the LSB
    and w3, w3, #0x01          // Isolate the LSB (which is now the target bit)
    
    mov w4, w3                 // Move the processed bit to w4 (to ensure w4 is correctly updated before duplication)
    
    ext v2.16b, v0.16b, v0.16b, #1  // Shift v0 left by one byte to make space for the new byte
    ins v2.b[0], w4                 // Insert the new byte at position 0 of v2
    mov v0.16b, v2.16b              // Move the temporary result back to v0

    add w2, w2, #1             // Increment the bit position counter
    cmp w2, #8                 // Compare the counter with 8 (number of bits in a byte)
    b.lt spread_bit_loop       // If the counter is less than 8, continue the loop

    ext v2.16b, v0.16b, v0.16b, #1  // Shift the last byte inserted into its final position in v2
    
    ret

// Function: prepare_xor_truth_table
// Description: Prepares the XOR truth table in a NEON register.
// Arguments: None
// Returns: None (the truth table is stored in v0)
prepare_xor_truth_table:
    // Load a pattern into a general-purpose register
    movz w8, #0x0001, lsl #16  // Load 0x0001 into the upper half of w8 (bits 16-31)
    movk w8, #0x0100           // Overlay 0x0100 into the lower half of w8 (bits 0-15)
    
    dup v0.4s, w8              // Duplicate the 32-bit value in w8 across all lanes of v0
    // After the above operation, v0 contains:
    // v0 = {0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00 0x00 0x01 0x01 0x00}
    
    ret

// Function: prepare_multiplication_table
// Description: Sets up a multiplication table in v1 to help calculate the index of each byte into the truth table.
// Arguments: None
// Returns: None (the multiplication table is stored in v1)
prepare_multiplication_table:
    // Load the patterns into NEON registers
    movi v1.8b, #0x02  // Set the lower half of v1 to 0x02
    movi v8.8b, #0x01  // Set the lower half of v8 to 0x01
    
    mov v1.d[1], v8.d[0]  // Move the lower half of v8 to the upper half of v1
    // After the above operations, v1 contains:
    // v1 = {0x02 0x02 0x02 0x02 0x02 0x02 0x02 0x02 0x01 0x01 0x01 0x01 0x01 0x01 0x01 0x01}
    
    ret

// Function: calculate_xor_result
// Description: Calculates the XOR result using the prepared data in NEON registers.
//              It multiplies the spread bits by the multiplication table to get the indices,
//              performs a table lookup using the XOR truth table, and then multiplies the result
//              by a predefined pattern to obtain the final XOR result.
// Arguments:
//   - v0: The XOR truth table
//   - v1: The multiplication table
//   - v2: The spread bits of the input operands
// Returns: The XOR result of the input operands in w0
calculate_xor_result:
    mul v3.16b, v2.16b, v1.16b // Multiply each byte in v2 (spread bits) by its corresponding byte in v1 (multiplication table)
                               // The upper half of v3 now contains the relevant Xindexes
    mov v3.d[1], v2.d[1]       // Move the upper half of v2 (Yindexes) to the lower half of v3
    ext.16b v1, v3, v3, #8     // Extract the upper half of v3 and store it in v1
    add.16b v1, v3, v1         // Add v3 and v1 to get the final indices for the truth table lookup
    mov.d v1[1], xzr           // Clear the upper half of v1 (set it to 0)
    tbl.8b v1, {v0}, v1        // Perform a table lookup using the indices in v1 and the truth table in v0
                               // Store the result in v1

    // Set up v0 with the desired values for the multiplication
    movz x1, #0x0201, lsl #0   // Load the lower 16 bits of x1 with 0x0201
    movk x1, #0x0804, lsl #16  // Load the next 16 bits of x1 with 0x0804
    movk x1, #0x2010, lsl #32  // Load the next 16 bits of x1 with 0x2010
    movk x1, #0x8040, lsl #48  // Load the upper 16 bits of x1 with 0x8040
    mov v0.d[0], x1            // Move the 64-bit value from x1 to the lower half of v0

    mul v1.16b, v1.16b, v0.16b // Multiply v1 (table lookup result) by v0 (predefined pattern) element-wise
    addv b0, v1.8b             // Sum the values in the lower half of v1 and store the result in b0
    umov w0, v0.b[0]           // Move the 8-bit scalar value from b0 to w0 (return value)

    ret
