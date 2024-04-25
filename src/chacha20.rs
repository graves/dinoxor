extern crate quickcheck;

use core::arch::aarch64::*;
use quickcheck::{Arbitrary, Gen};

/// ChaCha20 state consists of 16 words (u32)
const STATE_LEN: usize = 16;

/// Number of rounds in the ChaCha20 algorithm
const NUM_ROUNDS: usize = 10; // ChaCha20 uses 20 rounds, each function call here represents 2 rounds

/// The ChaCha20 state.
pub struct ChaCha20State {
    state: [u32; STATE_LEN],
}

// Newtype structs for different array sizes
#[derive(Clone, Debug)]
pub struct Key(pub [u8; 32]);
#[derive(Clone, Debug)]
pub struct Nonce(pub [u8; 12]);
#[derive(Clone, Debug)]
pub struct Block(pub [u8; 64]);

// Implement Arbitrary for each new type
impl Arbitrary for Key {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut arr = [0u8; 32];
        for byte in arr.iter_mut() {
            *byte = u8::arbitrary(g);
        }
        Key(arr)
    }
}

impl Arbitrary for Nonce {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut arr = [0u8; 12];
        for byte in arr.iter_mut() {
            *byte = u8::arbitrary(g);
        }
        Nonce(arr)
    }
}

impl Arbitrary for Block {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut arr = [0u8; 64];
        for byte in arr.iter_mut() {
            *byte = u8::arbitrary(g);
        }
        Block(arr)
    }
}

impl ChaCha20State {
    /// Creates a new ChaCha20 state initialized with the key, nonce, and block counter.
    pub fn new(key: &[u8; 32], nonce: &[u8; 12], counter: u32) -> Self {
        let mut state = [
            0x6170_7865,
            0x3320_646e,
            0x7962_2d32,
            0x6b20_6574, // Constants
            0,
            0,
            0,
            0, // 256-bit key
            0,
            0,
            0,
            0,
            counter,                                                      // Block counter
            u32::from_le_bytes([nonce[0], nonce[1], nonce[2], nonce[3]]), // Nonce
            u32::from_le_bytes([nonce[4], nonce[5], nonce[6], nonce[7]]),
            u32::from_le_bytes([nonce[8], nonce[9], nonce[10], nonce[11]]),
        ];

        for i in 0..8 {
            state[4 + i] =
                u32::from_le_bytes([key[4 * i], key[4 * i + 1], key[4 * i + 2], key[4 * i + 3]]);
        }

        ChaCha20State { state }
    }

    /// Resets the state with a new key, nonce, and block counter.
    pub fn reset(&mut self, key: &[u8; 32], nonce: &[u8; 12], counter: u32) {
        self.state = [
            0x6170_7865,
            0x3320_646e,
            0x7962_2d32,
            0x6b20_6574, // Constants
            0,
            0,
            0,
            0, // 256-bit key
            0,
            0,
            0,
            0,
            counter,                                                      // Block counter
            u32::from_le_bytes([nonce[0], nonce[1], nonce[2], nonce[3]]), // Nonce
            u32::from_le_bytes([nonce[4], nonce[5], nonce[6], nonce[7]]),
            u32::from_le_bytes([nonce[8], nonce[9], nonce[10], nonce[11]]),
        ];

        for i in 0..8 {
            self.state[4 + i] =
                u32::from_le_bytes([key[4 * i], key[4 * i + 1], key[4 * i + 2], key[4 * i + 3]]);
        }
    }

    /// Encrypt or decrypt data using the ChaCha20 block function.
    pub unsafe fn process(&mut self, input: &[u8], output: &mut [u8]) {
        assert_eq!(
            input.len(),
            output.len(),
            "Input and output must be the same length"
        );

        let mut x = [
            vld1q_u32(&self.state[0]),
            vld1q_u32(&self.state[4]),
            vld1q_u32(&self.state[8]),
            vld1q_u32(&self.state[12]),
        ];

        // Perform rounds
        for _ in 0..NUM_ROUNDS {
            // Column rounds
            self.quarter_round(&mut x, 0, 1, 2, 3);
            // Diagonal rounds
            self.diagonal_round(&mut x);
        }

        // Add back original state and serialize the state to the output
        for i in 0..4 {
            x[i] = vaddq_u32(x[i], vld1q_u32(&self.state[i * 4]));
            let output_bytes =
                core::slice::from_raw_parts((&x[i] as *const uint32x4_t) as *const u8, 16);
            for j in 0..16 {
                output[i * 16 + j] = input[i * 16 + j] ^ output_bytes[j]; // XOR to produce output
            }
        }
    }

    /// Perform the ChaCha20 quarter round operation
    fn quarter_round(&self, x: &mut [uint32x4_t; 4], a: usize, b: usize, c: usize, d: usize) {
        unsafe {
            x[a] = vaddq_u32(x[a], x[b]);
            x[d] = veorq_u32(x[d], x[a]);
            x[d] = vorrq_u32(vshlq_n_u32(x[d], 16), vshrq_n_u32(x[d], 16)); // Rotate by 16 bits

            x[c] = vaddq_u32(x[c], x[d]);
            x[b] = veorq_u32(x[b], x[c]);
            x[b] = vorrq_u32(vshlq_n_u32(x[b], 12), vshrq_n_u32(x[b], 20)); // Rotate by 12 bits

            x[a] = vaddq_u32(x[a], x[b]);
            x[d] = veorq_u32(x[d], x[a]);
            x[d] = vorrq_u32(vshlq_n_u32(x[d], 8), vshrq_n_u32(x[d], 24)); // Rotate by 8 bits

            x[c] = vaddq_u32(x[c], x[d]);
            x[b] = veorq_u32(x[b], x[c]);
            x[b] = vorrq_u32(vshlq_n_u32(x[b], 7), vshrq_n_u32(x[b], 25)); // Rotate by 7 bits
        }
    }

    /// Perform the ChaCha20 diagonal round operation
    fn diagonal_round(&mut self, x: &mut [uint32x4_t; 4]) {
        self.quarter_round(x, 0, 1, 2, 3);

        let temp = x[1];
        x[1] = x[2];
        x[2] = x[3];
        x[3] = temp;
    }
}
