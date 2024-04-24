#include <stdio.h>

#include <stdio.h>
#include <stdint.h>
#include <string.h>

extern uint8_t dinoxor(uint8_t x0, uint8_t x1);

// Function to swap two bytes
void swap(uint8_t *a, uint8_t *b) {
    uint8_t temp = *a;
    *a = *b;
    *b = temp;
}

/**
 * Key-Scheduling Algorithm (KSA)
 * 
 * The KSA initializes and shuffles the state array `S` using the provided key. The state array
 * starts as an identity permutation (0 to 255) and is shuffled in a way that is dependent on the key.
 * This shuffled state array is crucial for the security of the RC4 stream cipher, as it ensures that
 * the generated keystream will be unique for each key.
 *
 * @param key The encryption key as a string.
 * @param S The state array to be initialized and shuffled by the KSA.
 */
void KSA(char *key, uint8_t S[256]) {
    int len = strlen(key);
    for(int i = 0; i < 256; i++)
        S[i] = i;
    int j = 0;
    for(int i = 0; i < 256; i++) {
        // The KSA shuffling logic:
        // Each byte of the key influences the shuffling of the state array `S`.
        // The key is repeated as necessary to match the 256 iterations, ensuring the entire state array
        // is influenced by the key's data. This step is crucial for spreading the key's entropy throughout `S`.
        j = (j + S[i] + key[i % len]) % 256;

        // Swapping elements `S[i]` and `S[j]` further shuffles the state array.
        // This operation is critical for ensuring the state array's initial permutation
        // is well-mixed and heavily dependent on the key, laying the foundation for a secure keystream.
        swap(&S[i], &S[j]);
    }
}

/**
 * Pseudo-Random Generation Algorithm (PRGA)
 * 
 * The PRGA modifies the state array `S` further and generates a byte of the keystream at each iteration.
 * This keystream is then XORed with the plaintext to produce ciphertext or vice versa for decryption.
 * The security of the RC4 encryption relies on the complexity and unpredictability of the keystream,
 * which is generated based on the shuffled state array `S` produced by the KSA.
 *
 * @param S The shuffled state array from the KSA.
 * @param plaintext The input text (plaintext for encryption, ciphertext for decryption).
 * @param ciphertext The output text (ciphertext for encryption, decrypted plaintext for decryption).
 * @param len The length of the input text.
 */
void PRGA(uint8_t S[256], uint8_t *plaintext, uint8_t *ciphertext, size_t len) {
    int i = 0, j = 0;
    for(size_t n = 0; n < len; n++) {
        i = (i + 1) % 256;
        j = (j + S[i]) % 256;
        swap(&S[i], &S[j]);
        uint8_t rnd = S[(S[i] + S[j]) % 256]; // Generate the next byte of the keystream
        ciphertext[n] = rnd ^ plaintext[n]; // XOR it with the plaintext
    }
}

/**
 * RC4 Encryption/Decryption Function
 * 
 * This function combines the KSA and PRGA to encrypt or decrypt data using the RC4 algorithm.
 * RC4 is a symmetric stream cipher, meaning the same algorithm and key are used for both encryption
 * and decryption. The security of RC4 lies in the complexity of its keystream, which is unique for each key.
 *
 * @param key The encryption key as a string.
 * @param plaintext The input text (plaintext for encryption, ciphertext for decryption).
 * @param ciphertext The output text (ciphertext for encryption, decrypted plaintext for decryption).
 * @param len The length of the input text.
 */
void RC4(char *key, uint8_t *plaintext, uint8_t *ciphertext, size_t len) {
    uint8_t S[256]; // The state array, crucial for keystream generation
    KSA(key, S); // Initialize and shuffle the state array with the key
    PRGA(S, plaintext, ciphertext, len); // Generate the keystream and produce the output text
}

int main() {
    char *key = "Key"; // The encryption/decryption key
    uint8_t plaintext[] = "Plaintext"; // The plaintext to be encrypted
    size_t len = sizeof(plaintext) - 1; // Calculate the length of the plaintext
    uint8_t ciphertext[len]; // Array to hold the ciphertext

    // Encrypt the plaintext
    RC4(key, plaintext, ciphertext, len);
    printf("Ciphertext: ");
    for(size_t i = 0; i < len; i++)
        printf("%02hhX", ciphertext[i]); // Print each byte of ciphertext in hex
    printf("\n");

    // Decrypt the ciphertext (RC4 is symmetric, so the same function is used for decryption)
    uint8_t decryptedtext[len]; // Array to hold the decrypted text
    RC4(key, ciphertext, decryptedtext, len); // Decrypt the ciphertext
    printf("Decrypted Text: ");
    for(size_t i = 0; i < len; i++)
        printf("%c", decryptedtext[i]); // Print the decrypted text
    printf("\n");

    return 0;
}
