# Enigma Code Breaker

OCaml implementation of an Enigma cipher breaker that brute-forces rotor positions and plugboard configurations.

## Compilation

```bash
ocamlc -o breaker str.cma breaker.ml
```

## Execution

```bash
./breaker
```

## Algorithm

The program performs an exhaustive search over:
- All 17,576 rotor positions (26³)
- All plugboard configurations (0, 1, or 2 swaps)

For each configuration, it decrypts the ciphertext from `output/sample.txt` and searches for the crib "HEILHITLER". When found, it outputs the rotor positions, plugboard settings, and decrypted plaintext.

## Output Format

```
Found plausible config!
Positions: X Y Z
Plugboard: A<->B, C<->D
---
HEILHITLER
---
```

## Files

- `breaker.ml` - Source code
- `output/sample.txt` - Input ciphertext
- `input/sample.txt` - Reference plaintext
