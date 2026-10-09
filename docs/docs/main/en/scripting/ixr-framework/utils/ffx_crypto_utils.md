# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### ffx_crypto_utils: `\gamedata\scripts\ixr_framework\utils\ffx_crypto_utils.script`
Utilities for cryptographic operations (hashing, encoding and encryption):
* `calculate_crc64`
* `calculate_sha1`
* `calculate_sha256`
* `base64_encode`
* `base64_decode`
* `xor_encode`

---

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_crypto_utils.script]
--// Compute a CRC64 hash of the input string.
calculate_crc64(input)
args:
  input (string) - string to hash
retval: (string) - CRC64 hash as a hexadecimal string

--// Compute a SHA-1 hash of the input string.
calculate_sha1(input)
args:
  input (string) - string to hash
retval: (string) - SHA-1 hash as a hexadecimal string

--// Compute a SHA-256 hash of the input string.
calculate_sha256(input)
args:
  input (string) - string to hash
retval: (string) - SHA-256 hash as a hexadecimal string

--// Encode a string as Base64.
base64_encode(input)
args:
  input (string) - string to encode
retval: (string) - Base64-encoded string

--// Decode a Base64 string.
base64_decode(input)
args:
  input (string) - Base64 string to decode
retval: (string) - decoded original string

--// Encode a string using XOR encryption with the given key.
xor_encode(input, key)
args:
  input (string) - string to encode
  key (string) - encryption key (XOR)
retval: (string) - encoded string
```
:::

### Usage examples:
```lua
--// Compute hashes.
local crc = ffx_crypto_utils.calculate_crc64("hello world")
print("CRC64: " .. crc)

local sha1 = ffx_crypto_utils.calculate_sha1("hello world")
print("SHA-1: " .. sha1)

local sha256 = ffx_crypto_utils.calculate_sha256("hello world")
print("SHA-256: " .. sha256)

--// Base64 encoding/decoding.
local encoded = ffx_crypto_utils.base64_encode("Hello, World!")
print("Base64: " .. encoded)  --// SGVsbG8sIFdvcmxkIQ==

local decoded = ffx_crypto_utils.base64_decode("SGVsbG8sIFdvcmxkIQ==")
print("Decoded: " .. decoded) --// Hello, World!

--// XOR encryption.
local encrypted = ffx_crypto_utils.xor_encode("secret data", "mykey")
print("XOR encoded: " .. encrypted)
--// To decrypt, apply XOR again using the same key (symmetric).
local decrypted = ffx_crypto_utils.xor_encode(encrypted, "mykey")
print("Decrypted: " .. decrypted)
```
