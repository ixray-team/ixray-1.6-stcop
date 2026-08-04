# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### ffx_crypto_utils: `\gamedata\scripts\ixr_framework\utils\ffx_crypto_utils.script`
Утилиты для криптографических операций (хэширование, кодирование, шифрование):
* `calculate_crc64`
* `calculate_sha1`
* `calculate_sha256`
* `base64_encode`
* `base64_decode`
* `xor_encode`

---

#### Описание методов:

```lua
--// Вычислить CRC64-хэш от входной строки.
calculate_crc64(input)
args:
  input (string) - строка для вычисления хэша
retval: (string) - CRC64-хэш в виде шестнадцатеричной строки

--// Вычислить SHA-1 хэш от входной строки.
calculate_sha1(input)
args:
  input (string) - строка для вычисления хэша
retval: (string) - SHA-1 хэш в виде шестнадцатеричной строки

--// Вычислить SHA-256 хэш от входной строки.
calculate_sha256(input)
args:
  input (string) - строка для вычисления хэша
retval: (string) - SHA-256 хэш в виде шестнадцатеричной строки

--// Закодировать строку в формат Base64.
base64_encode(input)
args:
  input (string) - строка для кодирования
retval: (string) - строка в кодировке Base64

--// Декодировать строку из формата Base64.
base64_decode(input)
args:
  input (string) - строка в Base64 для декодирования
retval: (string) - декодированная исходная строка

--// Закодировать строку с помощью XOR-шифрования с указанным ключом.
xor_encode(input, key)
args:
  input (string) - строка для кодирования
  key (string) - ключ шифрования (XOR)
retval: (string) - закодированная строка
```

### Примеры использований:
```lua
--// Вычисление хэшей
local crc = ffx_crypto_utils.calculate_crc64("hello world")
print("CRC64: " .. crc)

local sha1 = ffx_crypto_utils.calculate_sha1("hello world")
print("SHA-1: " .. sha1)

local sha256 = ffx_crypto_utils.calculate_sha256("hello world")
print("SHA-256: " .. sha256)

--// Base64 кодирование/декодирование
local encoded = ffx_crypto_utils.base64_encode("Hello, World!")
print("Base64: " .. encoded)  --// SGVsbG8sIFdvcmxkIQ==

local decoded = ffx_crypto_utils.base64_decode("SGVsbG8sIFdvcmxkIQ==")
print("Decoded: " .. decoded) --// Hello, World!

--// XOR шифрование
local encrypted = ffx_crypto_utils.xor_encode("secret data", "mykey")
print("XOR encoded: " .. encrypted)
--// Для расшифровки используйте тот же XOR с тем же ключом (симметричный)
local decrypted = ffx_crypto_utils.xor_encode(encrypted, "mykey")
print("Decrypted: " .. decrypted)
```
