#pragma once
#include "StdAfx.h"
#include "pch_script.h"
#include "Crypto_script.h"



CFFxCrypto::CFFxCrypto() {}
CFFxCrypto::~CFFxCrypto() {}

const char* CFFxCrypto::CRC64(const char* input)
{
    if (!input)
    {
        return null_hex;
    }

    uint64_t crc = 0xFFFFFFFFFFFFFFFF;
    const unsigned char* bytes = reinterpret_cast<const unsigned char*>(input);

    for (size_t i = 0; i < strlen(input); ++i)
    {
        crc = CRC64_MASK[(crc ^ bytes[i]) & 0xFF] ^ (crc >> 8);
    }

    crc = ~crc;
    static char result[17];

    for (int i = 0; i < 16; i++)
    {
        result[i] = hex[(crc >> (60 - i * 4)) & 0xF];
    }

    result[16] = '\0';

    return result;
}

const char* CFFxCrypto::SHA256(const char* input)
{
    if (!input)
    {
        return "";
    }

    static char result[65];
    uint32_t h[8];
    memcpy(h, SHA256_H0, sizeof(SHA256_H0));
    unsigned char buffer[64];
    size_t bufferLen = 0;
    uint64_t bitCount = 0;
    size_t len = strlen(input);
    const unsigned char* data = reinterpret_cast<const unsigned char*>(input);

    for (size_t i = 0; i < len; i++)
    {
        buffer[bufferLen++] = data[i];

        if (bufferLen == 64)
        {
            uint32_t w[64];

            for (int j = 0; j < 16; j++)
            {
                w[j] = (buffer[j * 4] << 24) | (buffer[j * 4 + 1] << 16) | (buffer[j * 4 + 2] << 8) | buffer[j * 4 + 3];
            }

            for (int j = 16; j < 64; j++)
            {
                uint32_t s0 = ((w[j - 15] >> 7) | (w[j - 15] << 25)) ^ ((w[j - 15] >> 18) | (w[j - 15] << 14)) ^ (w[j - 15] >> 3);
                uint32_t s1 = ((w[j - 2] >> 17) | (w[j - 2] << 15)) ^ ((w[j - 2] >> 19) | (w[j - 2] << 13)) ^ (w[j - 2] >> 10);
                w[j] = s1 + w[j - 7] + s0 + w[j - 16];
            }

            uint32_t a = h[0], b = h[1], c = h[2], d = h[3];
            uint32_t e = h[4], f = h[5], g = h[6], hh = h[7];

            for (int j = 0; j < 64; j++)
            {
                uint32_t S1 = ((e >> 6) | (e << 26)) ^ ((e >> 11) | (e << 21)) ^ ((e >> 25) | (e << 7));
                uint32_t ch = (e & f) ^ (~e & g);
                uint32_t temp1 = hh + S1 + ch + SHA256_K[j] + w[j];
                uint32_t S0 = ((a >> 2) | (a << 30)) ^ ((a >> 13) | (a << 19)) ^ ((a >> 22) | (a << 10));
                uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
                uint32_t temp2 = S0 + maj;

                hh = g; g = f; f = e; e = d + temp1; d = c; c = b; b = a; a = temp1 + temp2;
            }

            h[0] += a; h[1] += b; h[2] += c; h[3] += d;
            h[4] += e; h[5] += f; h[6] += g; h[7] += hh;

            bitCount += 512;
            bufferLen = 0;
        }
    }

    buffer[bufferLen++] = 0x80;

    if (bufferLen > 56)
    {
        while (bufferLen < 64)
        {
            buffer[bufferLen++] = 0;
        }

        uint32_t w[64];
        for (int j = 0; j < 16; j++)
        {
            w[j] = (buffer[j * 4] << 24) | (buffer[j * 4 + 1] << 16) | (buffer[j * 4 + 2] << 8) | buffer[j * 4 + 3];
        }

        for (int j = 16; j < 64; j++)
        {
            uint32_t s0 = ((w[j - 15] >> 7) | (w[j - 15] << 25)) ^ ((w[j - 15] >> 18) | (w[j - 15] << 14)) ^ (w[j - 15] >> 3);
            uint32_t s1 = ((w[j - 2] >> 17) | (w[j - 2] << 15)) ^ ((w[j - 2] >> 19) | (w[j - 2] << 13)) ^ (w[j - 2] >> 10);
            w[j] = s1 + w[j - 7] + s0 + w[j - 16];
        }

        uint32_t a = h[0], b = h[1], c = h[2], d = h[3];
        uint32_t e = h[4], f = h[5], g = h[6], hh = h[7];

        for (int j = 0; j < 64; j++)
        {
            uint32_t S1 = ((e >> 6) | (e << 26)) ^ ((e >> 11) | (e << 21)) ^ ((e >> 25) | (e << 7));
            uint32_t ch = (e & f) ^ (~e & g);
            uint32_t temp1 = hh + S1 + ch + SHA256_K[j] + w[j];

            uint32_t S0 = ((a >> 2) | (a << 30)) ^ ((a >> 13) | (a << 19)) ^ ((a >> 22) | (a << 10));
            uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
            uint32_t temp2 = S0 + maj;

            hh = g;  g = f; f = e; e = d + temp1; d = c; c = b; b = a; a = temp1 + temp2;
        }

        h[0] += a; h[1] += b; h[2] += c; h[3] += d;
        h[4] += e; h[5] += f; h[6] += g; h[7] += hh;

        bufferLen = 0;
    }

    while (bufferLen < 56)
    {
        buffer[bufferLen++] = 0;
    }

    uint64_t totalBits = bitCount + (bufferLen - 1) * 8;
    for (int i = 0; i < 8; i++)
    {
        buffer[56 + i] = (totalBits >> (56 - i * 8)) & 0xFF;
    }

    uint32_t w[64];
    for (int j = 0; j < 16; j++)
    {
        w[j] = (buffer[j * 4] << 24) | (buffer[j * 4 + 1] << 16) | (buffer[j * 4 + 2] << 8) | buffer[j * 4 + 3];
    }

    for (int j = 16; j < 64; j++)
    {
        uint32_t s0 = ((w[j - 15] >> 7) | (w[j - 15] << 25)) ^ ((w[j - 15] >> 18) | (w[j - 15] << 14)) ^ (w[j - 15] >> 3);
        uint32_t s1 = ((w[j - 2] >> 17) | (w[j - 2] << 15)) ^ ((w[j - 2] >> 19) | (w[j - 2] << 13)) ^ (w[j - 2] >> 10);
        w[j] = s1 + w[j - 7] + s0 + w[j - 16];
    }

    uint32_t a = h[0], b = h[1], c = h[2], d = h[3];
    uint32_t e = h[4], f = h[5], g = h[6], hh = h[7];

    for (int j = 0; j < 64; j++)
    {
        uint32_t S1 = ((e >> 6) | (e << 26)) ^ ((e >> 11) | (e << 21)) ^ ((e >> 25) | (e << 7));
        uint32_t ch = (e & f) ^ (~e & g);
        uint32_t temp1 = hh + S1 + ch + SHA256_K[j] + w[j];
        uint32_t S0 = ((a >> 2) | (a << 30)) ^ ((a >> 13) | (a << 19)) ^ ((a >> 22) | (a << 10));
        uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
        uint32_t temp2 = S0 + maj;

        hh = g;  g = f; f = e;  e = d + temp1; d = c; c = b; b = a; a = temp1 + temp2;
    }

    h[0] += a; h[1] += b; h[2] += c; h[3] += d;
    h[4] += e; h[5] += f; h[6] += g; h[7] += hh;

    unsigned char hash[32];
    for (int i = 0; i < 8; i++)
    {
        hash[i * 4] = (h[i] >> 24) & 0xFF;
        hash[i * 4 + 1] = (h[i] >> 16) & 0xFF;
        hash[i * 4 + 2] = (h[i] >> 8) & 0xFF;
        hash[i * 4 + 3] = h[i] & 0xFF;
    }

    for (int i = 0; i < 32; i++)
    {
        result[i * 2] = hex[hash[i] >> 4];
        result[i * 2 + 1] = hex[hash[i] & 0x0F];
    }

    result[64] = '\0';

    return result;
}

const char* CFFxCrypto::SHA1(const char* input)
{
    if (!input)
    {
        return "";
    }

    static char result[41];

    uint32_t h[5];
    memcpy(h, SHA1_H0, sizeof(SHA1_H0));

    unsigned char buffer[64];
    size_t bufferLen = 0;
    uint64_t bitCount = 0;
    size_t len = strlen(input);
    const unsigned char* data = reinterpret_cast<const unsigned char*>(input);

    for (size_t i = 0; i < len; i++)
    {
        buffer[bufferLen++] = data[i];

        if (bufferLen == 64)
        {
            uint32_t w[80];

            for (int j = 0; j < 16; j++)
            {
                w[j] = (buffer[j * 4] << 24) | (buffer[j * 4 + 1] << 16) |  (buffer[j * 4 + 2] << 8) | buffer[j * 4 + 3];
            }

            for (int j = 16; j < 80; j++)
            {
                w[j] = ((w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16]) << 1) |  ((w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16]) >> 31);
            }

            uint32_t a = h[0], b = h[1], c = h[2], d = h[3], e = h[4];

            for (int j = 0; j < 80; j++)
            {
                uint32_t f;

                if (j < 20)
                {
                    f = (b & c) | ((~b) & d);
                }
                else if (j < 40)
                {
                    f = b ^ c ^ d;
                }
                else if (j < 60)
                {
                    f = (b & c) | (b & d) | (c & d);
                }
                else
                {
                    f = b ^ c ^ d;
                }

                uint32_t temp = ((a << 5) | (a >> 27)) + f + e + SHA1_K[j / 20] + w[j];
                e = d;
                d = c;
                c = (b << 30) | (b >> 2);
                b = a;
                a = temp;
            }

            h[0] += a;
            h[1] += b;
            h[2] += c;
            h[3] += d;
            h[4] += e;

            bitCount += 512;
            bufferLen = 0;
        }
    }

    buffer[bufferLen++] = 0x80;

    if (bufferLen > 56)
    {
        while (bufferLen < 64)
        {
            buffer[bufferLen++] = 0;
        }

        uint32_t w[80];
        for (int j = 0; j < 16; j++)
        {
            w[j] = (buffer[j * 4] << 24) | (buffer[j * 4 + 1] << 16) |  (buffer[j * 4 + 2] << 8) | buffer[j * 4 + 3];
        }

        for (int j = 16; j < 80; j++)
        {
            w[j] = ((w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16]) << 1) |  ((w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16]) >> 31);
        }

        uint32_t a = h[0], b = h[1], c = h[2], d = h[3], e = h[4];

        for (int j = 0; j < 80; j++)
        {
            uint32_t f;

            if (j < 20)
            {
                f = (b & c) | ((~b) & d);
            }
            else if (j < 40)
            {
                f = b ^ c ^ d;
            }
            else if (j < 60)
            {
                f = (b & c) | (b & d) | (c & d);
            }
            else
            {
                f = b ^ c ^ d;
            }

            uint32_t temp = ((a << 5) | (a >> 27)) + f + e + SHA1_K[j / 20] + w[j];
            e = d;
            d = c;
            c = (b << 30) | (b >> 2);
            b = a;
            a = temp;
        }

        h[0] += a;
        h[1] += b;
        h[2] += c;
        h[3] += d;
        h[4] += e;

        bufferLen = 0;
    }

    while (bufferLen < 56)
    {
        buffer[bufferLen++] = 0;
    }

    uint64_t totalBits = bitCount + (bufferLen - 1) * 8;
    for (int i = 0; i < 8; i++)
    {
        buffer[56 + i] = (totalBits >> (56 - i * 8)) & 0xFF;
    }

    uint32_t w[80];
    for (int j = 0; j < 16; j++)
    {
        w[j] = (buffer[j * 4] << 24) | (buffer[j * 4 + 1] << 16) |  (buffer[j * 4 + 2] << 8) | buffer[j * 4 + 3];
    }

    for (int j = 16; j < 80; j++)
    {
        w[j] = ((w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16]) << 1) | ((w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16]) >> 31);
    }

    uint32_t a = h[0], b = h[1], c = h[2], d = h[3], e = h[4];

    for (int j = 0; j < 80; j++)
    {
        uint32_t f;

        if (j < 20)
        {
            f = (b & c) | ((~b) & d);
        }
        else if (j < 40)
        {
            f = b ^ c ^ d;
        }
        else if (j < 60)
        {
            f = (b & c) | (b & d) | (c & d);
        }
        else
        {
            f = b ^ c ^ d;
        }

        uint32_t temp = ((a << 5) | (a >> 27)) + f + e + SHA1_K[j / 20] + w[j];
        e = d;
        d = c;
        c = (b << 30) | (b >> 2);
        b = a;
        a = temp;
    }

    h[0] += a;
    h[1] += b;
    h[2] += c;
    h[3] += d;
    h[4] += e;

    unsigned char hash[20];
    for (int i = 0; i < 5; i++)
    {
        hash[i * 4] = (h[i] >> 24) & 0xFF;
        hash[i * 4 + 1] = (h[i] >> 16) & 0xFF;
        hash[i * 4 + 2] = (h[i] >> 8) & 0xFF;
        hash[i * 4 + 3] = h[i] & 0xFF;
    }

    for (int i = 0; i < 20; i++)
    {
        result[i * 2] = hex[hash[i] >> 4];
        result[i * 2 + 1] = hex[hash[i] & 0x0F];
    }

    result[40] = '\0';

    return result;
}

const char* CFFxCrypto::Base64Encode(const char* input)
{
    if (!input)
    {
        return "";
    }

    static std::string result;
    result.clear();

    size_t len = strlen(input);
    const unsigned char* bytes = reinterpret_cast<const unsigned char*>(input);

    for (size_t i = 0; i < len; i += 3)
    {
        unsigned char b1 = bytes[i];
        unsigned char b2 = (i + 1 < len) ? bytes[i + 1] : 0;
        unsigned char b3 = (i + 2 < len) ? bytes[i + 2] : 0;

        unsigned char b4 = b1 >> 2;
        unsigned char b5 = ((b1 & 0x03) << 4) | (b2 >> 4);
        unsigned char b6 = ((b2 & 0x0F) << 2) | (b3 >> 6);
        unsigned char b7 = b3 & 0x3F;

        result += base64_chars[b4];
        result += base64_chars[b5];

        if (i + 1 < len)
            result += base64_chars[b6];
        else
            result += '=';

        if (i + 2 < len)
            result += base64_chars[b7];
        else
            result += '=';
    }

    return result.c_str();
}

const char* CFFxCrypto::Base64Decode(const char* input)
{
    if (!input)
    {
        return "";
    }

    static std::string result;
    result.clear();

    size_t len = strlen(input);
    if (len % 4 != 0)
    {
        return "";
    }

    unsigned char lookup[256];
    memset(lookup, 0xFF, sizeof(lookup));
    for (int i = 0; i < 64; i++)
    {
        lookup[(unsigned char)base64_chars[i]] = i;
    }

    for (size_t i = 0; i < len; i += 4)
    {
        unsigned char c1 = lookup[(unsigned char)input[i]];
        unsigned char c2 = lookup[(unsigned char)input[i + 1]];
        unsigned char c3 = lookup[(unsigned char)input[i + 2]];
        unsigned char c4 = lookup[(unsigned char)input[i + 3]];

        if (c1 == 0xFF || c2 == 0xFF || (c3 == 0xFF && input[i + 2] != '=') || (c4 == 0xFF && input[i + 3] != '='))
        {
            return "";
        }

        unsigned char b1 = (c1 << 2) | (c2 >> 4);
        result += static_cast<char>(b1);

        if (input[i + 2] != '=')
        {
            unsigned char b2 = ((c2 & 0x0F) << 4) | (c3 >> 2);
            result += static_cast<char>(b2);
        }

        if (input[i + 3] != '=')
        {
            unsigned char b3 = ((c3 & 0x03) << 6) | c4;
            result += static_cast<char>(b3);
        }
    }

    return result.c_str();
}

const char* CFFxCrypto::XorEncode(const char* input, const char* key)
{
    if (!input || !key) return "";

    static std::string result;
    result.clear();

    size_t len = strlen(input);
    size_t keyLen = strlen(key);

    for (size_t i = 0; i < len; i++)
    {
        result += static_cast<char>(input[i] ^ key[i % keyLen]);
    }

    return result.c_str();
}

using namespace luabind;
#pragma optimize("s",on)
void CFFxCrypto::script_register(lua_State* L)
{
    module(L)
        [
            class_<CFFxCrypto>("FFxCrypto")
                .def(constructor<>())
                .def("crc64", &CFFxCrypto::CRC64)
                .def("sha1", &CFFxCrypto::SHA1)
                .def("sha256", &CFFxCrypto::SHA256)
                .def("base64_encode", &CFFxCrypto::Base64Encode)
                .def("base64_decode", &CFFxCrypto::Base64Decode)
                .def("xor_encode", &CFFxCrypto::XorEncode)
        ];
}