#pragma once
#include "StdAfx.h"
#include "pch_script.h"
#include "Crypto_script.h"



CFFxCrypto::CFFxCrypto() {}
CFFxCrypto::~CFFxCrypto() {}

LPCSTR CFFxCrypto::CRC64(LPCSTR input)
{
    if (!input)
    {
        return null_hex;
    }

    uint64_t crc = 0xFFFFFFFFFFFFFFFF;
    const unsigned char* bytes = reinterpret_cast<const unsigned char*>(input);

    for (size_t i = 0; i < strlen(input); ++i)
    {
        crc = CRC64_TABLE[(crc ^ bytes[i]) & 0xFF] ^ (crc >> 8);
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

LPCSTR CFFxCrypto::SHA256(LPCSTR input)
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

using namespace luabind;
#pragma optimize("s",on)
void CFFxCrypto::script_register(lua_State* L)
{
    module(L)
        [
            class_<CFFxCrypto>("FFxCrypto")
                .def(constructor<>())
                .def("crc64", &CFFxCrypto::CRC64)
                .def("sha256", &CFFxCrypto::SHA256)
        ];
}