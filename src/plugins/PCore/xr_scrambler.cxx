#include <algorithm>
#include "xr_scrambler.h"

using namespace xray_re;

const int SEED_MULT = 0x8088405;

const int SEED_RU = 0x131a9d3;
const int SEED0_RU = 0x1329436;
const int SIZE_MULT_RU = 8;

const int SEED_WW = 0x16eb2eb;
const int SEED0_WW = 0x5bbc4b;
const int SIZE_MULT_WW = 4;

void xr_scrambler::init(cipher_config cc)
{
	if (cc == CC_RU) {
		m_seed = SEED_RU;
		init_sboxes(SEED0_RU, SIZE_MULT_RU);
	} else if (cc == CC_WW) {
		m_seed = SEED_WW;
		init_sboxes(SEED0_WW, SIZE_MULT_WW);
	}
}

void xr_scrambler::init_sboxes(int seed, int size_mult)
{
	for (int i = SBOX_SIZE - 1; i >= 0; --i)
		m_enc_sbox[i] = uint8_t(i);
	for (int i = size_mult*SBOX_SIZE, a, b; i != 0; --i) {
		seed = 1 + seed * SEED_MULT;
		a = (seed >> 24) & 0xff;
		do {
			seed = 1 + seed * SEED_MULT;
			b = (seed >> 24) & 0xff;
		} while (a == b);
		std::swap(m_enc_sbox[a], m_enc_sbox[b]);
	}
	for (int i = 0; i != SBOX_SIZE; ++i)
		m_dec_sbox[m_enc_sbox[i]] = uint8_t(i);
}

void xr_scrambler::decrypt(uint8_t* dest, const uint8_t* src, size_t size) const
{
	int seed = m_seed;
	for (size_t i = 0; i != size; ++i) {
		seed = 1 + seed * SEED_MULT;
		dest[i] = m_dec_sbox[src[i] ^ ((seed >> 24) & 0xff)];
	}
}

void xr_scrambler::encrypt(uint8_t* dest, const uint8_t* src, size_t size) const
{
	int seed = m_seed;
	for (size_t i = 0; i != size; ++i) {
		seed = 1 + seed * SEED_MULT;
		dest[i] = uint8_t(m_enc_sbox[src[i]] ^ ((seed >> 24) & 0xff));
	}
}
