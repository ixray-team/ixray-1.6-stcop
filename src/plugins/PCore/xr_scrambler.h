#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCRAMBLER_H__
#define __XR_SCRAMBLER_H__

#include "xr_types.h"

namespace xray_re {

class xr_scrambler {
public:
	enum cipher_config {
		CC_RU,
		CC_WW,
	};

			xr_scrambler();
			xr_scrambler(cipher_config cc);

	void		init(cipher_config cc);
	void		encrypt(uint8_t* dest, const uint8_t* src, size_t size) const;
	void		decrypt(uint8_t* dest, const uint8_t* src, size_t size) const;

private:
	enum {
		SBOX_SIZE	= 256,
	};

	void		init_sboxes(int seed0, int size_mult);

private:
	int		m_seed;
	uint8_t		m_enc_sbox[SBOX_SIZE];
	uint8_t		m_dec_sbox[SBOX_SIZE];
};

inline xr_scrambler::xr_scrambler() {}
inline xr_scrambler::xr_scrambler(cipher_config cc) { init(cc); }

} // end of namespace xray_re

#endif
