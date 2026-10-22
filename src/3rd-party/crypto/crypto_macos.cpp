// macOS implementation of the crypto layer.
// The bundled copies of OpenSSL are configured for Win32 and cannot be built
// here, so hashing goes through the system CommonCrypto and DSA, which has no
// available implementation, reports itself as unsupported.

#include "crypto.h"

#include <CommonCrypto/CommonDigest.h>

namespace crypto
{

struct MacShaContext
{
	CC_SHA1_CTX		m_ctx;
};

CRYPTO_API	void	xr_crypto_init	()
{
	// Nothing to initialise: CommonCrypto needs no seeding.
}

xr_sha256::xr_sha256	()
{
	m_sha_ctx			= reinterpret_cast<SHAstate_st*>(new MacShaContext());
}

xr_sha256::~xr_sha256	()
{
	xr_delete			(reinterpret_cast<MacShaContext*&>(m_sha_ctx));
}

void	xr_sha256::start_calculate	(u8 const * data, u32 data_size)
{
	MacShaContext* ctx	= reinterpret_cast<MacShaContext*>(m_sha_ctx);
	CC_SHA1_Init		(&ctx->m_ctx);
	ZeroMemory			(m_result, sizeof(m_result));
	VERIFY				(data_size);
	m_data_src			= data;
	m_data_size			= data_size;
}

bool	xr_sha256::continue_calculate	()
{
	u32 const to_calc	= m_data_size >= calc_chunk_size ? calc_chunk_size : m_data_size;
	MacShaContext* ctx	= reinterpret_cast<MacShaContext*>(m_sha_ctx);

	CC_SHA1_Update		(&ctx->m_ctx, m_data_src, to_calc);

	m_data_src			+= to_calc;
	m_data_size			-= to_calc;

	if (!m_data_size)
	{
		CC_SHA1_Final	(m_result, &ctx->m_ctx);
		return			true;
	}
	return				false;
}

xr_dsa::xr_dsa	(u8 const * /*p*/, u8 const * /*q*/, u8 const * /*g*/)
{
	m_dsa				= nullptr;
	Msg					("! crypto: DSA is not supported on macOS, signed data cannot be verified");
}

xr_dsa::~xr_dsa	()
{
}

shared_str const xr_dsa::sign	(private_key_t const & /*priv_key*/, u8 const * /*data*/, u32 const /*data_size*/)
{
	return				shared_str();
}

bool	xr_dsa::verify			(public_key_t const & /*pub_key*/, u8 const * /*data*/, u32 const /*data_size*/,
								 shared_str const & /*dsign*/)
{
	return				false;
}

} //namespace crypto
