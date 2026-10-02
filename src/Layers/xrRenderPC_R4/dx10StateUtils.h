#ifndef	dx10StateUtils_included
#define	dx10StateUtils_included
#pragma once

namespace dx10StateUtils
{
	ERHI_COMPARISON			ConvertCmpFunction(D3DCMPFUNC Func);
	ERHI_STENCIL_OP				ConvertStencilOp(D3DSTENCILOP Op);
	ERHI_BLEND					ConvertBlendArg(D3DBLEND Arg);
	ERHI_BLEND_OP				ConvertBlendOp(D3DBLENDOP Op);
	ERHI_TEXTURE_ADDRESS_MODE	ConvertTextureAddressMode(D3DTEXTUREADDRESS Mode);

	//	Set description to default values
	void	ResetDescription( RHIRasterizerDesc &desc );
	void	ResetDescription( RHIDepthStencilDesc &desc );
	void	ResetDescription( RHIBlendDesc &desc );
	void	ResetDescription(RHISampleDesc&desc );

	//	State comparison (memcmp doesn't work due to padding bytes in structure)
	bool	operator==(const RHIRasterizerDesc &desc1, const RHIRasterizerDesc &desc2);
	bool	operator==(const RHIDepthStencilDesc &desc1, const RHIDepthStencilDesc &desc2);
	bool	operator==(const RHIBlendDesc &desc1, const RHIBlendDesc &desc2);

	//	Calculate hash values
	u32		GetHash( const RHIRasterizerDesc &desc );
	u32		GetHash( const RHIDepthStencilDesc &desc );
	u32		GetHash( const RHIBlendDesc &desc );
	u32		GetHash( const RHISampleDesc&desc );

	//	Modify state to meet DX10 automatic modifications
	void	ValidateState(RHIRasterizerDesc &desc);
	void	ValidateState(RHIDepthStencilDesc &desc);
	void	ValidateState(RHIBlendDesc &desc);
	void	ValidateState(RHISampleDesc&desc);
};

#endif	//	dx10StateUtils_included