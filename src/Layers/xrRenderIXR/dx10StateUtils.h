#ifndef	dx10StateUtils_included
#define	dx10StateUtils_included
#pragma once

namespace dx10StateUtils
{
	ERHI_COMPARISON			ConvertCmpFunction(D3DCMPFUNC Func);
	ERHI_STENCIL_OP				ConvertStencilOp(D3DSTENCILOP Op);
	ERHI_BLEND					ConvertBlendArg(D3DBLEND Arg);
	ERHI_BLEND_OP				ConvertBlendOp(D3DBLENDOP Op);

	//	Set description to default values
	void	ResetDescription( RHIRasterizerDesc &desc );
	void	ResetDescription( RHIDepthStencilDesc &desc );
	void	ResetDescription( RHIBlendDesc &desc );

	//	Modify state to meet DX10 automatic modifications
	void	ValidateState(RHIDepthStencilDesc &desc);
	void	ValidateState(RHIBlendDesc &desc);
};

#endif	//	dx10StateUtils_included