#ifndef TSS_DEF_H
#define TSS_DEF_H

#pragma once
#include "RenderAPI.h"

class RENDER_API SimulatorStates
{
private:
	struct State
	{
		u32	type = u32(-1);		// 0=RS
		u32	v1 = u32(-1), v2 = u32(-1), v3 = u32(-1);
	};
private:
	xr_vector<State>		States;
public:
	void					set_RS	(u32 a, u32 b, u32 c);
	bool					equal	(SimulatorStates& S);
	void					clear	();
	void	UpdateState( GfxPipelineState &state) const;
	void	UpdateDesc( RHIRasterizerDesc &desc ) const;
	void	UpdateDesc( RHIDepthStencilDesc &desc ) const;
	void	UpdateDesc( RHIBlendDesc &desc ) const;
};
#endif
