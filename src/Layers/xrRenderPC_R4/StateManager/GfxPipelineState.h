#pragma once

class SimulatorStates;

struct GfxPipelineState
{
	RHIRasterizerDesc Raster = {};
	RHIDepthStencilDesc Depth = {};
	RHIBlendDesc Blend = {};
	void* RasterState = nullptr;
	void* DepthState = nullptr;
	void* BlendState = nullptr;
	u32 StencilRef = u32(-1);
	u32 AlphaRef = 0;

	static GfxPipelineState* Create(SimulatorStates& Code);
	void Release();
};
