#pragma once

enum ScreenPostProcessType
{
	Vignette = 0,
	Aberration,
	Saturation,
	Raindrops,
	GasMask,
	// Reuse the unoccupied SPP glass slot; shaders have six elements.
	ProceduralRaindrops = GasMask
};

enum class EffectSQ
{
	FXAA,
	CAS,
	NVG,
	Aberration,
	Raindrops,
	Gasmask,
};
