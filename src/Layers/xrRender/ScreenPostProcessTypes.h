#pragma once

enum ScreenPostProcessType
{
	Vignette = 0,
	Aberration,
	Saturation,
	Raindrops,
	GasMask,
	Winter,
	// Reuse the unoccupied SPP glass slot; shaders have six elements.
	ProceduralRaindrops = GasMask
};