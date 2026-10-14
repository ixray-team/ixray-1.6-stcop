#pragma once

#include "xrCore/xrCore.h"

class CDetailWind
{
public:
	struct Constants
	{
		Fvector4 global;
		Fvector4 xz1;
		Fvector4 xz1_dir;
		Fvector4 xz2;
		Fvector4 xz2_dir;
		Fvector4 xz3;
		Fvector4 xz3_dir;
		Fvector4 swirl;
		Fvector4 swirl_dir;
	};

	static void		ComputeConstants(Constants& out);
	static void		FillPreview(u8* rgba, u32 size, float zoom, u32 timeMs);

	static float	Hash(float x, float y);
	static float	Noise(float x, float y);
};
