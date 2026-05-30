#pragma once
// GPU Side

enum ColorsReturnType
{
	eImplicit,
	eDeflectors,
	eMumodel,
	eDetails,
	eCommon
};

struct RayRecvestIndex
{
	void* Owner = 0;
	size_t  INDEX_TASK;

	// Task Pos, Dir, Skip
	Fvector P;
	Fvector N;
};
 
// CPU Side

enum DeflectorLType
{
	eDefSun,
	eDefHemi,
	eDefRgb
};

#include "base_color.h"

struct RayTask
{
	Fvector wP, wN;
	float Range;
	void* Skip;
	float attention;
	DeflectorLType type;
	base_color_c* Cptr;
};

struct JiterPixel
{
	u32 V, U;
	Fvector wP, wN; void* skip;
	base_color_c C;

	void SetDataRays(u32 tV, u32 tU, Fvector& P, Fvector& N, void* F)
	{
		U = tU;
		V = tV;
		C.clear_color();

		wP = P;
		wN = N;
		skip = F;
	};
};

struct DetailsTask
{
	u32 X, Z;
	Fvector wP, wN; 
	base_color_c C;
	float SRange;
	Fvector SPosition;

	void SetDataRays(u32 tX, u32 tZ, Fvector& P, Fvector& N, float SR, Fvector SPos)
	{
		X = tX;
		Z = tZ;
		C.clear_color();

		wP = P;
		wN = N;
		SRange = SR;
		SPosition = SPos;
 	};
};