#pragma once
// GPU Side

enum ColorsReturnType
{
	eImplicit,
	eDeflectors,
	eMumodel,
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
struct RayTask
{
	Fvector wP, wN;
	float Range;
	Face* Skip;
	float attention;
	DeflectorLType type;
	base_color_c* Cptr;
};

struct JiterPixel
{
	u32 V, U;
	Fvector wP, wN; Face* skip;
	base_color_c C;

	void SetDataRays(u32 tV, u32 tU, Fvector& P, Fvector& N, Face* F)
	{
		U = tU;
		V = tV;
		C.clear_color();

		wP = P;
		wN = N;
		skip = F;
	};
};