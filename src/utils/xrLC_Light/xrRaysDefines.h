#pragma once

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

struct RayRecvestCPU
{
 	size_t  INDEX_TASK;
	u8		Jitter = 0; 

	// Task Pos, Dir, Skip
	Fvector P;
	Fvector N;
	void* Skip;

	void SetupParrams(size_t taskID, u8 J, Fvector& Pnew, Fvector& Nnew, void* skip)
	{
		INDEX_TASK = taskID;
		Jitter = J;
 		P = Pnew;
		N = Nnew;
		Skip = skip;
 	}
};

#include "R_light.h"

struct RayRecvestLPInfo : RayRecvestCPU
{
 	void SetupParramsCopy(RayRecvestCPU& S)
	{
		INDEX_TASK	= S.INDEX_TASK;
		Jitter		= S.Jitter;
		P			= S.P;
		N			= S.N;
		Skip		= S.Skip;
	}

	// result
	u8		LType;
	u8		CType = 0;
	bool	isSunOrHemi;

	// lighting precomp
	float   sqD;
	float   D;
	float   TaskRange;
	R_Light Light;

	void*	TaskProcesor;
};

 