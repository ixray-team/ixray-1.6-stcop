#pragma once
#include "xrFace.h"
#include "hash2D.h"

class ImplicitDeflector;
typedef hash2D <Face*, 384, 384> IHASH;

class ImplicitCalcGlobs
{
	IHASH* ImplicitHash;
	ImplicitDeflector* defl;

public:
	ImplicitCalcGlobs() :
		defl(0), ImplicitHash(0)
	{
	}

	IC IHASH& Hash()
	{
		R_ASSERT(ImplicitHash);
		return *ImplicitHash;
	}

	IC ImplicitDeflector& DATA()
	{
		R_ASSERT(defl);
		return *defl;
	}

	void Allocate();
	void Deallocate();
	void Initialize(ImplicitDeflector& d);
};