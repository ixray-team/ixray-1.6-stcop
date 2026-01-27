#pragma once
#include "xrFace.h"

class ImplicitDeflector;

class ImplicitCalcGlobs
{
 	ImplicitDeflector* defl;

public:
	ImplicitCalcGlobs() : defl(0) 
	{
	}
	 
	vecFace& query(float px, float py);

	IC ImplicitDeflector& DATA()
	{
		R_ASSERT(defl);
		return *defl;
	}

	// void Allocate();
	// void Deallocate();
	void Initialize(ImplicitDeflector& d);
};