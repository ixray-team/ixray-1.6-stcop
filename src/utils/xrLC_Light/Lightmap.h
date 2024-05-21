// Lightmap.h: interface for the CLightmap class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "lm_layer.h"

// refs
class CDeflector;

// def
class XRLC_LIGHT_API CLightmap  
{
public:
	lm_layer					lm;
	b_texture					lm_texture;
public:
	CLightmap					();
	~CLightmap					();
 
	void	Capture				( CDeflector *D, int b_u, int b_v, int s_u, int s_v, BOOL bRotate );
	void	Save				( LPCSTR path );
 
};