// Lightmap.h: interface for the CLightmap class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LIGHTMAP_H__889100E6_CF29_47EA_ABFD_41AE28DAC6B1__INCLUDED_)
#define AFX_LIGHTMAP_H__889100E6_CF29_47EA_ABFD_41AE28DAC6B1__INCLUDED_
#pragma once

#include "lm_layer.h"
#include "serialize.h"
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
  
#endif // !defined(AFX_LIGHTMAP_H__889100E6_CF29_47EA_ABFD_41AE28DAC6B1__INCLUDED_)
