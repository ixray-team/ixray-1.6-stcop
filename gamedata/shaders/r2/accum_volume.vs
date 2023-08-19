#include "common.h"

struct 	v2p
{
	float4 	hpos: 		POSITION;	// Clip-space position 	(for rasterization)
	float4 	tc:		TEXCOORD0;
};

//////////////////////////////////////////////////////////////////////////////////////////
//uniform float4x4	m_texgen;
uniform float4x4	m_texgen_J;

//////////////////////////////////////////////////////////////////////////////////////////
// Vertex
v2p 	main	( float4 P:	POSITION )
{
	v2p 		O;
	O.hpos 		= mul	(m_WVP,	  	P	);
	O.tc 		= mul	(m_texgen,	P	);
 	return	O;
}
FXVS;
