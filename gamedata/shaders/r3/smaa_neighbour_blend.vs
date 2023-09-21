#include "common.h"
//////////////////////////////////////////////////////////////////////////////////////////
#if defined(SM_5) || defined(SM_4_1)
    #define SMAA_HLSL_4_1
#else
    #define SMAA_HLSL_4
#endif

#define SMAA_INCLUDE_VS 1
uniform float4 screen_res;
#define SMAA_RT_METRICS screen_res.zwxy
//////////////////////////////////////////////////////////////////////////////////////////

#include "smaa.h"

//Struct
struct p_smaa
{
	float4 hpos			: SV_Position;
	float2 tc0			: TEXCOORD0;        // Texture coordinates         (for sampling maps)
	float4 offset		: TEXCOORD1;
};

struct	v2p_smaa
{
	float2 tc0 : TEXCOORD0;
	float4 HPos : POSITIONT;  	// Clip-space position 	(for rasterization)
};

//////////////////////////////////////////////////////////////////////////////////////////
// Vertex
p_smaa main(v2p_smaa I)
{
	p_smaa O;
	// Transform to screen space (in d3d9 it was done automatically)
	O.hpos.x 	=  (I.HPos.x * screen_res.z * 2 - 1);
	O.hpos.y 	= -(I.HPos.y * screen_res.w * 2 - 1);
	O.hpos.zw 	=	I.HPos.zw;
	
    O.tc0 		= 	I.tc0;

	SMAANeighborhoodBlendingVS(I.tc0, O.offset);

    return O; 
}
