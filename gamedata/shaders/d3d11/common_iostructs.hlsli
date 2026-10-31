#ifndef common_iostructs_h_included
#define common_iostructs_h_included

#include "shared\vertex.hlsli"

////////////////////////////////////////////////////////////////
//	This file contains io structs:
//	v_name	:	input for vertex shader.
//	v2p_name:	output for vertex shader.
//	p_name	:	input for pixel shader.
////////////////////////////////////////////////////////////////

struct v2p_TL0uv
{
    float4 Color : COLOR;
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

struct v2p_TL
{
    float2 Tex0 : TEXCOORD0;
    float4 Color : COLOR;
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

struct p_TL
{
    float2 Tex0 : TEXCOORD0;
    float4 Color : COLOR;
    float4 	HPos	: SV_POSITION;	// Clip-space position 	(for rasterization)
};

////////////////////////////////////////////////////////////////
struct v2p_postpr
{
    float2 Tex0 : TEXCOORD0; // base1 (duality)
    float2 Tex1 : TEXCOORD1; // base2 (duality)
    float2 Tex2 : TEXCOORD2; // base  (noise)
    float4 Color : COLOR0; // multiplier, color.w = noise_amount
    float4 Gray : COLOR1; // (.3,.3,.3.,amount)
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

struct p_postpr
{
    float2 Tex0 : TEXCOORD0; // base1 (duality)
    float2 Tex1 : TEXCOORD1; // base2 (duality)
    float2 Tex2 : TEXCOORD2; // base  (noise)
    float4 Color : COLOR0; // multiplier, color.w = noise_amount
    float4 Gray : COLOR1; // (.3,.3,.3.,amount)
    //	float4 	HPos	: SV_POSITION;	// Clip-space position 	(for rasterization)
};
////////////////////////////////////////////////////////////////
//	build	(bloom_build)
struct v_build
{
    float4 P : POSITIONT;
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float2 Tex2 : TEXCOORD2;
    float2 Tex3 : TEXCOORD3;
};

struct v2p_build
{
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float2 Tex2 : TEXCOORD2;
    float2 Tex3 : TEXCOORD3;
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

struct p_build
{
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float2 Tex2 : TEXCOORD2;
    float2 Tex3 : TEXCOORD3;
    //	float4 	HPos	: SV_POSITION;	// Clip-space position 	(for rasterization)
};
////////////////////////////////////////////////////////////////
//	filter	(bloom_filter)
struct v_filter
{
    float4 P : POSITIONT;
    float4 Tex0 : TEXCOORD0;
    float4 Tex1 : TEXCOORD1;
    float4 Tex2 : TEXCOORD2;
    float4 Tex3 : TEXCOORD3;
    float4 Tex4 : TEXCOORD4;
    float4 Tex5 : TEXCOORD5;
    float4 Tex6 : TEXCOORD6;
    float4 Tex7 : TEXCOORD7;
};

struct v2p_filter
{
    float4 Tex0 : TEXCOORD0;
    float4 Tex1 : TEXCOORD1;
    float4 Tex2 : TEXCOORD2;
    float4 Tex3 : TEXCOORD3;
    float4 Tex4 : TEXCOORD4;
    float4 Tex5 : TEXCOORD5;
    float4 Tex6 : TEXCOORD6;
    float4 Tex7 : TEXCOORD7;
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

struct p_filter
{
    float4 Tex0 : TEXCOORD0;
    float4 Tex1 : TEXCOORD1;
    float4 Tex2 : TEXCOORD2;
    float4 Tex3 : TEXCOORD3;
    float4 Tex4 : TEXCOORD4;
    float4 Tex5 : TEXCOORD5;
    float4 Tex6 : TEXCOORD6;
    float4 Tex7 : TEXCOORD7;
    //	float4 	HPos	: SV_POSITION;	// Clip-space position 	(for rasterization)
};

////////////////////////////////////////////////////////////////
struct v2p_dumb
{
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

////////////////////////////////////////////////////////////////
//	Volume
struct v2p_volume
{
    float4 tc : TEXCOORD0;
    float4 hpos : SV_POSITION; // Clip-space position 	(for rasterization)
};
struct p_volume
{
    float4 tc : TEXCOORD0;
    //	float4 	hpos	:SV_POSITION;	// Clip-space position 	(for rasterization)
};
////////////////////////////////////////////////////////////////
//	Static
struct v_static
{
    float4 Nh : NORMAL; // (nx,ny,nz,hemi occlusion)
    float4 T : TANGENT; // tangent
    float4 B : BINORMAL; // binormal
    int2 tc : TEXCOORD0; // (u,v)
#ifdef USE_LM_HEMI
    int2 lmh : TEXCOORD1; // (lmu,lmv)
#endif
    float4 P : POSITION; // (float,float,float,1)
};

struct gbuffer_data
{
    float3 P;
    float3 P_hud;
    float3 P_real;

    float depth;

    float mtl;
    float3 N;
    float hemi;
    float3 C;
    float gloss;

    float sss;
};

////////////////////////////////////////////////////////////////
//	Defer bumped
struct p_bumped_new
{
    float4 hpos : SV_POSITION;

    float4 tcdh : TEXCOORD0; // Texture coordinates, sun_occlusion || lm-hemi
    float4 position : TEXCOORD1; // position + hemi
    float3 M1 : TEXCOORD2; // nmap 2 eye - 1
    float3 M2 : TEXCOORD3; // nmap 2 eye - 2
    float3 M3 : TEXCOORD4; // nmap 2 eye - 3
	
#ifdef USE_LENGTH_BUFFER
	#ifndef USE_LM_HEMI
		float3 lmap : TEXCOORD5;
	#endif
#else
	#ifndef DISABLE_MOTION_VECTORS
		float4 hpos_curr : TEXCOORD5;
		float4 hpos_old : TEXCOORD6;
	#endif
#endif

#ifdef USE_HUD_RAINDROPS
    float4 drops_basis : TEXCOORD7;
#endif
};

struct p_bilbord
{
    float4 hpos : SV_POSITION;
    float4 af : COLOR1;

    float3 position : TEXCOORD0;
    float2 tc0 : TEXCOORD1;
    float2 tc1 : TEXCOORD2;

#ifndef DISABLE_MOTION_VECTORS
    float4 hpos_curr : TEXCOORD3;
    float4 hpos_old : TEXCOORD4;
#endif
};

struct p_shadow
{
    float2 tc0 : TEXCOORD0;
    float4 hpos : SV_POSITION;
};

struct v2p_screen
{
    float2 tc0 : TEXCOORD0;
    float4 HPos : POSITIONT; // Clip-space position (for rasterization)
};

//	Model
struct v_model
{
    float4 P : POSITION; // (float,float,float,1)
    float3 N : NORMAL; // (nx,ny,nz)
    float3 T : TANGENT; // (nx,ny,nz)
    float3 B : BINORMAL; // (nx,ny,nz)
    float2 tc : TEXCOORD0; // (u,v)
	
#ifndef DISABLE_MOTION_VECTORS
    float4 P_old : TEXCOORD1; // (float,float,float,1)
#endif
};

//	Fullscreen triangle
struct PSInputFullscreen
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

struct VSInputFullscreen
{
    float2 texcoord : TEXCOORD0;
    float4 hpos : POSITIONT;
};

struct VSOutputFullscreen
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0;
};

#endif //	common_iostructs_h_included
