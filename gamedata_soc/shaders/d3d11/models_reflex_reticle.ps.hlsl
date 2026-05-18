/*
	=====================================================================
	Addon      : Parallax Reflex Sights
	Link       : https://www.moddb.com/mods/stalker-anomaly/addons/parallax-reflex-sights
	Authors    : LVutner, party_50
	Date       : 06.02.2024
	Last Edit  : 12.05.2025
	=====================================================================
*/

#include "common.hlsli"
#include "mark_adjust.hlsli"

#define OFFSET 0.0039
#define PARALLAX 1000.0
#define SCALE 160.0

struct vf
{
	float4 hpos : SV_Position;
    float2 tc0 : TEXCOORD0;
    float3 P : TEXCOORD1;
	float3 T : TEXCOORD3;
	float3 B : TEXCOORD4;
	float3 N : TEXCOORD5;
};

float4 main(vf I): SV_Target
{
	float3x3 TBN = float3x3(I.T + OFFSET, I.B + OFFSET, I.N);
    float3 V_tangent = normalize(float3(dot(-I.P, TBN[0]), dot(-I.P, TBN[1]), dot(-I.P, TBN[2])));
    float2 parallax_tc = I.tc0 - V_tangent.xy * PARALLAX;
	
	parallax_tc = (parallax_tc + (SCALE - 1) / 2) / SCALE;
	
	#ifdef MARK_ADJUST
	parallax_tc = mark_adjust(parallax_tc);
	#endif

    return s_base.SampleLevel(smp_rtlinear, parallax_tc, 0.0);
}
