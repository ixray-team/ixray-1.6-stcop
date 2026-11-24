/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
#ifndef RAIN_INSTANCED_INCLUDED
#define RAIN_INSTANCED_INCLUDED
#include "common\common.hlsli"

#include "common\post_cb.hlsli"

struct RainVertex
{
	float4 HPos : SV_POSITION;
	float2 UV : TEXCOORD0;
	float3 Point : TEXCOORD1;
	nointerpolation float3 AxisX : TEXCOORD2;
	nointerpolation float3 AxisY : TEXCOORD3;
	nointerpolation float Fade : TEXCOORD4;
};

float RainRandom(uint Seed)
{
	Seed ^= Seed >> 16;
	Seed *= 0x7feb352du;
	Seed ^= Seed >> 15;
	Seed *= 0x846ca68bu;
	Seed ^= Seed >> 16;
	return float(Seed & 0x00ffffffu) / 16777216.0f;
}

RainVertex RainProject(float3 Point, float2 UV, float3 AxisX, float3 AxisY, float Fade)
{
	RainVertex O;
	O.HPos = mul(m_P, float4(Point, 1.0f));
	O.HPos.xy += m_taa_jitter.xy * O.HPos.w;
	O.UV = UV;
	O.Point = Point;
	O.AxisX = AxisX;
	O.AxisY = AxisY;
	O.Fade = Fade;
	return O;
}
#endif
