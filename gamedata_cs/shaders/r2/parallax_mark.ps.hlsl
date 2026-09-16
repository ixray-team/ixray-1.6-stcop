/*
	=====================================================================
	Addon      : Parallax Reflex Sights
	Link       : https://www.moddb.com/mods/stalker-anomaly/addons/parallax-reflex-sights
	Authors    : LVutner, party_50
	Date       : 06.02.2024
	Last Edit  : 06.02.2024
	Info	   : Ported back to shitX9 by LVutner
	=====================================================================
*/

#include "common.hlsli"

uniform float4 m_affects;

// Important:
// In perfect world OFFSET constants should be 0, but most of reflex sight lenses
// are not actually parallel to screen, so we compensate it. For PROJECT_DISTANCE=100
// offset values should be at least 0.005 even for perfect models and position configs due
// to normal vectors resolution.
//
// If you want the most realistic look, set PROJECT_DISTANCE to some high value (like 100.0),
// increase SIZE_FACTOR to something like 20.0, set OFFSET_X and OFFSET_Y to 0.005.
// Then you will have to adjust models so that mark texture point is exactly in center
// and edit aim position in configs.

#define OFFSET_X 0.004			// (default 0.004) Normal vector x coordinate max absolute value which is considered 0
#define OFFSET_Y 0.05			// (default 0.05) Normal vector y coordinate max absolute value which is considered 0
#define PROJECT_DISTANCE 20.0	// (default 20.0) Distance to projected mark
#define SIZE_FACTOR 4.0			// (default 4.0) Mark size factor

// Vertex to Pixel struct
struct vf
{
    float2 tc0 : TEXCOORD0;
    float3 v_pos : TEXCOORD1;
    float3 v_nrm : TEXCOORD2;
};

// This gives us cotangent basis that can be used instead of TBN.
// It is useful when tangents of your mesh are broken, or not available.
// Source: http://www.thetenthplanet.de/archives/1180
float3x3 cotangent_frame(float3 N, float3 P, float2 uv)
{
    // Get edge vectors of the pixel triangle
    float3 dp1 = ddx(P);
    float3 dp2 = ddy(P);
    float2 duv1 = ddx(uv);
    float2 duv2 = ddy(uv);

    // Solve the linear system
    float3 dp2perp = cross(dp2, N);
    float3 dp1perp = cross(N, dp1);
    float3 T = dp2perp * duv1.x + dp1perp * duv2.x;
    float3 B = dp2perp * duv1.y + dp1perp * duv2.y;

    // Construct a scale-invariant frame
    float invmax = rsqrt(max(dot(T, T), dot(B, B)));
    return float3x3(T * invmax, B * invmax, N);
}

// If N.xy vector is close to zero, make it zero
float3 offset_normal(float3 N)
{
	if (N.x > 0)
		N.x = max(N.x, OFFSET_X) - OFFSET_X;
	else
		N.x = min(N.x, -OFFSET_X) + OFFSET_X;
	
	if (N.y > 0)
		N.y = max(N.y, OFFSET_Y) - OFFSET_Y;
	else
		N.y = min(N.y, -OFFSET_Y) + OFFSET_Y;
		
	return N;
}

float4 main(vf I) : COLOR
{
    // Derive view direction from view space position
    float3 V = -I.v_pos;
    
    // Build cotangent frame
    float3x3 TBN = cotangent_frame(offset_normal(I.v_nrm), I.v_pos, I.tc0.xy);
    
    // Transform view direction to tangent space, and normalize (Just in case)
    float3 V_tangent = normalize(float3(dot(V, TBN[0]), dot(V, TBN[1]), dot(V, TBN[2])));
	
    // Calculate texture coordinates used to fetch the mark texture
    float2 parallax_tc = I.tc0 - V_tangent.xy * PROJECT_DISTANCE;
	
	// Upscaling the texture
	parallax_tc.x = (parallax_tc.x + (SIZE_FACTOR - 1) / 2) / SIZE_FACTOR;
	parallax_tc.y = (parallax_tc.y + (SIZE_FACTOR - 1) / 2) / SIZE_FACTOR;

    // Fetch the mark texture
    float4 mark_texture = tex2Dlod(s_base, float4(parallax_tc, 0.0, 0.0));

    // === Скрытие метки при выбросе ===
    const float MARK_MAX_STAGE = 0.3; // третья стадия (3/10)

    float mark_visibility = 1.0;

    if (m_affects.x >= MARK_MAX_STAGE)
    {
        // Стадия превышена — метка полностью выключена
        mark_visibility = 0.0;
    }
    else if (m_affects.x > 0.1)
    {
        // Нормированная стадия 0..1
        float stage = m_affects.x / MARK_MAX_STAGE;

        // Шанс выключения растёт квадратично
        float off_chance = stage * stage;

        // Мерцание
        float flicker_rate = 5.0 + 25.0 * stage;
        float flicker = frac(timers.z * flicker_rate + m_affects.y);

        if (flicker < off_chance)
            mark_visibility = 0.0;
    }

    mark_texture *= mark_visibility;

    return mark_texture;
}
