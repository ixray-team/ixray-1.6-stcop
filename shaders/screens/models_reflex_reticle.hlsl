#include "common\common.hlsli"
#include "common\scene_model.hlsli"

// Vertex to Pixel struct
struct vf
{
    float2 tc0 : TEXCOORD0;
	
    float3 v_pos : TEXCOORD1;
    float3 v_nrm : TEXCOORD2;
    nointerpolation uint material : TEXCOORD8; // of the scene's table, for a model drawn from the scene
	
#ifndef DISABLE_MOTION_VECTORS
    float4 hpos_curr : TEXCOORD3;
    float4 hpos_old  : TEXCOORD4;
#endif
	
    float4 hpos : SV_Position;
};

vf vs_main(PULL_INPUT)
{
    vf o;
    scene_model model = pull_scene_model(VertexId, DrawId);
    v_model v = model.vertex;

    o.hpos = scene_model_project(model, v.P); // Homogenous position
    o.tc0 = v.tc.xy; //Texture coordinates

    o.v_pos = scene_model_view(model, v.P); // Position in view space
    o.v_nrm = scene_model_view_turn(model, v.N); // Normal in view space
    o.material = model.material;

#ifndef DISABLE_MOTION_VECTORS
	o.hpos_curr = o.hpos;
    o.hpos_old = scene_model_project_old(model, v.P_old);
#endif
	
    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;

    return o;
}

/*
	=====================================================================
	Addon      : Parallax Reflex Sights
	Link       : https://www.moddb.com/mods/stalker-anomaly/addons/parallax-reflex-sights
	Authors    : LVutner, party_50
	Date       : 06.02.2024
	Last Edit  : 06.02.2024
	=====================================================================
*/

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

#define OFFSET 0.05				// (default 0.05) Normal vector coordinate max absolute value which is considered 0
#define PROJECT_DISTANCE 20.0	// (default 20.0) Distance to projected mark
#define SIZE_FACTOR 4.0			// (default 4.0) Mark size factor

// Vertex to Pixel struct

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

float3 offset_normal(float3 N)
{
	N.xy = N.xy > 0 ? max(N.xy, OFFSET) - OFFSET : min(N.xy, -OFFSET) + OFFSET;
	N.z += N.z > 0 ? EPS : -EPS;
	
	return normalize(N);
}

void ps_main(in vf I, out IXRayForward O)
{
    O = (IXRayForward)0;
    // Derive view direction from view space position
    float3 V = -I.v_pos;
    
    // Build cotangent frame
    // Important: In theory, you don't need to do this. It should be possible to pass TBN straight from VS
    float3x3 TBN = cotangent_frame(offset_normal(I.v_nrm), I.v_pos, I.tc0.xy);
    
    // Transform view direction to tangent space, and normalize (Just in case)
    float3 V_tangent = normalize(float3(dot(V, TBN[0]), dot(V, TBN[1]), dot(V, TBN[2])));
	
    // Calculate texture coordinates used to fetch the mark texture
    // Important: PROJECT_DISTANCE can be positive or negative, 0 = no projection at all
    float2 parallax_tc = I.tc0 - V_tangent.xy * PROJECT_DISTANCE;
	
	// Upscaling the texture
	parallax_tc.x = (parallax_tc.x + (SIZE_FACTOR - 1) / 2) / SIZE_FACTOR;
	parallax_tc.y = (parallax_tc.y + (SIZE_FACTOR - 1) / 2) / SIZE_FACTOR;
	

    // Fetch the mark texture
    // Important: We do not want texture to repeat itself, so we use sampler with CLAMP address
    // Important2: We do not want to sample mip levels of the mark texture, let's keep this thing sharp as fuck
	
    // The mark is the model's base map: a model the scene draws has it in the texture buckets, read by its material,
    // where a texture bound by name reads as nothing. The buckets' sampler wraps: outside the texture there is no mark
    SCENE_MODEL_MATERIAL(I);
#ifdef SURFACE_CLUSTER
    float4 mark_texture = SURFACE_MAP_AT(s_base, smp_rtlinear, parallax_tc, (float2)0.0f, (float2)0.0f);
    mark_texture *= all(saturate(parallax_tc) == parallax_tc) ? 1.0f : 0.0f;
#else
    float4 mark_texture = s_base.SampleLevel(smp_rtlinear, parallax_tc, 0.0);
#endif
	mark_texture.xyz = SceneFromGamma(mark_texture.xyz * mark_texture.w);
	mark_texture.w = 1.0f;
	
	O.Color = mark_texture;

    const float MARK_MAX_STAGE = 0.34;

    float mark_visibility = 1.0;

    if (m_affects.x >= MARK_MAX_STAGE)
    {
        mark_visibility = 0.0;
    }
    else if (m_affects.x > 0.1)
    {
        float stage = m_affects.x / MARK_MAX_STAGE;

        float off_chance = stage * stage;

        float flicker_rate = 5.0 + 25.0 * stage;
        float flicker = frac(timers.z * flicker_rate + m_affects.y);

        if (flicker < off_chance)
            mark_visibility = 0.0;
    }

    mark_texture.xyz *= mark_visibility;
    mark_texture.w = 1.0;

    O.Color = mark_texture;
	
#ifndef DISABLE_MOTION_VECTORS
    float Fade = saturate(max(mark_texture.x, max(mark_texture.y, mark_texture.z)) * 2.0f);
    
    O.Velocity.xy = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
    O.Velocity.w = 1.0f - Fade; 
    O.Velocity.xy *= Fade / SIZE_FACTOR;
    O.Velocity.z = 1.0f;
#endif
}
