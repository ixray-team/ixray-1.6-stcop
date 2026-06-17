#include "common.hlsli"
#include "sload.hlsli"

void cotangent_frame(inout p_bumped_new O)
{
    // Get edge vectors of the pixel triangle
    float3 dp1 = ddx(O.position.xyz);
    float3 dp2 = ddy(O.position.xyz);
	
    float2 duv1 = ddx(O.tcdh.xy);
    float2 duv2 = ddy(O.tcdh.xy);

	float3 N = normalize(float3(O.M1.z, O.M2.z, O.M3.z));

    // Solve the linear system
    float3 dp2perp = cross(dp2, N);
    float3 dp1perp = cross(N, dp1);
	
    float3 T = normalize(dp2perp * duv1.x + dp1perp * duv2.x);
    float3 B = normalize(dp2perp * duv1.y + dp1perp * duv2.y);
	
    float3x3 xform = float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z
	);

    O.M1 = xform[0];
    O.M2 = xform[1];
    O.M3 = xform[2];
}

void main(p_bumped_new I,
#ifdef FIX_CULL_NORMAL
	in bool is_front_face : SV_IsFrontFace,
#endif
	out IXRayGbufferPack O
)
{
    IXRayMaterial M = (IXRayMaterial)NULL;
    M.Depth = I.position.z;
	
#ifndef DISABLE_MOTION_VECTORS
	#ifdef USE_CLIP_NEAR_PLANE
		clip(I.hpos_curr.z - I.hpos_curr.w * 0.02f);
	#endif
#endif

#ifdef FIX_CULL_NORMAL
	float3(I.M1.z, I.M2.z, I.M3.z) *= is_front_face * 2.0f - 1.0f;
	
	#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
		cotangent_frame(I);
	#endif
#endif

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
    M.Point = I.position.xyz;

    SloadNew(I, M);

#ifdef USE_AREF
	#if defined(USE_HASHED_AREF) && !defined(DETAIL_SHADOW_PASS)
		clip(M.Color.w - hashed_alpha_test(M.Point));
	#else
		clip(M.Color.w - def_aref);
	#endif
	#ifdef USE_DXT1_HACK
		M.Color.xyz *= M.Color.w > 0.0f ? rcp(M.Color.w) : 0.0f;
	#endif
#endif

#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
#else
	M.Normal = float3(I.M1.z, I.M2.z, I.M3.z);
#endif

    M.Normal = normalize(M.Normal);

#ifdef USE_LM_HEMI
    float4 lm = s_hemi.Sample(smp_rtlinear, I.tcdh.zw);

    M.Sun = get_sun(lm);
    M.Hemi = get_hemi(lm);
#endif

#ifdef USE_LEGACY_LIGHT
	M.Material = L_material.w;
#endif

#ifdef USE_AREF
	#if !defined(USE_PBR) && defined(USE_TREEWAVE)
		M.SSS = 1.0f;
	#endif
#endif

#if defined(USE_TREEWAVE) || defined(USE_AREF)
	M.MaterialID = FOLIAGE_ID;
#elif defined(FORWARD_LIGHT)
	M.MaterialID = OBJECT_ID;
#else
	M.MaterialID = BASE_ID;
#endif

#ifndef DISABLE_MOTION_VECTORS
    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
#endif

    GbufferPack(O, M);
}

