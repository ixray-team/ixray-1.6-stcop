#include "common.hlsli"
#include "sload.hlsli"

void main (p_bumped_new I, out IXrayGbufferPack O) {
	IXrayMaterial M;
	M.Depth = I.position.z;
	
	M.Sun = I.tcdh.w;
	M.Hemi = I.tcdh.z;
	M.Point = I.position.xyz;
	
	SloadNew(I, M);
	
#ifdef USE_AREF
	#ifdef USE_HASHED_AREF
		clip(M.Color.w - hashed_alpha_test(M.Point));
	#else
		clip(M.Color.w - def_aref);
	#endif
	#ifdef USE_DXT1_HACK
		M.Color.xyz /= max(0.0001f, M.Color.w);
	#endif
#endif
	
	M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
	M.Normal = normalize(M.Normal);
	
#ifdef USE_LM_HEMI
	float4 lm = s_hemi.Sample(smp_rtlinear, I.tcdh.zw);
	
	M.Sun = get_sun(lm);
	M.Hemi = get_hemi(lm);
#endif
	
#ifdef USE_LEGACY_LIGHT
	#ifndef USE_PBR
		M.Metalness = L_material.w;
	#else
		M.Color.xyz *= M.AO; M.AO = 1.0f;
		float Specular = M.Metalness * dot(M.Color.xyz, LUMINANCE_VECTOR);
		M.Color.xyz = lerp(M.Color.xyz, F0, M.Metalness);
		M.Metalness = 0.5f - M.Roughness * M.Roughness * 0.5f;
		M.Roughness = Specular;
	#endif
#endif

#if defined(USE_AREF) && defined(USE_TREEWAVE)
	M.SSS = 1.0f;
#endif
	
	O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
	GbufferPack(O, M);
}

