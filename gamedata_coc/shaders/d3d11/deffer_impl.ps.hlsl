#include "common.hlsli"
#include "sload.hlsli"
#include "shadow.hlsli"

#ifndef USE_LENGTH_BUFFER
	#define OutStructure IXRayGbufferPack
#else
	#define OutStructure IXRayVSLRGBuffer
	
#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"

#endif

void main(p_bumped_new I, out OutStructure O)
{
    IXRayMaterial M = (IXRayMaterial)NULL;
    M.Depth = I.position.z;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
    M.Point = I.position.xyz;

#ifndef USE_LEGACY_LIGHT
	M.Specular = F0_BASE;
	M.Roughness = 0.5f;
	M.Metalness = 0.0f;
	M.SSS = 0.0f;
	M.AO = 1.0f;
#else
	M.Gloss = def_gloss;
#endif

    float2 tcdbump = I.tcdh.xy * dt_params.xy;
	
#ifdef USE_4_BUMP
	float4 Mask = s_mask.Sample(smp_base, I.tcdh.xy);
	Mask /= dot(Mask, 1.0f);

	#ifdef USE_TERRAIN_PARALLAX
		float2 texcoord = 0.0f;
		#ifdef USE_PBR
			[branch] if(Mask.x > EPS_L) { texcoord += UpdateTC(I, tcdbump, s_dn_r, 0) * Mask.x; } else { texcoord += tcdbump * Mask.x; }
			[branch] if(Mask.y > EPS_L) { texcoord += UpdateTC(I, tcdbump, s_dn_g, 0) * Mask.y; } else { texcoord += tcdbump * Mask.y; }
			[branch] if(Mask.z > EPS_L) { texcoord += UpdateTC(I, tcdbump, s_dn_b, 0) * Mask.z; } else { texcoord += tcdbump * Mask.z; }
			[branch] if(Mask.w > EPS_L) { texcoord += UpdateTC(I, tcdbump, s_dn_a, 0) * Mask.w; } else { texcoord += tcdbump * Mask.w; }
		#else
			[branch] if(Mask.x > EPS_L) { texcoord += UpdateTC(I, tcdbump, s_dn_rX, 3) * Mask.x; } else { texcoord += tcdbump * Mask.x; }
			[branch] if(Mask.y > EPS_L) { texcoord += UpdateTC(I, tcdbump, s_dn_gX, 3) * Mask.y; } else { texcoord += tcdbump * Mask.y; }
			[branch] if(Mask.z > EPS_L) { texcoord += UpdateTC(I, tcdbump, s_dn_bX, 3) * Mask.z; } else { texcoord += tcdbump * Mask.z; }
			[branch] if(Mask.w > EPS_L) { texcoord += UpdateTC(I, tcdbump, s_dn_aX, 3) * Mask.w; } else { texcoord += tcdbump * Mask.w; }
		#endif
		
		tcdbump = texcoord;
		I.tcdh.xy = tcdbump / dt_params.xy;
	#endif
#endif

    M.Color = s_base.Sample(smp_base, I.tcdh.xy);
    float4 Lmap = s_lmap.Sample(smp_base, I.tcdh.xy);

#ifdef USE_PBR
	#ifdef USE_4_BUMP
		float3 r_base = s_dt_r.Sample(smp_base, tcdbump).xyz * Mask.x;
		float3 g_base = s_dt_g.Sample(smp_base, tcdbump).xyz * Mask.y;
		float3 b_base = s_dt_b.Sample(smp_base, tcdbump).xyz * Mask.z;
		float3 a_base = s_dt_a.Sample(smp_base, tcdbump).xyz * Mask.w;

		float4 r_bump = s_dn_r.Sample(smp_base, tcdbump) * Mask.x;
		float4 g_bump = s_dn_g.Sample(smp_base, tcdbump) * Mask.y;
		float4 b_bump = s_dn_b.Sample(smp_base, tcdbump) * Mask.z;
		float4 a_bump = s_dn_a.Sample(smp_base, tcdbump) * Mask.w;

		float4 r_bumpX = s_dn_rX.Sample(smp_base, tcdbump) * Mask.x;
		float4 g_bumpX = s_dn_gX.Sample(smp_base, tcdbump) * Mask.y;
		float4 b_bumpX = s_dn_bX.Sample(smp_base, tcdbump) * Mask.z;
		float4 a_bumpX = s_dn_aX.Sample(smp_base, tcdbump) * Mask.w;

		//Unpack normals (if something is wrong - unpack and then blend them)
		M.Normal.xy = (r_bump.wy + g_bump.wy + b_bump.wy + a_bump.wy) * 2.0 - 1.0;
		M.Normal.z = sqrt(1.0f - saturate(dot(M.Normal.xy, M.Normal.xy)));

		#ifndef USE_DX_NORMAL_MAP
			M.Normal.y *= -1.0f;
		#endif

		M.Color.xyz *= (r_base + g_base + b_base + a_base) * 2.0f;
		
		#ifndef USE_LEGACY_LIGHT
			M.Metalness = r_bumpX.x + g_bumpX.x + b_bumpX.x + a_bumpX.x;
			M.Roughness = r_bumpX.y + g_bumpX.y + b_bumpX.y + a_bumpX.y;
			M.SSS = r_bumpX.z + g_bumpX.z + b_bumpX.z + a_bumpX.z;
			M.AO = r_bumpX.w + g_bumpX.w + b_bumpX.w + a_bumpX.w;
		#endif
	#else
		float4 Detail = s_detail.Sample(smp_base, tcdbump);
		float4 DetailBump = s_detailBump.Sample(smp_base, tcdbump);
		float4 DetailBumpX = s_detailBumpX.Sample(smp_base, tcdbump);

		#ifndef USE_LEGACY_LIGHT
			M.Roughness = DetailBumpX.y;
			M.Metalness = DetailBumpX.x;
			M.SSS = DetailBumpX.z;
			M.AO = DetailBumpX.w;
		#endif
		
		M.Normal.xy = DetailBump.wy * 2.0 - 1.0;
		M.Normal.z = sqrt(1.0f - saturate(dot(M.Normal.xy, M.Normal.xy)));

		#ifndef USE_DX_NORMAL_MAP
			M.Normal.y *= -1.0f;
		#endif
		
		M.Color.xyz *= Detail * 2.0f;
	#endif
#else
	#ifdef USE_4_BUMP
		float3 Detail_R = s_dt_r.Sample(smp_base, tcdbump).xyz * Mask.x;
		float3 Detail_G = s_dt_g.Sample(smp_base, tcdbump).xyz * Mask.y;
		float3 Detail_B = s_dt_b.Sample(smp_base, tcdbump).xyz * Mask.z;
		float3 Detail_A = s_dt_a.Sample(smp_base, tcdbump).xyz * Mask.w;
		
		float3 Detail = Detail_R + Detail_G + Detail_B + Detail_A;

		float4 Normal_R = s_dn_r.Sample(smp_base, tcdbump) * Mask.x;
		float4 Normal_G = s_dn_g.Sample(smp_base, tcdbump) * Mask.y;
		float4 Normal_B = s_dn_b.Sample(smp_base, tcdbump) * Mask.z;
		float4 Normal_A = s_dn_a.Sample(smp_base, tcdbump) * Mask.w;

		M.Normal = Normal_R.wzy + Normal_G.wzy + Normal_B.wzy + Normal_A.wzy - 0.5;
		
		#ifndef USE_LEGACY_LIGHT
			M.Specular = min(1.0f, Normal_R.x + Normal_G.x + Normal_B.x + Normal_A.x);
		#else
			M.Gloss = min(1.0f, Normal_R.x + Normal_G.x + Normal_B.x + Normal_A.x);
		#endif
	#else
		float4 Detail = s_detail.Sample(smp_base, tcdbump);
		float4 DetailBump = s_detailBump.Sample(smp_base, tcdbump);
		float4 DetailBumpX = s_detailBumpX.Sample(smp_base, tcdbump);
		
		#ifndef USE_LEGACY_LIGHT
			M.Roughness = DetailBump.x;
		#else
			M.Gloss = DetailBump.x;
		#endif
		
		M.Normal.xyz = DetailBump.wzy + DetailBumpX.xyz - 1.0f;
	#endif
	
	M.Normal.z *= 0.5f;
	M.Color.xyz *= Detail.xyz * 2.0f;
#endif

    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
    M.Normal = normalize(M.Normal);

    M.Sun = Lmap.w;
    M.Hemi = M.Color.w;

#ifdef USE_LEGACY_LIGHT
    M.Material = L_material.w;
#endif

// #ifdef IGNORE_SNOW_MASK_ON_TERRAIN
	// M.MaterialID = BaseID;
// #else
    // M.SnowMask = 1.0f;
// #endif

	M.MaterialID = TERRAIN_ID;
	
#ifndef USE_LENGTH_BUFFER
	#ifndef DISABLE_MOTION_VECTORS
		O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
	#endif
	
    GbufferPack(O, M);
#else
	
	float4 LightColor = float4(L_sun_color.xyz, 0.5f);
	float3 LightDir = mul((float3x3)m_V, L_sun_dir_w.xyz);
	
    M.Sun = saturate(M.Sun * 2.0f);
	
	#ifndef USE_R2_STATIC_SUN
		float4 Point = float4(M.Point.xyz, 1.f);
		Point.xyz += M.Normal * 0.025f;
		
		Point.xyz = mul(m_invV, Point).xyz;

		int cascade_index;
		float3 smap_texcoord;
		
		bool is_in_bounds = calc_cascades(Point.xyz, m_shadow_sun, cascade_index, smap_texcoord);

		float Shadow = 1.0;

		if(is_in_bounds)
		{
			Shadow = pcf_3x3(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 0.0, cascade_index);
		}

		if(cascade_index >= 2)
		{
			float3 Factor = smoothstep(0.499f, 0.498f, abs(smap_texcoord - 0.5f));
			float Fade = Factor.x * Factor.y * Factor.z;

			float FarShadow = saturate(M.Hemi * 8.0f - 2.0f);
			Shadow = lerp(FarShadow, Shadow, Fade);
		
			float3 FlatNormal = normalize(cross(ddx(M.Point.xyz), ddy(M.Point.xyz)));
			Shadow *= step(0.0f, dot(FlatNormal, -LightDir));
		}
		
		M.Sun = Shadow;
	#endif

	float ViewLength = length(M.Point);
	float3 View = M.Point.xyz * rcp(ViewLength);
	
	Lmap = Lmap * 4.0f;

	#ifndef USE_LEGACY_LIGHT
		M.Color.xyz = GammaToLinear(M.Color.xyz);
		M.Specular = M.Specular * M.Specular * 0.16f;
	
		float3 Diffuse = M.Color.xyz * float(1.0f - M.Metalness);
		float3 Specular = lerp(M.Specular, M.Color.xyz, M.Metalness);
		
		float3 Light = GammaToLinear(M.Sun) * DirectLight(LightColor, mul((float3x3)m_V, L_sun_dir_w.xyz), M.Normal, View, Diffuse, Specular, M.Roughness);
		float3 Ambient = GammaToLinear(M.AO) * AmbientLighting(View, M.Normal, Diffuse, Specular, M.Roughness, M.Hemi);
		
		Light += DirectLight(Lmap.xyzy, View, M.Normal, View, Diffuse, Specular, M.Roughness);
	#else
		float3 Light = M.Sun * DirectLightLegacy(LightColor, mul((float3x3)m_V, L_sun_dir_w.xyz), M.Normal, View, M.Color.xyz, M.Material, M.Gloss);
		float3 Ambient = AmbientLightingLegcay(View, M.Normal, M.Color.xyz, M.Material, M.Gloss, M.Hemi);
		
		Light += DirectLightLegacy(Lmap.xyzy, View, M.Normal, View, M.Color.xyz, M.Material, M.Gloss);
	#endif
	
    O.Color = Ambient + Light;
	O.Length = ViewLength;
#endif
}

