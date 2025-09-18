#include "common.hlsli"
#include "sload.hlsli"

#ifndef USE_LENGTH_BUFFER
	#define OutStructure IXrayGbufferPack
#else
	#define OutStructure f_forward
	
#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"

#endif

void main(p_bumped_new I, out OutStructure O)
{
    IXrayMaterial M;
    M.Depth = I.position.z;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
    M.Point = I.position.xyz;

    M.Color = s_base.Sample(smp_base, I.tcdh.xy);
	M.Metalness = 0.0f;
	M.SSS = 0.0f;
	M.AO = 1.0f;

    float4 Lmap = s_lmap.Sample(smp_base, I.tcdh.xy);
    float2 tcdbump = I.tcdh.xy * dt_params.xy;

#ifdef USE_PBR
	#ifdef USE_4_BUMP
		float4 Mask = s_mask.Sample(smp_base, I.tcdh.xy);
		Mask /= dot(Mask, 1.0f);

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

		//Unpack normals
		M.Normal.xy = (r_bump.wy + g_bump.wy + b_bump.wy + a_bump.wy) - 128.0f / 255.0f;
		M.Normal.z = sqrt(1.0f - dot(M.Normal.xy, M.Normal.xy));

		#ifndef USE_DX_NORMAL_MAP
			M.Normal.y *= -1.0f;
		#endif

		M.Color.xyz *= (r_base + g_base + b_base + a_base) * 2.0f; //LVutner: Change later
		M.Metalness = r_bumpX.x + g_bumpX.x + b_bumpX.x + a_bumpX.x;
		M.Roughness = r_bumpX.y + g_bumpX.y + b_bumpX.y + a_bumpX.y;
		M.SSS = r_bumpX.z + g_bumpX.z + b_bumpX.z + a_bumpX.z;
		M.AO = r_bumpX.w + g_bumpX.w + b_bumpX.w + a_bumpX.w;
	#else
		float4 Detail = s_detail.Sample(smp_base, tcdbump);
		float4 DetailBump = s_detailBump.Sample(smp_base, tcdbump);

		M.Roughness = DetailBump.x;
		M.Normal.xyz = DetailBump.wzy - 0.5f;
	#endif
#else
	#ifdef USE_4_BUMP
		float4 Mask = s_mask.Sample(smp_base, I.tcdh.xy);
		Mask /= dot(Mask, 1.0f);
		
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
		M.Roughness = min(1.0f, Normal_R.x + Normal_G.x + Normal_B.x + Normal_A.x);
	#else
		float4 Detail = s_detail.Sample(smp_base, tcdbump);
		float4 DetailBump = s_detailBump.Sample(smp_base, tcdbump);
		
		M.Roughness = DetailBump.x;
		M.Normal.xyz = DetailBump.wzy - 0.5f;
	#endif
		M.Normal.z *= 0.5f;
		M.Color.xyz *= Detail * 2.0f;
#endif	


    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
    M.Normal = normalize(M.Normal);

    M.Sun = Lmap.w;
    M.Hemi = M.Color.w;

#ifdef USE_LEGACY_LIGHT
    M.Metalness = L_material.w;
#else
    M.Roughness = 1.0f - M.Roughness * 0.9f;
#endif

#ifdef IGNORE_SNOW_MASK
	M.SnowMask = 0.0f;
#else
    M.SnowMask = 1.0f;
#endif

    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
	
#ifndef USE_LENGTH_BUFFER
    GbufferPack(O, M);
#else
	float4 LightColor = float4(L_sun_color.xyz, 0.5f);

    M.Sun = saturate(M.Sun * 2.0f);
    M.Color.xyz = PushGamma(saturate(M.Color.xyz));
	
	float ViewLength = length(M.Point);
	float3 View = M.Point.xyz * rcp(ViewLength);
	
#ifndef USE_PBR
	float3 F0 = 0.0f;
#else
	float3 F0 = 0.04f;
#endif

    float3 Light = M.Sun * DirectLight(LightColor, mul((float3x3)m_V, L_sun_dir_w.xyz), M.Normal, View, M.Color.xyz, M.Metalness, M.Roughness, F0);
    float3 Ambient = PushGamma(M.AO) * AmbientLighting(View, M.Normal, M.Color.xyz, M.Metalness, M.Roughness, M.Hemi, F0);
	
	Light += DirectLight(float4(Lmap.xyz, 0.5f), View, M.Normal, View, M.Color.xyz, M.Metalness, M.Roughness, F0);
	
    O.Color.xyz = Ambient + Light;
    O.Color.w = 1.0f;

    float Fog = PushGamma(saturate(ViewLength * fog_params.w + fog_params.x));
    O.Color = lerp(O.Color, PushGamma(fog_color), Fog);

    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
    O.Reactive = O.Color.w * 0.9f;
	
	O.Color.w = ViewLength;
	O.Color.xyz *= rcp(1.0f + O.Color.xyz);
#endif
}

