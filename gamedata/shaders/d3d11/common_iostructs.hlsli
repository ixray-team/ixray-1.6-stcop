#ifndef common_iostructs_h_included
#define common_iostructs_h_included

////////////////////////////////////////////////////////////////
//	This file contains io structs:
//	v_name	:	input for vertex shader.
//	v2p_name:	output for vertex shader.
//	p_name	:	input for pixel shader.
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//	TL0uv
struct v_TL0uv_positiont
{
    float4 P : POSITIONT;
    float4 Color : COLOR;
};

struct v_TL0uv
{
    float4 P : POSITION;
    float4 Color : COLOR;
};

struct v2p_TL0uv
{
    float4 Color : COLOR;
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

struct p_TL0uv
{
    float4 Color : COLOR;
    //	float4 	HPos	: SV_POSITION;	// Clip-space position 	(for rasterization)
};

////////////////////////////////////////////////////////////////
//	TL
struct v_TL_positiont
{
    float4 P : POSITIONT;
    float2 Tex0 : TEXCOORD0;
    float4 Color : COLOR;
};

struct v_TL
{
    float4 P : POSITION;
    float2 Tex0 : TEXCOORD0;
    float4 Color : COLOR;
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
//	TL2uv
struct v_TL2uv
{
    float4 P : POSITIONT;
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float4 Color : COLOR;
};

struct v2p_TL2uv
{
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float4 Color : COLOR;
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

struct p_TL2uv
{
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float4 Color : COLOR;
    //	float4 	HPos	: SV_POSITION;	// Clip-space position 	(for rasterization)
};
////////////////////////////////////////////////////////////////
//	postpr
struct v_postpr
{
    float4 P : POSITIONT;
    float2 Tex0 : TEXCOORD0; // base1 (duality)
    float2 Tex1 : TEXCOORD1; // base2 (duality)
    float2 Tex2 : TEXCOORD2; // base  (noise)
    float4 Color : COLOR0; // multiplier, color.w = noise_amount
    float4 Gray : COLOR1; // (.3,.3,.3.,amount)
};

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
//	aa_AA
struct v_aa_AA
{
    float4 P : POSITIONT;
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float2 Tex2 : TEXCOORD2;
    float2 Tex3 : TEXCOORD3;
    float2 Tex4 : TEXCOORD4;
    float4 Tex5 : TEXCOORD5;
    float4 Tex6 : TEXCOORD6;
};

struct v2p_aa_AA
{
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float2 Tex2 : TEXCOORD2;
    float2 Tex3 : TEXCOORD3;
    float2 Tex4 : TEXCOORD4;
    float4 Tex5 : TEXCOORD5;
    float4 Tex6 : TEXCOORD6;
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

struct p_aa_AA
{
    float2 Tex0 : TEXCOORD0;
    float2 Tex1 : TEXCOORD1;
    float2 Tex2 : TEXCOORD2;
    float2 Tex3 : TEXCOORD3;
    float2 Tex4 : TEXCOORD4;
    float4 Tex5 : TEXCOORD5;
    float4 Tex6 : TEXCOORD6;
    //	float4 	HPos	:SV_POSITION;	// Clip-space position 	(for rasterization)
};

struct p_aa_AA_sun
{
    float2 tc : TEXCOORD0;
    float2 unused : TEXCOORD1;
    float2 LT : TEXCOORD2;
    float2 RT : TEXCOORD3;
    float2 LB : TEXCOORD4;
    float2 RB : TEXCOORD5;
    //	float4 	HPos	:SV_POSITION;	// Clip-space position 	(for rasterization)
};

////////////////////////////////////////////////////////////////
//	dumb
struct v_dumb
{
    float4 P : POSITION; // Clip-space position 	(for rasterization)
};

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

struct v_static_color
{
    float4 P : POSITION; // (float,float,float,1)

    float4 Nh : NORMAL; // (nx,ny,nz,hemi occlusion)
    float4 T : TANGENT; // tangent
    float4 B : BINORMAL; // binormal

    float4 color : COLOR0; // (r,g,b,dir-occlusion)	//	Swizzle before use!!!
    int2 tc : TEXCOORD0; // (u,v)

#ifdef USE_LM_HEMI
    int2 lmh : TEXCOORD1; // (lmu,lmv)
#endif
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
    fl…64402 tokens truncated…d_texture_load", &ps_r__common_flags, RFLAG_DD_TEX_LOAD);
	CMD3(CCC_Token, "r_aa", &ps_r2_aa_type, aa_type_token);
	CMD4(CCC_Integer, "r__optimize_static_geom", &opt_static, 0, 2);
	CMD4(CCC_Integer, "r__optimize_dynamic_geom", &opt_dynamic, 0, 2);
	CMD3(CCC_Mask32, "r__optimize_shadow_geom", &ps_r__common_flags, RFLAG_OPT_SHAD_GEOM);
	CMD3(CCC_Mask32, "r__shader_cache", &ps_r__common_flags, RFLAG_USE_CACHE);
	
	CMD3(CCC_Token, "r__screenshot_format", &ps_screenshot_format, screenshot_format_token);
	CMD3(CCC_Token, "r4_mblur_quality", &ps_r4_mblur_quality, mblur_quality_token);
	CMD4(CCC_Float, "r4_mblur_power", &ps_r4_mblur_power, 0.0f, 1.0f);
	CMD4(CCC_Float, "r1_full_detail_distance_scale", &ps_r1_full_detail_distance_scale, 0.1f, 1.0f);

	CMD3(CCC_Mask32, "r1_use_terrain_mask", &ps_r1_flags, R1FLAG_TERRAIN_MASK);

	CMD4(CCC_Float, "r2_aref_quality", &ps_r2_def_aref_quality, 70.0f, 200.0f);
	CMD3(CCC_Mask32, "r2_use_bump", &ps_r__common_flags, R2FLAG_USE_BUMP);
	CMD3(CCC_Mask32, "r2_vignette", &ps_r2_ls_flags_ext, R2FLAG_SPP_VIGNETTE);
	CMD3(CCC_Mask32, "r2_aberration", &ps_r2_ls_flags_ext, R2FLAG_SPP_ABERRATION);
	CMD3(CCC_Mask32, "r2_saturation", &ps_r2_ls_flags_ext, R2FLAG_SPP_SATURATION);
	CMD3(CCC_Token, "r2_smap_size", &ps_r2_smapsize, qsmapsize_token);
	CMD3(CCC_Mask32, "r2_cloud_shadows", &ps_r2_ls_flags_ext, RFLAG_CLOUD_SHADOWS);	//Need restart

	CMD3(CCC_Mask32, "r4_hud_shadows", &ps_r2_ls_flags_ext, R4FLAG_SCREEN_SPACE_HUD_SHADOWS);
	CMD3(CCC_Mask32, "r4_hashed_alpha_test", &ps_r2_ls_flags_ext, R4FLAG_HASHED_ALPHA_TEST);
	CMD3(CCC_Mask32, "r4_sslr_water", &ps_r2_ls_flags_ext, R4FLAG_SSLR_ON_WATER);
	CMD3(CCC_Mask32, "r4_sslr_reflections", &ps_r2_ls_flags_ext, R4FLAG_SSLR_ON_WORLD);
	CMD3(CCC_Mask32, "r4_translucent_shadows", &ps_r2_ls_flags_ext, R4FLAG_TSHDOWS);
	CMD4(CCC_Float, "r4_cas_sharpening", &ps_r4_cas_sharpening, 0.0f, 1.0f);

	CMD3(CCC_Mask32, "r4_puddles", &ps_r2_ls_flags_ext, R4FLAG_PUDDLES);
	CMD3(CCC_Mask32, "r4_rain_puddles", &ps_r2_ls_flags_ext, R4FLAG_RAIN_PUDDLES);

	CMD2(CCC_Boolean, "r4_use_gasmask", &UseGasmak);
	CMD2(CCC_Boolean, "r2_use_rain_drops", &UseRainDrops);
	CMD3(CCC_Token, "r2_rain_drops_mode", &g_RainDropsMode, RainDropsModeTokens);
	CMD2(CCC_Boolean, "r2_use_weapon_raindrops", &UseWeaponRainDrops);

	// added by Papa Doenitz 2026-03-05
	CMD2(CCC_Boolean, "r2_new_autoexposure", &ps_r2_new_autoexposure);
	CMD4(CCC_Float, "r2_autoexposure_key", &ps_r2_autoexposure_key, 0.01f, 1.0f);
	CMD4(CCC_Float, "r2_autoexposure_min", &ps_r2_autoexposure_min, -10.0f, 10.0f);
	CMD4(CCC_Float, "r2_autoexposure_max", &ps_r2_autoexposure_max, -10.0f, 10.0f);
	CMD4(CCC_Float, "r2_autoexposure_bias", &ps_r2_autoexposure_bias, -10.0f, 10.0f);
	CMD4(CCC_Float, "r2_autoexposure_speed", &ps_r2_autoexposure_speed, 0.0f, 100.0f);

	CMD2(CCC_Boolean, "r2_autoexposure_use_center_weight", &ps_r2_autoexposure_center_weight);
	CMD4(CCC_Float, "r2_autoexposure_min_weight", &ps_r2_autoexposure_min_weight, 0.0f, 0.9f);
	CMD4(CCC_Float, "r2_autoexposure_gaussian", &ps_r2_autoexposure_gaussian, 0.0f, 10.0f);

	CMD2(CCC_Boolean, "r2_autoexposure_use_soft_log", &ps_r2_autoexposure_soft_log);
	CMD4(CCC_Float, "r2_autoexposure_soft_log_k", &ps_r2_autoexposure_soft_log_k, 0.0f, 10.0f);
	CMD4(CCC_Float, "r2_autoexposure_soft_limiter", &ps_r2_autoexposure_soft_limiter, 0.0f, 10.0f);
	CMD4(CCC_Float, "r2_autoexposure_soft_sensetivity", &ps_r2_autoexposure_sensitivity, 0.0f, 10.0f);

	CMD4(CCC_Float, "r2_bloom_amount", &ps_r2_bloom_amount, 0.0f, 3.0f);
	CMD4(CCC_Float, "r2_bloom_desaturation", &ps_r2_bloom_desaturation, 0.0f, 1.0f);
	CMD4(CCC_Float, "r2_bloom_tint_amount", &ps_r2_bloom_tint_amount, 0.0f, 1.0f);
	CMD4(CCC_Vector3, "r2_bloom_tint_color", &ps_r2_bloom_tint_color, Fvector().set(0.0f, 0.0f, 0.0f), Fvector().set(1.0f, 1.0f, 1.0f));



	CMD2(CCC_Boolean, "r2_new_bloom_tonemap", &ps_r2_new_bloom_tonemap);
	CMD2(CCC_Boolean, "r2_crossfeed", &ps_r2_crossfeed);
	CMD2(CCC_Boolean, "r2_vibrance", &ps_r2_vibrance);
	CMD4(CCC_Float, "r2_tonemap_compression", &ps_r2_tonemap_compression, 0.0f, 10.0f);
	CMD4(CCC_Float, "r2_tonemap_desaturation", &ps_r2_tonemap_desaturation, 0.0f, 1.0f);
	CMD4(CCC_Float, "r2_tonemap_vibrance", &ps_r2_tonemap_vibrance, -1.0f, 1.0f);
	CMD4(CCC_Float, "r2_tonemap_crossfeed", &ps_r2_tonemap_crossfeed, 0.0f, 0.9f);
	// end of new tonemapping/bloom/autoexposure settings




#ifdef DEBUG_DRAW
	CMD4(CCC_Integer, "r__lsleep_frames", &ps_r__LightSleepFrames, 4, 30);
	CMD4(CCC_Float, "r__ssa_glod_start", &ps_r__GLOD_ssa_start, 128, 512);
	CMD4(CCC_Float, "r__ssa_glod_end", &ps_r__GLOD_ssa_end, 16, 96);
	CMD4(CCC_Float, "r__wallmark_shift_pp", &ps_r__WallmarkSHIFT, 0.0f, 1.f);
	CMD4(CCC_Float, "r__wallmark_shift_v", &ps_r__WallmarkSHIFT_V, 0.0f, 1.f);
	CMD2(CCC_Boolean, "r__wallmark_dyn", &ps_r__WallmarkDyn);

	//	Allow real-time fog config reload
	CMD1(CCC_Fog_Reload, "r3_fog_reload");

	CMD3(CCC_Mask32, "r4_wireframe", &ps_r2_ls_flags_ext, R2FLAGEXT_WIREFRAME);//Need restart
	CMD2(CCC_R2GM, "r2em", &ps_r2_gmaterial);

	CMD2(CCC_Boolean, "ui_dbg_graphic", &Engine.External.EditorStates[(int)EditorUI::Shaders]);
	CMD1(CCC_DumpResources, "dump_resources");

	CMD2(CCC_Boolean, "r_particles_real_dt", &ps_r2_particle_dt);
	tw_min.set(-10, -10, -EPS_S);	tw_max.set(10, 10, EPS_S);
	CMD4(CCC_Vector3, "r_taa_jitter_scale", &ps_r_taa_jitter_scale, tw_min, tw_max);

	// test
	CMD4(CCC_Float, "r_developer_float_1", &ps_r__test_exp_to_shaders_1, -10000000.0f, 10000000.0f);
	CMD4(CCC_Float, "r_developer_float_2", &ps_r__test_exp_to_shaders_2, -10000000.0f, 10000000.0f);
	CMD4(CCC_Float, "r_developer_float_3", &ps_r__test_exp_to_shaders_3, -10000000.0f, 10000000.0f);
	CMD4(CCC_Float, "r_developer_float_4", &ps_r__test_exp_to_shaders_4, -10000000.0f, 10000000.0f);
	CMD1(CCC_memory_stats, "render_memory_stats");

	CMD4(CCC_Integer, "r2_wait_sleep", &ps_r2_wait_sleep, 0, 1);
	CMD4(CCC_Integer, "r2_dhemi_count", &ps_r2_dhemi_count, 4, 25);
	CMD4(CCC_Float, "r2_dhemi_sky_scale", &ps_r2_dhemi_sky_scale, 0.0f, 100.f);
	CMD4(CCC_Float, "r2_dhemi_light_scale", &ps_r2_dhemi_light_scale, 0, 100.f);
	CMD4(CCC_Float, "r2_dhemi_light_flow", &ps_r2_dhemi_light_flow, 0, 1.f);
	CMD4(CCC_Float, "r2_dhemi_smooth", &ps_r2_lt_smooth, 0.f, 10.f);
	CMD3(CCC_Mask32, "rs_hom_depth_draw", &ps_r2_ls_flags_ext, R_FLAGEXT_HOM_DEPTH_DRAW);
	CMD3(CCC_Mask32, "r2_shadow_cascede_zcul", &ps_r2_ls_flags_ext, R2FLAGEXT_SUN_ZCULLING);
	CMD3(CCC_Mask32, "r2_exp_splitscene", &ps_r2_ls_flags, R2FLAG_EXP_SPLIT_SCENE);
	CMD3(CCC_Mask32, "r2_exp_donttest_uns", &ps_r2_ls_flags, R2FLAG_EXP_DONT_TEST_UNSHADOWED);

	CMD4(CCC_Integer, "rs_dbg_draw_depth", &r_debug_render_depth, 0, 1);
#endif
}

void xrRender_apply_tf()
{
	Console->Execute("r__tf_aniso");
	Console->Execute("r__tf_mipbias");
}
