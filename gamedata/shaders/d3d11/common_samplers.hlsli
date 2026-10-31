#ifndef common_samplers_h_included
#define common_samplers_h_included

// Geometry phase / deferring               	//

sampler smp_nofilter : register(s0); //	Use D3DTADDRESS_CLAMP,	D3DTEXF_POINT,			D3DTEXF_NONE,	D3DTEXF_POINT
sampler smp_rtlinear : register(s1); //	Use D3DTADDRESS_CLAMP,	D3DTEXF_LINEAR,			D3DTEXF_NONE,	D3DTEXF_LINEAR
sampler smp_linear : register(s2); //	Use	D3DTADDRESS_WRAP,	D3DTEXF_LINEAR,			D3DTEXF_LINEAR,	D3DTEXF_LINEAR
sampler smp_base : register(s3); //	Use D3DTADDRESS_WRAP,	D3DTEXF_ANISOTROPIC, 	D3DTEXF_LINEAR,	D3DTEXF_ANISOTROPIC
sampler smp_base_clamp : register(s12);
#ifdef SMP_BASE_CLAMP
#define smp_base smp_base_clamp
#endif

Texture2D s_base : register(t0); //	smp_base
Texture2D s_generic : register(t27);
Texture2D s_bump : register(t1); //
Texture2D s_bumpX : register(t2); //
Texture2D s_detail : register(t3); //
Texture2D s_detailBump : register(t4); //
Texture2D s_detailBumpX : register(t5); //	Error for bump detail
// Texture2D 	s_bumpD;                //
Texture2D s_hemi : register(t6); //

Texture2D s_mask : register(t7); //

Texture2D s_dt_r : register(t9); //
Texture2D s_dt_g : register(t10); //
Texture2D s_dt_b : register(t11); //
Texture2D s_dt_a : register(t12); //

Texture2D s_dn_r : register(t13); //
Texture2D s_dn_g : register(t14); //
Texture2D s_dn_b : register(t15); //
Texture2D s_dn_a : register(t16); //

Texture2D s_dn_rX : register(t17);
Texture2D s_dn_gX : register(t18);
Texture2D s_dn_bX : register(t19);
Texture2D s_dn_aX : register(t20);

Texture2D s_dt_spec_r : register(t21);
Texture2D s_dt_spec_g : register(t22);
Texture2D s_dt_spec_b : register(t23);
Texture2D s_dt_spec_a : register(t24);

Texture2D s_refl : register(t29);

TextureCube s_env : register(t30);
TextureCube s_env_dist : register(t31);

TextureCube env_s0 : register(t32);
TextureCube env_s1 : register(t33);

TextureCube sky_s0 : register(t34);
TextureCube sky_s1 : register(t35);

Texture3D s_blue_noise : register(t36);

// Lighting/shadowing phase                     //

sampler smp_material : register(s4);

// uniform sampler2D       s_depth;                //
Texture2D s_position : register(t37); //	smp_nofilter or Load
Texture2D s_surface : register(t38); //	smp_nofilter or Load
Texture2D s_normal : register(t39); //	smp_nofilter or Load
Texture2D s_lmap : register(t8); // 2D/???cube projector lightmap
Texture3D s_material : register(t40); //	smp_material
// uniform sampler1D       s_attenuate;        	//

// Combine phase                                //
Texture2D s_diffuse : register(t41); // rgb.a = diffuse.gloss
Texture2D s_accumulator : register(t42); // rgb.a = diffuse.specular
// uniform sampler2D       s_generic;              //
Texture2D s_bloom : register(t43); //
Texture2D s_image : register(t44); // used in various post-processing
Texture2D s_velocity : register(t45); // used in various post-processing
Texture2D s_tonemap : register(t46); // actually MidleGray / exp(Lw + eps)

Texture2D n_bloom : register(t47); // new papa_doenitz bloom
Texture2D s_adapt : register(t48); // new adaptation texture

#endif //	#ifndef	common_samplers_h_included
