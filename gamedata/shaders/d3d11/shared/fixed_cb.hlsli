#ifndef FIXED_CB_HLSLI
#define FIXED_CB_HLSLI

#ifdef __cplusplus
struct alignas(16) shader_float4
{
	float x, y, z, w;

	constexpr shader_float4() : x(0.f), y(0.f), z(0.f), w(0.f) {}
	constexpr shader_float4(float x, float y, float z, float w) : x(x), y(y), z(z), w(w) {}
	shader_float4& set(float x_, float y_, float z_, float w_ = 1.f)
	{
		x = x_;
		y = y_;
		z = z_;
		w = w_;
		return *this;
	}
	template<class T>
	shader_float4& set(const T& v)
	{
		x = v.x;
		y = v.y;
		z = v.z;
		w = v.w;
		return *this;
	}
	constexpr bool operator==(const shader_float4& o) const
	{
		return x == o.x && y == o.y && z == o.z && w == o.w;
	}
};

#define FX_V4 shader_float4
#define FX_FN inline constexpr
#define FX_OUT(T, n) T& n
#define FX_CBUFFER(cpp_name, hlsl_name, slot) struct alignas(16) cpp_name {
#define FX_CBUFFER_END() };
#define FX_F4(name) shader_float4 name;
#define FX_F4A(name, count) shader_float4 name[count];
#define FX_F3(name) shader_float4 name;
#define FX_F3X4(name) shader_float4 name[3];
#define FX_F4X4(name) shader_float4 name[4];
#define FX_F4X4A(name, count) shader_float4 name[(count) * 4];
#define FX_INT(name) int name;
#define FX_F1(name) float name;
#define FX_PAD2(name) float name##_0; float name##_1;
#define FX_PAD3(name) float name##_0; float name##_1; float name##_2;
#else
#define FX_V4 float4
#define FX_FN
#define FX_OUT(T, n) out T n
#define FX_CBUFFER(cpp_name, hlsl_name, slot) cbuffer hlsl_name : register(slot) {
#define FX_CBUFFER_END() };
#define FX_F4(name) float4 name;
#define FX_F4A(name, count) float4 name[count];
#define FX_F3(name) float3 name;
#define FX_F3X4(name) float3x4 name;
#define FX_F4X4(name) float4x4 name;
#define FX_F4X4A(name, count) float4x4 name[count];
#define FX_INT(name) int name;
#define FX_F1(name) float name;
#define FX_PAD2(name) float2 name;
#define FX_PAD3(name) float3 name;
#endif

#define CB_FRAME_FIELDS \
	FX_F4(timers) \
	FX_F4(fog_plane) \
	FX_F4(fog_params) \
	FX_F4(fog_color) \
	FX_F4(L_sun_color) \
	FX_F4(L_sun_dir_w) \
	FX_F4(L_sun_dir_e) \
	FX_F4(L_hemi_color) \
	FX_F4(L_ambient) \
	FX_F4(L_sky_color) \
	FX_F4(env_wind) \
	FX_F4(water_intensity) \
	FX_F4(sun_shafts_intensity) \
	FX_F4(rain_params) \
	FX_F4(nvg_color) \
	FX_F4(m_hud_params) \
	FX_F4(m_zoom_deviation) \
	FX_F4(m_affects) \
	FX_F4(m_actor_params) \
	FX_F4(m_timearrow) \
	FX_F4(m_timearrow2) \
	FX_F4(test_exp_to_shaders_1) \
	FX_F4(test_exp_to_shaders_2) \
	FX_F4(color_params) \
	FX_F4(color_grading) \
	FX_F4(c_brightness) \
	FX_F4(c_colormap)

#define CB_FRAME_HUD_RAIN \
	FX_F4(hud_rain)

#define CB_VIEW_FIELDS \
	FX_F3X4(m_V) \
	FX_F4X4(m_P) \
	FX_F4X4(m_VP) \
	FX_F3X4(m_invV) \
	FX_F4X4(m_invP) \
	FX_F4X4(m_VP_old) \
	FX_F4X4(m_invVP_old) \
	FX_F3(eye_position) \
	FX_F3(eye_direction) \
	FX_F3(eye_normal) \
	FX_F4(m_taa_jitter) \
	FX_F4(screen_res) \
	FX_F4(scaled_screen_res) \
	FX_F4(pos_decompression_params2)

#define CB_OBJECT_XFORM \
	FX_F3X4(m_W) \
	FX_F3X4(m_invW) \
	FX_F3X4(m_WV) \
	FX_F4X4(m_WVP) \
	FX_F4X4(m_WVP_old)

#define CB_OBJECT_PROPS \
	FX_F4(L_dynamic_props) \
	FX_F4X4(m_plmap_xform) \
	FX_F4A(m_plmap_clamp, 2)

#define CB_PASS_FIELDS \
	FX_F4X4(m_P_hud) \
	FX_F4X4(m_texgen) \
	FX_F4X4(m_xform) \
	FX_F4X4(m_xform_v) \
	FX_F4(consts) \
	FX_F4(wave) \
	FX_F4(wind) \
	FX_F4(consts_old) \
	FX_F4(wave_old) \
	FX_F4(wind_old) \
	FX_F4(c_scale) \
	FX_F4(c_bias) \
	FX_F4(c_sun) \
	FX_F4X4(m_invP_hud) \
	FX_F4(mblur_params)

#define CB_PASS_REFLECTION \
	FX_F3X4(m_reflectionV) \
	FX_F3X4(m_invReflectionV) \
	FX_F4(reflection_params) \
	FX_F4(reflection_history_jitter)

#define CB_MATERIAL_FIELDS \
	FX_F4(L_material) \
	FX_F4(hemi_cube_pos_faces) \
	FX_F4(hemi_cube_neg_faces) \
	FX_F4(dt_params) \
	FX_F4(parallax) \
	FX_F1(def_aref) \
	FX_F1(m_AlphaRef) \
	FX_PAD2(_pad_material) \
	FX_F4(L_model_light_color) \
	FX_F4(L_model_light_dir) \
	FX_F4(triLOD) \
	FX_F4A(m_lmap, 2) \
	FX_F4(tfactor)

#define CB_LIGHT_FIELDS \
	FX_F4(Ldynamic_color) \
	FX_F4(Ldynamic_pos) \
	FX_F4(Ldynamic_dir) \
	FX_INT(Ldynamic_hud) \
	FX_PAD3(_pad_light) \
	FX_F4X4A(m_shadow_sun, 3) \
	FX_F4X4(m_shadow) \
	FX_F3X4(m_sunmask)

#define CB_LIGHT_XFORM \
	FX_F4X4(L_dynamic_xform)

// 3x4 storage keeps the transposed columns plus translation. Decode restores an affine matrix.
FX_FN void encode_float4x4(FX_OUT(FX_V4, s0), FX_OUT(FX_V4, s1), FX_OUT(FX_V4, s2), FX_OUT(FX_V4, s3), FX_V4 r0, FX_V4 r1, FX_V4 r2, FX_V4 r3)
{
	s0 = FX_V4(r0.x, r1.x, r2.x, r3.x);
	s1 = FX_V4(r0.y, r1.y, r2.y, r3.y);
	s2 = FX_V4(r0.z, r1.z, r2.z, r3.z);
	s3 = FX_V4(r0.w, r1.w, r2.w, r3.w);
}

FX_FN void decode_float4x4(FX_OUT(FX_V4, r0), FX_OUT(FX_V4, r1), FX_OUT(FX_V4, r2), FX_OUT(FX_V4, r3), FX_V4 s0, FX_V4 s1, FX_V4 s2, FX_V4 s3)
{
	encode_float4x4(r0, r1, r2, r3, s0, s1, s2, s3);
}

FX_FN void encode_float3x4(FX_OUT(FX_V4, s0), FX_OUT(FX_V4, s1), FX_OUT(FX_V4, s2), FX_V4 r0, FX_V4 r1, FX_V4 r2, FX_V4 r3)
{
	s0 = FX_V4(r0.x, r1.x, r2.x, r3.x);
	s1 = FX_V4(r0.y, r1.y, r2.y, r3.y);
	s2 = FX_V4(r0.z, r1.z, r2.z, r3.z);
}

FX_FN void decode_float3x4(FX_OUT(FX_V4, r0), FX_OUT(FX_V4, r1), FX_OUT(FX_V4, r2), FX_OUT(FX_V4, r3), FX_V4 s0, FX_V4 s1, FX_V4 s2)
{
	r0 = FX_V4(s0.x, s1.x, s2.x, 0.f);
	r1 = FX_V4(s0.y, s1.y, s2.y, 0.f);
	r2 = FX_V4(s0.z, s1.z, s2.z, 0.f);
	r3 = FX_V4(s0.w, s1.w, s2.w, 1.f);
}

#undef FX_V4
#undef FX_FN
#undef FX_OUT

#ifdef __cplusplus
FX_CBUFFER(CBFrame, cb_frame, b0)
CB_FRAME_FIELDS
CB_FRAME_HUD_RAIN
FX_CBUFFER_END()

FX_CBUFFER(CBView, cb_view, b1)
CB_VIEW_FIELDS
FX_CBUFFER_END()

FX_CBUFFER(CBObject, cb_object, b2)
CB_OBJECT_XFORM
CB_OBJECT_PROPS
FX_CBUFFER_END()

FX_CBUFFER(CBMaterial, cb_material, b3)
CB_MATERIAL_FIELDS
FX_CBUFFER_END()

FX_CBUFFER(CBLight, cb_light, b4)
CB_LIGHT_FIELDS
CB_LIGHT_XFORM
FX_CBUFFER_END()

FX_CBUFFER(CBPass, cb_pass, b5)
CB_PASS_FIELDS
CB_PASS_REFLECTION
FX_CBUFFER_END()

#undef FX_CBUFFER
#undef FX_CBUFFER_END
#undef FX_F4
#undef FX_F4A
#undef FX_F3
#undef FX_F3X4
#undef FX_F4X4
#undef FX_F4X4A
#undef FX_INT
#undef FX_F1
#undef FX_PAD2
#undef FX_PAD3
#undef CB_FRAME_FIELDS
#undef CB_FRAME_HUD_RAIN
#undef CB_VIEW_FIELDS
#undef CB_OBJECT_XFORM
#undef CB_OBJECT_PROPS
#undef CB_PASS_FIELDS
#undef CB_PASS_REFLECTION
#undef CB_MATERIAL_FIELDS
#undef CB_LIGHT_FIELDS
#undef CB_LIGHT_XFORM
#endif

#endif
