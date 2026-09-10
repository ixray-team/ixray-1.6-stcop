#include "common_sky.hlsli"
#include "metalic_roughness_light.hlsli"
#include "ScreenSpaceContactShadows.hlsl"
#include "shadow.hlsli"

Texture2D<float4> s_transmittance_lut;

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

float4 main(PSInput I) : SV_Target
{
    IXRayGbuffer O = (IXRayGbuffer)NULL;
    GbufferUnpack((uint2)I.hpos.xy, O);
	
	float3 Shift = O.Normal;
	
	if (O.SSS > 0.0f)
	{
		Shift *= dot(Ldynamic_dir.xyz, Shift) >= 0.0 ? -1.0f : 1.0f;
	}
	
	float4 Point = float4(O.Point.xyz, 1.f);
	Point.xyz = Shift * 0.025f + Point.xyz * 0.999f;
	
	int cascade_index;
	float3 smap_texcoord;
	bool is_in_bounds = calc_cascades(mul(m_invV, Point).xyz, m_shadow_sun, cascade_index, smap_texcoord);
	
	float Shadow = 1.0f;
	
	if(is_in_bounds)
	{
		Shadow *= shadow_sun(smap_texcoord, cascade_index);
	}
	
	if(cascade_index >= 2)
	{
		float3 Factor = smoothstep(0.5f, 0.49f, abs(smap_texcoord - 0.5f));
		float Fade = Factor.x * Factor.y * Factor.z;
	
		O.SSS *= 0.5f + 0.5f * Fade;	
		float FarShadow = dot(Ldynamic_dir.xyz, O.Normal.xyz);
		FarShadow = smoothstep(0.75f, 0.6f, FarShadow) * saturate(O.Hemi * 8.0f - 2.0f);
		Shadow = lerp(FarShadow, Shadow, Fade);
	}
	
#ifdef USE_SUNMASK
	Shadow *= sunmask(Point);
#endif
	
    float3 atmosphere_sun_direction = safe_normalize(-L_sun_dir_w);
    float camera_elevation_km = max(0.002f * eye_position.y + 0.2f, 0.0f);
    float3 atmosphere_position =float3(0.0f, SKY_EARTH_RADIUS + camera_elevation_km, 0.0f);
    float4 spectral_sun_transmittance = sky_transmittance_to_sun(s_transmittance_lut, smp_rtlinear, atmosphere_position, atmosphere_sun_direction);
    float3 atmospheric_sun_filter = sky_sun_transmittance_rgb(spectral_sun_transmittance);
	
#ifdef USE_LEGACY_LIGHT
    float3 Light = DirectLightLegacy(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.View.xyz, O.Color, O.Material, O.Gloss);
#else
    float3 Light = DirectLight(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.View.xyz, O.Color, O.Specular, O.Roughness);
#endif

    Light += SimpleTranslucency(Ldynamic_color.xyz, Ldynamic_dir.xyz, O.Normal) * O.SSS * O.Color;
    Light *= atmospheric_sun_filter;
#ifdef USE_HUD_SHADOWS
	if (O.Depth < 0.02f && dot(Shadow.xxx, Light.xyz) > EPS)
	{
		Light *= RayTraceContactShadow(I.texcoord, O.PointHud, Ldynamic_dir.xyz);
	}
#endif
	
	Light *= GammaToLinear(Shadow);
	return float4(Light, 0);
}