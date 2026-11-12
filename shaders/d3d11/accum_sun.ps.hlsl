#include "common.hlsli"
#include "metalic_roughness_light.hlsli"
#include "ScreenSpaceContactShadows.hlsl"
#include "shadow.hlsli"

Texture2D s_sun_contact : register(t26);

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

	if (O.Depth < 0.02f) {
		Point.xyz = -Ldynamic_dir.xyz * 0.4f;
	}

	int cascade_index;
	float3 smap_texcoord;
	float Shadow = 1.0f;
	
	float3 world = mul(m_invV, Point).xyz;
	bool is_in_bounds = calc_cascades(world, m_shadow_sun, cascade_index, smap_texcoord);
	if (is_in_bounds) {
		Shadow *= shadow_sun(smap_texcoord, cascade_index);

		float2 edge = smoothstep(0.40f, 0.49f, abs(smap_texcoord.xy - 0.5f));
		float blend = max(edge.x, edge.y);

		[unroll] for (int next_index = 1; next_index < 3; ++next_index) {
			// A cascade is a column along the sun: looking towards the sun it also covers receivers far beyond its range,
			// where its texels are several times smaller than a pixel and the shadow aliases under the camera jitter
			if (next_index == cascade_index + 1) {
				blend = max(blend, smoothstep(0.75f, 1.0f, O.ViewDist * length(m_shadow_sun[next_index - 1][0].xyz)));
			}
			[branch] if (next_index == cascade_index + 1 && blend > 0.0f) {
				float4 next = mul(m_shadow_sun[next_index], float4(world, 1.0f));
				float3 coord = next.xyz / next.w;
				bool valid = SunCacheParams.x > 0.0f ? CorrectSunShadowCoord(coord, m_shadow_sun[next_index], next_index, coord) : all(abs(coord - 0.5f) < 0.5f);
				if (valid) {
					Shadow = lerp(Shadow, shadow_sun(coord, next_index), blend);
				}
			}
		}
	}
	
	if (cascade_index >= 2) {
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
	
#ifdef USE_LEGACY_LIGHT
    float3 Light = DirectLightLegacy(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.View.xyz, O.Color, O.Material, O.Gloss);
#else
    float3 Light = DirectLight(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.View.xyz, O.Color, O.Specular, O.Roughness);
#endif

	if (O.Depth < 0.02f) {
		Light *= terminator_gate(int2(I.hpos.xy), O.PointHud, O.Normal, Ldynamic_dir.xyz);
	}

#ifdef USE_HUD_SHADOWS
	// Contact shadows treat back-lit surfaces as occluded by themselves, so keep translucency out of them
	if (O.Depth < 0.02f && dot(Shadow.xxx, Light.xyz) > EPS) {
		Light *= RayTraceContactShadow(I.texcoord, O.PointHud, O.Normal, Ldynamic_dir.xyz);
	}
#endif

	if (O.MaterialID == FOLIAGE_ID || O.MaterialID == TERRAIN_ID) {
		Light += SimpleTranslucency(Ldynamic_color.xyz, Ldynamic_dir.xyz, O.Normal) * O.SSS * O.Color;
	} else {
		const float HudSkinTranslucency = 0.35f;
		float SkinScale = O.Depth < 0.02f ? HudSkinTranslucency : 1.0f;
		Light += SkinTranslucency(Ldynamic_color.xyz, Ldynamic_dir.xyz, O.Normal, O.View) * O.SSS * SkinScale * O.Color;
	}

	if (Shadow > 0.0f) {
		Light *= GammaToLinear(min(1.0f, s_sun_contact.Load(int3(I.hpos.xy, 0)).x / Shadow));
	}

	Light *= GammaToLinear(Shadow);
	return float4(Light, 0);
}
