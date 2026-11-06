#include "common.hlsli"
#include "ScreenSpaceContactShadows.hlsl"

float3 calculate_cascade_dir(float3 V_P)
{
	float3 P = mul(m_invV, float4(V_P, 1.0f));

	float4 cascade_proj = mul(m_shadow_sun[0], float4(P, 1.0f));
	if (all(abs(cascade_proj.xyz / cascade_proj.w - 0.5f) < 0.5f)) {
		float3 dir = normalize(cross(m_shadow_sun[0][0].xyz, m_shadow_sun[0][1].xyz));
		float3 proj_dir = dot(m_shadow_sun[0][2].xyz, dir) < 0.0f ? -dir : dir;
		return normalize(mul(m_V, float4(proj_dir, 0.0f)));
	}

	return Ldynamic_dir.xyz;
}

float main(VSOutputFullscreen I) : SV_Target
{
	IXRayGbuffer O = (IXRayGbuffer)NULL;
	GbufferUnpackDepth((uint2)I.hpos.xy, O);

	float3 N = NormalDecode(s_normal.Load(int3(I.hpos.xy, 0)).xy * 2.0f - 1.0f);
	float fade = saturate((sun_contact_fade.x - O.Point.z) * sun_contact_fade.y);

	if (O.Depth < 0.02f || fade <= 0.0f) {
		return 1.0f;
	}

	float3 L = calculate_cascade_dir(O.Point);
	float NdotL = -dot(N, L);
	if (NdotL <= 0.0f) {
		return 1.0f;
	}
	fade *= saturate(NdotL / max(sun_contact_fade.w, 0.001f));

	float jitter = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, m_taa_jitter.z < 0.0f ? 0 : uint(m_taa_jitter.w) % 32)].x;
	return 1.0f - fade * SunContactOcclusion(I.texcoord, N * 0.025f + O.Point * 0.999f, N, L, jitter);
}
