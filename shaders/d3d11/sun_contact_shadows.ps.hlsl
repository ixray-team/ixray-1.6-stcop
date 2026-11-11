#include "common.hlsli"
#include "ScreenSpaceContactShadows.hlsl"

float main(VSOutputFullscreen I) : SV_Target
{
	IXRayGbuffer O = (IXRayGbuffer)NULL;
	GbufferUnpackDepth((uint2)I.hpos.xy, O);
	if (O.Depth < 0.02f || O.Depth >= 1.0f) {
		return 1.0f;
	}

	float3 world = mul(m_invV, float4(O.Point, 1.0f));

	float covered = 0.0f;
	for (int cascade = 0; cascade < SunContactStart; ++cascade) {
		float4 cascade_ndc = mul(m_shadow_sun[cascade], float4(world, 1.0f));
		float3 coverage = smoothstep(float3(0.49f, 0.49f, 0.5f), float3(0.40f, 0.40f, 0.49f), abs(cascade_ndc.xyz / cascade_ndc.w - 0.5f));
		covered = max(covered, coverage.x * coverage.y * coverage.z);
	}
	float fade = 1.0f - covered;

	float4 normal = s_normal.Load(int3(I.hpos.xy, 0));
	float3 N = NormalDecode(normal.xy * 2.0f - 1.0f);
	float nol = -dot(N, Ldynamic_dir.xyz);

	bool foliage = uint(normal.w * MAX_ID) == FOLIAGE_ID;
#ifndef USE_LEGACY_LIGHT
	// Trees and other alpha-tested foliage get no contact shadows, details do (see the SSS bit in deffer_base)
	if (foliage && !(uint(s_surface.Load(int3(I.hpos.xy, 0)).w * 255.0f + 0.5f) & 1)) {
		return 1.0f;
	}
#endif
	if (foliage) {
		N *= sign(nol);
		nol = abs(nol);
	}
	fade *= saturate(nol * 5.0f);

	if (fade <= 0.0f) {
		return 1.0f;
	}

	float jitter = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, m_taa_jitter.z < 0.0f ? 0 : uint(m_taa_jitter.w) % 32)].x;
	float3 ng = foliage ? 0.0f : geometric_normal(int2(I.hpos.xy), O.Point);
	return 1.0f - fade * SunContactOcclusion(I.texcoord, N * 0.025f + O.Point * 0.999f, ng, Ldynamic_dir.xyz, jitter);
}
