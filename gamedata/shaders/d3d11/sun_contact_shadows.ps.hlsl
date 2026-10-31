#include "common.hlsli"
#include "ScreenSpaceContactShadows.hlsl"

float main(VSOutputFullscreen I) : SV_Target
{
	IXRayGbuffer O = (IXRayGbuffer)NULL;
	GbufferUnpackDepth((uint2)I.hpos.xy, O);

	float3 Normal = NormalDecode(s_normal.Load(int3(I.hpos.xy, 0)).xy * 2.0f - 1.0f);
	float Fade = saturate((sun_contact_fade.x - O.Point.z) * sun_contact_fade.y);

	if (O.Depth < 0.02f || Fade <= 0.0f || dot(Normal, Ldynamic_dir.xyz) >= 0.0f)
	{
		return 1.0f;
	}

	float Jitter = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, m_taa_jitter.z < 0.0f ? 0 : uint(m_taa_jitter.w) % 32)].x;
	return 1.0f - Fade * SunContactOcclusion(I.texcoord, Normal * 0.025f + O.Point * 0.999f, Ldynamic_dir.xyz, Jitter);
}
