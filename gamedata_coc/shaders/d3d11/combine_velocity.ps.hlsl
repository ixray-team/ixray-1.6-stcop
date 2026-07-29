#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_light.hlsli"

struct PSInput
{
    float4 hpos : SV_POSITION;
    float2 TexCoord : TEXCOORD0;
};

float2 main(PSInput I) : SV_Target
{
	float Depth = s_position.Load(uint3(I.hpos.xy, 0)).x;
	clip(0.9999f - Depth);
	
	if(Depth < 0.02f)
	{
		return 0.0f;
	}
	
	float3 Point = GbufferGetPointRealUnjitter(I.TexCoord.xy, Depth);
	
	float4 PrevTexCoord = mul(m_VP_old, float4(mul(m_invV, float4(Point, 1.0f)).xyz, 1.0f));
	PrevTexCoord.xy /= PrevTexCoord.w;
	
	I.TexCoord.x = I.TexCoord.x * 2.0f - 1.0f;
	I.TexCoord.y = 1.0f - I.TexCoord.y * 2.0f;
	
	return I.TexCoord.xy - PrevTexCoord;
}

