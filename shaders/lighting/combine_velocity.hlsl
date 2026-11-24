#include "common\common.hlsli"
#include "common\reflections.hlsli"
#include "common\metalic_roughness_light.hlsli"
#include "common\thin_surface.hlsli"

struct PSInput
{
    float4 hpos : SV_POSITION;
    float2 TexCoord : TEXCOORD0;
};

float2 ps_main(PSInput I) : SV_Target
{
	float Depth = s_position.Load(uint3(I.hpos.xy, 0)).x;
	// The sky of the forward level frame writes no velocity of its own
#ifndef USE_SURFACE_LIGHTS
	clip(0.9999f - Depth);
#endif
	
#ifdef USE_SURFACE_VELOCITY
	// A model of the forward level frame has its own motion, which the depth cannot tell: its depth pass has written it
	if (Depth < 0.9999f && thin_unpack(s_thin.Load(int3(I.hpos.xy, 0))).id == THIN_ID_MOVING)
	{
		discard;
	}
#endif

	// The HUD has its own motion, which the depth cannot tell: the forward level frame's depth pass has written it
	if(Depth < 0.02f)
	{
#ifdef USE_SURFACE_VELOCITY
		discard;
#endif
		return 0.0f;
	}
	
	// The triangle gives the target's UV; the motion is a share of the view
	I.TexCoord /= view_uv.xy;
	float3 Point = GbufferGetPointRealUnjitter(I.TexCoord.xy, Depth);
	
	float4 PrevTexCoord = mul(m_VP_old, float4(mul(m_invV, float4(Point, 1.0f)).xyz, 1.0f));
	PrevTexCoord.xy /= PrevTexCoord.w;
	
	I.TexCoord.x = I.TexCoord.x * 2.0f - 1.0f;
	I.TexCoord.y = 1.0f - I.TexCoord.y * 2.0f;
	
	return I.TexCoord.xy - PrevTexCoord.xy;
}
