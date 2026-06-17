#include "common.hlsli"

uniform float4 m_hud_params;
uniform float4 m_zoom_deviation;

struct v2p
{
    float2 tc0: TEXCOORD0;
    float3 tc1: TEXCOORD1;
    float4 c0: COLOR0;
};

void main(v2p I, float4 pos2d : SV_POSITION, out IXRayForward O)
{
    if (m_hud_params.y * m_hud_params.a < 0.0001f)
	{
		discard;
	}

    float2 coords = I.tc0;
    coords.x *= 0.5f;
	
    float4 base2 = s_base.Sample(smp_base, coords);
    coords.x += 0.5f;
	
    float4 t_base = s_base.Sample(smp_base, coords);
	
    t_base = lerp(base2, t_base, m_zoom_deviation.z);
	t_base.xyz = detonemap(t_base.xyz);
	
    float4 t_vp2 = s_image[pos2d.xy];
	
	float alpha = m_hud_params.y * m_hud_params.a;
	t_vp2.w = saturate(alpha * 2.0f - 1.0f);
	t_vp2.xyz *= t_vp2.w * t_vp2.w;
	
	alpha = saturate(alpha * 2.0f);

    float3 final = lerp(t_vp2.xyz, t_base.xyz, t_base.a);
	float2 vel = 0.0f;
	
    O.Color = float4(final.xyz, alpha);
	O.Velocity = float4(vel, 0.0f, t_base.a * alpha);
}

