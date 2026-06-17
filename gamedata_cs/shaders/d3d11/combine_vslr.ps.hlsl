#include "common.hlsli"

inline void WrapOctP(inout float2 p)
{
    if (p.x > 1.0f)
    {
        p.x = 2.0f - p.x;
        p.y = -p.y;
    }
    else if (p.x < -1.0f)
    {
        p.x = -2.0f - p.x;
        p.y = -p.y;
    }

    if (p.y > 1.0f)
    {
        p.y = 2.0f - p.y;
        p.x = -p.x;
    }
    else if (p.y < -1.0f)
    {
        p.y = -2.0f - p.y;
        p.x = -p.x;
    }
}

float4 main(PSInputFullscreen I) : SV_Target
{
	float2 TexCoord = I.texcoord.xy * 2.0f - 1.0f;
	
	TexCoord /= 0.875f;
	WrapOctP(TexCoord);
	
	float3 View = NormalDecode(TexCoord);	
	View = mul((float3x3)m_V, View.xzy);
	
	float4 Env = s_env.SampleLevel(smp_linear, View.xyz, 0.0f);	
	Env.w = s_env_dist.SampleLevel(smp_linear, View.xyz, 0.0f).x;
	
	Env.w = saturate(2.5f * Env.w * fog_params.w + fog_params.x);
	
	Env.xyz = LinearToGamma(Env.xyz);
	Env.xyz *= rcp(1.0f + Env.xyz);
	
	return Env;
}

