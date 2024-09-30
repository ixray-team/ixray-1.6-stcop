#include "common.hlsli"
#include "shadow.hlsli"

struct v2p
{
    float3 lightToPos : TEXCOORD0; // light center to plane vector
    float3 vPos : TEXCOORD1; // position in camera space
    float fDensity : TEXCOORD2; // plane density along Z axis
};

uniform float4 m_lmap[2];
Texture2D s_noise;

#define USE_LMAP
#define USE_LMAPXFORM
#define USE_SHADOW

// Pixel
float4 main(v2p I) : SV_Target
{
    // ----- shadow
    float4 P4 = float4(I.vPos, 1);
    float4 PS = mul(m_shadow, P4);
    float s = 1.0f;
	
#ifdef USE_SHADOW
    s = shadow(PS);
#endif

    // ----- lightmap
    float4 lightmap = 1.0f;
	
#ifdef USE_LMAP
    #ifdef USE_LMAPXFORM
		PS.x = dot(P4, m_lmap[0]);
		PS.y = dot(P4, m_lmap[1]);
    #endif
    lightmap = s_lmap.Sample(smp_rtlinear, PS.xy / PS.w);
#endif

    // ----- attenuate
    float rsqr = dot(I.lightToPos, I.lightToPos); // distance 2 light (squared)
    float att = saturate(1 - rsqr * Ldynamic_pos.w); // q-linear attenuate

    // ----- noise
    PS.xy /= PS.w;
    PS.xy *= 0.3333;
    float time = timers.z * 0.1;
    PS.x += time;
	
    float4 t_noise = s_noise.SampleLevel(smp_linear, PS.xy, 0);
	
    PS.x -= time;
    PS.y -= time * 0.70091;
	
    t_noise *= s_noise.SampleLevel(smp_linear, PS.xy, 0);
    t_noise = t_noise * 0.5 + 0.5;

    // out
    float maxIntens = I.fDensity;
    float3 result = maxIntens * s * att;
	
    result *= lightmap.xyz;
    result *= Ldynamic_color.xyz * t_noise.xyz;

    return float4(result, 0);
}

