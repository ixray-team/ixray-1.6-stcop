#include"common.hlsli"
#include"shadow.hlsli"

struct PSInput
{
	float4 hpos : SV_Position;
	float4 hpos2d : TEXCOORD0;
};

float4 main(PSInput s) : SV_Target
{
	uint2 d = uint2(s.hpos.xy);
	uint m = (d.x ^ d.y) << 1u;

	float n = float((m & 4u | d.y & 2u) >> 1u | (m & 2u | d.y & 1u) << 2u) * .0625;
	float2 f = s.hpos2d.xy / s.hpos2d.w * float2(.5,-.5) + .5;

	float u = s_position.SampleLevel(smp_nofilter,f,0.).x;
	u = min(u,s.hpos.z);
    
	float3 h = GbufferGetPointRealUnjitter(f,u),e = mul(m_invV,float4(h,1.)).xyz;

	const uint x = 8;
	float3 y = e - eye_position;
	float r = rsqrt(dot(y,y)),L = 1. / r / x;
	float3 l = eye_position,w = y * r,z = w * L,p = l + z * n;
	float P = 0.;

	[unroll]
	for (uint c = 0; c < x; ++c)
	{
		float4 G = mul(m_shadow,float4(p,1.));
		G /= G.w;
		float t = s_smap.SampleCmpLevelZero(smp_smap,G.xy,G.z + 1e-4).x;
		float3 V = Ldynamic_pos.xyz - p.xyz;
		float o = dot(V,V), a = saturate(-o * Ldynamic_pos.w + 1.);

		a *= saturate(dot(-Ldynamic_dir.xyz,V * rsqrt(o)) + Ldynamic_dir.w); P += t * a; p += z;
	}

	P *= length(z); P = 1. - exp(-P / 8);

	return float4(PushGamma(P * Ldynamic_color.xyz),0.);
}