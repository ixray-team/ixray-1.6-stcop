#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "metalic_roughness_light.hlsli"

struct PSInput
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0;
};

float4 main(PSInput I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);
	
	if(O.Depth >= 1.0f) {
		return float2(0.0f, O.Depth).xxxy;
	}
	
	float4 SSLRMain = float4(BiteralReflectionsFiler(I.texcoord, s_image), O.Depth);

	
	float2 PrevDiffuseUV = I.texcoord.xy + s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy, 0).xy * float2(-0.5f, 0.5f);
	
    float4 SSLR_OldDiffyse = s_base.SampleLevel(smp_rtlinear, PrevDiffuseUV.xy, 0.0f);
	SSLR_OldDiffyse = lerp(SSLRMain, SSLR_OldDiffyse, GetBorderAtten(PrevDiffuseUV));
	
	float DepthClamp = 1.0f - saturate(10.0f * abs(SSLR_OldDiffyse.w - O.Depth));
	
	if(O.Depth < 0.02f) {
		DepthClamp = 1.0f - saturate(80.0f * abs(SSLR_OldDiffyse.w - O.Depth) * rcp(O.Depth) - 0.5f);
	}
	
	SSLRMain.xyz = lerp(SSLRMain.xyz, SSLR_OldDiffyse.xyz, 0.98f * DepthClamp);
	return saturate(SSLRMain);
}

