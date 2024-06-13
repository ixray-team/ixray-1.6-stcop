#ifndef	metalic_roughness_ambient_h_ixray_included
#define	metalic_roughness_ambient_h_ixray_included
#include "common.hlsli"

float3 FresnelSchlickRoughness(float3 F, float NdotV, float Roughness) {
    return F + max(0.0f, (1.0f - Roughness) - F) * pow(1.0f - NdotV, 5.0f);
}

float2 EpicGamesEnvBRDFApprox(float NdotV, float Roughness) {
	float4 R = Roughness * float4(-1, -0.0275, -0.572, 0.022) + float4(1, 0.0425, 1.04, -0.04);
	float A004 = min(R.x * R.x, exp2(-9.28 * NdotV)) * R.x + R.y;
	return float2(-1.04, 1.04) * A004 + R.zw;
}

void RemapVector(inout float3 vreflect)
{
	float3 vreflectabs = abs(vreflect);
	float vreflectmax = max(vreflectabs.x, max(vreflectabs.y, vreflectabs.z));
	vreflect /= vreflectmax; vreflect.y = vreflect.y * 2.0 - 1.0;
}

float3 CompureDiffuseIrradance(float3 N, float Hemi)
{
	float3 LightDirection = mul(m_invV, N).xyz;
	
	float3 SampleLast = env_s0.SampleLevel(smp_rtlinear, LightDirection, 0.0f);
	float3 SampleNext = env_s1.SampleLevel(smp_rtlinear, LightDirection, 0.0f);
	
	float3 Irradance = L_hemi_color.xyz * lerp(SampleLast, SampleNext, L_hemi_color.w);
	
	return Irradance * Irradance * Hemi;
}

float3 CompureSpecularIrradance(float3 R, float Hemi, float Roughness)
{
	float3 LightDirection = mul(m_invV, R); float4 MipLevels = 0.0f;
	sky_s0.GetDimensions(MipLevels.x, MipLevels.y, MipLevels.z, MipLevels.w);
	
	float Lod = MipLevels.w * Roughness;
	
	LightDirection.y = abs(LightDirection.y);
 	RemapVector(LightDirection);
	
	float3 SampleLast = sky_s0.SampleLevel(smp_rtlinear, LightDirection, Lod);
	float3 SampleNext = sky_s1.SampleLevel(smp_rtlinear, LightDirection, Lod);
	
	float3 Irradance = L_sky_color.xyz * lerp(SampleLast, SampleNext, L_hemi_color.w);
	return Irradance * Hemi * 0.8f;
}

float3 AmbientLighting(float3 Point, float3 Normal, float3 Color,
	float Metalness, float Roughness, float Hemi) {
		
	float3 N = normalize(Normal);
	float3 V = normalize(-Point);
	float3 R = reflect(-V, N);
	
#ifndef USE_LEGACY_LIGHT
	float3 DiffuseIrradance = CompureDiffuseIrradance(N, Hemi) + L_ambient.xyz;
	float3 SpecularIrradance = CompureSpecularIrradance(R, Hemi, Roughness);
	
	float NdotV = max(0.0, dot(N, V));	
	
	float2 BRDF = EpicGamesEnvBRDFApprox(NdotV, Roughness);
	float3 F = lerp(F0, Color, Metalness) * BRDF.x + BRDF.y;

	float3 DiffuseBRDF = (1.0f - F) * (1.0f - Metalness) * Color * DiffuseIrradance;
	float3 SpecularBRDF = F * SpecularIrradance;

	return SpecularBRDF + DiffuseBRDF;
#else
	float Specular = dot(V, R) * 0.5f + 0.5f;
	float2 Material = s_material.Sample(smp_material, float3(Hemi, Specular, Metalness)).xy;
	
	float3 DiffuseIrradance = CompureDiffuseIrradance(N, Material.x) + L_ambient.xyz;
	float3 SpecularIrradance = CompureDiffuseIrradance(R, Material.y);
	
	return DiffuseIrradance * Color + SpecularIrradance * Roughness;
#endif
}
#endif