#include "rain_instanced.hlsli"
// Rain uses cubemap lighting, independently of the opaque screen-reflection permutation.
#ifdef USE_VIEW_REFLECTIONS
#undef USE_VIEW_REFLECTIONS
#endif
#include "metalic_roughness_ambient.hlsli"

float4 main(RainVertex I) : SV_Target0
{
	float Feather = RainShape.w;
	float2 P = float2(I.UV.x * 2.0f - 1.0f, 1.0f - I.UV.y * 2.0f);
	float R2 = dot(P, P);
	float Edge = max(length(fwidth(P)), 1e-4f);
    
    //float F = saturate(1.0f - sqrt(R2) + 0.5f * Edge) / (Edge + Feather);
    //float Coverage = smoothstep(0.0f, 1.0f, F) * I.Fade;
    
	float Coverage = saturate((1.0f - sqrt(R2) + 0.5f * Edge) / (Edge + Feather)) * I.Fade;
	clip(Coverage - 1e-4f);
	float H = sqrt(saturate(1.0f - R2));
	float3 Facing = -normalize(cross(I.AxisX, I.AxisY));
	float3 LocalNormal = float3(P, H);
       
	//LocalNormal = normalize(s_base.SampleLevel(smp_base, I.UV, 0).xyz * 2.0f - 1.0f); // for normal map, if needed
    
	float3 N = normalize(I.AxisX * LocalNormal.x + I.AxisY * LocalNormal.y + Facing * LocalNormal.z);
	float3 Incident = normalize(I.Point);
	if (dot(Facing, Incident) > 0.0f)
	{
		N = -N;
	}
    
	//float Fog = saturate(length(I.Point) * fog_params.w + fog_params.x);
	//Coverage *= 1.0f - Fog;

	[branch] if (RainShading.x < 0.5f)
	{
		float3 Light = CompureDiffuseIrradance(N, 0.85f) + L_ambient.rgb;
		return float4((Light * RainColor.rgb), Coverage * saturate(0.333f * RainColor.a));
	}

	float NdotV = saturate(dot(N, -Incident)); // Incident * 0.5 + 0.5
	float Grazing = 1.0f - NdotV;
	float Fresnel = 0.0204f + 0.9796f * Grazing * Grazing * Grazing * Grazing * Grazing;
	float3 Reflected = RainColor.rgb * CompureSpecularIrradance(reflect(Incident, N), 1.0f, 0.08f);
    Reflected = GammaToLinear(Reflected);
    
	float Wr = Fresnel;
	float Wt = 0.0f;
	float3 Transmitted = 0.0f;
	float2 UV = I.HPos.xy * scaled_screen_res.zw;
	float2 Offset = N.xy * float2(1, -1) * H * 64.f * RainShading.w * (1.0f - 1.0f / 1.333f); // 64 to increase radius
	UV = clamp(UV + Offset * screen_res.zw, 0.5f * screen_res.zw, 1.0f - 0.5f * screen_res.zw);
    Transmitted = s_image.SampleLevel(smp_rtlinear, UV, 0).rgb + GammaToLinear(L_ambient.rgb);
	Wt = RainShading.z * (1.0f - Fresnel);
	float Weight = Wr + Wt;
    float3 Color = Wr * Reflected + Wt * Transmitted / max(Weight, 1e-5f); //(Wr * Reflected + Wt * Transmitted) / max(Weight, 1e-5f);
	Color = lerp(Color, RainColor.rgb, RainShading.y);
    
	return float4(Color, Coverage * saturate(Weight * RainColor.a));
}
