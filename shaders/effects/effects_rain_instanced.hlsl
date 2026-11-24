/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
#include "effects\rain_instanced.hlsli"
struct RainInstance
{
	float3 Position;
	uint Seed;
	float3 Direction;
	float Speed;
};
StructuredBuffer<RainInstance> RainInstances;

RainVertex vs_main(PULL_INPUT, uint InstanceID : SV_InstanceID)
{
	RingVertex I = PullRing(VertexId);
	RainInstance Drop = RainInstances[InstanceID];
	float3 Head = mul(m_V, float4(Drop.Position, 1.0f));
	float3 Direction = mul((float3x3)m_V, Drop.Direction);
	float DistanceSquared = dot(Head, Head);
	float3 Ray = DistanceSquared > 1e-6f ? Head * rsqrt(max(DistanceSquared, 1e-6f)) : float3(0, 0, 1);
	float3 Along = -Direction + Ray * dot(Direction, Ray);
	float Len2 = dot(Along, Along);
	float3 Reference = abs(Ray.y) < 0.99f ? float3(0, 1, 0) : float3(1, 0, 0);
	float3 AxisX = normalize(cross(Len2 > 1e-6f ? Along : Reference, Ray));
	float3 AxisY = cross(Ray, AxisX);
	float Radius = lerp(0.5f, 1.0f, RainRandom(Drop.Seed)) * RainShape.z;
	// Looking along the trajectory shows the round cross-section instead of a full-length streak.
	float HalfLength = lerp(min(RainShape.x, RainShape.y), RainShape.y, sqrt(saturate(Len2)));
	// Keep the leading edge at Head for every scale and viewing angle; only the tail changes length.
	float3 Point = Head + Radius * (AxisX * I.P.x * RainShape.x + AxisY * (I.P.y + 1.0f) * HalfLength);
	return RainProject(Point, I.UV, AxisX, AxisY, 1.0f);
}

// Rain uses cubemap lighting, independently of the opaque screen-reflection permutation.
#ifdef USE_VIEW_REFLECTIONS
#undef USE_VIEW_REFLECTIONS
#endif
#include "common\metalic_roughness_ambient.hlsli"

float4 ps_main(RainVertex I) : SV_Target0
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
    Reflected = SceneFromGamma(Reflected);
    
	float Wr = Fresnel;
	float Wt = 0.0f;
	float3 Transmitted = 0.0f;
	float2 UV = I.HPos.xy * scaled_screen_res.zw;
	float2 Offset = N.xy * float2(1, -1) * H * 64.f * RainShading.w * (1.0f - 1.0f / 1.333f); // 64 to increase radius
	// The picture behind the drop is of the frame before, in the view that frame had
	UV = max(ViewToTargetBefore((UV + Offset * screen_res.zw) / view_uv.xy), 0.5f * screen_res.zw);
    Transmitted = s_image.SampleLevel(smp_rtlinear, UV, 0).rgb + SceneFromGamma(L_ambient.rgb);
	Wt = RainShading.z * (1.0f - Fresnel);
	float Weight = Wr + Wt;
    float3 Color = Wr * Reflected + Wt * Transmitted / max(Weight, 1e-5f); //(Wr * Reflected + Wt * Transmitted) / max(Weight, 1e-5f);
	Color = lerp(Color, RainColor.rgb, RainShading.y);
    
	return float4(Color, Coverage * saturate(Weight * RainColor.a));
}
