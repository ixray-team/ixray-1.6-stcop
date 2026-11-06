#ifndef screenspacecontactshadows_hlsl_included
#define screenspacecontactshadows_hlsl_included

#ifndef HUD_SHADOWS_STEPS
#define HUD_SHADOWS_STEPS 35
#endif

#ifndef HUD_SHADOWS_TRACE_LEN
	#define HUD_SHADOWS_TRACE_LEN 0.07f
#endif

#if SUN_CONTACT_QUALITY == 1
	#define SUN_CONTACT_STEPS 8.0f
#elif SUN_CONTACT_QUALITY == 2
	#define SUN_CONTACT_STEPS 12.0f
#else
	#define SUN_CONTACT_STEPS 16.0f
#endif

float3 uv_to_view(float2 uv, float depth)
{
	float2 ndc = uv * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f) - m_taa_jitter.xy;
	float2 scale = depth < 0.02f ? float2(m_invP_hud._11, m_invP_hud._22) : float2(m_invP._11, m_invP._22);
	return float3(ndc * scale, 1.0f) * GbufferGetViewZ(depth);
}

float3 screen_to_view(int2 pixel)
{
	return uv_to_view(pixel * pos_decompression_params2.zw, s_position.Load(int3(pixel, 0)).x);
}

float3 geometric_normal(int2 pixel, float3 center)
{
	float3 dx0 = center - screen_to_view(pixel - int2(1, 0));
	float3 dx1 = screen_to_view(pixel + int2(1, 0)) - center;
	float3 dy0 = center - screen_to_view(pixel - int2(0, 1));
	float3 dy1 = screen_to_view(pixel + int2(0, 1)) - center;

	float3 n0 = (abs(dx0.z) < abs(dx1.z)) ? dx0 : dx1;
	float3 n1 = (abs(dy0.z) < abs(dy1.z)) ? dy0 : dy1;
	if (max(abs(n0.z), abs(n1.z)) > center.z * 0.1f) {
		return 0.0f;
	}

	float3 ng = normalize(cross(n0, n1));
	return dot(ng, center) > 0.0f ? -ng : ng;
}

float terminator_gate(int2 pixel, float3 center, float3 normal, float3 L)
{
	float3 ng = geometric_normal(pixel, center);
	return any(ng) ? saturate(-dot(normalize(normal + ng), L) * 5.0f) : 1.0f;
}

float RayTraceContactShadow(float2 TexCoord, float3 Point, float3 L)
{
	float3 ng = geometric_normal(int2(TexCoord * pos_decompression_params2.xy), Point);
	float plane = any(ng) ? dot(ng, Point) + Point.z * 0.01f : -1.0f;

	Point.xyz *= 0.99f;
	L *= min(Point.z, HUD_SHADOWS_TRACE_LEN);
	
	float4 start = mul(m_P_hud, float4(Point, 1.0f)); start.xyz /= start.w;
	float4 end = mul(m_P_hud, float4(Point - L, 1.0f)); end.xyz /= end.w;
	
	start.xy = start.xy * float2(0.5f, -0.5f) + 0.5f; start.z *= 0.02f;
	end.xy = end.xy * float2(0.5f, -0.5f) + 0.5f; end.z *= 0.02f;
	
	L = end.xyz - start.xyz;
	start.xy = TexCoord.xy;
	
	L *= min(1.0f, GetMaxDirLength(start.xyz, rcp(L)));
	L *= rcp(HUD_SHADOWS_STEPS);
	
	float visibility = 0.0f;	
	
	[loop]
	for (int i = 0; i < HUD_SHADOWS_STEPS; ++i) {
		start.xyz += L * float(0.8f + 0.4f * Hash(start.xyz));
		float depth = s_position.SampleLevel(smp_nofilter, start.xy, 0).x;
		
		if (depth <= start.z && dot(ng, uv_to_view(start.xy, depth)) > plane) {
			visibility += 0.2f;
			if (visibility >= 1.0f) {
				visibility = 1.0f;
				break;
			}
		}
	}
	
	visibility *= GetBorderAtten(start.xy, 0.0125f);
	return 1.0f - saturate(visibility);
}

// Depth march after raymarch.hlsl by Tomasz Stachowiak (Tiny Glade), dual licensed Apache-2.0 / MIT
// Every term varies continuously with depth (fixed step count, no pixel-count clamp), so receding surfaces show no iso-depth lines
float SunContactOcclusion(float2 TexCoord, float3 Point, float3 Normal, float3 L, float Jitter)
{
	float4 Start = mul(m_P, float4(Point, 1.0f));
	float4 End = mul(m_P, float4(Point - L * Point.z * 0.06f, 1.0f));

	float3 Delta = float3((End.xy / End.w - Start.xy / Start.w) * float2(0.5f, -0.5f), rcp(End.w) - rcp(Start.w));
	
	// NOTE(vertver): far-distance shadow fade
	float Pixels = length(Delta.xy * pos_decompression_params2.xy);
	if (Pixels <= 8.0f) {
		return 0.0f;
	}

	Jitter *= saturate((Pixels - 8.0f) / 32.0f);

	float2 Exit = max((1.0f - TexCoord) / Delta.xy, -TexCoord / Delta.xy);
	Delta *= min(1.0f, min(Exit.x, Exit.y));

	float StartT = min(2.0f * rcp(length(Delta.xy * pos_decompression_params2.xy)), 0.5f);
	float Thickness = 0.5f + Point.z * 0.03f;
	float Bias = 0.02f + Point.z * 0.002f;
	float Plane = dot(Normal, Point);
	float Occlusion = 0.0f;

	[loop]
	for (float Step = 0.0f; Step < SUN_CONTACT_STEPS && Occlusion < 1.0f; ++Step) {
		float T = (Step + Jitter) / SUN_CONTACT_STEPS;
		float3 Ray = float3(TexCoord, rcp(Start.w)) + Delta * lerp(StartT, 1.0f, T * T);
		
		float4 Raw = s_position.GatherRed(smp_nofilter, Ray.xy);
		if (any(Raw < 0.02f)) {
			continue;
		}

		float4 Z = rcp(m_invP._43 * Raw + m_invP._44);
		float2 F = frac(Ray.xy * pos_decompression_params2.xy - 0.5f);
		float Filtered = rcp(m_invP._43 * lerp(lerp(Raw.w, Raw.z, F.x), lerp(Raw.x, Raw.y, F.x), F.y) + m_invP._44);
		float Nearest = F.y < 0.5f ? (F.x < 0.5f ? Z.w : Z.z) : (F.x < 0.5f ? Z.x : Z.y);
		float RayZ = rcp(Ray.z);

		// NOTE(vertver): grazing angle fix
		float2 Ndc = float2(Ray.x * 2.0f - 1.0f, 1.0f - Ray.y * 2.0f) - m_taa_jitter.xy;
		if (any(Normal) && min(Filtered, Nearest) * dot(Normal, float3(Ndc.x / m_P._11, Ndc.y / m_P._22, 1.0f)) - Plane < Bias) {
			continue;
		}

		float Edge = saturate(2.0f - (max(max(Z.x, Z.y), max(Z.z, Z.w)) - min(min(Z.x, Z.y), min(Z.z, Z.w))) / Thickness);
		float Hit = saturate((RayZ - max(Filtered, Nearest)) / Bias - 1.0f) * smoothstep(Thickness, 0.5f * Thickness, RayZ - min(Filtered, Nearest));
		Occlusion = max(Occlusion, Hit * Edge * GetBorderAtten(Ray.xy, 0.0125f));
	}

	return Occlusion * saturate(Pixels * 0.125f - 1.0f);
}
#endif

