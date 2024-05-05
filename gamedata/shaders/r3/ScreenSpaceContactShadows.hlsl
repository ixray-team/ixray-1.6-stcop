#ifndef screenspacecontactshadows_hlsl_included
#define screenspacecontactshadows_hlsl_included

#define HUD_SHADOWS_STEPS 35

float SampleHudHitPoint(float2 TexCoord) {
	float depth = s_position.SampleLevel(smp_nofilter, TexCoord, 0).x;
	return m_P_hud._34 / (min(1.0f, depth * 50.0f) - m_P_hud._33);
}

float2 GetPointTexCoord(float3 Point)
{
	Point.xy /= pos_decompression_params.xy * Point.z;
	return saturate(Point.xy * 0.5f + 0.5f);
}

void RayTraceContactShadow(float2 TexCoord, float3 Point, float3 LightInvDir, inout float Shadow) {
	#ifdef USE_HUD_SHADOWS
		float3 Dir = 0.07f * normalize(LightInvDir) / HUD_SHADOWS_STEPS;
		Point.xyz *= 0.99f; float ContactShadow = 0.0f;
		
		[unroll(HUD_SHADOWS_STEPS)]
		for(int i = 0; i < HUD_SHADOWS_STEPS; ++i) {
			Point += Dir * float(0.8f + 0.4f * Hash(TexCoord));
			TexCoord = GetPointTexCoord(Point);
			
			if(all(min(TexCoord, 1.0 - TexCoord))) {
				float HitPointZ = SampleHudHitPoint(TexCoord);
				if(HitPointZ <= Point.z) {
					ContactShadow += 0.2f;
					if(ContactShadow >= 1.0f) {
						ContactShadow = 1.0f;
						break;
					}
				}
			} else {
				return;
			}
		}
		
		Shadow *= 1.0f - saturate(ContactShadow);
	#endif
}
#endif