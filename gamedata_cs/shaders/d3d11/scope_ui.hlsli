#ifndef SCOPE_UI_HLSLI
#define SCOPE_UI_HLSLI

Texture2D s_scope_ui;

float3 ApplyScopeUI(float3 Color, float2 ScreenPosition)
{
	// The item UI target has display resolution; the scope world can be scaled.
	float4 UI = s_scope_ui.SampleLevel(smp_rtlinear, ScreenPosition * scaled_screen_res.zw, 0);
	UI.xyz /= max(UI.w, EPS_S);
	return lerp(Color, UI.xyz, saturate(UI.w));
}

#endif
