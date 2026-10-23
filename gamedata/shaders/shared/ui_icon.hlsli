#ifndef UI_ICON_H
#define UI_ICON_H

// 3D inventory icon lighting shared by the D3D9 renderers (matches the D3D11 legacy path).
uniform float4 ui_icon_light; // rgb = light color * intensity, w = ambient scale
uniform float4 ui_icon_direction; // view-space light direction
uniform float4 ui_static_color; // icon tint (rgb) and opacity (w)
uniform float4 ui_emissive_tint;

float3 UIIconLightDirection()
{
    return dot(ui_icon_direction.xyz, ui_icon_direction.xyz) > 0.0001f ? normalize(ui_icon_direction.xyz) : float3(0.0f, 0.0f, 1.0f);
}

float3 UIIconTonemap(float3 Color)
{
    Color = max(Color, 0.0f) * 1.5f;
    return Color * (1.0f + Color * 0.34602f) / (Color + 1.0f);
}

float4 UIIconFinalize(float3 Color, float Alpha)
{
    return float4(UIIconTonemap(Color) * ui_static_color.xyz, saturate(Alpha) * ui_static_color.w);
}

#endif
