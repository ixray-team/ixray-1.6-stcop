#ifdef UI_ROUNDED_MSAA
Texture2DMS<float4> s_round_content;
Texture2DMS<float4> s_round_background;
#else
Texture2D<float4> s_round_content;
Texture2D<float4> s_round_background;
#endif
Texture2D<float4> s_flashlight_mask;
SamplerState smp_base;
// Slots b0-b5 belong to the engine's fixed constants and are rebound on view restoration.
cbuffer UIRoundedPixelConstants : register(b6)
{
    float4 ui_round_shape;
    float4 ui_round_color;
    float4 ui_round_inverse0, ui_round_inverse1, ui_round_inverse2;
    float4 ui_round_depth_z, ui_round_depth_w, ui_round_depth_range;
    float4 ui_flashlight_spot; // center in pixels, radius in pixels, soft edge fraction
    float4 ui_flashlight_grade; // darkness, saturation, self-multiply amount, enabled
    float4 ui_flashlight_mask_rect; // center in pixels, inverse screen size
    float4 ui_flashlight_mask_state; // textured mask selected, valid mask, scripted brightness drop, isolated subtree
    float4 ui_flashlight_volume_source; // source XYZ, scattering density
    float4 ui_flashlight_volume_view; // internal view depth, distance attenuation, near clip, clip feather
    float4 ui_flashlight_volume_settings; // march steps (0 disables), time, camera center XY
    float4 ui_flashlight_dust; // count (0 disables), size, speed, opacity
    float4 ui_flashlight_volume_color; // independent RGB multiplier, white by default
    float4 ui_flashlight_dust_color;
    float4 ui_flashlight_dust_animation; // accumulated motion/glint phases, glint gain, explicit phases enabled
    float4 ui_flashlight_surface_color; // footprint RGB multiplier, 1 + dust intensity (0 for legacy binaries)
};
#include "ui_flashlight_volume.hlsli"
struct RoundedOutput
{
    float4 Color : SV_TARGET;
    float Depth : SV_DEPTH;
};

RoundedOutput main(float4 Position : SV_POSITION
#ifdef UI_ROUNDED_MSAA
    , uint SampleIndex : SV_SampleIndex
#endif
)
{
    int2 Pixel = int2(Position.xy);
#ifdef UI_ROUNDED_MSAA
    float4 Background = s_round_background.Load(Pixel, SampleIndex);
    float4 Content = s_round_content.Load(Pixel, SampleIndex);
#else
    float4 Background = s_round_background.Load(int3(Pixel, 0));
    float4 Content = s_round_content.Load(int3(Pixel, 0));
#endif
    float3 P = float3(Position.xy, 1.0);
    float W = dot(ui_round_inverse2.xyz, P);
    float2 Local = float2(dot(ui_round_inverse0.xyz, P), dot(ui_round_inverse1.xyz, P)) / max(W, 1e-10);
    float2 HalfSize = ui_round_shape.xy * 0.5;
    float2 Q = abs(Local - HalfSize) - (HalfSize - ui_round_shape.z);
    float Distance = length(max(Q, 0.0)) + min(max(Q.x, Q.y), 0.0) - ui_round_shape.z;
    float AA = max(fwidth(Distance), 1e-5);
    float Outer = saturate(0.5 - Distance / AA);
    if (W <= 0.0 || any(ui_round_shape.xy <= 0.0)) { Outer = 0.0; }
    float2 InnerHalfSize = max(HalfSize - ui_round_shape.w, 0.0);
    float InnerRadius = max(ui_round_shape.z - ui_round_shape.w, 0.0);
    float2 InnerQ = abs(Local - HalfSize) - (InnerHalfSize - InnerRadius);
    float InnerDistance = length(max(InnerQ, 0.0)) + min(max(InnerQ.x, InnerQ.y), 0.0) - InnerRadius;
    float InnerAA = max(fwidth(InnerDistance), 1e-5);
    float Inner = saturate(0.5 - InnerDistance / InnerAA);
    if (any(InnerHalfSize <= 0.0)) { Inner = 0.0; }
    // With no stroke the inner and outer coverage must coincide exactly.
    if (ui_round_shape.w <= 0.0) { Inner = Outer; }
    float Stroke = max(Outer - Inner, 0.0) * ui_round_color.a;
    bool Isolated = ui_flashlight_grade.w > 0.5 && ui_flashlight_mask_state.w > 0.5;
    float Coverage = saturate(Content.a * (Outer - Stroke) + Stroke);
    RoundedOutput Result;
    Result.Color = Background * (1.0 - Outer) + Content * (Outer - Stroke) + float4(ui_round_color.rgb, 1.0) * Stroke;
    if (Isolated)
    {
        // The offscreen subtree is premultiplied; grade straight color before compositing once.
        Result.Color = float4((Content.rgb * (Outer - Stroke) + ui_round_color.rgb * Stroke) / max(Coverage, 1e-6), 1.0);
    }
    if (ui_flashlight_grade.w > 0.5)
    {
        float Power = FlashlightPower();
        float Cookie = FlashlightCookie(Position.xy);
        float3 Original = Result.Color.rgb;
        float Luminance = dot(Original, float3(0.2126, 0.7152, 0.0722));
        float3 Dark = lerp(Luminance.xxx, Original, ui_flashlight_grade.y);
        // Multiply by the image's own luminance: deepen shadow tones without shifting hue.
        Dark *= lerp(1.0, Luminance, ui_flashlight_grade.z);
        Dark *= 1.0 - ui_flashlight_grade.x;
        // Restore a luminous footprint independently of the weaker airborne shaft.
        // Power scales all emitted surface light, including the reflector's bright core.
        // Neutral grading permits an atmosphere-only parent above independently graded children.
        bool GradeSurface = ui_flashlight_grade.x > 0.0 || ui_flashlight_grade.y != 1.0 || ui_flashlight_grade.z > 0.0;
        float3 Surface = lerp(Dark, Original, Cookie) + Original * (GradeSurface ? 0.40 * Cookie * Cookie : 0.0);
        float EffectCoverage = Isolated ? 1.0 : Outer;
        Result.Color.rgb = lerp(Original, lerp(Dark, Surface, Power), EffectCoverage);
        float3 SurfaceColor = ui_flashlight_surface_color.w >= 1.0 ? ui_flashlight_surface_color.rgb : float3(1.0, 1.0, 1.0);
        Result.Color.rgb *= lerp(float3(1.0, 1.0, 1.0), SurfaceColor, Cookie * Power * EffectCoverage);
        Result.Color.rgb += FlashlightVolume(Position.xy) * EffectCoverage * Power;
    }
    if (Isolated)
    {
        Result.Color.rgb = Result.Color.rgb * Coverage + Background.rgb * (1.0 - Coverage);
        Result.Color.a = Coverage + Background.a * (1.0 - Coverage);
    }
    float3 LocalPoint = float3(Local, 1.0);
    Result.Depth = ui_round_depth_range.x + ui_round_depth_range.y * saturate(dot(ui_round_depth_z.xyz, LocalPoint) / max(dot(ui_round_depth_w.xyz, LocalPoint), 1e-10));
    return Result;
}
