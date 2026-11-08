#ifdef UI_ROUNDED_MSAA
Texture2DMS<float4> s_round_content;
Texture2DMS<float4> s_round_background;
#else
Texture2D<float4> s_round_content;
Texture2D<float4> s_round_background;
#endif
// Slots b0-b5 belong to the engine's fixed constants and are rebound on view restoration.
cbuffer UIRoundedPixelConstants : register(b6)
{
    float4 ui_round_shape;
    float4 ui_round_color;
    float4 ui_round_inverse0, ui_round_inverse1, ui_round_inverse2;
    float4 ui_round_depth_z, ui_round_depth_w, ui_round_depth_range;
};
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
    RoundedOutput Result;
    Result.Color = Background * (1.0 - Outer) + Content * (Outer - Stroke) + float4(ui_round_color.rgb, 1.0) * Stroke;
    float3 LocalPoint = float3(Local, 1.0);
    Result.Depth = ui_round_depth_range.x + ui_round_depth_range.y * saturate(dot(ui_round_depth_z.xyz, LocalPoint) / max(dot(ui_round_depth_w.xyz, LocalPoint), 1e-10));
    return Result;
}
