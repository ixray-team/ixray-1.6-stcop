// All coordinates are physical UI pixels; Z is distance in front of the UI plane.
float FlashlightCookie(float2 Pixel)
{
    float Value = 0.0;
    if (ui_flashlight_mask_state.x > 0.5)
    {
        if (ui_flashlight_mask_state.y > 0.5)
        {
            float2 UV = (Pixel - ui_flashlight_mask_rect.xy) * ui_flashlight_mask_rect.zw + 0.5;
            if (all(UV >= 0.0) && all(UV <= 1.0))
            {
                Value = saturate(s_flashlight_mask.SampleLevel(smp_base, saturate(UV), 0).r);
            }
        }
    }
    else
    {
        float Radius = max(ui_flashlight_spot.z, 1.0);
        float InnerRadius = Radius * (1.0 - ui_flashlight_spot.w);
        Value = 1.0 - smoothstep(InnerRadius, max(Radius, InnerRadius + 0.001), length(Pixel - ui_flashlight_spot.xy));
    }
    return Value;
}

// The surface footprint keeps its full size; airborne scattering uses a narrower, clearer core.
static const float FlashlightShaftWidth = 0.35;
static const float FlashlightShaftFadeLength = 0.65;

float FlashlightShaftCookie(float2 Pixel, float2 Center)
{
    // Preserve the mask's continuous gradient and natural reflector detail without synthetic spokes.
    return FlashlightCookie(Center + (Pixel - Center) / FlashlightShaftWidth);
}

uint FlashlightHash(uint Value)
{
    Value ^= Value >> 16;
    Value *= 0x7feb352du;
    Value ^= Value >> 15;
    Value *= 0x846ca68bu;
    return Value ^ (Value >> 16);
}

float3 FlashlightRandom(uint Index)
{
    return float3(FlashlightHash(Index * 3u + 1u), FlashlightHash(Index * 3u + 2u), FlashlightHash(Index * 3u + 3u)) / 4294967296.0;
}

float FlashlightPower()
{
    return 1.0 - saturate(ui_flashlight_mask_state.z);
}

float FlashlightVolumeCoverage(float Z, float CameraZ, float VisibleDepth)
{
    float NearClip = ui_flashlight_volume_view.z;
    float Clip = NearClip > 0.0 ? smoothstep(NearClip, NearClip + ui_flashlight_volume_view.w, CameraZ - Z) : 1.0;
    // Dense at the visible entrance; only ~2% remains halfway toward the canvas.
    float Along = saturate(1.0 - Z / max(VisibleDepth, 1.0));
    float Fade = 1.0 - smoothstep(0.0, FlashlightShaftFadeLength, Along);
    // Boost only the entrance: keep the far half almost transparent.
    float Entrance = 1.0 + 2.2 * exp(-Along * 8.0);
    return Clip * Fade * Fade * Entrance;
}

float3 FlashlightVolume(float2 Pixel)
{
    int Steps = clamp(int(ui_flashlight_volume_settings.x), 0, 64);
    int DustCount = clamp(int(ui_flashlight_dust.x), 0, 2048);
    if (Steps == 0 && DustCount == 0) { return 0.0; }
    if (ui_flashlight_mask_state.x > 0.5 && ui_flashlight_mask_state.y < 0.5) { return 0.0; }
    float2 Center = ui_flashlight_mask_state.x > 0.5 ? ui_flashlight_mask_rect.xy : ui_flashlight_spot.xy;
    float2 HalfSize = ui_flashlight_mask_state.x > 0.5 ? 0.5 / max(ui_flashlight_mask_rect.zw, 1e-8) : ui_flashlight_spot.zz;
    HalfSize *= FlashlightShaftWidth;
    float3 Source = ui_flashlight_volume_source.xyz;
    float Height = max(Source.z, 1.0);
    // The emitter is BEHIND the viewer. Its apex must never lie inside the visible UI volume.
    // Clamp also accepts the older renderer's camera constant without exposing the apex again.
    float CameraZ = clamp(ui_flashlight_volume_view.x, Height * 0.25, Height * 0.5);
    float VisibleDepth = CameraZ - max(ui_flashlight_volume_view.z, max(CameraZ * 0.01, 1.0));
    if (VisibleDepth <= 0.0) { return 0.0; }
    float2 Eye = ui_flashlight_volume_settings.zw;
    // Project the cone's near cross-section, not the hidden emitter, for screen bounds.
    float NearU = 1.0 - VisibleDepth / Height;
    float NearPerspective = CameraZ / max(CameraZ - VisibleDepth, 1e-4);
    float2 NearCenter = Eye + (lerp(Source.xy, Center, NearU) - Eye) * NearPerspective;
    float2 NearHalfSize = HalfSize * NearU * NearPerspective;
    float Padding = max(ui_flashlight_dust.y * 8.0, 2.0);
    if (any(Pixel < min(Center - HalfSize, NearCenter - NearHalfSize) - Padding) ||
        any(Pixel > max(Center + HalfSize, NearCenter + NearHalfSize) + Padding)) { return 0.0; }

    // Integrate a 3D cone along the virtual camera ray, stopped at the UI plane.
    // Stable per-pixel dithering breaks slice bands without introducing temporal flicker.
    float Jitter = frac(52.9829189 * frac(dot(floor(Pixel), float2(0.06711056, 0.00583715))));
    float Integral = 0.0;
    // Spend all samples in the nonzero portion of the fade; retain the original integral weight.
    float ActiveDepth = VisibleDepth * FlashlightShaftFadeLength;
    [loop] for (int Step = 0; Step < Steps; ++Step)
    {
        float Z = VisibleDepth - ActiveDepth + (Step + Jitter) / max(float(Steps), 1.0) * ActiveDepth;
        float3 Point = float3(lerp(Pixel, Eye, Z / CameraZ), Z);
        float2 Footprint = Source.xy + (Point.xy - Source.xy) * Height / max(Height - Z, 0.001);
        // Source distance is attenuated once below; a second inverse-square term made the shaft disappear.
        float Coverage = FlashlightVolumeCoverage(Z, CameraZ, VisibleDepth);
        if (Coverage > 0.0)
        {
            Integral += FlashlightShaftCookie(Footprint, Center) * Coverage * FlashlightShaftFadeLength;
        }
    }
    float Attenuation = ui_flashlight_volume_view.y;
    float Haze = 1.0 - exp(-ui_flashlight_volume_source.w * Integral * 10.0 * Attenuation / max(float(Steps), 1.0));

    // Four depth strata and local-cell lookup keep dense dust independent of the total particle count.
    float Dust = 0.0;
    float Time = ui_flashlight_dust_animation.w > 0.5 ? ui_flashlight_dust_animation.x : ui_flashlight_volume_settings.y * ui_flashlight_dust.z;
    float GlintTime = ui_flashlight_dust_animation.w > 0.5 ? ui_flashlight_dust_animation.y : Time;
    float GlintIntensity = ui_flashlight_dust_animation.w > 0.5 ? ui_flashlight_dust_animation.z : 1.0;
    if (DustCount > 0)
    {
        float2 ViewSize = max(Eye * 2.0, 1.0);
        int PerLayer = (DustCount + 3) >> 2;
        int Rows = max(int(ceil(sqrt(float(PerLayer) * ViewSize.y / ViewSize.x))), 1);
        int Columns = max(int(ceil(float(PerLayer) / float(Rows))), 1);
        int2 Grid = int2(Columns, Rows);
        float2 CellSize = ViewSize / float2(Grid);
        float MaxRadius = max(ui_flashlight_dust.y * 1.35, 0.45);
        int Neighbors = clamp(int(ceil(MaxRadius * 2.9 / min(CellSize.x, CellSize.y))), 1, 3);
        int2 Low = -min(int2(Neighbors, Neighbors), Grid >> 1);
        int2 High = min(int2(Neighbors, Neighbors), (Grid - 1) >> 1);
        [loop] for (int Layer = 0; Layer < 4; ++Layer)
        {
            float LayerDepth = lerp(0.08, 0.92, (float(Layer) + 0.5) * 0.25);
            float LayerPerspective = CameraZ / max(CameraZ - VisibleDepth * LayerDepth, 1.0);
            float2 Wind = float2(-0.006, 0.0015) * Time * LayerPerspective;
            int2 BaseCell = int2(floor(frac(Pixel / ViewSize - Wind) * float2(Grid)));
            [loop] for (int Y = Low.y; Y <= High.y; ++Y)
            {
                [loop] for (int X = Low.x; X <= High.x; ++X)
                {
                    int2 Cell = int2(frac((float2(BaseCell + int2(X, Y)) + 0.5) / float2(Grid)) * float2(Grid));
                    int LocalIndex = Cell.y * Columns + Cell.x;
                    int Index = Layer * PerLayer + LocalIndex;
                    if (LocalIndex >= PerLayer || Index >= DustCount) { continue; }
                    float3 Random = FlashlightRandom(uint(Index) + 17u);
                    float2 Eddy = float2(sin(Time * (0.45 + Random.x * 0.55) + Random.z * 19.0),
                                         cos(Time * (0.35 + Random.y * 0.5) + Random.z * 23.0)) * 0.12;
                    float2 RoomUV = frac((float2(Cell) + 0.15 + Random.xy * 0.7 + Eddy) / float2(Grid) + Wind);
                    float2 Position = RoomUV * ViewSize;
                    float DepthFraction = clamp(LayerDepth + (Random.z - 0.5) * 0.12 +
                        sin(Time * 0.18 + Random.y * 11.0) * 0.025, 0.04, 0.96);
                    float Z = VisibleDepth * DepthFraction;
                    float Perspective = CameraZ / max(CameraZ - Z, 1.0);
                    float Radius = clamp(ui_flashlight_dust.y * lerp(0.35, 0.85, Random.x) * sqrt(Perspective),
                                         0.20, MaxRadius);
                    // Pixel filtering keeps subpixel dust stable while preserving its small total energy.
                    float FilteredRadius = sqrt(Radius * Radius + 0.20);
                    float2 Offset = (Pixel - Position) / FilteredRadius;
                    float R2 = dot(Offset, Offset);
                    if (R2 >= 8.0) { continue; }
                    float3 Point = float3(lerp(Position, Eye, Z / CameraZ), Z);
                    float2 Footprint = Source.xy + (Point.xy - Source.xy) * Height / max(Height - Z, 0.001);
                    float2 EdgeFade = smoothstep(0.0, 0.015, RoomUV) * (1.0 - smoothstep(0.985, 1.0, RoomUV));
                    float NearClip = ui_flashlight_volume_view.z;
                    float Clip = NearClip > 0.0 ? smoothstep(NearClip, NearClip + ui_flashlight_volume_view.w, CameraZ - Z) : 1.0;
                    // Depth changes brightness and parallax without turning nearby motes into snowflakes.
                    float Coverage = Clip * smoothstep(0.02, 0.9, DepthFraction);
                    float Illumination = saturate(FlashlightShaftCookie(Footprint, Center));
                    // Compact antialiased grains: most are faint, a few catch a moving glint.
                    float Profile = exp2(-R2 * 3.0) * (Radius * Radius) / (FilteredRadius * FilteredRadius);
                    float GlintPhase = sin(GlintTime * (1.1 + Random.z * 1.7) + Random.x * 47.0);
                    float Glint = smoothstep(0.82, 1.0, GlintPhase);
                    float Brightness = lerp(0.20, 0.45, Random.y) + Glint * GlintIntensity * lerp(0.60, 1.40, Random.z);
                    Dust += Profile * Illumination * Coverage * EdgeFade.x * EdgeFade.y *
                        ui_flashlight_dust.w * Brightness;
                }
            }
        }
    }
    // Fine grains can glint in clear air; no broad halo surrounds the particles.
    return saturate(Haze * ui_flashlight_volume_color.rgb + Dust * sqrt(saturate(Attenuation)) * 3.0 * ui_flashlight_dust_color.rgb);
}
