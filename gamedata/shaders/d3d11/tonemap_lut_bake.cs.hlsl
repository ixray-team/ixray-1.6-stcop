#include "tonemap_gt7.hlsli"

Texture2D<float> s_tonemap_state;
RWTexture3D<float4> rw_tonemap_lut : register(u0);

[numthreads(8, 8, 8)]
void main(uint3 id : SV_DispatchThreadID)
{
    uint width, height, depth;
    rw_tonemap_lut.GetDimensions(width, height, depth);
    if (any(id >= uint3(width, height, depth)))
        return;

    float range = s_tonemap_state.Load(int3(1, 0, 0));
    float3 grid = float3(id) / float3(width - 1, height - 1, depth - 1);
    // LUT coordinates are already exposed linear RGB. Do not expose twice.
    float3 color = pow(grid, 4.0f) * range;
    color = LinearSRGBToRec2020(color);
    color = GT7Tonemap(color);
    color = Rec2020ToLinearSRGB(color);
    // Keep linear values, including out-of-gamut ones, until interpolation.
    rw_tonemap_lut[id] = float4(color, 1.0f);
}
