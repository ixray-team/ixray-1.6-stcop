#include "common.hlsli"

RWTexture3D<float4> rw_tonemap_lut : register(u0);

cbuffer TonemapLUTParams : register(b0)
{
    float4 tm_lut_params; // x = lut size, y = 1/lut size, z = hdr range, w = unused
    float4 autoexposure_params; // x - ps_r2_autoexposure_key, y - ps_r2_autoexposure_min, z - ps_r2_autoexposure_max, w - ps_r2_autoexposure_bias
    float4 tm_params1;    // gamma / misc
};

[numthreads(8, 8, 8)]
void main(uint3 dtid : SV_DispatchThreadID)
{
    const uint LUT_SIZE = 32;

    if (dtid.x >= LUT_SIZE || dtid.y >= LUT_SIZE || dtid.z >= LUT_SIZE)
        return;

    float3 uvw = (float3(dtid)) * tm_lut_params.y;
    float3 colorHDR = pow(uvw, 4.f) * 3.67926554928f; // GT7 SDR range

    float3 mapped = colorHDR;//TonemapComplex(colorHDR);
    mapped = LinearSRGBToRec2020(mapped);
    mapped = GT7Tonemap(mapped);
    mapped = Rec2020ToLinearSRGB(mapped);

    rw_tonemap_lut[dtid] = float4(mapped, 1.0f);
}