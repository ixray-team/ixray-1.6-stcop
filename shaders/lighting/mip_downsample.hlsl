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
// One mip of a layer from the mip above it, the box of the four texels under a texel. The source is that mip alone; an
// sRGB view decodes what is read and encodes what is written, so the average is in linear light
Texture2DArray<float4> s_mip_source : register(t0);

cbuffer mip_layer : register(b0)
{
    uint mip_layer_index;
};

float4 vs_main(uint vertex_id : SV_VertexID) : SV_Position
{
    float2 corner = float2((vertex_id << 1u) & 2u, vertex_id & 2u);
    return float4(corner * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 0.0f, 1.0f);
}

float4 ps_main(float4 position : SV_Position) : SV_Target
{
    uint3 size;
    s_mip_source.GetDimensions(size.x, size.y, size.z);

    int layer = int(mip_layer_index);
    int2 last = int2(size.xy) - 1;
    int2 texel = int2(position.xy) * 2;
    float4 sum = s_mip_source.Load(int4(min(texel, last), layer, 0));
    sum += s_mip_source.Load(int4(min(texel + int2(1, 0), last), layer, 0));
    sum += s_mip_source.Load(int4(min(texel + int2(0, 1), last), layer, 0));
    sum += s_mip_source.Load(int4(min(texel + 1, last), layer, 0));
    return sum * 0.25f;
}
