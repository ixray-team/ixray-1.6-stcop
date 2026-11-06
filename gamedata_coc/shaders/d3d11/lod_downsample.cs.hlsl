// IX-Ray editor LOD downsample + atlas packing compute shader.
//
// The editor renders LOD_SAMPLE_COUNT supersampled captures (one per viewing
// angle) and this shader reduces each (LOD_IMAGE_SIZE*SS_QUALITY)^2 capture to
// a LOD_IMAGE_SIZE^2 strip, packing all strips side-by-side into one atlas:
//   atlas width  = LOD_IMAGE_SIZE * LOD_SAMPLE_COUNT
//   atlas height = LOD_IMAGE_SIZE
//
// Source textures are B8G8R8A8_UNORM, so the shader resource view yields
// .r = Blue, .g = Green, .b = Red, .a = Alpha. The atlas is R8G8B8A8_UNORM
// with an unordered-access view, so writing the loaded value back as-is stores
// bytes (B,G,R,A) which is exactly the legacy A8R8G8B8 / B8G8R8A8 memory
// layout consumed by the downstream LOD / TGA writers.
//
// NOTE: the 8 source textures are bound to explicit t0..t7 registers. Dynamic
// indexing into a Texture2D array is not allowed here (it forces an unrollable
// loop that fails to compile), so the per-angle texture is selected via an
// if-chain on a statically-known texture object.

#define LOD_IMAGE_SIZE 64
#define LOD_SAMPLE_COUNT 8
#define SS_QUALITY 8

Texture2D<float4> gSrc0 : register(t0);
Texture2D<float4> gSrc1 : register(t1);
Texture2D<float4> gSrc2 : register(t2);
Texture2D<float4> gSrc3 : register(t3);
Texture2D<float4> gSrc4 : register(t4);
Texture2D<float4> gSrc5 : register(t5);
Texture2D<float4> gSrc6 : register(t6);
Texture2D<float4> gSrc7 : register(t7);
RWTexture2D<float4> gAtlas : register(u0);

float4 LoadSrc(uint angle, uint3 coord)
{
    float4 c = gSrc0.Load(coord);
    if (angle == 1) c = gSrc1.Load(coord);
    else if (angle == 2) c = gSrc2.Load(coord);
    else if (angle == 3) c = gSrc3.Load(coord);
    else if (angle == 4) c = gSrc4.Load(coord);
    else if (angle == 5) c = gSrc5.Load(coord);
    else if (angle == 6) c = gSrc6.Load(coord);
    else if (angle == 7) c = gSrc7.Load(coord);
    return c;
}

[numthreads(8, 8, 1)]
void main(uint3 DTid : SV_DispatchThreadID)
{
    uint outW = LOD_IMAGE_SIZE * LOD_SAMPLE_COUNT;
    if (DTid.x >= outW || DTid.y >= LOD_IMAGE_SIZE)
        return;

    uint angle = DTid.x / LOD_IMAGE_SIZE;
    uint lx    = DTid.x % LOD_IMAGE_SIZE;
    uint ly    = DTid.y;

    float4 sum = (float4)0;
    for (uint dy = 0; dy < SS_QUALITY; ++dy)
    {
        for (uint dx = 0; dx < SS_QUALITY; ++dx)
        {
            uint sx = lx * SS_QUALITY + dx;
            uint sy = ly * SS_QUALITY + dy;
            sum += LoadSrc(angle, uint3(sx, sy, 0));
        }
    }
    sum /= (SS_QUALITY * SS_QUALITY);

    // Swap R/B: the source is B8G8R8A8 (shader .r = Blue), but the LOD/TGA
    // consumers expect R,G,B,A, so write the red and blue channels swapped.
    gAtlas[DTid.xy] = float4(sum.b, sum.g, sum.r, sum.a);
}
