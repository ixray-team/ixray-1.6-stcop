#include "common.hlsli"
#include "shared\waterconfig.hlsli"
#include "shared\watermove.hlsli"

struct vv
{
    float4 P : POSITION;
    float2 tc : TEXCOORD0;
    float3 N : NORMAL;
    float4 C : COLOR0;
};

struct vf
{
    float2 tbase : TEXCOORD0; // base
    float2 tnorm0 : TEXCOORD1; // nm0
    float2 tnorm1 : TEXCOORD2; // nm1
    float3 M1 : TEXCOORD3;
    float3 M2 : TEXCOORD4;
    float3 M3 : TEXCOORD5;
    float3 v2point : TEXCOORD6;
    float4 tctexgen : TEXCOORD7;
    float3 pos : TEXCOORD8;
    float4 c0 : COLOR0;
    float4 hpos : POSITION;
};

uniform float4x4 m_texgen;
void main(in vv v, out vf o)
{
   // v.N = unpack_normal(v.N.xyz);

    // world
    float4 P = float4(mul(m_W, v.P), 1.0f);
    P = watermove(P);

    o.pos = P.xyz;
    o.v2point = P - eye_position;

    o.tbase = P.xz * 0.3; // unpack_tc_base(v.uv, v.T.w, v.B.w);
    o.tnorm0 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_0, P.xz, W_DISTORT_AMP_0);
    o.tnorm1 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_1, P.xz, W_DISTORT_AMP_1);

    // Calculate the 3x3 transform from tangent space to eye-space
    // TangentToEyeSpace = object2eye * tangent2object
    // = object2eye * transpose(object2tangent) (since the inverse of a rotation is its transpose)

    float3 N = normalize(v.N);

    float3 T = float3(-1.0f, 0.0f, 0.0f);
    T = normalize(T - dot(T, N) * N);

    float3 B = cross(N, T);

    float3x3 xform = mul((float3x3)m_W, float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z));

    // The pixel shader operates on the bump-map in [0..1] range
    // Remap this range in the matrix, anyway we are pixel-shader limited :)
    // ...... [ 2  0  0  0]
    // ...... [ 0  2  0  0]
    // ...... [ 0  0  2  0]
    // ...... [-1 -1 -1  1]
    // issue: strange, but it's slower :(
    // issue: interpolators? dp4? VS limited? black magic?

    // Feed this transform to pixel shader
    o.M1 = xform[0];
    o.M2 = xform[1];
    o.M3 = xform[2];

    // xform, input in world coords
    o.hpos = mul(m_VP, P);

    o.c0 = v.C;

    //	Igor: for additional depth dest
    o.tctexgen = mul(m_texgen, P);
    o.tctexgen.z = mul(m_V, P).z;
}

