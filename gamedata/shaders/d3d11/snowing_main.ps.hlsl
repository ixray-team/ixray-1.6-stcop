#include "common.hlsli"
#include "sload.hlsli"

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

void main(PSInput _I, out IXRayGbufferPack O)
{
    IXRayGbuffer G = (IXRayGbuffer)NULL;
    p_bumped_new I;

    GbufferUnpack((uint2)_I.hpos.xy, G);
	
	clip(G.SnowMask - 0.00001f);
	clip(0.9999f - G.Depth);
	
    I.position = float4(G.Point.xyz, 1.0f);
	I.snow_mask = G.SnowMask * smoothstep(0.2f, 0.3f, G.Hemi);

    float3 P = mul(m_invV, I.position);
    float3 N = normalize(mul((float3x3)m_invV, G.Normal.xyz));

    float3 T, B;
    I.tcdh.xy = P.xz * 0.2f;

    build_contangent_frame(P, N, I.tcdh.xy, T, B);

    float3x3 xform = mul((float3x3)m_V, float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z
		)
    );
    I.tcdh.zw = 0.5f;

    I.M1 = xform[0];
    I.M2 = xform[1];
    I.M3 = xform[2];
	
	I.hpos = _I.hpos;
	
#ifndef DISABLE_MOTION_VECTORS
    I.hpos_curr = I.hpos_old = I.hpos;
    O.Velocity = 0.0f;
#endif

    IXRayMaterial M = (IXRayMaterial)NULL;

    M.Sun = G.SSS;
    M.Hemi = G.Hemi;

    M.Depth = G.Point.z;
    M.Point = G.Point.xyz;

    SloadNew(I, M);

	M.Normal = mul(xform, M.Normal);
    M.Normal = lerp(G.Normal, M.Normal, I.snow_mask);
	M.Normal = normalize(M.Normal);
	
#ifndef USE_LEGACY_LIGHT
    M.Roughness = lerp(G.Roughness, M.Roughness, I.snow_mask);
#else
    M.Gloss = lerp(G.Gloss, M.Gloss, I.snow_mask);
#endif

    GbufferPack(O, M);
	
	O.Color.w = I.snow_mask;
	O.Material.w = I.snow_mask;

	O.Normal.w = 1.0f;
}

