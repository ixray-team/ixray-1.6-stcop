#include "common.hlsli"
#include "sload.hlsli"

void main(p_shadow _I, out IXrayGbufferPack O)
{
	
	discard;
    IXrayGbuffer G;
    p_bumped_new I;

    GbufferUnpack(_I.tc0.xy, _I.hpos.xy, G);
	
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

    I.hpos_curr = I.hpos_old = I.hpos = _I.hpos;
    IXrayMaterial M;

    M.Sun = G.SSS;
    M.Hemi = G.Hemi;

    M.Depth = G.Point.z;
    M.Point = G.Point.xyz;

    SloadNew(I, M);

	M.Normal = mul(xform, M.Normal);

    M.Normal = lerp(G.Normal, M.Normal, I.snow_mask);
    M.Roughness = lerp(G.Roughness, M.Roughness, I.snow_mask);

	M.Normal = normalize(M.Normal);

    O.Velocity = 0.0f;
    GbufferPack(O, M);
	
	O.Color.w = I.snow_mask;
	O.Material.w = I.snow_mask;

	O.Normal.w = 1.0f;
}

