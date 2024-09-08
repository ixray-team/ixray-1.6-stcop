#include "common.hlsli"	

void main(in v_editor I, out p_bumped_new O)
{
    float3 Pe = mul(m_WV, I.P);

    O.tcdh = float4(I.tc.xy, 0.7f, 0.5f);
    O.position = float4(Pe, 1.0f);

    float3 N = normalize(mul(m_W, I.N));
	O.tcdh.z = N.y * 0.3f + 0.5f;
    N = normalize(mul(m_V, N));

    O.M1 = N;
    O.M2 = N;
    O.M3 = N;

    O.hpos = mul(m_WVP, I.P);
}

