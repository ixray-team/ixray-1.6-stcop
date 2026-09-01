#include "common.hlsli"


// Vertex
v2p_volume main(float4 P : POSITION)
{
    v2p_volume O;
    O.hpos = mul(m_WVP, P);
    O.tc = mul(m_texgen, P);
    return O;
}

