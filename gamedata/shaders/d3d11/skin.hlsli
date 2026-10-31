#ifndef SKIN_H
#define SKIN_H

#include "common.hlsli"

v_model PullModel(uint VertexId)
{
    PulledVertex V = PullVertex(VertexId);
    v_model o;
    o.P = float4(V.P, 1.0f);

#ifndef DISABLE_MOTION_VECTORS
    o.P_old = o.P;
    [branch]
    if ((draw_flags & DRAW_SOURCE_MASK) == DRAW_SOURCE_SKINNED)
    {
        o.P_old.xyz += DecodeHalf3(V.C, V.X);
    }
#endif

    o.N = V.N;
    o.T = V.T;
    o.B = V.B;

    o.tc = V.UV;
    return o;
}

#endif
