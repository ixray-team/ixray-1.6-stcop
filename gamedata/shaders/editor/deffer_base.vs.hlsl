#ifndef SKIN_NONE
#include "deffer_model.vs.hlsl"
#else

#include "common.hlsli"	

void main(in v_model I, out p_bumped_new O, uint InstanceID : SV_InstanceID)
{
    editor_xforms X = editor_get_xforms(InstanceID);

    float3 Pe = mul(X.WV, float4(I.P.xyz, 1.0f));

    O.tcdh = float4(I.tc.xy, 0.7f, 0.5f);
    O.position = float4(Pe, 1.0f);

    float3 N = normalize(mul((float3x3)X.W, I.N.zyx));
	O.tcdh.z = N.y * 0.3f + 0.5f;
    N = normalize(mul((float3x3)m_V, N));

    O.M1 = N;
    O.M2 = N;
    O.M3 = N;

    O.hpos = mul(X.WVP, float4(I.P.xyz, 1.0f));
}

#endif