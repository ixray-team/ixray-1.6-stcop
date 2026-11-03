#include "common.hlsli"
#include "common_iostructs.hlsli"

#ifdef EDITOR_MODE
// Texture transform of the editor helper draws (fixed-function D3DTS_TEXTURE0 emulation)
uniform float4x4 m_tc_xform;
#endif

// Vertex
v2p_TL main(v_TL_positiont I)
{
    v2p_TL O;

    //	O.HPos = P;

    {
        I.P.xy += 0.5f;
        //		O.HPos.x = I.P.x/1024 * 2 - 1;
        //		O.HPos.y = (I.P.y/768 * 2 - 1)*-1;
        O.HPos.x = I.P.x * screen_res.z * 2 - 1;
        O.HPos.y = (I.P.y * screen_res.w * 2 - 1) * -1;
        O.HPos.zw = I.P.zw;
    }

#ifdef EDITOR_MODE
    O.Tex0 = mul(m_tc_xform, float4(I.Tex0, 1.0f, 0.0f)).xy;
#else
    O.Tex0 = I.Tex0;
#endif
    O.Color = I.Color.bgra; //	swizzle vertex colour

    return O;
}
