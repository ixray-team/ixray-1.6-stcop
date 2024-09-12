#include "common.hlsli"

struct a2v
{
    float4 P : POSITION;
};

#ifdef USE_AREF
void main(in v_static I, out p_shadow_csm O)
{
    O.tc0 = unpack_tc_base(I.tc, I.T.w, I.B.w);
#else
void main(in a2v I, out p_shadow_csm O)
{
#endif
    O.hpos = mul(m_WVP, I.P);
	O.w_pos = float4(mul(m_W, I.P),1.);
}