#include "common.h"

struct 	a2v {
	float4 P: POSITION;
};

#ifdef USE_AREF
void main(in v_static I, out p_shadow O) {
	O.tc0 = unpack_tc_base(I.tc,I.T.w,I.B.w);
#else
void main(in a2v I, out p_shadow O) {
	O.tc0 = 0.0f;
#endif
	O.hpos = mul(m_WVP, I.P);
	
#ifndef USE_HWSMAP
	O.depth = O.hpos.z;
#endif
}