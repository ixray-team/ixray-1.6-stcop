#include "common.h"
#include "skin.h"

void skinned_main(in v_model I, out p_bumped_new O) {	
	float3 Nw = mul((float3x3)m_W, (float3)I.N);
	float3 hc_pos = (float3)hemi_cube_pos_faces;
	float3 hc_neg = (float3)hemi_cube_neg_faces;
	float3 hc_mixed	= (Nw < 0.0f) ? -hc_neg : hc_pos;
	float hemi_val = saturate(dot(hc_mixed, Nw));
	float3 Pe = mul(m_WV, I.P);
	
	O.tcdh = float4(I.tc.xy, hemi_val, L_material.y);
	O.position = float4(Pe, 1.0f);

	float3 N = I.N * 2.0f;
	float3 T = I.T * 2.0f;
	float3 B = I.B * 2.0f;
	
	float3x3 xform = mul((float3x3)m_WV, float3x3(
		T.x, B.x, N.x,
		T.y, B.y, N.y,
		T.z, B.z, N.z
	));
	
	O.M1 = xform[0]; 
	O.M2 = xform[1]; 
	O.M3 = xform[2]; 

	O.hpos = mul(m_WVP, I.P);
	
	O.hpos_curr = mul(m_WVP, I.P);
	O.hpos_old = mul(m_WVP_old, I.P_old);
}

#if defined(SKIN_0)	
void main(in v_model_skinned_0 I, out p_bumped_new O) {
	skinned_main(skinning_0(I), O);
}
#elif defined(SKIN_1)
void main(in v_model_skinned_1 I, out p_bumped_new O) {
	skinned_main(skinning_1(I), O);
}
#elif defined(SKIN_2)
void main(in v_model_skinned_2 I, out p_bumped_new O) {
	skinned_main(skinning_2(I), O);
}
#elif defined(SKIN_3)
void main(in v_model_skinned_3 I, out p_bumped_new O) {
	skinned_main(skinning_3(I), O);
}
#elif defined(SKIN_4)
void main(in v_model_skinned_4 I, out p_bumped_new O) {
	skinned_main(skinning_4(I), O);
}
#else
void main(in v_model I, out p_bumped_new O) {
	skinned_main(I, O);
}
#endif