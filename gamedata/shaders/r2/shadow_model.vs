#include "common.h"
#include "skin.h"

p_shadow _main(v_model I) {
	p_shadow O;
	
	O.hpos = mul(m_WVP, I.P);
	O.tc0 = I.tc;
	
#ifndef USE_HWSMAP
	O.depth = O.hpos.z;
#endif

 	return	O;
}

#ifdef 	SKIN_NONE
p_shadow main(v_model v) { return _main(v); }
#endif

#ifdef 	SKIN_0
p_shadow main(v_model_skinned_0 v) { return _main(skinning_0(v)); }
#endif

#ifdef	SKIN_1
p_shadow main(v_model_skinned_1 v) { return _main(skinning_1(v)); }
#endif

#ifdef	SKIN_2
p_shadow main(v_model_skinned_2 v) { return _main(skinning_2(v)); }
#endif

#ifdef	SKIN_3
p_shadow main(v_model_skinned_3 v) { return _main(skinning_3(v)); }
#endif

#ifdef	SKIN_4
p_shadow main(v_model_skinned_4 v) { return _main(skinning_4(v)); }
#endif

