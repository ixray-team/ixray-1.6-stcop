#include "StdAfx.h"
#include "Build.h"

#include "../xrLC_Light/xrMU_Model.h"
#include "../xrLC_Light/xrMU_Model_Reference.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"

void SaveUVM			(const char* fname, xr_vector<b_rc_face>& vm)
{
	IWriter* W			= FS.w_open(fname);
	string256 tmp;
	// vertices
	for (u32 v_idx=0; v_idx<vm.size(); v_idx++){
		b_rc_face& rcf	= vm[v_idx];
		xr_sprintf			(tmp,"f %d %d [%3.2f,%3.2f]-[%3.2f,%3.2f]-[%3.2f,%3.2f]",rcf.dwMaterial,rcf.dwMaterialGame,
						rcf.t[0].x,rcf.t[0].y, rcf.t[1].x,rcf.t[1].y, rcf.t[2].x,rcf.t[2].y);
		W->w_string		(tmp);
	}
	FS.w_close	(W);
}
 