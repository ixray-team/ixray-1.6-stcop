#pragma once

#include "IRenderDetailModel.h"

class ECORE_API CDetail:
	public IRender_DetailModel
{
public:
	struct alignas(32) SlotItem// один кустик
	{
		Fvector quat;
		float scale;
		Fvector pos;
		float c_hemi;
	};
	
#ifdef USE_DX11
	ref_geom					hw_Geom;
	IRHIBuffer*			hw_VB;
	IRHIBuffer*			hw_IB;
#endif
	
	xr_vector<SlotItem> m_items[3][2];
	void			Load		(IReader* S);
	void			Optimize	();
	virtual void	Unload		();

	virtual void	transfer	(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset);
	virtual void	transfer	(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset, float du, float dv);
	virtual			~CDetail	();
};