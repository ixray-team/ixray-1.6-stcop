#ifndef DetailModelH
#define DetailModelH
#pragma once

#include "IRenderDetailModel.h"

class  CDetail		: public IRender_DetailModel
{
public:
	void			Load		(IReader* S);
#ifndef USE_OGL
	void			Optimize	();
#endif
	virtual void	Unload		();

	virtual void	transfer	(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset);
	virtual void	transfer	(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset, float du, float dv);
	virtual			~CDetail	();
};
#endif
