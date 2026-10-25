// FProgressive.h: interface for the FProgressive class.
//
//////////////////////////////////////////////////////////////////////

#ifndef FProgressiveH
#define FProgressiveH
#pragma once

#include "FVisual.h"
struct	FSlideWindowItem;

class	FProgressive	: public Fvisual
{
protected:
	FSlideWindowItem	nSWI		;
	FSlideWindowItem*	xSWI		;
	u32					last_lod	;
public:
    					FProgressive();
	virtual ~FProgressive() override;
	virtual void 		Render		(float LOD) override;		// LOD - Level Of Detail  [0.0f - min, 1.0f - max], -1 = Ignored
	virtual void 		Load		(const char* N, IReader *data,u32 dwFlags) override;
	virtual void 		Copy		(dxRender_Visual *pFrom) override;
	virtual void 		Release		() override;
private:
	FProgressive				(const FProgressive& other);
	void	operator=			( const FProgressive& other);
};

#endif
