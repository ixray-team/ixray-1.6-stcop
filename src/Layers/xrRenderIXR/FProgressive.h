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
	u32					last_lod	;
public:
    					FProgressive();
	virtual ~FProgressive() override;
	virtual void 		Render		(GfxCmdList& Cmd, float LOD) override;		// LOD - Level Of Detail  [0.0f - min, 1.0f - max], -1 = Ignored
	virtual void 		Load		(const char* N, IReader *data,u32 dwFlags) override;
	virtual void 		Copy		(dxRender_Visual *pFrom) override;
	virtual void 		Release		() override;
	// The window of the most detailed level: the index data holds every level, one after another
	const FSlideWindow*	GetWindow	() const { return nSWI.count ? nSWI.sw : nullptr; }
	const FSlideWindow*	GetLastWindow	() const { return nSWI.count ? &nSWI.sw[std::min(last_lod, u32(nSWI.count) - 1)] : nullptr; }
	// The window Render picks for this detail, kept as the last one as Render keeps it
	const FSlideWindow*	PickWindow		(float LOD) { last_lod = nSWI.count ? u32(iFloor((1.f - clampr(LOD, 0.f, 1.f)) * float(nSWI.count - 1) + 0.5f)) : last_lod; return GetLastWindow(); }
private:
	FProgressive				(const FProgressive& other);
	void	operator=			( const FProgressive& other);
};

#endif
