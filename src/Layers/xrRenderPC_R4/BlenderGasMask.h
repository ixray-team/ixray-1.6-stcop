#pragma once
#include "../../Include/xrRender/VisorGlassType.h"

class CBlenderGasMask :
	public IBlender
{
public:
	CBlenderGasMask(VisorGlassType GlassType = VisorGlassType::Curved);
	virtual ~CBlenderGasMask() = default;

	virtual const char* getComment() { return "GasMask effect"; }
	virtual bool canBeDetailed() { return false; }
	virtual bool canBeLMAPped() { return false; }

	virtual void Compile(CBlender_Compile& C);

private:
	VisorGlassType GlassType;
};