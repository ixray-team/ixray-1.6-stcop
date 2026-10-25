#pragma once
#include "../../Include/xrRender/VisorGlassType.h"

class CBlenderGasMask :
	public IBlender
{
public:
	CBlenderGasMask(VisorGlassType GlassType = VisorGlassType::Curved);
	virtual ~CBlenderGasMask() override = default;

	virtual const char* getComment() override { return "GasMask effect"; }
	virtual bool canBeDetailed() override { return false; }
	virtual bool canBeLMAPped() override { return false; }

	virtual void Compile(CBlender_Compile& C) override;

private:
	VisorGlassType GlassType;
};