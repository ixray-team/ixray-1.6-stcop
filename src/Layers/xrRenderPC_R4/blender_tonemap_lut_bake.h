#pragma once

class CBlender_tonemap_lut_bake : public IBlender
{
public:
	virtual const char* getComment() { return "INTERNAL: bake GT7 tonemap LUT"; }
	virtual bool canBeDetailed() { return false; }
	virtual bool canBeLMAPped() { return false; }
	virtual void Compile(CBlender_Compile& C);
	CBlender_tonemap_lut_bake() { description.CLS = 0; }
};
