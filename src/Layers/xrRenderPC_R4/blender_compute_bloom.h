#pragma once

class CBlender_compute_bloom : public IBlender
{
public:
	virtual const char* getComment() { return "INTERNAL: compute bloom"; }
	virtual bool canBeDetailed() { return false; }
	virtual bool canBeLMAPped() { return false; }
	virtual void Compile(CBlender_Compile& C);
	CBlender_compute_bloom() { description.CLS = 0; }
};
