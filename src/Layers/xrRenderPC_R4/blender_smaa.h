#pragma once

class CBlender_SMAA : public IBlender
{
public:
	virtual const char* getComment() override { return "CBlender_SMAA"; }
	virtual bool canBeDetailed() override { return false; }
	virtual bool canBeLMAPped() override { return false; }

	virtual void Compile(CBlender_Compile& C) override;

    CBlender_SMAA();
	virtual ~CBlender_SMAA() override;
};