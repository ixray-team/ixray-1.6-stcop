#pragma once

class CBlender_FXAA : public IBlender
{
public:
	virtual const char* getComment() override { return "CBlender_FXAA"; }
	virtual bool canBeDetailed() override { return false; }
	virtual bool canBeLMAPped() override { return false; }

	virtual void Compile(CBlender_Compile& C) override;

    CBlender_FXAA();
	virtual ~CBlender_FXAA() override;
};