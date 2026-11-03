#pragma once

class CBlender_Sharpening :
	public IBlender
{
public:
	virtual const char* getComment() override { return "INTERNAL: contrast adaptive sharpening"; }
	virtual bool canBeDetailed() override { return false; }
	virtual bool canBeLMAPped() override { return false; }

	virtual void Compile(CBlender_Compile& C) override;

	CBlender_Sharpening();
	virtual ~CBlender_Sharpening() override;
};
