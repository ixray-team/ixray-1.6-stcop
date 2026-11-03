#pragma once

class CBlender_new_adaptation : public IBlender
{
public:
	virtual const char*		getComment() override { return "INTERNAL: new adaptation calc"; }
	virtual bool		canBeDetailed() override { return false; }
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Compile(CBlender_Compile& C) override;

	CBlender_new_adaptation();
	virtual ~CBlender_new_adaptation() override;
};