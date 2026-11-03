#pragma once

class CBlender_gamma :
	public IBlender
{
public:
	virtual const char*		getComment() override { return "INTERNAL: gamma correction"; }
	virtual bool		canBeDetailed() override { return false; }
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Compile(CBlender_Compile& C) override;

	CBlender_gamma();
	virtual ~CBlender_gamma() override = default;
};