#pragma once

class CBlender_new_dof : public IBlender
{
public:
	virtual const char*  getComment() override { return "INTERNAL: new depth of field"; }
	virtual bool    canBeDetailed() override { return false; }
	virtual bool    canBeLMAPped() override { return false; }

	virtual void    Compile(CBlender_Compile& C) override;

    CBlender_new_dof();
	virtual ~CBlender_new_dof() override;
};