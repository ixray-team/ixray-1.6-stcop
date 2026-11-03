#pragma once

class CBlender_light_occq	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: occlusion testing";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_light_occq();
	virtual ~CBlender_light_occq() override;
};
