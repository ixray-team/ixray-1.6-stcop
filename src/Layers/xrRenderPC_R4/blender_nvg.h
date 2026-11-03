#pragma once

class CBlender_nvg : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: nightvision shader";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_nvg();
	virtual ~CBlender_nvg() override;
};
