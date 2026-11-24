#pragma once

class CBlender_SPP : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: calc SPP";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_SPP();
	virtual ~CBlender_SPP() override;
};
