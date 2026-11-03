#pragma once

class CBlender_scale : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: scale";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_scale();
	virtual ~CBlender_scale() override;
};
