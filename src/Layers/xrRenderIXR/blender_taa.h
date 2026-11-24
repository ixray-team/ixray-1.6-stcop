#pragma once

class CBlender_taa : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: temporal slop";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_taa();
	virtual ~CBlender_taa() override;
};
