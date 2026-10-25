#pragma once

class CBlender_accum_reflected : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: accumulate reflected light";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_accum_reflected();
	virtual ~CBlender_accum_reflected() override;
};

