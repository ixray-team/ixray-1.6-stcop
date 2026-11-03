#pragma once

class CBlender_accum_direct : public IBlender  
{
public:
	virtual const char*	getComment() override { return "INTERNAL: accumulate direct light";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_accum_direct();
	virtual ~CBlender_accum_direct() override;
};
