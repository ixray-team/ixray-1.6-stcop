#pragma once

class CBlender_accum_point : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: accumulate point light";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_accum_point();
	virtual ~CBlender_accum_point() override;
};
