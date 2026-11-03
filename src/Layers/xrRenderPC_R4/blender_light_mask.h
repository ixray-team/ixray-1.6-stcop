#pragma once

class CBlender_accum_direct_mask : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: mask direct light";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_accum_direct_mask();
	virtual ~CBlender_accum_direct_mask() override;
};

