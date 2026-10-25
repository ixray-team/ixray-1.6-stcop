#pragma once

class CBlender_combine : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: combiner";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_combine();
	virtual ~CBlender_combine() override;
};
