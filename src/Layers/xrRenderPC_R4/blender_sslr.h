#pragma once

class CBlender_sslr : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: screen space reflections";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_sslr();
	virtual ~CBlender_sslr() override;
};
