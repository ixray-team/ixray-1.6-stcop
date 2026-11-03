#pragma once

class CBlender_SSAO : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: calc SSAO";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_SSAO();
	virtual ~CBlender_SSAO() override;
};
