#pragma once

class CBlender_gtao : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: ground-truth based ambient occlusion";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_gtao();
	virtual ~CBlender_gtao() override;
};
