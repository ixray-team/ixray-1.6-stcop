#pragma once

class CBlender_gtao : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: ground-truth based ambient occlusion";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_gtao();
	virtual ~CBlender_gtao();
};
