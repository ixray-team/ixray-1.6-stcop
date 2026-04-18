#pragma once

class CBlender_gtao : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: ground-truth based ambient occlusion";	}
	virtual		bool		canBeDetailed()	{ return FALSE;	}
	virtual		bool		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_gtao();
	virtual ~CBlender_gtao();
};
