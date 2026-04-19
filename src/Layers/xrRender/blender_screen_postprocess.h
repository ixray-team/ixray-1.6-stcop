#pragma once

class CBlender_SPP : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: calc SPP";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_SPP();
	virtual ~CBlender_SPP();
};
