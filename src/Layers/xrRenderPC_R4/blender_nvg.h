#pragma once

class CBlender_nvg : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: nightvision shader";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_nvg();
	virtual ~CBlender_nvg();
};
