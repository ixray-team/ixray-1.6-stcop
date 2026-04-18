#pragma once

class CBlender_luminance	: public IBlender  
{
public:
	virtual		const char* getComment()	{ return "INTERNAL: luminance estimate";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_luminance();
	virtual ~CBlender_luminance();
};
