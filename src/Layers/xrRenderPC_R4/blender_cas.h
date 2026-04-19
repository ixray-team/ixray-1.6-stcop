#pragma once

class CBlender_cas : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: contrast adaptive sharpening";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_cas();
	virtual ~CBlender_cas();
};
