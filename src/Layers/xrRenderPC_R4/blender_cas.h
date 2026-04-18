#pragma once

class CBlender_cas : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: contrast adaptive sharpening";	}
	virtual		bool		canBeDetailed()	{ return FALSE;	}
	virtual		bool		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_cas();
	virtual ~CBlender_cas();
};
