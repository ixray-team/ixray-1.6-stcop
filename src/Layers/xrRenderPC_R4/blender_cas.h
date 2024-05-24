#pragma once

class CBlender_cas : public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "INTERNAL: contrast adaptive sharpening";	}
	virtual		BOOL		canBeDetailed()	{ return FALSE;	}
	virtual		BOOL		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_cas();
	virtual ~CBlender_cas();
};
