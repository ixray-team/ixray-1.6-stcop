#pragma once

class CBlender_nvg : public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "INTERNAL: nightvision shader";	}
	virtual		BOOL		canBeDetailed()	{ return FALSE;	}
	virtual		BOOL		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_nvg();
	virtual ~CBlender_nvg();
};
