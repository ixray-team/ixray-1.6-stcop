#pragma once

class CBlender_SPP : public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "INTERNAL: calc SPP";	}
	virtual		BOOL		canBeDetailed()	{ return FALSE;	}
	virtual		BOOL		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_SPP();
	virtual ~CBlender_SPP();
};
