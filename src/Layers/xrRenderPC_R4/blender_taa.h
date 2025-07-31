#pragma once

class CBlender_taa : public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "INTERNAL: temporal slop";	}
	virtual		BOOL		canBeDetailed()	{ return FALSE;	}
	virtual		BOOL		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_taa();
	virtual ~CBlender_taa();
};
