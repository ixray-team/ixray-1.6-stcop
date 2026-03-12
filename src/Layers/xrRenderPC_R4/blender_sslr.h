#pragma once

class CBlender_sslr : public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "INTERNAL: screen space reflections";	}
	virtual		BOOL		canBeDetailed()	{ return FALSE;	}
	virtual		BOOL		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_sslr();
	virtual ~CBlender_sslr();
};
