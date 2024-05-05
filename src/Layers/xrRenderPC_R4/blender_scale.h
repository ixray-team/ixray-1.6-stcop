#pragma once

class CBlender_scale : public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "INTERNAL: scale";	}
	virtual		BOOL		canBeDetailed()	{ return FALSE;	}
	virtual		BOOL		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_scale();
	virtual ~CBlender_scale();
};
