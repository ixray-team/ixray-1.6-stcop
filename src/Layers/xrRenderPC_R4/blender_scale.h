#pragma once

class CBlender_scale : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: scale";	}
	virtual		bool		canBeDetailed()	{ return FALSE;	}
	virtual		bool		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_scale();
	virtual ~CBlender_scale();
};
