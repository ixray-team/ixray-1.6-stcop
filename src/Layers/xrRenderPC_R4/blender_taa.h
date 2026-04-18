#pragma once

class CBlender_taa : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: temporal slop";	}
	virtual		bool		canBeDetailed()	{ return FALSE;	}
	virtual		bool		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_taa();
	virtual ~CBlender_taa();
};
