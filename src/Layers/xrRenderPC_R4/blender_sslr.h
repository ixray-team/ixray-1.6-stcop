#pragma once

class CBlender_sslr : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: screen space reflections";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_sslr();
	virtual ~CBlender_sslr();
};
