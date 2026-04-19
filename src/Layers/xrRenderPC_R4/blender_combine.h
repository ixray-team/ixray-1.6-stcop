#pragma once

class CBlender_combine : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: combiner";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_combine();
	virtual ~CBlender_combine();
};
