#pragma once

class CBlender_bloom_build : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: combine to bloom target";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_bloom_build();
	virtual ~CBlender_bloom_build();
};
