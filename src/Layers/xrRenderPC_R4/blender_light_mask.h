#pragma once

class CBlender_accum_direct_mask : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: mask direct light";	}
	virtual		bool		canBeDetailed()	{ return false;	}
	virtual		bool		canBeLMAPped()	{ return false;	}

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_accum_direct_mask();
	virtual ~CBlender_accum_direct_mask();
};

