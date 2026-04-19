#pragma once

class CBlender_deffer_flat : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "LEVEL: defer-base-normal";	}
	virtual		bool		canBeDetailed()	{ return true;	}
	virtual		bool		canBeLMAPped()	{ return false;	}
	virtual		bool		canUseSteepParallax	()	{ return true; }

	virtual		void		Save			(IWriter&	fs);
	virtual		void		Load			(IReader&	fs, u16 version);
	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_deffer_flat();
	virtual ~CBlender_deffer_flat();

private:
	xrP_TOKEN	oTessellation;
};
