// BlenderDefault.h: interface for the CBlenderDefault class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

class CBlender_default		: public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "LEVEL: lmap*base (default)";	}
	virtual		bool		canBeDetailed()	{ return TRUE; }
	virtual		bool		canBeLMAPped()	{ return TRUE; }

#ifdef _EDITOR
	virtual		bool		canUseSteepParallax() {
		return TRUE;
	}
#endif

	virtual		void		Save			(IWriter&	fs);
	virtual		void		Load			(IReader&	fs, u16 version);

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_default();
	virtual ~CBlender_default();

private:
	xrP_TOKEN	oTessellation;
};