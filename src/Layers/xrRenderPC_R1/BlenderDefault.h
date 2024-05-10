// BlenderDefault.h: interface for the CBlenderDefault class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

class CBlender_default		: public IBlender  
{
public:
	virtual		LPCSTR		getComment()	{ return "LEVEL: lmap*base (default)";	}
	virtual		BOOL		canBeDetailed()	{ return TRUE; }
	virtual		BOOL		canBeLMAPped()	{ return TRUE; }

	virtual		void		Save			(IWriter&	fs);
	virtual		void		Load			(IReader&	fs, u16 version);

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_default();
	virtual ~CBlender_default();

private:
	xrP_TOKEN	oTessellation;
};