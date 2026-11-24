// BlenderDefault.h: interface for the CBlenderDefault class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

class CBlender_default		: public IBlender  
{
public:
	virtual const char*		getComment() override { return "LEVEL: lmap*base (default)";	}
	virtual bool		canBeDetailed() override { return true; }
	virtual bool		canBeLMAPped() override { return true; }

#ifdef _EDITOR
	virtual		bool		canUseSteepParallax() {
		return true;
	}
#endif

	virtual void		Save			(IWriter&	fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_default();
	virtual ~CBlender_default() override;

private:
	xrP_TOKEN	oTessellation;
};