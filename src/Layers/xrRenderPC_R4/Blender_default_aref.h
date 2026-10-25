// Blender_default_aref.h: interface for the CBlender_default_aref class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

class CBlender_default_aref : public IBlender  
{
public:
	xrP_Integer	oAREF;
	xrP_BOOL	oBlend;
public:
	virtual const char*		getComment() override { return "LEVEL: lmap*base.aref";	}
	virtual bool		canBeDetailed() override { return true; }
	virtual bool		canBeLMAPped() override { return true; }

	virtual void		Save			(IWriter&	fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_default_aref();
	virtual ~CBlender_default_aref() override;
};