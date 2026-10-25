// BlenderDefault.h: interface for the CBlenderDefault class.
//
//////////////////////////////////////////////////////////////////////

#pragma once

class CBlender_BmmD : public IBlender  
{
public:
	string64	oT2_Name;	// name of secondary texture
	string64	oT2_xform;	// xform for secondary texture
	string64	oR_Name	;	//. задел на будущее
	string64	oG_Name	;	//. задел на будущее
	string64	oB_Name	;	//. задел на будущее
	string64	oA_Name	;	//. задел на будущее
public:
	virtual const char*		getComment() override { return "LEVEL: Implicit**detail";	}
	virtual bool		canBeDetailed() override { return true; }
	virtual bool		canBeLMAPped() override { return true; }

	virtual void		Save			( IWriter&	fs) override;
	virtual void		Load			( IReader&	fs, u16 version) override;

	virtual void		Compile			( CBlender_Compile& C) override;

	CBlender_BmmD();
	virtual ~CBlender_BmmD() override;
};

