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
	virtual		const char*		getComment()	{ return "LEVEL: Implicit**detail";	}
	virtual		bool		canBeDetailed()	{ return true; }
	virtual		bool		canBeLMAPped()	{ return true; }

	virtual		void		Save			( IWriter&	fs);
	virtual		void		Load			( IReader&	fs, u16 version);

	virtual		void		Compile			( CBlender_Compile& C);

	CBlender_BmmD();
	virtual ~CBlender_BmmD();
};

