// BlenderDefault.h: interface for the CBlenderDefault class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "../xrRender/blenders/Blender_Recorder.h"

class CBlender_LaEmB : public IBlender  
{
public:
	string64	oT2_Name;		// name of secondary texture
	string64	oT2_xform;		// xform for secondary texture
	string64	oT2_const;
	
	void		compile_ED	(CBlender_Compile& C);
	void		compile_EDc	(CBlender_Compile& C);
	void		compile_L	(CBlender_Compile& C);
	void		compile_Lc	(CBlender_Compile& C);
	void		compile_2	(CBlender_Compile& C);
	void		compile_2c	(CBlender_Compile& C);
	void		compile_3	(CBlender_Compile& C);
	void		compile_3c	(CBlender_Compile& C);
public:
	virtual		LPCSTR		getComment()	{ return "LEVEL: (lmap+env*const)*base";	}
	virtual		BOOL		canBeLMAPped()	{ return TRUE; }

	virtual		void		Save			(IWriter&  fs);
	virtual		void		Load			(IReader&	fs, u16 version);

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_LaEmB();
	virtual ~CBlender_LaEmB();
};