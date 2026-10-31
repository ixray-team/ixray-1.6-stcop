// BlenderDefault.h: interface for the CBlenderDefault class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "blenders/Blender_Recorder.h"

class CBlender_LaEmB : public IBlender  
{
public:
	string64	oT2_Name;		// name of secondary texture
	string64	oT2_xform;		// xform for secondary texture
	string64	oT2_const;

	virtual		const char*		getComment()	{ return "LEVEL: (lmap+env*const)*base";	}
	virtual		bool		canBeLMAPped()	{ return true; }

	virtual		void		Save			(IWriter&  fs);
	virtual		void		Load			(IReader&	fs, u16 version);

	virtual		void		Compile			(CBlender_Compile& C);

	CBlender_LaEmB();
	virtual ~CBlender_LaEmB();
};