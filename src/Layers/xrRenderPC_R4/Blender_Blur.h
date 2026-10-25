#pragma once
#include "blenders/Blender_Recorder.h"

class CBlender_Blur : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: blur";	}
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Save			(IWriter&  fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_Blur();
	virtual ~CBlender_Blur() override;
};