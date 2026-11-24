#ifndef BLENDER_EDITOR_WIRE_H
#define BLENDER_EDITOR_WIRE_H
#pragma once

class CBlender_Editor_Wire : public IBlender  
{
	string64	oT_Factor;
public:
	virtual const char*		getComment() override { return "EDITOR: wire";	}
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Save			(IWriter&  fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_Editor_Wire();
	virtual ~CBlender_Editor_Wire() override;
};

#endif //BLENDER_EDITOR_WIRE_H
