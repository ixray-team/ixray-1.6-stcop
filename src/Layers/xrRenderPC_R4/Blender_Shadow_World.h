#ifndef BLENDER_SHADOW_WORLD_H
#define BLENDER_SHADOW_WORLD_H
#pragma once

class CBlender_ShWorld : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: shadow projecting";	}
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Save			(IWriter&  fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_ShWorld();
	virtual ~CBlender_ShWorld() override;
};

#endif //BLENDER_SHADOW_WORLD_H
