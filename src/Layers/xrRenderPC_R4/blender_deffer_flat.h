#pragma once

class CBlender_deffer_flat : public IBlender  
{
public:
	virtual const char*		getComment() override { return "LEVEL: defer-base-normal";	}
	virtual bool		canBeDetailed() override { return true;	}
	// B_DEFAULT and B_VERT share this class; only B_DEFAULT is lightmapped (CLS is set by blender_create)
	virtual bool		canBeLMAPped() override { return getDescription().CLS == B_DEFAULT;	}
	virtual bool		canUseSteepParallax	() override { return true; }

	virtual void		Save			(IWriter&	fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;
	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_deffer_flat();
	virtual ~CBlender_deffer_flat() override;

private:
	xrP_TOKEN	oTessellation;
};
