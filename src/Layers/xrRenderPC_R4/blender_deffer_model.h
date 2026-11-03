#pragma once

class CBlender_deffer_model : public IBlender  
{
public:
	xrP_Integer	oAREF;
	xrP_BOOL	oBlend;
public:
	virtual const char*		getComment() override { return "LEVEL: deffer-model-flat";	}
	virtual bool		canBeDetailed() override { return true;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Save			(IWriter&	fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;
	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_deffer_model();
	virtual ~CBlender_deffer_model() override;

private:
	xrP_TOKEN	oTessellation;
};
