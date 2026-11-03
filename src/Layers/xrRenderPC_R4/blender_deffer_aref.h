#pragma once

class CBlender_deffer_aref : public IBlender  
{
public:
	xrP_Integer	oAREF;
	xrP_BOOL	oBlend;
	bool		lmapped;
public:
	virtual const char*		getComment() override { return "LEVEL: defer-base-aref";	}
	virtual bool		canBeDetailed() override { return true;		}
	virtual bool		canBeLMAPped() override { return lmapped;	}
	virtual bool		canUseSteepParallax	() override { return true; }

	virtual void		Save			(IWriter&	fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;
	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_deffer_aref					(bool _lmapped=false);
	virtual ~CBlender_deffer_aref			() override;
};
