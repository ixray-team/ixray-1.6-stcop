// Blender_Model.h: interface for the Blender_Screen_SET class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BLENDER_MODEL_H__A215FA40_D885_4D06_9032_ED934AE295E3__INCLUDED_)
#define AFX_BLENDER_MODEL_H__A215FA40_D885_4D06_9032_ED934AE295E3__INCLUDED_
#pragma once

class CBlender_Model : public IBlender  
{
public:
	xrP_Integer	oAREF;
	xrP_BOOL	oBlend;
public:
	virtual const char*		getComment() override { return "MODEL: Default"; }
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Save			(IWriter&	fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;
	
	CBlender_Model();
	virtual ~CBlender_Model() override;

private:
	xrP_TOKEN	oTessellation;
};

#endif // !defined(AFX_BLENDER_MODEL_H__A215FA40_D885_4D06_9032_ED934AE295E3__INCLUDED_)
