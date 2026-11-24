// Blender_Screen_GRAY.h: interface for the CBlender_Screen_GRAY class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BLENDER_SCREEN_GRAY_H__483E49EF_23EC_4810_9231_7EE4BD72CC3B__INCLUDED_)
#define AFX_BLENDER_SCREEN_GRAY_H__483E49EF_23EC_4810_9231_7EE4BD72CC3B__INCLUDED_
#pragma once

class CBlender_Screen_GRAY : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: gray-scale effect"; }
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Save			(IWriter&  fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;
	
	CBlender_Screen_GRAY();
	virtual ~CBlender_Screen_GRAY() override;

};

#endif // !defined(AFX_BLENDER_SCREEN_GRAY_H__483E49EF_23EC_4810_9231_7EE4BD72CC3B__INCLUDED_)
