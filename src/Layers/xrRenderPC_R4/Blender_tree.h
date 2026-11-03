// Blender_Tree.h: interface for the CBlender_Tree class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BLENDER_VERTEX_AREF_H__07141B30_A968_407E_86F8_D12702FE0B9B__INCLUDED_3)
#define AFX_BLENDER_VERTEX_AREF_H__07141B30_A968_407E_86F8_D12702FE0B9B__INCLUDED_3
#pragma once

class CBlender_Tree : public IBlender  
{
public:
	xrP_BOOL	oBlend;
	xrP_BOOL	oNotAnTree;
public:
	virtual const char*		getComment() override { return "LEVEL: trees/bushes";	}
	virtual bool		canBeLMAPped() override { return false; }
	virtual bool		canBeDetailed() override { return true; }

	virtual void		Save			(IWriter&	fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_Tree();
	virtual ~CBlender_Tree() override;
};

#endif // !defined(AFX_BLENDER_VERTEX_AREF_H__07141B30_A968_407E_86F8_D12702FE0B9B__INCLUDED_3)
