// Blender_Vertex_aref.h: interface for the CBlender_Vertex_aref class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BLENDER_VERTEX_AREF_H__07141B30_A968_407E_86F8_D12702FE0B9B__INCLUDED_2)
#define AFX_BLENDER_VERTEX_AREF_H__07141B30_A968_407E_86F8_D12702FE0B9B__INCLUDED_2
#pragma once

class CBlender_Detail_Still : public IBlender  
{
public:
	xrP_BOOL	oBlend;
public:
	virtual const char*		getComment() override { return "LEVEL: detail objects";	}
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Save			(IWriter&  fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_Detail_Still();
	virtual ~CBlender_Detail_Still() override;
};

#endif // !defined(AFX_BLENDER_VERTEX_AREF_H__07141B30_A968_407E_86F8_D12702FE0B9B__INCLUDED_2)
