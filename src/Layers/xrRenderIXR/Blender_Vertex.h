// Blender_Vertex.h: interface for the CBlender_Vertex class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BLENDER_VERTEX_H__D3B42F77_7018_4672_B6A5_6EE6BD947662__INCLUDED_)
#define AFX_BLENDER_VERTEX_H__D3B42F77_7018_4672_B6A5_6EE6BD947662__INCLUDED_
#pragma once

class CBlender_Vertex : public IBlender  
{
public:
	virtual const char*		getComment() override { return "LEVEL: diffuse*base";	}
	virtual bool		canBeDetailed() override { return true;	}
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Save			(IWriter&  fs) override;
	virtual void		Load			(IReader&	fs, u16 version) override;

	virtual void		Compile			(CBlender_Compile& C) override;

	CBlender_Vertex();
	virtual ~CBlender_Vertex() override;

private:
	xrP_TOKEN	oTessellation;
};

#endif // !defined(AFX_BLENDER_VERTEX_H__D3B42F77_7018_4672_B6A5_6EE6BD947662__INCLUDED_)
