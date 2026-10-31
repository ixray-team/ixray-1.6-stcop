#pragma once
#include "RenderAPI.h"
#include "GeometryHeap.h"

#include "IRenderDetailModel.h"

class RENDER_API CDetail:
	public IRender_DetailModel
{
public:
	struct alignas(16) SlotItem
	{
		Fvector quat;
		float scale;
		Fvector pos;
		float c_hemi;
		float trample_strength;
		float trample_visual;
		float trample_dirX;
		float trample_dirZ;
	};
	
	ref_geom			hw_Geom;
	GeometryAlloc		Alloc = {};
	ref_geom			TrimGeom;
	GeometryAlloc		TrimAlloc = {};
	shared_str			Texture;

	std::pair<IRHIBuffer*, IRHIShaderResourceView*> DetailGPUBoundBuffers[2][3];
	virtual void	LoadGeom();
	
	xr_vector<SlotItem> m_items[2][3];
	CDetail();
	void Load(IReader* S);
	bool LoadFromDM(const char* dm_path);
	void Optimize();
	bool Trim(xr_vector<fvfVertexIn>& Verts, xr_vector<u16>& Inds) const;
	virtual void Unload();

	virtual void transfer(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset);
	virtual void transfer(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset, float du, float dv);
	virtual ~CDetail();
};