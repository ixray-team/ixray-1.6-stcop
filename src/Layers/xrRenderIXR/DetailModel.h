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
	u32					FarCount = 0; // the indices each mesh starts with that make its far mesh, 0 without one
	float				FarCover = 1.0f; // the share of the model's area its far mesh keeps
	u32					SceneCluster = u32(-1); // GpuScene::GrassModel::Cluster
	shared_str			Texture;

	virtual void	LoadGeom();
	
	xr_vector<SlotItem> m_items[2][3];
	u32 FarStart[2][3] = {}; // where the instances drawn with the far mesh start in each list
	CDetail();
	void Load(IReader* S);
	bool LoadFromDM(const char* dm_path);
	void Optimize();
	virtual void Unload();

	virtual void transfer(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset) override;
	virtual void transfer(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset, float du, float dv) override;
	virtual ~CDetail() override;
};