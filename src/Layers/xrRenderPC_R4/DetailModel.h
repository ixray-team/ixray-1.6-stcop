#pragma once
#include "RenderAPI.h"

#include "IRenderDetailModel.h"

class RENDER_API CDetail :
	public IRender_DetailModel
{
public:
	struct alignas(16) InstanceTransform
	{
		Fvector quat;
		float scale;
		Fvector pos;
		float c_hemi;
	};
	struct alignas(16) StaticItem : InstanceTransform
	{
		float trample_strength;
		float trample_visual;
		float trample_dirX;
		float trample_dirZ;
	};
	struct alignas(16) SlotItem : StaticItem
	{
		Fvector4 WindBend;
		Fvector4 WindPrevious;
		Fvector4 WindVelocity;
		// xyz: previous derivative (entry flow/anchor on a trail); w: cached plant identity.
		Fvector4 WindPreviousVelocity;
	};

	ref_geom hw_Geom;
	ref_geom WindLowGeometry;
	IRHIBuffer* WindLowVertexBuffer = nullptr;
	IRHIBuffer* WindLowIndexBuffer = nullptr;
	u32 WindLowVertexCount = 0;
	u32 WindLowIndexCount = 0;
	IRHIBuffer* hw_VB;
	IRHIBuffer* hw_IB;

	struct InstancePacket
	{
		std::pair<IRHIBuffer*, IRHIShaderResourceView*> Buffers[4] = {};
		u32 Counts[4] = {};
		bool FarHasTrails = false;
	};
	InstancePacket RenderInstances;
	InstancePacket PreparedInstances;
	xr_vector<SlotItem> ProducerItems[3];
	xr_vector<StaticItem> ProducerStaticItems;
	xr_vector<InstanceTransform> ProducerDistantItems;
	xr_vector<u32> ProducerTrailIndices[4];
	static_assert(sizeof(StaticItem) == 48);
	static_assert(sizeof(InstanceTransform) == 32);
	static_assert(sizeof(SlotItem) == 112);
	virtual void LoadGeom();
	CDetail();
	shared_str TextureName;
	void Load(IReader* S);
	bool LoadFromDM(const char* dm_path);
	void Optimize();
	void ResetInstanceBuffers();
	virtual void Unload();

	virtual void transfer(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset) override;
	virtual void transfer(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset, float du, float dv) override;
	virtual ~CDetail() override;
};
