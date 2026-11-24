// WallmarksEngine.h: interface for the CWallmarksEngine class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "../../xrEngine/WallmarkHandle.h"

namespace WallmarksEngine {
	struct wm_slot;
}

class CSkeletonWallmark;
class CKinematics;

class CWallmarksEngine
{
public:
	typedef WallmarksEngine::wm_slot	wm_slot;

public:
	struct static_wallmark 
	{
		Fsphere				bounds;
		// From the world into the box the mark is projected through: x and y within -1 .. 1, z along the wall's normal
		Fmatrix				Project;
		Flags8				flags;
		float				ttl;
		xr_shared_ptr<StaticWallmarkHandle::CWallmarkHandle> handler;
	};

	using StaticWMVec = xr_vector<static_wallmark*>;
	using StaticWMVecIt = StaticWMVec::iterator;

	using WMSlotVec = xr_vector<wm_slot*>;
	using WMSlotVecIt = WMSlotVec::iterator;

private:
	StaticWMVec			static_pool;
	WMSlotVec			marks;
	xrCriticalSection	lock;

	class IMatrixBuilder
	{
	protected:
		Fvector contact_point;
	public:
		virtual void CreateMatrix(Fmatrix& out, const Fvector& FaceNormal) = 0;
		const Fvector& Contact() const { return contact_point; }
	};

	class CMatrixBuilder_SizeCam : public IMatrixBuilder
	{
		float sz;
		bool UseCameraDirection;
	public:
		CMatrixBuilder_SizeCam(const Fvector& contact_point, float sz, bool UseCameraDirection)
			: sz(sz), UseCameraDirection(UseCameraDirection) { this->contact_point = contact_point; }

		virtual void CreateMatrix(Fmatrix& out, const Fvector& FaceNormal) override;
	};

	class CMatrixBuilder_WHR : public IMatrixBuilder
	{
		float w, h, r;
	public:
		CMatrixBuilder_WHR(Fvector contact_point, float w, float h, float r)
			: w(w), h(h), r(r) { this->contact_point = contact_point; }

		virtual void CreateMatrix(Fmatrix& out, const Fvector& FaceNormal) override;
	};
private:
	wm_slot*			FindSlot				(ref_shader shader);
	wm_slot*			AppendSlot				(ref_shader shader);
private:
	static_wallmark*	AddWallmark_internal	(CDB::TRI* pTri, const Fvector* pVerts, ref_shader hTexture, IMatrixBuilder& matrix_builder, Flags8 WMFlags);

	static_wallmark*	static_wm_allocate		();
	void				static_wm_destroy		(static_wallmark*	W	);

public:
						CWallmarksEngine		();
						~CWallmarksEngine		();
	// edit wallmarks
	static_wallmark*	AddStaticWallmark		(CDB::TRI* pTri, const Fvector* pVerts, const Fvector &contact_point, ref_shader hTexture, float sz, Flags8 flags, bool UseCameraDirection = false);
	static_wallmark*	AddStaticWallmark		(CDB::TRI* pTri, const Fvector* pVerts, const Fvector &contact_point, ref_shader hTexture, float w, float h, float r, Flags8 flags);
	void				AddSkeletonWallmark		(intrusive_ptr<CSkeletonWallmark> wm);
	void				AddSkeletonWallmark		(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size);

	// render
	// The marks on skinned models and the level's baked ones; the static marks are decals, see CollectDecals
	void				Render					(GfxCmdList& Cmd);

	// A mark as a decal: the box it is projected through, the sphere around it, how much of it is left, the slot
	// of its shader and that shader's texture
	struct DecalSource
	{
		Fmatrix Project;
		Fsphere Bounds;
		float Opacity;
		u32 Slot;
		shared_str Texture;
	};

	// Ages the static marks, then writes those in view nearest to Eye, Limit at most; returns how many
	u32					CollectDecals			(DecalSource* Out, u32 Limit, const Fvector& Eye);

private:
	// The marks on skinned models in view this frame: their boxes move with the bones, so they are handed in anew
	xr_vector<DecalSource> Moving;
	u32					SlotIndex				(ref_shader shader);

public:

	void				clear					();
};
