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
		xr_vector<FVF::LIT>	verts;
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
	ref_geom			hGeom;

	Fvector				sml_normal;
	CFrustum			sml_clipper;
	sPoly				sml_poly_dest;
	sPoly				sml_poly_src;

	CDB::COLLIDER		xrc;
	CDB::Collector		sml_collector;
	xr_vector<u32>		sml_adjacency;

	xrCriticalSection	lock;

	class IMatrixBuilder
	{
	protected:
		Fvector contact_point;
	public:
		virtual void CreateMatrix(Fmatrix& out, const Fvector& FaceNormal) = 0;
		virtual void FindBoxCenterAndDim(Fvector& bc, Fvector& bd) = 0;
	};

	class CMatrixBuilder_SizeCam : public IMatrixBuilder
	{
		float sz;
		bool UseCameraDirection;
	public:
		CMatrixBuilder_SizeCam(const Fvector& contact_point, float sz, bool UseCameraDirection)
			: sz(sz), UseCameraDirection(UseCameraDirection) { this->contact_point = contact_point; }
		virtual void CreateMatrix(Fmatrix& out, const Fvector& FaceNormal) override;
		virtual void FindBoxCenterAndDim(Fvector& bc, Fvector& bd) override;
	};

	class CMatrixBuilder_WHR : public IMatrixBuilder
	{
		float w, h, r;
	public:
		CMatrixBuilder_WHR(Fvector contact_point, float w, float h, float r)
			: w(w), h(h), r(r) { this->contact_point = contact_point; }
		virtual void CreateMatrix(Fmatrix& out, const Fvector& FaceNormal) override;
		virtual void FindBoxCenterAndDim(Fvector& bc, Fvector& bd) override;
	};
private:
	wm_slot*			FindSlot				(ref_shader shader);
	wm_slot*			AppendSlot				(ref_shader shader);
private:
	void				BuildMatrix				(Fmatrix &dest, float invsz, const Fvector& from);
	void				RecurseTri				(u32 T,	Fmatrix &mView, static_wallmark	&W);
	static_wallmark*	AddWallmark_internal	(CDB::TRI* pTri, const Fvector* pVerts, ref_shader hTexture, IMatrixBuilder& matrix_builder, Flags8 WMFlags);

	static_wallmark*	static_wm_allocate		();
	void				static_wm_render		(static_wallmark*	W, FVF::LIT* &V);
	void				static_wm_destroy		(static_wallmark*	W	);

	void				skeleton_wm_render		(intrusive_ptr<CSkeletonWallmark>, FVF::LIT* &V);
public:
						CWallmarksEngine		();
						~CWallmarksEngine		();
	// edit wallmarks
	static_wallmark*	AddStaticWallmark		(CDB::TRI* pTri, const Fvector* pVerts, const Fvector &contact_point, ref_shader hTexture, float sz, Flags8 flags, bool UseCameraDirection = false);
	static_wallmark*	AddStaticWallmark		(CDB::TRI* pTri, const Fvector* pVerts, const Fvector &contact_point, ref_shader hTexture, float w, float h, float r, Flags8 flags);
	void				AddSkeletonWallmark		(intrusive_ptr<CSkeletonWallmark> wm);
	void				AddSkeletonWallmark		(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size);

	// render
	void				Render					();

	void				clear					();
};
