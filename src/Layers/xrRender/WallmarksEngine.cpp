// WallmarksEngine.cpp: implementation of the CWallmarksEngine class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "WallmarksEngine.h"

#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/x_ray.h"
#include "../../xrEngine/GameFont.h"
#include "SkeletonCustom.h"
namespace WallmarksEngine {
	struct wm_slot
	{
		typedef CWallmarksEngine::StaticWMVec	StaticWMVec;
		ref_shader		shader;
		StaticWMVec		static_items;
		xr_vector< intrusive_ptr<CSkeletonWallmark> > skeleton_items;
						wm_slot		(ref_shader sh)	{shader=sh;static_items.reserve(256);skeleton_items.reserve(256);}
	};
}

// #include "xr_effsun.h"

const float W_DIST_FADE		= 15.f;
const float	W_DIST_FADE_SQR	= W_DIST_FADE*W_DIST_FADE;
const float I_DIST_FADE_SQR	= 1.f/W_DIST_FADE_SQR;
const int MAX_TRIS = 16384;

IC bool operator == (const CWallmarksEngine::wm_slot* slot, const ref_shader& shader){return slot->shader==shader;}

void CWallmarksEngine::CMatrixBuilder_SizeCam::CreateMatrix(Fmatrix& out, const Fvector& FaceNormal)
{
	Fmatrix				mRot;
	float invsz = 1/sz;
	
	Fmatrix				mScale;
	Fvector				at,up,right,y;
	at.sub				(contact_point,FaceNormal);
	y.set				(0,1,0);
	if (std::abs(FaceNormal.y)>.99f) y.set(1,0,0);
	right.crossproduct	(y,FaceNormal);
	up.crossproduct		(FaceNormal,right);
	out.build_camera	(contact_point,at,up);
	mScale.scale		(invsz,invsz,invsz);
	out.mulA_43		(mScale);

	if (UseCameraDirection)
	{
		mRot.rotateZ(::Random.randF(-0.175f, 0.175f) - Device.vCameraDirection.getH());
	}
	else
	{
		mRot.rotateZ(::Random.randF(deg2rad(-20.f), deg2rad(20.f)));
	}

	out.mulA_43		(mRot);
}

void CWallmarksEngine::CMatrixBuilder_SizeCam::FindBoxCenterAndDim(Fvector& bc, Fvector& bd)
{
	Fbox bb_query;
	bb_query.set(contact_point,contact_point);
	bb_query.grow(sz*2.5f);
	bb_query.get_CD(bc,bd);
}

void CWallmarksEngine::CMatrixBuilder_WHR::CreateMatrix(Fmatrix& out, const Fvector& FaceNormal)
{
	Fmatrix				mScale, mRot;
	Fvector				at,up,right,y;
	at.sub				(contact_point,FaceNormal);
	y.set				(0,1,0);
	if (std::abs(FaceNormal.y)>.99f) y.set(1,0,0);
	right.crossproduct	(y,FaceNormal);
	up.crossproduct		(FaceNormal,right);
	out.build_camera	(contact_point,at,up);
	mRot.rotateZ(r);
	out.mulA_43		(mRot);
	mScale.scale		(2/w,2/h,std::max(2/w,2/h));
	out.mulA_43		(mScale);
}

void CWallmarksEngine::CMatrixBuilder_WHR::FindBoxCenterAndDim(Fvector& bc, Fvector& bd)
{
	Fbox bb_query;
	bb_query.set(contact_point,contact_point);
	bb_query.grow((sqrt(w*w+h*h))/2);
	bb_query.get_CD(bc,bd);
}

CWallmarksEngine::wm_slot* CWallmarksEngine::FindSlot	(ref_shader shader)
{
	WMSlotVecIt it				= std::find(marks.begin(),marks.end(),shader);
	return						(it!=marks.end())?*it:0;
}
CWallmarksEngine::wm_slot* CWallmarksEngine::AppendSlot	(ref_shader shader)
{
	marks.push_back(new wm_slot(shader));
	return marks.back();
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CWallmarksEngine::CWallmarksEngine	()
{
	static_pool.reserve		(256);
	marks.reserve			(256);
	hGeom.create			(FVF::F_LIT, RCache.Vertex.Buffer(), nullptr);
}

CWallmarksEngine::~CWallmarksEngine	()
{
	clear			();
	hGeom.destroy	();
}

void CWallmarksEngine::clear()
{
	{
		for (WMSlotVecIt p_it=marks.begin(); p_it!=marks.end(); p_it++){
			for (StaticWMVecIt m_it=(*p_it)->static_items.begin(); m_it!=(*p_it)->static_items.end(); m_it++)
				static_wm_destroy	(*m_it);
			xr_delete		(*p_it);
		}
		marks.clear	();
	}
	{
		for (u32 it=0; it<static_pool.size(); it++)
			xr_delete		(static_pool[it]);
		static_pool.clear	();
	}
}

// allocate
CWallmarksEngine::static_wallmark*	CWallmarksEngine::static_wm_allocate		()
{
	static_wallmark* W = 0;
	if (static_pool.empty())  W = new static_wallmark();
	else { W = static_pool.back(); static_pool.pop_back(); }

	W->ttl				= ps_r__WallmarkTTL;
	W->verts.clear		();
	return W;
}
// destroy
void		CWallmarksEngine::static_wm_destroy		(CWallmarksEngine::static_wallmark*	W	)
{
	static_pool.push_back	(W);
}
// render
void		CWallmarksEngine::static_wm_render		(CWallmarksEngine::static_wallmark*	W, FVF::LIT* &V)
{
	float		a		= 1-(W->ttl/ps_r__WallmarkTTL);
	int			aC		= iFloor	( a * 255.f);	clamp	(aC,0,255);
	u32			C		= color_rgba(128,128,128,aC);
	FVF::LIT*	S		= &*W->verts.begin	();
	FVF::LIT*	E		= &*W->verts.end	();
	for (; S!=E; S++, V++){
		V->p.set		(S->p);
		V->color		= C;
		V->t.set		(S->t);
	}
}
//--------------------------------------------------------------------------------
void CWallmarksEngine::RecurseTri(u32 t, Fmatrix &mView, CWallmarksEngine::static_wallmark	&W)
{
	CDB::TRI*	T			= sml_collector.getT()+t;
	if (T->dummy)			return;
	T->dummy				= 0xffffffff;
	
	// Some vars
	u32*		v_ids		= T->verts;
	Fvector*	v_data		= sml_collector.getV();
	sml_poly_src.clear		();
	sml_poly_src.push_back	(v_data[v_ids[0]]);
	sml_poly_src.push_back	(v_data[v_ids[1]]);
	sml_poly_src.push_back	(v_data[v_ids[2]]);
	sml_poly_dest.clear		();
	
	sPoly* P = sml_clipper.ClipPoly	(sml_poly_src, sml_poly_dest);
	
	//. todo
	// uv_gen = mView * []
	// UV = pos*uv_gen

	if (P) {
		// Create vertices and triangulate poly (tri-fan style triangulation)
		FVF::LIT			V0,V1,V2;
		Fvector				UV;

		mView.transform_tiny(UV, (*P)[0]);
		V0.set				((*P)[0],0,(1+UV.x)*.5f,(1-UV.y)*.5f);
		mView.transform_tiny(UV, (*P)[1]);
		V1.set				((*P)[1],0,(1+UV.x)*.5f,(1-UV.y)*.5f);

		for (u32 i=2; i<P->size(); i++)
		{
			mView.transform_tiny(UV, (*P)[i]);
			V2.set				((*P)[i],0,(1+UV.x)*.5f,(1-UV.y)*.5f);
			W.verts.push_back	(V0);
			W.verts.push_back	(V1);
			W.verts.push_back	(V2);
			V1					= V2;
		}
		
		// recurse
		for (int i=0; i<3; i++)
		{
			u32 adj					= sml_adjacency[3*t+i];
			if (0xffffffff==adj)	continue;
			CDB::TRI*	SML			= sml_collector.getT() + adj;
			v_ids					= SML->verts;

			Fvector test_normal;
			test_normal.mknormal	(v_data[v_ids[0]],v_data[v_ids[1]],v_data[v_ids[2]]);
			float cosa				= test_normal.dotproduct(sml_normal);
			if (cosa<0.034899f)		continue;	// cos(88)
			RecurseTri				(adj,mView,W);
		}
	}
}

void CWallmarksEngine::BuildMatrix	(Fmatrix &mView, float invsz, const Fvector& from)
{
	// build projection
	Fmatrix				mScale;
    Fvector				at,up,right,y;
	at.sub				(from,sml_normal);
	y.set				(0,1,0);
	if (std::abs(sml_normal.y)>.99f) y.set(1,0,0);
	right.crossproduct	(y,sml_normal);
	up.crossproduct		(sml_normal,right);
	mView.build_camera	(from,at,up);
	mScale.scale		(invsz,invsz,invsz);
	mView.mulA_43		(mScale);
}

CWallmarksEngine::static_wallmark* CWallmarksEngine::AddWallmark_internal	(CDB::TRI* pTri, const Fvector* pVerts, ref_shader hShader, IMatrixBuilder& matrix_builder, Flags8 WMFlags)
{
	// query for polygons in bounding box
	// calculate adjacency

	Fvector bbc,bbd;
	matrix_builder.FindBoxCenterAndDim(bbc,bbd);
	xrc.box_options(CDB::OPT_FULL_TEST);
	xrc.box_query(g_pGameLevel->ObjectSpace.GetStaticModel(),bbc,bbd);
	u32	triCount = xrc.r_count();
	if (0==triCount)	
		return nullptr;
	u32 real_tcnt = triCount + 1u;
	xr_vector<CDB::TRI>& tris = g_pGameLevel->ObjectSpace.GetStaticTris();
	sml_collector.clear();
	sml_collector.reserve(real_tcnt);
	sml_collector.add_face_packed_D(pVerts[pTri->verts[0]],pVerts[pTri->verts[1]],pVerts[pTri->verts[2]],0);
	for (CDB::RESULT& result : xrc.r_vec())
	{
		CDB::TRI& Tri = tris[result.id];
		if (&Tri==pTri) continue;
		auto& tridxs = Tri.verts;
		sml_collector.add_face_packed_D(pVerts[tridxs[0]],pVerts[tridxs[1]],pVerts[tridxs[2]],0);
	}
	sml_collector.calc_adjacency(sml_adjacency);


	// calc face normal
	Fvector	N;
	N.mknormal(pVerts[pTri->verts[0]],pVerts[pTri->verts[1]],pVerts[pTri->verts[2]]);
	sml_normal.set(N);

	// build 3D ortho-frustum
	Fmatrix mView;
	matrix_builder.CreateMatrix(mView, sml_normal);
	sml_clipper.CreateFromMatrix(mView,FRUSTUM_P_LRTB);

	// create wallmark
	static_wallmark* W = static_wm_allocate();
	W->verts.reserve(real_tcnt*3);
	RecurseTri(0, mView, *W);

	// calc sphere
	if (W->verts.size()<3) 
	{ 
		static_wm_destroy(W); 
		return nullptr; 
	}
	else 
	{
		Fbox bb; bb.invalidate();
		FVF::LIT* I=&*W->verts.begin();
		FVF::LIT* E=&*W->verts.end();
		for (; I!=E; I++) bb.modify(I->p);
		bb.getsphere(W->bounds.P, W->bounds.R);
	}

//	if (W->bounds.R < 1.f)	
	{
		// search if similar wallmark exists
		wm_slot* slot = FindSlot(hShader);
		if (slot)
		{
			StaticWMVecIt it = slot->static_items.begin();
			StaticWMVecIt end =	slot->static_items.end();
			for (; it!=end; it++)	
			{
				static_wallmark* wm = *it;
				if (wm->bounds.P.similar(W->bounds.P,0.02f))
				{ // replace
					static_wm_destroy(wm);
					*it = W;
					return W;
				}
			}
		} else
			slot = AppendSlot(hShader);

		// no similar - register _new_
		slot->static_items.push_back(W);
	}
	return W;
}

CWallmarksEngine::static_wallmark* CWallmarksEngine::AddStaticWallmark(CDB::TRI* pTri, const Fvector* pVerts, const Fvector &contact_point, ref_shader hTexture, float sz, Flags8 flags, bool UseCameraDirection)
{
	// optimization cheat: don't allow wallmarks more than 100 m from viewer/actor
	if (!flags.test(StaticWallmarkHandle::flForceSpawn) && contact_point.distance_to_sqr(Device.vCameraPosition) > _sqr(100.f))
	{
		return nullptr;
	}

	CMatrixBuilder_SizeCam builder(contact_point, sz, UseCameraDirection);

	// Physics may add wallmarks in parallel with rendering
	lock.Enter				();
	auto result = AddWallmark_internal(pTri,pVerts,hTexture,builder, flags);
	lock.Leave				();
	return result;
}

CWallmarksEngine::static_wallmark* CWallmarksEngine::AddStaticWallmark(CDB::TRI* pTri, const Fvector* pVerts,
	const Fvector& contact_point, ref_shader hTexture, float w, float h, float r, Flags8 flags)
{
	// optimization cheat: don't allow wallmarks more than 100 m from viewer/actor
	if (!flags.test(StaticWallmarkHandle::flForceSpawn) && contact_point.distance_to_sqr(Device.vCameraPosition) > _sqr(100.f))
	{
		return nullptr;
	}

	CMatrixBuilder_WHR builder(contact_point, w, h, r);

	// Physics may add wallmarks in parallel with rendering
	lock.Enter				();
	auto result = AddWallmark_internal(pTri,pVerts,hTexture,builder, flags);
	lock.Leave				();
	return result;
}
extern bool ps_r__WallmarkDyn;
void CWallmarksEngine::AddSkeletonWallmark	(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size)
{	
	if(!ps_r__WallmarkDyn || ::RImplementation.phase != CRender::PHASE_NORMAL)				return;
	// optimization cheat: don't allow wallmarks more than 50 m from viewer/actor
	if (xf->c.distance_to_sqr(Device.vCameraPosition) > _sqr(50.f))				return;

	VERIFY					(obj&&xf&&(size>EPS_L));
	lock.Enter				();
	obj->AddWallmark		(xf,start,dir,sh,size);
	lock.Leave				();
}

void CWallmarksEngine::AddSkeletonWallmark(intrusive_ptr<CSkeletonWallmark> wm)
{
	if(!ps_r__WallmarkDyn || ::RImplementation.phase != CRender::PHASE_NORMAL) return;

	if (!::RImplementation.val_bHUD)
	{
		lock.Enter			();
		// search if similar wallmark exists
		wm_slot* slot		= FindSlot	(wm->Shader());
		if (0==slot) slot	= AppendSlot(wm->Shader());
		// no similar - register _new_
		slot->skeleton_items.push_back(wm);
		lock.Leave			();
	}
}

extern float r_ssaDISCARD;
ICF void BeginStream(ref_geom hGeom, u32& w_offset, FVF::LIT*& w_verts, FVF::LIT*& w_start)
{
	PROF_EVENT("BeginStream");
	w_offset				= 0;
	w_verts					= (FVF::LIT*)RCache.Vertex.Lock	(MAX_TRIS*3,hGeom->vb_stride,w_offset);
	w_start					= w_verts;
}

ICF void FlushStream(ref_geom hGeom, ref_shader shader, u32& w_offset, FVF::LIT*& w_verts, FVF::LIT*& w_start, bool bSuppressCull)
{
	PROF_EVENT("FlushStream");
	u32 w_count					= u32(w_verts-w_start);
	RCache.Vertex.Unlock		(w_count,hGeom->vb_stride);
	if (w_count)
	{
		RCache.set_Shader(shader);
		RCache.set_Geometry(hGeom);
		if (bSuppressCull)
		{
			GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		}

		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, w_offset, w_count / 3);
		if (bSuppressCull)
		{
			GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
		}
		Device.Statistic->RenderDUMP_WMT_Count += w_count / 3;
	}
}

void CWallmarksEngine::Render()
{
//	if (marks.empty())			return;
	// Projection and xform
	float	_43					= Device.mProject._43;
	Device.mProject._43			-= ps_r__WallmarkSHIFT; 
	RCache.set_xform_world		(Fidentity);
	RCache.set_xform_project	(Device.mProject);

	Fmatrix	mSavedView			= Device.mView;
	Fvector	mViewPos			;
			mViewPos.mad		(Device.vCameraPosition, Device.vCameraDirection,ps_r__WallmarkSHIFT_V);
	Device.mView.build_camera_dir	(mViewPos,Device.vCameraDirection,Device.vCameraTop);
	RCache.set_xform_view		(Device.mView);

	Device.Statistic->RenderDUMP_WM.Begin	();
	Device.Statistic->RenderDUMP_WMS_Count	= 0;
	Device.Statistic->RenderDUMP_WMD_Count	= 0;
	Device.Statistic->RenderDUMP_WMT_Count	= 0;

	float	ssaCLIP				= r_ssaDISCARD/4;

	lock.Enter		();			// Physics may add wallmarks in parallel with rendering
	
	for (WMSlotVecIt slot_it=marks.begin(); slot_it!=marks.end(); slot_it++){
		u32			w_offset;
		FVF::LIT	*w_verts, *w_start;
		BeginStream	(hGeom,w_offset,w_verts,w_start);
		wm_slot* slot			= *slot_it;	
		// static wallmarks
		{
			PROF_EVENT("STATIC_WALLMARKS");
			for (StaticWMVecIt w_it=slot->static_items.begin(); w_it!=slot->static_items.end(); ){
				static_wallmark* W	= *w_it;
				if (RImplementation.ViewBase.testSphere_dirty(W->bounds.P,W->bounds.R)){
					Device.Statistic->RenderDUMP_WMS_Count++;
					float dst	= Device.vCameraPosition.distance_to_sqr(W->bounds.P);
					float ssa	= W->bounds.R * W->bounds.R / dst;
					if (ssa>=ssaCLIP)	{
						u32 w_count		= u32(w_verts-w_start);
						if ((w_count+W->verts.size())>=(MAX_TRIS*3)){
							FlushStream	(hGeom,slot->shader,w_offset,w_verts,w_start,FALSE);
							BeginStream	(hGeom,w_offset,w_verts,w_start);
						}
						static_wm_render	(W,w_verts);
					}
					W->ttl	-= 0.1f*Device.fTimeDelta;	// visible wallmarks fade much slower
				} else {
					W->ttl	-= Device.fTimeDelta;
				}
				if (W->ttl<=EPS){	
					static_wm_destroy	(W);
					*w_it				= slot->static_items.back();
					slot->static_items.pop_back();
				}else{
					w_it++;
				}
			}
		}
		// Flush stream
		FlushStream				(hGeom,slot->shader,w_offset,w_verts,w_start,FALSE);	//. remove line if !(suppress cull needed)
		BeginStream				(hGeom,w_offset,w_verts,w_start);

		// dynamic wallmarks
		{
			PROF_EVENT("DYNAMIC_WALLMARKS");
			for (xr_vector<intrusive_ptr<CSkeletonWallmark> >::iterator w_it=slot->skeleton_items.begin(); w_it!=slot->skeleton_items.end(); w_it++){
				intrusive_ptr<CSkeletonWallmark> W	= *w_it;
				if (!W){
					continue	;
				}
				float dst	= Device.vCameraPosition.distance_to_sqr(W->m_Bounds.P);
				float ssa	= W->m_Bounds.R * W->m_Bounds.R / dst;
				if (ssa>=ssaCLIP){
					Device.Statistic->RenderDUMP_WMD_Count++;
					u32 w_count		= u32(w_verts-w_start);
					if ((w_count+W->VCount())>=(MAX_TRIS*3)){
						FlushStream	(hGeom,slot->shader,w_offset,w_verts,w_start,TRUE);
						BeginStream	(hGeom,w_offset,w_verts,w_start);
					}

					FVF::LIT	*w_save = w_verts;
					try {
						W->Parent()->RenderWallmark	(W,w_verts);
					} catch (...)
					{
						Msg		("! Failed to render dynamic wallmark");
						w_verts = w_save;
					}
				}
			}
			slot->skeleton_items.clear();
		}
		// Flush stream
		FlushStream				(hGeom,slot->shader,w_offset,w_verts,w_start,TRUE);
	}

	lock.Leave();				// Physics may add wallmarks in parallel with rendering

	// Level-wmarks
	RImplementation.r_dsgraph_render_wmarks	();
	Device.Statistic->RenderDUMP_WM.End		();

	// Projection
	Device.mView				= mSavedView;
	Device.mProject._43			= _43;
	RCache.set_xform_view		(Device.mView);
	RCache.set_xform_project	(Device.mProject);
}
