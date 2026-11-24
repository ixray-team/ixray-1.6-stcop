// WallmarksEngine.cpp: implementation of the CWallmarksEngine class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "WallmarksEngine.h"

#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/x_ray.h"
#include "../../xrEngine/GameFont.h"
#include "SkeletonCustom.h"
#include "ShaderBindings.h"
namespace WallmarksEngine {
	struct wm_slot
	{
		typedef CWallmarksEngine::StaticWMVec	StaticWMVec;
		ref_shader		shader;
		StaticWMVec		static_items;
						wm_slot		(ref_shader sh)	{shader=sh;static_items.reserve(256);}
	};
}

// #include "xr_effsun.h"

const float W_DIST_FADE		= 15.f;
const float	W_DIST_FADE_SQR	= W_DIST_FADE*W_DIST_FADE;
const float I_DIST_FADE_SQR	= 1.f/W_DIST_FADE_SQR;
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
		mRot.rotateZ(::Random.randF(-0.175f, 0.175f) - RFrame.vCameraDirection.getH());
	}
	else
	{
		mRot.rotateZ(::Random.randF(deg2rad(-20.f), deg2rad(20.f)));
	}

	out.mulA_43		(mRot);
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
}

CWallmarksEngine::~CWallmarksEngine	()
{
	clear			();
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
	return W;
}
// destroy
void		CWallmarksEngine::static_wm_destroy		(CWallmarksEngine::static_wallmark*	W	)
{
	if (W->handler)
	{
		W->handler->Reset();
		W->handler.reset();
	}
	W->flags.zero();
	static_pool.push_back	(W);
}
// A static mark is the box it is projected through, on the wall of pTri, and the sphere around that box
CWallmarksEngine::static_wallmark* CWallmarksEngine::AddWallmark_internal	(CDB::TRI* pTri, const Fvector* pVerts, ref_shader hShader, IMatrixBuilder& matrix_builder, Flags8 WMFlags)
{
	Fvector Normal;
	Normal.mknormal(pVerts[pTri->verts[0]], pVerts[pTri->verts[1]], pVerts[pTri->verts[2]]);

	static_wallmark* W = static_wm_allocate();
	W->flags = WMFlags;
	W->handler = WMFlags.test(StaticWallmarkHandle::flHandler) ? xr_make_shared<StaticWallmarkHandle::CWallmarkHandle>(&W->flags) : nullptr;
	matrix_builder.CreateMatrix(W->Project, Normal);

	const Fmatrix& Box = W->Project;
	W->bounds.P = matrix_builder.Contact();
	W->bounds.R = _sqrt(1.0f / (_sqr(Box._11) + _sqr(Box._21) + _sqr(Box._31)) + 1.0f / (_sqr(Box._12) + _sqr(Box._22) + _sqr(Box._32)));

	wm_slot* slot = FindSlot(hShader);

	if (!slot)
	{
		slot = AppendSlot(hShader);
	}

	// A mark at the place of another replaces it
	for (static_wallmark*& Other : slot->static_items)
	{
		if (Other->bounds.P.similar(W->bounds.P, 0.02f))
		{
			static_wm_destroy(Other);
			Other = W;
			return W;
		}
	}

	slot->static_items.push_back(W);
	return W;
}

// The mark's own texture is the one its pass binds as s_base: the list may hold others before it
static shared_str MarkTexture(const ref_shader& Shader)
{
	if (Shader && Shader->E[0] && !Shader->E[0]->passes.empty() && Shader->E[0]->passes[0]->T)
	{
		for (const std::pair<u32, ref_texture>& Bound : *Shader->E[0]->passes[0]->T)
		{
			if (Bound.first == ShaderBindings::Texture("s_base"))
			{
				return Bound.second->cName;
			}
		}
	}

	return shared_str();
}

u32 CWallmarksEngine::SlotIndex(ref_shader shader)
{
	if (!FindSlot(shader))
	{
		AppendSlot(shader);
	}

	return u32(std::find(marks.begin(), marks.end(), shader) - marks.begin());
}

u32 CWallmarksEngine::CollectDecals(DecalSource* Out, u32 Limit, const Fvector& Eye)
{
	xrCriticalSectionGuard Guard(lock);
	xr_vector<std::pair<float, DecalSource>> Near;

	for (u32 Slot = 0; Slot < marks.size(); ++Slot)
	{
		StaticWMVec& Items = marks[Slot]->static_items;

		for (u32 Each = 0; Each < Items.size();)
		{
			static_wallmark* Mark = Items[Each];
			const bool Seen = RImplementation.ViewBase.testSphere_dirty(Mark->bounds.P, Mark->bounds.R);

			if (Mark->flags.test(StaticWallmarkHandle::flTimeToLive))
			{
				Mark->ttl -= (Seen ? 0.1f : 1.0f) * RFrame.fTimeDelta;
			}

			if (Mark->ttl <= EPS || Mark->flags.test(StaticWallmarkHandle::flForceRemove))
			{
				static_wm_destroy(Mark);
				Items[Each] = Items.back();
				Items.pop_back();
				continue;
			}

			if (Seen)
			{
				Near.push_back({ Mark->bounds.P.distance_to_sqr(Eye), { Mark->Project, Mark->bounds, clampr(Mark->ttl / ps_r__WallmarkTTL, 0.0f, 1.0f), Slot, MarkTexture(marks[Slot]->shader) } });
			}

			++Each;
		}
	}

	for (const DecalSource& Source : Moving)
	{
		Near.push_back({ Source.Bounds.P.distance_to_sqr(Eye), Source });
	}

	Moving.clear();

	const u32 Count = std::min(Limit, u32(Near.size()));
	std::partial_sort(Near.begin(), Near.begin() + Count, Near.end(), [](const std::pair<float, DecalSource>& Left, const std::pair<float, DecalSource>& Right) { return Left.first < Right.first; });

	for (u32 Each = 0; Each < Count; ++Each)
	{
		Out[Each] = Near[Each].second;
	}

	return Count;
}

CWallmarksEngine::static_wallmark* CWallmarksEngine::AddStaticWallmark(CDB::TRI* pTri, const Fvector* pVerts, const Fvector &contact_point, ref_shader hTexture, float sz, Flags8 flags, bool UseCameraDirection)
{
	// optimization cheat: don't allow wallmarks more than 100 m from viewer/actor
	if (!flags.test(StaticWallmarkHandle::flForceSpawn) && contact_point.distance_to_sqr(RFrame.vCameraPosition) > _sqr(100.f))
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
	if (!flags.test(StaticWallmarkHandle::flForceSpawn) && contact_point.distance_to_sqr(RFrame.vCameraPosition) > _sqr(100.f))
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
	if (xf->c.distance_to_sqr(RFrame.vCameraPosition) > _sqr(50.f))				return;

	VERIFY					(obj&&xf&&(size>EPS_L));
	lock.Enter				();
	obj->AddWallmark		(xf,start,dir,sh,size);
	lock.Leave				();
}

// A mark of a skinned model in view joins the frame's decals: its box is taken from the bone's space into the world
void CWallmarksEngine::AddSkeletonWallmark(intrusive_ptr<CSkeletonWallmark> wm)
{
	if (!ps_r__WallmarkDyn || ::RImplementation.phase != CRender::PHASE_NORMAL || ::RImplementation.GraphMain.val_bHUD || ::RImplementation.Scene.Objects().get_HUD())
	{
		return;
	}

	Fmatrix BoneWorld;
	BoneWorld.mul(*wm->XFORM(), wm->Parent()->LL_GetBoneInstance(wm->Bone).mTransform);

	DecalSource Source;
	BoneWorld.transform_tiny(Source.Bounds.P, wm->Centre);
	Source.Bounds.R = wm->Radius;

	if (!RImplementation.ViewBase.testSphere_dirty(Source.Bounds.P, Source.Bounds.R))
	{
		return;
	}

	Fmatrix WorldToBone;
	WorldToBone.invert(BoneWorld);
	Source.Project.mul(wm->BoxFromBone, WorldToBone);
	Source.Opacity = clampr(1.0f - (RFrame.fTimeGlobal - wm->TimeStart()) / ps_r__WallmarkTTL, 0.0f, 1.0f);
	Source.Texture = MarkTexture(wm->Shader());

	xrCriticalSectionGuard Guard(lock);
	Source.Slot = SlotIndex(wm->Shader());
	Moving.push_back(Source);
}

void CWallmarksEngine::Render(GfxCmdList& Cmd)
{
//	if (marks.empty())			return;
	// Projection and xform
	float	_43					= RFrame.mProject._43;
	RFrame.mProject._43			-= ps_r__WallmarkSHIFT; 
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(Cmd.GetView().View, RFrame.mProject);

	Fmatrix	mSavedView			= RFrame.mView;
	Fvector	mViewPos			;
			mViewPos.mad		(RFrame.vCameraPosition, RFrame.vCameraDirection,ps_r__WallmarkSHIFT_V);
	RFrame.mView.build_camera_dir	(mViewPos,RFrame.vCameraDirection,RFrame.vCameraTop);
	Cmd.SetView(RFrame.mView, Cmd.GetView().Project);

	Device.Statistic->RenderDUMP_WM.Begin	();
	Device.Statistic->RenderDUMP_WMS_Count	= 0;
	Device.Statistic->RenderDUMP_WMD_Count	= 0;
	Device.Statistic->RenderDUMP_WMT_Count	= 0;

	Device.Statistic->RenderDUMP_WM.End		();

	// Projection
	RFrame.mView				= mSavedView;
	RFrame.mProject._43			= _43;
	Cmd.SetView(RFrame.mView, RFrame.mProject);
}
