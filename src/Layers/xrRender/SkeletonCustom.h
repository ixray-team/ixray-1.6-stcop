//---------------------------------------------------------------------------
#ifndef SkeletonCustomH
#define SkeletonCustomH

#include "FHierrarhyVisual.h"
#include "../../xrEngine/bone.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../xrEngine/VisMask.h"

// refs
class	 CKinematics;
class CSkeletonX;
struct	SEnumVerticesCallback;

#pragma warning(push)
#pragma warning(disable:4275)
class  CSkeletonWallmark : public intrusive_base // 4+4+4+12+4+16+16 = 60 + 4 = 64
{
#pragma warning(pop)
	CKinematics*		m_Parent;		// 4
	const Fmatrix*		m_XForm;		// 4
	ref_shader			m_Shader;		// 4
	Fvector3			m_ContactPoint;	// 12		model space
	float				m_fTimeStart;	// 4
public:
	Fsphere				m_LocalBounds;	// 16		model space
	struct WMFace
	{
		Fvector vert[3];
		Fvector2 uv[3];
		u16 bone_id[3][4]
		{
			{BI_NONE, BI_NONE, BI_NONE, BI_NONE},
			{BI_NONE, BI_NONE, BI_NONE, BI_NONE},
			{BI_NONE, BI_NONE, BI_NONE, BI_NONE}
		};
		float weight[3][3]
		{
			{0.f, 0.f, 0.f},
			{0.f, 0.f, 0.f},
			{0.f, 0.f, 0.f}
		};
	};

	xr_vector<WMFace>	m_Faces;		// 16 
public:
	Fsphere				m_Bounds;		// 16		world space
public:									
						CSkeletonWallmark	(CKinematics* p,const Fmatrix* m, ref_shader s, const Fvector& cp, float ts):
						m_Parent(p),m_XForm(m),m_Shader(s),m_fTimeStart(ts),m_ContactPoint(cp)
						{}
						~CSkeletonWallmark	(){}

	ICF CKinematics*	Parent				(){return m_Parent;}
	ICF u32				VCount				(){return (u32)m_Faces.size()*3;}
	ICF bool			Similar				(ref_shader& sh, const Fvector& cp, float eps){return (m_Shader==sh)&&m_ContactPoint.similar(cp,eps);}
	ICF float			TimeStart			(){return m_fTimeStart;}
	ICF const Fmatrix*	XFORM				(){return m_XForm;}
	ICF const Fvector3&	ContactPoint		(){return m_ContactPoint;}
	ICF ref_shader		Shader				(){return m_Shader;}
};

using SkeletonWMVec = xr_vector<intrusive_ptr<CSkeletonWallmark>>;
using SkeletonWMVecIt = SkeletonWMVec::iterator;

class 	CKinematics: public FHierrarhyVisual, public IKinematics
{
private:
	typedef FHierrarhyVisual	inherited;
	friend class				CBoneData;
	friend class				CSkeletonX;


	xr_vector<shared_str>		L_parents;
	xr_vector<xr_vector<u16> > 	GroupIDs;

public: 
#ifdef DEBUG
	bool						dbg_single_use_marker;
#endif
			void				Bone_Calculate		(CBoneData* bd, Fmatrix* parent);
			void				CLBone				(const CBoneData* bd, CBoneInstance &bi, const Fmatrix *parent, u8 mask_channel = (1<<0));

			void				BoneChain_Calculate	(const CBoneData* bd, CBoneInstance &bi,u8 channel_mask, bool ignore_callbacks);
			void				Bone_GetAnimPos		(Fmatrix& pos,u16 id, u8 channel_mask, bool ignore_callbacks);


	virtual	void				BuildBoneMatrix		( const CBoneData* bd, CBoneInstance &bi, const Fmatrix *parent, u8 mask_channel = (1<<0) );
	virtual void				OnCalculateBones	(){}
	
public:
	dxRender_Visual*			m_lod;

public:
	Fmatrix						mOldWorldMartrix;
	Fmatrix						mOldWorldMartrixTmp;

	u32							dwFirstRenderFrame;

	void						StoreVisualMatrix(Fmatrix& world_matrix);
protected:
	SkeletonWMVec				wallmarks;
	u32							wm_frame;

	xr_vector<dxRender_Visual*>	children_invisible	;

	// Globals
    CInifile*					pUserData;
	CBoneInstance*				bone_instances;	// bone instances
	vecBones*					bones;			// all bones	(shared)
	u16							iRoot;			// Root bone index

	// Fast search
	accel*						bone_map_N;		// bones  associations	(shared)	- sorted by name
	accel*						bone_map_P;		// bones  associations	(shared)	- sorted by name-pointer

	bool						Update_Visibility		;
	u32							UCalc_Time				;
	s32							UCalc_Visibox			;
	u32							Visibox_frame = 0;
	xrCriticalSection			UCalc_Mutex;
	xrCriticalSection			UCalc_Mutex2;

	VisMask						visimask;
    
	CSkeletonX*					LL_GetChild				(u32 idx);

	// internal functions
	virtual CBoneData*			CreateBoneData			(u16 ID){return new CBoneData(ID);}
	virtual void				IBoneInstances_Create	();
	virtual void				IBoneInstances_Destroy	();
	void						Visibility_Invalidate	()	{ Update_Visibility=true; };
	void						Visibility_Update		()	;

    void						LL_Validate				();
public:
	UpdateCallback				Update_Callback;
	void*						Update_Callback_Param;
public:
	// wallmarks
	void						AddWallmark			(const Fmatrix* parent, const Fvector3& start, const Fvector3& dir, ref_shader shader, float size);
	void						CalculateWallmarks	();
	void						RenderWallmark		(intrusive_ptr<CSkeletonWallmark> wm, FVF::LIT* &verts);
	void						ClearWallmarks		();
public:
				
				bool			PickBone			(const Fmatrix &parent_xform, IKinematics::pick_result &r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id);
	virtual		void			EnumBoneVertices	(SEnumVerticesCallback &C, u16 bone_id = u16(-1));
	virtual		void			EnumBoneVertices	(xr_vector<Fvector>& m_vec, u16 bone_id = u16(-1));
	virtual		void			EnumBoneVertices	(buffer_vector<Fvector>& m_vec, u16 bone_id = u16(-1));
	virtual		u32				GetFacesCount		(u16 bone_id = u16(-1));
public:
								CKinematics			();
	virtual						~CKinematics		();

	// Low level interface
				u16				_BCL	LL_BoneID			(const char*  B);
				u16				_BCL	LL_BoneID			(const shared_str& B);
				const char*			_BCL	LL_BoneName_dbg		(u16 ID);

				CInifile*		_BCL	LL_UserData			()						{return pUserData;}
				accel*					LL_Bones			()						{return bone_map_N;}
	ICF			CBoneInstance&	_BCL	LL_GetBoneInstance	(u16 bone_id)			{
		VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone: %s, bone_id: %d", dbg_name.c_str(), LL_BoneName_dbg(bone_id), bone_id));
		VERIFY(bone_instances); 
		return bone_instances[bone_id];
	}
	ICF const CBoneInstance& _BCL LL_GetBoneInstance(u16 bone_id) const
	{
		VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));
		VERIFY(bone_instances); 
		return bone_instances[bone_id];
	}
	CBoneData&					_BCL	LL_GetData			(u16 bone_id)
    {
		VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));
        VERIFY(bones);
        CBoneData& bd =  *((*bones)[bone_id]) ;
        return bd;
    }

	virtual	const IBoneData&_BCL	GetBoneData(u16 bone_id) const
	{
		VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));
        VERIFY(bones);
        CBoneData& bd =  *((*bones)[bone_id]) ;
        return bd;
	}
	CBoneData*	_BCL	LL_GetBoneData		(u16 bone_id)
	{
		
		VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));
        VERIFY(bones);
		u32	sz = sizeof(vecBones);
		u32	sz1=  sizeof(((*bones)[bone_id])->children);
		Msg("sz: %d",sz);
		Msg("sz1: %d",sz1);
        CBoneData* bd =  ((*bones)[bone_id]) ;
        return bd;
	}
	u16						_BCL	LL_BoneCount		()	const			{	return u16(bones->size());										}
	u16								LL_VisibleBoneCount	()					{	return visimask.count();										}
	ICF Fmatrix&			_BCL	LL_GetTransform		(u16 bone_id)		{	return LL_GetBoneInstance(bone_id).mTransform;					}
	ICF const Fmatrix&		_BCL	LL_GetTransform		(u16 bone_id) const	{	return LL_GetBoneInstance(bone_id).mTransform;					}

	ICF void				_BCL	LL_GetBoneLocalPosition(u16 bone_id, Fvector& result)
	{
		{
			xrCriticalSectionGuard g(UCalc_Mutex);
			result = LL_GetBoneInstance(bone_id).mTransform.c;
		}
	}

	ICF void				_BCL	LL_GetBoneLocalTransform(u16 bone_id, Fmatrix& result)
	{
		{
			xrCriticalSectionGuard g(UCalc_Mutex);
			result = LL_GetBoneInstance(bone_id).mTransform;
		}
	}

	ICF void				_BCL	LL_GetBoneWorldPosition	(u16 bone_id, const Fmatrix& xform, Fvector& result)
	{
		LL_GetBoneLocalPosition(bone_id, result);
		xform.transform_tiny(result);
	}

	ICF void				_BCL	LL_GetBoneWorldTransform(u16 bone_id, const Fmatrix& xform, Fmatrix& result)
	{
		LL_GetBoneLocalTransform(bone_id, result);
		result.mulA_43(xform);
	}

	ICF void CalculateBBox(bool bforce = true)
	{
		if (!bforce && Device.dwFrame == Visibox_frame)
			return;

		Visibox_frame = Device.dwFrame;

		// the update itself
		Fbox	Box; Box.invalidate();
		for (u32 b = 0; b < bones->size(); b++)
		{
			if (!LL_GetBoneVisible(u16(b)))		continue;
			Fobb& obb = (*bones)[b]->obb;
			Fmatrix& Mbone = bone_instances[b].mTransform;
			Fmatrix		Mbox;	obb.xform_get(Mbox);
			Fmatrix		X;		X.mul_43(Mbone, Mbox);
			Fvector& S = obb.m_halfsize;

			Fvector			P, A;
			A.set(-S.x, -S.y, -S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(-S.x, -S.y, S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(S.x, -S.y, S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(S.x, -S.y, -S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(-S.x, S.y, -S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(-S.x, S.y, S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(S.x, S.y, S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(S.x, S.y, -S.z); X.transform_tiny(P, A); Box.modify(P);
		}
		if (bones->size())
		{
			// previous frame we have updated box - update sphere
			vis.box.min = (Box.min);
			vis.box.max = (Box.max);
			vis.box.getsphere(vis.sphere.P, vis.sphere.R);
		}
	}

	ICF Fmatrix&					LL_GetTransform_R	(u16 bone_id)		{	return LL_GetBoneInstance(bone_id).mRenderTransform;			}	// rendering only
	ICF Fmatrix&					LL_GetTransform_R_old(u16 bone_id)		{	return LL_GetBoneInstance(bone_id).mRenderTransform_old;		}	// rendering only old
	Fobb&							LL_GetBox			(u16 bone_id)		{	VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));	return (*bones)[bone_id]->obb;	}
	const Fbox&				_BCL	GetBox				()const				{	return vis.box ;}
	void							LL_GetBindTransform (xr_vector<Fmatrix>& matrices);
	void							LL_GetBindTransform	(buffer_vector<Fmatrix>& matrices);
    int 							LL_GetBoneGroups 	(xr_vector<xr_vector<u16> >& groups);

	u16						_BCL	LL_GetBoneRoot		()					{	return iRoot;													}
	void							LL_SetBoneRoot		(u16 bone_id)		{ VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));	iRoot=bone_id;					}

    bool					_BCL	LL_GetBoneVisible	(u16 bone_id)		{ 
		VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone: %s, bone_id: %d", dbg_name.c_str(), LL_BoneName_dbg(bone_id), bone_id));
		return visimask.is(bone_id);	}
	void							LL_SetBoneVisible	(u16 bone_id, bool val, bool bRecursive);
	VisMask					_BCL	LL_GetBonesVisible	()					{	return visimask;	}
	void							LL_SetBonesVisible	(VisMask mask);
	void							LL_SetBonesVisibleAll() { visimask.set_all(); };

	virtual void					BonesHistory_Invalidate() override;

	// Main functionality
	virtual void					CalculateBones				(bool bForceExact	=	false);		// Recalculate skeleton
	void							CalculateBones_Invalidate	();
	void							Callback					(UpdateCallback C, void* Param)		{	Update_Callback	= C; Update_Callback_Param	= Param;	}

	//	Callback: data manipulation
	virtual void					SetUpdateCallback(UpdateCallback pCallback) {Update_Callback = pCallback;}
	virtual void					SetUpdateCallbackParam(void* pCallbackParam) {Update_Callback_Param = pCallbackParam;}

	virtual UpdateCallback			GetUpdateCallback() { return Update_Callback;}
	virtual void*					GetUpdateCallbackParam() { return Update_Callback_Param;}

	// debug
#ifdef DEBUG_DRAW
	void							DebugRender			(Fmatrix& XFORM);
#endif

protected:
	virtual shared_str				getDebugName()	{ return dbg_name; }
public:


	// General "Visual" stuff
    virtual void					Copy				(dxRender_Visual *pFrom);
	virtual void					Load				(const char* N, IReader *data, u32 dwFlags);
	virtual void 					Spawn				();
	virtual void					Depart				();
    virtual void 					Release				();

	virtual	IKinematicsAnimated*dcast_PKinematicsAnimated() { return nullptr;	}
	virtual IRenderVisual*	_BCL dcast_RenderVisual() { return this; }
	virtual IKinematics*	_BCL dcast_PKinematics()  { return this; }
//	virtual	CKinematics*		dcast_PKinematics	()				{ return this;	}

	virtual u32					mem_usage			(bool bInstance)
	{
		u32 sz					= sizeof(*this);
		sz						+= bone_instances?bone_instances->mem_usage():0;
		if (!bInstance){
//			sz					+= pUserData?pUserData->mem_usage():0;
			for (vecBonesIt b_it=bones->begin(); b_it!=bones->end(); b_it++)
				sz				+= sizeof(vecBones::value_type)+(*b_it)->mem_usage();
		}
		return sz;
	}
private:
	bool						m_is_original_lod;

};
IC CKinematics* PCKinematics		(dxRender_Visual* V)		{ return V?(CKinematics*)V->dcast_PKinematics():nullptr; }
//---------------------------------------------------------------------------
#endif
