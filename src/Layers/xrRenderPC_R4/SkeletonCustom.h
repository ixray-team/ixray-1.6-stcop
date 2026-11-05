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
						virtual ~CSkeletonWallmark	() override {}

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
	friend class				SkeletonBatch;


	xr_vector<shared_str>		L_parents;
	xr_vector<xr_vector<u16> > 	GroupIDs;

	void						CalculateBoneHierarchy	(const CBoneData* bd, const Fmatrix* parent);
	void						AdvanceTracks			();

public:
#ifdef DEBUG
	bool						dbg_single_use_marker;
#endif
			virtual void				Bone_Calculate		(CBoneData* bd, Fmatrix* parent) override;
			void				CLBone				(const CBoneData* bd, CBoneInstance &bi, const Fmatrix *parent, u8 mask_channel = (1<<0));

			void				BoneChain_Calculate	(const CBoneData* bd, CBoneInstance &bi,u8 channel_mask, bool ignore_callbacks);
			virtual void				Bone_GetAnimPos		(Fmatrix& pos,u16 id, u8 channel_mask, bool ignore_callbacks) override;


	virtual	void				BuildBoneMatrix		( const CBoneData* bd, CBoneInstance &bi, const Fmatrix *parent, u8 mask_channel = (1<<0) );
	virtual void				OnCalculateBones	(){}
	
public:
	dxRender_Visual*			m_lod;
	CKinematics*				m_reference;

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
	u32							TracksTime = 0;			// RDEVICE.dwTimeGlobal of the last OnCalculateBones
	u32							BatchMarker = 0;		// SkeletonBatch that already holds this skeleton
	// Guards bone calculation and bone reads that need a whole matrix. See SkeletonRigid.cpp for the wait rules.
	xrCriticalSection			UCalc_Mutex;

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
				
	virtual bool			PickBone			(const Fmatrix &parent_xform, IKinematics::pick_result &r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id) override;
	virtual void			EnumBoneVertices	(SEnumVerticesCallback &C, u16 bone_id = u16(-1)) override;
	virtual void			EnumBoneVertices	(xr_vector<Fvector>& m_vec, u16 bone_id = u16(-1)) override;
	virtual void			EnumBoneVertices	(buffer_vector<Fvector>& m_vec, u16 bone_id = u16(-1)) override;
	virtual u32				GetFacesCount		(u16 bone_id = u16(-1)) override;
public:
								CKinematics			();
	virtual ~CKinematics		() override;

	// Low level interface
	virtual u16				_BCL	LL_BoneID			(const char*  B) override;
	virtual u16				_BCL	LL_BoneID			(const shared_str& B) override;
	virtual const char*			_BCL	LL_BoneName_dbg		(u16 ID) override;

	virtual CInifile*		_BCL	LL_UserData			() override {return pUserData;}
	virtual accel*					LL_Bones			() override {return bone_map_N;}
	ICF	virtual CBoneInstance&	_BCL	LL_GetBoneInstance	(u16 bone_id) override
	{
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
	virtual CBoneData&					_BCL	LL_GetData			(u16 bone_id) override
	{
		VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));
        VERIFY(bones);
        CBoneData& bd =  *((*bones)[bone_id]) ;
        return bd;
    }

	virtual const IBoneData&_BCL	GetBoneData(u16 bone_id) const override
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
	virtual u16						_BCL	LL_BoneCount		()	const override {	return u16(bones->size());										}
	virtual u16								LL_VisibleBoneCount	() override {	return visimask.count();										}
	ICF virtual Fmatrix&			_BCL	LL_GetTransform		(u16 bone_id) override {	return LL_GetBoneInstance(bone_id).mTransform;					}
	ICF virtual const Fmatrix&		_BCL	LL_GetTransform		(u16 bone_id) const override {	return LL_GetBoneInstance(bone_id).mTransform;					}

	virtual void					_BCL	LL_GetBoneLocalPosition(u16 bone_id, Fvector& result) override;
	virtual void					_BCL	LL_GetBoneLocalTransform(u16 bone_id, Fmatrix& result) override;

	ICF virtual void				_BCL	LL_GetBoneWorldPosition	(u16 bone_id, const Fmatrix& xform, Fvector& result) override
	{
		LL_GetBoneLocalPosition(bone_id, result);
		xform.transform_tiny(result);
	}

	ICF virtual void				_BCL	LL_GetBoneWorldTransform(u16 bone_id, const Fmatrix& xform, Fmatrix& result) override
	{
		LL_GetBoneLocalTransform(bone_id, result);
		result.mulA_43(xform);
	}

	ICF virtual void CalculateBBox(bool bforce = true) override
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

	ICF virtual Fmatrix&					LL_GetTransform_R	(u16 bone_id) override {	return LL_GetBoneInstance(bone_id).mRenderTransform;			}	// rendering only
	ICF Fmatrix&					LL_GetTransform_R_old(u16 bone_id)		{	return LL_GetBoneInstance(bone_id).mRenderTransform_old;		}	// rendering only old
	virtual Fobb&							LL_GetBox			(u16 bone_id) override
	{	VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));	return (*bones)[bone_id]->obb;	}
	virtual const Fbox&				_BCL	GetBox				()const override {	return vis.box ;}
	virtual void							LL_GetBindTransform (xr_vector<Fmatrix>& matrices) override;
	virtual void							LL_GetBindTransform	(buffer_vector<Fmatrix>& matrices) override;
    virtual int 							LL_GetBoneGroups 	(xr_vector<xr_vector<u16> >& groups) override;

	virtual u16						_BCL	LL_GetBoneRoot		() override {	return iRoot;													}
	virtual void							LL_SetBoneRoot		(u16 bone_id) override
	{ VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone_id: %d", dbg_name.c_str(), bone_id));	iRoot=bone_id;					}

    virtual bool					_BCL	LL_GetBoneVisible	(u16 bone_id) override
    { 
		VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone: %s, bone_id: %d", dbg_name.c_str(), LL_BoneName_dbg(bone_id), bone_id));
		return visimask.is(bone_id);	}
	virtual void							LL_SetBoneVisible	(u16 bone_id, bool val, bool bRecursive) override;
	virtual VisMask					_BCL	LL_GetBonesVisible	() override {	return visimask;	}
	virtual void							LL_SetBonesVisible	(VisMask mask) override;
	virtual void							LL_SetBonesVisibleAll() override { visimask.set_all(); };

	virtual void					LL_SetSimpleBoneVisible	(u16 bone_id, bool val) override;
	virtual bool					LL_GetSimpleBoneVisible	(u16 bone_id) override;

	virtual void					BonesHistory_Invalidate() override;
	
	// Main functionality
	virtual void					CalculateBones				(bool bForceExact	=	false) override;		// Recalculate skeleton
	// Advances animation tracks for this frame without bone calculation. Track callbacks run on the calling thread.
	void							PrepareBones				();
	virtual void							CalculateBones_Invalidate	() override;
	virtual void							Callback					(UpdateCallback C, void* Param) override
	{	Update_Callback	= C; Update_Callback_Param	= Param;	}

	//	Callback: data manipulation
	virtual void					SetUpdateCallback(UpdateCallback pCallback) override {Update_Callback = pCallback;}
	virtual void					SetUpdateCallbackParam(void* pCallbackParam) override {Update_Callback_Param = pCallbackParam;}

	virtual void					LL_SetShader(LPCSTR sh_name) override;
	virtual void					LL_RestoreShader() override;
	virtual void					LL_SetShader(u16 bone_id, LPCSTR sh_name) override;
	virtual void					LL_RestoreShader(u16 bone_id) override;

	virtual void					LL_SetTexture(LPCSTR tex_name) override;
	virtual void					LL_RestoreTexture() override;
	virtual void					LL_SetTexture(u16 bone_id, LPCSTR tex_name) override;
	virtual void					LL_RestoreTexture(u16 bone_id) override;

	virtual xr_vector<IRenderVisual*>& LL_GetChilds() override;

	virtual UpdateCallback			GetUpdateCallback() override { return Update_Callback;}
	virtual void*					GetUpdateCallbackParam() override { return Update_Callback_Param;}

	// debug
#ifdef DEBUG_DRAW
	virtual void							DebugRender			(Fmatrix& XFORM) override;
#endif

protected:
	virtual shared_str				getDebugName() override { return dbg_name; }
public:


	// General "Visual" stuff
	virtual void					Copy				(dxRender_Visual *pFrom) override;
	virtual void					Load				(const char* N, IReader *data, u32 dwFlags) override;
	virtual void 					ReloadUserData		() override;
	virtual void 					Spawn				() override;
	virtual void					Depart				() override;
	virtual void 					Release				() override;

	virtual IKinematicsAnimated*dcast_PKinematicsAnimated() override { return nullptr;	}
	virtual IRenderVisual*	_BCL dcast_RenderVisual() override { return this; }
	virtual IKinematics*	_BCL dcast_PKinematics() override { return this; }
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
