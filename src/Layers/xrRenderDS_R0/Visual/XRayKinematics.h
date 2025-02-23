#pragma once
class XRayKinematics : public XRayFHierrarhyVisual, public IKinematics
{
public:
#ifdef DEBUG
	BOOL dbg_single_use_marker;
#endif
	void CLBone(const CBoneData* bd, CBoneInstance& bi, const Fmatrix* parent, u8 mask_channel = (1 << 0));
	virtual CBoneData* CreateBoneData(u16 ID) { return xr_new<CBoneData>(ID); }
	virtual void BuildBoneMatrix(const CBoneData* bd, CBoneInstance& bi, const Fmatrix* parent, u8 mask_channel = (1 << 0));
	void BoneChain_Calculate(const CBoneData* bd, CBoneInstance& bi, u8 channel_mask, bool ignore_callbacks);
	XRaySkeletonX* LL_GetChild(u32 idx);
	virtual void				OnCalculateBones() {}
	void LL_Validate();
	CBoneData* LL_GetBoneData(u16 bone_id)
	{

		VERIFY(bone_id < LL_BoneCount());
		VERIFY(bones);
		u32 sz = sizeof(vecBones);
		u32 sz1 = sizeof(((*bones)[bone_id])->children);
		Msg("sz: %d", sz);
		Msg("sz1: %d", sz1);
		CBoneData* bd = ((*bones)[bone_id]);
		return bd;
	}

	BonesVisible					bonesvisible;
	IC void						Visibility_Invalidate() { Update_Visibility = TRUE; };
	void						Visibility_Update();
	virtual void				IBoneInstances_Create();
	virtual void				IBoneInstances_Destroy();
public:
	virtual shared_str				getDebugName();
	virtual ~XRayKinematics();
	XRayKinematics();
	virtual void Bone_Calculate(CBoneData* bd, Fmatrix* parent);

	virtual void Bone_GetAnimPos(Fmatrix& pos, u16 id, u8 channel_mask, bool ignore_callbacks);

	virtual bool PickBone(const Fmatrix& parent_xform, pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id);
	virtual void EnumBoneVertices(SEnumVerticesCallback& C, u16 bone_id);

	// Low level interface
	virtual u16 LL_BoneID(LPCSTR B);
	virtual u16 LL_BoneID(const shared_str& B);
	virtual LPCSTR LL_BoneName_dbg(u16 ID);

	virtual CInifile* LL_UserData() { return pUserData; }
	virtual accel* LL_Bones() { return bone_map_N; }

	virtual CBoneInstance& LL_GetBoneInstance(u16 bone_id)
	{
		VERIFY(bone_id < LL_BoneCount());
		VERIFY(bone_instances);
		return bone_instances[bone_id];
	}

	virtual CBoneData& LL_GetData(u16 bone_id)
	{
		VERIFY(bone_id < LL_BoneCount());
		VERIFY(bones);
		CBoneData& bd = *((*bones)[bone_id]);
		return bd;
	}

	virtual const IBoneData& GetBoneData(u16 bone_id) const
	{
		VERIFY(bone_id < LL_BoneCount());
		VERIFY(bones);
		CBoneData& bd = *((*bones)[bone_id]);
		return bd;
	}

	virtual u16 LL_BoneCount() const { return u16(bones->size()); }

private:
	static IC u32 btwCount1(u32 v)
	{
		const u32 g31 = 0x49249249ul; // = 0100_1001_0010_0100_1001_0010_0100_1001
		const u32 g32 = 0x381c0e07ul; // = 0011_1000_0001_1100_0000_1110_0000_0111
		v = (v & g31) + ((v >> 1)& g31) + ((v >> 2)& g31);
		v = ((v + (v >> 3))& g32) + ((v >> 6)& g32);
		return (v + (v >> 9) + (v >> 18) + (v >> 27)) & 0x3f;
	}

	static IC u64 btwCount1(u64 v)
	{
		return btwCount1(u32(v & u32(-1))) + btwCount1(u32(v >> u64(32)));
	}

public:
	u16 LL_VisibleBoneCount()
	{
		u32 Count = (LL_BoneCount() / 64) + 1;
		u64 CountBone = 0;
		for (u32 i = 0; i < Count - 1; i++)
		{
			CountBone += btwCount1(bonesvisible.visimask[i].flags);
		}
		{
			u64 flags = bonesvisible.visimask[Count - 1].flags;
			flags &= (u64(1) << (LL_BoneCount() % 64)) - 1;
			CountBone += btwCount1(flags);
		}
		return (u16)CountBone;
	}
	const	CBoneInstance& LL_GetBoneInstance(u16 bone_id) const { VERIFY(bone_id < LL_BoneCount()); VERIFY(bone_instances); return bone_instances[bone_id]; }

	virtual  Fmatrix& LL_GetTransform(u16 bone_id) { return LL_GetBoneInstance(bone_id).mTransform; }
	virtual const Fmatrix& LL_GetTransform(u16 bone_id) const { return LL_GetBoneInstance(bone_id).mTransform; }
	virtual   Fmatrix& LL_GetTransform_R(u16 bone_id) { return LL_GetBoneInstance(bone_id).mRenderTransform; } // rendering only

	virtual Fobb& LL_GetBox(u16 bone_id)
	{
		VERIFY(bone_id < LL_BoneCount());
		return (*bones)[bone_id]->obb;
	}
	virtual const Fbox& GetBox() const { return Vis.box; }

	virtual void LL_GetBindTransform(xr_vector<Fmatrix>& matrices);
	virtual int  LL_GetBoneGroups(xr_vector<xr_vector<u16>>& groups);

	virtual u16  LL_GetBoneRoot() { return iRoot; }
	virtual void LL_SetBoneRoot(u16 bone_id)
	{
		VERIFY(bone_id < LL_BoneCount());
		iRoot = bone_id;
	}

	BOOL					_BCL	LL_GetBoneVisible(u16 bone_id) { VERIFY(bone_id < LL_BoneCount()); return bonesvisible.is(bone_id); }
	virtual void LL_SetBoneVisible(u16 bone_id, BOOL val, BOOL bRecursive);
	BonesVisible						_BCL	LL_GetBonesVisible() { return bonesvisible; }
	void							LL_SetBonesVisible(BonesVisible mask);

	virtual void				Release();
	// Main functionality
	virtual void CalculateBones(BOOL bForceExact = FALSE); // Recalculate skeleton
	virtual void CalculateBones_Invalidate();
	virtual void Callback(UpdateCallback C, void* Param)
	{
		Update_Callback = C;
		Update_Callback_Param = Param;
	}
	virtual void SetUpdateCallback(UpdateCallback pCallback) { Update_Callback = pCallback; }
	virtual void SetUpdateCallbackParam(void* pCallbackParam) { Update_Callback_Param = pCallbackParam; }

	virtual UpdateCallback GetUpdateCallback() { return Update_Callback; }
	virtual void* GetUpdateCallbackParam() { return Update_Callback_Param; }
	virtual IKinematicsAnimated* dcast_PKinematicsAnimated() { return 0; }
	virtual IRenderVisual* dcast_RenderVisual() { return this; }
	virtual IKinematics* dcast_PKinematics() { return this; }
#ifdef DEBUG
	virtual void						DebugRender(Fmatrix& XFORM);
#endif
public:
	UpdateCallback Update_Callback;
	void* Update_Callback_Param;
	virtual void Load(const char* N, IReader* data, u32 dwFlags);
	virtual void Copy(XRayRenderVisual* from);
	virtual void Spawn();
	virtual void				Depart();
public:
	XRayRenderVisual *m_lod;

	CBoneInstance* bone_instances; // bone instances
protected:
	//SkeletonWMVec				wallmarks;
	u32 wm_frame;

	BearVector<XRayRenderVisual *> children_invisible;

	// Globals
	CInifile *pUserData;
	vecBones *bones;			   // all bones	(shared)
	u16 iRoot;					   // Root bone index

	// Fast search
	accel *bone_map_N; // bones  associations	(shared)	- sorted by name
	accel *bone_map_P; // bones  associations	(shared)	- sorted by name-pointer

	BOOL Update_Visibility;
	u32 UCalc_Time;
	s32 UCalc_Visibox;


private:
	bool m_is_original_lod;
};