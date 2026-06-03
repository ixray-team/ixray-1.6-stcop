#pragma once
#include "Animation.h"
#include "XRayKinematics.h"

//*** Bone Instance *******************************************************************************
#pragma pack(push,8)
class CBlendInstance	// Bone Instance Blend List (per-bone data)
{
public:
	typedef FixedVector<CBlend*, MAX_BLENDED>	BlendSVec;
	typedef BlendSVec::iterator				BlendSVecIt;
	typedef BlendSVec::const_iterator		BlendSVecCIt;
private:
	BlendSVec			Blend;
public:
	xrSRWLock								blend_lock;

	// methods
	IC	BlendSVec& blend_vector() { return Blend; }
	void				construct();
	void				blend_add(CBlend* H);
	void				blend_remove(CBlend* H);

	size_t					mem_usage()
	{
		size_t sz = sizeof(*this);
		for (BlendSVecIt it = Blend.begin(); it != Blend.end(); it++)
			sz += (*it)->mem_usage();
		return			sz;
	}
};
#pragma pack(pop)

class CDS0_KinematicsAnimated:
	public CDS0_Kinematics,
	public IKinematicsAnimated
{
	typedef CDS0_Kinematics						inherited;
	friend class								CBoneData;
	friend class								CMotionDef;
	friend class								CSkeletonX;
private:
	// Motion control
	void						Bone_Motion_Start(CBoneData* bd, CBlend* handle);	// with recursion
	void						Bone_Motion_Stop(CBoneData* bd, CBlend* handle);	// with recursion

	void						Bone_Motion_Start_IM(CBoneData* bd, CBlend* handle);
	void						Bone_Motion_Stop_IM(CBoneData* bd, CBlend* handle);

public:
	// Calculation
private:

	void LL_BuldBoneMatrixDequatize(const CBoneData* bd, u8 channel_mask, SKeyTable& keys) override;
	void LL_BoneMatrixBuild(CBoneInstance& bi, const Fmatrix* parent, const SKeyTable& keys) override;
	virtual	void BuildBoneMatrix(const CBoneData* bd, CBoneInstance& bi, const Fmatrix* parent, u8 mask_channel = (1 << 0)) override;
public:

	virtual void OnCalculateBones() override;
public:
#ifdef _EDITOR
public:
#else
private:
#endif
	u32											Update_LastTime;

	CBlendInstance* blend_instances;

	struct SMotionsSlot {
		shared_motions							motions;
		BoneMotionsVec							bone_motions;
	};
	
	using MotionsSlotVec = xr_vector<SMotionsSlot>;
	using MotionsSlotVecIt = MotionsSlotVec::iterator;
	
	MotionsSlotVec								m_Motions;

	CPartition* m_Partition;

	IBlendDestroyCallback* m_blend_destroy_callback;
	IUpdateTracksCallback* m_update_tracks_callback;
	// Blending
	FixedVector<CBlend, MAX_BLENDED_POOL>			blend_pool;
	BlendSVec									blend_cycles[MAX_PARTS];
	BlendSVec									blend_fx;
	animation::channels							channels;
protected:
	// internal functions
	virtual void IBoneInstances_Create() override;
	virtual void IBoneInstances_Destroy() override;

	void IBlend_Startup();
	void ChannelFactorsStartup();
	CBlend* IBlend_Create();
private:
	void IBlendSetup(CBlend& B, u16 part, u8 channel, MotionID motion_ID, bool  bMixing, float blendAccrue, float blendFalloff, float Speed, bool noloop, PlayCallback Callback, LPVOID CallbackParam);
	void IFXBlendSetup(CBlend& B, MotionID motion_ID, float blendAccrue, float blendFalloff, float Power, float Speed, u16 bone);
	
public:
#if (defined DEBUG || defined _EDITOR)
	std::pair<str_c, str_c>	LL_MotionDefName_dbg(MotionID	ID) override;
	void LL_DumpBlends_dbg() override;
#endif
	u32 LL_PartBlendsCount(u32 bone_part_id) override;
	CBlend* LL_PartBlend(u32 bone_part_id, u32 n) override;
	void LL_IterateBlends(IterateBlendsCallback& callback) override;

	void SetUpdateTracksCalback(IUpdateTracksCallback* callback) override;
	IUpdateTracksCallback* GetUpdateTracksCalback() override { return m_update_tracks_callback; }

#ifdef _EDITOR
	size_t							LL_CycleCount() { size_t cnt = 0; for (size_t k = 0; k < m_Motions.size(); k++) cnt += m_Motions[k].motions.cycle()->size(); return cnt; }
	size_t							LL_FXCount() { size_t cnt = 0; for (size_t k = 0; k < m_Motions.size(); k++) cnt += m_Motions[k].motions.fx()->size(); return cnt; }
	accel_map* LL_Motions(size_t slot) { return m_Motions[slot].motions.motion_map(); }
	MotionID					ID_Motion(str_c  N, u16 slot);
#endif
	u16 LL_MotionsSlotCount() override { return (u16)m_Motions.size(); }
	const shared_motions& LL_MotionsSlot(u16 idx) override { return m_Motions[idx].motions; }

	IC CMotionDef* LL_GetMotionDef(MotionID id) override { return m_Motions[id.slot].motions.motion_def(id.idx); }
	IC CMotion* LL_GetRootMotion(MotionID id) override { return &m_Motions[id.slot].bone_motions[iRoot]->at(id.idx); }
	IC CMotion* LL_GetMotion(MotionID id, u16 bone_id) override { return &m_Motions[id.slot].bone_motions[bone_id]->at(id.idx); }

	virtual IBlendDestroyCallback* GetBlendDestroyCallback() override;
	virtual void SetBlendDestroyCallback(IBlendDestroyCallback* cb) override;
	// Low level interface
	MotionID LL_MotionID(str_c B) override;
	u16 LL_PartID(str_c B) override;

	CBlend* LL_PlayFX(u16 bone, MotionID motion, float blendAccrue, float blendFalloff, float Speed, float Power) override;
	CBlend* LL_PlayCycle(u16 partition, MotionID motion, bool  bMixing, float blendAccrue, float blendFalloff, float Speed, bool noloop, PlayCallback Callback, LPVOID CallbackParam, u8 channel = 0) override;
	CBlend* LL_PlayCycle(u16 partition, MotionID motion, bool bMixIn, PlayCallback Callback, LPVOID CallbackParam, u8 channel = 0) override;
	void LL_FadeCycle(u16 partition, float	falloff, u8 mask_channel = (1 << 0)) override;
	void LL_CloseCycle(u16 partition, u8 mask_channel = (1 << 0)) override;
	void LL_SetChannelFactor(u16 channel, float factor) override;
	CBlendInstance& LL_GetBlendInstance(u16 bone_id) { VERIFY(bone_id < LL_BoneCount()); return blend_instances[bone_id]; }

	// Main functionality
	void UpdateTracks() override;								// Update motions
	void LoadOmf(const char* path, const char* name);
	void LL_UpdateTracks(float dt, bool b_force, bool leave_blends) override;						// Update motions
	void LL_UpdateFxTracks(float dt);
	void DestroyCycle(CBlend& B);

	// cycles
	MotionID ID_Cycle(str_c  N) override;
	MotionID ID_Cycle_Safe(str_c  N) override;
	MotionID ID_Cycle(shared_str  N) override;
	MotionID ID_Cycle_Safe(shared_str  N) override;
	CBlend* PlayCycle(str_c  N, bool bMixIn = true, PlayCallback Callback = nullptr, LPVOID CallbackParam = nullptr, u8 channel = 0) override;
	CBlend* PlayCycle(MotionID M, bool bMixIn = true, PlayCallback Callback = nullptr, LPVOID CallbackParam = nullptr, u8 channel = 0) override;
	CBlend* PlayCycle(u16 partition, MotionID M, bool bMixIn = true, PlayCallback Callback = nullptr, LPVOID CallbackParam = nullptr, u8 channel = 0) override;
	// fx'es
	MotionID ID_FX(str_c  N) override;
	MotionID ID_FX_Safe(str_c  N) override;
	CBlend* PlayFX(str_c  N, float power_scale) override;
	CBlend* PlayFX(MotionID M, float power_scale) override;

	CBlend* PlayFX_Safe(str_c N, float power_scale) override;
	const CPartition& partitions() const override { return *m_Partition; };

	// General "Visual" stuff
	virtual void Copy(CDS0_RenderVisual* pFrom) override;
	virtual void Load(const char* N, IReader* data, u32 dwFlags) override;
	virtual void Spawn() override;
	virtual	IKinematicsAnimated* dcast_PKinematicsAnimated() override { return this; }
	virtual IRenderVisual* dcast_RenderVisual() override { return this; }
	virtual IKinematics* dcast_PKinematics() override { return this; }
	virtual void LL_SetBonesVisibleAll() override { visimask.set_all(); };

	void ProcessOmfFiles(const char* pathOmf, const char* nameOgf);

	void append_motion_from_path(const char* nameOgf, const char* pathOmf) override;

	virtual ~CDS0_KinematicsAnimated() override;
	CDS0_KinematicsAnimated();

	IC	const BlendSVec& blend_cycle(const size_t& bone_part_id) const
	{
		VERIFY(bone_part_id < MAX_PARTS);
		return					(blend_cycles[bone_part_id]);
	}

	virtual float get_animation_length(MotionID motion_ID) override;
};