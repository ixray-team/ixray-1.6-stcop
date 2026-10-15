//---------------------------------------------------------------------------
#ifndef SkeletonAnimatedH
#define SkeletonAnimatedH
#include "RenderAPI.h"

#include "SkeletonCustom.h"
#include "Animation.h"
#include		"../../xrEngine/SkeletonMotions.h"

#include		"../../Include/xrRender/KinematicsAnimated.h"

//*** Bone Instance *******************************************************************************
#pragma pack(push,8)
class 		CBlendInstance	// Bone Instance Blend List (per-bone data)
{
public:
	typedef FixedVector<CBlend*,MAX_BLENDED>	BlendSVec;
	typedef BlendSVec::iterator				BlendSVecIt;
	typedef BlendSVec::const_iterator		BlendSVecCIt;
	xrSRWLock								blend_lock;
private:
	BlendSVec			Blend;
public:

	// methods
IC	BlendSVec			&blend_vector	()	{ return Blend;}
	void				construct		();
	void				blend_add		(CBlend* H);
	void				blend_remove	(CBlend* H);

	u32					mem_usage		()
	{
		xrSRWLockGuard guard(&blend_lock, true);
		u32 sz			= sizeof(*this);
		for (CBlend* B : Blend)
			sz			+= B->mem_usage();
		return			sz;
	}
};
#pragma pack(pop)



//typedef void	( * MotionMarkCallback)		(CBlend*		P);

//*** The visual itself ***************************************************************************
class  RENDER_API	CKinematicsAnimated	: public CKinematics, public IKinematicsAnimated
{
	typedef CKinematics							inherited;
	friend class								CBoneData;
	friend class								CMotionDef;
	friend class								CSkeletonX;
private:
	// Motion control
	void						Bone_Motion_Start		(CBoneData* bd, CBlend* handle);	// with recursion
	void						Bone_Motion_Stop		(CBoneData* bd, CBlend* handle);	// with recursion

	void						Bone_Motion_Start_IM	(CBoneData* bd, CBlend* handle);
	void						Bone_Motion_Stop_IM		(CBoneData* bd, CBlend* handle);

public: 
	// Calculation
private:
	
	virtual void						LL_BuldBoneMatrixDequatize	( const CBoneData* bd, u8 channel_mask,  SKeyTable& keys ) override;
	virtual void						LL_BoneMatrixBuild			( CBoneInstance &bi, const Fmatrix *parent, const SKeyTable& keys ) override;
	void						LL_BlendKeys				( CKey& Result, const SKeyTable& keys );
	virtual void					BuildBoneMatrix				( const CBoneData* bd, CBoneInstance &bi, const Fmatrix *parent, u8 mask_channel = (1<<0) ) override;

	struct SInertialBone
	{
		Fquaternion				LastQ;
		Fquaternion				PrevQ;
		Fvector					LastT = {};
		Fvector					PrevT = {};
		Fvector					SourceVelocityR = {};
		Fvector					SourceVelocityT = {};
		Fvector					OffsetR = {};
		Fvector					VelocityR = {};
		Fvector					OffsetT = {};
		Fvector					VelocityT = {};
		bool					Valid = false;
		bool					Pending = false;
		bool					Enabled = true;
	};

	xr_vector<SInertialBone>	m_InertialBones;
	float						m_InertialTime = 0.0f;
	float						m_InertialStamp = -1.0f;
	float						m_InertialDelta = 0.0f;

	void						InertialAdvance			();
	void						InertialMarkPart		( u16 part );
	void						InertialApply			( u16 bone, CKey& key, bool update );
public:
	virtual void				OnCalculateBones		() override;
public:
	u32											Update_LastTime;

	CBlendInstance*								blend_instances;

	struct SMotionsSlot{
		shared_motions							motions;
		BoneMotionsVec							bone_motions;
	};

	using MotionsSlotVec = xr_vector<SMotionsSlot>;
	using MotionsSlotVecIt = MotionsSlotVec::iterator;

	MotionsSlotVec								m_Motions;

    CPartition*									m_Partition;
	
	IBlendDestroyCallback						*m_blend_destroy_callback;
	IUpdateTracksCallback						*m_update_tracks_callback;
	// Blending
	FixedVector<CBlend, MAX_BLENDED_POOL>			blend_pool;
	BlendSVec									blend_cycles[MAX_PARTS];
	BlendSVec									blend_fx;
	animation::channels							channels;
protected:
	// internal functions
	virtual void				IBoneInstances_Create	() override;
	virtual void				IBoneInstances_Destroy	() override;

	void						IBlend_Startup			();
	void						ChannelFactorsStartup	();
	CBlend*						IBlend_Create			();
private:
	void						IBlendSetup				(CBlend& B,u16 part,u8 channel, MotionID motion_ID, bool  bMixing, float blendAccrue, float blendFalloff, float Speed, bool noloop, PlayCallback Callback, LPVOID CallbackParam);
	void						IFXBlendSetup			(CBlend &B, MotionID motion_ID, float blendAccrue, float blendFalloff,float Power ,float Speed,u16 bone);
	virtual void				LoadOmf(const char* path, const char* name);
	virtual	void				ProcessOmfFiles(const char* omfPath, const char* nameOgf);
	void						CollectOmfFiles(const char* omfPath, xr_vector<xr_string>& Files);
	void						LoadOmfFiles(const xr_vector<xr_string>& Files, const char* nameOgf);
	bool						LoadOmfSlot(const char* path, const char* name, SMotionsSlot& Slot);
//.	bool						LoadMotions				(const char* N, IReader *data);
public:
#ifdef DEBUG
	virtual std::pair<const char*,const char*>	LL_MotionDefName_dbg	(MotionID	ID) override;
	virtual void						LL_DumpBlends_dbg		( ) override;
#endif
	virtual u32							LL_PartBlendsCount			( u32 bone_part_id ) override;
	virtual CBlend						*LL_PartBlend				( u32 bone_part_id, u32 n ) override;
	virtual void						LL_IterateBlends			( IterateBlendsCallback &callback ) override;

	virtual void						SetUpdateTracksCalback		( IUpdateTracksCallback	*callback ) override;
	virtual IUpdateTracksCallback		*GetUpdateTracksCalback		( ) override { return m_update_tracks_callback; }
//	const char*						LL_MotionDefName_dbg	(LPVOID		ptr);

    u32							LL_CycleCount	(){u32 cnt=0; for (u32 k=0; k<m_Motions.size(); k++) cnt+=m_Motions[k].motions.cycle()->size(); return cnt;}
    u32							LL_FXCount		(){u32 cnt=0; for (u32 k=0; k<m_Motions.size(); k++) cnt+=m_Motions[k].motions.fx()->size(); return cnt;}
	accel_map*					LL_Motions		(u32 slot){return m_Motions[slot].motions.motion_map();}
	MotionID					ID_Motion		(const char*  N, u16 slot);
	virtual u16							LL_MotionsSlotCount() override {return (u16)m_Motions.size();}
	virtual const shared_motions&		LL_MotionsSlot	(u16 idx) override {return m_Motions[idx].motions;}

	IC virtual CMotionDef*				LL_GetMotionDef	(MotionID id) override {return m_Motions[id.slot].motions.motion_def(id.idx);}
	IC virtual CMotion*					LL_GetRootMotion(MotionID id) override {return &m_Motions[id.slot].bone_motions[iRoot]->at(id.idx);}
	IC virtual CMotion*					LL_GetMotion	(MotionID id, u16 bone_id) override {return &m_Motions[id.slot].bone_motions[bone_id]->at(id.idx);}
	IC virtual IBlendDestroyCallback	*GetBlendDestroyCallback	( ) override;
	virtual void					SetBlendDestroyCallback		( IBlendDestroyCallback	*cb ) override;
	// Low level interface
	virtual MotionID					LL_MotionID		(const char* B) override;
	virtual u16							LL_PartID		(const char* B) override;

	virtual CBlend*						LL_PlayFX		(u16 bone,		MotionID motion, float blendAccrue,	float blendFalloff, float Speed, float Power) override;
	virtual CBlend*						LL_PlayCycle	(u16 partition, MotionID motion, bool  bMixing,		float blendAccrue,	float blendFalloff, float Speed, bool noloop, PlayCallback Callback, LPVOID CallbackParam, u8 channel = 0) override;
	virtual CBlend*						LL_PlayCycle	(u16 partition, MotionID motion, bool bMixIn, PlayCallback Callback, LPVOID CallbackParam, u8 channel = 0) override;
	virtual void						LL_FadeCycle	(u16 partition, float	falloff, u8 mask_channel = (1<<0)) override;
	virtual void						LL_CloseCycle	(u16 partition, u8 mask_channel = (1<<0)) override;
	virtual void						LL_SetChannelFactor (u16 channel,float factor) override;
	CBlendInstance&				LL_GetBlendInstance	(u16 bone_id)	{	VERIFY(bone_id<LL_BoneCount()); return blend_instances[bone_id];	}
	                                                                
	// Main functionality
	virtual void						UpdateTracks	() override;								// Update motions
	virtual void						LL_UpdateTracks	( float dt, bool b_force, bool leave_blends ) override;						// Update motions
	void						LL_UpdateFxTracks( float dt );
	void						DestroyCycle	(CBlend &B);

	// cycles
	virtual MotionID					ID_Cycle		(const char*  N) override;
	virtual MotionID					ID_Cycle_Safe	(const char*  N) override;
	virtual MotionID					ID_Cycle		(shared_str  N) override;
	virtual MotionID					ID_Cycle_Safe	(shared_str  N) override;
	virtual CBlend*						PlayCycle		(const char*  N,  bool bMixIn=true, PlayCallback Callback=nullptr, LPVOID CallbackParam=nullptr, u8 channel = 0) override;
	virtual CBlend*						PlayCycle		(MotionID M, bool bMixIn=true, PlayCallback Callback=nullptr, LPVOID CallbackParam=nullptr, u8 channel = 0) override;
	virtual CBlend*						PlayCycle		(u16 partition, MotionID M, bool bMixIn=true, PlayCallback Callback=nullptr, LPVOID CallbackParam=nullptr, u8 channel = 0) override;
	// fx'es
	virtual MotionID					ID_FX			(const char*  N) override;
	virtual MotionID					ID_FX_Safe		(const char*  N) override;
	virtual CBlend*						PlayFX			(const char*  N, float power_scale) override;
	virtual CBlend*						PlayFX			(MotionID M, float power_scale) override;

	CBlend*						PlayFX_Safe		(const char*	 N, float power_scale) override;

	virtual const CPartition&			partitions		() const override {return *m_Partition;};

	// General "Visual" stuff
	virtual void				Copy			(dxRender_Visual *pFrom) override;
	virtual void				Load			(const char* N, IReader *data, u32 dwFlags) override;
	virtual void				append_motion_from_path(const char* nameOgf, const char* pathOmf) override;
	virtual void				Release			() override;
	virtual void				Spawn			() override;
	virtual IKinematicsAnimated*dcast_PKinematicsAnimated() override { return this;	}
	virtual IRenderVisual*	_BCL	dcast_RenderVisual		() override { return this; }
	virtual IKinematics*	_BCL 	dcast_PKinematics		() override { return this; }

	virtual ~CKinematicsAnimated	() override;
								CKinematicsAnimated		();

	virtual u32					mem_usage		(bool bInstance) override
	{
		u32 sz					= CKinematics::mem_usage(bInstance)+sizeof(*this)+(bInstance&&blend_instances?blend_instances->mem_usage():0);
		return sz;
	}

	IC	const BlendSVec			&blend_cycle	(const u32 &bone_part_id) const
	{
		VERIFY					(bone_part_id < MAX_PARTS);
		return					(blend_cycles[bone_part_id]);
	}

	virtual float				get_animation_length (MotionID motion_ID) override;
	void						LL_SetInertialization	(float time) override;
	void						LL_SetInertializationBone(u16 bone, bool enabled) override;
	bool						LL_GetMotionBoneTransform(MotionID motion, float time, u16 bone, Fmatrix& result) override;
};
//IC CKinematicsAnimated* PKinematicsAnimated(IRender_Visual* V) { return V?V->dcast_PKinematicsAnimated():0; }
IC CKinematicsAnimated* PKinematicsAnimated(IRenderVisual* V) { return V?(CKinematicsAnimated*)V->dcast_PKinematicsAnimated():nullptr; }
//---------------------------------------------------------------------------
#endif
