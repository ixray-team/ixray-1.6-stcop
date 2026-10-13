//---------------------------------------------------------------------------
#include 	"stdafx.h"


#include 	"SkeletonCustom.h"

extern int	psSkeletonUpdate;

#ifdef DEBUG
void check_kinematics(CKinematics* _k, const char* s);
#endif

// Skeletons whose bones this thread is calculating right now
static thread_local u32 g_SkeletonUpdateDepth = 0;

// ::Random is shared by all threads; same generator as CRandom
static thread_local u32 g_SkeletonRandomState = 1;

static s32 SkeletonRandom(s32 Max)
{
	g_SkeletonRandomState = g_SkeletonRandomState * 214013u + 2531011u;
	const s32 Value = s32((g_SkeletonRandomState >> 16) & 0x7fff);
	return Max > 0 ? Value % Max : 0;
}

// Bone and visual callbacks read other skeletons (IK ray picks hit collision forms, NPCs look at
// the actor). Two threads that calculate neighbouring skeletons could then wait for each other.
// A thread inside a calculation therefore never waits for another skeleton: an update is skipped,
// because its owner is calculating it right now, and a read takes the current matrices unlocked.
class SkeletonLock
{
public:
	SkeletonLock(xrCriticalSection& InMutex, bool InUpdate) :
		Mutex(InMutex),
		Update(InUpdate)
	{
		if (g_SkeletonUpdateDepth == 0)
		{
			Mutex.Enter();
			Locked = true;
		}
		else
		{
			Locked = Mutex.TryEnter();
		}

		if (Locked && Update)
		{
			++g_SkeletonUpdateDepth;
		}
	}

	~SkeletonLock()
	{
		if (!Locked)
		{
			return;
		}

		if (Update)
		{
			--g_SkeletonUpdateDepth;
		}
		Mutex.Leave();
	}

	SkeletonLock(const SkeletonLock&) = delete;
	SkeletonLock& operator=(const SkeletonLock&) = delete;

	bool IsLocked() const
	{
		return Locked;
	}

private:
	xrCriticalSection& Mutex;
	bool Update;
	bool Locked = false;
};

void CKinematics::AdvanceTracks()
{
	// Once per game tick, even when a track callback calculates this skeleton again
	if (TracksTime == RDEVICE.dwTimeGlobal)
		return;

	TracksTime = RDEVICE.dwTimeGlobal;
	OnCalculateBones();
}

void CKinematics::PrepareBones()
{
	if (RDEVICE.dwTimeGlobal == UCalc_Time)
		return;

	SkeletonLock Lock(UCalc_Mutex, true);
	if (!Lock.IsLocked() || RDEVICE.dwTimeGlobal == UCalc_Time)
		return;

	AdvanceTracks();
}

void CKinematics::CalculateBones(bool bForceExact)
{
	PROF_EVENT("CKinematics::CalculateBones");
	// early out.
	// check if the info is still relevant
	// skip all the computations - assume nothing changes in a small period of time :)
	if (RDEVICE.dwTimeGlobal == UCalc_Time)
		return;	// early out for "fast" update

	SkeletonLock Lock(UCalc_Mutex, true);
	// Another thread calculates the bones right now, or has done it while this one waited
	if (!Lock.IsLocked() || RDEVICE.dwTimeGlobal == UCalc_Time)
		return;

	AdvanceTracks();

	if (!bForceExact && (RDEVICE.dwTimeGlobal < (UCalc_Time + UCalc_Interval)))
		return;	// early out for "slow" update

	if (Update_Visibility)
		Visibility_Update();


	// here we have either:
	//	1:	timeout elapsed
	//	2:	exact computation required
	UCalc_Time = RDEVICE.dwTimeGlobal;

	// exact computation
	// Calculate bones
#ifdef DEBUG
	RDEVICE.Statistic->Animation.Begin();
#endif

	CalculateBoneHierarchy(bones->at(iRoot),&Fidentity);
#ifdef DEBUG
	check_kinematics				(this, dbg_name.c_str() );
	RDEVICE.Statistic->Animation.End	();
#endif
	VERIFY( LL_GetBonesVisible()._visimask.flags !=0 );
	// Calculate BOXes/Spheres if needed
	UCalc_Visibox++;
	if (UCalc_Visibox>=psSkeletonUpdate)
	{
		// mark
		UCalc_Visibox = -SkeletonRandom(psSkeletonUpdate-1);

		CalculateBBox();

#ifdef DEBUG
		// Validate
		VERIFY3	(_valid(vis.box.min)&&_valid(vis.box.max),	"Invalid bones-xform in model", dbg_name.c_str());
		if(vis.sphere.R>1000.f)
		{
			for(u16 ii=0; ii<LL_BoneCount();++ii){
				Fmatrix tr;
				tr = LL_GetTransform(ii);
				Msg("bone %s",LL_BoneName_dbg(ii));
				Log("bone_matrix",tr);
			}
			Log("end-------");
		}
		VERIFY3	(vis.sphere.R<1000.f,						"Invalid bones-xform in model", dbg_name.c_str());
#endif
	}

	//
	if (Update_Callback)
		Update_Callback(this);
}

void CKinematics::LL_GetBoneLocalPosition(u16 bone_id, Fvector& result)
{
	SkeletonLock Lock(UCalc_Mutex, false);
	result = LL_GetBoneInstance(bone_id).mTransform.c;
}

void CKinematics::LL_GetBoneLocalTransform(u16 bone_id, Fmatrix& result)
{
	SkeletonLock Lock(UCalc_Mutex, false);
	result = LL_GetBoneInstance(bone_id).mTransform;
}

#ifdef DEBUG
void check_kinematics(CKinematics* _k, const char* s)
{
	CKinematics* K = _k;
	Fmatrix&	MrootBone		= K->LL_GetBoneInstance(K->LL_GetBoneRoot()).mTransform;
	if(MrootBone.c.y >10000)
	{
		Msg("all bones transform:--------[%s]",s);

		for(u16 ii=0; ii<K->LL_BoneCount();++ii){
			Fmatrix tr;

			tr = K->LL_GetTransform(ii);
			Msg("bone %s",K->LL_BoneName_dbg(ii));
			Log("bone_matrix",tr);
		}
		Log("end-------");
		VERIFY3(0,"check_kinematics failed for ", s);
	}
}
#endif

void CKinematics::BuildBoneMatrix( const CBoneData* bd, CBoneInstance &bi, const Fmatrix *parent, u8 channel_mask/* = (1<<0)*/ )
{
	bi.mTransform.mul_43(*parent,bd->bind_transform);
}

void CKinematics::CLBone( const CBoneData* bd, CBoneInstance &bi, const Fmatrix *parent, u8 channel_mask /*= (1<<0)*/)
{
	u16 SelfID = bd->GetSelfID();

	if (LL_GetBoneVisible(SelfID))
	{
		if (bi.callback_overwrite())
		{
			if (bi.callback())
				bi.callback()(&bi);
		}
		else
		{
			BuildBoneMatrix( bd, bi, parent, channel_mask );
#ifndef MASTER_GOLD
			R_ASSERT2( _valid( bi.mTransform ), "anim kils bone matrix" );
#endif // #ifndef MASTER_GOLD
			if (bi.callback())
			{
				bi.callback()(&bi);
#ifndef MASTER_GOLD
				R_ASSERT2( _valid( bi.mTransform ), make_string<const char*>( "callback kils bone matrix bone: %s " , bd->name.c_str() ) );
#endif // #ifndef MASTER_GOLD
			}
		}
		bi.mRenderTransform.mul_43(bi.mTransform,bd->m2b_transform);
	}
}

void CKinematics::Bone_GetAnimPos(Fmatrix& pos,u16 id,u8 mask_channel, bool ignore_callbacks)
{
	R_ASSERT(id<LL_BoneCount());
	CBoneInstance bi = bone_instances[id];
	Fvector last_c = bi.mTransform.c;
	BoneChain_Calculate(&LL_GetData(id),bi,mask_channel,ignore_callbacks);
#ifndef MASTER_GOLD
	R_ASSERT( _valid( bi.mTransform ) );
#endif
	pos.set( bi.mTransform );
	pos.c.set(last_c);
}

void CKinematics::Bone_Calculate(CBoneData* bd, Fmatrix *parent)
{
	SkeletonLock Lock(UCalc_Mutex, true);
	if (Lock.IsLocked())
		CalculateBoneHierarchy(bd, parent);
}

void CKinematics::CalculateBoneHierarchy(const CBoneData* bd, const Fmatrix* parent)
{
	CBoneInstance &BONE_INST = bone_instances[bd->GetSelfID()];
	CLBone( bd, BONE_INST, parent, u8(-1) );
	// Calculate children
	for (const CBoneData* BD : bd->children)
		CalculateBoneHierarchy(BD, &BONE_INST.mTransform);
}

void CKinematics::BoneChain_Calculate(const CBoneData* bd, CBoneInstance &bi, u8 mask_channel, bool ignore_callbacks)
{
	u16 SelfID = bd->GetSelfID();

	//ignore callbacks
	BoneCallback bc = bi.callback();
	bool ow = bi.callback_overwrite();
	if(ignore_callbacks)
		bi.set_callback( bi.callback_type(), nullptr, bi.callback_param(), false );

	if(SelfID==LL_GetBoneRoot())
	{
		CLBone( bd, bi, &Fidentity, mask_channel );
		//restore callback
		bi.set_callback( bi.callback_type(), bc, bi.callback_param(), ow );
		return;
	}
	u16 ParentID = bd->GetParentID();
	R_ASSERT( ParentID != BI_NONE );
	CBoneData* ParrentDT = &LL_GetData(ParentID);
	CBoneInstance parrent_bi = bone_instances[ParentID];
	BoneChain_Calculate(ParrentDT, parrent_bi, mask_channel, ignore_callbacks);
	CLBone( bd, bi, &parrent_bi.mTransform, mask_channel );
	//restore callback
	bi.set_callback( bi.callback_type(), bc, bi.callback_param(), ow );

}
