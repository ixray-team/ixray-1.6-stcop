//---------------------------------------------------------------------------
#include 	"stdafx.h"


#include 	"SkeletonCustom.h"

extern int	psSkeletonUpdate;

#ifdef DEBUG
void check_kinematics(CKinematics* _k, const char* s);
#endif

void CKinematics::CalculateBones(bool bForceExact)
{
	PROF_EVENT("CKinematics::CalculateBones");
	// early out.
	// check if the info is still relevant
	// skip all the computations - assume nothing changes in a small period of time :)
	if (RDEVICE.dwTimeGlobal == UCalc_Time)
		return;	// early out for "fast" update

	xrCriticalSectionGuard guard(&UCalc_Mutex);
	OnCalculateBones();

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

	Bone_Calculate(bones->at(iRoot),&Fidentity);
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
		UCalc_Visibox = -(::Random.randI(psSkeletonUpdate-1));

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
	xrCriticalSectionGuard guard(&UCalc_Mutex2);
	u16 SelfID = bd->GetSelfID();
	CBoneInstance &BONE_INST = bone_instances[SelfID];
	CLBone( bd, BONE_INST, parent, u8(-1) );
	// Calculate children
	for (CBoneData* BD : bd->children)
		Bone_Calculate(BD, &BONE_INST.mTransform);

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
