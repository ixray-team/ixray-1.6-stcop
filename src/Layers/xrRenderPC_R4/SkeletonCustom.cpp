//---------------------------------------------------------------------------
#include 	"stdafx.h"

#include	"SkeletonX.h"
#include "../../xrEngine/Fmesh.h"

#include "../../xrEngine/Render.h"

int psSkeletonUpdate = 32;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
bool	pred_N(const std::pair<shared_str,u32>&	N, const char* B)			{
	return xr_strcmp(*N.first,B)<0;
}
u16		CKinematics::LL_BoneID		(const char* B)			{
	accel::iterator I	= std::lower_bound	(bone_map_N->begin(),bone_map_N->end(),B,pred_N);
	if (I == bone_map_N->end())			return BI_NONE;
	if (0 != xr_strcmp(*(I->first),B))	return BI_NONE;
	return				u16(I->second);
}
bool	pred_P(const std::pair<shared_str,u32>&	N, const shared_str& B)	{
	return N.first._get() < B._get();
}
u16		CKinematics::LL_BoneID		(const shared_str& B)	{
	accel::iterator I	= std::lower_bound	(bone_map_P->begin(),bone_map_P->end(),B,pred_P);
	if (I == bone_map_P->end())			return BI_NONE;
	if (I->first._get() != B._get() )	return BI_NONE;
	return				u16(I->second);
}

//
const char* CKinematics::LL_BoneName_dbg	(u16 ID)
{
	CKinematics::accel::iterator _I, _E=bone_map_N->end();
	for (_I	= bone_map_N->begin(); _I!=_E; ++_I)	if (_I->second==ID) return *_I->first;
	return nullptr;
}

#ifdef DEBUG_DRAW
void CKinematics::DebugRender(Fmatrix& XFORM)
{
	CalculateBones	();
#ifdef DEBUG
	CBoneData::BoneDebug	dbgLines;
	(*bones)[iRoot]->DebugQuery	(dbgLines);

	Fvector Z;  Z.set(0,0,0);
	Fvector H1; H1.set(0.01f,0.01f,0.01f);
	Fvector H2; H2.mul(H1,2);
	for (u32 i=0; i<dbgLines.size(); i+=2)	{
		Fmatrix& M1 = bone_instances[dbgLines[i]].mTransform;
		Fmatrix& M2 = bone_instances[dbgLines[i+1]].mTransform;

		Fvector P1,P2;
		M1.transform_tiny(P1,Z);
		M2.transform_tiny(P2,Z);
		RCache.dbg_DrawLINE(XFORM, P1, P2, color_xrgb(0, 255, 0));

		Fmatrix M;
		M.mul_43(XFORM,M2);
		RCache.dbg_DrawOBB(M, H1, color_xrgb(255, 255, 255));
		RCache.dbg_DrawOBB(M, H2, color_xrgb(255, 255, 255));
	}
#endif

	for (u32 b=0; b<bones->size(); b++)
	{
		Fobb&		obb		= (*bones)[b]->obb;
		Fmatrix&	Mbone	= bone_instances[b].mTransform;
		Fmatrix		Mbox;	obb.xform_get(Mbox);
		Fmatrix		X;		X.mul(Mbone,Mbox);
		Fmatrix		W;		W.mul(XFORM,X);
		RCache.dbg_DrawOBB(W, obb.m_halfsize, color_xrgb(0, 0, 255));
	}
}
#endif

CKinematics::CKinematics()
{
#ifdef DEBUG
	dbg_single_use_marker		= false;
#endif

	m_is_original_lod = false;

	mOldWorldMartrix.identity();
	mOldWorldMartrixTmp.identity();

	dwFirstRenderFrame = 0;
}

CKinematics::~CKinematics	()
{
	IBoneInstances_Destroy	();
	// wallmarks
	ClearWallmarks			();

	if(m_lod)
	{
		if ( CKinematics* lod_kinematics = smart_cast<CKinematics*>(m_lod) )
		{
			if ( lod_kinematics->m_is_original_lod )
			{
				lod_kinematics->Release();
			}
		}

		xr_delete(m_lod);
	}
}

void	CKinematics::IBoneInstances_Create()
{
	// VERIFY2				(bones->size() < 64, "More than 64 bones is a crazy thing!");
	u32				size	= (u32)bones->size();
	bone_instances			=xr_alloc<CBoneInstance>(size);
	for (u32 i=0; i<size; i++)
		bone_instances[i].construct();
}

void	CKinematics::IBoneInstances_Destroy()
{
	if (bone_instances) {
		xr_free(bone_instances);
		bone_instances = nullptr;
	}
}

bool	pred_sort_N(const std::pair<shared_str,u32>& A, const std::pair<shared_str,u32>& B)	{
	return xr_strcmp(A.first,B.first)<0;
}
bool	pred_sort_P(const std::pair<shared_str,u32>& A, const std::pair<shared_str,u32>& B)	{
	return A.first._get() < B.first._get();
}

CSkeletonX* CKinematics::LL_GetChild(u32 idx)
{
	IRenderVisual* V = children[idx];
	CSkeletonX* B = smart_cast<CSkeletonX*>(V);
	return B;
}

void	CKinematics::Load(const char* N, IReader *data, u32 dwFlags)
{
	PROF_EVENT(__FUNCTION__);
	//Msg				("skeleton: %s",N);
	inherited::Load	(N, data, dwFlags);

    pUserData		= nullptr;
    m_lod			= nullptr;
	m_reference		= nullptr;
    // loading lods

	IReader* LD 	= data->open_chunk(OGF_S_LODS);
    if (LD)
	{
		PROF_EVENT("CKinematics::Load: LOD");
        string_path		short_name;
        xr_strcpy		(short_name,sizeof(short_name),N);

        if (strext(short_name)) *strext(short_name)=0;
        // From stream
		{
			string_path		lod_name;
			LD->r_string	(lod_name, sizeof(lod_name));
//.         strconcat		(sizeof(name_load),name_load, short_name, ":lod:", lod_name.c_str());
            m_lod 			= (dxRender_Visual*) ::Render->model_CreateChild(lod_name, nullptr);

			if ( CKinematics* lod_kinematics = smart_cast<CKinematics*>(m_lod) )
			{
				lod_kinematics->m_is_original_lod = true;
			}

            VERIFY3(m_lod,"Cant create LOD model for", N);
//.			VERIFY2			(m_lod->Type==MT_HIERRARHY || m_lod->Type==MT_PROGRESSIVE || m_lod->Type==MT_NORMAL,lod_name.c_str());
/*
			strconcat		(name_load, short_name, ":lod:1");
            m_lod 			= ::Render->model_CreateChild(name_load,LD);
			VERIFY			(m_lod->Type==MT_SKELETON_GEOMDEF_PM || m_lod->Type==MT_SKELETON_GEOMDEF_ST);
*/
        }
        LD->close	();
    }

 
	// User data
	IReader* UD 	= data->open_chunk(OGF_S_USERDATA);
    pUserData		= UD?new CInifile(UD,FS.get_path(_game_config_)->m_Path):nullptr;
    if (UD)			UD->close();

	// Globals
	bone_map_N		= new accel		();
	bone_map_P		= new accel		();
	bones			= new vecBones	();
	bone_instances	= nullptr;

	// Load bones
	bool FoundedChunk = !!data->find_chunk(OGF_S_BONE_NAMES);
	R_ASSERT2(FoundedChunk, "Not found chunk OGF_S_BONE_NAMES");

    visimask.zero	();
	int dwCount = data->r_u32();
	for(; dwCount; dwCount--) {
		string256	buf;

		// Bone
		u16			ID				= u16(bones->size());
		data->r_stringZ				(buf,sizeof(buf));	_strlwr(buf);
		CBoneData* pBone 			= CreateBoneData(ID);
		pBone->name					= shared_str(buf);
		pBone->child_faces.resize	(children.size());
		bones->push_back			(pBone);
		bone_map_N->push_back		(std::make_pair(pBone->name,ID));
		bone_map_P->push_back		(std::make_pair(pBone->name,ID));

		// It's parent
		data->r_stringZ				(buf,sizeof(buf));	_strlwr(buf);
		L_parents.push_back			(buf);

		data->r						(&pBone->obb,sizeof(Fobb));
		visimask.set(ID, true);
	}
	std::sort	(bone_map_N->begin(),bone_map_N->end(),pred_sort_N);
	std::sort	(bone_map_P->begin(),bone_map_P->end(),pred_sort_P);

	// Attach bones to their parents
	iRoot = BI_NONE;
	for (u32 i=0; i<bones->size(); i++)
	{
		shared_str	P 		= L_parents[i];
		CBoneData* B	= (*bones)[i];
		if (!P||!P[0]) {
			// no parent - this is root bone
			R_ASSERT	(BI_NONE==iRoot);
			iRoot		= u16(i);
			B->SetParentID(BI_NONE);
			continue;
		} else {
			u16 ID		= LL_BoneID(P);
			R_ASSERT	(ID!=BI_NONE);
			(*bones)[ID]->children.push_back(B);
			B->SetParentID(ID);
		}
	}
	R_ASSERT	(BI_NONE != iRoot);

	// Free parents
    L_parents.clear();

    // IK data
	IReader* IKD 	= data->open_chunk(OGF_S_IKDATA);
    if (IKD){
		PROF_EVENT("CKinematics::Load: IK data");
        for (u32 i=0; i<bones->size(); i++) {
            CBoneData*	B 	= (*bones)[i];
            u16 vers		= (u16)IKD->r_u32();
            IKD->r_stringZ	(B->game_mtl_name);
            IKD->r			(&B->shape,sizeof(SBoneShape));
            B->IK_data.Import(*IKD,vers);
            Fvector vXYZ,vT;
            IKD->r_fvector3	(vXYZ);
            IKD->r_fvector3	(vT);
            B->bind_transform.setXYZi(vXYZ);
            B->bind_transform.translate_over(vT);
	        B->mass			= IKD->r_float();
    	    IKD->r_fvector3	(B->center_of_mass);
        }
        // calculate model to bone converting matrix
        (*bones)[LL_GetBoneRoot()]->CalculateM2B(Fidentity);
    	IKD->close();
    }

	// after load process
	{
		for (u16 child_idx=0; child_idx<(u16)children.size(); child_idx++)
			LL_GetChild(child_idx)->AfterLoad	(this,child_idx);
	}

	// unique bone faces
	{
		PROF_EVENT("CKinematics::Load: unique bone faces");
		for (u32 bone_idx=0; bone_idx<bones->size(); bone_idx++) {
			CBoneData*	B 	= (*bones)[bone_idx];
			for (u32 child_idx=0; child_idx<children.size(); child_idx++){
				CBoneData::FacesVec faces		= B->child_faces[child_idx];
				std::sort						(faces.begin(),faces.end());
				CBoneData::FacesVecIt new_end	= std::unique(faces.begin(),faces.end());
				faces.erase						(new_end,faces.end());
				B->child_faces[child_idx].clear();
				B->child_faces[child_idx]		= faces;
			}
		}
	}

	// reset update_callback
	Update_Callback	= nullptr;
	// reset update frame
	wm_frame		= u32(-1);

    LL_Validate		();
}

void CKinematics::ReloadUserData()
{
	xr_string fn; fn.reserve(300);

	fn = FS.get_path(_game_meshes_)->m_Path;
	fn += *dbg_name;
	if (nullptr == strext(*dbg_name))
		fn += ".ogf";

	if (!FS.exist(fn.c_str()))
	{
		fn = FS.get_path("$level$")->m_Path;
		fn += *dbg_name;
		if (nullptr == strext(*dbg_name))
			fn += ".ogf";
	}

	IReader* data = FS.r_open(fn.c_str());
	if (!data) return;

	if (m_reference && m_reference->pUserData != pUserData)
		xr_delete(pUserData);

	IReader* UD = data->open_chunk(OGF_S_USERDATA);
	pUserData = UD ? new CInifile(UD, FS.get_path(_game_config_)->m_Path) : nullptr;
	UD->close();
	data->close();
}

void iBuildGroups(CBoneData* B, buffer_vector<u16>& tgt, u16 id, u16& last_id)
{
    if (B->IK_data.ik_flags.is(SJointIKData::flBreakable))
		id = ++last_id;

	tgt[B->GetSelfID()]	= id;

    for (CBoneData* BD : B->children)
    	iBuildGroups(BD,tgt,id,last_id);
}

void CKinematics::LL_Validate()
{
	PROF_EVENT(__FUNCTION__);
	// check breakable
    bool bCheckBreakable			= false;
    for (u16 k=0; k<LL_BoneCount(); k++){
        if (LL_GetData(k).IK_data.ik_flags.is(SJointIKData::flBreakable)&&(LL_GetData(k).IK_data.type!=jtNone)) {
        	bCheckBreakable			= true;
            break;
        }
    }

    if (bCheckBreakable){
        bool bValidBreakable		= true;

		GroupIDs.clear();
        LL_GetBoneGroups			(GroupIDs);

		buffer_vector<u16> b_parts(_alloca((size_t)LL_BoneCount() * sizeof(u16)), (size_t)LL_BoneCount(), (size_t)LL_BoneCount(), BI_NONE);
		//���� �������� ���������� �������� �� xr_vector

        CBoneData* root = &LL_GetData(LL_GetBoneRoot());

        u16 last_id = 0;
        iBuildGroups(root,b_parts,0,last_id);

		for (u16 g = 0; g < (u16)GroupIDs.size(); ++g)
		{
			xr_vector<u16>& group = GroupIDs[g];
			u16 bp_id = b_parts[group[0]];

			for (u32 b = 1; b < GroupIDs[g].size(); b++)
			{
				if (bp_id != b_parts[GroupIDs[g][b]]) 
				{ 
					bValidBreakable = false; break;
				}
			}
		}
    
        if (bValidBreakable==false)
		{
            for (u16 k=0; k<LL_BoneCount(); k++)
			{
                CBoneData& BD = LL_GetData(k);

				if (BD.IK_data.ik_flags.is(SJointIKData::flBreakable))
				{
					BD.IK_data.ik_flags.set(SJointIKData::flBreakable, false);
				}
			}
#ifdef DEBUG            
            Msg						("! ERROR: Invalid breakable object: '%s'",*dbg_name);
#endif
        }
    }
}

void CKinematics::Copy(dxRender_Visual *P) 
{
	inherited::Copy	(P);

	CKinematics* pFrom = smart_cast<CKinematics*>(P);
	VERIFY(pFrom);
	m_reference = pFrom;
	pUserData = pFrom->pUserData;
	bones = pFrom->bones;
	iRoot = pFrom->iRoot;
	bone_map_N = pFrom->bone_map_N;
	bone_map_P = pFrom->bone_map_P;
	visimask = pFrom->visimask;

	IBoneInstances_Create();

	for (u32 i = 0; i < children.size(); i++)
	{
		LL_GetChild(i)->SetParent(this);
	}

	CalculateBones_Invalidate();

	m_lod = (pFrom->m_lod) ? (dxRender_Visual*)::Render->model_Duplicate(pFrom->m_lod) : nullptr;
	dwFirstRenderFrame = 0;
}

void CKinematics::CalculateBones_Invalidate	()
{	
	UCalc_Time		= 0x0; 
	UCalc_Visibox	= psSkeletonUpdate;		
}

void CKinematics::Spawn			()
{
	inherited::Spawn			();
	// bones
	for (u32 i=0; i<bones->size(); i++)
		bone_instances[i].construct();
	Update_Callback				= nullptr;
	CalculateBones_Invalidate	();
	// wallmarks
	ClearWallmarks				();
	Visibility_Invalidate		();
	LL_SetBoneRoot( 0 );
}

void CKinematics::Depart		()
{
	inherited::Depart			();
	// wallmarks
	ClearWallmarks				();

	// unmask all bones
	visimask.zero				();
	if(bones)
	{
		u32 count = (u32)bones->size();
#ifdef DEBUG
    	if (count > 64)
        	Msg("ahtung !!! %d", count);
#endif // #ifdef DEBUG
		for(u32 b = 0; b < count; b++) visimask.set(b, true);
	}
	// visibility
	children.insert				(children.end(),children_invisible.begin(),children_invisible.end());
	children_invisible.clear	();
	SetParentObjectId(ALife::_OBJECT_ID(-1));
	if (m_reference && m_reference->pUserData != pUserData)
	{
		xr_delete(pUserData);
		pUserData = m_reference->pUserData;
	}
}

void CKinematics::Release		()
{
	// xr_free bones
	for (u32 i=0; i<bones->size(); i++)
    {
		CBoneData* &B = (*bones)[i];
		xr_delete(B);
	}

	// destroy shared data
    xr_delete(pUserData	);
	xr_delete(bones		);
	xr_delete(bone_map_N);
	xr_delete(bone_map_P);

	inherited::Release();
}

void CKinematics::LL_SetBoneVisible(u16 bone_id, bool val, bool bRecursive)
{
	VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone: %s, bone_id: %d", dbg_name.c_str(), LL_BoneName_dbg(bone_id), bone_id));

	if (bone_id == BI_NONE || bone_id >= LL_BoneCount())
		return;

	visimask.set(bone_id, val);

	// Keep hidden bones at their original origin. Zero-scaling at the model
	// origin stretches skinned vertices toward the actor's root/seat; moving
	// them to the parent's center collapses car-light vertices at the
	// door/trunk hinge instead of inside the lamp housing.
	CBoneInstance& BI = bone_instances[bone_id];
	const Fvector last_pos = BI.mHiddenTransformValid ? BI.mTransformHidden.c : BI.mTransform.c;
	if(!val)
	{
		if(!BI.mHiddenTransformValid)
		{
			BI.mTransformHidden = BI.mTransform;
			BI.mHiddenTransformValid = true;
		}
        BI.mTransform.scale(0.f,0.f,0.f);
		BI.mTransform.c = last_pos;
	}
	else
	{
		// Restore the exact pose that was hidden. The animation update will
		// overwrite animated bones, while bones without keys (jaw) keep their
		// last valid transform instead of a zero-scaled matrix.
		if(BI.mHiddenTransformValid)
		{
			BI.mTransform = BI.mTransformHidden;
			BI.mHiddenTransformValid = false;
		}
		CalculateBones_Invalidate();
	}

	BI.mRenderTransform.mul_43(BI.mTransform,(*bones)[bone_id]->m2b_transform);
	if(bRecursive)
	{
		for(CBoneData* BD : (*bones)[bone_id]->children)
			LL_SetBoneVisible(BD->GetSelfID(), val, bRecursive);
	}
	Visibility_Invalidate();
}

void CKinematics::LL_SetBonesVisible(VisMask mask)
{
	visimask.zero();
	for(u32 b = 0; b < bones->size(); b++)
	{
		CBoneInstance& BI = bone_instances[b];
		if(mask.is(b))
		{
			visimask.set(b, true);
			if(BI.mHiddenTransformValid)
			{
				BI.mTransform = BI.mTransformHidden;
				BI.mHiddenTransformValid = false;
			}
		}
		else
		{
			if(!BI.mHiddenTransformValid)
			{
				BI.mTransformHidden = BI.mTransform;
				BI.mHiddenTransformValid = true;
			}
	    	Fmatrix& A		= BI.mTransform;
	    	Fmatrix& B		= BI.mRenderTransform;
        	A.scale			(0.f,0.f,0.f);
	        B.mul_43		(A,(*bones)[b]->m2b_transform);
        }
	}
	CalculateBones_Invalidate();
	Visibility_Invalidate();
}

void CKinematics::Visibility_Update()
{
	Update_Visibility = false;
	// check visible
	for (u32 c_it=0; c_it<children.size(); c_it++)
	{
		CSkeletonX* _c = (CSkeletonX*)children[c_it];
		if (!_c->has_visible_bones())
		{
			// move into invisible list
			children_invisible.push_back(children[c_it]);	
			swap(children[c_it],children.back());
			children.pop_back();
			Update_Visibility = true;
		}
	}

	// check invisible
	for (u32 _it=0; _it<children_invisible.size(); _it++)
	{
		CSkeletonX* _c = (CSkeletonX*)children_invisible[_it];
		if (_c->has_visible_bones())
		{
			// move into visible list
			children.push_back(children_invisible[_it]);	
			swap(children_invisible[_it],children_invisible.back());
			children_invisible.pop_back();
			Update_Visibility = true;
		}
	}
}

void CKinematics::LL_SetSimpleBoneVisible(u16 bone_id, bool val)
{
	for (u32 c_it = 0; c_it < children.size(); c_it++)
	{
		CSkeletonX* _c = static_cast<CSkeletonX*>(children[c_it]);
		if (_c->uses_bone(bone_id))
		{
			_c->set_simple_visible(val);
		}
	}
}

bool CKinematics::LL_GetSimpleBoneVisible(u16 bone_id)
{
	for (u32 c_it = 0; c_it < children.size(); c_it++)
	{
		CSkeletonX* _c = static_cast<CSkeletonX*>(children[c_it]);
		if (_c->uses_bone(bone_id))
		{
			return _c->get_simple_visible();
		}
	}

	return true;
}

void RecursiveBindTransform(CKinematics* K, xr_vector<Fmatrix>& matrices, u16 bone_id, const Fmatrix& parent)
{
	CBoneData& BD = K->LL_GetData	(bone_id);
	Fmatrix& BM = matrices[bone_id];
	// Build matrix
	BM.mul_43(parent,BD.bind_transform);
	for (CBoneData* BD : BD.children)
		RecursiveBindTransform(K, matrices, BD->GetSelfID(), BM);
}

void RecursiveBindTransform(CKinematics* K, buffer_vector<Fmatrix>& matrices, u16 bone_id, const Fmatrix& parent)
{
	CBoneData& BD = K->LL_GetData(bone_id);
	Fmatrix& BM = matrices[bone_id];
	// Build matrix
	BM.mul_43(parent, BD.bind_transform);
	for (CBoneData* BD : BD.children)
		RecursiveBindTransform(K, matrices, BD->GetSelfID(), BM);
}

void CKinematics::LL_GetBindTransform(xr_vector<Fmatrix>& matrices)
{
	matrices.resize(bones->size());
	RecursiveBindTransform(this,matrices,iRoot,Fidentity);
}

void CKinematics::LL_GetBindTransform(buffer_vector<Fmatrix>& matrices)
{
	matrices.resize(bones->size());
	RecursiveBindTransform(this, matrices, iRoot, Fidentity);
}


int CKinematics::LL_GetBoneGroups(xr_vector<xr_vector<u16> >& groups)
{
	groups.resize(children.size());
    for (u16 bone_idx=0; bone_idx<(u16)bones->size(); bone_idx++)
	{
        CBoneData* B = (*bones)[bone_idx];
        for (u32 child_idx=0; child_idx<children.size(); child_idx++)
		{
        	if (!B->child_faces[child_idx].empty())
            	groups[child_idx].push_back(bone_idx);
        }
    }
    return (int)groups.size();
}

void CKinematics::StoreVisualMatrix(Fmatrix& world_matrix)
{
	PROF_EVENT("StoreVisualMatrix")

	if (dwFirstRenderFrame != RDEVICE.dwFrame)
	{
		if(dwFirstRenderFrame != 0)
		{
			mOldWorldMartrix.set(mOldWorldMartrixTmp);
			mOldWorldMartrixTmp.set(world_matrix);

			for(u16 i = 0; i < bones->size(); ++i)
			{
				auto& Bi = bone_instances[i];

				Bi.mRenderTransform_old.set(Bi.mRenderTransform_tmp);
				Bi.mRenderTransform_tmp.set(Bi.mRenderTransform);
			}
		}
		else
		{
			mOldWorldMartrixTmp.set(world_matrix);
			mOldWorldMartrix.set(world_matrix);

			for(u16 i = 0; i < bones->size(); ++i)
			{
				auto& Bi = bone_instances[i];

				Bi.mRenderTransform_tmp.set(Bi.mRenderTransform);
				Bi.mRenderTransform_old.set(Bi.mRenderTransform);
			}
		}

		if (Hair.IsUsed())
		{
			Hair.Resolve([this](const char* Name) { return LL_BoneID(Name); }, [this](u16 Id) -> const Fobb& { return LL_GetData(Id).obb; });
			Hair.Update(world_matrix, bone_instances, u16(bones->size()), dwFirstRenderFrame == 0 || RDEVICE.dwFrame - dwFirstRenderFrame > 1);
		}

		dwFirstRenderFrame = RDEVICE.dwFrame;
	}
}

void CKinematics::BonesHistory_Invalidate()
{
	dwFirstRenderFrame = 0;
}

void CKinematics::LL_SetShader(LPCSTR shader_name)
{
	for (dxRender_Visual* V : children)
	{
		V->shader_name = shader_name;
		V->reload_shader();
	}
}

void CKinematics::LL_RestoreShader()
{
	for (dxRender_Visual* V : children)
	{
		V->restore_shader();
	}
}

void CKinematics::LL_SetShader(u16 bone_id, LPCSTR shader_name)
{
	for (u32 c_it = 0; c_it < children.size(); c_it++)
	{
		CSkeletonX* _c = static_cast<CSkeletonX*>(children[c_it]);
		if (_c->uses_bone(bone_id))
		{
			_c->shader_name = shader_name;
			_c->reload_shader();
		}
	}
}
void CKinematics::LL_RestoreShader(u16 bone_id)
{
	for (u32 c_it = 0; c_it < children.size(); c_it++)
	{
		CSkeletonX* _c = static_cast<CSkeletonX*>(children[c_it]);
		if (_c->uses_bone(bone_id))
			_c->restore_shader();
	}
}

void CKinematics::LL_SetTexture(LPCSTR texture_name)
{
	for (dxRender_Visual* V : children)
	{
		V->texture_name = texture_name;
		V->reload_shader();
	}
}

void CKinematics::LL_RestoreTexture()
{
	for (dxRender_Visual* V : children)
	{
		CSkeletonX* S = static_cast<CSkeletonX*>(V);
		V->restore_texture();
	}
}

void CKinematics::LL_SetTexture(u16 bone_id, LPCSTR texture_name)
{
	for (u32 c_it = 0; c_it < children.size(); c_it++)
	{
		CSkeletonX* _c = static_cast<CSkeletonX*>(children[c_it]);
		if (_c->uses_bone(bone_id))
		{
			_c->texture_name = texture_name;
			_c->reload_shader();
		}
	}
}
void CKinematics::LL_RestoreTexture(u16 bone_id)
{
	for (u32 c_it = 0; c_it < children.size(); c_it++)
	{
		CSkeletonX* _c = static_cast<CSkeletonX*>(children[c_it]);
		if (_c->uses_bone(bone_id))
			_c->restore_texture();
	}
}

xr_vector<IRenderVisual*>& CKinematics::LL_GetChilds()
{
	return reinterpret_cast<xr_vector<IRenderVisual*>&>(children);
}