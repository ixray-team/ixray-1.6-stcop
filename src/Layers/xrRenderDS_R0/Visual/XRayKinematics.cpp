#include "stdafx.h"
#include "XRayKinematics.h"
#include "XRaySkeletonX.h"
#include "XRayModelPool.h"

bool	pred_sort_N(const std::pair<shared_str, u32>& A, const std::pair<shared_str, u32>& B) {
	return xr_strcmp(A.first, B.first) < 0;
}
bool	pred_sort_P(const std::pair<shared_str, u32>& A, const std::pair<shared_str, u32>& B) {
	return A.first._get() < B.first._get();
}
int	psSkeletonUpdate;
IC void iBuildGroups(CBoneData* B, U16Vec& tgt, u16 id, u16& last_id)
{
	if (B->IK_data.ik_flags.is(SJointIKData::flBreakable)) id = ++last_id;
	tgt[B->GetSelfID()] = id;
	for (xr_vector<CBoneData*>::iterator bone_it = B->children.begin(); bone_it != B->children.end(); bone_it++)
		iBuildGroups(*bone_it, tgt, id, last_id);
}
void CDS0_Kinematics::LL_Validate()
{
	BOOL bCheckBreakable = FALSE;
	for (u16 k = 0; k < LL_BoneCount(); k++) {
		if (LL_GetData(k).IK_data.ik_flags.is(SJointIKData::flBreakable) && (LL_GetData(k).IK_data.type != jtNone)) {
			bCheckBreakable = TRUE;
			break;
		}
	}

	if (bCheckBreakable)
	{
		BOOL bValidBreakable = TRUE;

#pragma todo("container is created in stack!")
		xr_vector<xr_vector<u16> > 	groups;
		LL_GetBoneGroups(groups);

#pragma todo("container is created in stack!")
		xr_vector<u16>   			b_parts(LL_BoneCount(), BI_NONE);
		CBoneData* root = &LL_GetData(LL_GetBoneRoot());
		u16 last_id = 0;
		iBuildGroups(root, b_parts, 0, last_id);

		for (u16 g = 0; g < (u16)groups.size(); ++g) {
			xr_vector<u16>& group = groups[g];
			u16 bp_id = b_parts[group[0]];
			for (u32 b = 1; b < groups[g].size(); b++)
				if (bp_id != b_parts[groups[g][b]]) { bValidBreakable = FALSE; break; }
		}

		if (bValidBreakable == FALSE) {
			for (u16 k = 0; k < LL_BoneCount(); k++) {
				CBoneData& BD = LL_GetData(k);
				if (BD.IK_data.ik_flags.is(SJointIKData::flBreakable))
					BD.IK_data.ik_flags.set(SJointIKData::flBreakable, FALSE);
			}
#ifdef DEBUG            
			Msg("! ERROR: Invalid breakable object: '%s'", *DebugName);
#endif
		}
	}
}
#ifdef DEBUG_DRAW
void CDS0_Kinematics::DebugRender(Fmatrix& XFORM)
{
}
#endif
 void	CDS0_Kinematics::Release()
{
	 for (u32 i = 0; i < bones->size(); i++)
	 {
		 CBoneData*& B = (*bones)[i];
		 xr_delete(B);
	 }

	 // destroy shared data
	 xr_delete(pUserData);
	 xr_delete(bones);
	 xr_delete(bone_map_N);
	 xr_delete(bone_map_P);
}
void CDS0_Kinematics::Load(const char* N, IReader* data, u32 dwFlags)
{
	CDS0_FHierrarhyVisual::Load(N, data, dwFlags);

	pUserData = NULL;
	m_lod = NULL;
	// loading lods

	IReader* LD = data->open_chunk(OGF_S_LODS);
	if (LD)
	{
		string_path		short_name;
		xr_strcpy(short_name, sizeof(short_name), N);

		if (strext(short_name)) *strext(short_name) = 0;
		// From stream
		{
			string_path		lod_name;
			LD->r_string(lod_name, sizeof(lod_name));
			//.         strconcat		(sizeof(name_load),name_load, short_name, ":lod:", lod_name.c_str());
			m_lod = (CDS0_RenderVisual*) GRenderInterface.model_CreateChild(lod_name, NULL);

			if (CDS0_Kinematics* lod_kinematics = dynamic_cast<CDS0_Kinematics*>(m_lod))
			{
				lod_kinematics->m_is_original_lod = true;
			}

			VERIFY3(m_lod, "Cant create LOD model for", N);
			//.			VERIFY2			(m_lod->Type==MT_HIERRARHY || m_lod->Type==MT_PROGRESSIVE || m_lod->Type==MT_NORMAL,lod_name.c_str());
			/*
						strconcat		(name_load, short_name, ":lod:1");
						m_lod 			= ::Render->model_CreateChild(name_load,LD);
						VERIFY			(m_lod->Type==MT_SKELETON_GEOMDEF_PM || m_lod->Type==MT_SKELETON_GEOMDEF_ST);
			*/
		}
		LD->close();
	}

#ifndef _EDITOR    
	// User data
	IReader* UD = data->open_chunk(OGF_S_USERDATA);
	pUserData = UD ? new CInifile(UD, FS.get_path("$game_config$")->m_Path) : 0;
	if (UD)			UD->close();
#endif

	// Globals
	bone_map_N = new accel();
	bone_map_P = new accel();
	bones = new vecBones();
	bone_instances = NULL;

	// Load bones
#pragma todo("container is created in stack!")
	xr_vector<shared_str>	L_parents;

	R_ASSERT(data->find_chunk(OGF_S_BONE_NAMES));

	visimask.zero();
	int dwCount = data->r_u32();

	for (; dwCount; dwCount--) {
		string256	buf;

		// Bone
		u16			ID = u16(bones->size());
		data->r_stringZ(buf, sizeof(buf));	_strlwr(buf);
		CBoneData* pBone = CreateBoneData(ID);
		pBone->name = shared_str(buf);
		pBone->child_faces.resize(children.size());
		bones->push_back(pBone);
		bone_map_N->push_back(std::make_pair(pBone->name, ID));
		bone_map_P->push_back(std::make_pair(pBone->name, ID));

		// It's parent
		data->r_stringZ(buf, sizeof(buf));	_strlwr(buf);
		L_parents.push_back(buf);

		data->r(&pBone->obb, sizeof(Fobb));
		visimask.set(ID, true);
	}
	std::sort(bone_map_N->begin(), bone_map_N->end(), pred_sort_N);
	std::sort(bone_map_P->begin(), bone_map_P->end(), pred_sort_P);

	// Attach bones to their parents
	iRoot = BI_NONE;
	for (u32 i = 0; i < bones->size(); i++) {
		shared_str	P = L_parents[i];
		CBoneData* B = (*bones)[i];
		if (!P || !P[0]) {
			// no parent - this is root bone
			R_ASSERT(BI_NONE == iRoot);
			iRoot = u16(i);
			B->SetParentID(BI_NONE);
			continue;
		}
		else {
			u16 ID = LL_BoneID(P);
			R_ASSERT(ID != BI_NONE);
			(*bones)[ID]->children.push_back(B);
			B->SetParentID(ID);
		}
	}
	R_ASSERT(BI_NONE != iRoot);

	// Free parents
	L_parents.clear();

	// IK data
	IReader* IKD = data->open_chunk(OGF_S_IKDATA);
	if (IKD) {
		for (u32 i = 0; i < bones->size(); i++) {
			CBoneData* B = (*bones)[i];
			u16 vers = (u16)IKD->r_u32();
			IKD->r_stringZ(B->game_mtl_name);
			IKD->r(&B->shape, sizeof(SBoneShape));
			B->IK_data.Import(*IKD, vers);
			Fvector vXYZ, vT;
			IKD->r_fvector3(vXYZ);
			IKD->r_fvector3(vT);
			B->bind_transform.setXYZi(vXYZ);
			B->bind_transform.translate_over(vT);
			B->mass = IKD->r_float();
			IKD->r_fvector3(B->center_of_mass);
		}
		// calculate model to bone converting matrix
		(*bones)[LL_GetBoneRoot()]->CalculateM2B(Fidentity);
		IKD->close();
	}

	// after load process
	{
		for (u16 child_idx = 0; child_idx < (u16)children.size(); child_idx++)
			LL_GetChild(child_idx)->AfterLoad(this, child_idx);
	}

	// unique bone faces
	{
		for (u32 bone_idx = 0; bone_idx < bones->size(); bone_idx++) {
			CBoneData* B = (*bones)[bone_idx];
			for (u32 child_idx = 0; child_idx < children.size(); child_idx++) {
				CBoneData::FacesVec faces = B->child_faces[child_idx];
				std::sort(faces.begin(), faces.end());
				CBoneData::FacesVecIt new_end = std::unique(faces.begin(), faces.end());
				faces.erase(new_end, faces.end());
				B->child_faces[child_idx].clear();
				B->child_faces[child_idx] = faces;
			}
		}
	}

	// reset update_callback
	Update_Callback = NULL;
	// reset update frame
	wm_frame = u32(-1);
	LL_Validate();
}

void CDS0_Kinematics::Copy(CDS0_RenderVisual* from)
{
	CDS0_FHierrarhyVisual::Copy(from);

	CDS0_Kinematics* pFrom = dynamic_cast<CDS0_Kinematics*>(from);
	VERIFY(pFrom);
	pUserData = pFrom->pUserData;
	bones = pFrom->bones;
	iRoot = pFrom->iRoot;
	bone_map_N = pFrom->bone_map_N;
	bone_map_P = pFrom->bone_map_P;
	visimask = pFrom->visimask;

	IBoneInstances_Create();

	for (u32 i = 0; i < children.size(); i++)
		LL_GetChild(i)->SetParent(this);

	CalculateBones_Invalidate();

	m_lod = (pFrom->m_lod) ? (CDS0_RenderVisual*)::Render->model_Duplicate(pFrom->m_lod) : 0;
}

void CDS0_Kinematics::Spawn()
{
	CDS0_FHierrarhyVisual::Spawn();
	// bones
	for (u32 i = 0; i < bones->size(); i++)
		bone_instances[i].construct();
	Update_Callback = NULL;
	CalculateBones_Invalidate();
	// wallmarks
	//ClearWallmarks();
	Visibility_Invalidate();
	LL_SetBoneRoot(0);
}

void CDS0_Kinematics::Depart()
{
	CDS0_FHierrarhyVisual::Depart();
	// wallmarks
	//ClearWallmarks();

	// unmask all bones
	visimask.zero();
	if (bones)
	{
		u16 count = bones->size();

		for (u16 b = 0; b < count; b++) 
			visimask.set(b, true);
	}
	// visibility
	children.insert(children.end(), children_invisible.begin(), children_invisible.end());
	children_invisible.clear();
}

void CDS0_Kinematics::CLBone(const CBoneData* bd, CBoneInstance& bi, const Fmatrix* parent, u8 channel_mask)
{
	u16 SelfID = bd->GetSelfID();

	if (LL_GetBoneVisible(SelfID)) 
	{
		if (bi.callback_overwrite()) 
		{
			if (bi.callback())	bi.callback()(&bi);
		}
		else 
		{
			BuildBoneMatrix(bd, bi, parent, channel_mask);
			if (bi.callback())
			{
				bi.callback()(&bi);
			}
		}
		bi.mRenderTransform.mul_43(bi.mTransform, bd->m2b_transform);
	}
}

void CDS0_Kinematics::BuildBoneMatrix(const CBoneData* bd, CBoneInstance& bi, const Fmatrix* parent, u8 mask_channel)
{
	bi.mTransform.mul_43(*parent, bd->bind_transform);
}

void CDS0_Kinematics::BoneChain_Calculate(const CBoneData* bd, CBoneInstance& bi, u8 channel_mask, bool ignore_callbacks)
{
	u16 SelfID = bd->GetSelfID();
	//CBlendInstance& BLEND_INST	= LL_GetBlendInstance(SelfID);
	//CBlendInstance::BlendSVec &Blend = BLEND_INST.blend_vector();
//ignore callbacks
	BoneCallback bc = bi.callback();
	BOOL		 ow = bi.callback_overwrite();
	if (ignore_callbacks)
	{
		bi.set_callback(bi.callback_type(), 0, bi.callback_param(), 0);

	}
	if (SelfID == LL_GetBoneRoot())
	{
		CLBone(bd, bi, &Fidentity, channel_mask);
		//restore callback	
		bi.set_callback(bi.callback_type(), bc, bi.callback_param(), ow);
		return;
	}
	u16 ParentID = bd->GetParentID();
	R_ASSERT(ParentID != BI_NONE);
	CBoneData* ParrentDT = &LL_GetData(ParentID);
	CBoneInstance parrent_bi = LL_GetBoneInstance(ParentID);
	BoneChain_Calculate(ParrentDT, parrent_bi, channel_mask, ignore_callbacks);
	CLBone(bd, bi, &parrent_bi.mTransform, channel_mask);
	//restore callback
	bi.set_callback(bi.callback_type(), bc, bi.callback_param(), ow);
}

CDS0_SkeletonX* CDS0_Kinematics::LL_GetChild(u32 idx)
{
	IRenderVisual* V = children[idx];
	CDS0_SkeletonX* B = dynamic_cast<CDS0_SkeletonX*>(V);
	return B;
}

void CDS0_Kinematics::OnCalculateBones()
{
}

void CDS0_Kinematics::Visibility_Update()
{
	Update_Visibility = FALSE;
	// check visible
	for (u32 c_it = 0; c_it < children.size(); c_it++) 
	{
		CDS0_SkeletonX* _c = dynamic_cast<CDS0_SkeletonX*>	(children[c_it]); VERIFY(_c);
		if (!_c->has_visible_bones()) 
		{
			// move into invisible list
			children_invisible.push_back(children[c_it]);
			swap(children[c_it], children.back());
			children.pop_back();
			Update_Visibility = TRUE;
		}
	}

	// check invisible
	for (u32 _it = 0; _it < children_invisible.size(); _it++)
	{
		CDS0_SkeletonX* _c = dynamic_cast<CDS0_SkeletonX*>(children_invisible[_it]); VERIFY(_c);
		if (_c->has_visible_bones())
		{
			// move into visible list
			children.push_back(children_invisible[_it]);
			swap(children_invisible[_it], children_invisible.back());
			children_invisible.pop_back();
			Update_Visibility = TRUE;
		}
	}
}

void	CDS0_Kinematics::IBoneInstances_Create()
{
	// VERIFY2				(bones->size() < 64, "More than 64 bones is a crazy thing!");
	size_t size = bones->size();
	bone_instances = xr_alloc<CBoneInstance>(size);
	for (size_t i = 0; i < size; i++)
		bone_instances[i].construct();
}

void	CDS0_Kinematics::IBoneInstances_Destroy()
{
	if (bone_instances) {
		xr_free(bone_instances);
		bone_instances = NULL;
	}
}


shared_str CDS0_Kinematics::getDebugName()
{
	return CDS0_FHierrarhyVisual::getDebugName();
}

CDS0_Kinematics::~CDS0_Kinematics()
{

	IBoneInstances_Destroy();
	// wallmarks
	if (m_lod)
	{
		xr_delete(m_lod);
	}
}

CDS0_Kinematics::CDS0_Kinematics()
{
	Update_Callback = 0;
#ifdef DEBUG
	dbg_single_use_marker = FALSE;
#endif

	m_is_original_lod = false;
}

void CDS0_Kinematics::Bone_Calculate(CBoneData* bd, Fmatrix* parent)
{
	u16							SelfID = bd->GetSelfID();
	CBoneInstance& BONE_INST = LL_GetBoneInstance(SelfID);
	CLBone(bd, BONE_INST, parent, u8(-1));
	// Calculate children
	for (xr_vector<CBoneData*>::iterator C = bd->children.begin(); C != bd->children.end(); C++)
		Bone_Calculate(*C, &BONE_INST.mTransform);
}

void CDS0_Kinematics::Bone_GetAnimPos(Fmatrix& pos, u16 id, u8 channel_mask, bool ignore_callbacks)
{
	R_ASSERT(id < LL_BoneCount());
	CBoneInstance bi = LL_GetBoneInstance(id);
	Fvector last_c = bi.mTransform.c;
	BoneChain_Calculate(&LL_GetData(id), bi, channel_mask, ignore_callbacks);
#ifndef MASTER_GOLD
	R_ASSERT(_valid(bi.mTransform));
#endif
	pos.set(bi.mTransform);
	pos.c.set(last_c);
}

bool CDS0_Kinematics::PickBone(const Fmatrix& parent_xform, pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
{
	Fvector S, D;//normal		= {0,0,0}
	// transform ray from world to model
	Fmatrix P;	P.invert(parent_xform);
	P.transform_tiny(S, start);
	P.transform_dir(D, dir);
	for (u32 i = 0; i < children.size(); i++)
		if (LL_GetChild(i)->PickBone(r, dist, S, D, bone_id))
		{
			parent_xform.transform_dir(r.normal);
			parent_xform.transform_tiny(r.tri[0]);
			parent_xform.transform_tiny(r.tri[1]);
			parent_xform.transform_tiny(r.tri[2]);
			return true;
		}
	return false;
}

void CDS0_Kinematics::EnumBoneVertices(SEnumVerticesCallback& C, u16 bone_id)
{
	for (u32 i = 0; i < children.size(); i++)
		LL_GetChild(i)->EnumBoneVertices(C, bone_id);
}


LPCSTR CDS0_Kinematics::LL_BoneName_dbg(u16 ID)
{
	accel::iterator _I, _E = bone_map_N->end();
	for (_I = bone_map_N->begin(); _I != _E; ++_I)	if (_I->second == ID) return *_I->first;
	return 0;
}

inline bool	pred_N(const std::pair<shared_str, u32>& N, LPCSTR B)
{
	return xr_strcmp(*N.first, B) < 0;
}
u16		CDS0_Kinematics::LL_BoneID(LPCSTR B) 
{
	accel::iterator I = std::lower_bound(bone_map_N->begin(), bone_map_N->end(), B, pred_N);
	if (I == bone_map_N->end())			return BI_NONE;
	if (0 != xr_strcmp(*(I->first), B))	return BI_NONE;
	return				u16(I->second);
}
inline bool	pred_P(const std::pair<shared_str, u32>& N, const shared_str& B) 
{
	return N.first._get() < B._get();
}
u16		CDS0_Kinematics::LL_BoneID(const shared_str& B)
{
	accel::iterator I = std::lower_bound(bone_map_P->begin(), bone_map_P->end(), B, pred_P);
	if (I == bone_map_P->end())			return BI_NONE;
	if (I->first._get() != B._get())	return BI_NONE;
	return				u16(I->second);
}
inline void RecursiveBindTransform(CDS0_Kinematics* K, xr_vector<Fmatrix>& matrices, u16 bone_id, const Fmatrix& parent)
{
	CBoneData& BD = K->LL_GetData(bone_id);
	Fmatrix& BM = matrices[bone_id];
	// Build matrix
	BM.mul_43(parent, BD.bind_transform);
	for (xr_vector<CBoneData*>::iterator C = BD.children.begin(); C != BD.children.end(); C++)
		RecursiveBindTransform(K, matrices, (*C)->GetSelfID(), BM);
}

void CDS0_Kinematics::LL_GetBindTransform(xr_vector<Fmatrix>& matrices)
{
	matrices.resize(LL_BoneCount());
	RecursiveBindTransform(this, matrices, iRoot, Fidentity);
}

int CDS0_Kinematics::LL_GetBoneGroups(xr_vector<xr_vector<u16>>& groups)
{
	groups.resize(children.size());
	for (u16 bone_idx = 0; bone_idx < (u16)bones->size(); bone_idx++) {
		CBoneData* B = (*bones)[bone_idx];
		for (u32 child_idx = 0; child_idx < children.size(); child_idx++) {
			if (!B->child_faces[child_idx].empty()) {
				groups[child_idx].push_back(bone_idx);
			}
		}
	}
	return groups.size();
}

void CDS0_Kinematics::LL_SetBoneVisible(u16 bone_id, BOOL val, BOOL bRecursive)
{
	//VERIFY2(bone_id < LL_BoneCount(), make_string<const char*>("visual_name: %s, bone: %s, bone_id: %d", dbg_name.c_str(), LL_BoneName_dbg(bone_id), bone_id));
	visimask.set(bone_id, !!val);

	if (!visimask.is(bone_id)) {
		bone_instances[bone_id].mTransform.scale(0.f, 0.f, 0.f);
	}
	else {
		CalculateBones_Invalidate();
	}

	bone_instances[bone_id].mRenderTransform.mul_43(bone_instances[bone_id].mTransform, (*bones)[bone_id]->m2b_transform);
	if (bRecursive) 
	{
		for (xr_vector<CBoneData*>::iterator C = (*bones)[bone_id]->children.begin(); C != (*bones)[bone_id]->children.end(); C++) 
		{
			LL_SetBoneVisible((*C)->GetSelfID(), val, bRecursive);
		}
	}
	Visibility_Invalidate();
}

void CDS0_Kinematics::LL_SetBonesVisible(VisMask mask)
{
	visimask.zero();
	for (u32 b = 0; b < bones->size(); b++) {
		if (mask.is(b)) {
			visimask.set(b, true);
		}
		else {
			Fmatrix& A = bone_instances[b].mTransform;
			Fmatrix& B = bone_instances[b].mRenderTransform;
			A.scale(0.f, 0.f, 0.f);
			B.mul_43(A, (*bones)[b]->m2b_transform);
		}
	}
	CalculateBones_Invalidate();
	Visibility_Invalidate();
}

void CDS0_Kinematics::CalculateBones(BOOL bForceExact)
{
	// early out.
	// check if the info is still relevant
	// skip all the computations - assume nothing changes in a small period of time :)
	if (Device.dwTimeGlobal == UCalc_Time)
		return;	// early out for "fast" update

	OnCalculateBones();
	if (!bForceExact && (Device.dwTimeGlobal < (UCalc_Time + UCalc_Interval)))
		return;	// early out for "slow" update

	if (Update_Visibility)
		Visibility_Update();

	// here we have either:
	//	1:	timeout elapsed
	//	2:	exact computation required
	UCalc_Time = Device.dwTimeGlobal;

	// exact computation
	// Calculate bones
	Bone_Calculate(bones->at(iRoot), &Fidentity);

	// Calculate BOXes/Spheres if needed
	UCalc_Visibox++;
	if (UCalc_Visibox >= psSkeletonUpdate)
	{
		// mark
		UCalc_Visibox = -(::Random.randI(psSkeletonUpdate - 1));

		R_ASSERT(LL_VisibleBoneCount());

		// the update itself
		Fbox Box; 
		Box.invalidate();

		for (u32 b = 0; b < bones->size(); b++)
		{
			if (!LL_GetBoneVisible(u16(b)))
				continue;

			Fobb& obb = (*bones)[b]->obb;
			Fmatrix& Mbone = bone_instances[b].mTransform;
			Fmatrix Mbox;
			obb.xform_get(Mbox);
			Fmatrix X;
			X.mul_43(Mbone, Mbox);
			Fvector& S = obb.m_halfsize;

			Fvector P, A;
			A.set(-S.x, -S.y, -S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(-S.x, -S.y, S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(S.x, -S.y, S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(S.x, -S.y, -S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(-S.x, S.y, -S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(-S.x, S.y, S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(S.x, S.y, S.z); X.transform_tiny(P, A); Box.modify(P);
			A.set(S.x, S.y, -S.z); X.transform_tiny(P, A); Box.modify(P);
		}

		if (!bones->empty())
		{
			// previous frame we have updated box - update sphere
			Vis.box.min = (Box.min);
			Vis.box.max = (Box.max);
			Vis.box.getsphere(Vis.sphere.P, Vis.sphere.R);
		}
#ifdef DEBUG
		// Validate
		VERIFY3(_valid(Vis.box.min) && _valid(Vis.box.max), "Invalid bones-xform in model", getDebugName().c_str());
		if (Vis.sphere.R > 1000.f)
		{
			for (u16 ii = 0; ii < LL_BoneCount(); ++ii) {
				Fmatrix tr;
				tr = LL_GetTransform(ii);
				Msg("bone %s", LL_BoneName_dbg(ii));
				Log("bone_matrix", tr);
			}
			Log("end-------");
		}
		VERIFY3(Vis.sphere.R < 1000.f, "Invalid bones-xform in model", getDebugName().c_str());
#endif
	}

	//
	if (Update_Callback)	Update_Callback(this);
}

void CDS0_Kinematics::CalculateBones_Invalidate()
{
	UCalc_Time = 0x0;
	UCalc_Visibox = psSkeletonUpdate;
}
