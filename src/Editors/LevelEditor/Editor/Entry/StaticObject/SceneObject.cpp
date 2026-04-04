#include "stdafx.h"

#define BLINK_TIME 300.f


CSceneObject::CSceneObject(LPVOID data, const char* name):CCustomObject(data,name)
{
	Construct	(data);
}

void CSceneObject::Construct(LPVOID data)
{
	FClassID		= OBJCLASS_SCENEOBJECT;

	m_ReferenceName = "";
	m_pReference 	= 0;

	m_TBBox.invalidate();
	m_iBlinkTime	= 0;
	m_BlinkSurf		= 0;

	m_Flags.zero	();
}

void CSceneObject::ReloadReferences()
{
	if (m_Flags.test(flUseSurface))
	{
		ClearSurface();
	}
}

CSceneObject::~CSceneObject()
{
	if (m_pReference) 
	{
		for (auto _M : m_pReference->Meshes())
		{
			_M->RemoveColor(this);
		}
	}

	for (CSurface* i : m_Surfaces) { i->OnDeviceDestroy(); xr_delete(i); }
	Lib.RemoveEditObject(m_pReference);
}


void CSceneObject::EvictObject()
{
	if (m_pReference)
	{
		m_pReference->EvictObject();
	}
}


void CSceneObject::Select(bool flag)
{
	inherited::Select(flag);
	if (flag) Blink();
}


int CSceneObject::GetFaceCount()
{
	return m_pReference?m_pReference->GetFaceCount():0;
}

int CSceneObject::GetSurfFaceCount(const char* surf_name)
{
	return m_pReference?m_pReference->GetSurfFaceCount(surf_name):0;
}

int CSceneObject::GetVertexCount()
{
	return m_pReference?m_pReference->GetVertexCount():0;
}

void CSceneObject::OnUpdateTransform()
{
	inherited::OnUpdateTransform();
	// update bounding volume
	if (m_pReference){
		m_TBBox.set		(m_pReference->GetBox());
		m_TBBox.xform	(_Transform());
	}
}

bool CSceneObject::GetBox( Fbox& box ) 
{
	if (!m_pReference) return false;
	box.set(m_TBBox);
	return true;
}

bool CSceneObject::GetUTBox( Fbox& box )
{
	if (!m_pReference) return false;
	box.set(m_pReference->GetBox());
	return true;
}

bool CSceneObject::IsRender()
{
	if (!m_pReference) return false;
	return inherited::IsRender();
}


void CSceneObject::Render(int priority, bool strictB2F)
{
	if (!IsLoaded)
	{
		return;
	}

	if (m_CO_Flags.test(flObjectInGroup))
	{
		auto Tool = Scene->GetTool(OBJCLASS_GROUP);
		if (!Tool->IsVisible())
		{
			return;
		}
	}

	inherited::Render(priority, strictB2F);

	if (!m_pReference)
	{
		return;
	}

	if (Selected())
	{
		if (1 == priority)
		{
			if (false == strictB2F)
			{
				EDevice->SetShader(EDevice->m_WireShader);
				RCache.set_xform_world(_Transform());

				u32 clr = Locked() ? 0xFFFF0000 : 0xFFFFFFFF;
				DU_impl.DrawSelectionBoxB(m_pReference->GetBox(), &clr);
			}
			else
			{
				RenderBlink();
			}
		}
	}

	Scene->SelectLightsForObject(this);
	m_pReference->Render(this, _Transform(), priority, strictB2F, &m_Surfaces);
}

u32 CSceneObject::RenderPriorityMask() const
{
    return m_pReference ? m_pReference->RenderPriorityMask() : (1u << 1);
}

void CSceneObject::RenderBlink()
{
	if (m_iBlinkTime > 0)
	{
		if (m_iBlinkTime > (int)EDevice->dwTimeGlobal)
		{
			int alpha = iFloor(sqrtf(float(m_iBlinkTime - EDevice->dwTimeGlobal) / BLINK_TIME) * 64);
			m_pReference->RenderSelection(this, 0, D3DCOLOR_ARGB(alpha, 255, 255, 255));
		}
		else
		{
			m_iBlinkTime = 0;
			m_BlinkSurf = 0;
		}

		UI->RedrawScene();
	}
}

void CSceneObject::RenderSingle()
{
	if (!m_pReference) 
	{
		return;
	}

	RenderBlink();
	m_pReference->RenderSingle(this, _Transform());
}

void CSceneObject::RenderBones()
{
	if (!m_pReference) return;
	m_pReference->RenderBones(_Transform());
}

void CSceneObject::RenderEdge(CEditableMesh* mesh, u32 color)
{
	if (!m_pReference) return;

	if (::Render->occ_visible(m_TBBox))
	{
		m_pReference->RenderEdge(this, mesh, color);
	}
}

void CSceneObject::RenderSelection(u32 color)
{
	if (!m_pReference) return;
	m_pReference->RenderSelection(this, 0, color);
}

bool CSceneObject::FrustumPick(const CFrustum& frustum)
{
	if (!m_pReference) return false;
	if (::Render->occ_visible(m_TBBox))
		return m_pReference->FrustumPick(frustum, _Transform());
	return false;
}

bool CSceneObject::SpherePick(const Fvector& center, float radius)
{
	if (!m_pReference) return false;
	float fR; Fvector vC;
	m_TBBox.getsphere(vC,fR);
	float R=radius+fR;
	float dist_sqr=center.distance_to_sqr(vC);
	if (dist_sqr<R*R) return true;
	return false;
}

bool CSceneObject::RayPick(float& dist, const Fvector& S, const Fvector& D, SRayPickInfo* pinf)
{
	if (!IsLoaded && !pinf->IsForcePickup)
		return false;

	if (!m_pReference) return false;
	if (::Render->occ_visible(m_TBBox))
		if (m_pReference->RayPick(dist, S, D, _ITransform(), pinf)){
			if (pinf) pinf->s_obj = this;
			return true;
		}
	return false;
}

void CSceneObject::RayQuery(SPickQuery& pinf)
{
	if (!m_pReference) return;
	m_pReference->RayQuery(_Transform(), _ITransform(), pinf);
}

void CSceneObject::BoxQuery(SPickQuery& pinf)
{
	if (!m_pReference) return;
	m_pReference->BoxQuery(_Transform(), _ITransform(), pinf);
}

bool CSceneObject::BoxPick(const Fbox& box, SBoxPickInfoVec& pinf)
{
	if (!m_pReference) return false;
	return m_pReference->BoxPick(this, box, _ITransform(), pinf);
}

void CSceneObject::GetFullTransformToWorld( Fmatrix& m )
{
	m.set(_Transform());
}

void CSceneObject::GetFullTransformToLocal( Fmatrix& m )
{
	m.set(_ITransform());
}

CEditableObject* CSceneObject::UpdateReference()
{
	Lib.RemoveEditObject(m_pReference);
	m_pReference = (m_ReferenceName.size()) ? Lib.CreateEditObject(*m_ReferenceName) : 0;
	UpdateTransform();

	ClearSurface();

	return m_pReference;
}

CEditableObject* CSceneObject::SetReference(const char* ref_name)
{
	m_ReferenceName	= ref_name;
	return UpdateReference();
}

void CSceneObject::OnFrame()
{
	inherited::OnFrame();
	if (!m_pReference)
	{
		return;
	}
	if (m_pReference)
	{
		m_pReference->OnFrame();
	}
	for(auto& elem : m_Surfaces)
	{
		if(elem->UseShared && elem->m_pData.first != elem->m_pData.second->m_Name)
		{
			elem->m_pData.first = elem->m_pData.second->m_Name;
		}
	}

#if 0
	if (psDeviceFlags.is(rsStatistic)){
		if (IsStatic()||IsMUStatic()||Selected()){
			EDevice->EStatistic->dwLevelSelFaceCount 	+= GetFaceCount();
			EDevice->EStatistic->dwLevelSelVertexCount += GetVertexCount();
		}
	}
#endif
}

void CSceneObject::ReferenceChange(PropValue* sender)
{
	CSector* OldSector = nullptr;
	for (auto MeshObJ : m_pReference->Meshes())
	{
		OldSector = PortalUtils.FindSector(this, MeshObJ);

		if (OldSector != nullptr)
			break;
	}

	Scene->BeforeObjectChange(this);
	UpdateReference	();

	if (OldSector)
	{
		for (auto MeshObJ : m_pReference->Meshes())
		{
			OldSector->AddMesh(this, MeshObJ);
		}
	}
}

void CSceneObject::OnChangeSharedMode(PropValue* sender)
{
	for (CSurface* i : m_Surfaces)
	{
		if(i->UseShared != i->UseSharedPrev)
		{
			if(i->UseSharedPrev)
			{
				auto TempShared = i->m_pData.second;
				i->m_pData.second = new SSurfaceData(*TempShared);
			} else
			{
				auto Shared = CSharedMaterialLibrary::Instance().GetData(i->m_pData.first);
				if(!Shared)
				{
					CSharedMaterialLibrary::Instance().MakeSharedCopy(i->m_pData.second);
					Shared = CSharedMaterialLibrary::Instance().GetData(i->m_pData.first);
				}
				i->m_pDataOld = i->m_pData.second;
				i->m_pData.second = Shared;
			}
			i->UseSharedPrev = i->UseShared;
		}	
	}
	OnChangeShader(sender);
	Tools->UpdateProperties();
}

void CSceneObject::OnChangeSharedMaterial(PropValue* sender)
{
	for (CSurface* i : m_Surfaces)
	{
		if(i->m_pData.first != i->m_pData.second->m_Name)
		{
			i->m_pData.second = CSharedMaterialLibrary::Instance().GetData(i->m_pData.first);
		}
	}
	OnChangeShader(sender);
	Tools->UpdateProperties();
}

void CSceneObject::OnChangeShader(PropValue* sender)
{
	OnChangeSurface(sender);
	for (CSurface* i : m_Surfaces) { i->OnDeviceDestroy(); }
}

void CSceneObject::OnChangeSurface(PropValue* sender)
{
	m_Flags.set(flUseSurface, true);
}

bool CSceneObject::AfterEditGameMtl(PropValue* sender,shared_str&str)
{
	return str != "materials\\occ";
}

void CSceneObject::OnClickClearSurface(ButtonValue*, bool&, bool&)
{
	Scene->UndoSave();
	ClearSurface();
}

void CSceneObject::OnBatchProcessMaterial(ButtonValue* value, bool& bModif, bool& bSafe)
{
	switch(value->btn_num)
	{
	case 0: // unique
		{
			for (auto elem : m_Surfaces)
			{
				elem->UseShared = false;
			}
			break;
		}
	case 1: // shared
		{
			for (auto elem : m_Surfaces)
			{
				elem->UseShared = true;
			}
			break;
		}
	}
	OnChangeSharedMode(value);
}

void CSceneObject::FillProp(const char* pref, PropItemVec& items)
{
	static shared_str occ_name = "materials\\occ";
	inherited::FillProp(pref, items);
	PropValue* V = PHelper().CreateChoose(items, PrepareKey(pref, "Reference"), &m_ReferenceName, smObject);
	V->OnChangeEvent.bind(this, &CSceneObject::ReferenceChange);

	if (IsDynamic())
	{
		inherited::AnimationFillProp(pref, items);
	}
	
	SurfaceVec& s_lst = m_Surfaces;

	shared_str Pref1 = PrepareKey(pref, "Surfaces").c_str();
	xr_vector<CSurface*> SortedSurfaces(s_lst.begin(), s_lst.end());

	std::sort
	(
		SortedSurfaces.begin(), SortedSurfaces.end(),
		[](const CSurface* a, const CSurface* b)
		{
			return xr_strcmp(a->_Name(), b->_Name()) < 0;
		}
	);

	{
		auto BatchButton = PHelper().CreateButton(items, PrepareKey(Pref1.c_str(), "Batch Material Convert"), "All unique,All shared", 0);
		BatchButton->OnBtnClickEvent.bind(this, &CSceneObject::OnBatchProcessMaterial);
	}
	for (CSurface* s : SortedSurfaces)
	{
		shared_str Pref2 = PrepareKey(Pref1.c_str(), s->_Name()).c_str();
		if (s->_GameMtlName() != occ_name)
		{
			// TODO: Add switch option
			auto B = PHelper().CreateBool(items, PrepareKey(Pref2.c_str(), "Use shared material"), &s->UseShared);
			B->OnChangeEvent.bind(this, &CSceneObject::OnChangeSharedMode);

			if(s->UseShared)
			{
				auto SMC = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Shared Material Name"), &s->m_pData.first, smSharedMaterial);
				SMC->OnChangeEvent.bind(this, &CSceneObject::OnChangeSharedMaterial);
			}
			
			MultiChooseValue* MultiValue = PHelper().CreateChooseTexture(items, PrepareKey(Pref2.c_str(), "TextureView"));
			ChooseValue* CV = nullptr;
			
			if(s->UseShared)
			{
				CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Tex"), &s->m_pData.second->m_Texture, smDisabled);
				CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Shader"), &s->m_pData.second->m_ShaderName, smDisabled);
				CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Compile"), &s->m_pData.second->m_ShaderXRLCName, smDisabled);
				CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Mtl"), &s->m_pData.second->m_GameMtlName, smDisabled);
			} else
			{
				MultiValue->DropCallback = [this, s](const char* File)
				{
					s->m_pData.second->m_Texture = File;
					OnChangeShader(nullptr);
				};

				CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Tex"), &s->m_pData.second->m_Texture, smTexture);
				CV->OnChangeEvent.bind(this, &CSceneObject::OnChangeShader);

				CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Shader"), &s->m_pData.second->m_ShaderName, smEShader);
				CV->OnChangeEvent.bind(this, &CSceneObject::OnChangeShader);

				CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Compile"), &s->m_pData.second->m_ShaderXRLCName, smCShader);
				CV->OnChangeEvent.bind(this, &CSceneObject::OnChangeSurface);
				
				CV = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Mtl"), &s->m_pData.second->m_GameMtlName, smGameMaterial);
				CV->OnChangeEvent.bind(this, &CSceneObject::OnChangeSurface);
				CV->OnAfterEditEvent.bind(this, &CSceneObject::AfterEditGameMtl);
			}
			if(s->m_pDataOld)
			{
				xr_delete(s->m_pDataOld);
			}
		}
	}

	PHelper().CreateButton(items, PrepareKey(Pref1.c_str(), "Action"), "Clear", ButtonValue::flFirstOnly)->OnBtnClickEvent.bind(this, &CSceneObject::OnClickClearSurface);
}

bool CSceneObject::GetSummaryInfo(SSceneSummary* inf)
{
	inherited::GetSummaryInfo	(inf);
	CEditableObject* E 	= GetReference(); R_ASSERT(E);
	if (IsStatic()||IsMUStatic()){
		for(auto& elem : E->m_Surfaces)
		{
			float area			= 0.f;
			float pixel_area	= 0.f;
			for(auto& Mesh : E->Meshes())
			{
				area += Mesh->CalculateSurfaceArea(elem,true);
				pixel_area += Mesh->CalculateSurfacePixelArea(elem,true);
			}
			xr_string temp = ChangeFileExt(xr_string(elem->_Texture()), "");
			xr_strlwr(temp);
			inf->AppendTexture(temp.c_str(),SSceneSummary::sttBase,area,pixel_area,E->m_LibName.c_str());
		}
		if (m_Flags.is(CEditableObject::eoUsingLOD)){
			inf->AppendTexture(E->GetLODTextureName().c_str(),SSceneSummary::sttLOD,0,0,"$LOD$");
			inf->lod_objects.insert	(E->m_LibName.c_str());
			inf->object_lod_ref_cnt++;
		}
		if (m_Flags.is(CEditableObject::eoMultipleUsage)){
			inf->mu_objects.insert(E->m_LibName.c_str());
			inf->object_mu_ref_cnt++;
		}

		inf->face_cnt		+= E->GetFaceCount	();
		inf->vert_cnt		+= E->GetVertexCount();
	}
	if (m_Flags.is(CEditableObject::eoHOM)){
		inf->hom_face_cnt	+= E->GetFaceCount	();
		inf->hom_vert_cnt	+= E->GetVertexCount();
	}
	if (m_Flags.is(CEditableObject::eoSoundOccluder)){
		inf->snd_occ_face_cnt += E->GetFaceCount();
		inf->snd_occ_vert_cnt += E->GetVertexCount();
	}
	inf->AppendObject	(E->GetName());
	return true;
}

extern xr_token ECORE_API eo_type_token[];

void CSceneObject::OnShowHint(AStringVec& dest)
{
	inherited::OnShowHint(dest);
	dest.push_back(xr_string("Reference: ")+*m_ReferenceName);
	dest.push_back(xr_string("-------"));
	float dist			= UI->ZFar();
	SRayPickInfo pinf;
	if (m_pReference->RayPick(dist,UI->m_CurrentRStart,UI->m_CurrentRDir,_ITransform(),&pinf)){
		dest.push_back(xr_string("Object Type: ")+get_token_name(eo_type_token,pinf.e_obj->m_objectFlags.flags));
		R_ASSERT(pinf.e_mesh);
		CSurface* surf=pinf.e_mesh->GetSurfaceByFaceID(pinf.inf.id);
		dest.push_back(xr_string("Surface: ")+xr_string(surf->_Name()));
		dest.push_back(xr_string("2 Sided: ")+xr_string(surf->_flags().is(SSurfaceData::sf2Sided)?"on":"off"));
		if (pinf.e_obj->m_objectFlags.is(CEditableObject::eoSoundOccluder)){
			dest.push_back(xr_string("Game Mtl: ")+xr_string(surf->_GameMtlName()));
			int gm_id			= surf->_GameMtl(); 
			if (gm_id!=GAMEMTL_NONE_ID){ 
				SGameMtl* mtl 	=  GameMaterialLibraryEditors->GetMaterialByID(gm_id);
				string256 Data = {};
				sprintf(Data, "Occlusion Factor: %3.2f", mtl->fSndOcclusionFactor);

				if (mtl)		dest.push_back(Data);
			}
		}else if (pinf.e_obj->m_objectFlags.is(CEditableObject::eoHOM)){
		}else{
			dest.push_back(xr_string("Texture: ")+xr_string(surf->_Texture()));
			dest.push_back(xr_string("Shader: ")+xr_string(surf->_ShaderName()));
			dest.push_back(xr_string("LC Shader: ")+xr_string(surf->_ShaderXRLCName()));
			dest.push_back(xr_string("Game Mtl: ")+xr_string(surf->_GameMtlName()));
		}
	}
}


void CSceneObject::Blink(CSurface* surf)
{
	m_BlinkSurf		= surf;
	m_iBlinkTime	= EDevice->dwTimeGlobal+BLINK_TIME+EDevice->dwTimeDelta;
}

bool CSceneObject::Validate(bool bMsg)
{
	CEditableObject* E 	= GetReference(); R_ASSERT(E);
	return E->Validate();
}

void CSceneObject::ClearSurface()
{
	for (CSurface* i : m_Surfaces)
	{
		i->OnDeviceDestroy(); 
		xr_delete(i);
	}

	m_Surfaces.clear();

	if (m_pReference)
	{
		for (size_t i = 0; i < m_pReference->SurfaceCount(); i++)
		{
			CSurface* surf = new CSurface();
			surf->CopyFrom(m_pReference->Surfaces()[i]);
			m_Surfaces.push_back(surf);
			if (surf->IsVoid())
			{
				if (m_pReference->IsSkeleton())
					Engine.External.SetSkinningMode(4);

				surf->OnDeviceCreate();

				if (m_pReference->IsSkeleton())
					Engine.External.SetSkinningMode();
			}
		}
	}
	m_Flags.set(flUseSurface, 0);
	Tools->UpdateProperties();
}