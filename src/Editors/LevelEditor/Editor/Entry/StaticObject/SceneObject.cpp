#include "stdafx.h"

#include "../../../../TiramisuMaterialEditor/LegacyObjectMaterialMigration.h"
#include "../../../../xrECore/Editor/EditorRenderBackend.h"
#include "../../../UI/MaterialEditor/UIMaterialEditorForm.h"

#define BLINK_TIME 300.f

namespace
{
struct FLegacySceneMaterialMigrationState
{
	Tiramisu::Editor::TiramisuLegacyObjectMaterialMigrationService Service;
	bool InitializationAttempted = false;
};

FLegacySceneMaterialMigrationState& GetLegacySceneMaterialMigrationState()
{
	static FLegacySceneMaterialMigrationState State;
	return State;
}

void LogMaterialDiagnostics(
	const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	for (const FMaterialDiagnostic& Diagnostic :
		Diagnostics)
	{
		if (Diagnostic.Severity ==
			EMaterialDiagnosticSeverity::Info)
		{
			continue;
		}
		Msg("%c [Tiramisu material migration:%s] %s",
			Diagnostic.Severity ==
					EMaterialDiagnosticSeverity::Error
				? '!' : '~',
			Diagnostic.Code.c_str(), Diagnostic.Message.c_str());
	}
}

bool EnsureLegacySceneMaterialMigrationInitialized()
{
	FLegacySceneMaterialMigrationState& State =
		GetLegacySceneMaterialMigrationState();
	if (State.Service.IsInitialized())
		return true;
	if (State.InitializationAttempted)
		return false;
	State.InitializationAttempted = true;

	string_path MaterialRoot = {};
	FS.update_path(MaterialRoot, "$game_render_materials$", "");
	xr_vector<FMaterialDiagnostic> Diagnostics;
	const bool Initialized = State.Service.Initialize(
		std::filesystem::path(MaterialRoot), &Diagnostics);
	LogMaterialDiagnostics(Diagnostics);
	if (!Initialized)
		Msg("! Tiramisu could not initialize legacy scene material migration.");
	return Initialized;
}
} // namespace


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
	m_RenderMaterialsResolved = false;

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
		return;

	if (m_CO_Flags.test(flObjectInGroup))
	{
		auto Tool = Scene->GetTool(OBJCLASS_GROUP);
		if (!Tool->IsVisible())
		{
			return;
		}
	}

	inherited::Render(priority,strictB2F);
	if (!m_pReference) return;

	Scene->SelectLightsForObject(this);
	m_pReference->Render(_Transform(), priority, strictB2F, &m_Surfaces);
	if (Selected()){
		if (1==priority){
			if (false==strictB2F){
				EDevice->SetShader(EDevice->m_WireShader);
				RCache.set_xform_world(_Transform());
				u32 clr = Locked()?0xFFFF0000:0xFFFFFFFF;
				DU_impl.DrawSelectionBoxB(m_pReference->GetBox(),&clr);
			}else{
				RenderBlink	();
			}
		}
	}
}

void CSceneObject::RenderBlink()
{
	if (m_iBlinkTime>0){
		if (m_iBlinkTime>(int)EDevice->dwTimeGlobal){
			int alpha = iFloor(sqrtf(float(m_iBlinkTime-EDevice->dwTimeGlobal)/BLINK_TIME)*64);
			m_pReference->RenderSelection(_Transform(),0, m_BlinkSurf, D3DCOLOR_ARGB(alpha,255,255,255));
			UI->RedrawScene	();
		}else{
			m_iBlinkTime 	= 0;
			m_BlinkSurf		= 0;
		}
	}
}

void CSceneObject::RenderSingle()
{
	if (!m_pReference) 		return;
	m_pReference->RenderSingle(_Transform());
	RenderBlink				();
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
		m_pReference->RenderEdge(_Transform(), mesh, 0, color);
}

void CSceneObject::RenderSelection(u32 color)
{
	if (!m_pReference) return;
	m_pReference->RenderSelection(_Transform(),0, 0, color);
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
	if (!m_pReference) return;
	if (m_pReference) m_pReference->OnFrame();

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

void CSceneObject::OnChangeShader(PropValue* sender)
{
	OnChangeSurface(sender);
	for (CSurface* i : m_Surfaces) { i->OnDeviceDestroy(); }
}

void CSceneObject::OnChangeSurface(PropValue* sender)
{
	m_Flags.set(flUseSurface, 1);
	m_RenderMaterials.clear();
	m_RenderMaterialsResolved = false;
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

void CSceneObject::OnOpenRenderMaterial(
	ButtonValue* Sender, bool&, bool&)
{
	if (!Sender || Sender->tag >= m_RenderMaterials.size() || !MainForm)
		return;
	const char* MaterialAsset =
		m_RenderMaterials[Sender->tag].MaterialAsset.c_str();
	if (!MaterialAsset || !MaterialAsset[0])
		return;

	string_path MaterialRoot = {};
	FS.update_path(MaterialRoot, "$game_render_materials$", "");
	const std::filesystem::path MaterialPath =
		std::filesystem::path(MaterialRoot) / MaterialAsset;
	UIMaterialEditorForm* MaterialEditor =
		MainForm->GetMaterialEditorForm();
	if (!MaterialEditor ||
		!MaterialEditor->OpenInstanceFile(MaterialPath))
	{
		Msg("! Cannot open generated MaterialInstance '%s'.",
			MaterialPath.string().c_str());
	}
}

void CSceneObject::FillProp(const char* pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);
	PropValue* V = PHelper().CreateChoose(items, PrepareKey(pref, "Reference"), &m_ReferenceName, smObject);
	V->OnChangeEvent.bind(this, &CSceneObject::ReferenceChange);

	if (IsDynamic())
	{
		inherited::AnimationFillProp(pref, items);
	}

	SurfaceVec& s_lst = m_Surfaces;

	xr_vector<CSurface*> SortedSurfaces(s_lst.begin(), s_lst.end());

	std::sort
	(
		SortedSurfaces.begin(), SortedSurfaces.end(),
		[](const CSurface* a, const CSurface* b)
		{
			return xr_strcmp(a->_Name(), b->_Name()) < 0;
		}
	);

	if (GetEditorRenderBackend().GetKind() ==
		EEditorRenderBackendKind::Tiramisu)
	{
		ResolveRenderMaterials();
		const shared_str MaterialsPrefix =
			PrepareKey(pref, "Materials").c_str();
		for (CSurface* Surface : SortedSurfaces)
		{
			const shared_str SurfacePrefix =
				PrepareKey(MaterialsPrefix.c_str(), Surface->_Name()).c_str();
			const char* MaterialAsset =
				GetRenderMaterialAsset(Surface->_Name());
			PHelper().CreateCaption(items,
				PrepareKey(SurfacePrefix.c_str(), "Material Instance"),
				MaterialAsset && MaterialAsset[0]
					? MaterialAsset : "<error material>");
			PHelper().CreateCaption(items,
				PrepareKey(SurfacePrefix.c_str(), "Two Sided"),
				Surface->m_Flags.is(CSurface::sf2Sided) ? "Yes" : "No");
			for (size_t BindingIndex = 0;
				BindingIndex < m_RenderMaterials.size(); ++BindingIndex)
			{
				if (xr_strcmp(
						m_RenderMaterials[BindingIndex].SurfaceName.c_str(),
						Surface->_Name()) != 0)
				{
					continue;
				}
				ButtonValue* OpenButton = PHelper().CreateButton(items,
					PrepareKey(SurfacePrefix.c_str(), "Action"), "Open",
					ButtonValue::flFirstOnly);
				OpenButton->tag = BindingIndex;
				OpenButton->OnBtnClickEvent.bind(
					this, &CSceneObject::OnOpenRenderMaterial);
				break;
			}
		}
		return;
	}

	static shared_str occ_name = "materials\\occ";
	shared_str Pref1 = PrepareKey(pref, "Surfaces").c_str();
	for (CSurface* s : SortedSurfaces)
	{
		shared_str Pref2 = PrepareKey(Pref1.c_str(), s->_Name()).c_str();
		if (s->m_GameMtlName != occ_name)
		{
			MultiChooseValue* MultiValue = PHelper().CreateChooseTexture(items, PrepareKey(Pref2.c_str(), "TextureView"));
			MultiValue->DropCallback = [this, s](const char* File)
			{
				s->m_Texture = File;
				OnChangeShader(nullptr);
			};

			ChooseValue* Val = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Tex"), &s->m_Texture, smTexture);
			Val->OnChangeEvent.bind(this, &CSceneObject::OnChangeShader);

			Val = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Shader"), &s->m_ShaderName, smEShader);
			Val->OnChangeEvent.bind(this, &CSceneObject::OnChangeShader);

			Val = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Compile"), &s->m_ShaderXRLCName, smCShader);
			Val->OnChangeEvent.bind(this, &CSceneObject::OnChangeSurface);

			Val = MultiValue->CreateValue(PrepareKey(Pref2.c_str(), "Mtl"), &s->m_GameMtlName, smGameMaterial);
			Val->OnChangeEvent.bind(this, &CSceneObject::OnChangeSurface);
			Val->OnAfterEditEvent.bind(this, &CSceneObject::AfterEditGameMtl);
		}
	}

	PHelper().CreateButton(items, PrepareKey(Pref1.c_str(), "Action"), "Clear", ButtonValue::flFirstOnly)->OnBtnClickEvent.bind(this, &CSceneObject::OnClickClearSurface);
}

bool CSceneObject::GetSummaryInfo(SSceneSummary* inf)
{
	inherited::GetSummaryInfo	(inf);
	CEditableObject* E 	= GetReference(); R_ASSERT(E);
	if (IsStatic()||IsMUStatic()){
		for(SurfaceIt 	s_it=E->m_Surfaces.begin(); s_it!=E->m_Surfaces.end(); s_it++){
			float area			= 0.f;
			float pixel_area	= 0.f;
			for(EditMeshIt m = E->Meshes().begin();m!=E->Meshes().end();m++){
				area			+= (*m)->CalculateSurfaceArea(*s_it,true);
				pixel_area		+= (*m)->CalculateSurfacePixelArea(*s_it,true);
			}
			xr_string temp = ChangeFileExt(xr_string(*(*s_it)->m_Texture), "");
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
		dest.push_back(xr_string("2 Sided: ")+xr_string(surf->m_Flags.is(CSurface::sf2Sided)?"on":"off"));
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
	m_RenderMaterials.clear();
	m_RenderMaterialsResolved = false;

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

bool CSceneObject::ResolveRenderMaterials(const bool DeferDatabaseSave)
{
	if (m_RenderMaterialsResolved)
		return m_RenderMaterials.size() == m_Surfaces.size();
	m_RenderMaterialsResolved = true;
	m_RenderMaterials.clear();
	if (m_Surfaces.empty())
		return true;
	if (!EnsureLegacySceneMaterialMigrationInitialized())
		return false;

	xr_vector<Tiramisu::Editor::FLegacyObjectSurfaceDescriptor> Surfaces;
	Surfaces.reserve(m_Surfaces.size());
	for (const CSurface* Surface : m_Surfaces)
	{
		Tiramisu::Editor::FLegacyObjectSurfaceDescriptor Descriptor;
		if (Surface)
		{
			Descriptor.SurfaceName = Surface->_Name();
			Descriptor.ShaderName = Surface->_ShaderName();
			Descriptor.CompilerShaderName = Surface->_ShaderXRLCName();
			Descriptor.GameMaterialName = Surface->_GameMtlName();
			Descriptor.TextureName = Surface->_Texture();
			Descriptor.VertexMapName = Surface->_VMap();
			Descriptor.Flags = Surface->m_Flags.get();
			Descriptor.VertexFormat = Surface->_FVF();
			Descriptor.TwoSided =
				Surface->m_Flags.is(CSurface::sf2Sided);
		}
		Surfaces.push_back(std::move(Descriptor));
	}

	Tiramisu::Editor::FLegacyObjectMaterialMigrationResult Result =
		GetLegacySceneMaterialMigrationState().Service.Migrate(
			// A live level may contain thousands of components that reference
			// the same library object. The conversion dump owns component-level
			// provenance; the viewport migration must not duplicate it in the
			// shared database or turn first-frame lookup into quadratic work.
			DeferDatabaseSave ? xr_string_view{} :
				xr_string_view{m_ReferenceName.c_str()},
			Surfaces, DeferDatabaseSave);
	LogMaterialDiagnostics(Result.Diagnostics);
	if (!Result.Succeeded() ||
		Result.Bindings.size() != m_Surfaces.size())
	{
		Msg("! Tiramisu could not resolve render materials for legacy object '%s'.",
			m_ReferenceName.c_str());
		return false;
	}

	m_RenderMaterials.reserve(Result.Bindings.size());
	for (const Tiramisu::Editor::FLegacyObjectMaterialBinding& Binding :
		Result.Bindings)
	{
		m_RenderMaterials.push_back(
			{Binding.SurfaceName.c_str(), Binding.MaterialAsset.c_str()});
	}
	return true;
}

const char* CSceneObject::GetRenderMaterialAsset(
	const char* SurfaceName) const
{
	if (!SurfaceName)
		return nullptr;
	for (const FRenderMaterialBinding& Binding : m_RenderMaterials)
	{
		if (xr_strcmp(Binding.SurfaceName.c_str(), SurfaceName) == 0)
			return Binding.MaterialAsset.c_str();
	}
	return nullptr;
}

bool CSceneObject::FlushRenderMaterialMigration()
{
	FLegacySceneMaterialMigrationState& State =
		GetLegacySceneMaterialMigrationState();
	if (!State.Service.IsInitialized())
		return true;
	xr_vector<FMaterialDiagnostic> Diagnostics;
	const bool Flushed = State.Service.FlushDatabase(Diagnostics);
	LogMaterialDiagnostics(Diagnostics);
	return Flushed;
}
