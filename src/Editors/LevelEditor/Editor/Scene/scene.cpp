#include "stdafx.h"
#include "lephysics.h"
#include "../xrEngine/xr_input.h"

EScene* Scene;

st_LevelOptions::st_LevelOptions()
{
	Reset();
}

void st_LevelOptions::Reset()
{
	m_FNLevelPath		= "level";
	m_LevelPrefix		= "level_prefix";
	m_LightHemiQuality	= 3;
	m_LightSunQuality	= 3;
	m_BOPText			= "";
	m_map_version		= "1.0";
	m_BuildParams.Init	();
	m_BuildParams.setHighQuality();
	m_mapUsage.SetDefaults	();
}

void st_LevelOptions::SetCustomQuality()
{
	m_BuildParams.m_quality	= ebqCustom;
}

void st_LevelOptions::SetDraftQuality()
{
	m_BuildParams.setDraftQuality();
	m_LightHemiQuality	= 0;
	m_LightSunQuality	= 0;
}

void st_LevelOptions::SetHighQuality()
{
	m_BuildParams.setHighQuality();
	m_LightHemiQuality	= 3;
	m_LightSunQuality	= 3;
}


#define MAX_VISUALS 16384
#ifdef USE_ARENA_ALLOCATOR
extern char* s_fake_array;
#endif
EScene::EScene()
{
#ifdef USE_ARENA_ALLOCATOR
	s_fake_array = new char(64 * 1024 * 1024);
#endif
	m_Valid = false;
	m_Locked = 0;

	for (int i=0; i<OBJCLASS_COUNT; i++)
		m_SceneTools.insert(std::make_pair((ObjClassID)i,(ESceneToolBase*)NULL));
	g_SpatialSpace = new ISpatial_DB();
	g_SpatialSpacePhysic = new ISpatial_DB();
	// first init scene graph for objects
   // mapRenderObjects.init(MAX_VISUALS);
// 	Build options
	m_SummaryInfo	= 0;
	//ClearSnapList	(false);
//   g_frmConflictLoadObject 		= new TfrmAppendObjectInfo((TComponent*)NULL);


}

EScene::~EScene()
{
	//xr_delete(g_frmConflictLoadObject);
	xr_delete(g_SpatialSpace);
	xr_delete(g_SpatialSpacePhysic);

	VERIFY( m_Valid == false );
	m_ESO_SnapObjects.clear	();

#ifdef USE_ARENA_ALLOCATOR
	xr_free(s_fake_array);
#endif
}

void EScene::OnCreate()
{
	CreateSceneTools		();
	
	m_LastAvailObject 		= 0;
	m_LevelOp.Reset			();
	ELog.Msg				( mtInformation, "Scene: initialized" );
	m_Valid 				= true;
	m_RTFlags.zero			();
	ExecCommand				(COMMAND_UPDATE_CAPTION);
	//m_SummaryInfo 			= TProperties::CreateForm("Level Summary Info", 0, alNone, 0,0,0, TProperties::plFolderStore|TProperties::plItemFolders);
}

void EScene::OnDestroy()
{
	g_scene_physics.DestroyAll();

	//TProperties::DestroyForm(m_SummaryInfo);
	Unload					(FALSE);
	UndoClear				();
	ELog.Msg				( mtInformation, "Scene: cleared" );
	m_LastAvailObject 		= 0;
	m_Valid 				= false;
	DestroySceneTools		();
	
}

void EScene::AppendObject( CCustomObject* object, bool bUndo )
{
	VERIFY			  	(object);
	VERIFY				(m_Valid);

	switch (object->FClassID)
	{
	case OBJCLASS_SCENEOBJECT:
		m_RTFlags.set(flIsBuildedCForm, FALSE);
		UI->RedrawScene();
		break;

	case OBJCLASS_SPAWNPOINT:
		CSpawnPoint* Spawn = dynamic_cast<CSpawnPoint*>(object);
		if (Spawn && Spawn->IsGraphPoint())
		{
			m_RTFlags.set(flIsBuildedGameGraph, FALSE);
			UI->RedrawScene();
			break;
		}
		break;
	}

	ESceneCustomOTool* mt	= GetOTool(object->FClassID);
	VERIFY3(mt,"Can't find Object Tools:",GetTool(object->FClassID)->ClassDesc());
	mt->_AppendObject	(object);
	UI->UpdateScene		();
	if (bUndo){	
		object->Select	(true);
		UndoSave();
	}
}

bool EScene::RemoveObject( CCustomObject* object, bool bUndo, bool bDeleting )
{
	VERIFY				(object);
	VERIFY				(m_Valid);

	switch (object->FClassID)
	{
	case OBJCLASS_SCENEOBJECT:
		m_RTFlags.set(flIsBuildedCForm, FALSE);
		UI->RedrawScene();
		break;
	case OBJCLASS_SPAWNPOINT:
		CSpawnPoint* Spawn = dynamic_cast<CSpawnPoint*>(object);
		if (Spawn && Spawn->IsGraphPoint())
		{
			m_RTFlags.set(flIsBuildedGameGraph, FALSE);
			UI->RedrawScene();
			break;
		}
		break;
	}

	ESceneCustomOTool* mt 	= GetOTool(object->FClassID);
	if (mt&&mt->IsEditable())
	{
		mt->_RemoveObject(object);
		// signal everyone "I'm deleting"
//        if (object->ClassID==OBJCLASS_SCENEOBJECT)
		{
			m_ESO_SnapObjects.remove			(object);

			SceneToolsMapPairIt _I = m_SceneTools.begin();
			SceneToolsMapPairIt _E = m_SceneTools.end();
			for (; _I!=_E; _I++){
				ESceneToolBase* mt = _I->second;
				if (mt)
					mt->OnObjectRemove(object, bDeleting);
			}
			UpdateSnapList						();
		}
		UI->UpdateScene	();
	}
	if (bUndo)		   	UndoSave();
	return true;
}

void EScene::BeforeObjectChange( CCustomObject* object )
{
	VERIFY				(object);
	VERIFY				(m_Valid);

	ESceneCustomOTool* mt 	= GetOTool(object->FClassID);
	if (mt&&mt->IsEditable()){
		SceneToolsMapPairIt _I = m_SceneTools.begin();
		SceneToolsMapPairIt _E = m_SceneTools.end();
		for (; _I!=_E; _I++){
			ESceneToolBase* mt 		= _I->second;
			if (mt)
				mt->OnBeforeObjectChange(object);
		}
		UI->UpdateScene	();
	}
}

int EScene::MultiRenameObjects()
{
	int cnt						= 0;
	
	if (LTools->GetTarget()==OBJCLASS_DUMMY){
		SceneToolsMapPairIt t_it 	= m_SceneTools.begin();
		SceneToolsMapPairIt t_end 	= m_SceneTools.end();
		for (; t_it!=t_end; t_it++)
		{
			ESceneCustomOTool* ot	= dynamic_cast<ESceneCustomOTool*>(t_it->second);
			if (ot&&(t_it->first!=OBJCLASS_DUMMY))
				cnt					+= ot->MultiRenameObjects	();
		}
	}else{
		ESceneCustomOTool* ot		= GetOTool(LTools->GetTarget());
		if (ot) cnt					+= ot->MultiRenameObjects	();
	}
	return cnt;
}

void EScene::OnFrame( float dT )
{
	if(!valid()) return;
	if(locked()) return;

	SceneToolsMapPairIt t_it = m_SceneTools.begin();
	SceneToolsMapPairIt t_end = m_SceneTools.end();

	for(; t_it != t_end; t_it++) {
		if(t_it->second && t_it->second->IsEnabled() && t_it->second->IsVisible()) {
			t_it->second->OnFrame();
		}
	}

	if(m_RTFlags.test(flUpdateSnapList))
		UpdateSnapListReal();

	if(IsPlayInEditor()) 
	{
		if(pInput->iGetAsyncKeyState(SDL_SCANCODE_LALT)) 
		{
			if (pInput->IsAcquire)
			{
				pInput->unacquire();
				pInput->KeyboardButtonUpdate(SDL_SCANCODE_LALT, false);
				UI->IsEnableInput = true;
				ShowCursor(TRUE);
			}
		}
	}

	if(m_RTFlags.test(flIsStopPlayInEditor))
	{
		m_RTFlags.set(flIsStopPlayInEditor, FALSE);
		if(IsPlayInEditor())
		{
			ShowCursor(TRUE);
			pInput->unacquire();
			SDL_WarpMouseInWindow(g_AppInfo.Window, 
			Device.TargetWidth / 2, Device.TargetHeight / 2);
			g_pGameLevel->IR_Release();
			Device.seqParallel.clear();
			g_pGameLevel->net_Stop();
			Device.seqParallel.clear();
			DEL_INSTANCE(g_pGameLevel);
			DEL_INSTANCE(g_hud);
			GetTool(OBJCLASS_SPAWNPOINT)->m_EditFlags.set(ESceneToolBase::flVisible, true);
			UI->RedrawScene();
		}
	}
}

void EScene::Reset()
{
	// unload scene
	Unload				(FALSE);
	// reset tools
	SceneToolsMapPairIt t_it 	= m_SceneTools.begin();
	SceneToolsMapPairIt t_end 	= m_SceneTools.end();
	for (; t_it!=t_end; t_it++)
		if (t_it->second&&t_it->first!=OBJCLASS_DUMMY)
			t_it->second->Reset	();
	g_scene_physics.UpdateLevelCollision();
}

void EScene::Unload		(BOOL bEditableOnly)
{
	m_LastAvailObject 	= 0;
	Clear				(bEditableOnly);
	//if (m_SummaryInfo) 	m_SummaryInfo->HideProperties();
}

ECORE_API xrGUID generate_guid();
void EScene::Clear(BOOL bEditableToolsOnly)
{
	// clear snap
	ClearSnapList(false);
	// clear scene tools
	SceneToolsMapPairIt t_it = m_SceneTools.begin();
	SceneToolsMapPairIt t_end = m_SceneTools.end();
	for (; t_it != t_end; t_it++)
		if (t_it->second && t_it->first != OBJCLASS_DUMMY) {
			if (!bEditableToolsOnly || (bEditableToolsOnly && t_it->second->IsEditable())) {
				t_it->second->Clear();
			}
		}

	Tools->ClearDebugDraw();

	m_RTFlags.set(flRT_Unsaved | flRT_Modified, FALSE);

	m_GUID = generate_guid();
	string256 Data = {};
	sprintf(Data, "\\\\%s\\%s", Core.CompName, Core.UserName);

	m_OwnerName = Data;
	m_CreateTime = time(NULL);

	m_SaveCache.free();
	m_cfrom_builder.clear();
	m_level_graph.clear();
	m_game_graph.clear();
	m_RTFlags.set(flIsBuildedAIMap | flIsBuildedGameGraph | flIsBuildedCForm, FALSE);

	SDL_SetWindowTitle(g_AppInfo.Window, "IXR: Level Editor");
}

const Fvector& EScene::GetCameraPosition() const
{
	return EDevice->m_Camera.GetPosition();
}

bool EScene::GetBox(Fbox& box, ObjClassID classfilter)
{
	return GetBox(box,ListObj(classfilter));
}


bool EScene::GetBox(Fbox& box, ObjectList& lst)
{
	box.invalidate();
	bool bRes=false;

	for(auto Obj : lst)
	{
		Fbox bb;

		if (!Obj->GetBox(bb))
			continue;

		if (bb.max.x > 100000.0f || bb.max.y > 100000.0f || bb.max.z > 100000.0f) 
		{
			ELog.Msg(mtError, "ERROR: Bounding box [%s]", Obj->GetName());
			continue;
		}

		box.modify(bb.min);
		box.modify(bb.max);
		bRes = true;
	}

	return bRes;
}


void EScene::Modified()
{
	switch (LTools->CurrentClassID())
	{
	case OBJCLASS_SPAWNPOINT:
	{
		ObjectList lst;
		if (Scene->GetQueryObjects(lst, LTools->CurrentClassID(), 1, -1, 0))
		{
			for (CCustomObject* Obj : lst)
			{
				CSpawnPoint* Spawn = dynamic_cast<CSpawnPoint*>(Obj);
				if (Spawn&&Spawn->IsGraphPoint())
				{
					m_RTFlags.set(flIsBuildedGameGraph, FALSE);
					break;
				}
			}
		}
	}
		break;
	case OBJCLASS_AIMAP:
		m_RTFlags.set(flIsBuildedAIMap, FALSE);
		break;
	case OBJCLASS_SCENEOBJECT:
		m_RTFlags.set(flIsBuildedCForm, FALSE);
		break;
	case OBJCLASS_DUMMY:
		m_RTFlags.set(flIsBuildedCForm, FALSE);
		m_RTFlags.set(flIsBuildedAIMap, FALSE);
		m_RTFlags.set(flIsBuildedGameGraph, FALSE);
		break;
	}
	m_RTFlags.set(flRT_Modified|flRT_Unsaved,TRUE);
	g_scene_physics.OnSceneModified();
	ExecCommand(COMMAND_UPDATE_CAPTION);
	UIObjectList::Refresh();
}

bool EScene::IsUnsaved()
{
	return (m_RTFlags.is(flRT_Unsaved) && (ObjCount()||!Tools->GetEditFileName().empty()));
}
bool EScene::IsModified()
{
	return (m_RTFlags.is(flRT_Modified));
}

bool EScene::IfModified()
{
	if (locked()){ 
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return false;
	}
	if (m_RTFlags.is(flRT_Unsaved) && (ObjCount()||!Tools->GetEditFileName().empty())){
		int mr = ELog.DlgMsg(mtConfirmation, "The scene has been modified. Do you want to save your changes?");
		switch(mr){
		case mrYes: if (!ExecCommand(COMMAND_SAVE)) return false; break;
		case mrNo:{ 
			m_RTFlags.set(flRT_Unsaved,FALSE); 
			ExecCommand	(COMMAND_UPDATE_CAPTION);
		}break;
		case mrCancel: return false;
		}
	}
	return true;
}

void EScene::OnObjectsUpdate()
{
	SceneToolsMapPairIt t_it 	= m_SceneTools.begin();
	SceneToolsMapPairIt t_end 	= m_SceneTools.end();
	for (; t_it!=t_end; t_it++)
		if (t_it->second)		t_it->second->OnSceneUpdate();
}

void EScene::OnDeviceCreate()
{
	SceneToolsMapPairIt t_it 	= m_SceneTools.begin();
	SceneToolsMapPairIt t_end 	= m_SceneTools.end();
	for (; t_it!=t_end; t_it++)
		if (t_it->second)		t_it->second->OnDeviceCreate();
}

void EScene::OnDeviceDestroy()
{
	SceneToolsMapPairIt t_it 	= m_SceneTools.begin();
	SceneToolsMapPairIt t_end 	= m_SceneTools.end();
	for (; t_it!=t_end; t_it++)
		if (t_it->second)		t_it->second->OnDeviceDestroy();
}

void EScene::OnShowHint(AStringVec& dest)
{
	CCustomObject* obj = RayPickObject(flt_max,UI->m_CurrentRStart,UI->m_CurrentRDir,LTools->CurrentClassID(),0,0);
	if (obj) obj->OnShowHint(dest);
}

bool EScene::ExportGame(SExportStreams* F)
{
	bool bres = true;
	SceneToolsMapPairIt t_it 	= m_SceneTools.begin();
	SceneToolsMapPairIt t_end 	= m_SceneTools.end();
	for (; t_it!=t_end; t_it++)
		if (t_it->second)		if (!t_it->second->ExportGame(F)) bres=false;
	return bres;
}

bool EScene::Validate(bool bNeedOkMsg, bool bTestPortal, bool bTestHOM, bool bTestGlow, bool bTestShaderCompatible, bool bFullTest)
{
	bool bRes = true;
	SceneToolsMapPairIt t_it 	= m_SceneTools.begin();
	SceneToolsMapPairIt t_end 	= m_SceneTools.end();
	for (; t_it!=t_end; t_it++){
		if (t_it->second){
			if (!t_it->second->Validate(bFullTest)){
				ELog.Msg(mtError,"ERROR: Validate '%s' failed!",t_it->second->ClassDesc());
				bRes = false;
			}
		}
	}

	if (bTestPortal){
		if (Scene->ObjCount(OBJCLASS_SECTOR)||Scene->ObjCount(OBJCLASS_PORTAL))
			if (!PortalUtils.Validate(true))
				bRes = false;
	}
	if (bTestHOM){
		bool bHasHOM=false;
		ObjectList& lst = ListObj(OBJCLASS_SCENEOBJECT);
		for(ObjectIt it=lst.begin();it!=lst.end();it++){
			CEditableObject* O = ((CSceneObject*)(*it))->GetReference(); R_ASSERT(O);
			if (O->m_objectFlags.is(CEditableObject::eoHOM)){ bHasHOM = true; break; }
		}
		if (!bHasHOM)
			Msg("!Level doesn't contain HOM objects!");
//.			if (mrNo==ELog.DlgMsg(mtConfirmation,mbYes |mbNo,"Level doesn't contain HOM.\nContinue anyway?"))
//.				return false;
	}
	if (ObjCount(OBJCLASS_SPAWNPOINT)==0){
		ELog.Msg(mtError,"*ERROR: Can't find any Spawn Object.");
		bRes = false;
	}
/* St4lker0k765: what's the point of these checks?
	if (ObjCount(OBJCLASS_LIGHT) == 0) {
		ELog.Msg(mtError,"*ERROR: Can't find any Light Object.");
		bRes = false;
	}*/
	if (ObjCount(OBJCLASS_SCENEOBJECT)==0){
		ELog.Msg(mtError,"*ERROR: Can't find any Scene Object.");
		bRes = false;
	}
/*	if (bTestGlow)
	{
		if (ObjCount(OBJCLASS_GLOW)==0){
			ELog.Msg(mtError,"*ERROR: Can't find any Glow Object.");
			bRes = false;
		}
	}*/
	if (FindDuplicateName()){
		ELog.Msg(mtError,"*ERROR: Found duplicate object name.");
		bRes = false;
	}
	
	if (bTestShaderCompatible){
		bool res = true;
		ObjectList& lst = ListObj(OBJCLASS_SCENEOBJECT);
		using EOSet = xr_set<CEditableObject*>;
		EOSet objects;
		int static_obj = 0; 
		for(ObjectIt it=lst.begin();it!=lst.end();it++)
		{
			CSceneObject* S = (CSceneObject*)(*it);
			if (S->IsStatic()||S->IsMUStatic()){
				static_obj++;
				CEditableObject* O = ((CSceneObject*)(*it))->GetReference(); R_ASSERT(O);
				if (objects.find(O)==objects.end()){
					if (!O->CheckShaderCompatible()) res = false;
					objects.insert(O);
				}
			}
		}
		if (!res){ 
			ELog.Msg	(mtError,"*ERROR: Scene has non compatible shaders. See log.");
			bRes = false;
		}
		if (0==static_obj){ 
			ELog.Msg	(mtError,"*ERROR: Can't find static geometry.");
			bRes = false;
		}
	}
	
	if (!SndLib->Validate()) 
		bRes = false;

	{
		ObjectList& lst = ListObj(OBJCLASS_PS);
		for(ObjectIt it=lst.begin();it!=lst.end();it++){
			EParticlesObject* S = (EParticlesObject*)(*it);
			if (!S->GetParticles()){
				ELog.Msg(mtError,"*ERROR: Particle System has no reference.");
				bRes = false;
			}
		}
	}
	
	if (bRes){
		if (bNeedOkMsg) ELog.DlgMsg(mtInformation,"Validation OK!");
	}else{
		ELog.DlgMsg(mtInformation,"Validation FAILED!");
	}
	return bRes;
}

xr_string EScene::LevelPath()
{
	string_path path;
	if (m_LevelOp.m_FNLevelPath.size()){
		FS.update_path	(path,"$level$",m_LevelOp.m_FNLevelPath.c_str());
		strcat(path,"\\");
	}
	return xr_string(path);
}

void EScene::SelectLightsForObject(CCustomObject* obj)
{
	ESceneCustomOTool* t 			= Scene->GetOTool(OBJCLASS_LIGHT);
	if(!t)
		return;

	ESceneLightTool* lt 		= dynamic_cast<ESceneLightTool*>(t);
	VERIFY						(lt);
	lt->SelectLightsForObject	(obj);
}

void EScene::HighlightTexture(LPCSTR t_name, bool allow_ratio, u32 t_width, u32 t_height, bool leave_previous)
{
	if (!leave_previous)
		Tools->ClearDebugDraw();

	SceneToolsMapPairIt t_it 	= m_SceneTools.begin();
	SceneToolsMapPairIt t_end 	= m_SceneTools.end();

	for (; t_it!=t_end; ++t_it)
		if (t_it->second)		t_it->second->HighlightTexture(t_name,allow_ratio,t_width,t_height,!leave_previous);

	UI->RedrawScene				();
}

xr_token		js_token	[ ]={
	{ "1 - Low",			1	},
	{ "4 - Medium",			4	},
	{ "9 - High",			9	},
	{ 0,					0 	}
};

void EScene::OnBuildControlClick	(ButtonValue* V, bool& bModif, bool& bSafe)
{
	switch (V->btn_num){
	case 0: m_LevelOp.SetDraftQuality();	break;
	case 1: m_LevelOp.SetHighQuality();		break;
	case 2: m_LevelOp.SetCustomQuality();	break;
	}
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void EScene::OnRTFlagsChange	(PropValue* sender)
{
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void EScene::OnNameChange(PropValue* sender)
{
	m_RTFlags.set(flIsBuildedGameGraph, false); m_game_graph.clear();
	UI->RedrawScene();
}

// TODO: Fix this shit (it doesn't show up in LE props)
void EScene::FillProp(LPCSTR pref, PropItemVec& items, ObjClassID cls_id)
{
	PHelper().CreateCaption		(items,PrepareKey(pref,"Scene\\Name"),			LTools->m_LastFileName.c_str());

	PHelper().CreateRText		(items,PrepareKey(pref,"Scene\\Name prefix"),	&m_LevelOp.m_LevelPrefix);

	PropValue* V;
	auto NaneProp = PHelper().CreateRText		(items,PrepareKey(pref,"Scene\\Build options\\Level path"),		&m_LevelOp.m_FNLevelPath);
	NaneProp->OnChangeEvent.bind(this, &EScene::OnNameChange);
	PHelper().CreateRText		(items,PrepareKey(pref,"Scene\\Build options\\Custom data"),	&m_LevelOp.m_BOPText);
	PHelper().CreateRText		(items,PrepareKey(pref,"Scene\\Map version"),					&m_LevelOp.m_map_version);

	m_LevelOp.m_mapUsage.FillProp("Scene\\Usage", items);

	// common
	ButtonValue* B;
	B=PHelper().CreateButton	(items,PrepareKey(pref,"Scene\\Build options\\Quality"), "Draft,High,Custom",0);
	B->OnBtnClickEvent.bind		(this,&EScene::OnBuildControlClick);

	BOOL enabled				= (m_LevelOp.m_BuildParams.m_quality==ebqCustom);
	V=PHelper().CreateU8		(items,PrepareKey(pref,"Scene\\Build options\\Lighting\\Hemisphere quality [0-3]"),	&m_LevelOp.m_LightHemiQuality,	0,3);		V->Owner()->Enable(enabled);
	V=PHelper().CreateU8		(items,PrepareKey(pref,"Scene\\Build options\\Lighting\\Sun shadow quality [0-3]"),	&m_LevelOp.m_LightSunQuality,	0,3);       V->Owner()->Enable(enabled);

	// Build Options
	// Normals & optimization
	V=PHelper().CreateFloat		(items,PrepareKey(pref,"Scene\\Build options\\Optimizing\\Normal smooth angle"), 	&m_LevelOp.m_BuildParams.m_sm_angle,					0.f,180.f);	V->Owner()->Enable(enabled);
	V=PHelper().CreateFloat		(items,PrepareKey(pref,"Scene\\Build options\\Optimizing\\Weld distance (m)"),		&m_LevelOp.m_BuildParams.m_weld_distance,				0.f,1.f,0.001f,4);	V->Owner()->Enable(enabled);

	// Light maps
	V=PHelper().CreateFloat		(items,PrepareKey(pref,"Scene\\Build options\\Lighting\\Pixel per meter"),			&m_LevelOp.m_BuildParams.m_lm_pixels_per_meter,			0.f,20.f);	V->Owner()->Enable(enabled);
	V=PHelper().CreateU32		(items,PrepareKey(pref,"Scene\\Build options\\Lighting\\Error (LM collapsing)"), 	&m_LevelOp.m_BuildParams.m_lm_rms,						0,255);		V->Owner()->Enable(enabled);
	V=PHelper().CreateU32		(items,PrepareKey(pref,"Scene\\Build options\\Lighting\\Error (LM zero)"),			&m_LevelOp.m_BuildParams.m_lm_rms_zero,					0,255);		V->Owner()->Enable(enabled);
	V=PHelper().CreateToken32	(items,PrepareKey(pref,"Scene\\Build options\\Lighting\\Jitter samples"),			&m_LevelOp.m_BuildParams.m_lm_jitter_samples, 			js_token);	V->Owner()->Enable(enabled);
	
	// tools options
	{
		SceneToolsMapPairIt _I 			= FirstTool();
		SceneToolsMapPairIt _E			= LastTool();
		for(; _I!=_E; _I++)
		{
			ESceneToolBase* mt		= _I->second;
			if((_I->first!=OBJCLASS_DUMMY) && mt)
			{
				mt->FillProp			(mt->ClassDesc(), items);
			}
		}
	}
}

void EScene::FillPropObjects(LPCSTR pref, PropItemVec& items, ObjClassID cls_id)
{
	if (OBJCLASS_DUMMY == cls_id)
	{
		SceneToolsMapPairIt _I = FirstTool();
		SceneToolsMapPairIt _E = LastTool();
		for (; _I != _E; _I++)
		{
			ESceneToolBase* mt = _I->second;
			if ((_I->first != OBJCLASS_DUMMY) && mt)
			{
				mt->FillPropObjects(mt->ClassDesc(), items);
			}
		}
	}
	else {
		ESceneToolBase* mt = GetTool(cls_id);
		if (mt)
		{
			mt->FillPropObjects("", items);
		}
	}
}

void EScene::Play()
{
	if (IsPlayInEditor())
		return;
	if (!BuildSpawn())
		return;

	pInput->acquire();
	UI->IsEnableInput = false;

	g_pGamePersistent->m_game_params.reset();
	g_pGamePersistent->m_game_params.m_e_game_type = eGameIDNoGame;
	g_hud = (CCustomHUD*)NEW_INSTANCE(CLSID_HUDMANAGER);
	g_pGameLevel = (IGame_Level*)NEW_INSTANCE(CLSID_EDITOR_LEVEL);
	g_pGameLevel->net_Start("all/single/alife/new", "localhost");
	g_pGameLevel->LoadEditor(m_LevelOp.m_FNLevelPath);
	g_pGameLevel->IR_Capture();
	GetTool(OBJCLASS_SPAWNPOINT)->m_EditFlags.set(ESceneToolBase::flVisible, false);
	ShowCursor(FALSE);
}

bool EScene::IsPlayInEditor()
{
	return g_pGameLevel;
}

void EScene::Stop()
{
	if (!IsPlayInEditor())
		return;

	UI->IsEnableInput = true;
	pInput->unacquire();

	::Sound->set_geometry_env(nullptr);
	::Sound->set_geometry_som(nullptr);

	Console->Hide();
	m_RTFlags.set(flIsStopPlayInEditor, TRUE);

	g_pGamePersistent->Environment().Invalidate();
}

void EScene::LoadCFrom(CObjectSpace* Space, CDB::build_callback cb)
{
	m_cfrom_builder.Load(Space, cb);
}

IReader* EScene::LoadSpawn()
{
	return new IReader(m_spawn_data.pointer(), m_spawn_data.size());
}



IGameGraph* EScene::GetGameGraph()
{
	return &m_game_graph;
}


ILevelGraph* EScene::GetLevelGraph()
{
	return &m_level_graph;
}

bool EScene::BuildAIMap()
{
	if (!m_RTFlags.is(flIsBuildedAIMap))
	{
		if (!m_level_graph.build())
		{
			return false;;
		}
		UI->CloseConsole();
		m_game_graph.clear();
		m_RTFlags.set(flIsBuildedAIMap, TRUE);
		m_RTFlags.set(flIsBuildedGameGraph, FALSE);
		UI->RedrawScene();
	}
	return true;
}

bool EScene::BuildGameGraph()
{
	if (!m_RTFlags.is(flIsBuildedGameGraph))
	{
		if (m_level_graph.empty())
		{
			if (!BuildAIMap())
				return false;
		}
		UI->ShowConsole();
		if (!m_graph_builder.build_graph())
		{
			UI->CloseConsole();
			return false;
		}

		UI->CloseConsole();
		m_RTFlags.set(flIsBuildedGameGraph, TRUE);
		UI->RedrawScene();
	}
	return true;
}

bool EScene::BuildCForm()
{
	if (!m_RTFlags.is(flIsBuildedCForm))
	{
		if (!m_cfrom_builder.build())
		{
			Msg("! CForm is empty!");
			return false;
		}
		m_RTFlags.set(flIsBuildedCForm, TRUE);
		UI->RedrawScene();
	}

	if (!m_RTFlags.is(flIsBuildedSndEnv))
	{
		CMemoryWriter stream;
		Builder.PreparePath();
		xr_string lev_sound_env = Builder.MakeLevelPath("level.snd_env");
		EFS.MarkFile(lev_sound_env.c_str(), true);

		if (LSndLib->MakeEnvGeometry(stream, false))
			stream.save_to(lev_sound_env.c_str());

		m_RTFlags.set(flIsBuildedSndEnv, TRUE);
	}
	return true;
}


bool EScene::RayPick(const Fvector& start, const Fvector& dir, float& dis ,Fvector* pt, Fvector* n)
{
	return Tools->RayPick(start, dir, dis, pt, n);
}

void EScene::RegisterSubstObjectName(const xr_string& _from, const xr_string& _to)
{
	xr_string _tmp;
	bool b = GetSubstObjectName(_from, _tmp);
	if(b)
		Msg("! subst for '%s' already exist -'%s'",_from.c_str(), _tmp.c_str());

	TSubstPairs_it It      = m_subst_pairs.begin();
	TSubstPairs_it It_e    = m_subst_pairs.end();
	for(;It!=It_e;++It)
	{
		if(It->first == _from)
		{
			It->second = _to;
			break;
		}
	}

	if(It==It_e)
		m_subst_pairs.push_back(TSubstPair(_from, _to));
}

bool EScene::GetSubstObjectName(const xr_string& _from, xr_string& _to) const
{
	TSubstPairs_cit It      = m_subst_pairs.begin();
	TSubstPairs_cit It_e    = m_subst_pairs.end();
	for(;It!=It_e;++It)
	{
		if(It->first == _from)
		{
			_to = It->second;
			break;
		}
	}

	return (It!=It_e);
}
