#include "stdafx.h"
#include "Utils/Cursor3D.h"
#include "UI/UIEditLibrary.h"

#define DETACH_FRAME(a) 	if (a){ a=0; }
#define ATTACH_FRAME(a,b)	if (a){b=a;}

CLevelTool*	LTools=(CLevelTool*)Tools;

TShiftState ssRBOnly;

CLevelTool::CLevelTool()
{
	fFogness	= 0.9f;
	dwFogColor	= 0xffffffff;
	m_Flags.zero();
	m_ToolForm = 0;
	m_CompilerProcess.hProcess = 0;

	pCurTool = nullptr;
	mtPropObj = CreateEventA(nullptr, true, false, nullptr);
	thread_spawn(mtUpdateProperties, "PropertiesAsync", 1, this);
}

CLevelTool::~CLevelTool()
{
	
}

bool CLevelTool::OnCreate()
{
	inherited::OnCreate();
	target = OBJCLASS_DUMMY;
	sub_target = -1;
	pCurTool = 0;
	ssRBOnly = ssRight;
	m_Flags.set(flChangeAction, FALSE);
	m_Flags.set(flChangeTarget, FALSE);
	Scene->OnCreate();
	ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_SCENEOBJECT);
	m_Props = new UIPropertiesForm();
	m_Props->SetModifiedEvent(TOnCloseEvent(this, &CLevelTool::OnPropsModified));
	m_WorldProps = new UIPropertiesForm();
	m_WorldProps->SetModifiedEvent(TOnCloseEvent(this, &CLevelTool::OnPropsModified));

	return true;
}


void CLevelTool::OnDestroy()
{
	inherited::OnDestroy();
	xr_delete(m_Props);
	xr_delete(m_WorldProps);
	/*TfrmObjectList::DestroyForm(pObjectListForm);
	TProperties::DestroyForm(m_Props);*/
	// scene destroing
	if (pCurTool)
		pCurTool->OnDeactivate();
	Scene->OnDestroy		();
}

void CLevelTool::Reset()
{
	RealSetTarget(GetTarget(),estDefault,true);
}


bool  CLevelTool::MouseStart(TShiftState Shift)
{
	if (Scene->IsPlayInEditor())
		return false;

	inherited::MouseStart(Shift);
	if(pCurTool && pCurTool->pCurControl)
	{
		if ((pCurTool->pCurControl->Action()!=etaSelect)&&
			(!pCurTool->IsEditable() || !pCurTool->AllowMouseStart() || (pCurTool->FClassID==OBJCLASS_DUMMY)))
			return false;

		return pCurTool->pCurControl->Start(Shift);
	}
	return false;
}

void  CLevelTool::MouseMove(TShiftState Shift)
{
	inherited::MouseMove(Shift);
	if(pCurTool&&pCurTool->pCurControl)
	{
		if (HiddenMode())
			ExecCommand(COMMAND_UPDATE_PROPERTIES);

		pCurTool->pCurControl->Move(Shift);
	}
}

bool  CLevelTool::MouseEnd(TShiftState Shift)
{
	inherited::MouseEnd(Shift);
	if(pCurTool&&pCurTool->pCurControl)
	{
		if (HiddenMode())
			ExecCommand(COMMAND_UPDATE_PROPERTIES);

		return pCurTool->pCurControl->End(Shift);
	}
	return false;
}

void  CLevelTool::OnObjectsUpdate()
{
	UpdateProperties(false);
	if(pCurTool&&pCurTool->pCurControl)
		return pCurTool->OnObjectsUpdate();
}

bool  CLevelTool::HiddenMode()
{
	if(pCurTool&&pCurTool->pCurControl)
		return pCurTool->pCurControl->HiddenMode();
	return false;
}

bool  CLevelTool::KeyDown   (WORD Key, TShiftState Shift)
{
	if(pCurTool&&pCurTool->pCurControl)
		return pCurTool->pCurControl->KeyDown(Key,Shift);

	return false;
}

bool  CLevelTool::KeyUp     (WORD Key, TShiftState Shift)
{
	if(pCurTool&&pCurTool->pCurControl)
		return pCurTool->pCurControl->KeyUp(Key,Shift);

	return false;
}

bool  CLevelTool::KeyPress  (WORD Key, TShiftState Shift)
{
	if(pCurTool&&pCurTool->pCurControl)
		return pCurTool->pCurControl->KeyPress(Key,Shift);

	return false;
}


void CLevelTool::RealSetAction   (ETAction act)
{
	inherited::SetAction(act);
	if (pCurTool)
		pCurTool->SetAction(act);

	ExecCommand(COMMAND_UPDATE_TOOLBAR);
	m_Flags.set	(flChangeAction,FALSE);
}

void  CLevelTool::SetAction(ETAction act)
{
	// если мышь захвачена - изменим action после того как она освободится
	if (UI->IsMouseCaptured() || UI->IsMouseInUse())
	{
		m_Flags.set(flChangeAction, TRUE);
		iNeedAction = act;
	}
	else
		RealSetAction(act);
}

void  CLevelTool::RealSetTarget   (ObjClassID tgt,int sub_tgt,bool bForced)
{
	if(bForced||(target!=tgt)||(sub_target!=sub_tgt)){
		target 					= tgt;
		sub_target 				= sub_tgt;
		if (pCurTool){
			DETACH_FRAME(m_ToolForm);
			pCurTool->OnDeactivate();
		}
		pCurTool				= Scene->GetTool(tgt);
		VERIFY					(pCurTool);
		pCurTool->SetSubTarget	(sub_target);

		pCurTool->OnActivate	();

		pCurTool->SetAction		(GetAction());

		if (pCurTool->IsEditable())
			ATTACH_FRAME(pCurTool->pForm, m_ToolForm);
	}
	UI->RedrawScene();
	//fraLeftBar->ChangeTarget(tgt);
	//fraLeftBar->UpdateSnapList();
	ExecCommand(COMMAND_UPDATE_TOOLBAR);
	m_Flags.set(flChangeTarget,FALSE);
}

void  CLevelTool::ResetSubTarget()
{
	VERIFY(pCurTool);
	pCurTool->ResetSubTarget();
}

void  CLevelTool::SetTarget(ObjClassID tgt, int sub_tgt)
{
	// если мышь захвачена - изменим target после того как она освободится
	if (UI->IsMouseCaptured()||UI->IsMouseInUse()||!false){
		m_Flags.set(flChangeTarget,TRUE);
		if(tgt == OBJCLASS_WAY && sub_tgt==2 && target==tgt)
		{
			iNeedTarget		= tgt;
			iNeedSubTarget  = (sub_target)?0:1;
		}else
		{
			iNeedTarget		= tgt;
			iNeedSubTarget  = sub_tgt;
		}
	}else
		RealSetTarget(tgt,sub_tgt,false);
}


ObjClassID CLevelTool::CurrentClassID()
{
	return GetTarget();
}


void CLevelTool::OnShowHint(AStringVec& ss)
{
	Scene->OnShowHint(ss);
}


bool CLevelTool::Pick(TShiftState Shift)
{
	if( Scene->locked() && (esEditLibrary==UI->GetEState())){
		UI->m_CurrentCp = MainForm->GetRenderForm()->GetMousePos();
		UI->m_StartCp = UI->m_CurrentCp;
		EDevice->m_Camera.MouseRayFromPoint(UI->m_CurrentRStart, UI->m_CurrentRDir, UI->m_CurrentCp );
		SRayPickInfo pinf;
		//TfrmEditLibrary::RayPick(UI->m_CurrentRStart,UI->m_CurrentRDir,&pinf);
		return true;
	}
	return false;
}


void CLevelTool::RefreshProperties()
{
	//m_Props->RefreshForm();
}

bool CLevelTool::UpdateCamera()
{
	if (Scene->IsPlayInEditor())
	{
		g_pGameLevel->Cameras().ApplyDevice(VIEWPORT_NEAR);

		extern ENGINE_API float psHUD_FOV;
		Device.mProject_hud.build_projection(deg2rad(psHUD_FOV), Device.fASPECT,
			HUD_VIEWPORT_NEAR, g_pGamePersistent->Environment().CurrentEnv->far_plane);

		Device.mView_hud.set(Device.mView);
		Device.mFullTransform_hud.mul(Device.mProject_hud, Device.mView_hud);

		return true;
	}
	return false;
}

void CLevelTool::ShowProperties(LPCSTR focus_to_item)
{
	RealUpdateProperties	();
	if(MainForm)
		MainForm->GetPropertiesFrom()->Open();
   
	/*
	if(focus_to_item)
		m_Props->SelectFolder	(focus_to_item);
	else
	{
		if(pCurTool && pCurTool->ClassID!=OBJCLASS_DUMMY)
		{
			LPCSTR cn = pCurTool->ClassDesc();
			m_Props->SelectFolder	(cn);
		}
	}
	*/
	UI->RedrawScene			();
}


void CLevelTool::mtUpdateProperties(void* This)
{
	CLevelTool* pTool = (CLevelTool*)This;

	while (true)
	{
		WaitForSingleObject(pTool->mtPropObj, INFINITE);

		pTool->m_WorldProps->ClearProperties();
		pTool->m_Props->ClearProperties();

		if (pTool->m_WorldProps->IsModified()) Scene->UndoSave();

		PropItemVec itemsworld;

		// scene common props
		Scene->FillProp("", itemsworld, pTool->CurrentClassID());
		pTool->m_WorldProps->AssignItems(itemsworld);

		if (pTool->m_Props->IsModified())
			Scene->UndoSave();

		PropItemVec items;
		Scene->FillPropObjects("", items, pTool->CurrentClassID());
		pTool->m_Props->AssignItemsAsync(std::move(items));

		pTool->PropUpdateIsCompleted = true;

		ResetEvent(pTool->mtPropObj);
	}
}

void CLevelTool::RealUpdateProperties()
{
	PropUpdateIsCompleted = false;
	SetEvent(mtPropObj);
	m_Flags.set(flUpdateProperties, FALSE);
	m_Props->setModified(FALSE);
}


void CLevelTool::OnPropsClose()
{
	/*if (m_Props->IsModified()) Scene->UndoSave();*/
}


void  CLevelTool::OnPropsModified()
{
	Scene->Modified();
//	Scene->UndoSave();
	UI->RedrawScene();
}



bool CLevelTool::IfModified()
{
  /*  EEditorState est 		= UI->GetEState();
	switch(est){
	case esEditLightAnim: 	return TfrmEditLightAnim::FinalClose();
	case esEditLibrary: 	return TfrmEditLibrary::FinalClose();
	case esEditScene:		return Scene->IfModified();
	default: THROW;
	}*/
	return false;
}


void CLevelTool::ZoomObject(BOOL bSelectedOnly)
{
	if( !Scene->locked() ){
		Scene->ZoomExtents(CurrentClassID(),bSelectedOnly);
	} else {
		if (UI->GetEState()==esEditLibrary){
		   //   TfrmEditLibrary::ZoomObject();
		}
	}
}


void CLevelTool::GetCurrentFog(u32& fog_color, float& s_fog, float& e_fog)
{

	if (psDeviceFlags.is(rsEnvironment)&&psDeviceFlags.is(rsFog)||UI->IsPlayInEditor())
	{
		s_fog				= g_pGamePersistent->Environment().CurrentEnv->fog_near;
		e_fog				= g_pGamePersistent->Environment().CurrentEnv->fog_far;
		Fvector& f_clr		= g_pGamePersistent->Environment().CurrentEnv->fog_color;
		fog_color 			= color_rgba_f(f_clr.x,f_clr.y,f_clr.z,1.f);
	}
	else
	{
   
		s_fog				= psDeviceFlags.is(rsFog)?(1.0f - fFogness)* 0.85f * UI->ZFar():0.99f*UI->ZFar();
		e_fog				= psDeviceFlags.is(rsFog)?0.91f * UI->ZFar():UI->ZFar();

	}
	
}


LPCSTR CLevelTool::GetInfo()
{
	static xr_string sel;
	int cnt = Scene->SelectionCount(true,CurrentClassID());
	sel = " Sel: " + xr_string::ToString(cnt);

	return sel.c_str();
}


void  CLevelTool::OnFrame()
{

	if (psDeviceFlags.is(rsEnvironment) &&! UI->IsPlayInEditor()&& g_pGamePersistent&&g_pGamePersistent->pEnvironment)
	{
		g_pGamePersistent->Environment().SetGameTime(g_pGamePersistent->Environment().GetGameTime() + Device.fTimeDelta * g_pGamePersistent->Environment().fTimeFactor, g_pGamePersistent->Environment().fTimeFactor);
	}
	Scene->OnFrame		(EDevice->fTimeDelta);
	EEditorState est 	= UI->GetEState();
	if ((est==esEditScene)||(est==esEditLibrary)||(est==esEditLightAnim)){
		if (true/*!UI->IsMouseCaptured()*/)
		{
			// если нужно изменить target выполняем после того как мышь освободится
			if(m_Flags.is(flChangeTarget)) 		RealSetTarget(iNeedTarget,iNeedSubTarget,false);
			// если нужно изменить action выполняем после того как мышь освободится
			if(m_Flags.is(flChangeAction)) 		RealSetAction(ETAction(iNeedAction));
		}
		if (m_Flags.is(flUpdateProperties)) 	RealUpdateProperties();
		if (m_Flags.is(flUpdateObjectList)) 	RealUpdateObjectList();
		//TfrmEditLightAnim::OnIdle();
	}

	if (IsCompilerRunning())
	{
		DWORD ExitCode = 0;
		if (GetExitCodeProcess(m_CompilerProcess.hProcess, &ExitCode) == 0)
		{
			Msg("! Cannot return exit code in compiler process (%d).\n", GetLastError());
			m_CompilerProcess.hProcess = 0;

		}
		else
		{
			if (ExitCode != STILL_ACTIVE)
			{
				CloseHandle(m_CompilerProcess.hProcess);
				CloseHandle(m_CompilerProcess.hThread);
				m_CompilerProcess.hProcess = 0;
			}
		}
	}  
	if (IsGameRunning())
	{
		DWORD ExitCode = 0;
		if (GetExitCodeProcess(m_GameProcess.hProcess, &ExitCode) == 0)
		{
			Msg("! Cannot return exit code in compiler process (%d).\n", GetLastError());
			m_GameProcess.hProcess = 0;

		}
		else
		{
			if (ExitCode != STILL_ACTIVE)
			{
				CloseHandle(m_GameProcess.hProcess);
				CloseHandle(m_GameProcess.hThread);
				m_GameProcess.hProcess = 0;
			}
		}
	}
}


void  CLevelTool::RenderEnvironment()
{
	// draw sky
	EEditorState est 		= UI->GetEState();
	switch(est){
	case esEditLightAnim:
	case esEditScene:		
		if (psDeviceFlags.is(rsEnvironment)|| UI->IsPlayInEditor())
		{ 
			g_pGamePersistent->Environment().RenderSky	();
			g_pGamePersistent->Environment().RenderClouds	();
		}
	}
}

void  CLevelTool::Render()
{
	// Render update
	if(!Scene->IsPlayInEditor()) {
		::Render->Calculate();
		::Render->Render();
	}

	EEditorState est = UI->GetEState();
	// draw scene
	switch(est)
	{
	case esEditLibrary:
		UIEditLibrary::OnRender(); 
		break;

	case esEditLightAnim:
	case esEditScene:
		Scene->Render(EDevice->m_Camera.GetTransform()); 
		if (psDeviceFlags.is(rsEnvironment) || UI->IsPlayInEditor())
			g_pGamePersistent->Environment().RenderLast();
	break;
	case esBuildLevel: Builder.OnRender(); break;
	}

	// draw cursor
	LUI->m_Cursor->Render();
    inherited::Render();
}


void CLevelTool::ShowObjectList()
{
 //if (pObjectListForm) pObjectListForm->ShowObjectList();
}


void CLevelTool::RealUpdateObjectList()
{
   //if (pObjectListForm) pObjectListForm->UpdateObjectList();
	m_Flags.set(flUpdateObjectList,FALSE);
}


bool CLevelTool::IsModified()
{
	return Scene->IsUnsaved();
}


#include "../XrECore/Editor/EditMesh.h"
bool CLevelTool::RayPick(const Fvector& start, const Fvector& dir, float& dist, Fvector* pt, Fvector* n)
{
	if (Scene->ObjCount()&&(UI->GetEState()==esEditScene)){
		SRayPickInfo pinf;
		pinf.inf.range	= dist;
		if (Scene->RayPickObject(dist, start,dir,OBJCLASS_SCENEOBJECT,&pinf,0)){ 
			dist		= pinf.inf.range;
			if (pt) 	pt->set(pinf.pt); 
			if (n){	
				const Fvector* PT[3];
				pinf.e_mesh->GetFacePT(pinf.inf.id, PT);
				n->mknormal(*PT[0],*PT[1],*PT[2]);
			}
			return true;
		}
	}
	Fvector N={0.f,-1.f,0.f};
	Fvector P={0.f,0.f,0.f};
	Fplane PL; PL.build(P,N);
	float d;
	if (PL.intersectRayDist(start,dir,d)&&(d<=dist)){
		dist = d;
		if (pt) pt->mad(start,dir,dist); 
		if (n)	n->set(N);
		return true;
	}else return false;
}

bool CLevelTool::GetSelectionPosition(Fmatrix& result)
{
	if(pCurTool)
	{
		Fvector 			center;
		Fbox 				BB;
		BB.invalidate		();
//    	pCurTool->GetBBox	(BB, true);

		const CCustomObject* object = pCurTool->LastSelected();
		if(!object)
			return false;
			
		const_cast<CCustomObject*>(object)->GetBox		(BB);
		
		BB.getcenter		(center);
		center.y			= BB.max.y;

		Fvector2			pt_ss;
		pt_ss.set			(10000,-10000);
		Fvector				pt_ss_3d;
		BB.setb				(center, Fvector().set(1.0f,1.0f,1.0f));
		for(int k=0;k<8;++k)
		{
			Fvector pt;
			BB.getpoint(k,pt);
			EDevice->mFullTransform.transform(pt_ss_3d, pt);
			
			pt_ss.x = _min(pt_ss.x, pt_ss_3d.y);
			pt_ss.y = _max(pt_ss.y, pt_ss_3d.y);
		}

		float r_bb_ss	 = pt_ss.y - pt_ss.x;
		clamp(r_bb_ss, 0.0f,0.10f);
		float des_radius = 0.2f; 
		float csale 	 = des_radius/r_bb_ss;
		
		result.scale	(csale,csale,csale);
		result.c 		= center;
		return 			true;
	}else
		return 			false;
}
void   CLevelTool::Simulate()
{
/*	if (!g_scene_physics.Simulating())
		g_scene_physics.CreateShellsSelected();
	else
		g_scene_physics.DestroyAll();
	UI->RedrawScene();
	ExecCommand(COMMAND_REFRESH_UI_BAR);*/
}
void   CLevelTool::UseSimulatePositions()
{
	/*g_scene_physics.UseSimulatePoses();*/
}

void CLevelTool::RunGame(const char* Params)
{
	if (IsGameRunning() || IsCompilerRunning())
	{
		return;
	}
	m_GameProcess = {};
	STARTUPINFOA si = {};

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	ZeroMemory(&m_GameProcess, sizeof(m_GameProcess));


	string_path CommandLine;
	xr_sprintf(CommandLine, "xrEngine.exe %s", Params);
	Msg("~ Run Game %s.\n", CommandLine);
	// Start the child process. 
	if (!CreateProcessA(NULL,   // No module name (use command line)
		CommandLine,        // Command line
		NULL,           // Process handle not inheritable
		NULL,           // Thread handle not inheritable
		FALSE,          // Set handle inheritance to FALSE
		0,              // No creation flags
		NULL,           // Use parent's environment block
		NULL,           // Use parent's starting directory 
		&si,            // Pointer to STARTUPINFO structure
		&m_GameProcess)           // Pointer to PROCESS_INFORMATION structure
		)
	{
		Msg("! PlayPC:CreateProcess failed (%d).\n", GetLastError());
		return;
	}
}

void CLevelTool::RunXrLC()
{
	if (m_CompilerProcess.hProcess)
		return;

	if (m_GameProcess.hProcess)
		return;

	STARTUPINFOA si = {};

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	ZeroMemory(&m_CompilerProcess, sizeof(m_CompilerProcess));

	;

	string_path CommandLine;
	const xr_string& CompPath = ((CLevelPreferences*)EPrefs)->Compiler_xrLC.c_str();

	if (CompPath.empty())
	{
		xr_sprintf(CommandLine, " xrLC.exe -f %s", Scene->m_LevelOp.m_FNLevelPath.c_str());
	}
	else
	{
		xr_sprintf(CommandLine, " %s -f %s", CompPath.data(), Scene->m_LevelOp.m_FNLevelPath.c_str());
	}

	Msg("~ Run %s.\n", CommandLine);
	// Start the child process. 
	if (!CreateProcessA(NULL,   // No module name (use command line)
		CommandLine,        // Command line
		NULL,           // Process handle not inheritable
		NULL,           // Thread handle not inheritable
		FALSE,          // Set handle inheritance to FALSE
		0,              // No creation flags
		NULL,           // Use parent's environment block
		NULL,           // Use parent's starting directory 
		&si,            // Pointer to STARTUPINFO structure
		&m_CompilerProcess)           // Pointer to PROCESS_INFORMATION structure
		)
	{
		Msg("! XrLC:CreateProcess failed (%d).\n", GetLastError());
		return;
	}
}
void CLevelTool::RunXrDO()
{
	if (m_CompilerProcess.hProcess)
		return;

	if (m_GameProcess.hProcess)
		return;

	STARTUPINFOA si = {};

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	ZeroMemory(&m_CompilerProcess, sizeof(m_CompilerProcess));


	string_path CommandLine;
	const xr_string& CompPath = ((CLevelPreferences*)EPrefs)->Compiler_xrDO.c_str();
	if (CompPath.empty())
	{
		xr_sprintf(CommandLine, "xrDO_light.exe -f %s", Scene->m_LevelOp.m_FNLevelPath.c_str());
	}
	else
	{
		xr_sprintf(CommandLine, "%s -f %s", CompPath.data(), Scene->m_LevelOp.m_FNLevelPath.c_str());
	}
	Msg("~ Run %s.\n", CommandLine);
	// Start the child process. 
	if (!CreateProcessA(NULL,   // No module name (use command line)
		CommandLine,        // Command line
		NULL,           // Process handle not inheritable
		NULL,           // Thread handle not inheritable
		FALSE,          // Set handle inheritance to FALSE
		0,              // No creation flags
		NULL,           // Use parent's environment block
		NULL,           // Use parent's starting directory 
		&si,            // Pointer to STARTUPINFO structure
		&m_CompilerProcess)           // Pointer to PROCESS_INFORMATION structure
		)
	{
		Msg("! xrDO_light:CreateProcess failed (%d).\n", GetLastError());
		return;
	}
}
void CLevelTool::RunXrAI_Spawn(bool current_level)
{
	if (m_CompilerProcess.hProcess)
		return;

	if (m_GameProcess.hProcess)
		return;

	STARTUPINFOA si = {};

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	ZeroMemory(&m_CompilerProcess, sizeof(m_CompilerProcess));


	string_path CommandLine;
	const xr_string& CompPath = ((CLevelPreferences*)EPrefs)->Compiler_xrAI.c_str();
	if (CompPath.empty())
	{
		xr_sprintf(CommandLine, "xrAI.exe -no_separator_check -s %s -out all", current_level ? Scene->m_LevelOp.m_FNLevelPath.c_str() : "");
	}
	else
	{
		xr_sprintf(CommandLine, "%s -no_separator_check -s %s -out all", CompPath.data(), current_level ? Scene->m_LevelOp.m_FNLevelPath.c_str() : "");
	}
	Msg("~ Run %s.\n", CommandLine);
	// Start the child process. 
	if (!CreateProcessA(NULL,   // No module name (use command line)
		CommandLine,        // Command line
		NULL,           // Process handle not inheritable
		NULL,           // Thread handle not inheritable
		FALSE,          // Set handle inheritance to FALSE
		0,              // No creation flags
		NULL,           // Use parent's environment block
		NULL,           // Use parent's starting directory 
		&si,            // Pointer to STARTUPINFO structure
		&m_CompilerProcess)           // Pointer to PROCESS_INFORMATION structure
		)
	{
		Msg("! xrAI:CreateProcess failed (%d).\n", GetLastError());
		return;
	}
}
void CLevelTool::RunXrAI_AIMap(bool draw)
{
	if (m_CompilerProcess.hProcess)
		return;

	if (m_GameProcess.hProcess)
		return;

	STARTUPINFOA si = {};

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	ZeroMemory(&m_CompilerProcess, sizeof(m_CompilerProcess));


	string_path CommandLine;
	const xr_string& CompPath = ((CLevelPreferences*)EPrefs)->Compiler_xrAI.c_str();
	if (CompPath.empty())
	{
		xr_sprintf(CommandLine, "xrAI.exe -f %s %s", Scene->m_LevelOp.m_FNLevelPath.c_str(), draw ? "-draft" : "");
	}
	else
	{
		xr_sprintf(CommandLine, "%s -f %s %s", CompPath.data(), Scene->m_LevelOp.m_FNLevelPath.c_str(), draw ? "-draft" : "");
	}
	
	Msg("~ Run %s.\n", CommandLine);
	// Start the child process. 
	if (!CreateProcessA(NULL,   // No module name (use command line)
		CommandLine,        // Command line
		NULL,           // Process handle not inheritable
		NULL,           // Thread handle not inheritable
		FALSE,          // Set handle inheritance to FALSE
		0,              // No creation flags
		NULL,           // Use parent's environment block
		NULL,           // Use parent's starting directory 
		&si,            // Pointer to STARTUPINFO structure
		&m_CompilerProcess)           // Pointer to PROCESS_INFORMATION structure
		)
	{
		Msg("! xrAI:CreateProcess failed (%d).\n", GetLastError());
		return;
	}
}
void CLevelTool::RunXrAI_Verify()
{
	if (m_CompilerProcess.hProcess)
		return;

	if (m_GameProcess.hProcess)
		return;

	STARTUPINFOA si = {};

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	ZeroMemory(&m_CompilerProcess, sizeof(m_CompilerProcess));


	string_path CommandLine;
	const xr_string& CompPath = ((CLevelPreferences*)EPrefs)->Compiler_xrAI.c_str();
	if (CompPath.empty())
	{
		xr_sprintf(CommandLine, "xrAI.exe -verify %s", Scene->m_LevelOp.m_FNLevelPath.c_str());
	}
	else
	{
	    xr_sprintf(CommandLine, "%s -verify %s", CompPath.data(), Scene->m_LevelOp.m_FNLevelPath.c_str());
	}
	
	Msg("~ Run %s.\n", CommandLine);
	// Start the child process. 
	if (!CreateProcessA(NULL,   // No module name (use command line)
		CommandLine,        // Command line
		NULL,           // Process handle not inheritable
		NULL,           // Thread handle not inheritable
		FALSE,          // Set handle inheritance to FALSE
		0,              // No creation flags
		NULL,           // Use parent's environment block
		NULL,           // Use parent's starting directory 
		&si,            // Pointer to STARTUPINFO structure
		&m_CompilerProcess)           // Pointer to PROCESS_INFORMATION structure
		)
	{
		Msg("! xrAI:CreateProcess failed (%d).\n", GetLastError());
		return;
	}
}

bool CLevelTool::IsCompilerRunning()
{
	return m_CompilerProcess.hProcess;
}

bool CLevelTool::IsGameRunning()
{
	return m_GameProcess.hProcess;
}

void CLevelTool::Terminated()
{
	if (IsGameRunning())
	{
		TerminateProcess(m_GameProcess.hProcess, 1);
	}
	if (IsCompilerRunning())
	{
		TerminateProcess(m_CompilerProcess.hProcess, 1);
	}
}
