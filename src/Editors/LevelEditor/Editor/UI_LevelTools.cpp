#include "stdafx.h"
#include "Utils/Cursor3D.h"
#include "UI/UIEditLibrary.h"
#include "Scene/LEPhysics.h"
#include "../Viewports/ViewportMesh.h"

CLevelTool*	LTools=(CLevelTool*)Tools;

TShiftState ssRBOnly;

int CLevelTool::AddViewport(IViewport* VP)
{
	Viewlist.push_back(VP);
	return Viewlist.size() - 1;
}

void CLevelTool::RemoveViewport(IViewport* VP)
{
	auto Iter = std::find(Viewlist.begin(), Viewlist.end(), VP);
	if (Iter != Viewlist.end())
	{
		Viewlist.erase(Iter);
	}
}

CLevelTool::CLevelTool()
{
	fFogness	= 0.9f;
	dwFogColor	= 0xffffffff;
	m_Flags.zero();
	m_ToolForm = 0;
	m_CompilerProcess.hProcess = 0;

	CurrentTool = nullptr;
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
	CurrentTool = 0;
	ssRBOnly = ssRight;
	m_Flags.set(flChangeAction, false);
	m_Flags.set(flChangeTarget, false);
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

	// scene destroing
	if (CurrentTool)
	{
		CurrentTool->OnDeactivate();
	}

	Scene->OnDestroy();
}

void CLevelTool::Reset()
{
	RealSetTarget(GetTarget(),estDefault,true);
}


bool CLevelTool::MouseStart(TShiftState Shift)
{
	if (Scene->IsPlayInEditor())
	{
		return false;
	}

	inherited::MouseStart(Shift);
	if(CurrentTool && CurrentTool->pCurControl)
	{
		if ((CurrentTool->pCurControl->Action() != etaSelect) && (!CurrentTool->IsEditable() || !CurrentTool->AllowMouseStart() || (CurrentTool->FClassID == OBJCLASS_DUMMY)))
		{
			return false;
		}

		return CurrentTool->pCurControl->Start(Shift);
	}
	return false;
}

void CLevelTool::MouseMove(TShiftState Shift)
{
	inherited::MouseMove(Shift);
	if(CurrentTool&&CurrentTool->pCurControl)
	{
		if (HiddenMode())
		{
			ExecCommand(COMMAND_UPDATE_PROPERTIES);
		}

		CurrentTool->pCurControl->Move(Shift);
	}
}

bool CLevelTool::MouseEnd(TShiftState Shift)
{
	inherited::MouseEnd(Shift);
	if(CurrentTool&&CurrentTool->pCurControl)
	{
		if (HiddenMode())
		{
			ExecCommand(COMMAND_UPDATE_PROPERTIES);
		}

		return CurrentTool->pCurControl->End(Shift);
	}
	return false;
}

void CLevelTool::OnObjectsUpdate()
{
	UpdateProperties(false);
	if (CurrentTool && CurrentTool->pCurControl)
	{
		return CurrentTool->OnObjectsUpdate();
	}
}

bool CLevelTool::HiddenMode()
{
	if (CurrentTool && CurrentTool->pCurControl)
	{
		return CurrentTool->pCurControl->HiddenMode();
	}
	
	return false;
}

bool CLevelTool::KeyDown(WORD Key, TShiftState Shift)
{
	if (CurrentTool && CurrentTool->pCurControl)
	{
		return CurrentTool->pCurControl->KeyDown(Key, Shift);
	}

	return false;
}

bool CLevelTool::KeyUp(WORD Key, TShiftState Shift)
{
	if (CurrentTool && CurrentTool->pCurControl)
	{
		return CurrentTool->pCurControl->KeyUp(Key, Shift);
	}

	return false;
}

bool CLevelTool::KeyPress(WORD Key, TShiftState Shift)
{
	if (CurrentTool && CurrentTool->pCurControl)
	{
		return CurrentTool->pCurControl->KeyPress(Key, Shift);
	}

	return false;
}

bool CLevelTool::MouseWheel(int direction, TShiftState Shift)
{
	if (CurrentTool && CurrentTool->pCurControl)
	{
		return CurrentTool->pCurControl->Wheel(direction, Shift);
	}

	return false;
}

void CLevelTool::RealSetAction(ETAction act)
{
	inherited::SetAction(act);
	if (CurrentTool)
	{
		CurrentTool->SetAction(act);
	}

	m_Flags.set	(flChangeAction,false);
}

void CLevelTool::SetAction(ETAction act)
{
	// если мышь захвачена - изменим action после того как она освободится
	if (UI->IsMouseCaptured() || UI->IsMouseInUse())
	{
		m_Flags.set(flChangeAction, true);
		iNeedAction = act;
		return;
	}

	RealSetAction(act);
}

void  CLevelTool::RealSetTarget(ObjClassID tgt, int sub_tgt, bool bForced)
{
	if (bForced || (target != tgt) || (sub_target != sub_tgt))
	{
		target = tgt;
		sub_target = sub_tgt;

		if (CurrentTool)
		{
			m_ToolForm = nullptr;
			CurrentTool->OnDeactivate();
		}

		CurrentTool = Scene->GetTool(tgt);
		VERIFY(CurrentTool);

		CurrentTool->SetSubTarget(sub_target);
		CurrentTool->OnActivate();
		CurrentTool->SetAction(GetAction());

		if (CurrentTool->IsEditable())
		{
			if (CurrentTool->pForm)
			{
				m_ToolForm = CurrentTool->pForm;
			};
		}
	}

	UI->RedrawScene();
	m_Flags.set(flChangeTarget, false);
}

void CLevelTool::ResetSubTarget()
{
	VERIFY(CurrentTool);
	CurrentTool->ResetSubTarget();
}

void CLevelTool::SetTarget(ObjClassID tgt, int sub_tgt)
{
	m_Flags.set(flChangeTarget, true);
	if (tgt == OBJCLASS_WAY && sub_tgt == 2 && target == tgt)
	{
		iNeedTarget = tgt;
		iNeedSubTarget = (sub_target) ? 0 : 1;
	}
	else
	{
		iNeedTarget = tgt;
		iNeedSubTarget = sub_tgt;
	}
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
		UI->CurrentView().m_Camera.MouseRayFromPoint(UI->m_CurrentRStart, UI->m_CurrentRDir, UI->m_CurrentCp );
		SRayPickInfo pinf;
		//TfrmEditLibrary::RayPick(UI->m_CurrentRStart,UI->m_CurrentRDir,&pinf);
		return true;
	}
	return false;
}

bool CLevelTool::UpdateCamera()
{
	if (Scene->IsPlayInEditor())
	{
		extern ENGINE_API float psHUD_FOV;
		Device.mProject_hud.build_projection(deg2rad(psHUD_FOV), Device.fASPECT, Device.fHUDViewportNear, g_pGamePersistent->Environment().CurrentEnv->far_plane);

		Device.mView_hud.set(Device.mView);
		Device.mFullTransform_hud.mul(Device.mProject_hud, Device.mView_hud);

		return true;
	}

	return false;
}

void CLevelTool::ShowProperties(const char* focus_to_item)
{
	UpdateProperties();
	if (MainForm)
	{
		MainForm->GetPropertiesForm()->Open();
	}

	UI->RedrawScene();
}

void CLevelTool::mtUpdateProperties(void* This)
{
	CLevelTool* pTool = (CLevelTool*)This;

	while (true)
	{
		WaitForSingleObject(pTool->mtPropObj, INFINITE);

		while (!pTool->m_WorldProps->DrawComplete || !pTool->m_Props->DrawComplete)
		{
			std::this_thread::yield();
		}
		
		pTool->m_WorldProps->ClearProperties();
		pTool->m_Props->ClearProperties();

		if (pTool->m_WorldProps->IsModified())
		{
			Scene->UndoSave();
		}

		PropItemVec itemsworld;

		// scene common props
		Scene->FillProp("", itemsworld, pTool->CurrentClassID());
		pTool->m_WorldProps->AssignItems(itemsworld);

		if (pTool->m_Props->IsModified())
		{
			Scene->UndoSave();
		}

		PropItemVec items;
		Scene->FillPropObjects("", items, pTool->CurrentClassID());
		pTool->m_Props->AssignItemsAsync(std::move(items));

		if (MainForm != nullptr && MainForm->GetPropertiesForm())
		{
			MainForm->GetPropertiesForm()->PropUpdateIsCompleted = true;
		}

		ResetEvent(pTool->mtPropObj);
	}
}

void CLevelTool::UpdateProperties()
{
	if (MainForm != nullptr && MainForm->GetPropertiesForm())
	{
		MainForm->GetPropertiesForm()->PropUpdateIsCompleted = false;
	}

	SetEvent(mtPropObj);
	m_Flags.set(flUpdateProperties, false);
	m_Props->setModified(false);
}

void  CLevelTool::OnPropsModified()
{
	Scene->Modified();
	UI->RedrawScene();
}

bool CLevelTool::IfModified()
{
	return false;
}

void CLevelTool::ZoomObject(bool bSelectedOnly)
{
	if (!Scene->locked())
	{
		Scene->ZoomExtents(CurrentClassID(), bSelectedOnly);
	}
}

void CLevelTool::GetCurrentFog(u32& fog_color, float& s_fog, float& e_fog)
{
	if (psDeviceFlags.is(rsEnvironment) && psDeviceFlags.is(rsFog) || UI->IsPlayInEditor())
	{
		s_fog = g_pGamePersistent->Environment().CurrentEnv->fog_near;
		e_fog = g_pGamePersistent->Environment().CurrentEnv->fog_far;
		Fvector& f_clr = g_pGamePersistent->Environment().CurrentEnv->fog_color;
		fog_color = color_rgba_f(f_clr.x, f_clr.y, f_clr.z, 1.f);
	}
	else
	{
		s_fog = psDeviceFlags.is(rsFog) ? (1.0f - fFogness) * 0.85f * UI->ZFar() : 0.99f * UI->ZFar();
		e_fog = psDeviceFlags.is(rsFog) ? 0.91f * UI->ZFar() : UI->ZFar();
	}
}

const char* CLevelTool::GetInfo()
{
	static xr_string sel;
	int cnt = Scene->SelectionCount(true,CurrentClassID());
	sel = " Sel: " + xr_string::ToString(cnt);

	return sel.c_str();
}

void CLevelTool::OnFrame()
{
	if (psDeviceFlags.is(rsEnvironment) && !UI->IsPlayInEditor() && g_pGamePersistent && g_pGamePersistent->pEnvironment)
	{
		g_pGamePersistent->Environment().SetGameTime(g_pGamePersistent->Environment().GetGameTime() + Device.fTimeDelta * g_pGamePersistent->Environment().fTimeFactor, g_pGamePersistent->Environment().fTimeFactor);
	}
	Scene->OnFrame(EDevice->fTimeDelta);
	EEditorState est = UI->GetEState();
	if ((est == esEditScene) || (est == esEditLibrary) || (est == esEditLightAnim))
	{
		// если нужно изменить target выполняем после того как мышь освободится
		if (m_Flags.is(flChangeTarget)) 		RealSetTarget(iNeedTarget, iNeedSubTarget, false);
		// если нужно изменить action выполняем после того как мышь освободится
		if (m_Flags.is(flChangeAction)) 		RealSetAction(ETAction(iNeedAction));

		if (m_Flags.is(flUpdateProperties)) 	UpdateProperties();
		if (m_Flags.is(flUpdateObjectList)) 	UpdateObjectList();
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

void CLevelTool::Render()
{
	// Render update
	if(!Scene->IsPlayInEditor())
	{
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
			Scene->Render(UI->CurrentView().m_Camera.GetTransform());
		    if (psDeviceFlags.is(rsEnvironment) || UI->IsPlayInEditor())
		    {
		        g_pGamePersistent->Environment().RenderFlares();
		        g_pGamePersistent->Environment().RenderLast();
		    }
		break;
		case esBuildLevel: Builder.OnRender(); break;
		case esEditCustom:
		{
			for (IViewport* VP : Viewlist)
			{
				VP->Render();
			}
		}
	}

	// draw cursor
	LUI->m_Cursor->Render();
    inherited::Render();
}

void CLevelTool::UpdateObjectList()
{
	m_Flags.set(flUpdateObjectList,false);
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
	if (CurrentTool)
	{
		Fvector center;
		Fbox BB;
		BB.invalidate();

		const CCustomObject* object = CurrentTool->LastSelected();
		if (!object)
		{
			return false;
		}

		const_cast<CCustomObject*>(object)->GetBox(BB);

		BB.getcenter(center);
		center.y = BB.max.y;

		Fvector2 pt_ss;
		pt_ss.set(10000, -10000);
		Fvector pt_ss_3d;
		BB.setb(center, Fvector().set(1.0f, 1.0f, 1.0f));
		for (int k = 0; k < 8; ++k)
		{
			Fvector pt;
			BB.getpoint(k, pt);
			EDevice->mFullTransform.transform(pt_ss_3d, pt);

			pt_ss.x = std::min(pt_ss.x, pt_ss_3d.y);
			pt_ss.y = std::max(pt_ss.y, pt_ss_3d.y);
		}

		float r_bb_ss = pt_ss.y - pt_ss.x;
		clamp(r_bb_ss, 0.0f, 0.10f);
		float des_radius = 0.2f;
		float csale = des_radius / r_bb_ss;

		result.scale(csale, csale, csale);
		result.c = center;
		return true;
	}

	return false;
}

void CLevelTool::Simulate()
{
	if (!g_scene_physics.Simulating())
	{
		g_scene_physics.CreateShellsSelected();
	}
	else
	{
		g_scene_physics.DestroyAll();
	}

    UI->RedrawScene();
}

void CLevelTool::UseSimulatePositions()
{
    g_scene_physics.UseSimulatePoses();
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
		false,          // Set handle inheritance to false
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
		xr_sprintf(CommandLine, "xrLC.exe -f %s", Scene->m_LevelOp.m_FNLevelPath.c_str());
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
		false,          // Set handle inheritance to false
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
		false,          // Set handle inheritance to false
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
		false,          // Set handle inheritance to false
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
		false,          // Set handle inheritance to false
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
		false,          // Set handle inheritance to false
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