#include "stdafx.h"
#include "x_ray.h"
#include "Application.h"
#include "IGame_Level.h"
#include "IGame_Persistent.h"
#include "XR_IOConsole.h"
#include "std_classes.h"
#include "../xrCDB/ISpatial.h"
#include "ILoadingScreen.h"

//---------------------------------------------------------------------
// 2446363
// umbt@ukr.net
//////////////////////////////////////////////////////////////////////////
struct _SoundProcessor : public pureFrame
{
	virtual void  OnFrame()
	{
		Device.Statistic->Sound.Begin();
		::Sound->update(Device.vCameraPosition, Device.vCameraDirection, Device.vCameraTop);
		Device.Statistic->Sound.End();
	}
}	SoundProcessor;

CApplication::CApplication()
{
	ll_dwReference = 0;

	max_load_stage = 0;

	// events
	g_pEventManager->Attach(this);

	// levels
	Level_Current = u32(-1);
	
	if (Device.IsEditorMode())
		return;

	Level_Scan();

	// Register us
	Device.seqFrame.Add(this, REG_PRIORITY_HIGH + 1000);

	if (psDeviceFlags.test(mtSound))	Device.seqFrameMT.Add(&SoundProcessor);
	else								Device.seqFrame.Add(&SoundProcessor);

	Console->Show();

	// App Title
	loadingScreen = nullptr;
}

CApplication::~CApplication()
{
	if (Console != nullptr)
		Console->Hide();

	if (DevicePtr != nullptr && !Device.IsEditorMode())
	{
		Device.seqFrameMT.Remove(&SoundProcessor);
		Device.seqFrame.Remove(&SoundProcessor);
		Device.seqFrame.Remove(this);
	}

	// events
	g_pEventManager->Detach(this);
}

extern bool quiting;

void CApplication::OnEvent(EVENT E, u64 P1, u64 P2)
{
	g_pEventManager->OnEvent(E, P1, P2);

	if (E == g_pEventManager->eQuit)
	{
		quiting = true;
		for (u32 i = 0; i < Levels.size(); i++)
		{
			xr_free(Levels[i].folder);
			xr_free(Levels[i].name);
		}
		Levels.clear();
	}
	else if (E == g_pEventManager->eStart)
	{
		LPSTR		op_server = LPSTR(P1);
		LPSTR		op_client = LPSTR(P2);
		Level_Current = u32(-1);
		R_ASSERT(0 == g_pGameLevel);
		R_ASSERT(0 != g_pGamePersistent);

#ifdef NO_SINGLE
		Console->Execute("main_menu on");
		if ((op_server == nullptr) ||
			(!xr_strlen(op_server)) ||
			(
				(strstr(op_server, "/dm") || strstr(op_server, "/deathmatch") ||
					strstr(op_server, "/tdm") || strstr(op_server, "/teamdeathmatch") ||
					strstr(op_server, "/ah") || strstr(op_server, "/artefacthunt") ||
					strstr(op_server, "/cta") || strstr(op_server, "/capturetheartefact")
					) &&
				!strstr(op_server, "/alife")
				)
			)
#endif // #ifdef NO_SINGLE
		{
			Console->Execute("main_menu off");
			Console->Hide();
			//-----------------------------------------------------------
			g_pGamePersistent->PreStart(op_server);
			//-----------------------------------------------------------
			g_pGameLevel = (IGame_Level*)NEW_INSTANCE(CLSID_GAME_LEVEL);
			pApp->LoadBegin();
			g_pGamePersistent->Start(op_server);
			g_pGameLevel->net_Start(op_server, op_client);
			pApp->LoadEnd();
		}
		xr_free(op_server);
		xr_free(op_client);
	}
	else if (E == g_pEventManager->eDisconnect)
	{
		if (g_pGameLevel)
		{
			Console->Hide();
			g_pGameLevel->net_Stop();
			DEL_INSTANCE(g_pGameLevel);
			Console->Show();

			if ((FALSE == g_pEventManager->Event.Peek("KERNEL:quit")) && (FALSE == g_pEventManager->Event.Peek("KERNEL:start")))
			{
				Console->Execute("main_menu off");
				Console->Execute("main_menu on");
			}
		}
		R_ASSERT(0 != g_pGamePersistent);
		g_pGamePersistent->Disconnect();
	}
	else if (E == g_pEventManager->eConsole)
	{
		LPSTR command = (LPSTR)P1;
		Console->ExecuteCommand(command, false);
		xr_free(command);
	}
	else if (E == g_pEventManager->eStartMPDemo)
	{
		LPSTR demo_file = LPSTR(P1);

		R_ASSERT(0 == g_pGameLevel);
		R_ASSERT(0 != g_pGamePersistent);

		Console->Execute("main_menu off");
		Console->Hide();
		Device.Reset(false);

		g_pGameLevel = (IGame_Level*)NEW_INSTANCE(CLSID_GAME_LEVEL);
		shared_str server_options = g_pGameLevel->OpenDemoFile(demo_file);

		//-----------------------------------------------------------
		g_pGamePersistent->PreStart(server_options.c_str());
		//-----------------------------------------------------------

		pApp->LoadBegin();
		g_pGamePersistent->Start("");//server_options.c_str()); - no prefetch !
		g_pGameLevel->net_StartPlayDemo();
		pApp->LoadEnd();

		xr_free(demo_file);
	}
}

static	CTimer	phase_timer;
extern	ENGINE_API BOOL			g_appLoaded = FALSE;

void CApplication::LoadBegin()
{
	ll_dwReference++;
	if (1 == ll_dwReference) {

		g_appLoaded = FALSE;

		phase_timer.Start();
		load_stage = 0;
	}
}

void CApplication::LoadEnd()
{
	ll_dwReference--;
	if (0 == ll_dwReference) {
		Msg("* phase time: %d ms", phase_timer.GetElapsed_ms());
		Msg("* phase cmem: %d K", Memory.mem_usage() / 1024);
		Console->Execute("stat_memory");
		g_appLoaded = TRUE;
		//		DUMP_PHASE;
	}
}

void CApplication::SetLoadingScreen(ILoadingScreen* newScreen) {
	if (loadingScreen) {
		Log("! Trying to create new loading screen, but there is already one..");
		DestroyLoadingScreen();
	}

	loadingScreen = newScreen;
}

void CApplication::DestroyLoadingScreen() { xr_delete(loadingScreen); }

void CApplication::LoadDraw()
{
	if (g_appLoaded)				return;
	Device.dwFrame += 1;


	if (!Device.Begin())		return;

	if (g_dedicated_server)
		Console->OnRender();
	else
		load_draw_internal();

	Device.End();
}

void CApplication::LoadForceFinish() {
	if (loadingScreen)
		loadingScreen->ForceFinish();
}

void CApplication::SetLoadStageTitle(pcstr _ls_title)
{
	const static bool isLoadingStagesEnabled = EngineExternal()[EEngineExternalUI::ShowLoadingStages];
	if (loadingScreen && isLoadingStagesEnabled)
		loadingScreen->SetStageTitle(_ls_title);
	Log(_ls_title);
	// для корректной работы лоадскрина в ЧН и ТЧ
}

void CApplication::LoadTitleInt(LPCSTR str1, LPCSTR str2, LPCSTR str3)
{
	const static bool disableLoadScreenTips = EngineExternal()[EEngineExternalRender::DisableLoadScreenTips];
	if (loadingScreen && !disableLoadScreenTips)
	{
		loadingScreen->SetStageTip(str1, str2, str3);
	}
}

void CApplication::LoadStage()
{
	VERIFY(ll_dwReference);
	Msg("* phase time: %d ms", phase_timer.GetElapsed_ms());	phase_timer.Start();
	Msg("* phase cmem: %d K", Memory.mem_usage() / 1024);

	if (g_pGamePersistent->GameType() == 1 && !xr_strcmp(g_pGamePersistent->m_game_params.m_alife, "alife"))
		max_load_stage = 17;
	else
		max_load_stage = 14;
	LoadDraw();
	++load_stage;
}

void CApplication::LoadSwitch()
{
}

// Sequential
void CApplication::OnFrame()
{
	g_pEventManager->Event.OnFrame();
	g_SpatialSpace->update();
	g_SpatialSpacePhysic->update();
}

void CApplication::Level_Append(LPCSTR folder)
{
	string_path	N1, N2, N3, N4;
	xr_strconcat(N1, folder, "level");
	xr_strconcat(N2, folder, "level.ltx");
	xr_strconcat(N3, folder, "level.geom");
	xr_strconcat(N4, folder, "level.cform");
	if (
		FS.exist("$game_levels$", N1) &&
		FS.exist("$game_levels$", N2) &&
		FS.exist("$game_levels$", N3) &&
		FS.exist("$game_levels$", N4)
		)
	{
		sLevelInfo			LI;
		LI.folder = xr_strdup(folder);
		LI.name = 0;
		Levels.push_back(LI);
	}
}

void CApplication::Level_Scan()
{
	for (u32 i = 0; i < Levels.size(); i++)
	{
		xr_free(Levels[i].folder);
		xr_free(Levels[i].name);
	}
	Levels.clear();


	xr_vector<char*>* folder = FS.file_list_open("$game_levels$", FS_ListFolders | FS_RootOnly);
	//.	R_ASSERT							(folder&&folder->size());

	for (u32 i = 0; i < folder->size(); ++i)
		Level_Append((*folder)[i]);

	FS.file_list_close(folder);
}

void gen_logo_name(string_path& dest, LPCSTR level_name, int num = -1)
{
	xr_strconcat(dest, "intro\\intro_", level_name);

	u32 len = xr_strlen(dest);
	if (dest[len - 1] == '\\')
		dest[len - 1] = 0;

	if (num < 0)
		return;

	string16 buff;
	xr_strcat(dest, sizeof(dest), "_");
	xr_strcat(dest, sizeof(dest), itoa(num + 1, buff, 10));
}

// Return true if logo exists
// Always sets the path even if logo doesn't exist
bool set_logo_path(string_path& path, pcstr levelName, int count = -1)
{
	gen_logo_name(path, levelName, count);
	string_path temp2;
	return FS.exist(temp2, "$game_textures$", path, ".dds") || FS.exist(temp2, "$level$", path, ".dds");
}

void CApplication::Level_Set(u32 L)
{
    if (L >= Levels.size())
        return;
    FS.get_path("$level$")->_set(Levels[L].folder);
    Level_Current = L;

    static string_path path;
    path[0] = 0;

    int count = 0;
    while (true)
    {
        if (set_logo_path(path, Levels[L].folder, count))
            count++;
        else
            break;
    }

    if (count)
    {
        const int num = ::Random.randI(count);
        gen_logo_name(path, Levels[L].folder, num);
    }
    else if (!set_logo_path(path, Levels[L].folder))
    {
        if (!set_logo_path(path, "no_start_picture"))
            path[0] = 0;
    }

    if (path[0] && loadingScreen)
        loadingScreen->SetLevelLogo(path);
}

int CApplication::Level_ID(LPCSTR name, LPCSTR ver, bool bSet)
{
	int result = -1;

	bool arch_res = false;

	for (CLocatorAPI::archive& Arch : FS.m_archives)
	{
		if (Arch.hSrcFile == nullptr)
		{
			LPCSTR ln = Arch.header->r_string("header", "level_name");
			LPCSTR lv = Arch.header->r_string("header", "level_ver");

			if (0 == _stricmp(ln, name) && 0 == _stricmp(lv, ver))
			{
				FS.LoadArchive(Arch);
				arch_res = true;
			}
		}
	}

	if (arch_res)
		Level_Scan();

	string256 buffer;
	xr_strconcat(buffer, name, "\\");
	for (u32 I = 0; I < Levels.size(); ++I)
	{
		if (0 == _stricmp(buffer, Levels[I].folder))
		{
			result = int(I);
			break;
		}
	}

	if (bSet && result != -1)
		Level_Set(result);

	if (arch_res)
		g_pGamePersistent->OnAssetsChanged();

	return result;
}

CInifile* CApplication::GetArchiveHeader(LPCSTR name, LPCSTR ver)
{
	for (CLocatorAPI::archive& Arch : FS.m_archives)
	{
		LPCSTR ln = Arch.header->r_string("header", "level_name");
		LPCSTR lv = Arch.header->r_string("header", "level_ver");

		if (0 == _stricmp(ln, name) && 0 == _stricmp(lv, ver))
		{
			return Arch.header;
		}
	}
	return nullptr;
}

void CApplication::LoadAllArchives()
{
	if (FS.load_all_unloaded_archives())
	{
		Level_Scan();
		g_pGamePersistent->OnAssetsChanged();
	}
}

void CApplication::load_draw_internal()
{
	Device.m_pRender->SetupDefaultTarget();

	if (loadingScreen)
		loadingScreen->Update(load_stage, max_load_stage);
	else
		Device.m_pRender->ClearTarget();
}
