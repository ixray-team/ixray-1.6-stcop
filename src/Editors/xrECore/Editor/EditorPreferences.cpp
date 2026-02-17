//---------------------------------------------------------------------------
#include "stdafx.h"

#include <fstream>

#include "../xrEUI/xrUITheme.h"

#include "ui_main.h"
#include "UI_ToolsCustom.h"
#include "../xrEngine/Environment.h"
#include "../xrEngine/IGame_Persistent.h"
//---------------------------------------------------------------------------
CCustomPreferences* EPrefs=nullptr;
//---------------------------------------------------------------------------
// extern ENGINE_API bool bIsRaindropCollision;
// extern ENGINE_API bool bIsSndOnRoof;

CCustomPreferences::CCustomPreferences()
{
	bOpen = false;
	// view
	view_np				= 0.1f;
	view_fp				= 1500.f;
	view_fov			= 60.f;
	// fog    
	fog_color			= 0x00555555;
	fog_fogness			= 0.9;
	// camera
	cam_fly_speed		= 5.0f;
	cam_fly_alt			= 1.8f;
	cam_sens_rot		= 0.6f;
	cam_sens_move		= 0.6f;
	// tools mouse
	tools_sens_move		= 0.3f;
	tools_sens_rot		= 0.3f;
	tools_sens_scale	= 0.3f;
	// box pick
	bp_lim_depth		= true;
	bp_cull				= true;
	bp_depth_tolerance	= 0.1f;
	// snap
	snap_angle			= deg2rad(5.f);
	snap_move			= 0.1f;
	snap_moveto			= 0.5f;
	scale_fixed         = 0.1f;
	// grid
	grid_cell_size		= 1.f;
	grid_cell_count		= 100;
	// scene
	scene_undo_level	= 125;
	scene_recent_count	= 10;
	scene_clear_color	= DEFAULT_CLEARCOLOR;
	// objects
	object_flags.zero	();
    // Weather
    env_speed           = 12.f;
    env_from_time       = 0.f;
    env_to_time         = 24.f * 60.f * 60.f;
    // sWeather         = "";
	//sound
	sound_volume		= 1.f;

	//other
	custom_icons.clear();

	string_path AppDataPath = {};
	FS.update_path(AppDataPath, "$app_data_root$", "");

	if (!std::filesystem::exists(AppDataPath))
	{
		std::filesystem::create_directory(AppDataPath);
	}

}
//---------------------------------------------------------------------------

CCustomPreferences::~CCustomPreferences()
{
}

void CCustomPreferences::ApplyValues()
{
	Tools->m_MoveSnap = snap_move;
	Tools->m_MoveSnapTo = snap_moveto;
	Tools->m_RotateSnapAngle = snap_angle;
	Tools->m_ScaleFixed = scale_fixed;

	UI->CurrentView().m_Camera.SetViewport(view_np, view_fp, view_fov);
	Tools->SetFog	(fog_color,fog_fogness);

	UI->m_MouseSM	= 0.2f*tools_sens_move*tools_sens_move;
	UI->m_MouseSR	= 0.02f*tools_sens_rot*tools_sens_rot;
	UI->m_MouseSS	= 0.02f*tools_sens_scale*tools_sens_scale;

	UI->CurrentView().m_Camera.SetSensitivity	(cam_sens_move, cam_sens_rot);
	UI->CurrentView().m_Camera.SetFlyParams	(cam_fly_speed, cam_fly_alt);

	ExecCommand		(COMMAND_UPDATE_GRID);
}
//---------------------------------------------------------------------------

void  CCustomPreferences::OnClose	()
{
	ApplyValues	();	
}
//---------------------------------------------------------------------------


void CheckValidate(ShortcutValue*, const xr_shortcut& new_val, bool& result)
{
	{
		xr_shortcut null;
		if (null.similar(new_val)) { return; }
	}
	result 					= true; 
	ECommandVec& cmds		= GetEditorCommands();
	for (u32 cmd_idx=0; cmd_idx<cmds.size(); cmd_idx++){
		SECommand*& CMD		= cmds[cmd_idx];
		if (CMD&&CMD->editable){
			VERIFY(!CMD->sub_commands.empty());
			for (u32 sub_cmd_idx=0; sub_cmd_idx<CMD->sub_commands.size(); sub_cmd_idx++){
				SESubCommand*& SUB_CMD = CMD->sub_commands[sub_cmd_idx];
				if (SUB_CMD->shortcut.similar(new_val)){ result = false; return;}
			}
		}
	}
}

void CCustomPreferences::OnKeyboardCommonFileClick(ButtonValue* B, bool& bModif, bool&)
{
	bModif = false;
	xr_string fn;
	switch(B->btn_num)
	{
	case 0:
	    if(EFS.GetOpenName("$import$", fn, false, nullptr, 6, "*.json"))
		{
			json File;
			if (std::filesystem::exists(xr_path(fn)))
			{
				std::ifstream f(fn.data());
				f >> File;
				LoadShortcuts(File);
			}
	    }
	break;
	case 1:
	    if(EFS.GetSaveName("$import$", fn, nullptr, 6, "*.json"))
		{
			json File;
		    SaveShortcuts(File);

			if (!fn.ends_with(".json"))
			{
				fn += ".json";
			}

			std::ofstream o(fn.data());
			o << File;
	    }
	break;
	}
}

void CCustomPreferences::FillProp(PropItemVec& props)
{
	PHelper().CreateU32		(props,"Common\\Recent Count", 						&scene_recent_count, 	0, 		25);
	PHelper().CreateU32		(props,"Common\\Undo Level", 						&scene_undo_level, 		0, 		125);
	PHelper().CreateBool	(props,"Common\\More Stats Info", 					&bMoreStats);

	PHelper().CreateBool	(props,"Viewport\\Buttons\\Axis", 					&ShowAxisButtons);
	PHelper().CreateBool	(props,"Viewport\\Buttons\\Old Camera Controls", 	&ShowOldCameraButtons);

	PHelper().CreateFloat	(props,"Viewport\\Grid\\Cell Size", 				&grid_cell_size, 		0.1f, 	10.f);
	PHelper().CreateU32		(props,"Viewport\\Grid\\Cell Count", 				&grid_cell_count, 		10, 	1000);

	PHelper().CreateFloat	(props,"Viewport\\Camera\\Move Sens", 				&cam_sens_move);
	PHelper().CreateFloat	(props,"Viewport\\Camera\\Rotate Sens", 			&cam_sens_rot);
	PHelper().CreateFloat	(props,"Viewport\\Camera\\Fly Speed", 				&cam_fly_speed, 		0.01f, 	100.f);
	PHelper().CreateFloat	(props,"Viewport\\Camera\\Fly Altitude", 			&cam_fly_alt, 			0.f, 	1000.f);

	PHelper().CreateColor	(props,"Viewport\\Fog\\Color", 						&fog_color);
	PHelper().CreateFloat	(props,"Viewport\\Fog\\Fogness", 					&fog_fogness, 			0.f, 	100.f);

	PHelper().CreateFloat	(props,"Viewport\\Near Plane", 						&view_np, 				0.01f, 	10.f);
	PHelper().CreateFloat	(props,"Viewport\\Far Plane", 						&view_fp, 				10.f, 	10000.f);
	PHelper().CreateFloat	(props,"Viewport\\FOV", 							&view_fov, 				0.1f, 	170.f);
	PHelper().CreateFloat	(props,"Viewport\\Render Radius", 					&EDevice->RenderRadius, 10.f, 	100000.f);
	PHelper().CreateColor	(props,"Viewport\\Clear Color", 					&scene_clear_color);

	PHelper().CreateFlag32	(props,"Objects\\Show\\Hint", 						&object_flags, 			epoShowHint);
	PHelper().CreateFlag32	(props,"Objects\\Show\\Pivot", 						&object_flags, 			epoDrawPivot);
	PHelper().CreateFlag32	(props,"Objects\\Show\\Animation Path", 			&object_flags, 			epoDrawAnimPath);
	PHelper().CreateFlag32	(props,"Objects\\Show\\LOD's", 						&object_flags, 			epoDrawLOD);

	PHelper().CreateFlag32	(props,"Objects\\Skeleton\\Draw Joints", 			&object_flags, 			epoDrawJoints);
	PHelper().CreateFlag32	(props,"Objects\\Skeleton\\Draw Bone Axis", 		&object_flags, 			epoDrawBoneAxis);
	PHelper().CreateFlag32	(props,"Objects\\Skeleton\\Draw Bone Names", 		&object_flags, 			epoDrawBoneNames);
	PHelper().CreateFlag32	(props,"Objects\\Skeleton\\Draw Bone Shapes", 		&object_flags, 			epoDrawBoneShapes);

	PHelper().CreateFlag32	(props,"Objects\\Group Object\\Select ingroup", 	&object_flags, 			epoSelectInGroup);

	PHelper().CreateFlag32	(props,"Objects\\Loading\\Deffered Loading RB", 	&object_flags, 			epoDeffLoadRB);
	PHelper().CreateFlag32	(props,"Objects\\Loading\\Deffered Loading CF", 	&object_flags, 			epoDeffLoadCF);

	PHelper().CreateFlag32	(props,"Objects\\Library\\Discard Instance", 		&object_flags, 			epoDiscardInstance);

	PHelper().CreateBool	(props,"Tools\\Box Pick\\Limited Depth", 			&bp_lim_depth);
	PHelper().CreateBool	(props,"Tools\\Box Pick\\Back Face Culling", 		&bp_cull);
	PHelper().CreateFloat	(props,"Tools\\Box Pick\\Depth Tolerance", 			&bp_depth_tolerance, 	0.f, 	10000.f);

	PHelper().CreateFloat	(props,"Tools\\Sens\\Move", 						&tools_sens_move);
	PHelper().CreateFloat	(props,"Tools\\Sens\\Rotate", 						&tools_sens_rot);
	PHelper().CreateFloat	(props,"Tools\\Sens\\Scale", 						&tools_sens_scale);

	PHelper().CreateAngle	(props,"Tools\\Snap\\Angle", 						&snap_angle, 			0, 		PI_MUL_2);
	PHelper().CreateFloat	(props,"Tools\\Snap\\Move", 						&snap_move, 			0.01f, 	500.f);
	PHelper().CreateFloat	(props,"Tools\\Snap\\Move To", 						&snap_moveto, 			0.01f, 	1000.f);
	PHelper().CreateFloat	(props,"Tools\\Snap\\Scale Fixed", 					&scale_fixed, 			0.01f, 	1000.f);

    /*/ Weather
    PHelper().CreateFloat(props, "Weather\\Weather from Time", &env_from_time, 0.f, 86400.f);
    PHelper().CreateFloat(props, "Weather\\Weather to Time", &env_to_time, 0.f, 24.f * 60.f * 60.f);
    PHelper().CreateFloat(props, "Weather\\Weather time factor", &env_speed, 0.f, 10000.f);
    PHelper().CreateRText(props, "Weather\\Weather Cycle", &sWeather);*/

	PHelper().CreateSText	(props, "Compilers Path\\xrLC",						&Compiler_xrLC);
	PHelper().CreateSText	(props,	"Compilers Path\\xrAI",						&Compiler_xrAI);
	PHelper().CreateSText	(props, "Compilers Path\\xrDO",						&Compiler_xrDO);
	
	PHelper().CreateBool(props, "Compilers\\LODs For All MU", &LODsForAllMU);
	

	ButtonValue* B = PHelper().CreateButton(props, "Keyboard\\Common\\File", "Load,Save", 0);
	B->OnBtnClickEvent.bind(this, &CCustomPreferences::OnKeyboardCommonFileClick);
	ECommandVec& cmds = GetEditorCommands();
	for (u32 cmd_idx = 0; cmd_idx < cmds.size(); cmd_idx++)
	{
		SECommand*& CMD = cmds[cmd_idx];
		if (CMD && CMD->editable)
		{
			VERIFY(!CMD->sub_commands.empty());
			for (u32 sub_cmd_idx = 0; sub_cmd_idx < CMD->sub_commands.size(); sub_cmd_idx++)
			{
				SESubCommand*& SUB_CMD = CMD->sub_commands[sub_cmd_idx];
				string128 nm;
				sprintf(nm, "%s%s%s", CMD->Desc(), !SUB_CMD->desc.empty() ? "\\" : "", SUB_CMD->desc.c_str());
				ShortcutValue* V = PHelper().CreateShortcut(props, PrepareKey("Keyboard\\Shortcuts", nm), &SUB_CMD->shortcut);
				V->OnValidateResultEvent.bind(CheckValidate);
			}
		}
	}
}

void CCustomPreferences::Edit()
{
	if (bOpen)
		return;

	bOpen = true;
	// fill prop
	PropItemVec props;

	FillProp						(props);

	m_ItemProps->AssignItems		(props);
	UI->Push(this, false);
   // m_ItemProps->ShowPropertiesModal();

	// save changed options
}
//---------------------------------------------------------------------------
extern bool bAllowLogCommands;

template<typename T>
T GetSafe(const nlohmann::json& j, const char* key, T& Out)
{
	if (!j.is_object())
	{
		return Out;
	}

	auto it = j.find(key);
	if (it == j.end() || it->is_null())
	{
		return Out;
	}

	try
	{
		if constexpr (std::is_same_v<T, xr_string>)
		{
			Out = it->get<std::string>().c_str();
		}
		else if constexpr (std::is_same_v<T, shared_str>)
		{
			Out = it->get<std::string>().c_str();
		}
		else if constexpr(std::is_same_v<std::remove_cvref_t<T>, char*>)
		{
			auto OutTemp = it->get<std::string>();
			xr_strcpy(Out, OutTemp.c_str());
		}
		else
		{
			if constexpr (std::is_same_v<T, bool>)
			{
				if (it->is_number())
				{
					Out = !!it->get<int>();
				}
				else 
				{
					Out = it->get<T>();
				}
			}
			else
			{
				Out = it->get<T>();
			}
		}
	}
	catch (...)
	{
	}

	return Out;
}

const nlohmann::json* GetObjectSafe(const nlohmann::json& j, const char* key)
{
	if (!j.is_object())
	{
		return nullptr;
	}

	auto it = j.find(key);
	if (it == j.end() || !it->is_object())
	{
		return nullptr;
	}

	return &(*it);
}

void CCustomPreferences::Load()
{
	// --- EDITOR PREFS ---
	const auto* prefs = GetObjectSafe(JSONData, "editor_prefs");
	if (!prefs)
	{
		return;
	}

	// flags
	GetSafe(*prefs, "device_flags", psDeviceFlags.flags);
	GetSafe(*prefs, "sound_flags", psSoundFlags.flags);

	// theme
	if (int themeID; GetSafe(*prefs, "theme", themeID))
	{
		CUIThemeManager::Get().ThemeID = themeID;
	}

	// sound
	GetSafe(*prefs, "sound_volume", sound_volume);

	// tools
	GetSafe(*prefs, "tools_settings", Tools->m_Settings.flags);

	// view
	GetSafe(*prefs, "view_np", view_np);
	GetSafe(*prefs, "view_fp", view_fp);
	GetSafe(*prefs, "view_fov", view_fov);

	// fog
	GetSafe(*prefs, "fog_color", fog_color);
	GetSafe(*prefs, "fog_fogness", fog_fogness);

	// camera
	GetSafe(*prefs, "cam_fly_speed", cam_fly_speed);
	GetSafe(*prefs, "cam_fly_alt", cam_fly_alt);
	GetSafe(*prefs, "cam_sens_rot", cam_sens_rot);
	GetSafe(*prefs, "cam_sens_move", cam_sens_move);

	// ui toggles
	GetSafe(*prefs, "ShowAxisButtons", ShowAxisButtons);
	GetSafe(*prefs, "ShowOldCameraButtons", ShowOldCameraButtons);
	GetSafe(*prefs, "bMoreStats", bMoreStats);
	GetSafe(*prefs, "bp_cull", bp_cull);
	GetSafe(*prefs, "scale_fixed", scale_fixed);

	// tools sens
	GetSafe(*prefs, "tools_sens_move", tools_sens_move);
	GetSafe(*prefs, "tools_sens_rot", tools_sens_rot);
	GetSafe(*prefs, "tools_sens_scale", tools_sens_scale);

	// misc
	GetSafe(*prefs, "bp_lim_depth", bp_lim_depth);
	GetSafe(*prefs, "snap_angle", snap_angle);
	GetSafe(*prefs, "snap_move", snap_move);
	GetSafe(*prefs, "snap_moveto", snap_moveto);

	GetSafe(*prefs, "bp_depth_tolerance", bp_depth_tolerance);

	// grid / scene
	GetSafe(*prefs, "grid_cell_size", grid_cell_size);
	GetSafe(*prefs, "grid_cell_count", grid_cell_count);
	GetSafe(*prefs, "scene_undo_level", scene_undo_level);
	GetSafe(*prefs, "scene_recent_count", scene_recent_count);
	GetSafe(*prefs, "scene_clear_color", scene_clear_color);

	// object flags
	GetSafe(*prefs, "object_flags", object_flags.flags);

	// --- RENDER ---
	if (const auto* render = GetObjectSafe(JSONData, "render"))
	{
		GetSafe(*render, "render_radius", EDevice->RenderRadius);
		GetSafe(*render, "w", start_w);
		GetSafe(*render, "h", start_h);
		GetSafe(*render, "quality", EDevice->m_ScreenQuality);

		int x = 0, y = 0;
		if (GetSafe(*render, "x", x) && GetSafe(*render, "y", y))
		{
			SDL_SetWindowPosition(g_AppInfo.Window, x, y);
		}

		GetSafe(*render, "maximized", start_maximized);
	}

	// --- WINDOWS ---
	if (const auto* windows = GetObjectSafe(JSONData, "windows"))
	{
		GetSafe(*windows, "log", bAllowLogCommands);
	}

	// --- RECENT FILES ---
	for (u32 i = 0; i < scene_recent_count; ++i)
	{
		char buffer[64];
		snprintf(buffer, sizeof(buffer), "recent_files_%d", i);

		xr_string fn;
		GetSafe(*prefs, buffer, fn);
		if (!fn.empty())
		{
			auto it = std::find(scene_recent_list.begin(), scene_recent_list.end(), fn);
			if (it == scene_recent_list.end())
			{
				scene_recent_list.push_back(Platform::ANSI_TO_UTF8(fn));
			}
		}
	}

	// --- WEATHER ---
	if (prefs)
	{
		if (prefs->contains("env_from_time"))
		{
			GetSafe(*prefs, "env_from_time", env_from_time);
			GetSafe(*prefs, "env_to_time", env_to_time);
			GetSafe(*prefs, "env_speed", env_speed);
		}

		GetSafe(*prefs, "weather", sWeather);
	}

	// --- CONTENT BROWSER EXTRA ---
	if (const auto* cb = GetObjectSafe(JSONData, "ContentBrowser"))
	{
		GetSafe(*cb, "file_custom_icon", custom_icons);
	}
	// load shortcuts
	LoadShortcuts(JSONData);
	UI->LoadSettings(JSONData);
}

void CCustomPreferences::Save()
{
	CUIThemeManager& ThemeInstance = CUIThemeManager::Get();

	JSONData["editor_prefs"]["theme"] = ThemeInstance.ThemeID;
	JSONData["editor_prefs"]["device_flags"] = psDeviceFlags.flags;
	JSONData["editor_prefs"]["sound_flags"] = psSoundFlags.flags;
	JSONData["editor_prefs"]["sound_volume"] = sound_volume;

	JSONData["editor_prefs"]["tools_settings"]=Tools->m_Settings.flags;

	JSONData["editor_prefs"]["view_np"]=view_np;
	JSONData["editor_prefs"]["view_fp"]=view_fp;
	JSONData["editor_prefs"]["view_fov"]=view_fov;

	JSONData["editor_prefs"]["fog_color"]=fog_color;
	JSONData["editor_prefs"]["fog_fogness"]=fog_fogness;

	JSONData["editor_prefs"]["cam_fly_speed"]=cam_fly_speed;
	JSONData["editor_prefs"]["cam_fly_alt"]=cam_fly_alt;
	JSONData["editor_prefs"]["cam_sens_rot"]=cam_sens_rot;
	JSONData["editor_prefs"]["cam_sens_move"]=cam_sens_move;

	JSONData["editor_prefs"]["ShowAxisButtons"] = ShowAxisButtons;
	JSONData["editor_prefs"]["ShowOldCameraButtons"] = ShowOldCameraButtons;

	JSONData["editor_prefs"]["tools_sens_rot"]=tools_sens_rot;
	JSONData["editor_prefs"]["tools_sens_move"]=tools_sens_move;
	JSONData["editor_prefs"]["tools_sens_scale"]=tools_sens_scale;
	JSONData["editor_prefs"]["bMoreStats"]=bMoreStats;

	JSONData["editor_prefs"]["bp_lim_depth"]=bp_lim_depth;
	JSONData["editor_prefs"]["bp_cull"]=bp_cull;
	JSONData["editor_prefs"]["bp_depth_tolerance"]=bp_depth_tolerance;

	JSONData["editor_prefs"]["snap_angle"]=snap_angle;
	JSONData["editor_prefs"]["snap_move"]=snap_move;
	JSONData["editor_prefs"]["snap_moveto"]=snap_moveto;
	JSONData["editor_prefs"]["scale_fixed"]=scale_fixed;

	JSONData["editor_prefs"]["grid_cell_size"]=grid_cell_size;
	JSONData["editor_prefs"]["grid_cell_count"]=grid_cell_count;

	JSONData["editor_prefs"]["scene_undo_level"]=scene_undo_level;
	JSONData["editor_prefs"]["scene_recent_count"]= scene_recent_list.size();
	JSONData["editor_prefs"]["scene_clear_color"]=scene_clear_color;

	JSONData["editor_prefs"]["object_flags"]=object_flags.flags;

	for (AStringIt it = scene_recent_list.begin(); it != scene_recent_list.end(); it++)
	{
		string256 buffer1 = {};
		string256 buffer2 = {};
		sprintf(buffer1, "recent_files_%d", int(it - scene_recent_list.begin()));
		sprintf(buffer2, "%s", it->c_str());

		xr_string L = buffer1;
		xr_string V = buffer2;
		JSONData["editor_prefs"][L.c_str()] = V.c_str();
	}

	auto WndFlags = SDL_GetWindowFlags(g_AppInfo.Window);

    // Weather
    JSONData["editor_prefs"]["env_from_time"] = env_from_time;
    JSONData["editor_prefs"]["env_to_time"] = env_to_time;
    JSONData["editor_prefs"]["env_speed"] = env_speed;
    JSONData["editor_prefs"]["weather"] = sWeather.c_str() ? sWeather.c_str() : "";

	JSONData["render"]["maximized"] = WndFlags & SDL_WINDOW_MAXIMIZED;

	JSONData["render"]["w"] = EDevice->Width;
	JSONData["render"]["h"] = EDevice->Height;
	JSONData["render"]["quality"] = EDevice->m_ScreenQuality;

	int X, Y;
	SDL_GetWindowPosition(g_AppInfo.Window, &X, &Y);
	JSONData["render"]["x"]=X;
	JSONData["render"]["y"]=Y;

	JSONData["windows"]["log"]=bAllowLogCommands;
	JSONData["render"]["render_radius"]=EDevice->RenderRadius;
	
	JSONData["ContentBrowser"]["file_custom_icon"] = custom_icons;
	// load shortcuts
	SaveShortcuts(JSONData);
	UI->SaveSettings(JSONData);
}

void CCustomPreferences::Draw()
{
	if (!bOpen)
		return;

	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(300, 400));
	if (!ImGui::Begin("Editor Preferences", &bOpen))
	{
		OnClose();
		Save();
		ImGui::PopStyleVar();
		ImGui::End();
		return;
	}

	IsDocked = ImGui::IsWindowDocked();
	IsFocused = ImGui::IsWindowFocused();

	ImGui::PopStyleVar();
	{
		m_ItemProps->Draw();
	}
	ImGui::End();
}

void CCustomPreferences::LoadConfig()
{
	string_path fn;
	INI_NAME(fn);

	string_path jfn;
	JSON_NAME(jfn);

	if (std::filesystem::exists(jfn))
	{
		std::ifstream f(jfn);
		if (!f.is_open())
		{
			Msg("! Cannot open file: %s", jfn);
			return;
		}

		f >> JSONData;
	}
	else
	{
		int DisplayX = GetSystemMetrics(SM_CXFULLSCREEN);
		int DisplayY = GetSystemMetrics(SM_CYFULLSCREEN);

		EDevice->Height = DisplayY;
		EDevice->Width = DisplayX;

		string_path dscfn;
		FS.update_path(dscfn, "$server_data_root$", "default_shortcuts.json");

		if (std::filesystem::exists(dscfn))
		{
			json File;
			std::ifstream f(dscfn);
			f >> File;
			LoadShortcuts(File);
		}
		
		Save();
	}

	Load				();
	ApplyValues			();

	UI->m_Size.set((int)start_w, (int)start_h);
}

void CCustomPreferences::SaveConfig()
{
	string_path jfn;
	JSON_NAME(jfn);

	Save();

	std::ofstream o(jfn);
	o << JSONData;

}

void CCustomPreferences::AppendRecentFile(const char* name)
{
	for (AStringIt it = scene_recent_list.begin(); it != scene_recent_list.end(); it++)
	{
		if (*it == name)
		{
			scene_recent_list.erase(it);
			break;
		}
	}

	scene_recent_list.insert(scene_recent_list.begin(), Platform::ANSI_TO_UTF8(name));
	while (scene_recent_list.size() > 10)
	{
		scene_recent_list.pop_back();
	}
}

void CCustomPreferences::OnCreate()
{
	LoadConfig();
	m_ItemProps = new UIPropertiesForm();
	//m_ItemProps 		= TProperties::CreateModalForm("Editor Preferences",false,0,0,TOnCloseEvent(this,&CCustomPreferences::OnClose),TProperties::plItemFolders|TProperties::plFullSort); //TProperties::plFullExpand TProperties::plFullSort TProperties::plNoClearStore|TProperties::plFolderStore|
}

void CCustomPreferences::OnDestroy()
{
  
	xr_delete(m_ItemProps);
	SaveConfig();
}