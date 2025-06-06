#include "stdafx.h"
#include "IGame_Level.h"

#include "x_ray.h"
#include "xr_ioc_cmd.h"
#include "CameraManager.h"
#include "Environment.h"
#include "xr_input.h"
#include "CustomHUD.h"
#include "string_table.h"
#include "../Include/xrRender/RenderDeviceRender.h"

#include "GamepadService.h"
#include "xr_object.h"

xr_token* vid_quality_token = nullptr;

ENGINE_API float devfloat1 = 1.0f;
ENGINE_API float devfloat2 = 1.0f;
ENGINE_API float devfloat3 = 1.0f;
ENGINE_API float devfloat4 = 1.0f;
float SheduleScaleDedicated = 0;
ENGINE_API float ps_render_scale = 1.0f;
ENGINE_API u32 ps_render_scale_preset = 0;
u32 ps_gamepad_prefix_override = 0;

xr_token vid_bpp_token[] =
{
	{ "16", 16 },
	{ "32", 32 },
	{ nullptr,	 0 }
};

xr_token vid_scale_preset_token[] = 
{
	{ "st_scale_native", 0 },
	{ "st_scale_quality", 1 },
	{ "st_scale_balanced", 2 },
	{ "st_scale_performance", 3 },
	{ "st_scale_ultraperformance", 4 },

	{ "st_scale_custom", 5 },
	{ nullptr, 0 }
};

xr_token gamepad_prefix_override_token[] = 
{
	{ "gp_none", 0 },
	{ "xbox1", 1 },
	{ "ps4", 2 },
	{ "ps5", 3 },
//	todo: fix some icon bugs for switch
//	{ "switch", 4 },

	{ nullptr, 0 }
};

ENGINE_API u32 ps_r_scale_mode = 1;
ENGINE_API u32 ps_proxy_r_scale_mode = 1;
xr_token qscale_mode_token[] = 
{
#ifdef DEBUG_DRAW
	{ "st_filter_point", 0},
#endif
	{ "st_filter_linear", 1},
	{ "st_filter_dlss", 2},
	{ "st_filter_fsr", 3},
//	{ "st_filter_xess", 4},
	{ nullptr, 0 }
};

void IConsole_Command::add_to_LRU(shared_str const& arg) {
	if (!arg.size() || bEmptyArgsHandled) 
	{
		return;
	}
	
	if (bool dup = std::ranges::contains(m_LRU, arg); !dup)
	{
		m_LRU.push_back(arg);

		if (m_LRU.size() > LRU_MAX_COUNT)
		{
			m_LRU.erase(m_LRU.begin());
		}
	}
}

void  IConsole_Command::add_LRU_to_tips(vecTips& tips) 
{
	vecLRU::reverse_iterator	it_rb = m_LRU.rbegin();
	vecLRU::reverse_iterator	it_re = m_LRU.rend();
	for (; it_rb != it_re; ++it_rb) {
		tips.push_back( *it_rb );
	}
}

class CCC_Quit : public IConsole_Command
{
public:
	CCC_Quit(const char* N) : IConsole_Command(N)  { bEmptyArgsHandled = true; }
	virtual void Execute(const char* args) 
	{
		if (Device.IsEditorMode())
		{
			Msg("This command cannot be executed in Editors.");
			return;
		}
		Console->Hide();
		g_pEventManager->Event.Defer("KERNEL:disconnect");
		g_pEventManager->Event.Defer("KERNEL:quit");
	}
};

class CCC_MotionsStat : public IConsole_Command
{
public:
	CCC_MotionsStat(const char* N) : IConsole_Command(N)  { bEmptyArgsHandled = true; }
	virtual void Execute(const char* args) {
		//g_pMotionsContainer->dump();
		//	TODO: move this console commant into renderer
		VERIFY(0);
	}
};

class CCC_TexturesStat : public IConsole_Command
{
public:
	CCC_TexturesStat(const char* N) : IConsole_Command(N)  { bEmptyArgsHandled = true; }
	virtual void Execute(const char* args) 
	{
		Device.DumpResourcesMemoryUsage();
	}
};

class CCC_E_Dump : public IConsole_Command
{
public:
	CCC_E_Dump(const char* N) : IConsole_Command(N)  { bEmptyArgsHandled = true; }
	virtual void Execute(const char* args) {
		g_pEventManager->Event.Dump();
	}
};

class CCC_E_Signal : public IConsole_Command
{
public:
	CCC_E_Signal(const char* N) : IConsole_Command(N)  { }
	virtual void Execute(const char* args) {
		char	Event[128],Param[128];
		Event[0]=0; Param[0]=0;
		sscanf	(args,"%[^,],%s",Event,Param);
		g_pEventManager->Event.Signal	(Event,(u64)Param);
	}
};

class CCC_Help : public IConsole_Command
{
public:
	CCC_Help(const char* N) : IConsole_Command(N)
	{
		bEmptyArgsHandled = true;
	}
	
	virtual void Execute(const char* args)
	{
	    Log("--- Command listing begin ---");
	    {
	    	u32 max_name_len = 0;
	    	u32 max_status_len = 0;

	    	for (auto cmd_ptr : Console->Commands | std::views::values)
	    	{
	    		u32 cur_name_len = xr_strlen(cmd_ptr->Name());
	    		max_name_len = std::max(cur_name_len, max_name_len);

	    		TStatus status;
	    		cmd_ptr->Status(status);

	    		u32 cur_status_len = xr_strlen(status);
	    		max_status_len = std::max(cur_status_len, max_status_len);
	    	}

	    	auto print_center = [](TStatus& out, const TStatus& str, u32 width)
	    	{
	    		u32 len = xr_strlen(str);

	    		if (len >= width)
	    		{
	    			xr_strcpy(out, str);
	    			return;
	    		}

	    		u32 left  = (width - len) / 2;
	    		u32 right = width - len - left;

	    		u32 char_pos = 0;

	    		for (u32 i = 0; i < left; ++i)
	    			out[char_pos++] = VK_SPACE;

	    		for (u32 i = 0; i < len; ++i)
	    			out[char_pos++] = str[i];

	    		for (u32 i = 0; i < right; ++i)
	    			out[char_pos++] = VK_SPACE;

	    		out[char_pos] = '\0';
	    	};

	    	for (const auto& cmd_ptr : Console->Commands | std::views::values)
	    	{
	    		TStatus status;
	    		cmd_ptr->Status(status);

	    		TInfo info;
	    		cmd_ptr->Info(info);

	    		TStatus formatted_status;
	    		print_center(formatted_status, status, max_status_len);

				Msg("%-*s (%s) - %s",
					max_name_len,
					cmd_ptr->Name(),
					formatted_status,
					info);
	    	}
	    	
	    	Msg(" ");
	    }

	    Log("--- Console usage begin ---");
	    {
	    	Msg("Key: Ctrl + A                              === Select all");
	    	Msg("Key: Ctrl + C                              === Copy to clipboard");
	    	Msg("Key: Ctrl + V                              === Paste from clipboard");
	    	Msg("Key: Ctrl + X                              === Cut to clipboard");
	    	Msg("Key: Ctrl + Z                              === Undo");
	    	Msg("Key: Ctrl + Insert                         === Copy to clipboard");
	    	Msg("Key: Shift + Insert                        === Paste from clipboard");
	    	Msg("Key: Shift + Delete                        === Cut to clipboard");
	    	Msg("Key: Insert                                === Toggle mode <Insert>");
	    	Msg("Key: Back / Delete                         === Delete symbol left/right");
	    	Msg("Key: Up/Down                               === Prev/Next command in tips list");
	    	Msg("Key: Ctrl+Up/Ctrl+Down                     === Prev/Next executing command");
	    	Msg("Key: Left, Right, Home, End {+Shift/+Ctrl} === Navigation in text");
	    	Msg("Key: PageUp/PageDown                       === Scrolling history");
	    	Msg("Key: Tab/Shift+Tab                         === Next/Prev possible command from list");
	    	Msg("Key: Enter/NumEnter                        === Execute current command");
	    	
	    	Msg(" ");
	    }
	}
};

void CCC_SaveCFG::Execute(const char* args)
{
	string_path cfg_full_name;
	xr_strcpy(cfg_full_name, xr_strlen(args) > 0 ? args : Console->ConfigFile);

	bool b_abs_name = xr_strlen(cfg_full_name) > 2 && cfg_full_name[1] == ':';

	if (!b_abs_name)
		FS.update_path(cfg_full_name, _app_data_root_, cfg_full_name);

	if (strext(cfg_full_name))
		*strext(cfg_full_name) = 0;
	xr_strcat(cfg_full_name, ".ltx");

	bool b_allow = true;
	if (FS.exist(cfg_full_name))
	{
#ifdef IXR_WINDOWS
		b_allow = !!SetFileAttributes(Platform::ANSI_TO_TCHAR(cfg_full_name), FILE_ATTRIBUTE_NORMAL);
#else
		struct stat st;
    	if (stat(cfg_full_name, &st) == 0) 
		{
    	    // Добавляем права на запись для владельца
    	    mode_t new_mode = st.st_mode | S_IWUSR;
    	    b_allow = (chmod(cfg_full_name, new_mode) == 0);
    	}
		else
		{
    	    b_allow = false;
    	}
#endif
	}

	if (b_allow)
	{
		IWriter* F = FS.w_open(cfg_full_name);
		for (auto& val : Console->Commands | std::views::values)
		{
			val->Save(F);
		}
		FS.w_close(F);
		Msg("Config-file [%s] saved successfully", cfg_full_name);
	}
	else
		Msg("!Cannot store config file [%s]", cfg_full_name);
}

void CCC_LoadCFG::Execute(const char* args) 
{
	string_path cfg_name;
	xr_strcpy(cfg_name, xr_strlen(args) > 0 ? args : Console->ConfigFile);
	Msg("Executing config-script \"%s\"...", cfg_name);

	if (strext(cfg_name))
		*strext(cfg_name) = 0;
	xr_strcat(cfg_name,".ltx");

	string_path cfg_full_name;

	FS.update_path(cfg_full_name, _app_data_root_, cfg_name);
		
	if( nullptr == FS.exist(cfg_full_name) )
		xr_strcpy(cfg_full_name, cfg_name);
	
	IReader* F = FS.r_open(cfg_full_name);
	
	if (F)
	{
		string1024 str;
		while (!F->eof())
		{
			F->r_string(str,sizeof(str));
			if(allow(str))
				Console->Execute(str);
		}
		FS.r_close(F);
		Msg("[%s] successfully loaded.",cfg_full_name);
	}
	else
		Msg("! Cannot open script file [%s]",cfg_full_name);
}

CCC_LoadCFG_custom::CCC_LoadCFG_custom(const char* cmd)
:CCC_LoadCFG(cmd)
{
	xr_strcpy(m_cmd, cmd);
}

bool CCC_LoadCFG_custom::allow(const char* cmd)
{
	return cmd == strstr(cmd, m_cmd);
}

class CCC_Start : public IConsole_Command
{
	void	parse		(LPSTR dest, const char* args, const char* name)
	{
		dest[0]	= 0;
		if (strstr(args,name))
			sscanf(strstr(args,name)+xr_strlen(name),"(%[^)])",dest);
	}

	void	protect_Name_strlwr( LPSTR str )
	{
 		string4096	out;
		xr_strcpy( out, sizeof(out), str );
		_strlwr( str );

		const char* name_str = "name=";
		const char* name1 = strstr( str, name_str );
		if ( !name1 || !xr_strlen( name1 ) )
		{
			return;
		}
		int begin_p = xr_strlen( str ) - xr_strlen( name1 ) + xr_strlen( name_str );
		if ( begin_p < 1 )
		{
			return;
		}

		const char* name2 = strchr( name1, '/' );
		int end_p = xr_strlen( str ) - (name2? xr_strlen(name2) : 0);
		if ( begin_p >= end_p )
		{
			return;
		}
		for ( int i = begin_p; i < end_p;++i )
		{
			str[i] = out[i];
		}
	}
public:
	CCC_Start(const char* N) : IConsole_Command(N)	{ 	  bLowerCaseArgs = false; }
	virtual void Execute(const char* args)
	{
		string4096	op_server,op_client,op_demo;
		op_server[0] = 0;
		op_client[0] = 0;
		
		parse		(op_server,args,"server");	// 1. server
		parse		(op_client,args,"client");	// 2. client
		parse		(op_demo, args,	"demo");	// 3. demo
		
		_strlwr( op_server );
		protect_Name_strlwr( op_client );

		if(!op_client[0] && strstr(op_server,"single"))
			xr_strcpy(op_client, "localhost");

		if (0 == xr_strlen(op_client) && 0 == xr_strlen(op_demo))
		{
			Msg("! Can't start game without client. Arguments: '%s'.",args);
			return;
		}
		if (g_pGameLevel)
			g_pEventManager->Event.Defer("KERNEL:disconnect");
		
		if (xr_strlen(op_demo))
		{
			g_pEventManager->Event.Defer	("KERNEL:start_mp_demo",u64(xr_strdup(op_demo)),0);
		} else
		{
			g_pEventManager->Event.Defer	("KERNEL:start",u64(xr_strlen(op_server)?xr_strdup(op_server):nullptr),u64(xr_strdup(op_client)));
		}
	}
};

class CCC_Disconnect : public IConsole_Command
{
public:
	CCC_Disconnect(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
	virtual void Execute(const char* args) {
		g_pEventManager->Event.Defer("KERNEL:disconnect");
	}
};

class CCC_VID_Reset : public IConsole_Command
{
public:
	CCC_VID_Reset(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
	virtual void Execute(const char* args) {
		if (Device.b_is_Ready) {
			Device.Reset	();
		}
	}
};

class CCC_VidMode : public CCC_Token
{
	u32		_dummy;
public :
					CCC_VidMode(const char* N) : CCC_Token(N, &_dummy, nullptr) { bEmptyArgsHandled = false; }
	virtual void	Execute(const char* args){
		u32 _w, _h;
		int cnt = sscanf		(args,"%dx%d",&_w,&_h);
		if(cnt==2){
			psCurrentVidMode[0] = _w;
			psCurrentVidMode[1] = _h;
		}else{
			Msg("! Wrong video mode [%s]", args);
		}
	}
	virtual void	Status	(TStatus& S)	
	{ 
		xr_sprintf(S,sizeof(S),"%dx%d",psCurrentVidMode[0],psCurrentVidMode[1]); 
	}
	virtual xr_token* GetToken()				{return vid_mode_token;}
	virtual void	Info	(TInfo& I)
	{	
		xr_strcpy(I,sizeof(I),"change screen resolution WxH");
	}

	virtual void fill_tips(vecTips& tips, u32 mode) {
		TStatus  str, cur;
		Status( cur );

		bool res = false;
		xr_token* tok = GetToken();
		while (tok->name && !res) {
			if (!xr_strcmp(tok->name, cur)) {
				xr_sprintf(str, sizeof(str), "%s  (current)", tok->name);
				tips.emplace_back(str );
				res = true;
			}
			tok++;
		}
		if (!res) {
			tips.emplace_back("---  (current)" );
		}
		tok = GetToken();
		while (tok->name) {
			tips.emplace_back(tok->name );
			tok++;
		}
	}

};

class CCC_SND_Restart : public IConsole_Command
{
public:
	CCC_SND_Restart(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
	virtual void Execute(const char* args) {
		Sound->_restart();
	}
};

float	ps_gamma=1.f,ps_brightness=1.f,ps_contrast=1.f;
class CCC_Gamma : public CCC_Float
{
public:
	CCC_Gamma	(const char* N, float* V) : CCC_Float(N,V,0.5f,1.5f)	{}

	virtual void Execute(const char* args)
	{
		if (Device.IsEditorMode())
		{
			Msg("This command cannot be used in Editors.");
			return;
		}
		CCC_Float::Execute		(args);
		Device.m_pRender->setGamma(ps_gamma);
		Device.m_pRender->setBrightness(ps_brightness);
		Device.m_pRender->setContrast(ps_contrast);
		Device.m_pRender->updateGamma();
	}
};

ENGINE_API bool r2_sun_static = true;

u32	renderer_value	= 0;
class CCC_r2 : public CCC_Token
{
	typedef CCC_Token inherited;
public:
	CCC_r2(const char* N) :inherited(N, &renderer_value, vid_quality_token){renderer_value=0; }
	virtual			~CCC_r2	()
	{
		
	}
	virtual void	Execute	(const char* args)
	{
		tokens					= vid_quality_token;

		inherited::Execute		(args);
		//	0 - r1
		//	1..3 - r2
		//	4 - r3
		psDeviceFlags.set(rsR2, std::string("renderer_r2") == tokens[renderer_value].name);
		psDeviceFlags.set(rsR4, std::string("renderer_r4") == tokens[renderer_value].name);
	}

	virtual void	Save	(IWriter *F)	
	{
		tokens = vid_quality_token;
		inherited::Save(F);
	}

	virtual xr_token* GetToken()
	{
		tokens = vid_quality_token;
		return inherited::GetToken();
	}

};

class CCC_GamepadPrefixOverride : public CCC_Token
{
	typedef CCC_Token inherited;
public:
	CCC_GamepadPrefixOverride(const char* N) : inherited(N, &ps_gamepad_prefix_override, gamepad_prefix_override_token) { ps_gamepad_prefix_override = 0; }
	virtual ~CCC_GamepadPrefixOverride() {}
	virtual void	Execute	(const char* args)
	{
		tokens = gamepad_prefix_override_token;

		inherited::Execute		(args);
		pInput->SelectGamepadPrefix();
		CStringTable::ReparseKeyBindings();
	}

	virtual void	Save	(IWriter *F)	
	{
		tokens = gamepad_prefix_override_token;
		inherited::Save(F);
	}

	virtual xr_token* GetToken()
	{
		tokens = gamepad_prefix_override_token;
		return inherited::GetToken();
	}

};

class CCC_soundDevice : public CCC_Token
{
	typedef CCC_Token inherited;
public:
	CCC_soundDevice(const char* N) :inherited(N, &snd_device_id, nullptr){}
	virtual			~CCC_soundDevice	()
	{}

	virtual void Execute(const char* args)
	{
		GetToken				();
		if(!tokens)				return;
		inherited::Execute		(args);

		::Sound->SwitchAuidoDevice(args);
	}

	virtual void	Status	(TStatus& S)
	{
		GetToken				();
		if(!tokens)				return;
		inherited::Status		(S);
	}

	virtual xr_token* GetToken()
	{
		tokens					= snd_devices_token;
		return inherited::GetToken();
	}

	virtual void Save(IWriter *F)	
	{
		GetToken				();
		if(!tokens)				return;
		inherited::Save			(F);
	}
};

class ENGINE_API CCC_HideConsole : public IConsole_Command
{
public		:
	CCC_HideConsole(const char* N) : IConsole_Command(N)
	{
		bEmptyArgsHandled	= true;
	}

	virtual void	Execute	(const char* args)
	{
		Console->Hide	();
	}
	virtual void	Status	(TStatus& S)
	{
		S[0]			= 0;
	}
	virtual void	Info	(TInfo& I)
	{	
		xr_sprintf		(I,sizeof(I),"hide console");
	}
};

ENGINE_API float psHUD_FOV_def = 33.75f;
ENGINE_API float psHUD_FOV = psHUD_FOV_def;
ENGINE_API bool g_3d_scopes = false;
ENGINE_API bool turn_nvg = false;
ENGINE_API bool ui_3d_cursor = false;
ENGINE_API Fcolor nvg_color;

ENGINE_API int m_look_cam_fp_zoom = 0; // first-person aiming

extern int			rsDVB_Size;
extern int			rsDIB_Size;
extern int			psNET_ClientUpdate;
extern int			psNET_ClientPending;
extern int			psNET_ServerUpdate;
extern int			psNET_ServerPending;
extern int			psNET_DedicatedSleep;
extern char			psNET_Name[32];
extern Flags32		psEnvFlags;
extern int			g_ErrorLineCount;

extern int fps_limit;
extern int main_menu_fps_limit;
extern bool IsFpsShow;
extern float fps_smoothing_alpha;

extern bool use_smoothed_delta;

#ifdef IXRAY_PROFILER
class CCC_Profiler : public IConsole_Command
{
	bool start_profile = false;
public:
	CCC_Profiler(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; };
	virtual void Execute(const char* args)
	{
		if (!start_profile)
		{
			//OPTICK_SET_MEMORY_ALLOCATOR(
			//        [](size_t size) -> void * { return operator new(size); },
			//        [](void *p) { operator delete(p); },
			//        []() { /* Do some TLS initialization here if needed */ }
			//);
			OPTICK_START_CAPTURE(Optick::Mode::Type(Optick::Mode::INSTRUMENTATION | Optick::Mode::TAGS | Optick::Mode::AUTOSAMPLING | Optick::Mode::SWITCH_CONTEXT | Optick::Mode::IO | Optick::Mode::SYS_CALLS | Optick::Mode::OTHER_PROCESSES));
			start_profile = true;
		}
		else
		{
			OPTICK_STOP_CAPTURE();
			shared_str str; str.printf("%s.opt", args ? args : "profile_name");
			OPTICK_SAVE_CAPTURE(str.c_str());
			//OPTICK_SHUTDOWN();
			start_profile = false;
		}
	}
};
#endif
void CCC_Register()
{
	extern XRCORE_API bool ignore_error_window;
	CMD2(CCC_Boolean, "ignore_error_window", &ignore_error_window);
	CMD2(CCC_Boolean, "use_smoothed_delta", &use_smoothed_delta);

	CMD2(CCC_Boolean, "ui_dbg_weather",		&Engine.External.EditorStates[(int)EditorUI::Weather]);
	CMD2(CCC_Boolean, "ui_dbg_draw",		&Engine.External.EditorStates[(int)EditorUI::DebugDraw]);
	CMD2(CCC_Boolean, "ui_dbg_cmd_vars",	&Engine.External.EditorStates[(int)EditorUI::CmdVars]);
	CMD2(CCC_Boolean, "ui_dbg_cmd_console", &Engine.External.EditorStates[(int)EditorUI::CmdConsole]);

#ifdef IXRAY_PROFILER
	CMD1(CCC_Profiler, "profiler_switch");
#endif
	// General
	CMD1(CCC_Help,		"help"					);
	CMD1(CCC_Quit,		"quit"					);
	CMD1(CCC_Start,		"start"					);
	CMD1(CCC_Disconnect,"disconnect"			);
	CMD1(CCC_SaveCFG,	"cfg_save"				);
	CMD1(CCC_LoadCFG,	"cfg_load"				);
	CMD3(CCC_Mask32,		"mt_particles",			&psDeviceFlags,			mtParticles);
#ifdef DEBUG
	CMD1(CCC_MotionsStat,	"stat_motions"		);
	CMD1(CCC_TexturesStat,	"stat_textures"		);
#endif // DEBUG

#ifdef DEBUG

	CMD3(CCC_Mask32,		"mt_sound",				&psDeviceFlags,			mtSound);
	CMD3(CCC_Mask32,		"mt_physics",			&psDeviceFlags,			mtPhysics);
	CMD3(CCC_Mask32,		"mt_network",			&psDeviceFlags,			mtNetwork);
	
	// Events
	CMD1(CCC_E_Dump,	"e_list"				);
	CMD1(CCC_E_Signal,	"e_signal"				);

	CMD3(CCC_Mask32,		"rs_wireframe",			&psDeviceFlags,		rsWireframe);
	CMD3(CCC_Mask32,		"rs_clear_bb",			&psDeviceFlags,		rsClearBB);
	CMD3(CCC_Mask32,		"rs_occlusion",			&psDeviceFlags,		rsOcclusion);
#endif

#ifndef MASTER_GOLD
	CMD3(CCC_Mask32,		"rs_render_details",	&psDeviceFlags,		rsDetails);
	CMD3(CCC_Mask32,		"rs_render_statics",	&psDeviceFlags,		rsDrawStatic);
	CMD3(CCC_Mask32,		"rs_render_dynamics",	&psDeviceFlags,		rsDrawDynamic);
	CMD3(CCC_Mask32,		"rs_render_portals",	&psGameFlags,		rsDrawPortals);
#endif
	// Render device states
	CMD3(CCC_Mask32, "rs_device_active", &psDeviceFlags, rsDeviceActive);

	extern xr_token fps_text_pos_tokens[4];
	extern u32 fps_text_current_pos;

	CMD2(CCC_Boolean, "rs_fps_show", &IsFpsShow)
	CMD4(CCC_Integer, "rs_fps_limit", &fps_limit, 0, 1000)
	CMD4(CCC_Float, "rs_fps_smoothing_factor", &fps_smoothing_alpha, EPS_S, 1.f - EPS_S)
	CMD4(CCC_Integer, "rs_main_menu_fps_limit", &main_menu_fps_limit, 0, 1000)
	CMD3(CCC_Token, "rs_fps_pos", &fps_text_current_pos, fps_text_pos_tokens)


	CMD3(CCC_Mask32,		"rs_v_sync",			&psDeviceFlags,		rsVSync				);
	
#ifdef MASTER_GOLD
	CMD3(CCC_Mask32, "rs_fullscreen", &psDeviceFlags, rsFullscreen);
#endif // MASTER_GOLD

	//CMD3(CCC_Mask32,		"rs_refresh_60hz",		&psDeviceFlags,		rsRefresh60hz			);
	CMD3(CCC_Mask32,		"rs_stats",				&psDeviceFlags,		rsStatistic				);
	CMD4(CCC_Float,		"rs_vis_distance",		&psVisDistance,		0.4f,	1.0f			);
	CMD3(CCC_Mask32,		"r_actor_shadow",		&psGameFlags,		rsActorShadow			);

	CMD3(CCC_Mask32,		"rs_cam_pos",			&psDeviceFlags,		rsCameraPos				);
#ifdef DEBUG_DRAW
	CMD3(CCC_Mask32,		"rs_occ_draw",			&psDeviceFlags,		rsOcclusionDraw			);
	CMD3(CCC_Mask32,		"rs_occ_stats",			&psDeviceFlags,		rsOcclusionStats		);
#endif // DEBUG

	CMD2(CCC_Gamma,		"rs_c_gamma"			,&ps_gamma			);
	CMD2(CCC_Gamma,		"rs_c_brightness"		,&ps_brightness		);
	CMD2(CCC_Gamma,		"rs_c_contrast"			,&ps_contrast		);

	// Texture manager	
	CMD4(CCC_Integer,	"texture_lod",			&psTextureLOD,				0,	4	);
	CMD4(CCC_Integer,	"net_dedicated_sleep",	&psNET_DedicatedSleep,		0,	64	);

	// General video control
	CMD1(CCC_VidMode, "vid_mode");
	CMD3(CCC_Token, "vid_scale_preset", &ps_render_scale_preset, vid_scale_preset_token);
	CMD4(CCC_Float, "vid_scale", &ps_render_scale, 0.3f, 2.0f);
	CMD3(CCC_Token, "vid_scale_mode", &ps_proxy_r_scale_mode, qscale_mode_token);

#ifdef DEBUG
	CMD3(CCC_Token,		"vid_bpp",				&psCurrentBPP,	vid_bpp_token );
#endif // DEBUG

	CMD1(CCC_VID_Reset, "vid_restart"			);
	
	// Sound
	CMD4(CCC_Float,     "snd_compression",      &psSoundCompression, 0.0f, 1.0f);
	CMD2(CCC_Float,		"snd_volume_eff",		&psSoundVEffects);
	CMD2(CCC_Float,		"snd_volume_music",		&psSoundVMusic);
	CMD2(CCC_Float,		"snd_volume_shooting",	&psSoundVShooting);
	CMD1(CCC_SND_Restart,"snd_restart"			);
	CMD3(CCC_Mask32,		"snd_acceleration",		&psSoundFlags,		ss_Hardware	);
	CMD3(CCC_Mask32,		"snd_efx",				&psSoundFlags,		ss_EFX		);
	CMD3(CCC_Mask32,		"snd_hrtf",				&psSoundFlags,		ss_HRTF		);

#ifdef DEBUG
	CMD3(CCC_Mask32,		"snd_stats",			&g_stats_flags,		st_sound	);
	CMD3(CCC_Mask32,		"snd_stats_min_dist",	&g_stats_flags,		st_sound_min_dist );
	CMD3(CCC_Mask32,		"snd_stats_max_dist",	&g_stats_flags,		st_sound_max_dist );
	CMD3(CCC_Mask32,		"snd_stats_ai_dist",	&g_stats_flags,		st_sound_ai_dist );
	CMD3(CCC_Mask32,		"snd_stats_info_name",	&g_stats_flags,		st_sound_info_name );
	CMD3(CCC_Mask32,		"snd_stats_info_object",&g_stats_flags,		st_sound_info_object );

	CMD4(CCC_Integer,	"error_line_count",		&g_ErrorLineCount,	6,	1024	);
#endif // DEBUG

	// Mouse
	CMD2(CCC_Boolean,	"mouse_invert",			&psMouseInvert);
	CMD4(CCC_Float,		"mouse_sens",			&psMouseSens,		0.001f, 0.6f);
	CMD4(CCC_Float,		"mouse_sens_ui",		&psMouseUISens,		0.01f, 2.f);

	CMD4(CCC_Float,		"gamepad_sens",			&psGamepadSens,		0.1f, 0.8f);
	CMD2(CCC_Boolean,	"gamepad_invert",		&psGamepadInvert);
	CMD1(CCC_GamepadPrefixOverride, "gamepad_prefix_override");
	CMD2(CCC_Boolean, "gamepad_vibration", &CGamepadService::FeedbackMode);

	CMD2(CCC_Boolean,	"gyroscope_invert_x",		&psGyroscopeInvertX);
	CMD2(CCC_Boolean,	"gyroscope_invert_y",		&psGyroscopeInvertY);
	CMD2(CCC_Boolean,	"gyroscope_enabled",		&CGamepadService::GyroscopeEnabled);
	CMD4(CCC_Float,		"gyroscope_deadzone",		&CGamepadService::GyroscopeDeadZone, 0.01f, 1.f);
	CMD4(CCC_Float,		"gyroscope_sensitivity",	&CGamepadService::GyroscopeSensitivity, 0.01f, 2.5f);

	CMD4(CCC_Float,		"touchpad_sens",			&psTouchpadSens,		1.0f, 50.0f);

#ifndef MASTER_GOLD
	// Other
	CMD4(CCC_Float,		"developer_float_1",	&devfloat1, -100000.0f, 100000.0f);
	CMD4(CCC_Float,		"developer_float_2",	&devfloat2, -100000.0f, 100000.0f);
	CMD4(CCC_Float,		"developer_float_3",	&devfloat3, -100000.0f, 100000.0f);
	CMD4(CCC_Float,		"developer_float_4",	&devfloat4, -100000.0f, 100000.0f);
#endif

	// Camera
	CMD4(CCC_Float,		"cam_inert", &psCamInert, 0.0f, 100.0f);
	CMD2(CCC_Float,		"cam_slide_inert",		&psCamSlideInert);

	CMD4(CCC_Float, "cam_viewport_near", &Device.fViewportNear, 0.05f, 0.2f);
	CMD4(CCC_Float, "cam_hud_viewport_near", &Device.fHUDViewportNear, 0.001f, 0.1f);

	if(!Device.IsEditorMode()) {
		CMD1(CCC_r2, "renderer");
	}
	else {
		psDeviceFlags.set(rsR2, true);
		psDeviceFlags.set(rsR4, false);
	}

	CMD1(CCC_soundDevice, "snd_device"			);

	psSoundOcclusionScale	= pSettings->r_float	("sound","occlusion_scale");clamp(psSoundOcclusionScale,	0.1f,	.5f);

	extern	int	g_Dump_Export_Obj;
	extern	int	g_Dump_Import_Obj;
	CMD4(CCC_Integer,	"net_dbg_dump_export_obj",	&g_Dump_Export_Obj, 0, 1);
	CMD4(CCC_Integer,	"net_dbg_dump_import_obj",	&g_Dump_Import_Obj, 0, 1);

	extern int g_svDedicateServerUpdateReate;
	CMD4(CCC_Integer, "sv_dedicated_server_update_rate", &g_svDedicateServerUpdateReate, 1, 1000);
	
	
	CMD4(CCC_Float, "sv_shedule_scale", &SheduleScaleDedicated, 0, 5);

	CMD1(CCC_HideConsole,		"hide");

#ifdef	DEBUG
	extern bool debug_destroy;
	CMD2(CCC_Boolean, "debug_destroy", &debug_destroy);
#endif
};
 
