#include "stdafx.h"
#include "xr_input.h"
#include "xr_ioc_cmd.h"
#include "xr_level_controller.h"
#include "string_table.h"
#include "UIGamepadButtons.h"

ENGINE_API _binding	g_key_bindings[bindings_count]; 
ENGINE_API _key_group g_current_keygroup = _sp;

ENGINE_API _action  actions[]		= {
	{ "left",				kLEFT					,_both,			agDefault},
	{ "right",				kRIGHT					,_both,			agDefault},
	{ "up",					kUP						,_both,			agDefault},
	{ "down",				kDOWN					,_both,			agDefault},
	{ "jump",				kJUMP					,_both,			agDefault},
	{ "crouch",				kCROUCH					,_both,			agDefault},
	{ "accel",				kACCEL					,_both,			agDefault},
	{ "sprint_toggle",  	kSPRINT_TOGGLE  		,_both,			agDefault},
																
	{ "forward",			kFWD					,_both,			agDefault},
	{ "back",				kBACK					,_both,			agDefault},
	{ "lstrafe",			kL_STRAFE				,_both,			agDefault},
	{ "rstrafe",			kR_STRAFE				,_both,			agDefault},
																
	{ "llookout",			kL_LOOKOUT				,_both,			agAiming},
	{ "rlookout",			kR_LOOKOUT				,_both,			agAiming},
																
	{ "cam_1",				kCAM_1					,_both,			agDefault},
	{ "cam_2",				kCAM_2					,_both,			agDefault},
	{ "cam_3",				kCAM_3					,_both,			agDefault},
	{ "cam_zoom_in",		kCAM_ZOOM_IN			,_both,			agDefault},
	{ "cam_zoom_out",		kCAM_ZOOM_OUT			,_both,			agDefault},
															
	{ "torch",				kTORCH					,_both,			agDefault},
	{ "night_vision",		kNIGHT_VISION			,_both,			agUIRadialWeapon},
	{ "show_detector",		kDETECTOR				,_both,			agDefault},

	{ "wpn_1",				kWPN_1					,_both,			agDefault},
	{ "wpn_2",				kWPN_2					,_both,			agDefault},
	{ "wpn_3",				kWPN_3					,_both,			agDefault},
	{ "wpn_4",				kWPN_4					,_both,			agDefault},
	{ "wpn_5",				kWPN_5					,_both,			agDefault},
	{ "wpn_6",				kWPN_6					,_both,			agDefault},
	{ "wpn_7",				kWPN_7					,_both,			agDefault},
	{ "artefact",			kARTEFACT				,_both,			agDefault},
	{ "wpn_next",			kWPN_NEXT				,_both,			agUIRadialWeapon},	// means next ammo type
	{ "wpn_fire",			kWPN_FIRE				,_both,			agDefault},
	{ "wpn_zoom",			kWPN_ZOOM				,_both,			agDefault},
	{ "wpn_zoom_inc",		kWPN_ZOOM_INC			,_both,			agAiming},
	{ "wpn_zoom_dec",		kWPN_ZOOM_DEC			,_both,			agAiming},
	{ "wpn_reload",			kWPN_RELOAD				,_both,			agDefault},
	{ "wpn_func",			kWPN_FUNC				,_both,			agUIRadialWeapon},
	{ "wpn_firemode_prev",	kWPN_FIREMODE_PREV		,_both,			agDefault},
	{ "wpn_firemode_next",	kWPN_FIREMODE_NEXT		,_both,			agUIRadialWeapon},
															
	{ "pause",				kPAUSE					,_both,			agDefault},
	{ "drop",				kDROP					,_both,			agUIRadialWeapon},
	{ "use",				kUSE					,_both,			agDefault},
	{ "scores",				kSCORES					,_both,			agDefault},
	{ "chat",				kCHAT					,_mp,			agDefault},
	{ "chat_team",			kCHAT_TEAM				,_mp,			agDefault},
	{ "voice_chat",         kVOICE_CHAT             ,_mp,			agDefault},
	{ "voice_distance",     kVOICE_DISTANCE         ,_mp,			agDefault},
	{ "screenshot",			kSCREENSHOT				,_both,			agDefault},
	{ "quit",				kQUIT					,_both,			agDefault},
	{ "console",			kCONSOLE				,_both,			agDefault},
	{ "inventory",			kINVENTORY				,_both,			agDefault},
	{ "buy_menu",			kBUY					,_mp,			agDefault},
	{ "skin_menu",			kSKIN					,_mp,			agDefault},
	{ "team_menu",			kTEAM					,_mp,			agDefault},
	{ "active_jobs",		kACTIVE_JOBS			,_both,			agDefault},
	{ "map",				kMAP					,_both,			agDefault},
	{ "contacts",			kCONTACTS				,_sp,			agDefault},

	{ "vote_begin",			kVOTE_BEGIN				,_mp,			agDefault},
	{ "show_admin_menu",	kSHOW_ADMIN_MENU		,_mp,			agDefault},
	{ "vote",				kVOTE					,_mp,			agDefault},
	{ "vote_yes",			kVOTEYES				,_mp,			agDefault},
	{ "vote_no",			kVOTENO					,_mp,			agDefault},
																
	{ "next_slot",			kNEXT_SLOT				,_both,			agDefault},
	{ "prev_slot",			kPREV_SLOT				,_both,			agDefault},
															
	{ "speech_menu_0",		kSPEECH_MENU_0			,_mp,			agDefault},
	{ "speech_menu_1",		kSPEECH_MENU_1			,_mp,			agDefault},
																
	{ "quick_use_1",		kQUICK_USE_1			,_both,			agDefault},
	{ "quick_use_2",		kQUICK_USE_2			,_both,			agDefault},
	{ "quick_use_3",		kQUICK_USE_3			,_both,			agDefault},
	{ "quick_use_4",		kQUICK_USE_4			,_both,			agDefault},

	{ "quick_save",			kQUICK_SAVE				,_sp,			agDefault},
	{ "quick_load",			kQUICK_LOAD				,_sp,			agDefault},
	{ "alife_command",		kALIFE_CMD				,_sp,			agDefault},
	{ "use_bandage",		kUSE_BANDAGE			,_sp,			agDefault},
	{ "use_medkit",			kUSE_MEDKIT				,_sp,			agDefault},
	{ "turn_engine",		kENGINE					,_sp,			agTransport},
	{ "brake",				kBRAKE					,_sp,			agTransport},
	{ "transmission_up",	kTRANSMISSION_UP		,_sp,			agTransport},
	{ "transmission_down",	kTRANSMISSION_DOWN		,_sp,			agTransport},
	
	{ "ui_tab_left",		kUI_TAB_LEFT,			_both,			agUIGeneral},
	{ "ui_tab_right",		kUI_TAB_RIGHT,			_both,			agUIGeneral},
	{ "ui_tab_sec_left",	kUI_TAB_SECONDARY_LEFT,	_both,			agUIGeneral},
	{ "ui_tab_sec_right",	kUI_TAB_SECONDARY_RIGHT,_both,			agUIGeneral},
	{ "ui_left",			kUI_LEFT,				_both,			agUIGeneral},
	{ "ui_right",			kUI_RIGHT,				_both,			agUIGeneral},
	{ "ui_up",				kUI_UP,					_both,			agUIGeneral},
	{ "ui_down",			kUI_DOWN,				_both,			agUIGeneral},
	{ "ui_sec_left",		kUI_SECONDARY_LEFT,		_both,			agUIGeneral},
	{ "ui_sec_right",		kUI_SECONDARY_RIGHT,	_both,			agUIGeneral},
	{ "ui_sec_up",			kUI_SECONDARY_UP,		_both,			agUIGeneral},
	{ "ui_sec_down",		kUI_SECONDARY_DOWN,		_both,			agUIGeneral},
	{ "ui_accept",			kUI_ACCEPT,				_both,			agUIGeneral},
	{ "ui_back",			kUI_BACK,				_both,			agUIGeneral},
	{ "ui_hint",			kUI_HINT,				_both,			agUIGeneral},
	{ "ui_action_1",		kUI_ACTION_1,			_both,			agUIGeneral},
	{ "ui_action_2",		kUI_ACTION_2,			_both,			agUIGeneral},
	{ "inv_sort_prev",		kINV_SORT_PREV,			_both,			agUIGeneral},
	{ "inv_sort_next",		kINV_SORT_NEXT,			_both,			agUIGeneral},

	{ "pda_log_to_start",	kPDA_LOG_TO_START,		_both,			agUILogMenu},
	{ "pda_log_to_end",	 	kPDA_LOG_TO_END,		_both,			agUILogMenu},
	{ "pda_log_scroll_down",kPDA_LOG_SCROLL_DOWN,	_both,			agUILogMenu},
	{ "pda_log_scroll_up",	kPDA_LOG_SCROLL_UP,		_both,			agUILogMenu},
	{ "pda_log_date_prev",	kPDA_LOG_DATE_PREV,		_both,			agUILogMenu},
	{ "pda_log_date_next",	kPDA_LOG_DATE_NEXT,		_both,			agUILogMenu},
	{ "pda_log_show_news", 	kPDA_LOG_SHOW_NEWS,		_both,			agUILogMenu},
	{ "pda_log_show_dialogs", 	kPDA_LOG_SHOW_DIALOGS, _both,		agUILogMenu},

	{ "pda_tasks_toggle_list",		kPDA_TASKS_TOGGLE_LIST,			_both, agUITaskMenu},
	{ "pda_tasks_toggle_legend",	kPDA_TASKS_TOGGLE_LEGEND,		_both, agUITaskMenu},
	{ "pda_tasks_next",				kPDA_TASKS_NEXT,				_both, agUITaskMenu},
	{ "pda_tasks_prev",				kPDA_TASKS_PREV,				_both, agUITaskMenu},
	{ "pda_tasks_toggle_map",		kPDA_TASKS_TOGGLE_MAP,			_both, agUITaskMenu},
	{ "pda_tasks_map_show_me",		kPDA_TASKS_MAP_SHOW_ME,			_both, agUITaskMenu},
	{ "pda_tasks_map_zoom_in",		kPDA_TASKS_MAP_ZOOM_IN,			_both, agUITaskMenu},
	{ "pda_tasks_map_zoom_out",		kPDA_TASKS_MAP_ZOOM_OUT,		_both, agUITaskMenu},
	{ "pda_tasks_filter_next",		kPDA_TASKS_FILTER_NEXT,			_both, agUITaskMenu},
	{ "pda_tasks_filter_prev",		kPDA_TASKS_FILTER_PREV,			_both, agUITaskMenu},
	{ "pda_tasks_filter_toggle",	kPDA_TASKS_FILTER_TOGGLE,		_both, agUITaskMenu},

	{ "custom1",			kCUSTOM1				,_sp,			agDefault},
	{ "custom2",			kCUSTOM2				,_sp,			agDefault},
	{ "custom3",			kCUSTOM3				,_sp,			agDefault},
	{ "custom4",			kCUSTOM4				,_sp,			agDefault},
	{ "custom5",			kCUSTOM5				,_sp,			agDefault},
	{ "custom6",			kCUSTOM6				,_sp,			agDefault},
	{ "custom7",			kCUSTOM7				,_sp,			agDefault},
	{ "custom8",			kCUSTOM8				,_sp,			agDefault},
	{ "custom9",			kCUSTOM9				,_sp,			agDefault},
	{ "custom10",			kCUSTOM10				,_sp,			agDefault},
	{ "custom11",			kCUSTOM11				,_sp,			agDefault},
	{ "custom12",			kCUSTOM12				,_sp,			agDefault},
	{ "custom13",			kCUSTOM13				,_sp,			agDefault},
	{ "custom14",			kCUSTOM14				,_sp,			agDefault},
	{ "custom15",			kCUSTOM15				,_sp,			agDefault},
	{ "cam_autoaim",		kCAM_AUTOAIM			,_sp,			agDefault},

	{ "clear_gasmask",		kCLEARGASMASK			,_both,			agDefault},
	{ "wpn_torch",			kTACTICALTORCH			,_both,			agDefault},
	{ "wpn_laser",			kLASER					,_both,			agDefault},
	{ "wpn_zoom_alter",		kWPN_ZOOM_ALTER			,_both,			agDefault},
	{ "scope_brightness_plus",kBRIGHTNESS_PLUS		,_both,			agDefault},
	{ "scope_brightness_minus",kBRIGHTNESS_MINUS	,_both,			agDefault},
	{ "quick_grenade",		kQUICK_GRENADE			,_both,			agDefault},
	{ "quick_kick",			kQUICK_KICK				,_both,			agDefault},
	{ "wpn_magcheck",		kMAG_CHECK				,_both,			agDefault},
	{ "wpn_firecheck",		kFIREMODE_CHECK			,_both,			agDefault},
	{ "show_quick_slots",   kSHOW_QUICK_SLOTS		,_both,			agDefault},
	{ "wpn_chamber_load",   kWPN_CHAMBER_LOAD		,_both,			agDefault},
	{ "wpn_chamber_unload", kWPN_CHAMBER_UNLOAD     ,_both,			agDefault},
	{ "wpn_chamber_check",  kWPN_CHAMBER_CHECK      ,_both,			agDefault},

	{ "wpn_radial_menu",	kWPN_RADIAL_MENU		,_both,			agDefault},
	{ "actormenu_action",	kACTORMENU_ACTION		,_both,			agUIGeneral},
	{ "safemode",			kSAFEMODE				,_both,			agDefault },

	{ nullptr, 				kLASTACTION				,_both,			agDefault}
};															

_keyboard keyboards[] = {
	{ "kA",                     SDL_SCANCODE_A,                  "A" },
	{ "kB",                     SDL_SCANCODE_B,                  "B" },
	{ "kC",                     SDL_SCANCODE_C,                  "C" },
	{ "kD",                     SDL_SCANCODE_D,                  "D" },
	{ "kE",                     SDL_SCANCODE_E,                  "E" },
	{ "kF",                     SDL_SCANCODE_F,                  "F" },
	{ "kG",                     SDL_SCANCODE_G,                  "G" },
	{ "kH",                     SDL_SCANCODE_H,                  "H" },
	{ "kI",                     SDL_SCANCODE_I,                  "I" },
	{ "kJ",                     SDL_SCANCODE_J,                  "J" },
	{ "kK",                     SDL_SCANCODE_K,                  "K" },
	{ "kL",                     SDL_SCANCODE_L,                  "L" },
	{ "kM",                     SDL_SCANCODE_M,                  "M" },
	{ "kN",                     SDL_SCANCODE_N,                  "N" },
	{ "kO",                     SDL_SCANCODE_O,                  "O" },
	{ "kP",                     SDL_SCANCODE_P,                  "P" },
	{ "kQ",                     SDL_SCANCODE_Q,                  "Q" },
	{ "kR",                     SDL_SCANCODE_R,                  "R" },
	{ "kS",                     SDL_SCANCODE_S,                  "S" },
	{ "kT",                     SDL_SCANCODE_T,                  "T" },
	{ "kU",                     SDL_SCANCODE_U,                  "U" },
	{ "kV",                     SDL_SCANCODE_V,                  "V" },
	{ "kW",                     SDL_SCANCODE_W,                  "W" },
	{ "kX",                     SDL_SCANCODE_X,                  "X" },
	{ "kY",                     SDL_SCANCODE_Y,                  "Y" },
	{ "kZ",                     SDL_SCANCODE_Z,                  "Z" },

	{ "k1",                     SDL_SCANCODE_1,                  "1" },
	{ "k2",                     SDL_SCANCODE_2,                  "2" },
	{ "k3",                     SDL_SCANCODE_3,                  "3" },
	{ "k4",                     SDL_SCANCODE_4,                  "4" },
	{ "k5",                     SDL_SCANCODE_5,                  "5" },
	{ "k6",                     SDL_SCANCODE_6,                  "6" },
	{ "k7",                     SDL_SCANCODE_7,                  "7" },
	{ "k8",                     SDL_SCANCODE_8,                  "8" },
	{ "k9",                     SDL_SCANCODE_9,                  "9" },
	{ "k0",                     SDL_SCANCODE_0,                  "0" },

	{ "kRETURN",                SDL_SCANCODE_RETURN,             "Return" },
	{ "kESCAPE",                SDL_SCANCODE_ESCAPE,             "Escape" },
	{ "kBACK",                  SDL_SCANCODE_BACKSPACE,          "Backspace" },
	{ "kTAB",                   SDL_SCANCODE_TAB,                "Tab" },
	{ "kSPACE",                 SDL_SCANCODE_SPACE,              "Space" },

	{ "kMINUS",                 SDL_SCANCODE_MINUS,              "Minus" },
	{ "kEQUALS",                SDL_SCANCODE_EQUALS,             "Equals" },
	{ "kLBRACKET",              SDL_SCANCODE_LEFTBRACKET,        "Left bracket" },
	{ "kRBRACKET",              SDL_SCANCODE_RIGHTBRACKET,       "Right bracket" },
	{ "kBACKSLASH",             SDL_SCANCODE_BACKSLASH,          "Backslash" },
	{ "kNONUSHASH",             SDL_SCANCODE_NONUSHASH,          "Non US Hash" },

	{ "kSEMICOLON",             SDL_SCANCODE_SEMICOLON,          "Semicolon" },
	{ "kAPOSTROPHE",            SDL_SCANCODE_APOSTROPHE,         "Apostrophe" },
	{ "kGRAVE",                 SDL_SCANCODE_GRAVE,              "Grave" },
	{ "kCOMMA",                 SDL_SCANCODE_COMMA,              "Comma" },
	{ "kPERIOD",                SDL_SCANCODE_PERIOD,             "Period" },
	{ "kSLASH",                 SDL_SCANCODE_SLASH,              "Slash" },

	{ "kCAPITAL",               SDL_SCANCODE_CAPSLOCK,           "Caps Lock" },

	{ "kF1",                    SDL_SCANCODE_F1,                 "F1" },
	{ "kF2",                    SDL_SCANCODE_F2,                 "F2" },
	{ "kF3",                    SDL_SCANCODE_F3,                 "F3" },
	{ "kF4",                    SDL_SCANCODE_F4,                 "F4" },
	{ "kF5",                    SDL_SCANCODE_F5,                 "F5" },
	{ "kF6",                    SDL_SCANCODE_F6,                 "F6" },
	{ "kF7",                    SDL_SCANCODE_F7,                 "F7" },
	{ "kF8",                    SDL_SCANCODE_F8,                 "F8" },
	{ "kF9",                    SDL_SCANCODE_F9,                 "F9" },
	{ "kF10",                   SDL_SCANCODE_F10,                "F10" },
	{ "kF11",                   SDL_SCANCODE_F11,                "F11" },
	{ "kF12",                   SDL_SCANCODE_F12,                "F12" },

	{ "kPRINTSCREEN",           SDL_SCANCODE_PRINTSCREEN,        "Print Screen" },
	{ "kSCROLL",                SDL_SCANCODE_SCROLLLOCK,         "Scroll Lock" },
	{ "kPAUSE",                 SDL_SCANCODE_PAUSE,              "Pause" },
	{ "kINSERT",                SDL_SCANCODE_INSERT,             "Insert" },

	{ "kHOME",                  SDL_SCANCODE_HOME,               "Home" },
	{ "kPGUP",                  SDL_SCANCODE_PAGEUP,             "Page Up" },
	{ "kDELETE",                SDL_SCANCODE_DELETE,             "Delete" },
	{ "kEND",                   SDL_SCANCODE_END,                "End" },
	{ "kPGDN",                  SDL_SCANCODE_PAGEDOWN,           "Page Down" },

	{ "kRIGHT",                 SDL_SCANCODE_RIGHT,              "Right" },
	{ "kLEFT",                  SDL_SCANCODE_LEFT,               "Left" },
	{ "kDOWN",                  SDL_SCANCODE_DOWN,               "Down" },
	{ "kUP",                    SDL_SCANCODE_UP,                 "Up" },

	{ "kNUMLOCK",               SDL_SCANCODE_NUMLOCKCLEAR,       "Num Lock" },

	{ "kDIVIDE",                SDL_SCANCODE_KP_DIVIDE,          "Numpad Divide" },
	{ "kMULTIPLY",              SDL_SCANCODE_KP_MULTIPLY,        "Numpad Multiply" },
	{ "kSUBTRACT",              SDL_SCANCODE_KP_MINUS,           "Numpad Minus" },
	{ "kADD",                   SDL_SCANCODE_KP_PLUS,            "Numpad Plus" },
	{ "kNUMPADENTER",           SDL_SCANCODE_KP_ENTER,           "Numpad Enter" },

	{ "kNUMPAD1",               SDL_SCANCODE_KP_1,               "Numpad 1" },
	{ "kNUMPAD2",               SDL_SCANCODE_KP_2,               "Numpad 2" },
	{ "kNUMPAD3",               SDL_SCANCODE_KP_3,               "Numpad 3" },
	{ "kNUMPAD4",               SDL_SCANCODE_KP_4,               "Numpad 4" },
	{ "kNUMPAD5",               SDL_SCANCODE_KP_5,               "Numpad 5" },
	{ "kNUMPAD6",               SDL_SCANCODE_KP_6,               "Numpad 6" },
	{ "kNUMPAD7",               SDL_SCANCODE_KP_7,               "Numpad 7" },
	{ "kNUMPAD8",               SDL_SCANCODE_KP_8,               "Numpad 8" },
	{ "kNUMPAD9",               SDL_SCANCODE_KP_9,               "Numpad 9" },
	{ "kNUMPAD0",               SDL_SCANCODE_KP_0,               "Numpad 0" },

	{ "kNUMPADPERIOD",          SDL_SCANCODE_KP_PERIOD,          "Numpad Period" },
	{ "kNONUSBACKSLASH",        SDL_SCANCODE_NONUSBACKSLASH,     "Non US Backslash" },
	{ "kAPPLICATION",           SDL_SCANCODE_APPLICATION,        "Application" },
	{ "kPOWER",                 SDL_SCANCODE_POWER,              "Power" },
	{ "kNUMPADEQUALS",          SDL_SCANCODE_KP_EQUALS,          "Numpad Equals" },

	{ "kF13",                   SDL_SCANCODE_F13,                "F13" },
	{ "kF14",                   SDL_SCANCODE_F14,                "F14" },
	{ "kF15",                   SDL_SCANCODE_F15,                "F15" },
	{ "kF16",                   SDL_SCANCODE_F16,                "F16" },
	{ "kF17",                   SDL_SCANCODE_F17,                "F17" },
	{ "kF18",                   SDL_SCANCODE_F18,                "F18" },
	{ "kF19",                   SDL_SCANCODE_F19,                "F19" },
	{ "kF20",                   SDL_SCANCODE_F20,                "F20" },
	{ "kF21",                   SDL_SCANCODE_F21,                "F21" },
	{ "kF22",                   SDL_SCANCODE_F22,                "F22" },
	{ "kF23",                   SDL_SCANCODE_F23,                "F23" },
	{ "kF24",                   SDL_SCANCODE_F24,                "F24" },

	{ "kEXECUTE",               SDL_SCANCODE_EXECUTE,            "Execute" },
	{ "kHELP",                  SDL_SCANCODE_HELP,               "Help" },
	{ "kMENU",                  SDL_SCANCODE_MENU,               "Menu" },

	{ "kSELECT",                SDL_SCANCODE_SELECT,             "Select" },
	{ "kSTOP",                  SDL_SCANCODE_STOP,               "Stop" },

	{ "kREDO",                  SDL_SCANCODE_AGAIN,              "Redo" },
	{ "kUNDO",                  SDL_SCANCODE_UNDO,               "Undo" },

	{ "kCUT",                   SDL_SCANCODE_CUT,                "Cut" },
	{ "kCOPY",                  SDL_SCANCODE_COPY,               "Copy" },
	{ "kPASTE",                 SDL_SCANCODE_PASTE,              "Paste" },

	{ "kFIND",                  SDL_SCANCODE_FIND,               "Find" },

	{ "kMUTE",                  SDL_SCANCODE_MUTE,               "Mute" },
	{ "kVOLUMEUP",              SDL_SCANCODE_VOLUMEUP,           "Volume Up" },
	{ "kVOLUMEDOWN",            SDL_SCANCODE_VOLUMEDOWN,         "Volume Down" },

	{ "kNUMPADCOMMA",           SDL_SCANCODE_KP_COMMA,           "Numpad Comma" },
	{ "kNUMPADEQUALSAS400",     SDL_SCANCODE_KP_EQUALSAS400,     "Equals AS400" },

	{ "kINTERNATIONAL1",        SDL_SCANCODE_INTERNATIONAL1,     "kINTERNATIONAL1" },
	{ "kINTERNATIONAL2",        SDL_SCANCODE_INTERNATIONAL2,     "kINTERNATIONAL2" },
	{ "kYEN",                   SDL_SCANCODE_INTERNATIONAL3,     "Yen" },
	{ "kINTERNATIONAL4",        SDL_SCANCODE_INTERNATIONAL4,     "kINTERNATIONAL4" },
	{ "kINTERNATIONAL5",        SDL_SCANCODE_INTERNATIONAL5,     "kINTERNATIONAL5" },
	{ "kINTERNATIONAL6",        SDL_SCANCODE_INTERNATIONAL6,     "kINTERNATIONAL6" },
	{ "kINTERNATIONAL7",        SDL_SCANCODE_INTERNATIONAL7,     "kINTERNATIONAL7" },
	{ "kINTERNATIONAL8",        SDL_SCANCODE_INTERNATIONAL8,     "kINTERNATIONAL8" },
	{ "kINTERNATIONAL9",        SDL_SCANCODE_INTERNATIONAL9,     "kINTERNATIONAL9" },

	{ "kHANGUL",                SDL_SCANCODE_LANG1,              "Hangul" },
	{ "kHANJA",                 SDL_SCANCODE_LANG2,              "Hanja" },
	{ "kKATAKANA",              SDL_SCANCODE_LANG3,              "Katakana" },
	{ "kHIRAGANA",              SDL_SCANCODE_LANG4,              "Hiragana" },
	{ "kZENHANKAKU",            SDL_SCANCODE_LANG5,              "Zen-Han-kaku" },
	{ "kLANG6",                 SDL_SCANCODE_LANG6,              "kLANG6"},
	{ "kLANG7",                 SDL_SCANCODE_LANG7,              "kLANG7" },
	{ "kLANG8",                 SDL_SCANCODE_LANG8,              "kLANG8"},
	{ "kLANG9",                 SDL_SCANCODE_LANG9,              "kLANG9"},

	{ "kALTERASE",              SDL_SCANCODE_ALTERASE,           "Alterase" },
	{ "kCANCEL",                SDL_SCANCODE_CANCEL,             "Cancel" },
	{ "kCLEAR",                 SDL_SCANCODE_CLEAR,              "Clear" },
	{ "kPRIOR",                 SDL_SCANCODE_PRIOR,              "Prior" },
	{ "kRETURN2",               SDL_SCANCODE_RETURN2,            "Return 2" },
	{ "kSEPARATOR",             SDL_SCANCODE_SEPARATOR,          "Separator" },
	{ "kOUT",                   SDL_SCANCODE_OUT,                "Out" },
	{ "kOPER",                  SDL_SCANCODE_OPER,               "Oper" },
	{ "kCLEARAGAIN",            SDL_SCANCODE_CLEARAGAIN,         "Clear Again" },
	{ "kCRSEL",                 SDL_SCANCODE_CRSEL,              "Crsel" },
	{ "kEXSEL",                 SDL_SCANCODE_EXSEL,              "Excel" },

	{ "kNUMPAD_00",             SDL_SCANCODE_KP_00,              "Numpad 00" },
	{ "kNUMPAD_000",            SDL_SCANCODE_KP_000,             "Numpad 000" },
	{ "kTHOUSANDSSEPARATOR",    SDL_SCANCODE_THOUSANDSSEPARATOR, "Thousand Separator" },
	{ "kDECIMALSEPARATOR",      SDL_SCANCODE_DECIMALSEPARATOR,   "Decimal Separator" },
	{ "kCURRENCYUNIT",          SDL_SCANCODE_CURRENCYUNIT,       "Currency Unit" },
	{ "kCURRENCYSUBUNIT",       SDL_SCANCODE_CURRENCYSUBUNIT,    "Currency Subunit" },

	{ "kNUMPAD_LEFTPAREN",      SDL_SCANCODE_KP_LEFTPAREN,       "Numpad Left Paren" },
	{ "kNUMPAD_RIGHTPAREN",     SDL_SCANCODE_KP_RIGHTPAREN,      "Numpad Right Paren" },
	{ "kNUMPAD_LEFTBRACE",      SDL_SCANCODE_KP_LEFTBRACE,       "Numpad Left Brace" },
	{ "kNUMPAD_RIGHTBRACE",     SDL_SCANCODE_KP_RIGHTBRACE,      "Numpad Right Brace" },
	{ "kNUMPAD_TAB",            SDL_SCANCODE_KP_TAB,             "Numpad Tab" },
	{ "kNUMPAD_BACKSPACE",      SDL_SCANCODE_KP_BACKSPACE,       "Numpad Backspace" },

	{ "kNUMPAD_A",              SDL_SCANCODE_KP_A,               "Numpad A" },
	{ "kNUMPAD_B",              SDL_SCANCODE_KP_B,               "Numpad B" },
	{ "kNUMPAD_C",              SDL_SCANCODE_KP_C,               "Numpad C" },
	{ "kNUMPAD_D",              SDL_SCANCODE_KP_D,               "Numpad D" },
	{ "kNUMPAD_E",              SDL_SCANCODE_KP_E,               "Numpad E" },
	{ "kNUMPAD_F",              SDL_SCANCODE_KP_F,               "Numpad F" },

	{ "kNUMPAD_XOR",            SDL_SCANCODE_KP_XOR,             "Numpad XOR" },

	{ "kNUMPAD_POWER",          SDL_SCANCODE_KP_POWER,           "Numpad Power" },
	{ "kNUMPAD_PERCENT",        SDL_SCANCODE_KP_PERCENT,         "Numpad Percent" },

	{ "kNUMPAD_LESS",           SDL_SCANCODE_KP_LESS,            "Numpad Less" },
	{ "kNUMPAD_GREATER",        SDL_SCANCODE_KP_GREATER,         "Numpad Greater" },

	{ "kNUMPAD_AMPERSAND",      SDL_SCANCODE_KP_AMPERSAND,       "Numpad Ampersand" },
	{ "kNUMPAD_DBLAMPERSAND",   SDL_SCANCODE_KP_DBLAMPERSAND,    "Numpad Double Ampersand" },

	{ "kNUMPAD_VERTICALBAR",    SDL_SCANCODE_KP_VERTICALBAR,     "Numpad Vertical Bar" },
	{ "kNUMPAD_DBLVERTICALBAR", SDL_SCANCODE_KP_DBLVERTICALBAR,  "Numpad Double Vertical Bar" },

	{ "kNUMPAD_COLON",          SDL_SCANCODE_KP_COLON,           "Numpad Colon" },
	{ "kNUMPAD_HASH",           SDL_SCANCODE_KP_HASH,            "Numpad Hash" },
	{ "kNUMPAD_SPACE",          SDL_SCANCODE_KP_SPACE,           "Numpad Space" },
	{ "kNUMPAD_AT",             SDL_SCANCODE_KP_AT,              "Numpad At" },
	{ "kNUMPAD_EXCLAM",         SDL_SCANCODE_KP_EXCLAM,          "Numpad Exclam" },

	{ "kNUMPAD_MEMSTORE",       SDL_SCANCODE_KP_MEMSTORE,        "Numpad Mem Store" },
	{ "kNUMPAD_MEMRECALL",      SDL_SCANCODE_KP_MEMRECALL,       "Numpad Mem Recall" },
	{ "kNUMPAD_MEMCLEAR",       SDL_SCANCODE_KP_MEMCLEAR,        "Numpad Mem Clear" },
	{ "kNUMPAD_MEMADD",         SDL_SCANCODE_KP_MEMADD,          "Numpad Mem Add" },
	{ "kNUMPAD_MEMSUBTRACT",    SDL_SCANCODE_KP_MEMSUBTRACT,     "Numpad Mem Subtract" },
	{ "kNUMPAD_MEMMULTIPLY",    SDL_SCANCODE_KP_MEMMULTIPLY,     "Numpad Mem Multiply" },
	{ "kNUMPAD_MEMDIVIDE",      SDL_SCANCODE_KP_MEMDIVIDE,       "Numpad Mem Divide" },

	{ "kNUMPAD_PLUSMINUS",      SDL_SCANCODE_KP_PLUSMINUS,       "Numpad Plus-Minus" },
	{ "kNUMPAD_CLEAR",          SDL_SCANCODE_KP_CLEAR,           "Numpad Clear" },
	{ "kNUMPAD_CLEARENTRY",     SDL_SCANCODE_KP_CLEARENTRY,      "Numpad Clear Entry" },
	{ "kNUMPAD_BINARY",         SDL_SCANCODE_KP_BINARY,          "Numpad Binary" },
	{ "kNUMPAD_OCTAL",          SDL_SCANCODE_KP_OCTAL,           "Numpad Octal" },
	{ "kNUMPAD_DECIMAL",        SDL_SCANCODE_KP_DECIMAL,         "Numpad Decimal" },
	{ "kNUMPAD_HEXADECIMAL",    SDL_SCANCODE_KP_HEXADECIMAL,     "Numpad Hexadecimal" },

	{ "kLCONTROL",              SDL_SCANCODE_LCTRL,              "Left Ctrl" },
	{ "kLSHIFT",                SDL_SCANCODE_LSHIFT,             "Left shift" },
	{ "kLMENU",                 SDL_SCANCODE_LALT,               "Left Alt" },
	{ "kLWIN",                  SDL_SCANCODE_LGUI,               "Left Windows" },
	{ "kRCONTROL",              SDL_SCANCODE_RCTRL,              "Right Ctrl" },
	{ "kRSHIFT",                SDL_SCANCODE_RSHIFT,             "Right Shift" },
	{ "kRMENU",                 SDL_SCANCODE_RALT,               "Right Alt" },
	{ "kRWIN",                  SDL_SCANCODE_RGUI,               "Right Windows" },

	{ "kMODE",                  SDL_SCANCODE_MODE,               "Mode" },

	{ "kAUDIONEXT",             SDL_SCANCODE_MEDIA_NEXT_TRACK,   "Audio Next" },
	{ "kAUDIOPREV",             SDL_SCANCODE_MEDIA_PREVIOUS_TRACK,"Audio Prev" },
	{ "kAUDIOSTOP",             SDL_SCANCODE_MEDIA_STOP,         "Audio Stop" },
	{ "kAUDIOPLAY",             SDL_SCANCODE_MEDIA_PLAY,         "Audio Play" },
	{ "kAUDIOMUTE",             SDL_SCANCODE_MUTE,               "Audio Mute" },

	{ "kMEDIASELECT",           SDL_SCANCODE_MEDIA_SELECT,       "Media Select" },

	{ "kNUMPAD_AC_SEARCH",      SDL_SCANCODE_AC_SEARCH,          "AC Search" },
	{ "kNUMPAD_AC_HOME",        SDL_SCANCODE_AC_HOME,            "AC Home" },
	{ "kNUMPAD_AC_BACK",        SDL_SCANCODE_AC_BACK,            "AC Back" },
	{ "kNUMPAD_AC_FORWARD",     SDL_SCANCODE_AC_FORWARD,         "AC Forward" },
	{ "kNUMPAD_AC_STOP",        SDL_SCANCODE_AC_STOP,            "AC Stop" },
	{ "kNUMPAD_AC_REFRESH",     SDL_SCANCODE_AC_REFRESH,         "AC Refresh" },
	{ "kNUMPAD_AC_BOOKMARKS",   SDL_SCANCODE_AC_BOOKMARKS,       "AC Bookmarks" },

	{ "kEJECT",                 SDL_SCANCODE_MEDIA_EJECT,        "Eject" },
	{ "kSLEEP",                 SDL_SCANCODE_SLEEP,              "Sleep" },

	{ "mouse1",                 MOUSE_1,                         "Left mouse button" },
	{ "mouse2",                 MOUSE_2,                         "Right mouse button" },
	{ "mouse3",                 MOUSE_3,                         "Mouse wheel button" },
	{ "mouse4",                 MOUSE_4,                         "Mouse X1" },
	{ "mouse5",                 MOUSE_5,                         "Mouse X2" },

	{ nullptr,                  0}
};


_keyboard gamepads[] = 
{
	{ "cA",                     SDL_GAMEPAD_BUTTON_SOUTH,         "A" },
	{ "cB",                     SDL_GAMEPAD_BUTTON_EAST,          "B" },
	{ "cX",                     SDL_GAMEPAD_BUTTON_WEST,          "X" },
	{ "cY",                     SDL_GAMEPAD_BUTTON_NORTH,         "Y" },
	{ "cBACK",                  SDL_GAMEPAD_BUTTON_BACK,          "Back" },
	{ "cSTART",                 SDL_GAMEPAD_BUTTON_START,         "Start" },
	{ "cLS",                    SDL_GAMEPAD_BUTTON_LEFT_STICK,    "LS" },
	{ "cRS",                    SDL_GAMEPAD_BUTTON_RIGHT_STICK,   "RS" },
	{ "cLB",                    SDL_GAMEPAD_BUTTON_LEFT_SHOULDER, "LB" },
	{ "cRB",                    SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER,"RB" },
	{ "cDPAD_UP",               SDL_GAMEPAD_BUTTON_DPAD_UP,       "D-Pad Up" },
	{ "cDPAD_DOWN",             SDL_GAMEPAD_BUTTON_DPAD_DOWN,     "D-Pad Down" },
	{ "cDPAD_LEFT",             SDL_GAMEPAD_BUTTON_DPAD_LEFT,     "D-Pad Left" },
	{ "cDPAD_RIGHT",            SDL_GAMEPAD_BUTTON_DPAD_RIGHT,    "D-Pad Right" },

	{ "cLTRIGGER",				DIK_LTRIGGER,					  "LT" },
	{ "cRTRIGGER",				DIK_RTRIGGER,					  "RT" },

	{ "cLSTICK_UP",				DIK_LSTICK_UP,					  "LS Up" },
	{ "cLSTICK_DOWN",			DIK_LSTICK_DOWN,				  "LS Down" },
	{ "cLSTICK_LEFT",			DIK_LSTICK_LEFT,				  "LS Left" },
	{ "cLSTICK_RIGHT",			DIK_LSTICK_RIGHT,				  "LS Right" },

	{ "cRSTICK_UP",				DIK_RSTICK_UP,					  "RS Up" },
	{ "cRSTICK_DOWN",			DIK_RSTICK_DOWN,				  "RS Down" },
	{ "cRSTICK_LEFT",			DIK_RSTICK_LEFT,				  "RS Left" },
	{ "cRSTICK_RIGHT",			DIK_RSTICK_RIGHT,				  "RS Right" },

	{ nullptr,                  0}
};
void initialize_bindings()
{
#ifdef DEBUG
	int i1=0;
	while(true)
	{
		_keyboard& _k1			= keyboards[i1];
		if(_k1.key_name==nullptr)	break;
		int i2 = i1;
		while (true)
		{
			_keyboard& _k2			= keyboards[i2];
			if(_k2.key_name==nullptr)	break;
			if(_k1.dik==_k2.dik && i1!=i2)
			{
				Msg("%s==%s",_k1.key_name,_k2.key_name);
			}
			++i2;
		}
		++i1;
	}
#endif

	for(int idx=0; idx<bindings_count; ++idx)
		g_key_bindings[idx].m_action = &actions[idx];
	
}

void remap_keys()
{
	int idx = 0;
	string128 buff;
	while (keyboards[idx].key_name)
	{
		buff[0] = 0;
		_keyboard& kb = keyboards[idx];
		bool res = pInput->get_dik_name(kb.dik, buff, sizeof(buff));
		kb.key_local_name = res ? buff : kb.key_local_name;

		if (kb.key_local_name.starts_with('k'))
		{
			kb.key_local_name = kb.key_local_name.substr(1);
		}

		++idx;
	}
	idx = 0;
	while (gamepads[idx].key_name)
	{
		buff[0] = 0;
		_keyboard& kb = gamepads[idx];

		if (kb.key_local_name.starts_with('c'))
		{
			kb.key_local_name = kb.key_local_name.substr(1);
		}

		++idx;
	}
}

ENGINE_API const char* id_to_action_name(EGameActions _id)
{
	int idx				= 0;
	while( actions[idx].action_name )
	{
		if(_id==actions[idx].id )
			return actions[idx].action_name;
		++idx;
	}
	Msg				("! cant find corresponding [action_name] for id");
	return			nullptr;
}

ENGINE_API EGameActions action_name_to_id(const char* _name)
{
	_action* action = action_name_to_ptr(_name);
	if(action)
		return action->id;
	else
		return	kNOTBINDED;
}

ENGINE_API _action* action_name_to_ptr(const char* _name)
{
	int idx				= 0;
	while( actions[idx].action_name )
	{
		if( !_stricmp(_name,actions[idx].action_name) )
			return &actions[idx];
		++idx;
	}
	Msg				("! cant find corresponding [id] for action_name %s", _name);
	return			nullptr;
}

ENGINE_API const char*	dik_to_keyname			(int _dik)
{
	_keyboard* kb = dik_to_ptr(_dik, true);
	if(kb)
		return kb->key_local_name.data();
	else
		return nullptr;
}

ENGINE_API _keyboard* dik_to_ptr(int _dik, bool bSafe)
{
	int idx =0;
	while(keyboards[idx].key_name)
	{
		_keyboard&	kb		= keyboards[idx];
		if(kb.dik==_dik)
			return &keyboards[idx];
		++idx;
	}	
	idx = 0;
	while(gamepads[idx].key_name)
	{
		_keyboard&	kb		= gamepads[idx];
		if(kb.dik==_dik)
			return &gamepads[idx];
		++idx;
	}	
	if (!bSafe)
		Msg			("! cant find corresponding [_keyboard] for dik");
	return			nullptr;
}

ENGINE_API int	keyname_to_dik (const char* _name)
{
	_keyboard* _kb = keyname_to_ptr(_name);
    if (_kb)
		return _kb->dik;
    return 0;
}

ENGINE_API _keyboard* keyname_to_ptr(const char* _name)
{
	xr_string TestName = _name;

	for (_keyboard& KeyData : keyboards)
	{
		if (KeyData.key_name == nullptr)
			continue;

		if (TestName.EqualWithCaseInsensitive(KeyData.key_local_name))
			return &KeyData;

		if (TestName.EqualWithCaseInsensitive(KeyData.key_name))
			return &KeyData;
	}
	for (_keyboard& gpKeyData : gamepads)
	{
		if (gpKeyData.key_name == nullptr)
			continue;

		if (TestName.EqualWithCaseInsensitive(gpKeyData.key_local_name))
			return &gpKeyData;

		if (TestName.EqualWithCaseInsensitive(gpKeyData.key_name))
			return &gpKeyData;
	}

	Msg("! cant find corresponding [_keyboard*] for keyname %s", _name);
	return nullptr;
}

ENGINE_API bool is_group_not_conflicted(_key_group g1, _key_group g2)
{
	return ((g1==_sp && g2==_mp) || (g1==_mp && g2==_sp));
}

ENGINE_API bool is_action_group_matching(_action_group g1, _action_group g2)
{
	return g1 == g2;
}

bool is_group_matching(_key_group g1, _key_group g2)
{
	return ( (g1==g2) || (g1==_both) || (g2==_both) );
}

ENGINE_API bool is_binded(EGameActions _action_id, int _dik)
{
	_binding* pbinding = &g_key_bindings[_action_id];
	if (pInput->GetControllerMode())
	{
		if (pbinding->m_gamepad[0] && pbinding->m_gamepad[0]->dik == _dik)
			return true;

		if (pbinding->m_gamepad[1] && pbinding->m_gamepad[1]->dik == _dik)
			return true;
	}
	else
	{
		if (pbinding->m_keyboard[0] && pbinding->m_keyboard[0]->dik == _dik)
			return true;

		if (pbinding->m_keyboard[1] && pbinding->m_keyboard[1]->dik == _dik)
			return true;
	}

	return false;
}

ENGINE_API int get_action_dik(EGameActions _action_id, int idx)
{
	_binding* pbinding = &g_key_bindings[_action_id];
	
	if(idx==-1)
	{
		if (pInput->GetControllerMode())
		{
			if (pbinding->m_gamepad[0])
				return pbinding->m_gamepad[0]->dik;

			if (pbinding->m_gamepad[1])
				return pbinding->m_gamepad[1]->dik;
		}
		else
		{
			if (pbinding->m_keyboard[0])
				return pbinding->m_keyboard[0]->dik;

			if (pbinding->m_keyboard[1])
				return pbinding->m_keyboard[1]->dik;
		}
	}
	else
	{
		if (pInput->GetControllerMode())
		{
			if (pbinding->m_gamepad[idx])
				return pbinding->m_gamepad[idx]->dik;
		}
		else
		{
			if (pbinding->m_keyboard[idx])
				return pbinding->m_keyboard[idx]->dik;
		}
	}
	return 0;
}

ENGINE_API EGameActions get_binded_action(int _dik, _action_group _ai)
{
	for(int idx=0; idx<bindings_count; ++idx)
	{
		_binding*	binding = &g_key_bindings[idx];

		bool b_is_group_matching	= is_group_matching(binding->m_action->key_group,g_current_keygroup);
		
		if(!b_is_group_matching)	continue;

		if (pInput->GetControllerMode())
		{
			if (binding->m_gamepad[0] && binding->m_gamepad[0]->dik == _dik && b_is_group_matching && binding->m_action->action_group == _ai)
				return binding->m_action->id;

			if (binding->m_gamepad[1] && binding->m_gamepad[1]->dik == _dik && b_is_group_matching && binding->m_action->action_group == _ai)
				return binding->m_action->id;
		}
		else
		{
			if (binding->m_keyboard[0] && binding->m_keyboard[0]->dik == _dik && b_is_group_matching && binding->m_action->action_group == _ai)
				return binding->m_action->id;

			if (binding->m_keyboard[1] && binding->m_keyboard[1]->dik == _dik && b_is_group_matching && binding->m_action->action_group == _ai)
				return binding->m_action->id;
		}
	}
	return kNOTBINDED;
}

const char* GetGamepadSymbol(int dik)
{
	//if (!xr_strcmp(pInput->GamepadPrefix(), "xbox1"))
	{
		switch (dik)
		{
		case SDL_GAMEPAD_BUTTON_SOUTH:
			return XBOX_A;
		case SDL_GAMEPAD_BUTTON_EAST:
			return XBOX_B;
		case SDL_GAMEPAD_BUTTON_WEST:
			return XBOX_X;
		case SDL_GAMEPAD_BUTTON_NORTH:
			return XBOX_Y;
		case SDL_GAMEPAD_BUTTON_BACK:
			return XBOX_Back;
		case SDL_GAMEPAD_BUTTON_GUIDE:
			return XBOX_Guide;
		case SDL_GAMEPAD_BUTTON_START:
			return XBOX_Start;
		case SDL_GAMEPAD_BUTTON_LEFT_STICK:
			return XBOX_LS;
		case SDL_GAMEPAD_BUTTON_RIGHT_STICK:
			return XBOX_RS;
		case SDL_GAMEPAD_BUTTON_LEFT_SHOULDER:
			return XBOX_LB;
		case SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER:
			return XBOX_RB;
		case SDL_GAMEPAD_BUTTON_DPAD_UP:
			return XBOX_DPAD_UP;
		case SDL_GAMEPAD_BUTTON_DPAD_DOWN:
			return XBOX_DPAD_DOWN;
		case SDL_GAMEPAD_BUTTON_DPAD_LEFT:
			return XBOX_DPAD_LEFT;
		case SDL_GAMEPAD_BUTTON_DPAD_RIGHT:
			return XBOX_DPAD_RIGHT;
		case DIK_LTRIGGER:
			return XBOX_LTRIGGER;
		case DIK_RTRIGGER:
			return XBOX_RTRIGGER;
		case DIK_LSTICK_UP:
			return XBOX_LSTICK_UP;
		case DIK_LSTICK_DOWN:
			return XBOX_LSTICK_DOWN;
		case DIK_LSTICK_LEFT:
			return XBOX_LSTICK_LEFT;
		case DIK_LSTICK_RIGHT:
			return XBOX_LSTICK_RIGHT;
		case DIK_RSTICK_UP:
			return XBOX_RSTICK_UP;
		case DIK_RSTICK_DOWN:
			return XBOX_RSTICK_DOWN;
		case DIK_RSTICK_LEFT:
			return XBOX_RSTICK_LEFT;
		case DIK_RSTICK_RIGHT:
			return XBOX_RSTICK_RIGHT;
		}
	}
	
	return "NONE";
}

ENGINE_API void GetActionAllBinding(const char* _action, char* dst_buff, int dst_buff_sz)
{
	int action_id = action_name_to_id(_action);
	_binding* pbinding = &g_key_bindings[action_id];

	if (action_id == kNOTBINDED)
	{
		Msg("! [ERROR]: Action not found %s", _action);
		dst_buff[0] = 0;
		return;
	}

	string128 prim = {};
	string128 sec = {};
	string128 gp_prim = {};

	if (pbinding->m_keyboard[0])
		xr_strcpy(prim, pbinding->m_keyboard[0]->key_local_name.c_str());

	if (pbinding->m_keyboard[1])
		xr_strcpy(sec, pbinding->m_keyboard[1]->key_local_name.c_str());

	if (pbinding->m_gamepad[0])
	{
		const char* gpSymbol = GetGamepadSymbol(pbinding->m_gamepad[0]->dik);
		if (gpSymbol)
			xr_strcpy(gp_prim, gpSymbol);
	}

	if (!pbinding->m_keyboard[0] && !pbinding->m_keyboard[1] &&
		!pbinding->m_gamepad[0] && !pbinding->m_gamepad[1])
	{
		xr_strcpy(dst_buff, dst_buff_sz, g_pStringTable->translate("st_key_notbinded").c_str());
		return;
	}

	if (pInput->GetControllerMode())
	{
		if (gp_prim[0])
			xr_strcpy(dst_buff, dst_buff_sz, gp_prim);
		else
			dst_buff[0] = 0;

		return;
	}

	if (prim[0] && sec[0])
	{
		xr_strcpy(dst_buff, dst_buff_sz, prim);
		xr_strcat(dst_buff, dst_buff_sz, " , ");
		xr_strcat(dst_buff, dst_buff_sz, sec);
	}
	else if (prim[0])
	{
		xr_strcpy(dst_buff, dst_buff_sz, prim);
	}
	else if (sec[0])
	{
		xr_strcpy(dst_buff, dst_buff_sz, sec);
	}
	else
	{
		dst_buff[0] = 0;
	}
}

ENGINE_API bool any_binded_key_for_action_pressed_c(int actionId)
{
	int bindingsCnt = kLASTACTION;
	for (int i = 0; i < bindingsCnt; ++i)
	{
		if (g_key_bindings[i].m_action->id == actionId)
		{
			for (int k = 0; k < 2; ++k)
			{
				if (g_key_bindings[i].m_gamepad[k])
				{
					int dik = g_key_bindings[i].m_gamepad[k]->dik;
					if (pInput->iGetAsyncGamepadKeyState(dik))
						return true;
				}
			}
			return false;
		}
	}

	return false;
}

ENGINE_API ConsoleBindCmds bindConsoleCmds;
bool bRemapped = false;

class CCC_Bind : public IConsole_Command
{
	int m_work_idx;
public:
	CCC_Bind(const char* N, int idx) : IConsole_Command(N),m_work_idx(idx) {};
	virtual void Execute(const char* args) 
	{
		string256							action;
		string256							key;
		*action								= 0;
		*key								= 0;

		sscanf								(args,"%s %s", action, key);
		if (!*action)
			return;

		if (!*key)
			return;

		if(!bRemapped) {
			remap_keys	();
			bRemapped	= true;
		}

		if (!action_name_to_ptr(action))
			return;

		int action_id						= action_name_to_id			(action);
		if (action_id==kNOTBINDED)
			return;

		_keyboard*	pkeyboard				= keyname_to_ptr(key);
		if (!pkeyboard)
			return;

		_binding*	curr_pbinding			= &g_key_bindings[action_id];

		curr_pbinding->m_keyboard[m_work_idx]= pkeyboard;
			
		{
			for(int idx=0; idx<bindings_count; ++idx)
			{
				_binding*	binding			= &g_key_bindings[idx];
				if(binding==curr_pbinding)	continue;

				bool b_conflict = !is_group_not_conflicted(binding->m_action->key_group, curr_pbinding->m_action->key_group);
				bool b_action_match = is_action_group_matching(binding->m_action->action_group, curr_pbinding->m_action->action_group);

				if(binding->m_keyboard[0]==pkeyboard && b_conflict && b_action_match)
					binding->m_keyboard[0]=nullptr;
				
				if(binding->m_keyboard[1]==pkeyboard && b_conflict && b_action_match)
					binding->m_keyboard[1]=nullptr;
			}
		}


		CStringTable::ReparseKeyBindings();
	}
	virtual void Save(IWriter* F) 
	{
		// Don't write "default_controls" section header - it causes conflicts when loading user.ltx
		// The default_controls command would reload default_controls.ltx and overwrite user bindings
		for(int idx=0; idx<bindings_count;++idx)
		{
			_binding* pbinding = &g_key_bindings[idx];
			if( pbinding->m_keyboard[m_work_idx] )
			{
				F->w_printf("%s %s %s\r\n", 
							cName, 
							pbinding->m_action->action_name,
							pbinding->m_keyboard[m_work_idx]->key_name);
			}
		}
	}

    virtual void fill_tips(vecTips& tips, u32 mode)
    {
        for (int idx = 0; idx < bindings_count; ++idx) {
            if (idx > bindings_count)
                continue;
            _binding* pbinding = &g_key_bindings[idx];
            if (!pbinding)
                continue;
            if (!pbinding->m_action)
                continue;
            if (!pbinding->m_action->action_name)
                continue;
            tips.push_back(pbinding->m_action->action_name);
        }
        IConsole_Command::fill_tips(tips, mode);
    }
};

class CCC_BindGamepad : public IConsole_Command
{
	int m_work_idx;
public:
	CCC_BindGamepad(const char* N, int idx) : IConsole_Command(N), m_work_idx(idx) {};
	virtual void Execute(const char* args) 
	{
		string256							action;
		string256							key;
		*action								= 0;
		*key								= 0;

		sscanf								(args,"%s %s", action, key);
		if (!*action)
			return;

		if (!*key)
			return;

		if(!bRemapped) {
			remap_keys	();
			bRemapped	= true;
		}

		if (!action_name_to_ptr(action))
			return;

		int action_id						= action_name_to_id			(action);
		if (action_id==kNOTBINDED)
			return;

		_keyboard*	pkeyboard				= keyname_to_ptr(key);
		if (!pkeyboard)
			return;

		_binding*	curr_pbinding			= &g_key_bindings[action_id];

		curr_pbinding->m_gamepad[m_work_idx] = pkeyboard;
			
		{
			for(int idx=0; idx<bindings_count; ++idx)
			{
				_binding*	binding			= &g_key_bindings[idx];
				if(binding==curr_pbinding)	continue;

				bool b_conflict = !is_group_not_conflicted(binding->m_action->key_group, curr_pbinding->m_action->key_group);

				if(binding->m_gamepad[0] == pkeyboard && b_conflict && is_action_group_matching(binding->m_action->action_group, curr_pbinding->m_action->action_group))
					binding->m_gamepad[0] = nullptr;

				if (binding->m_gamepad[1] == pkeyboard && b_conflict && is_action_group_matching(binding->m_action->action_group, curr_pbinding->m_action->action_group))
					binding->m_gamepad[1] = nullptr;
			}
		}


		CStringTable::ReparseKeyBindings();
	}
	virtual void Save(IWriter* F) 
	{
		for(int idx=0; idx<bindings_count;++idx)
		{
			_binding* pbinding = &g_key_bindings[idx];
			if( pbinding->m_gamepad[m_work_idx])
			{
				F->w_printf("%s %s %s\r\n", 
							cName, 
							pbinding->m_action->action_name,
							pbinding->m_gamepad[m_work_idx]->key_name);
			}
		}
	}

    virtual void fill_tips(vecTips& tips, u32 mode)
    {
        for (int idx = 0; idx < bindings_count; ++idx) {
            if (idx > bindings_count)
                continue;
            _binding* pbinding = &g_key_bindings[idx];
            if (!pbinding)
                continue;
            if (!pbinding->m_action)
                continue;
            if (!pbinding->m_action->action_name)
                continue;
            tips.push_back(pbinding->m_action->action_name);
        }
        IConsole_Command::fill_tips(tips, mode);
    }
};

class CCC_UnBind : public IConsole_Command
{
	int m_work_idx;
public:
	CCC_UnBind(const char* N, int idx) : IConsole_Command(N),m_work_idx(idx) 
	{ bEmptyArgsHandled=true; };
	virtual void Execute(const char* args)
	{
		int action_id						= action_name_to_id			(args);
		_binding*	pbinding				= &g_key_bindings[action_id];
		pbinding->m_keyboard[m_work_idx]	= nullptr;

		CStringTable::ReparseKeyBindings();
	}

    virtual void fill_tips(vecTips& tips, u32 mode)
    {
        for (int idx = 0; idx < bindings_count; ++idx) {
            if (idx > bindings_count)
                continue;
            _binding* pbinding = &g_key_bindings[idx];
            if (!pbinding)
                continue;
            if (!pbinding->m_action)
                continue;
            if (!pbinding->m_action->action_name)
                continue;
            tips.push_back(pbinding->m_action->action_name);
        }
        IConsole_Command::fill_tips(tips, mode);
    }
};

class CCC_UnBindGamepad : public IConsole_Command
{
	int m_work_idx;
public:
	CCC_UnBindGamepad(const char* N, int idx) : IConsole_Command(N), m_work_idx(idx)
	{ bEmptyArgsHandled=true; };
	virtual void Execute(const char* args)
	{
		int action_id						= action_name_to_id			(args);
		_binding*	pbinding				= &g_key_bindings[action_id];
		pbinding->m_gamepad[m_work_idx] = nullptr;

		CStringTable::ReparseKeyBindings();
	}

    virtual void fill_tips(vecTips& tips, u32 mode)
    {
        for (int idx = 0; idx < bindings_count; ++idx) {
            if (idx > bindings_count)
                continue;
            _binding* pbinding = &g_key_bindings[idx];
            if (!pbinding)
                continue;
            if (!pbinding->m_action)
                continue;
            if (!pbinding->m_action->action_name)
                continue;
            tips.push_back(pbinding->m_action->action_name);
        }
        IConsole_Command::fill_tips(tips, mode);
    }
};

class CCC_ListActions : public IConsole_Command
{
public:
	CCC_ListActions(const char* N) : IConsole_Command(N)
	{ bEmptyArgsHandled=true; };

	virtual void Execute(const char* args) {
		Log("- --- Action list start ---");
		for(int idx=0; idx<bindings_count;++idx)
		{
			_binding* pbinding = &g_key_bindings[idx];
			Msg("-%s", pbinding->m_action->action_name);
		}
		Log("- --- Action list end   ---");
	}
};

class CCC_UnBindAll : public IConsole_Command
{
public:
	CCC_UnBindAll(const char* N) : IConsole_Command(N)
	{ bEmptyArgsHandled=true; };

	virtual void Execute(const char* args) 
	{
		for(int idx=0; idx<bindings_count;++idx)
		{
			_binding* pbinding		= &g_key_bindings[idx];
			pbinding->m_keyboard[0]	= nullptr;
			pbinding->m_keyboard[1]	= nullptr;
			pbinding->m_gamepad[0] = nullptr;
			pbinding->m_gamepad[1] = nullptr;
		}
		bindConsoleCmds.clear();
	}
};

class CCC_DefControls : public CCC_UnBindAll
{
public:
	CCC_DefControls(const char* N) : CCC_UnBindAll(N){}

	virtual void Execute(const char* args) 
	{
		CCC_UnBindAll::Execute(args);
		string_path				_cfg;
		string_path				cmd;
		const char* file_dir = "ixray_settings\\default_controls.ltx";
		if (!FS.exist(_game_config_, file_dir))
		{
			file_dir = "default_controls.ltx";
		}
		FS.update_path(_cfg, _game_config_, file_dir);
		xr_strconcat(cmd,"cfg_load", " ", _cfg);
		Console->Execute		(cmd);

		string_path platformFileDir;
		xr_strconcat(platformFileDir, "ixray_settings\\default_controls_", EngineExternal().GetCurrentPlatformName(), ".ltx");
		if (FS.exist(_game_config_, platformFileDir))
		{
			FS.update_path(_cfg, _game_config_, platformFileDir);
			xr_strconcat(cmd, "cfg_load ", _cfg);
			Console->Execute(cmd);
		}
	}
};

class CCC_BindList : public IConsole_Command
{
public:
	CCC_BindList(const char* N) : IConsole_Command(N)
	{ bEmptyArgsHandled=true; };

	virtual void Execute(const char* args) {
		Log				("- --- Bind list start ---");
		string512		buff;			
		
		for(int idx=0; idx<bindings_count;++idx)
		{
			_binding* pbinding		= &g_key_bindings[idx];
			xr_sprintf		(buff,"[%s] primary is[%s] secondary is[%s] gamepad primary is [%s] gamepad secondary is [%s]",
						pbinding->m_action->action_name,
						(pbinding->m_keyboard[0])?pbinding->m_keyboard[0]->key_local_name.c_str():"nullptr",
						(pbinding->m_keyboard[1])?pbinding->m_keyboard[1]->key_local_name.c_str():"nullptr",
						(pbinding->m_gamepad[0])?pbinding->m_gamepad[0]->key_local_name.c_str():"nullptr",
						(pbinding->m_gamepad[1])?pbinding->m_gamepad[1]->key_local_name.c_str():"nullptr");
			Log		(buff);
		}
		Log				("- --- Bind list end   ---");
	}
};

class CCC_BindConsoleCmd : public IConsole_Command
{
public:
	CCC_BindConsoleCmd(const char* N) : IConsole_Command(N) {};
	virtual void Execute(const char* args) 
	{
		string512				console_command;
		string256				key;
		int cnt					= _GetItemCount(args,' ');
		_GetItems				(args,0,cnt-1,console_command,' ');
		_GetItem				(args,cnt-1,key,' ');

		int dik					= keyname_to_dik(key);
        if (dik)
			bindConsoleCmds.bind	(dik, console_command);
	}

	virtual void Save(IWriter* F) 
	{
		bindConsoleCmds.save(F);
	}

    virtual void fill_tips(vecTips& tips, u32 mode)
    {
        CConsole::vecCMD_IT it;
        for (it = Console->Commands.begin(); it != Console->Commands.end(); it++) {
            IConsole_Command& C = *(it->second);
            tips.push_back(C.Name());
        }
        IConsole_Command::fill_tips(tips, mode);
    }

};

class CCC_UnBindConsoleCmd : public IConsole_Command
{
public:
	CCC_UnBindConsoleCmd(const char* N) : IConsole_Command(N)
	{ bEmptyArgsHandled=false; };

	virtual void Execute(const char* args) 
	{
        if (bindConsoleCmds.m_bindConsoleCmds.empty())
            return;
		int _dik = keyname_to_dik	(args);
		if(_dik)
			bindConsoleCmds.unbind		(_dik);
	}
    virtual void fill_tips(vecTips& tips, u32 mode)
    {
        xr_map<int, _conCmd>::iterator it = bindConsoleCmds.m_bindConsoleCmds.begin();
        for (; it != bindConsoleCmds.m_bindConsoleCmds.end(); ++it) 
		{
            if (bindConsoleCmds.m_bindConsoleCmds.empty())
                continue;
            const char* keyname = dik_to_keyname(it->first);
            tips.push_back(keyname);
        }
    }
};

void ConsoleBindCmds::bind(int dik, const char* N)
{
	_conCmd& c	= m_bindConsoleCmds[dik];
	c.cmd		= N;
}
void ConsoleBindCmds::unbind(int dik)
{
	xr_map<int,_conCmd>::iterator it = m_bindConsoleCmds.find(dik);
	if(it==m_bindConsoleCmds.end())
		return;

	m_bindConsoleCmds.erase(it);
}

void ConsoleBindCmds::clear()
{
	m_bindConsoleCmds.clear();
}

bool ConsoleBindCmds::execute(int dik)
{
	xr_map<int,_conCmd>::iterator it = m_bindConsoleCmds.find(dik);
	
	if (it == m_bindConsoleCmds.end())
		return false;

	string512 buffer;
	_GetItem(it->second.cmd.c_str(), 0, buffer, ' ');

	if (buffer[0] != '\0')
	{
		if (IConsole_Command* cmd = Console->GetCommand(buffer))
		{
			if (CCC_Boolean* ccc_bool = cmd->dcast_bool())
			{
				ccc_bool->Toggle();
				Console->Execute(ccc_bool->Name());
				return true;
			}

			if (CCC_Integer* ccc_int = cmd->dcast_int())
			{
				ccc_int->Toggle();
				Console->Execute(ccc_int->Name());
				return true;
			}

			if (CCC_Mask16* ccc_mask16 = cmd->dcast_mask16())
			{
				ccc_mask16->Toggle();
				Console->Execute(ccc_mask16->Name());
				return true;
			}
			
			if (CCC_Mask32* ccc_mask32 = cmd->dcast_mask32())
			{
				ccc_mask32->Toggle();
				Console->Execute(ccc_mask32->Name());
				return true;
			}

			if (CCC_Mask64* ccc_mask64 = cmd->dcast_mask64())
			{
				ccc_mask64->Toggle();
				Console->Execute(ccc_mask64->Name());
				return true;
			}
		}
	}

	Console->Execute(it->second.cmd.c_str());
	return true;
}

void ConsoleBindCmds::save(IWriter* F)
{
	xr_map<int,_conCmd>::iterator it = m_bindConsoleCmds.begin();
	
	for(;it!=m_bindConsoleCmds.end();++it)
	{
		const char* keyname		= dik_to_keyname(it->first);
		F->w_printf("bind_console %s%s\r\n", *it->second.cmd, keyname);
	}
}

ENGINE_API void CCC_RegisterInput()
{
	initialize_bindings									();
	CMD2(CCC_Bind,				"bind",					0);
	CMD2(CCC_Bind,				"bind_sec",				1);
	CMD2(CCC_BindGamepad,		"bind_gamepad",			0);
	CMD2(CCC_BindGamepad,		"bind_gamepad_sec",		1);
	CMD2(CCC_UnBind,			"unbind",				0);
	CMD2(CCC_UnBind,			"unbind_sec",			1);
	CMD2(CCC_UnBindGamepad,		"unbind_gamepad",		0);
	CMD2(CCC_UnBindGamepad,		"unbind_gamepad_sec",	1);
	CMD1(CCC_UnBindAll,			"unbindall"				);
	CMD1(CCC_DefControls,		"default_controls"		);
	CMD1(CCC_ListActions,		"list_actions"			);

	CMD1(CCC_BindList,			"bind_list"				);
	CMD1(CCC_BindConsoleCmd,	"bind_console"			);
	CMD1(CCC_UnBindConsoleCmd,	"unbind_console"		);
};
