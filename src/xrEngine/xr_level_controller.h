#pragma once

enum EGameActions
{
	kLEFT,
	kRIGHT,
	kUP,
	kDOWN,
	kJUMP,
	kCROUCH,
	kACCEL,
	kSPRINT_TOGGLE,
						
	kFWD,
	kBACK,
	kL_STRAFE,
	kR_STRAFE,
						
	kL_LOOKOUT,
	kR_LOOKOUT,
						
	kCAM_1,
	kCAM_2,
	kCAM_3,
	kCAM_ZOOM_IN,
	kCAM_ZOOM_OUT,
						
	kTORCH,
	kNIGHT_VISION,
	kDETECTOR,
	kWPN_1,
	kWPN_2,
	kWPN_3,
	kWPN_4,
	kWPN_5,
	kWPN_6,

	kWPN_7, // abstract naming for 3rd weapon slot

	kARTEFACT,
	kWPN_NEXT,
	kWPN_FIRE,
	kWPN_ZOOM,
	kWPN_ZOOM_INC,
	kWPN_ZOOM_DEC,
	kWPN_RELOAD,
	kWPN_FUNC,
	kWPN_FIREMODE_PREV,
	kWPN_FIREMODE_NEXT,

	kPAUSE,
	kDROP,
	kUSE,
	kSCORES,
	kCHAT,
	kCHAT_TEAM, 
	kVOICE_CHAT,
	kVOICE_DISTANCE,
	kSCREENSHOT,
	kQUIT,
	kCONSOLE,
	kINVENTORY,
	kBUY,
	kSKIN,
	kTEAM,
	kACTIVE_JOBS,
	kMAP,
	kCONTACTS,

	kVOTE_BEGIN,
	kSHOW_ADMIN_MENU,
	kVOTE,
	kVOTEYES,
	kVOTENO,
						
	kNEXT_SLOT,
	kPREV_SLOT,
						
	kSPEECH_MENU_0,
	kSPEECH_MENU_1,
						
	kQUICK_USE_1,
	kQUICK_USE_2,		
	kQUICK_USE_3,		
	kQUICK_USE_4,
	
	kQUICK_SAVE,
	kQUICK_LOAD,
	kALIFE_CMD,

	kUSE_BANDAGE,
	kUSE_MEDKIT,
	kENGINE,
	kBRAKE,
	kTRANSMISSION_UP,
	kTRANSMISSION_DOWN,
	kBOOST,
	kSIGNAL,

	// Controller UI actions
	kUI_TAB_LEFT,
	kUI_TAB_RIGHT,
	kUI_TAB_SECONDARY_LEFT,
	kUI_TAB_SECONDARY_RIGHT,

	kUI_LEFT,
	kUI_RIGHT,
	kUI_UP,
	kUI_DOWN,
	kUI_SECONDARY_LEFT,
	kUI_SECONDARY_RIGHT,
	kUI_SECONDARY_UP,
	kUI_SECONDARY_DOWN,

	kUI_ACCEPT,
	kUI_BACK,
	kUI_HINT,

	kUI_ACTION_1,
	kUI_ACTION_2,
	kINV_SORT_PREV,
	kINV_SORT_NEXT,
	kINV_SORT_CYCLE,

	kPDA_LOG_TO_START,
	kPDA_LOG_TO_END,
	kPDA_LOG_SCROLL_DOWN,
	kPDA_LOG_SCROLL_UP,
	kPDA_LOG_DATE_PREV,
	kPDA_LOG_DATE_NEXT,
	kPDA_LOG_SHOW_NEWS,
	kPDA_LOG_SHOW_DIALOGS,

	kPDA_TASKS_TOGGLE_LIST,
	kPDA_TASKS_TOGGLE_LEGEND,
	kPDA_TASKS_NEXT,
	kPDA_TASKS_PREV,
	kPDA_TASKS_TOGGLE_MAP,
	kPDA_TASKS_MAP_SHOW_ME,
	kPDA_TASKS_MAP_ZOOM_IN,
	kPDA_TASKS_MAP_ZOOM_OUT,
	kPDA_TASKS_FILTER_NEXT,
	kPDA_TASKS_FILTER_PREV,
	kPDA_TASKS_FILTER_TOGGLE,

	kCUSTOM1,
	kCUSTOM2,
	kCUSTOM3,
	kCUSTOM4,
	kCUSTOM5,
	kCUSTOM6,
	kCUSTOM7,
	kCUSTOM8,
	kCUSTOM9,
	kCUSTOM10,
	kCUSTOM11,
	kCUSTOM12,
	kCUSTOM13,
	kCUSTOM14,
	kCUSTOM15,

	kCAM_AUTOAIM,

	kCLEARGASMASK,
	kTACTICALTORCH,
	kLASER,
	kWPN_ZOOM_ALTER,
	kBRIGHTNESS_PLUS,
	kBRIGHTNESS_MINUS,
	kQUICK_GRENADE,
	kQUICK_KICK,
	kMAG_CHECK,
	kFIREMODE_CHECK,
	kSHOW_QUICK_SLOTS,
	kWPN_CHAMBER_LOAD,
	kWPN_CHAMBER_UNLOAD,
	kWPN_CHAMBER_CHECK,

	kWPN_RADIAL_MENU,
	kACTORMENU_ACTION,
	kSAFEMODE,
	kINV_UNLOAD_RUCK,
	kQUICK_BOLT,

	kPDA_TASKS_PERSONAL_SPOT_CURSOR,
	kPDA_TASKS_PERSONAL_SPOT_ACTOR,
	kPDA_TASKS_PERSONAL_SPOT_REMOVE,
	kPDA_TASKS_PERSONAL_SPOT_TRACK,
	kPDA_TASKS_PERSONAL_SPOT_NEXT,
	kPDA_TASKS_PERSONAL_SPOT_PREV,
	kPERSONAL_SPOT_QUICK,
	kPDA_TASKS_PERSONAL_SPOTS_LIST,
	kPDA_TASKS_MAP_SHOW_TARGET,
	kPDA_TASK_JOURNAL_FILTER_PREV,
	kPDA_TASK_JOURNAL_FILTER_NEXT,
	kPDA_TASK_JOURNAL_PREV,
	kPDA_TASK_JOURNAL_NEXT,
	kPDA_TASK_JOURNAL_SHOW_ON_MAP,
	kPDA_TASK_JOURNAL_TRACK,
	kPDA_RECORDS_CATEGORY_PREV,
	kPDA_RECORDS_CATEGORY_NEXT,
	kPDA_RECORDS_PREV,
	kPDA_RECORDS_NEXT,
	kPDA_RECORDS_SHOW_ON_MAP,
	kPDA_RECORDS_TRACK,
	kPDA_RECORDS_ACTION,

	kLASTACTION,
	kFIRST_CUSTOM_ACTION = kLASTACTION,
	kNOTBINDED = kLASTACTION + 256,
	kFORCEDWORD		= u32(-1)
};

struct _keyboard		
{
	const char*		key_name;
	int			dik;
	xr_string	key_local_name;
};
enum _key_group
{
	_both	=	(1<<0)			,
	_sp		=	_both | (1<<1)	,
	_mp		=	_both | (1<<2)	,
};
enum _action_group : u32
{
	agDefault	=	(1<<0),
	agTransport	=	(1<<2),
	agUIGeneral	=	(1<<3),
	agUIRadialWeapon = (1<<4),
	agUILogMenu	=	(1<<5),
	agUITaskMenu	=	(1<<6),
	agAiming	=	(1<<7),
	agUITaskJournal	=	(1<<8),
	agUIRecords	=	(1<<9),
	agCustom	=	(1<<10),
	agAny		= u32(-1)
};

extern ENGINE_API _key_group g_current_keygroup;

ENGINE_API bool is_group_not_conflicted(_key_group g1, _key_group g2);
ENGINE_API bool is_action_group_matching(_action_group g1, _action_group g2);

struct _action
{
	const char*			action_name;
	EGameActions	id;
	_key_group		key_group;
	_action_group	action_group;
	const char*		context;
};

ENGINE_API const char*			dik_to_keyname			(int _dik);
ENGINE_API int				keyname_to_dik			(const char* _name);
ENGINE_API _keyboard*		keyname_to_ptr			(const char* _name);
ENGINE_API _keyboard*		dik_to_ptr				(int _dik, bool bSafe);

ENGINE_API const char*			id_to_action_name		(EGameActions _id);
ENGINE_API EGameActions	action_name_to_id		(const char* _name);
ENGINE_API _action*		action_name_to_ptr		(const char* _name);

extern ENGINE_API _action		actions		[];
//extern _keyboard	keyboards	[];
//extern xr_vector< _keyboard >	keyboards;

#define bindings_count kLASTACTION
constexpr int ActionSlotsCount = kNOTBINDED;
struct _binding
{
	_action*		m_action;
	_keyboard*		m_keyboard[2];
	_keyboard*		m_gamepad[2];
};

extern ENGINE_API _binding g_key_bindings[];

ENGINE_API bool				is_binded			(EGameActions action_id, int dik);
ENGINE_API int					get_action_dik		(EGameActions action_id, int idx=-1);
ENGINE_API EGameActions		get_binded_action	(int dik, _action_group _ai = agDefault);
ENGINE_API bool any_binded_key_for_action_pressed_c(int actionId);

ENGINE_API void	 CCC_RegisterInput();

struct _conCmd	
{
	shared_str	cmd;
};

class ENGINE_API ConsoleBindCmds
{
public:
	xr_map<int,_conCmd>		m_bindConsoleCmds;

	void 	bind			(int dik, const char* N);
	void 	unbind			(int dik);
	bool 	execute			(int dik);
	void 	clear			();
	void 	save			(IWriter* F);
};

ENGINE_API void GetActionAllBinding	(const char* action, char* dst_buff, int dst_buff_sz);

struct SHotkeyInfo
{
	shared_str Label;
	shared_str OptionsGroup;
	int Hint = 0;
};

namespace Hotkeys
{
using LayerHandler = xr_delegate<bool(EGameActions, int)>;
using CustomHandler = xr_delegate<bool(EGameActions)>;

ENGINE_API void SetCustomHandler(const CustomHandler& Handler);

ENGINE_API void RegisterContext(const char* Context, const char* Parent);
ENGINE_API void SetLayerProperties(const char* Context, int Priority, bool BlockAll);
ENGINE_API bool LayersEnabled();
ENGINE_API u32 PushLayer(const char* Context, const xr_vector<shared_str>& Handles, const LayerHandler& Handler, bool Root = false);
ENGINE_API void PopLayer(u32 Id);
ENGINE_API bool DispatchLayers(int Dik);
ENGINE_API EGameActions ActionInContext(int Dik, const char* Context);
ENGINE_API const char* ContextLabel(const char* Context);
ENGINE_API bool IsContextRelated(const char* Left, const char* Right);
ENGINE_API EGameActions FindKeyboardConflict(EGameActions Id, int Dik);
ENGINE_API EGameActions FindKeyboardOverlap(EGameActions Id, int Dik);
ENGINE_API const char* ContextParent(const char* Context);
ENGINE_API EGameActions Register(const char* Name, const char* Context);
ENGINE_API EGameActions Find(const char* Name);
ENGINE_API bool IsCustom(EGameActions Id);
ENGINE_API const char* Context(EGameActions Id);
ENGINE_API void SetInfo(EGameActions Id, const char* Label, const char* OptionsGroup, int Hint);
ENGINE_API const SHotkeyInfo* Info(EGameActions Id);
ENGINE_API EGameActions ActionAt(int Dik, const char* Context);
ENGINE_API void ContextActions(const char* Context, xr_vector<EGameActions>& Result);
ENGINE_API void OptionsActions(xr_vector<EGameActions>& Result);
ENGINE_API void PushContext(const char* Context);
ENGINE_API void PopContext(const char* Context);
ENGINE_API const char* ActiveContext();
ENGINE_API void KeyText(EGameActions Id, char* Buffer, int BufferSize);
ENGINE_API u32 Revision();
ENGINE_API void NotifyBindingsChanged();
ENGINE_API void AddPendingBind(const char* Command, const char* Action, const char* Key);
ENGINE_API void LoadConfig();
ENGINE_API void ApplyConfigDefaults();
}

class CHotkeyContextScope
{
public:
	~CHotkeyContextScope()
	{
		SetActive(false);
	}

	void SetRoot(bool Value)
	{
		Root = Value;
	}

	void SetHandler(const xr_vector<shared_str>& HandledContexts, const Hotkeys::LayerHandler& LayerHandler)
	{
		const bool WasActive = Active;
		SetActive(false);
		Handles = HandledContexts;
		Handler = LayerHandler;
		SetActive(WasActive);
	}

	void SetContext(const char* Name)
	{
		const bool WasActive = Active;
		SetActive(false);
		Context = Name;
		SetActive(WasActive);
	}

	void SetActive(bool Value)
	{
		if (!Context.size() || (!Value && !Active))
		{
			return;
		}
		if (Active)
		{
			Hotkeys::PopLayer(LayerId);
			LayerId = 0;
		}
		Active = Value;
		if (Active)
		{
			LayerId = Hotkeys::PushLayer(Context.c_str(), Handles, Handler, Root);
		}
	}

private:
	shared_str Context;
	xr_vector<shared_str> Handles;
	Hotkeys::LayerHandler Handler;
	u32 LayerId = 0;
	bool Root = false;
	bool Active = false;
};

extern ENGINE_API ConsoleBindCmds bindConsoleCmds;

#define MOUSE_1		(SDL_SCANCODE_COUNT + 100)
#define MOUSE_2		(SDL_SCANCODE_COUNT + 101)
#define MOUSE_3		(SDL_SCANCODE_COUNT + 102)

#define MOUSE_4		(SDL_SCANCODE_COUNT + 103)
#define MOUSE_5		(SDL_SCANCODE_COUNT + 104)
#define MOUSE_6		(SDL_SCANCODE_COUNT + 105)
#define MOUSE_7		(SDL_SCANCODE_COUNT + 106)
#define MOUSE_8		(SDL_SCANCODE_COUNT + 107)
