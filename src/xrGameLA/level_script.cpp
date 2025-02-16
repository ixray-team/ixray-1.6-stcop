////////////////////////////////////////////////////////////////////////////
//	Module 		: level_script.cpp
//	Created 	: 28.06.2004
//  Modified 	: 28.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Level script export
////////////////////////////////////////////////////////////////////////////

#include "pch_script.h"
#include "level.h"
#include "actor.h"
#include "script_game_object.h"
#include "patrol_path_storage.h"
#include "xrServer.h"
#include "client_spawn_manager.h"
#include "../igame_persistent.h"
#include "game_cl_base.h"
#include "ui/UIDialogWnd.h"
#include "date_time.h"
#include "ai_space.h"
#include "level_graph.h"
#include "PHCommander.h"
#include "PHScriptCall.h"
#include "HUDManager.h"
#include "../xrScripts/script_engine.h"
#include "game_cl_single.h"
#include "../xr_input.h"
#include "UIGameSP.h"
#include "UIGameCustom.h"
#include "ui/UIInventoryWnd.h"
#include "ui/UIPdaWnd.h"
#include "ui/UITalkWnd.h"
#include "map_manager.h"
#include "map_location.h"
#include "phworld.h"
#include "Weapon.h"
#include "alife_simulator.h"
#include "alife_time_manager.h"

using namespace luabind;


LPCSTR command_line	()
{
	return		(Core.Params);
}
void save_allowed	(bool b)
{
	Actor()->b_saveAllowed = b;
}
bool is_save_allowed	()
{
	return Actor()->b_saveAllowed;
}

void animation_draw(bool b)
{
	m_bDraw_off = b;
}

void animation_holster(bool b)
{
	m_bHolster_off = b;
}

extern Flags32 psActorFlags;
void ai_ignore_actor	(bool value)
{
	psActorFlags.set(AF_INVISIBLE, value);
}

#ifdef DEBUG
void check_object(CScriptGameObject *object)
{
	try {
		Msg	("check_object %s",object->Name());
	}
	catch(...) {
		object = object;
	}
}

CScriptGameObject *tpfGetActor()
{
	static bool first_time = true;
	if (first_time)
		ai().script_engine().script_log(eLuaMessageTypeError,"Do not use level.actor function!");
	first_time = false;
	
	CActor *l_tpActor = smart_cast<CActor*>(Level().CurrentEntity());
	if (l_tpActor)
		return	(smart_cast<CGameObject*>(l_tpActor)->lua_game_object());
	else
		return	(0);
}
#endif

CScriptGameObject *get_object_by_name(LPCSTR caObjectName)
{
	CGameObject*	pGameObject = smart_cast<CGameObject*>(Level().Objects.FindObjectByName(caObjectName));
	if (!pGameObject)
		return nullptr;
	
	return pGameObject->lua_game_object();
}

CScriptGameObject *get_object_by_id(u32 id)
{
	CGameObject* pGameObject = smart_cast<CGameObject*>(Level().Objects.net_Find(id));
	if(!pGameObject)
		return nullptr;

	return pGameObject->lua_game_object();
}

void send_event_key_pressed(int id)
{
	Level().IR_OnKeyboardPress(id);
}

void send_event_key_release(int id)
{
	Level().IR_OnKeyboardRelease(id);
}

void send_event_key_hold(int id)
{
	Level().IR_OnKeyboardHold(id);
}

void send_event_mouse_move(int x,int y)
{
	Level().IR_OnMouseMove(x,y);
}
void send_event_mouse_wheel(int direction)
{
	Level().IR_OnMouseWheel(direction);
}


LPCSTR get_weather	()
{
	return			(*g_pGamePersistent->Environment().GetWeather());
}

LPCSTR get_past_wdesc()
{
	return			(g_pGamePersistent->Environment().Current[0] ? g_pGamePersistent->Environment().Current[0]->m_identifier.c_str() : "null");
}

LPCSTR get_next_wdesc()
{
	return			(g_pGamePersistent->Environment().Current[1] ? g_pGamePersistent->Environment().Current[1]->m_identifier.c_str() : "null");
}

float get_past_wdesc_execution_time()
{
	return			(g_pGamePersistent->Environment().Current[0] ? g_pGamePersistent->Environment().Current[0]->exec_time : -1.f);
}

float get_next_wdesc_execution_time()
{
	return			(g_pGamePersistent->Environment().Current[1] ? g_pGamePersistent->Environment().Current[1]->exec_time : -1.f);
}

float get_weather_game_time()
{
	return			(&g_pGamePersistent->Environment() ? g_pGamePersistent->Environment().GetGameTime() : -1.f);
}

void set_past_wdesc(LPCSTR WeatherSection)
{
	if (&g_pGamePersistent->Environment())
	{
		g_pGamePersistent->Environment().SetEnvDesc(WeatherSection, g_pGamePersistent->Environment().Current[0]);
	}
}

void set_next_wdesc(LPCSTR WeatherSection)
{
	if (&g_pGamePersistent->Environment())
	{
		g_pGamePersistent->Environment().SetEnvDesc(WeatherSection, g_pGamePersistent->Environment().Current[1]);
	}
}

void set_weather	(LPCSTR weather_name, bool forced)
{
	return			(g_pGamePersistent->Environment().SetWeather(weather_name,forced));
}

void set_game_time	(u32 new_hours, u32 new_mins)
{
	Level().SetGameTime(new_hours, new_mins);
}

bool set_weather_fx	(LPCSTR weather_name)
{
	return			(g_pGamePersistent->Environment().SetWeatherFX(weather_name));
}

bool start_weather_fx_from_time	(LPCSTR weather_name, float time)
{
	return			(g_pGamePersistent->Environment().StartWeatherFXFromTime(weather_name, time));
}

bool is_wfx_playing	()
{
	return			(g_pGamePersistent->Environment().IsWFXPlaying());
}

float get_wfx_time	()
{
	return			(g_pGamePersistent->Environment().wfx_time);
}

void set_time_factor(float time_factor)
{
	if (!OnServer())
		return;

	Level().Server->game->SetGameTimeFactor(time_factor);
}

float get_time_factor()
{
	return			(Level().GetGameTimeFactor());
}

void set_game_difficulty(ESingleGameDifficulty dif)
{
	g_SingleGameDifficulty		= dif;
	game_cl_Single* game		= smart_cast<game_cl_Single*>(Level().game); VERIFY(game);
	game->OnDifficultyChanged	();
}
ESingleGameDifficulty get_game_difficulty()
{
	return g_SingleGameDifficulty;
}

u32 get_time_days()
{
	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time((g_pGameLevel && Level().game) ? Level().GetGameTime() : ai().alife().time_manager().game_time(), year, month, day, hours, mins, secs, milisecs);
	return			day;
}

u32 get_time_hours()
{
	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time((g_pGameLevel && Level().game) ? Level().GetGameTime() : ai().alife().time_manager().game_time(), year, month, day, hours, mins, secs, milisecs);
	return			hours;
}

u32 get_time_minutes()
{
	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time((g_pGameLevel && Level().game) ? Level().GetGameTime() : ai().alife().time_manager().game_time(), year, month, day, hours, mins, secs, milisecs);
	return			mins;
}

float cover_in_direction(u32 level_vertex_id, const Fvector &direction)
{
	float			y,p;
	direction.getHP	(y,p);
	return			(ai().level_graph().high_cover_in_direction(y,level_vertex_id));
}

float rain_factor()
{
	return			(g_pGamePersistent->Environment().CurrentEnv->rain_density);
}

u32	vertex_in_direction(u32 level_vertex_id, Fvector direction, float max_distance)
{
	direction.normalize_safe();
	direction.mul	(max_distance);
	Fvector			start_position = ai().level_graph().vertex_position(level_vertex_id);
	Fvector			finish_position = Fvector(start_position).add(direction);
	u32				result = u32(-1);
	ai().level_graph().farthest_vertex_in_direction(level_vertex_id,start_position,finish_position,result,0);
	return			(ai().level_graph().valid_vertex_id(result) ? result : level_vertex_id);
}

bool valid_vertex_id(u32 level_vertex_id)
{
	return ai().level_graph().valid_vertex_id(level_vertex_id);
}

Fvector vertex_position(u32 level_vertex_id)
{
	return			(ai().level_graph().vertex_position(level_vertex_id));
}

u32 vertex_id(Fvector position)
{
	return (ai().level_graph().vertex_id(position));
}

void map_add_object_spot(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml = Level().MapManager().AddMapLocation(spot_type,id);
	if( xr_strlen(text) )
			ml->SetHint(text);
}

void map_add_object_spot_ser(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml = Level().MapManager().AddMapLocation(spot_type,id);
	if( xr_strlen(text) )
			ml->SetHint(text);

	ml->SetSerializable(true);
}

u16 map_add_position_spot_ser(Fvector position, LPCSTR levelName, LPCSTR spot_type, LPCSTR text)
{
	u16 id = 0;
	CMapLocation* ml = Level().MapManager().AddUserLocation(spot_type, levelName, position, &id);
	if( xr_strlen(text) )
		ml->SetHint(text);

	ml->SetSerializable(true);
	return id;
}

void map_change_spot_hint(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml	= Level().MapManager().GetMapLocation(spot_type, id);
	if(!ml)				return;
	ml->SetHint			(text);
}

LPCSTR map_get_spot_hint(u16 id, LPCSTR spot_type)
{
	CMapLocation* ml	= Level().MapManager().GetMapLocation(spot_type, id);
	if(!ml)				return "";
	return				ml->GetHint();
}

void map_remove_object_spot(u16 id, LPCSTR spot_type)
{
	Level().MapManager().RemoveMapLocation(spot_type, id);
}

void map_highlight_spot(u16 id, LPCSTR spot_type, bool state)
{
	CMapLocation* ml	= Level().MapManager().GetMapLocation(spot_type, id);
	if (ml==nullptr) return;
	ml->HighlightSpot(state);
}

u16 map_has_object_spot(u16 id, LPCSTR spot_type)
{
	return Level().MapManager().HasMapLocation(spot_type, id);
}

bool patrol_path_exists(LPCSTR patrol_path)
{
	return		(!!ai().patrol_paths().path(patrol_path,true));
}

LPCSTR get_name()
{
	return		(*Level().name());
}

void prefetch_sound	(LPCSTR name)
{
	Level().PrefetchSound(name);
}


CClientSpawnManager	&get_client_spawn_manager()
{
	return		(Level().client_spawn_manager());
}

void start_stop_menu(CUIDialogWnd* pDialog, bool bDoHideIndicators)
{
	if (!pDialog->IsShown())
		pDialog->ShowDialog		(bDoHideIndicators);
	else
		pDialog->HideDialog		();
}

void add_dialog_to_render(CUIDialogWnd* pDialog)
{
	CurrentGameUI()->AddDialogToRender(pDialog);
}

void remove_dialog_to_render(CUIDialogWnd* pDialog)
{
	CurrentGameUI()->RemoveDialogToRender(pDialog);
}

CUIDialogWnd* main_input_receiver()
{
	return CurrentGameUI()->TopInputReceiver();
}

void hide_indicators()
{
	CurrentGameUI()->HideShownDialogs();

	CurrentGameUI()->ShowGameIndicators(false);
	CurrentGameUI()->ShowCrosshair(false);
}

void show_indicators()
{
	CurrentGameUI()->ShowGameIndicators(true);
	CurrentGameUI()->ShowCrosshair(true);
}

bool indicators_shown()
{
	return (CurrentGameUI()->GameIndicatorsShown());
}

bool is_level_present()
{
	return (!!g_pGameLevel);
}

void add_call(const luabind::functor<bool> &condition,const luabind::functor<void> &action)
{
	luabind::functor<bool>		_condition = condition;
	luabind::functor<void>		_action = action;
	CPHScriptCondition	* c=new CPHScriptCondition(_condition);
	CPHScriptAction		* a=new CPHScriptAction(_action);
	Level().ph_commander_scripts().add_call(c,a);
}

void remove_call(const luabind::functor<bool> &condition,const luabind::functor<void> &action)
{
	CPHScriptCondition	c(condition);
	CPHScriptAction		a(action);
	Level().ph_commander_scripts().remove_call(&c,&a);
}

void add_call(const luabind::object &lua_object, LPCSTR condition,LPCSTR action)
{
//	try{	
//		CPHScriptObjectCondition	*c=new CPHScriptObjectCondition(lua_object,condition);
//		CPHScriptObjectAction		*a=new CPHScriptObjectAction(lua_object,action);
		luabind::functor<bool>		_condition = object_cast<luabind::functor<bool> >(lua_object[condition]);
		luabind::functor<void>		_action = object_cast<luabind::functor<void> >(lua_object[action]);
		CPHScriptObjectConditionN	*c=new CPHScriptObjectConditionN(lua_object,_condition);
		CPHScriptObjectActionN		*a=new CPHScriptObjectActionN(lua_object,_action);
		Level().ph_commander_scripts().add_call_unique(c,c,a,a);
//	}
//	catch(...)
//	{
//		Msg("add_call excepted!!");
//	}
}

void remove_call(const luabind::object &lua_object, LPCSTR condition,LPCSTR action)
{
	CPHScriptObjectCondition	c(lua_object,condition);
	CPHScriptObjectAction		a(lua_object,action);
	Level().ph_commander_scripts().remove_call(&c,&a);
}

void add_call(const luabind::object &lua_object, const luabind::functor<bool> &condition,const luabind::functor<void> &action)
{

	CPHScriptObjectConditionN	*c=new CPHScriptObjectConditionN(lua_object,condition);
	CPHScriptObjectActionN		*a=new CPHScriptObjectActionN(lua_object,action);
	Level().ph_commander_scripts().add_call(c,a);
}

void remove_call(const luabind::object &lua_object, const luabind::functor<bool> &condition,const luabind::functor<void> &action)
{
	CPHScriptObjectConditionN	c(lua_object,condition);
	CPHScriptObjectActionN		a(lua_object,action);
	Level().ph_commander_scripts().remove_call(&c,&a);
}

void remove_calls_for_object(const luabind::object &lua_object)
{
	CPHSriptReqObjComparer c(lua_object);
	Level().ph_commander_scripts().remove_calls(&c);
}

CPHWorld* physics_world()
{
	return	ph_world;
}
CEnvironment *environment()
{
	return		(g_pGamePersistent->pEnvironment);
}

CEnvDescriptor *current_environment(CEnvironment *self)
{
	return		(self->CurrentEnv);
}
extern bool g_bDisableAllInput;
extern bool g_bDisableKeyboardInput;
void disable_input()
{
	g_bDisableAllInput = true;
}
void enable_input()
{
	g_bDisableAllInput = false;
}
void disable_keyboard_input()
{
	g_bDisableKeyboardInput = true;
}
void enable_keyboard_input()
{
	g_bDisableKeyboardInput = false;
}

void spawn_phantom(const Fvector &position)
{
	Level().spawn_item("m_phantom", position, u32(-1), u16(-1), false);
}

Fbox get_bounding_volume()
{
	return Level().ObjectSpace.GetBoundingVolume();
}

void iterate_sounds					(LPCSTR prefix, u32 max_count, const CScriptCallbackEx<void> &callback)
{
	for (int j=0, N = _GetItemCount(prefix); j<N; ++j) {
		string_path					fn, s;
		LPSTR						S = (LPSTR)&s;
		_GetItem					(prefix,j,s);
		if (FS.exist(fn,"$game_sounds$",S,".ogg"))
			callback				(prefix);

		for (u32 i=0; i<max_count; ++i)
		{
			string_path					name;
			xr_sprintf					(name,"%s%d",S,i);
			if (FS.exist(fn,"$game_sounds$",name,".ogg"))
				callback			(name);
		}
	}
}

void iterate_sounds1				(LPCSTR prefix, u32 max_count, luabind::functor<void> functor)
{
	CScriptCallbackEx<void>		temp;
	temp.set					(functor);
	iterate_sounds				(prefix,max_count,temp);
}

void iterate_sounds2				(LPCSTR prefix, u32 max_count, luabind::object object, luabind::functor<void> functor)
{
	CScriptCallbackEx<void>		temp;
	temp.set					(functor,object);
	iterate_sounds				(prefix,max_count,temp);
}

#include "actoreffector.h"
float add_cam_effector(LPCSTR fn, int id, bool cyclic, LPCSTR cb_func)
{
	CAnimatorCamEffectorScriptCB* e		= new CAnimatorCamEffectorScriptCB(cb_func);
	e->SetType					((ECamEffectorType)id);
	e->SetCyclic				(cyclic);
	e->Start					(fn);
	Actor()->Cameras().AddCamEffector(e);
	return						e->GetAnimatorLength();
}

float add_cam_effector2(LPCSTR fn, int id, bool cyclic, LPCSTR cb_func)
{
	CAnimatorCamEffectorScriptCB* e		= new CAnimatorCamEffectorScriptCB(cb_func);
	e->m_bAbsolutePositioning	= true;
	e->SetType					((ECamEffectorType)id);
	e->SetCyclic				(cyclic);
	e->Start					(fn);
	Actor()->Cameras().AddCamEffector(e);
	return						e->GetAnimatorLength();
}

void remove_cam_effector(int id)
{
	Actor()->Cameras().RemoveCamEffector((ECamEffectorType)id );
}
		
float get_snd_volume()
{
	return psSoundVFactor;
}

void set_snd_volume(float v)
{
	psSoundVFactor = v;
	clamp(psSoundVFactor,0.0f,1.0f);
}
#include "actor_statistic_mgr.h"
void add_actor_points(LPCSTR sect, LPCSTR detail_key, int cnt, int pts)
{
	return Actor()->StatisticMgr().AddPoints(sect, detail_key, cnt, pts);
}

void add_actor_points_str(LPCSTR sect, LPCSTR detail_key, LPCSTR str_value)
{
	return Actor()->StatisticMgr().AddPoints(sect, detail_key, str_value);
}

int get_actor_points(LPCSTR sect)
{
	return Actor()->StatisticMgr().GetSectionPoints(sect);
}

int RandomInteger0()
{
	int rondo = Random.randI();

	return rondo;
}

int RandomInteger1(int max)
{
	int rondo = Random.randI(max);

	return rondo;
}

int RandomInteger2(int min, int max)
{
	int rondo = Random.randI(min, max);

	return rondo;
}

float RandomFloat0()
{
	float rondo = Random.randF();

	return rondo;
}

float RandomFloat1(float max)
{
	float rondo = Random.randF(max);

	return rondo;
}

float RandomFloat2(float min, float max)
{
	float rondo = Random.randF(min, max);

	return rondo;
}

extern int get_actor_ranking();
extern void add_human_to_top_list		(u16 id);
extern void remove_human_from_top_list	(u16 id);



#include "ActorEffector.h"
void add_complex_effector(LPCSTR section, int id)
{
	AddEffector(Actor(),id, section);
}

void remove_complex_effector(int id)
{
	RemoveEffector(Actor(),id);
}

#include "postprocessanimator.h"
void add_pp_effector(LPCSTR fn, int id, bool cyclic)
{
	CPostprocessAnimator* pp		= new CPostprocessAnimator(id, cyclic);
	pp->Load						(fn);
	Actor()->Cameras().AddPPEffector	(pp);
}

void remove_pp_effector(int id)
{
	CPostprocessAnimator*	pp	= smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if(pp) pp->Stop(1.0f);

}

void set_pp_effector_factor(int id, float f, float f_sp)
{
	CPostprocessAnimator*	pp	= smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if(pp) pp->SetDesiredFactor(f,f_sp);
}

void set_pp_effector_factor2(int id, float f)
{
	CPostprocessAnimator*	pp	= smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if(pp) pp->SetCurrentFactor(f);
}

#include "relation_registry.h"

 int g_community_goodwill(LPCSTR _community, int _entity_id)
 {
	 CHARACTER_COMMUNITY c;
	 c.set					(_community);

 	return RELATION_REGISTRY().GetCommunityGoodwill(c.index(), u16(_entity_id));
 }

void g_set_community_goodwill(LPCSTR _community, int _entity_id, int val)
{
	 CHARACTER_COMMUNITY	c;
	 c.set					(_community);
	RELATION_REGISTRY().SetCommunityGoodwill(c.index(), u16(_entity_id), val);
}

void g_change_community_goodwill(LPCSTR _community, int _entity_id, int val)
{
	 CHARACTER_COMMUNITY	c;
	 c.set					(_community);
	RELATION_REGISTRY().ChangeCommunityGoodwill(c.index(), u16(_entity_id), val);
}

int get_pressed_key(void)
{
	return pInput->GetLastKeyPressed();
}

int get_released_key(void)
{
	return pInput->GetLastKeyReleased();
}

int get_button_count(void)
{
	return pInput->GetButtonCount();
}

void enable_pda_skills(bool val)
{
	CUIGameSP* pGameSP = smart_cast<CUIGameSP*>(CurrentGameUI());
	if(pGameSP)pGameSP->EnableSkills(val);
}

void enable_pda_downloads(bool val)
{
	CUIGameSP* pGameSP = smart_cast<CUIGameSP*>(CurrentGameUI());
	if(pGameSP)pGameSP->EnableDownloads(val);
}

void switch_to_upgrade()
{
	CUIGameSP* ui_game_sp = smart_cast<CUIGameSP*>(CurrentGameUI());
	if (ui_game_sp)
	{
		if (ui_game_sp->TalkMenu && ui_game_sp->TalkMenu->IsShown())
			ui_game_sp->TalkMenu->SwitchToUpgrade();
	}
}

void upgrade_pda(bool upgrade)
{
	CUIGameSP* pGameSP = smart_cast<CUIGameSP*>(HUD().GetGameUI());
	if (!pGameSP) return;

	Actor()->TransferInfo("pda_upgraded", upgrade);

	xr_delete(pGameSP->m_PdaMenu);
	pGameSP->m_PdaMenu = new CUIPdaWnd();
}

#include "level_sounds.h"
void set_level_sound_enabled(bool state)
{
	if (state)
		Level().level_sound_manager().Enable();
	else
		Level().level_sound_manager().Disable();
}

bool level_sound_enabled()
{
	return Level().level_sound_manager().Enabled();
}

bool is_mixed_mode() 
{
#ifdef DEBUG
	return true;
#else
	return false;
#endif
}

u32 get_build_id() 
{
	return 0;
}
#include "dynamic_patrol_path.h"
void add_patrol(CDynamicPatrolPath *patrol) 
{
	ai().patrol_paths().add_patrol_path(patrol);
}

#pragma optimize("s",on)
void CLevel::script_register(lua_State *L)
{
	class_<CEnvDescriptor>("CEnvDescriptor")
		.def_readonly("fog_density",			&CEnvDescriptor::fog_density)
		.def_readonly("far_plane",				&CEnvDescriptor::far_plane),

	class_<CEnvironment>("CEnvironment")
		.def("current",							current_environment);

	module(L,"level")
	[
		// obsolete\deprecated
		def("object_by_id",						get_object_by_id),
		def("object_by_name",					get_object_by_name),
		def("send_event_key_pressed",			send_event_key_pressed),
		def("send_event_key_release",			send_event_key_release),
		def("send_event_key_hold",				send_event_key_hold),
		def("send_event_mouse_move",			send_event_mouse_move),
		def("send_event_mouse_wheel",			send_event_mouse_wheel),
		
#ifdef DEBUG
		def("debug_actor",						tpfGetActor),
		def("check_object",						check_object),
#endif
	 	
		def("get_weather",						get_weather),
		def("get_weather_game_time",			get_weather_game_time),
		def("get_past_wdesc_execution_time",	get_past_wdesc_execution_time),
		def("get_next_wdesc_execution_time",	get_next_wdesc_execution_time),
		def("get_past_wdesc",					get_past_wdesc),
		def("get_next_wdesc",					get_next_wdesc),
		def("set_weather",						set_weather),
		def("start_weather_fx_from_time",		start_weather_fx_from_time),
		def("set_weather_fx",					set_weather_fx),
		def("set_past_wdesc",					set_past_wdesc),
		def("set_next_wdesc",					set_next_wdesc),

		def("set_game_time",					set_game_time),
		def("is_wfx_playing",					is_wfx_playing),
		def("level_sound_enabled",				level_sound_enabled),
		def("set_level_sound_enabled",			set_level_sound_enabled),
		def("environment",						environment),
		def("get_wfx_time",						get_wfx_time),
		
		def("set_time_factor",					set_time_factor),
		def("get_time_factor",					get_time_factor),

		def("set_game_difficulty",				set_game_difficulty),
		def("get_game_difficulty",				get_game_difficulty),

		def("save_allowed",						&save_allowed),
		def("is_save_allowed",					&is_save_allowed),
		def("animation_draw",					&animation_draw),
		def("animation_holster",				&animation_holster),
		

		def("ai_ignore_actor",						&ai_ignore_actor),

		def("enable_pda_skills",			&enable_pda_skills),
		def("enable_pda_downloads",			&enable_pda_downloads),
		def("upgrade_pda",					&upgrade_pda),
		def("switch_to_upgrade",		    &switch_to_upgrade),
		
		def("get_time_days",					get_time_days),
		def("get_time_hours",					get_time_hours),
		def("get_time_minutes",					get_time_minutes),

		def("cover_in_direction",				cover_in_direction),
		def("vertex_in_direction",				vertex_in_direction),
		def("valid_vertex_id",					valid_vertex_id),
		def("vertex_position",					vertex_position),
		def("vertex_id",						&vertex_id),

		def("rain_factor",						rain_factor),
		def("patrol_path_exists",				patrol_path_exists),
		def("name",								get_name),
		def("prefetch_sound",					prefetch_sound),

		def("client_spawn_manager",				get_client_spawn_manager),

		def("map_add_object_spot_ser",			map_add_object_spot_ser),
		def("map_add_position_spot_ser",		map_add_position_spot_ser),
		def("map_add_object_spot",				map_add_object_spot),
		def("map_remove_object_spot",			map_remove_object_spot),
		def("map_highlight_spot",				map_highlight_spot),
		def("map_get_spot_hint",				map_get_spot_hint),
		def("map_has_object_spot",				map_has_object_spot),
		def("map_change_spot_hint",				map_change_spot_hint),

		def("add_patrol",						add_patrol),

		def("start_stop_menu",					start_stop_menu),
		def("add_dialog_to_render",				add_dialog_to_render),
		def("remove_dialog_to_render",			remove_dialog_to_render),
		def("main_input_receiver",				main_input_receiver),
		def("hide_indicators",					hide_indicators),
		def("show_indicators",					show_indicators),
		def("indicators_shown",					indicators_shown),
		def("add_call",							((void (*) (const luabind::functor<bool> &,const luabind::functor<void> &)) &add_call)),
		def("add_call",							((void (*) (const luabind::object &,const luabind::functor<bool> &,const luabind::functor<void> &)) &add_call)),
		def("add_call",							((void (*) (const luabind::object &, LPCSTR, LPCSTR)) &add_call)),
		def("remove_call",						((void (*) (const luabind::functor<bool> &,const luabind::functor<void> &)) &remove_call)),
		def("remove_call",						((void (*) (const luabind::object &,const luabind::functor<bool> &,const luabind::functor<void> &)) &remove_call)),
		def("remove_call",						((void (*) (const luabind::object &, LPCSTR, LPCSTR)) &remove_call)),
		def("remove_calls_for_object",			remove_calls_for_object),
		def("present",							is_level_present),
		def("disable_input",					disable_input),
		def("enable_input",						enable_input),
		def("disable_keyboard_input",			disable_keyboard_input),
		def("enable_keyboard_input",			enable_keyboard_input),
		def("spawn_phantom",					spawn_phantom),

		def("get_bounding_volume",				get_bounding_volume),

		def("iterate_sounds",					&iterate_sounds1),
		def("iterate_sounds",					&iterate_sounds2),
		def("physics_world",					&physics_world),
		def("get_snd_volume",					&get_snd_volume),
		def("set_snd_volume",					&set_snd_volume),
		def("add_cam_effector",					&add_cam_effector),
		def("add_cam_effector2",				&add_cam_effector2),
		def("remove_cam_effector",				&remove_cam_effector),
		def("add_pp_effector",					&add_pp_effector),
		def("set_pp_effector_factor",			&set_pp_effector_factor),
		def("set_pp_effector_factor",			&set_pp_effector_factor2),
		def("remove_pp_effector",				&remove_pp_effector),

		def("add_complex_effector",				&add_complex_effector),
		def("remove_complex_effector",			&remove_complex_effector),
		
		def("game_id",							&GameID)
	],
	
	module(L,"actor_stats")
	[
		def("add_points",						&add_actor_points),
		def("add_points_str",					&add_actor_points_str),
		def("get_points",						&get_actor_points),
		def("add_to_ranking",					&add_human_to_top_list),
		def("remove_from_ranking",				&remove_human_from_top_list),
		def("get_actor_ranking",				&get_actor_ranking)
	];

	module(L)
	[
		def("command_line",						&command_line),
		def("IsGameTypeSingle",					&IsGameTypeSingle),
		def("IsMixedMode",						&is_mixed_mode),
		def("EngineBuildId",					&get_build_id)
	];

	module(L,"relation_registry")
	[
		def("community_goodwill",				&g_community_goodwill),
		def("set_community_goodwill",			&g_set_community_goodwill),
		def("change_community_goodwill",		&g_change_community_goodwill)
	];
	// lost alpha start
	module(L, "keyboard")
	[
		def("get_pressed_key", &get_pressed_key),
		def("get_released_key", &get_released_key)	//,
//		def("get_button_count", &get_button_count)
	];

	module(L, "Random")
	[
		def("I", &RandomInteger0),
		def("I", &RandomInteger1),
		def("I", &RandomInteger2),
		def("F", &RandomFloat0),
		def("F", &RandomFloat1),
		def("F", &RandomFloat2)
	];
}
