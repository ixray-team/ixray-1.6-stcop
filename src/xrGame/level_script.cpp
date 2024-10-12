﻿////////////////////////////////////////////////////////////////////////////
//	Module 		: level_script.cpp
//	Created 	: 28.06.2004
//  Modified 	: 28.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Level script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "Level.h"
#include "Actor.h"
#include "script_game_object.h"
#include "patrol_path_storage.h"
#include "xrServer.h"
#include "client_spawn_manager.h"
#include "../xrEngine/igame_persistent.h"
#include "game_cl_base.h"
#include "UIGameCustom.h"
#include "../xrUI/Widgets/UIDialogWnd.h"
#include "../xrEngine/date_time.h"
#include "ai_space.h"
#include "level_graph.h"
#include "PHCommander.h"
#include "PHScriptCall.h"
#include "../xrScripts/script_engine.h"
#include "game_cl_single.h"
#include "game_sv_single.h"
#include "map_manager.h"
#include "map_spot.h"
#include "map_location.h"
#include "physics_world_scripted.h"
#include "alife_simulator.h"
#include "alife_time_manager.h"
#include "UI/UIGameTutorial.h"
#include "../xrEngine/string_table.h"
#include "ui/UIInventoryUtilities.h"
#include "alife_object_registry.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "HUDAnimItem.h"
#include "ActorCondition.h"
#include "../xrEngine/XR_IOConsole.h"
#include "Inventory.h"
#include "ShootingObject.h"
#include "Weapon.h"
#include "raypick.h"


#include "ai_object_location.h"

using namespace luabind;

void block_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().block_action(cmd);
}

bool is_block_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return false;
	}

	return Level().is_block_action(cmd);
}

void unblock_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().unblock_action(cmd);
}

void press_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().IR_OnKeyboardPress(cmd);
}

void hold_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().IR_OnKeyboardHold(cmd);
}

void release_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().IR_OnKeyboardRelease(cmd);
}

void LockActorWithCameraRotation_script() {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().LockActorWithCameraRotation();
}

void UnLockActor_script() {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().UnLockActor();
}

LPCSTR command_line()
{
	return		(Core.Params);
}
bool IsDynamicMusic()
{
	return !!psActorFlags.test(AF_DYNAMIC_MUSIC);
}

bool IsImportantSave()
{
	return !!psActorFlags.test(AF_IMPORTANT_SAVE);
}

float get_compass_direction()
{
	float compass_angle, p;
	Device.vCameraDirection.getHP(compass_angle, p);

	return compass_angle;
}

#ifdef DEBUG
void check_object(CScriptGameObject* object)
{
	try {
		Msg("check_object %s", object->Name());
	}
	catch (...) {
		object = object;
	}
}


CScriptGameObject* tpfGetActor()
{
	static bool first_time = true;
	if (first_time)
		ai().script_engine().script_log(eLuaMessageTypeError, "Do not use level.actor function!");
	first_time = false;

	CActor* l_tpActor = smart_cast<CActor*>(Level().CurrentEntity());
	if (l_tpActor)
		return	(smart_cast<CGameObject*>(l_tpActor)->lua_game_object());
	else
		return	(0);
}

CScriptGameObject* get_object_by_name(LPCSTR caObjectName)
{
	static bool first_time = true;
	if (first_time)
		ai().script_engine().script_log(eLuaMessageTypeError, "Do not use level.object function!");
	first_time = false;

	CGameObject* l_tpGameObject = smart_cast<CGameObject*>(Level().Objects.FindObjectByName(caObjectName));
	if (l_tpGameObject)
		return		(l_tpGameObject->lua_game_object());
	else
		return		(0);
}
#endif

CScriptGameObject* get_object_by_id(u16 id)
{
	CGameObject* pGameObject = smart_cast<CGameObject*>(Level().Objects.net_Find(id));
	if (!pGameObject)
		return nullptr;

	return pGameObject->lua_game_object();
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

LPCSTR get_weather()
{
	return			(*g_pGamePersistent->Environment().GetWeather());
}

void set_weather(LPCSTR weather_name, bool forced)
{
	g_pGamePersistent->Environment().SetWeather(weather_name, forced);
}

bool set_weather_fx(LPCSTR weather_name)
{
	return		(g_pGamePersistent->Environment().SetWeatherFX(weather_name));
}

bool start_weather_fx_from_time(LPCSTR weather_name, float time)
{
	return		(g_pGamePersistent->Environment().StartWeatherFXFromTime(weather_name, time));
}

bool is_wfx_playing()
{
	return			(g_pGamePersistent->Environment().IsWFXPlaying());
}

float get_wfx_time()
{
	return			(g_pGamePersistent->Environment().wfx_time);
}

void stop_weather_fx()
{
	g_pGamePersistent->Environment().StopWFX();
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

void set_global_time_factor(float tf) {
	if (!OnServer())
		return;

	Device.time_factor(tf);
}

float get_global_time_factor() { return (Device.time_factor()); }

void set_game_difficulty(ESingleGameDifficulty dif)
{
	g_SingleGameDifficulty = dif;
	game_cl_Single* game = smart_cast<game_cl_Single*>(Level().game); VERIFY(game);
	game->OnDifficultyChanged();
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

void change_game_time(u32 days, u32 hours, u32 mins)
{
	game_sv_Single* tpGame = smart_cast<game_sv_Single*>(Level().Server->game);
	if (tpGame && ai().get_alife())
	{
		u32 value = days * 86400 + hours * 3600 + mins * 60;
		float fValue = static_cast<float> (value);
		value *= 1000;//msec		
		g_pGamePersistent->Environment().ChangeGameTime(fValue);
		tpGame->alife().time_manager().change_game_time(value);
	}
}

float high_cover_in_direction(u32 level_vertex_id, const Fvector& direction)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
		return 0.0f;
	}

	float			y, p;
	direction.getHP(y, p);
	return			(ai().level_graph().high_cover_in_direction(y, level_vertex_id));
}

float low_cover_in_direction(u32 level_vertex_id, const Fvector& direction)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
		return 0.0f;
	}

	float			y, p;
	direction.getHP(y, p);
	return			(ai().level_graph().low_cover_in_direction(y, level_vertex_id));
}

float rain_factor()
{
	return			(g_pGamePersistent->Environment().CurrentEnv->rain_density);
}

u32	vertex_in_direction(u32 level_vertex_id, Fvector direction, float max_distance)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
		return u32(-1);
	}

	direction.normalize_safe();
	direction.mul(max_distance);
	Fvector			start_position = ai().level_graph().vertex_position(level_vertex_id);
	Fvector			finish_position = Fvector(start_position).add(direction);
	u32				result_ = u32(-1);
	ai().level_graph().farthest_vertex_in_direction(level_vertex_id, start_position, finish_position, result_, 0);
	return			(ai().level_graph().valid_vertex_id(result_) ? result_ : level_vertex_id);
}

Fvector vertex_position(u32 level_vertex_id)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
		return Fvector{};
	}
	return			(ai().level_graph().vertex_position(level_vertex_id));
}

void map_add_object_spot(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml = Level().MapManager().AddMapLocation(spot_type, id);
	if (xr_strlen(text))
	{
		ml->SetHint(text);
	}
}

void map_add_object_spot_ser(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml = Level().MapManager().AddMapLocation(spot_type, id);
	if (xr_strlen(text))
		ml->SetHint(text);

	ml->SetSerializable(true);
}

void map_change_spot_hint(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml = Level().MapManager().GetMapLocation(spot_type, id);
	if (!ml)				return;
	ml->SetHint(text);
}

void map_remove_object_spot(u16 id, LPCSTR spot_type)
{
	Level().MapManager().RemoveMapLocation(spot_type, id);
}

u16 map_has_object_spot(u16 id, LPCSTR spot_type)
{
	return Level().MapManager().HasMapLocation(spot_type, id);
}

CMapManager* get_map_manager()
{
	return &Level().MapManager();
}

bool patrol_path_exists(LPCSTR patrol_path)
{
	return		(!!ai().patrol_paths().path(patrol_path, true));
}

LPCSTR get_name()
{
	return		(*Level().name());
}

void prefetch_sound(LPCSTR name)
{
	Level().PrefetchSound(name);
}


CClientSpawnManager& get_client_spawn_manager()
{
	return		(Level().client_spawn_manager());
}
/*
void start_stop_menu(CUIDialogWnd* pDialog, bool bDoHideIndicators)
{
	if(pDialog->IsShown())
		pDialog->HideDialog();
	else
		pDialog->ShowDialog(bDoHideIndicators);
}
*/

void add_dialog_to_render(CUIDialogWnd* pDialog)
{
	CurrentGameUI()->AddDialogToRender(pDialog);
}

void remove_dialog_to_render(CUIDialogWnd* pDialog)
{
	CurrentGameUI()->RemoveDialogToRender(pDialog);
}

void hide_indicators()
{
	if (CurrentGameUI())
	{
		CurrentGameUI()->HideShownDialogs();
		CurrentGameUI()->ShowGameIndicators(false);
		CurrentGameUI()->ShowCrosshair(false);
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, TRUE);
}

void hide_indicators_safe()
{
	if (CurrentGameUI())
	{
		CurrentGameUI()->ShowGameIndicators(false);
		CurrentGameUI()->ShowCrosshair(false);

		CurrentGameUI()->OnExternalHideIndicators();
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, TRUE);
}

void show_indicators()
{
	if (CurrentGameUI())
	{
		CurrentGameUI()->ShowGameIndicators(true);
		CurrentGameUI()->ShowCrosshair(true);
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, FALSE);
}

void show_weapon(bool b)
{
	psHUD_Flags.set(HUD_WEAPON_RT2, b);
}

bool is_level_present()
{
	return (!!g_pGameLevel);
}

void add_call(const luabind::functor<bool>& condition, const luabind::functor<void>& action)
{
	luabind::functor<bool>		_condition = condition;
	luabind::functor<void>		_action = action;
	CPHScriptCondition* c = new CPHScriptCondition(_condition);
	CPHScriptAction* a = new CPHScriptAction(_action);
	Level().ph_commander_scripts().add_call(c, a);
}

void remove_call(const luabind::functor<bool>& condition, const luabind::functor<void>& action)
{
	CPHScriptCondition	c(condition);
	CPHScriptAction		a(action);
	Level().ph_commander_scripts().remove_call(&c, &a);
}

void add_call(const luabind::object& lua_object, LPCSTR condition, LPCSTR action)
{
	//	try{	
	//		CPHScriptObjectCondition	*c=new CPHScriptObjectCondition(lua_object,condition);
	//		CPHScriptObjectAction		*a=new CPHScriptObjectAction(lua_object,action);
	luabind::functor<bool>		_condition = object_cast<luabind::functor<bool>>(lua_object[condition]);
	luabind::functor<void>		_action = object_cast<luabind::functor<void>>(lua_object[action]);
	CPHScriptObjectConditionN* c = new CPHScriptObjectConditionN(lua_object, _condition);
	CPHScriptObjectActionN* a = new CPHScriptObjectActionN(lua_object, _action);
	Level().ph_commander_scripts().add_call_unique(c, c, a, a);
	//	}
	//	catch(...)
	//	{
	//		Msg("add_call excepted!!");
	//	}
}

void remove_call(const luabind::object& lua_object, LPCSTR condition, LPCSTR action)
{
	CPHScriptObjectCondition	c(lua_object, condition);
	CPHScriptObjectAction		a(lua_object, action);
	Level().ph_commander_scripts().remove_call(&c, &a);
}

void add_call(const luabind::object& lua_object, const luabind::functor<bool>& condition, const luabind::functor<void>& action)
{

	CPHScriptObjectConditionN* c = new CPHScriptObjectConditionN(lua_object, condition);
	CPHScriptObjectActionN* a = new CPHScriptObjectActionN(lua_object, action);
	Level().ph_commander_scripts().add_call(c, a);
}

void remove_call(const luabind::object& lua_object, const luabind::functor<bool>& condition, const luabind::functor<void>& action)
{
	CPHScriptObjectConditionN	c(lua_object, condition);
	CPHScriptObjectActionN		a(lua_object, action);
	Level().ph_commander_scripts().remove_call(&c, &a);
}

void remove_calls_for_object(const luabind::object& lua_object)
{
	CPHSriptReqObjComparer c(lua_object);
	Level().ph_commander_scripts().remove_calls(&c);
}

cphysics_world_scripted* physics_world_scripted()
{
	return	get_script_wrapper<cphysics_world_scripted>(*physics_world());
}
CEnvironment* environment()
{
	return		(g_pGamePersistent->pEnvironment);
}

CEnvDescriptor* current_environment(CEnvironment* self_)
{
	return		(self_->CurrentEnv);
}
extern bool g_bDisableAllInput;
void disable_input()
{
	g_bDisableAllInput = true;
#ifdef DEBUG
	Msg("input disabled");
#endif // #ifdef DEBUG
}
void enable_input()
{
	g_bDisableAllInput = false;
#ifdef DEBUG
	Msg("input enabled");
#endif // #ifdef DEBUG
}

void spawn_phantom(const Fvector& position)
{
	Level().spawn_item("m_phantom", position, u32(-1), u16(-1), false);
}

Fbox get_bounding_volume()
{
	return Level().ObjectSpace.GetBoundingVolume();
}

void iterate_sounds(LPCSTR prefix, u32 max_count, const CScriptCallbackEx<void>& callback)
{
	for (int j = 0, N = _GetItemCount(prefix); j < N; ++j) {
		string_path					fn, s;
		LPSTR						S = (LPSTR)&s;
		_GetItem(prefix, j, s);
		if (FS.exist(fn, "$game_sounds$", S, ".ogg"))
			callback(prefix);

		for (u32 i = 0; i < max_count; ++i)
		{
			string_path					name;
			xr_sprintf(name, "%s%d", S, i);
			if (FS.exist(fn, "$game_sounds$", name, ".ogg"))
				callback(name);
		}
	}
}

void iterate_sounds1(LPCSTR prefix, u32 max_count, luabind::functor<void> functor)
{
	CScriptCallbackEx<void>		temp;
	temp.set(functor);
	iterate_sounds(prefix, max_count, temp);
}

void iterate_sounds2(LPCSTR prefix, u32 max_count, luabind::object object, luabind::functor<void> functor)
{
	CScriptCallbackEx<void>		temp;
	temp.set(functor, object);
	iterate_sounds(prefix, max_count, temp);
}

#include "actoreffector.h"
float add_cam_effector(LPCSTR fn, int id, bool cyclic, LPCSTR cb_func)
{
	CAnimatorCamEffectorScriptCB* e = new CAnimatorCamEffectorScriptCB(cb_func);
	e->SetType((ECamEffectorType)id);
	e->SetCyclic(cyclic);
	e->Start(fn);
	Actor()->Cameras().AddCamEffector(e);
	return						e->GetAnimatorLength();
}

float add_cam_effector2(LPCSTR fn, int id, bool cyclic, LPCSTR cb_func, float cam_fov)
{
	CAnimatorCamEffectorScriptCB* e = new CAnimatorCamEffectorScriptCB(cb_func);
	e->m_bAbsolutePositioning = true;
	e->m_fov = cam_fov;
	e->SetType((ECamEffectorType)id);
	e->SetCyclic(cyclic);
	e->Start(fn);
	Actor()->Cameras().AddCamEffector(e);
	return						e->GetAnimatorLength();
}

void remove_cam_effector(int id)
{
	Actor()->Cameras().RemoveCamEffector((ECamEffectorType)id);
}

float get_snd_volume()
{
	return psSoundVFactor;
}

void set_snd_volume(float v)
{
	psSoundVFactor = v;
	clamp(psSoundVFactor, 0.0f, 1.0f);
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



#include "ActorEffector.h"
void add_complex_effector(LPCSTR section, int id)
{
	AddEffector(Actor(), id, section);
}

void remove_complex_effector(int id)
{
	RemoveEffector(Actor(), id);
}

#include "postprocessanimator.h"
void add_pp_effector(LPCSTR fn, int id, bool cyclic)
{
	CPostprocessAnimator* pp = new CPostprocessAnimator(id, cyclic);
	pp->Load(fn);
	Actor()->Cameras().AddPPEffector(pp);
}

void remove_pp_effector(int id)
{
	CPostprocessAnimator* pp = smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if (pp) pp->Stop(1.0f);

}

void set_pp_effector_factor(int id, float f, float f_sp)
{
	CPostprocessAnimator* pp = smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if (pp) pp->SetDesiredFactor(f, f_sp);
}

void set_pp_effector_factor2(int id, float f)
{
	CPostprocessAnimator* pp = smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if (pp) pp->SetCurrentFactor(f);
}

#include "relation_registry.h"

int g_community_goodwill(LPCSTR _community, int _entity_id)
{
	CHARACTER_COMMUNITY c;
	c.set(_community);

	return RELATION_REGISTRY().GetCommunityGoodwill(c.index(), u16(_entity_id));
}

void g_set_community_goodwill(LPCSTR _community, int _entity_id, int val)
{
	CHARACTER_COMMUNITY	c;
	c.set(_community);
	RELATION_REGISTRY().SetCommunityGoodwill(c.index(), u16(_entity_id), val);
}

void g_change_community_goodwill(LPCSTR _community, int _entity_id, int val)
{
	CHARACTER_COMMUNITY	c;
	c.set(_community);
	RELATION_REGISTRY().ChangeCommunityGoodwill(c.index(), u16(_entity_id), val);
}

int g_get_community_relation(LPCSTR comm_from, LPCSTR comm_to)
{
	CHARACTER_COMMUNITY	community_from;
	community_from.set(comm_from);
	CHARACTER_COMMUNITY	community_to;
	community_to.set(comm_to);

	return RELATION_REGISTRY().GetCommunityRelation(community_from.index(), community_to.index());
}

void g_set_community_relation(LPCSTR comm_from, LPCSTR comm_to, int value)
{
	CHARACTER_COMMUNITY	community_from;
	community_from.set(comm_from);
	CHARACTER_COMMUNITY	community_to;
	community_to.set(comm_to);

	RELATION_REGISTRY().SetCommunityRelation(community_from.index(), community_to.index(), value);
}

int g_get_general_goodwill_between(u16 from, u16 to)
{
	CHARACTER_GOODWILL presonal_goodwill = RELATION_REGISTRY().GetGoodwill(from, to); VERIFY(presonal_goodwill != NO_GOODWILL);

	CSE_ALifeTraderAbstract* from_obj = smart_cast<CSE_ALifeTraderAbstract*>(ai().alife().objects().object(from));
	CSE_ALifeTraderAbstract* to_obj = smart_cast<CSE_ALifeTraderAbstract*>(ai().alife().objects().object(to));

	if (!from_obj || !to_obj) {
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "RELATION_REGISTRY::get_general_goodwill_between  : cannot convert obj to CSE_ALifeTraderAbstract!");
		return (0);
	}
	CHARACTER_GOODWILL community_to_obj_goodwill = RELATION_REGISTRY().GetCommunityGoodwill(from_obj->Community(), to);
	CHARACTER_GOODWILL community_to_community_goodwill = RELATION_REGISTRY().GetCommunityRelation(from_obj->Community(), to_obj->Community());

	return presonal_goodwill + community_to_obj_goodwill + community_to_community_goodwill;
}

u32 vertex_id(Fvector position)
{
	return	(ai().level_graph().vertex_id(position));
}

u32 render_get_dx_level()
{
	return ::Render->get_dx_level();
}

CUISequencer* g_tutorial = nullptr;
CUISequencer* g_tutorial2 = nullptr;

void start_tutorial(LPCSTR name)
{
	if (load_screen_renderer.IsActive()) {
		return;
	}

	if (g_tutorial) {
		VERIFY(!g_tutorial2);
		g_tutorial2 = g_tutorial;
	};

	g_tutorial = new CUISequencer();
	g_tutorial->Start(name);
	if (g_tutorial2)
		g_tutorial->m_pStoredInputReceiver = g_tutorial2->m_pStoredInputReceiver;

}

void stop_tutorial()
{
	if (g_tutorial)
		g_tutorial->Stop();
}

LPCSTR tutorial_name()
{
	if (g_tutorial)
		return g_tutorial->m_name;
	return "invalid";
}

LPCSTR translate_string(LPCSTR str)
{
	return *g_pStringTable->translate(str);
}

bool has_active_tutotial()
{
	return (g_tutorial != nullptr);
}

bool valid_vertex_id(u32 level_vertex_id) {
	return ai().level_graph().valid_vertex_id(level_vertex_id);
}

bool is_accessible_vertex_id(u32 level_vertex_id) {
	return ai().level_graph().is_accessible(level_vertex_id);
}

void disable_vertex(u32 vertex_id) {
	ai().level_graph().set_mask(vertex_id);
}

void enable_vertex(u32 vertex_id) {
	ai().level_graph().clear_mask(vertex_id);
}

bool is_dedicated()
{
	return g_dedicated_server;
}

//ability to update level netpacket
void g_send(NET_Packet& P, bool bReliable = 0, bool bSequential = 1, bool bHighPriority = 0, bool bSendImmediately = 0)
{
	Level().Send(P, net_flags(bReliable, bSequential, bHighPriority, bSendImmediately));
}

void u_event_gen(NET_Packet& P, u32 _event, u32 _dest)
{
	CGameObject::u_EventGen(P, _event, _dest);
}

void u_event_send(NET_Packet& P)
{
	CGameObject::u_EventSend(P);
}

//can spawn entities like bolts, phantoms, ammo, etc. which normally crash when using alife():create()
void spawn_section(LPCSTR sSection, Fvector3 vPosition, u32 LevelVertexID, u16 ParentID, bool bReturnItem = false)
{
	Level().spawn_item(sSection, vPosition, LevelVertexID, ParentID, bReturnItem);
}

#include "HUDManager.h"
//ability to get the target game_object at crosshair
CScriptGameObject* g_get_target_obj()
{
	collide::rq_result& RQ = HUD().GetCurrentRayQuery();
	if (RQ.O)
	{
		CGameObject* game_object = static_cast<CGameObject*>(RQ.O);
		if (game_object)
			return game_object->lua_game_object();
	}
	return (0);
}

float g_get_target_dist()
{
	collide::rq_result& RQ = HUD().GetCurrentRayQuery();
	if (RQ.range)
		return RQ.range;
	return (0);
}

u32 g_get_target_element()
{
	collide::rq_result& RQ = HUD().GetCurrentRayQuery();
	if (RQ.element)
	{
		return RQ.element;
	}
	return (0);
}

u8 get_active_cam()
{
	CActor* actor = smart_cast<CActor*>(Level().CurrentViewEntity());
	if (actor)
		return (u8)actor->active_cam();

	return 255;
}

void LevelPressAction(EGameActions cmd)
{
	Level().IR_OnKeyboardPress(cmd);
}

void LevelReleaseAction(EGameActions cmd)
{
	Level().IR_OnKeyboardRelease(cmd);
}

void LevelHoldAction(EGameActions cmd)
{
	Level().IR_OnKeyboardHold(cmd);
}

bool valid_vertex(u32 level_vertex_id)
{
	return ai().level_graph().valid_vertex_id(level_vertex_id);
}

xrTime get_start_time()
{
	return (xrTime(Level().GetStartGameTime()));
}

void reload_language()
{
	g_pStringTable->ReloadLanguage(); 
}

CScriptGameObject* get_view_entity_script()
{
	CGameObject* pGameObject = smart_cast<CGameObject*>(Level().CurrentViewEntity());
	if (!pGameObject)
		return (0);

	return pGameObject->lua_game_object();
}

void set_view_entity_script(CScriptGameObject* go)
{
	CObject* o = smart_cast<CObject*>(&go->object());
	if (o)
		Level().SetViewEntity(o);
}

void set_active_cam(u8 mode)
{
	CActor* actor = smart_cast<CActor*>(Level().CurrentViewEntity());
	if (actor && mode <= ACTOR_DEFS::EActorCameras::eacMaxCam)
		actor->cam_Set((ACTOR_DEFS::EActorCameras)mode);
}

namespace level_nearest
{
	xr_vector<CObject*> ObjectList;
	void Set(float Radius, const Fvector& Pos)
	{
		ObjectList.clear();
		g_pGameLevel->ObjectSpace.GetNearest(ObjectList, Pos, Radius, nullptr);
	}

	u32 Size()
	{
		return (u32)ObjectList.size();
	}

	CScriptGameObject* Get(int Idx)
	{
		if (Idx > (int)ObjectList.size())
			return nullptr;

		CGameObject* pObj = smart_cast<CGameObject*>(ObjectList[Idx]);
		return pObj->lua_game_object();
	}
}
void patrol_path_add(LPCSTR patrol_path, CPatrolPath* path) {
	ai().patrol_paths_raw().add_path(shared_str(patrol_path), path);
								
}

void patrol_path_remove(LPCSTR patrol_path) {
	ai().patrol_paths_raw().remove_path(shared_str(patrol_path));
}
#pragma optimize("s",on)
void CLevel::script_register(lua_State* L)
{
	class_<CEnvDescriptor>("CEnvDescriptor")
		.def_readonly("fog_density", &CEnvDescriptor::fog_density)
		.def_readonly("far_plane", &CEnvDescriptor::far_plane),

		class_<CEnvironment>("CEnvironment")
		.def("current", current_environment);

	module(L, "level")
		[
			// obsolete\deprecated
			def("object_by_id", get_object_by_id),
#ifdef DEBUG
				def("debug_object", get_object_by_name),
				def("debug_actor", tpfGetActor),
				def("check_object", check_object),
#endif

				def("get_weather", get_weather),
				def("set_weather", set_weather),
				def("set_weather_fx", set_weather_fx),
				def("set_past_weather", set_past_wdesc),
				def("set_next_weather", set_next_wdesc),
				def("get_weather_game_time", get_weather_game_time),
				def("get_past_wdesc_execution_time", get_past_wdesc_execution_time),
				def("get_next_wdesc_execution_time", get_next_wdesc_execution_time),
				def("get_past_weather", get_past_wdesc),
				def("get_next_weather", get_next_wdesc),
				def("start_weather_fx_from_time", start_weather_fx_from_time),
				def("is_wfx_playing", is_wfx_playing),
				def("get_wfx_time", get_wfx_time),
				def("stop_weather_fx", stop_weather_fx),

				def("environment", environment),

				def("set_time_factor", set_time_factor),
				def("get_time_factor", get_time_factor),

				def("set_global_time_factor", &set_global_time_factor),
				def("get_global_time_factor", &get_global_time_factor),

				def("set_game_difficulty", set_game_difficulty),
				def("get_game_difficulty", get_game_difficulty),

				def("get_time_days", get_time_days),
				def("get_time_hours", get_time_hours),
				def("get_time_minutes", get_time_minutes),
				def("change_game_time", change_game_time),

				def("high_cover_in_direction", high_cover_in_direction),
				def("low_cover_in_direction", low_cover_in_direction),
				def("vertex_in_direction", vertex_in_direction),
				def("rain_factor", rain_factor),
				def("patrol_path_exists", patrol_path_exists),
				def("vertex_position", vertex_position),
				def("name", get_name),
				def("prefetch_sound", prefetch_sound),

				def("client_spawn_manager", get_client_spawn_manager),

				def("map_add_object_spot_ser", map_add_object_spot_ser),
				def("map_add_object_spot", map_add_object_spot),
				//-		def("map_add_object_spot_complex",		map_add_object_spot_complex),
				def("map_remove_object_spot", map_remove_object_spot),
				def("map_has_object_spot", map_has_object_spot),
				def("map_change_spot_hint", map_change_spot_hint),
				def("map_manager",						get_map_manager),

				def("add_dialog_to_render", add_dialog_to_render),
				def("remove_dialog_to_render", remove_dialog_to_render),
				def("hide_indicators", hide_indicators),
				def("hide_indicators_safe", hide_indicators_safe),

				def("show_indicators", show_indicators),
				def("show_weapon", show_weapon),
				def("add_call", ((void (*) (const luabind::functor<bool> &, const luabind::functor<void> &)) & add_call)),
				def("add_call", ((void (*) (const luabind::object&, const luabind::functor<bool> &, const luabind::functor<void> &)) & add_call)),
				def("add_call", ((void (*) (const luabind::object&, LPCSTR, LPCSTR)) & add_call)),
				def("remove_call", ((void (*) (const luabind::functor<bool> &, const luabind::functor<void> &)) & remove_call)),
				def("remove_call", ((void (*) (const luabind::object&, const luabind::functor<bool> &, const luabind::functor<void> &)) & remove_call)),
				def("remove_call", ((void (*) (const luabind::object&, LPCSTR, LPCSTR)) & remove_call)),
				def("remove_calls_for_object", remove_calls_for_object),
				def("present", is_level_present),
				def("disable_input", disable_input),
				def("enable_input", enable_input),
				def("spawn_phantom", spawn_phantom),

				def("get_bounding_volume", get_bounding_volume),

				def("iterate_sounds", &iterate_sounds1),
				def("iterate_sounds", &iterate_sounds2),
				def("physics_world", &physics_world_scripted),
				def("get_snd_volume", &get_snd_volume),
				def("set_snd_volume", &set_snd_volume),
				def("add_cam_effector", &add_cam_effector),
				def("add_cam_effector2", &add_cam_effector2),
				def("remove_cam_effector", &remove_cam_effector),
				def("add_pp_effector", &add_pp_effector),
				def("set_pp_effector_factor", &set_pp_effector_factor),
				def("set_pp_effector_factor", &set_pp_effector_factor2),
				def("remove_pp_effector", &remove_pp_effector),
				def("get_compass_direction", &get_compass_direction),

				def("add_complex_effector", &add_complex_effector),
				def("remove_complex_effector", &remove_complex_effector),

				def("valid_vertex_id", valid_vertex_id),
				def("is_accessible_vertex_id", is_accessible_vertex_id),
				def("disable_vertex", disable_vertex),
				def("enable_vertex", enable_vertex),
				def("vertex_id", &vertex_id),

				def("game_id", &GameID),

				def("block_action", &block_action_script),
				def("is_block_action", &is_block_action_script),
				def("unblock_action", &unblock_action_script),
				def("press_action", &press_action_script),
				def("hold_action", &hold_action_script),
				def("release_action", &release_action_script),
				def("lock_actor", &LockActorWithCameraRotation_script),
				def("unlock_actor", &UnLockActor_script),

				def("patrol_path_add", &patrol_path_add),
				def("patrol_path_remove", &patrol_path_remove),
				def("u_event_gen", &u_event_gen), //Send events via packet
				def("u_event_send", &u_event_send),
				def("send", &g_send), //allow the ability to send netpacket to level
				def("get_target_obj", &g_get_target_obj), //intentionally named to what is in xray extensions
				def("get_target_dist", &g_get_target_dist),
				def("press_action", &LevelPressAction),
				def("release_action", &LevelReleaseAction),
				def("hold_action", &LevelHoldAction),
				def("get_target_element", &g_get_target_element), //Can get bone cursor is targetting
				def("get_view_entity", &get_view_entity_script),
				def("set_view_entity", &set_view_entity_script),
				def("spawn_item", &spawn_section),
				def("get_active_cam", &get_active_cam),
				def("set_active_cam", &set_active_cam),
				def("get_start_time", &get_start_time),
				def("valid_vertex", &valid_vertex)
		],

		module(L, "nearest")
		[
			def("set", &level_nearest::Set),
				def("size", &level_nearest::Size),
				def("get", &level_nearest::Get)
		];

	module(L, "animslot")
		[
			def("play", &CHUDAnimItem::PlayHudAnim)
		];

	module(L, "actor_stats")
		[
			def("add_points", &add_actor_points),
				def("add_points_str", &add_actor_points_str),
				def("get_points", &get_actor_points)
		];
	module(L)
	[
	   class_<CRayPick>("ray_pick")
	   .def(								constructor<>())
	   .def(								constructor<Fvector&, Fvector&, float, collide::rq_target, CScriptGameObject*>())
	   .def("set_position",				&CRayPick::set_position)
	   .def("set_direction",				&CRayPick::set_direction)
	   .def("set_range",					&CRayPick::set_range)
	   .def("set_flags",					&CRayPick::set_flags)
	   .def("set_ignore_object",			&CRayPick::set_ignore_object)
	   .def("query",						&CRayPick::query)
	   .def("get_result",					&CRayPick::get_result)
	   .def("get_object",					&CRayPick::get_object)
	   .def("get_distance",				&CRayPick::get_distance)
	   .def("get_element",					&CRayPick::get_element),	
    class_<script_rq_result>("rq_result")
      .def_readonly("object",			&script_rq_result::O)
      .def_readonly("range",			&script_rq_result::range)
      .def_readonly("element",		&script_rq_result::element)
      .def(								constructor<>()), 	
    class_<enum_exporter<collide::rq_target> >("rq_target")
      .enum_("targets")
    [
      value("rqtNone",						int(collide::rqtNone)),
      value("rqtObject",						int(collide::rqtObject)),
      value("rqtStatic",						int(collide::rqtStatic)),
      value("rqtShape",						int(collide::rqtShape)),
      value("rqtObstacle",					int(collide::rqtObstacle)),
      value("rqtBoth",						int(collide::rqtBoth)),
      value("rqtDyn",							int(collide::rqtDyn))
    ]
	];  

	module(L)
		[
			def("command_line", &command_line),
				def("IsGameTypeSingle", &IsGameTypeSingle),
				def("IsDynamicMusic", &IsDynamicMusic),
				def("render_get_dx_level", &render_get_dx_level),
				def("IsImportantSave", &IsImportantSave),
				def("IsDedicated", &is_dedicated),
				def("OnClient", &OnClient),
				def("OnServer", &OnServer)
		];

	module(L, "relation_registry")
		[
			def("community_goodwill", &g_community_goodwill),
				def("set_community_goodwill", &g_set_community_goodwill),
				def("change_community_goodwill", &g_change_community_goodwill),

				def("community_relation", &g_get_community_relation),
				def("set_community_relation", &g_set_community_relation),
				def("get_general_goodwill_between", &g_get_general_goodwill_between)
		];

	module(L, "game")
		[
			class_< xrTime >("CTime")
				.enum_("date_format")
				[
					value("DateToDay", int(InventoryUtilities::edpDateToDay)),
						value("DateToMonth", int(InventoryUtilities::edpDateToMonth)),
						value("DateToYear", int(InventoryUtilities::edpDateToYear))
				]
				.enum_("time_format")
				[
					value("TimeToHours", int(InventoryUtilities::etpTimeToHours)),
						value("TimeToMinutes", int(InventoryUtilities::etpTimeToMinutes)),
						value("TimeToSeconds", int(InventoryUtilities::etpTimeToSeconds)),
						value("TimeToMilisecs", int(InventoryUtilities::etpTimeToMilisecs))
				]
				.def(constructor<>())
				.def(constructor<const xrTime&>())
				.def(const_self < xrTime())
				.def(const_self <= xrTime())
				.def(const_self > xrTime())
				.def(const_self >= xrTime())
				.def(const_self == xrTime())
				.def(self + xrTime())
				.def(self - xrTime())

				.def("diffSec", &xrTime::diffSec_script)
				.def("add", &xrTime::add_script)
				.def("sub", &xrTime::sub_script)

				.def("save", &xrTime::Save)
				.def("load", &xrTime::Load)

				.def("setHMS", &xrTime::setHMS)
				.def("setHMSms", &xrTime::setHMSms)
				.def("set", &xrTime::set)
				.def("get", &xrTime::get, out_value<2>() + out_value<3>() + out_value<4>() + out_value<5>() + out_value<6>() + out_value<7>() + out_value<8>())
				.def("dateToString", &xrTime::dateToString)
				.def("timeToString", &xrTime::timeToString),
				// declarations
				def("time", get_time),
				def("get_game_time", get_time_struct),
				//			def("get_surge_time",	Game::get_surge_time),
				//			def("get_object_by_name",Game::get_object_by_name),

				def("start_tutorial", &start_tutorial),
				def("stop_tutorial", &stop_tutorial),
				def("has_active_tutorial", &has_active_tutotial),
				def("active_tutorial_name", &tutorial_name),
				def("translate_string",		&translate_string),
				def("reload_language",		&reload_language)
//				def("log_stack_trace",		&LogStackTrace)
		];
}
