////////////////////////////////////////////////////////////////////////////
//	Module 		: level_script.cpp
//	Created 	: 28.06.2004
//  Modified 	: 28.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Level script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "Level.h"
#include "Actor.h"
#include "script_game_object.h"
#include "patrol_path_storage.h"
#include "xrServer.h"
#include "client_spawn_manager.h"
#include "../xrEngine/IGame_Persistent.h"
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
#include "ui/UIGameTutorial.h"
#include "../xrEngine/string_table.h"
#include "ui/UIInventoryUtilities.h"
#include "alife_object_registry.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "ActorCondition.h"
#include "player_hud.h"
#include "../xrEngine/XR_IOConsole.h"
#include "Inventory.h"
#include "ShootingObject.h"
#include "Weapon.h"
#include "raypick.h"
#include "ai_object_location.h"
#include "inventory_upgrade_manager.h"
#include "ActorHelmet.h"
#include "DynamicWallmarkZone.h"
#include "InventoryVolumeSystem.h"
#include "PickupManager.h"
#include "UIActorMenu.h"
#include "../xrServerEntities/restriction_space.h"
#include "../xrEngine/GameMtlLib.h"
#include "../xrEngine/Rain.h"
#include "../xrEngine/thunderbolt.h"
#include "material_manager.h"
#include "../xrUI/Widgets/UIActionRepeaters.h"
#include "Cutscenes/CutsceneItem.h"
#include "Cutscenes/CutsceneManager.h"

#include "ElectronicsProblemsManager.h"
#include "SamZone.h"
#include "SaveObjectHelpers.h"

using namespace luabind;

void show_legs(bool val)
{
	g_player_hud->m_show_legs += val ? 1 : -1;
}

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

const char* command_line	()
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
	}
}


CScriptGameObject* tpfGetActor()
{
	static bool first_time = true;
	if (first_time)
		ai().script_engine().script_log(eLuaMessageTypeError, "Do not use level.actor function!");
	first_time = false;

	CObject* current_entity = Level().CurrentEntity();
	if (CActor* l_tpActor = current_entity != nullptr ? current_entity->cast_actor() : nullptr)
	{
		return (smart_cast<CGameObject*>(l_tpActor)->lua_game_object());
	}
	else
	{
		return 0;
	}
}

CScriptGameObject* get_object_by_name(const char* caObjectName)
{
	static bool first_time = true;
	if (first_time)
	{
		ai().script_engine().script_log(eLuaMessageTypeError, "Do not use level.object function!");
	}

	first_time = false;

	CObject* finded_object = Level().Objects.FindObjectByName(caObjectName);
	if (CGameObject* l_tpGameObject = finded_object != nullptr ? finded_object->cast_game_object() : nullptr)
	{
		return l_tpGameObject->lua_game_object();
	}
	else
	{
		return 0;
	}
}
#endif

CScriptGameObject *get_object_by_id(ALife::_OBJECT_ID id)
{
	CObject* finded_object = Level().Objects.net_Find(id);
	CGameObject* pGameObject = finded_object != nullptr ? finded_object->cast_game_object() : nullptr;
	if (!pGameObject)
	{
		//g_pScriptEngine->print_stack();
		return nullptr;
	}

	return pGameObject->lua_game_object();
}

const char* get_past_wdesc()
{
	return			(g_pGamePersistent->Environment().Current[0] ? g_pGamePersistent->Environment().Current[0]->m_identifier.c_str() : "null");
}

const char* get_next_wdesc()
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
	return (g_pGamePersistent->Environment().GetGameTime());
}

void set_past_wdesc(const char* WeatherSection)
{
	g_pGamePersistent->Environment().SetEnvDesc(WeatherSection, g_pGamePersistent->Environment().Current[0]);
}

void set_next_wdesc(const char* WeatherSection)
{
	g_pGamePersistent->Environment().SetEnvDesc(WeatherSection, g_pGamePersistent->Environment().Current[1]);
}

const char* get_weather	()
{
	return			(*g_pGamePersistent->Environment().GetWeather());
}

void set_weather	(const char* weather_name, bool forced)
{
		g_pGamePersistent->Environment().SetWeather(weather_name,forced);
}

bool set_weather_fx	(const char* weather_name)
{
		return		(g_pGamePersistent->Environment().SetWeatherFX(weather_name));
}

bool start_weather_fx_from_time	(const char* weather_name, float time)
{
	return		(g_pGamePersistent->Environment().StartWeatherFXFromTime(weather_name, time));
}

bool is_wfx_playing	()
{
	return			(g_pGamePersistent->Environment().IsWFXPlaying());
}

float get_wfx_time	()
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
	g_SingleGameDifficulty		= dif;
	if (g_pGameLevel)
	{
		game_cl_Single* game		= Game().cast_game_cl_single();
		VERIFY(game);
		game->OnDifficultyChanged	();
	}
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
	game_sv_Single* tpGame = Level().Server->game != nullptr ? Level().Server->game->cast_game_sv_single() : nullptr;
	if(tpGame && ai().get_alife())
	{
		u32 value		= days*86400+hours*3600+mins*60;
		float fValue	= static_cast<float> (value);
		value			*= 1000;//msec		
		g_pGamePersistent->Environment().ChangeGameTime(fValue);
		tpGame->alife().time_manager().change_game_time(value);
	}
}

void set_game_date_time(LPCSTR date, LPCSTR time)
{
	game_sv_Single* tpGame = smart_cast<game_sv_Single*>(Level().Server->game);
	if (tpGame && ai().get_alife())
	{
		u32	years, months, days, hours, minutes, seconds;
		sscanf(time, "%d:%d:%d", &hours, &minutes, &seconds);
		sscanf(date, "%d.%d.%d", &days, &months, &years);
		auto newTime = generate_time(years, months, days, hours, minutes, seconds);
		float fValue = static_cast<float>(days * 86400 + hours * 3600 + minutes * 60);
		g_pGamePersistent->Environment().ChangeGameTime(fValue);
		tpGame->alife().time_manager().set_date_time(newTime);
	}
}

float high_cover_in_direction(u32 level_vertex_id, const Fvector &direction)
{
    if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
        return 0.0f;
    }

	float			y,p;
	direction.getHP	(y,p);
	return			(ai().level_graph().high_cover_in_direction(y,level_vertex_id));
}

float low_cover_in_direction(u32 level_vertex_id, const Fvector &direction)
{
    if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
        return 0.0f;
    }

	float			y,p;
	direction.getHP	(y,p);
	return			(ai().level_graph().low_cover_in_direction(y,level_vertex_id));
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
	direction.mul	(max_distance);
	Fvector			start_position = ai().level_graph().vertex_position(level_vertex_id);
	Fvector			finish_position = Fvector(start_position).add(direction);
	u32				result_ = u32(-1);
	ai().level_graph().farthest_vertex_in_direction(level_vertex_id,start_position,finish_position,result_,0);
	return			(ai().level_graph().valid_vertex_id(result_) ? result_ : level_vertex_id);
}

Fvector vertex_position(u32 level_vertex_id)
{
    if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
        return Fvector{};
    }
	return			(ai().level_graph().vertex_position(level_vertex_id));
}

void map_add_object_spot(ALife::_OBJECT_ID id, const char* spot_type, const char* text)
{
	CMapLocation* ml = Level().MapManager().AddMapLocation(spot_type,id);
	if ( xr_strlen(text) )
	{
		ml->SetHint(text);
	}
}

void map_add_object_spot_ser(ALife::_OBJECT_ID id, const char* spot_type, const char* text)
{
	CMapLocation* ml = Level().MapManager().AddMapLocation(spot_type,id);

	if (text && xr_strlen(text))
			ml->SetHint(text);

	ml->SetSerializable(true);
}

void map_change_spot_hint(ALife::_OBJECT_ID id, const char* spot_type, const char* text)
{
	CMapLocation* ml	= Level().MapManager().GetMapLocation(spot_type, id);
	if(!ml)				return;
	ml->SetHint			(text);
}

void map_remove_object_spot(ALife::_OBJECT_ID id, const char* spot_type)
{
	Level().MapManager().RemoveMapLocation(spot_type, id);
}

bool map_has_object_spot(ALife::_OBJECT_ID id, const char* spot_type)
{
	return Level().MapManager().HasMapLocation(spot_type, id);
}

CMapManager* get_map_manager()
{
	return &Level().MapManager();
}

bool patrol_path_exists(const char* patrol_path)
{
	return		(!!ai().patrol_paths().path(patrol_path,true));
}

const char* get_name()
{
	return		(*Level().name());
}

void prefetch_sound	(const char* name)
{
	Level().PrefetchSound(name);
}


CClientSpawnManager	&get_client_spawn_manager()
{
	return		(Level().client_spawn_manager());
}

void start_stop_menu(CUIDialogWnd* pDialog, bool bDoHideIndicators)
{
	if(pDialog->IsShown())
		pDialog->HideDialog();
	else
		pDialog->ShowDialog(bDoHideIndicators);
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
	if(CurrentGameUI())
	{
		CurrentGameUI()->HideShownDialogs();
		CurrentGameUI()->ShowGameIndicators(false);
		CurrentGameUI()->ShowCrosshair(false);
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, true);
}

void hide_indicators_safe()
{
	if(CurrentGameUI())
	{
		CurrentGameUI()->ShowGameIndicators(false);
		CurrentGameUI()->ShowCrosshair(false);

		CurrentGameUI()->OnExternalHideIndicators();
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, true);
}

void show_indicators()
{
	if(CurrentGameUI())
	{
		CurrentGameUI()->ShowGameIndicators(true);
		CurrentGameUI()->ShowCrosshair(true);
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, false);
}

void show_weapon(bool b)
{
	psHUD_Flags.set	(HUD_WEAPON_RT2, b);
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

void add_call(const luabind::object &lua_object, const char* condition,const char* action)
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

void remove_call(const luabind::object &lua_object, const char* condition,const char* action)
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

cphysics_world_scripted* physics_world_scripted()
{
	return	get_script_wrapper<cphysics_world_scripted>(*physics_world());
}
CEnvironment *environment()
{
	return		(g_pGamePersistent->pEnvironment);
}

CEnvDescriptor *current_environment(CEnvironment *self_)
{
	return		(self_->CurrentEnv);
}

extern bool g_bDisableAllInput;
extern bool g_bDisableMouseMove;

void disable_input()
{
	g_bDisableAllInput = true;
	g_bDisableMouseMove = true;
#ifdef DEBUG
	Msg("input disabled");
#endif // #ifdef DEBUG
}

void enable_input()
{
	g_bDisableAllInput = false;
	g_bDisableMouseMove = false;
#ifdef DEBUG
	Msg("input enabled");
#endif // #ifdef DEBUG
}

void disable_mouse_move()
{
	g_bDisableMouseMove = true;
#ifdef DEBUG
	Msg("mouse move disabled");
#endif // #ifdef DEBUG
}

void enable_mouse_move()
{
	g_bDisableMouseMove = false;
#ifdef DEBUG
	Msg("mouse move enabled");
#endif // #ifdef DEBUG
}

void spawn_phantom(const Fvector &position)
{
	Level().spawn_item("m_phantom", position, u32(-1), ALife::INVALID_OBJECT_ID, false);
}

Fbox get_bounding_volume()
{
	return Level().ObjectSpace.GetBoundingVolume();
}

void iterate_sounds					(const char* prefix, u32 max_count, const CScriptCallbackEx<void> &callback)
{
	for (int j=0, N = _GetItemCount(prefix); j<N; ++j) {
		string_path					fn, s;
		LPSTR						S = (LPSTR)&s;
		_GetItem					(prefix,j,s);
		if (FS.exist(fn,_game_sounds_,S,".ogg"))
			callback				(prefix);

		for (u32 i=0; i<max_count; ++i)
		{
			string_path					name;
			xr_sprintf					(name,"%s%d",S,i);
			if (FS.exist(fn,_game_sounds_,name,".ogg"))
				callback			(name);
		}
	}
}

void iterate_sounds1				(const char* prefix, u32 max_count, luabind::functor<void> functor)
{
	CScriptCallbackEx<void>		temp;
	temp.set					(functor);
	iterate_sounds				(prefix,max_count,temp);
}

void iterate_sounds2				(const char* prefix, u32 max_count, luabind::object object, luabind::functor<void> functor)
{
	CScriptCallbackEx<void>		temp;
	temp.set					(functor,object);
	iterate_sounds				(prefix,max_count,temp);
}

#include "ActorEffector.h"
float add_cam_effector(const char* fn, int id, bool cyclic, const char* cb_func)
{
	CAnimatorCamEffectorScriptCB* e		= new CAnimatorCamEffectorScriptCB(cb_func);
	e->SetType					((ECamEffectorType)id);
	e->SetCyclic				(cyclic);
	e->Start					(fn);
	Actor()->Cameras().AddCamEffector(e);
	return						e->GetAnimatorLength();
}

float add_cam_effector_without_fov(const char* fn, int id, bool cyclic, const char* cb_func)
{
	CAnimatorCamEffectorScriptCB* e = new CAnimatorCamEffectorScriptCB(cb_func);
	e->m_bAbsolutePositioning = true;
	e->SetType((ECamEffectorType)id);
	e->SetCyclic(cyclic);
	e->Start(fn);
	Actor()->Cameras().AddCamEffector(e);
	return						e->GetAnimatorLength();
}

float add_cam_effector2(const char* fn, int id, bool cyclic, const char* cb_func, float cam_fov)
{
	CAnimatorCamEffectorScriptCB* e		= new CAnimatorCamEffectorScriptCB(cb_func);
	e->m_bAbsolutePositioning	= true;
	e->m_fov					= cam_fov;
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

bool is_inventory_volume_enabled()
{
	return CInventoryVolumeSystem::Get().IsEnabled();
}

void set_inventory_volume_enabled(bool enabled)
{
	CInventoryVolumeSystem::Get().SetScriptEnabled(enabled);
}
#include "actor_statistic_mgr.h"
void add_actor_points(const char* sect, const char* detail_key, int cnt, int pts)
{
	return Actor()->StatisticMgr().AddPoints(sect, detail_key, cnt, pts);
}

void add_actor_points_str(const char* sect, const char* detail_key, const char* str_value)
{
	return Actor()->StatisticMgr().AddPoints(sect, detail_key, str_value);
}

int get_actor_points(const char* sect)
{
	return Actor()->StatisticMgr().GetSectionPoints(sect);
}
extern int get_actor_ranking();
extern void add_human_to_top_list(ALife::_OBJECT_ID id);
extern void remove_human_from_top_list(ALife::_OBJECT_ID id);

#include "ActorEffector.h"
void add_complex_effector(const char* section, int id)
{
	AddEffector(Actor(),id, section);
}

void remove_complex_effector(int id)
{
	RemoveEffector(Actor(),id);
}

#include "PostprocessAnimator.h"
void add_pp_effector(const char* fn, int id, bool cyclic)
{
	CPostprocessAnimator* pp		= new CPostprocessAnimator(id, cyclic);
	pp->Load						(fn);
	auto actor = Actor();
	R_ASSERT(actor);
	actor->Cameras().AddPPEffector	(pp);
}

void remove_pp_effector(int id)
{
	if (Actor() == nullptr)
	{
		return;
	}

	CPostprocessAnimator* pp = smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if (pp)
	{
		pp->Stop(1.0f);
	}
}

void set_pp_effector_factor(int id, float f, float f_sp)
{
	if (Actor() == nullptr)
	{
		return;
	}

	CPostprocessAnimator*	pp	= smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if(pp) pp->SetDesiredFactor(f,f_sp);
}

void set_pp_effector_factor2(int id, float f)
{
	if (Actor() == nullptr)
	{
		return;
	}

	CPostprocessAnimator*	pp	= smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if(pp) pp->SetCurrentFactor(f);
}

#include "relation_registry.h"

int g_community_goodwill(const char* _community, ALife::_OBJECT_ID _entity_id)
 {
	 CHARACTER_COMMUNITY c;
	 c.set					(_community);

 	return RELATION_REGISTRY().GetCommunityGoodwill(c.index(), _entity_id);
 }

void g_set_community_goodwill(const char* _community, int _entity_id, int val)
{
	CHARACTER_COMMUNITY	c;
	c.set					(_community);
	RELATION_REGISTRY().SetCommunityGoodwill(c.index(), u16(_entity_id), val);
}

void g_change_community_goodwill(const char* _community, int _entity_id, int val)
{
	CHARACTER_COMMUNITY	c;
	c.set					(_community);
	RELATION_REGISTRY().ChangeCommunityGoodwill(c.index(), u16(_entity_id), val);
}

int g_get_community_relation( const char* comm_from, const char* comm_to )
{
	CHARACTER_COMMUNITY	community_from;
	community_from.set( comm_from );
	CHARACTER_COMMUNITY	community_to;
	community_to.set( comm_to );

	return RELATION_REGISTRY().GetCommunityRelation( community_from.index(), community_to.index() );
}

void g_set_community_relation( const char* comm_from, const char* comm_to, int value )
{
	CHARACTER_COMMUNITY	community_from;
	community_from.set( comm_from );
	CHARACTER_COMMUNITY	community_to;
	community_to.set( comm_to );

	RELATION_REGISTRY().SetCommunityRelation( community_from.index(), community_to.index(), value );
}

int g_get_general_goodwill_between ( ALife::_OBJECT_ID from, ALife::_OBJECT_ID to)
{
	s32 presonal_goodwill		= RELATION_REGISTRY().GetGoodwill(from, to); VERIFY(presonal_goodwill != -type_max(s32));

	CSE_ALifeTraderAbstract* from_obj	= smart_cast<CSE_ALifeTraderAbstract*>(ai().alife().objects().object(from));
	CSE_ALifeTraderAbstract* to_obj		= smart_cast<CSE_ALifeTraderAbstract*>(ai().alife().objects().object(to));

	if (!from_obj||!to_obj){
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"RELATION_REGISTRY::get_general_goodwill_between  : cannot convert obj to CSE_ALifeTraderAbstract!");
		return (0);
	}	
	s32 community_to_obj_goodwill		= RELATION_REGISTRY().GetCommunityGoodwill	(from_obj->Community(), to					);
	s32 community_to_community_goodwill	= RELATION_REGISTRY().GetCommunityRelation	(from_obj->Community(), to_obj->Community()	);
	
	return presonal_goodwill + community_to_obj_goodwill + community_to_community_goodwill;
}

u32 vertex_id	(Fvector position)
{
	return	(ai().level_graph().vertex_id(position));
}

u32 render_get_dx_level()
{
	return ::Render->get_dx_level();
}

CUISequencer* g_tutorial = nullptr;
CUISequencer* g_tutorial2 = nullptr;

void start_tutorial(const char* name)
{
	if (load_screen_renderer.IsActive()) {
		return;
	}

	if(g_tutorial){
		VERIFY				(!g_tutorial2);
		g_tutorial2			= g_tutorial;
	};

	g_tutorial							= new CUISequencer();
	g_tutorial->Start					(name);
	if(g_tutorial2)
		g_tutorial->m_pStoredInputReceiver = g_tutorial2->m_pStoredInputReceiver;

}

void stop_tutorial()
{
	if(g_tutorial)
		g_tutorial->Stop();	
}

const char* tutorial_name()
{
	if (g_tutorial)
		return g_tutorial->m_name;
	return "invalid";
}

const char* translate_string(const char* str)
{
	return *g_pStringTable->ParseStringFromScript(str);
}

const char* current_language()
{
	return g_pStringTable->LangName().c_str();
}

bool has_active_tutotial()
{
	return (g_tutorial!=nullptr);
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

void g_send2(NET_Packet& P, bool bReliable = 0)
{
	Level().Send(P, net_flags(bReliable, 1, 0, 0));
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
void spawn_section(const char* sSection, Fvector3 vPosition, u32 LevelVertexID, ALife::_OBJECT_ID ParentID, bool bReturnItem = false)
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
	CObject* current_entity = Level().CurrentViewEntity();
	if (CActor* actor = current_entity != nullptr ? current_entity->cast_actor() : nullptr)
	{
		return (u8)actor->active_cam();
	}

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

CScriptGameObject* get_view_entity_script()
{
	CObject* current_entity = Level().CurrentViewEntity();
	if (CGameObject* pGameObject = current_entity != nullptr ? current_entity->cast_game_object() : nullptr)
	{
		return pGameObject->lua_game_object();
	}

	return 0;
}

void set_view_entity_script(CScriptGameObject* go)
{
	if (CObject* o = go->object().dcast_CObject())
	{
		Level().SetViewEntity(o);
	}
}

void set_active_cam(u8 mode)
{
	CObject* current_entity = Level().CurrentViewEntity();
	CActor* actor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;
	if (actor != nullptr && mode <= ACTOR_DEFS::EActorCameras::eacMaxCam)
	{
		actor->cam_Set((ACTOR_DEFS::EActorCameras)mode);
	}
}

namespace level_nearest
{
	xr_vector<ISpatialShared> ObjectList;
	void Set(float Radius, const Fvector& Pos)
	{
		g_SpatialSpace->q_sphere(ObjectList, 0, ESPATIAL_TYPE::COLLIDEABLE, Pos, Radius);
	}

	u32 Size()
	{
		return (u32)ObjectList.size();
	}

	CScriptGameObject* Get(int Idx)
	{
		if (Idx > (int)ObjectList.size())
		{
			return 0;
		}
		ISpatialShared& SS = ObjectList[Idx];
		if(!SS.get())
		{
			return 0;
		}
		CObject* O = SS->dcast_CObject();
		if(!O || O->getDestroy() || !O->cast_game_object())
		{
			return 0;
		}

		CGameObject* pObj = O->cast_game_object();
		return pObj->lua_game_object();
	}
}

void patrol_path_add(const char* patrol_path, CPatrolPath* path)
{
	ai().patrol_paths_raw().add_path(shared_str(patrol_path), path);
}

void patrol_path_remove(const char* patrol_path)
{
	ai().patrol_paths_raw().remove_path(shared_str(patrol_path));
}

void ReloadLanguage(const char* lang)
{
	g_pStringTable->ReloadLanguage(lang);
}

void RefreshNames()
{
	if (g_pGameLevel == nullptr)
		return;

	Level().m_upgrade_manager->RefreshTranslations();

	for (auto& [id, pointer] : ai().alife().objects().objects())
	{
		const auto obj = g_pGameLevel->Objects.net_Find(id);
		if (obj != nullptr)
		{
			auto* owner = obj->cast_inventory_item();
			if (owner)
			{
				owner->RefreshTranslations();
				continue;
			}
		}

		auto trader = pointer->cast_trader_abstract();
		if (trader == nullptr)
		{
			continue;
		}

		trader->m_character_name = TranslateName(trader->m_character_name_raw.c_str());

		if (obj != nullptr)
		{
			CInventoryOwner* owner = obj->cast_inventory_owner();
			if (owner)
			{
				owner->RefreshNamesNPC();
			}
		}
	}
}

void launch_sam(CScriptGameObject* launch_object, CScriptGameObject* target)
{
	if (OnClient()) {
		return;
	}
	if (!launch_object)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "launch_sam: launch object is NULL!");
		return;
	}
	auto sam = smart_cast<CSamZone*>(&launch_object->object());
	if (!sam)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "launch_sam: launch object [%s] is not a CSamZone!", launch_object->object().Name());
		return;
	}
	if (!target)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "target_sam: target is NULL!");
		return;
	}
	sam->LaunchMissile(&target->object());
}
		
void switch_wallmark(CScriptGameObject* object, bool isOn)
{
	if (OnClient()) {
		return;
	}
	if (!object)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "switch_wallmark: dynamic wallmark object is NULL!");
		return;
	}
	auto DW = smart_cast<CDynamicWallmarkZone*>(&object->object());
	if (!DW)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "switch_wallmark: dynamic wallmark object [%s] is not a CDynamicWallmarkZone!", object->object().Name());
		return;
	}
	DW->SwitchWallmark(isOn);
}

bool IsUIShown()
{
	return CurrentGameUI()->GameIndicatorsShown();
}

bool IndicatorsShown()
{
	if (!IsUIShown())
	{
		return false;
	}

	CActor* actor = Level().CurrentControlEntity() != nullptr ? Level().CurrentControlEntity()->cast_actor() : nullptr;
	if (actor == nullptr)
	{
		return false;
	}

	PIItem active_item = actor->inventory().ActiveItem();

	if (active_item == nullptr)
	{
		return false;
	}

	CWeapon* wpn = active_item->cast_weapon();
	if (wpn == nullptr)
	{
		return true;
	}
	
	if (wpn->IsUIForceHiding())
	{
		return false;
	}
	else if (wpn->IsUIForceUnhiding())
	{
		return true;
	}
	else if (wpn->IsGrenadeMode())
	{
		return true;
	}

	if (wpn->IsZoomed() && (wpn->get_ScopeStatus() == 1 || (wpn->get_ScopeStatus() == 2 && wpn->IsScopeAttached())))
	{
		return false;
	}

	return true;
}

bool InventoryShown()
{
	if (!CurrentGameUI())
	{
		return false;
	}

	return !!CurrentGameUI()->GetActiveInventoryWindow();
}

bool ElectronicsBreak()
{
	if (CElectronicsProblemsManager* electronics_manager = Level().GetElectronicsProblemsManager())
	{
		return electronics_manager->ElectronicsProblemsInc();
	}

	return false;
}

bool IsPickupMode()
{
	if (CActor* actor = Level().CurrentControlEntity() != nullptr ? Level().CurrentControlEntity()->cast_actor() : nullptr)
	{
		return actor->GetPickupManager()->GetPickupMode();
	}

	return false;
}

bool IsActorBurned()
{
	if (CActor* actor = Level().CurrentControlEntity() != nullptr ? Level().CurrentControlEntity()->cast_actor() : nullptr)
	{
		return actor->IsActorBurning();
	}

	return false;
}

bool IsElectronicsRestore()
{
	if (CElectronicsProblemsManager* electronics_manager = Level().GetElectronicsProblemsManager())
	{
		return electronics_manager->ElectronicsProblemsDec();
	}

	return false;
}

bool ElectronicsReset()
{
	if (CElectronicsProblemsManager* electronics_manager = Level().GetElectronicsProblemsManager())
	{
		electronics_manager->ResetElectronicsProblems();
		return true;
	}

	return false;
}

bool IsElectronicsApply()
{
	if (CElectronicsProblemsManager* electronics_manager = Level().GetElectronicsProblemsManager())
	{
		return electronics_manager->ElectronicsProblemsImmediateApply();
	}

	return false;
}

int GetParameterUpgradedInt()
{
	return 0;
}

int ValidSavedGameInt(int number, const char* name)
{
	return 1;
}

bool IsTacticalHud()
{
	if (CActor* actor = Level().CurrentControlEntity() != nullptr ? Level().CurrentControlEntity()->cast_actor() : nullptr)
	{
		if (CHelmet* helmet = actor->GetHelmet())
		{
			return helmet->m_fShowNearestEnemiesDistance > 0.0f;
		}
	}

	return false;
}

CScriptGameObject* get_object_by_client(u32 clientID)
{
	xrClientData* xrCData = Level().Server->ID_to_client(clientID);
	if (!xrCData || !xrCData->owner)
	{
		return 0;
	}

	CObject* net_finded = Level().Objects.net_Find(xrCData->owner->ID);

	CGameObject* pGameObject = net_finded != nullptr ? net_finded->cast_game_object() : nullptr;
	if (!pGameObject)
	{
		return 0;
	}

	return pGameObject->lua_game_object();
}

ALife::_OBJECT_ID get_local_player_id()
{
	return Game().local_player->GameID;
}

int get_g_actor_id()
{
	if (!Actor())
		return -1;

	return Actor()->ID();
}

void send_script_event_to_client(u32 cleintId, NET_Packet& P)
{
	R_ASSERT2(OnServer(), "Avaliable only on server");
	Level().Server->SendTo(ClientID(cleintId), P, net_flags(true, true));
}

void send_script_event_broadcast(NET_Packet& P)
{
	R_ASSERT2(OnServer(), "Avaliable only on server");
	Level().Server->SendBroadcast(BroadcastCID, P, net_flags(true, true));
}

ScriptEvent* get_front_server_event()
{
	return Level().Server->GetFrontServerScriptEvent();
}
void pop_front_server_event()
{
	Level().Server->PopFrontServerScriptEvent();
}

ScriptEvent* get_last_server_event()
{
	return Level().Server->GetLastServerScriptEvent();
}

void pop_last_server_event()
{
	Level().Server->PopLastServerScriptEvent();
}

u32 get_size_server_events()
{
	return Level().Server->GetSizeServerScriptEvent();
}

void send_script_event_to_server(NET_Packet& P)
{
	Level().Send(P, net_flags(true, true));
}

NET_Packet* get_last_client_event()
{
	return Level().GetLastClientScriptEvent();
}

void pop_last_client_event()
{
	Level().PopLastClientScriptEvent();
}

u32 get_size_client_events()
{
	return Level().GetSizeClientScriptEvent();
}

u32 get_build_id()
{
	return Core.BuildId;
}

extern ENGINE_API float psHUD_FOV;

Fvector2 World2Ui(Fvector pos, bool hud)
{
	Fmatrix world = {}, res = {};
	world.identity();
	world.c = pos;

	if (hud)
	{
		Fmatrix fp ={};
		Fmatrix ft = {};
		Fmatrix fv = {};
		fv.build_camera_dir(Device.vCameraPosition, Device.vCameraDirection, Device.vCameraTop);
		fp.build_projection(
			deg2rad(psHUD_FOV * Device.fFOV),
			Device.fASPECT, RDEVICE.fViewportNear,
			g_pGamePersistent->Environment().CurrentEnv->far_plane);

		ft.mul(fp, fv);
		res.mul(ft, world);
	}
	else
	{
		res.mul(Device.mFullTransform, world);
	}

	Fvector4 vRes = {};
	vRes.w = res._44;
	vRes.x = res._41 / vRes.w;
	vRes.y = res._42 / vRes.w;
	vRes.z = res._43 / vRes.w;

	if (vRes.z < 0 || vRes.w < 0) return { -9999,0 };
	if (abs(vRes.x) > 1.f || abs(vRes.y) > 1.f) return { -9999,0 };

	float x = (1.f + vRes.x) / 2.f * Device.TargetWidth;
	float y = (1.f - vRes.y) / 2.f * Device.TargetHeight;

	float widthFk = Device.TargetWidth / UI_BASE_WIDTH;
	float heightFk = Device.TargetHeight / UI_BASE_HEIGHT;

	x /= widthFk;
	y /= heightFk;

	return { x, y };
}

void jump_level(const Fvector& m_position, u32 m_level_vertex_id, GameGraph::_GRAPH_ID m_game_vertex_id, const Fvector& m_angles)
{
	NET_Packet p;
	p.w_begin(M_CHANGE_LEVEL);
	p.w(&m_game_vertex_id, sizeof(m_game_vertex_id));
	p.w(&m_level_vertex_id, sizeof(m_level_vertex_id));
	p.w_vec3(m_position);
	p.w_vec3(m_angles);
	Level().Send(p, net_flags(true));
}

void CallGameOver()
{
	CurrentGameUI()->HideShownDialogs();
	start_tutorial("game_over");
}

float get_fog_distance()
{
	auto CurrEnv = g_pGamePersistent->Environment().CurrentEnv;
	if (CurrEnv != nullptr)
	{
		return CurrEnv->fog_distance;
	}

	return 400.0f;
}

void spawn_anomaly(const char* str, int level_vertex_id, const Fvector& position, float rad)
{
	VERIFY(!physics_world()->Processing());
	string128 tmp;
	VERIFY3(3 == _GetItemCount(str), "Bad record format in artefact_spawn_zones", str);
	float zone_radius = (float)atof(_GetItem(str, 1, tmp));
	const char* zone_sect = _GetItem(str, 0, tmp);

	CSE_Abstract* object = Level().spawn_item(zone_sect,
		position,
		level_vertex_id,
		ALife::INVALID_OBJECT_ID,
		true
	);
	CSE_ALifeAnomalousZone* AlifeZone = smart_cast<CSE_ALifeAnomalousZone*>(object);
	VERIFY(AlifeZone);
	CShapeData::shape_def		_shape;
	_shape.data.sphere.P.set(0.0f, 0.0f, 0.0f);
	_shape.data.sphere.R = rad;
	_shape.type = CShapeData::cfSphere;
	AlifeZone->assign_shapes(&_shape, 1);
	AlifeZone->m_owner_id = 0;
	AlifeZone->m_space_restrictor_type = RestrictionSpace::eRestrictorTypeNone;

	NET_Packet					P;
	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		SaveObjectNetPacketHelper::PrepareLocalSpawnPacket(P, *object);
	}
	else
	{
		object->Spawn_Write(P, true);
	}
	Level().Send(P, net_flags(true));
	F_entity_Destroy(object);
}

void set_time_factor_single(float value) // FNAS
{
	Level().SetGameTimeFactor(value);
}

const char* GetActorMaterialPairName()
{
	u16 mtl_idx = Actor() ? Actor()->material().last_material_idx() : GAMEMTL_NONE_IDX;
	if (mtl_idx != GAMEMTL_NONE_IDX)
	{
		SGameMtl* mtl = GMLib.GetMaterialByIdx(mtl_idx);
		return (mtl) ? mtl->m_Name.c_str() : "";
	}
	else
	{
		return "";
	}
}


std::unordered_map<shared_str, xr_vector<shared_str>> m_named_stash;

void SetNamedStashStringVector(const char* name, luabind::object const& table)
{
	VERIFY(table.type() == LUA_TTABLE);

	xr_vector<shared_str>& runtime_vector = m_named_stash[name];
	runtime_vector.clear();

	luabind::object::iterator I = table.begin();
	luabind::object::iterator E = table.end();

	for (; I != E; ++I)
	{
		luabind::object t_value = *I;
		if (t_value.type() != LUA_TSTRING)
		{
			VERIFY(t_value.type() != LUA_TNIL);
			continue;
		}

		shared_str t_strng_value = luabind::object_cast<const char*>(t_value);
		runtime_vector.push_back(t_strng_value);
	}
}

bool IsExistsNamedStashStringVector(const char* name)
{
	return m_named_stash.find(name) != m_named_stash.end();
}

luabind::object GetNamedStashStringVector(const char* name)
{
	luabind::object vector_to_lua = luabind::newtable(ai().script_engine().lua());

	if (IsExistsNamedStashStringVector(name))
	{
		xr_vector<shared_str>& runtime_vector = m_named_stash[name];
		size_t cnt = runtime_vector.size();

		for (size_t i = 0; i < cnt; i++)
		{
			string128 tmp;
			vector_to_lua[i + 1] = runtime_vector[i].c_str();
		}
	}

	return vector_to_lua;
}

void RemoveNamedStashStringVector(const char* name)
{
	if (IsExistsNamedStashStringVector(name))
	{
		xr_vector<shared_str>& runtime_vector = m_named_stash[name];
		runtime_vector.clear();
	}
}

void RemoveAllNamedStashStringVectors()
{
	m_named_stash.clear();
}

const xr_vector<CScriptGameObject*>& GetOnlineGameObjectsBySphereSpatial(const Fvector& _center, const float _radius, luabind::object const& eSpatialTypes)
{
	u64 mask = 0;

	VERIFY(eSpatialTypes.type() == LUA_TTABLE);

	luabind::object::iterator I = eSpatialTypes.begin();
	luabind::object::iterator E = eSpatialTypes.end();

	for (; I != E; ++I)
	{
		luabind::object  tValue = *I;
		if (tValue.type() == LUA_TNIL || tValue.type() != LUA_TNUMBER)
		{
			continue;
		}

		if (mask == 0)
		{
			mask = luabind::object_cast<u64>(tValue);
		}
		else
		{
			mask |= luabind::object_cast<u64>(tValue);
		}
	}

	static xr_vector<CScriptGameObject*> m_objects;
	m_objects.clear();

	static xr_vector<ISpatialShared> R;
	R.clear();
	R.reserve(64);

	g_SpatialSpace->q_sphere(R, 0, ESPATIAL_TYPE(mask), _center, _radius);

	m_objects.reserve(R.size());

	for (ISpatialShared& spatial : R)
	{
		if (!spatial.get())
		{
			continue;
		}

		if (CObject* obj = spatial->dcast_CObject())
		{
			if (obj->getDestroy() || !obj->cast_game_object())
			{
				continue;
			}

			if (CScriptGameObject* luaobj = obj->cast_game_object()->lua_game_object())
			{
				m_objects.push_back(luaobj);
			}
		}
	}

	return m_objects;
}

const xr_vector<CScriptGameObject*>& GetOnlineGameObjectsByObbBoxSpatial(const Fvector& _center, const Fvector box_halfsize, const Fvector box_direction, luabind::object const& eSpatialTypes)
{
	u64 mask = 0;

	VERIFY(eSpatialTypes.type() == LUA_TTABLE);

	luabind::object::iterator I = eSpatialTypes.begin();
	luabind::object::iterator E = eSpatialTypes.end();

	for (; I != E; ++I)
	{
		luabind::object  tValue = *I;
		if (tValue.type() == LUA_TNIL || tValue.type() != LUA_TNUMBER)
		{
			continue;
		}

		if (mask == 0)
		{
			mask = luabind::object_cast<u64>(tValue);
		}
		else 
		{
			mask |= luabind::object_cast<u64>(tValue);
		}
	}

	static xr_vector<CScriptGameObject*> m_objects;
	m_objects.clear();

	static xr_vector<ISpatialShared> R;
	R.clear();
	R.reserve(64);
	
	Fobb obb;
	obb.identity();
	obb.m_translate = _center;
	obb.m_halfsize = box_halfsize;
	obb.m_rotate.k = box_direction;
	Fvector::generate_orthonormal_basis_normalized(obb.m_rotate.k, obb.m_rotate.j, obb.m_rotate.i);

	g_SpatialSpace->q_obb(R, 0, ESPATIAL_TYPE(mask), obb);
	

	m_objects.reserve(R.size());

	for(ISpatialShared& spatial :R)
	{
		if (!spatial.get())
		{
			continue;
		}

		if (CObject* obj = spatial->dcast_CObject())
		{
			if (obj->getDestroy() || !obj->cast_game_object())
			{
				continue;
			}

			if (CScriptGameObject* luaobj = obj->cast_game_object()->lua_game_object())
			{
				m_objects.push_back(luaobj);
			}
		}
	}

	return m_objects;
}

void ws_element_set_text(u16 id, const char* section, const char* text)
{
	if (CGameObject* obj = smart_cast<CGameObject*>(Level().Objects.net_Find(id)))
	{
		Level().WorldSpaceUIManager->ElementSetText(obj, section, text);
	}
}

void ws_element_show(u16 id, const char* section, bool show = true)
{
	if (CGameObject* obj = smart_cast<CGameObject*>(Level().Objects.net_Find(id)))
	{
		Level().WorldSpaceUIManager->ElementShow(obj, section, show);
	}
}

#pragma optimize("s",on)
void CLevel::script_register(lua_State *L)
{
	class_<CEnvDescriptor>("CEnvDescriptor")
		.def_readonly("fog_density",			&CEnvDescriptor::fog_density)
		.def_readonly("far_plane",				&CEnvDescriptor::far_plane),

	class_<CEnvironment>("CEnvironment")
		.def("current",							current_environment);

	module(L)
	[
		class_<CFFxRandom>("FFxRandom")
			.def(constructor<>())
			.def(constructor<u32, u32>())
			.def("is_counter_valid", &CFFxRandom::is_counter_valid)
			.def("get_seed", &CFFxRandom::get_seed)
			.def("get_counter", &CFFxRandom::get_counter)
			.def("set_state", &CFFxRandom::set_state)
			.def("next_int", &CFFxRandom::next_int)
			.def("next_int_range", &CFFxRandom::next_int_range)
			.def("next_float", &CFFxRandom::next_float)
			.def("next_float_range", &CFFxRandom::next_float_range)
			.def("next_bool", &CFFxRandom::next_bool)
			.def("next_bool_probability", &CFFxRandom::next_bool_probability)
	];

	module(L,"level")
	[
		class_<enum_exporter<ESPATIAL_TYPE>>("e_spatial_type")
			.enum_("e_spatial_types")
			[
				value("NONE", u64(ESPATIAL_TYPE::NONE)),
				value("INVALIDSECTOR", u64(ESPATIAL_TYPE::INVALIDSECTOR)),
				value("RENDERABLE", u64(ESPATIAL_TYPE::RENDERABLE)),
				value("LIGHTSOURCE", u64(ESPATIAL_TYPE::LIGHTSOURCE)),
				value("LIGHTSOURCEHEMI", u64(ESPATIAL_TYPE::LIGHTSOURCEHEMI)),
				value("PHYSIC", u64(ESPATIAL_TYPE::PHYSIC)),
				value("SHAPE", u64(ESPATIAL_TYPE::SHAPE)),
				value("PARTICLE", u64(ESPATIAL_TYPE::PARTICLE)),
				value("COLLIDEABLE", u64(ESPATIAL_TYPE::COLLIDEABLE)),
				value("VISIBLEFORAI", u64(ESPATIAL_TYPE::VISIBLEFORAI)),
				value("REACTTOSOUND", u64(ESPATIAL_TYPE::REACTTOSOUND)),
				value("OBSTACLE", u64(ESPATIAL_TYPE::OBSTACLE)),
				value("RENDERABLESHADOW", u64(ESPATIAL_TYPE::RENDERABLESHADOW)),
				value("LADDER", u64(ESPATIAL_TYPE::LADDER)),
				value("ACTOR", u64(ESPATIAL_TYPE::ACTOR)),
				value("ACTOR_DEAD", u64(ESPATIAL_TYPE::ACTOR_DEAD)),
				value("ACTOR_ALIVE", u64(ESPATIAL_TYPE::ACTOR_ALIVE)),
				value("AI", u64(ESPATIAL_TYPE::AI)),
				value("AI_DEAD", u64(ESPATIAL_TYPE::AI_DEAD)),
				value("AI_ALIVE", u64(ESPATIAL_TYPE::AI_ALIVE)),
				value("STALKER", u64(ESPATIAL_TYPE::STALKER)),
				value("STALKER_WOUNDED", u64(ESPATIAL_TYPE::STALKER_WOUNDED)),
				value("STALKER_DEAD", u64(ESPATIAL_TYPE::STALKER_DEAD)),
				value("STALKER_ALIVE", u64(ESPATIAL_TYPE::STALKER_ALIVE)),
				value("MONSTER", u64(ESPATIAL_TYPE::MONSTER)),
				value("MONSTER_DEAD", u64(ESPATIAL_TYPE::MONSTER_DEAD)),
				value("MONSTER_ALIVE", u64(ESPATIAL_TYPE::MONSTER_ALIVE)),
				value("CROW", u64(ESPATIAL_TYPE::CROW)),
				value("CROW_DEAD", u64(ESPATIAL_TYPE::CROW_DEAD)),
				value("CROW_ALIVE", u64(ESPATIAL_TYPE::CROW_ALIVE)),
				value("ITEM", u64(ESPATIAL_TYPE::ITEM)),
				value("WEAPON", u64(ESPATIAL_TYPE::WEAPON)),
				value("MISSILE", u64(ESPATIAL_TYPE::MISSILE)),
				value("ROCKET", u64(ESPATIAL_TYPE::ROCKET)),
				value("ARTEFACT", u64(ESPATIAL_TYPE::ARTEFACT)),
				value("ANOMALY_DETECTOR", u64(ESPATIAL_TYPE::ANOMALY_DETECTOR)),
				value("CAR", u64(ESPATIAL_TYPE::CAR)),
				value("HELI", u64(ESPATIAL_TYPE::HELI)),
				value("PHYSIC_OBJECT", u64(ESPATIAL_TYPE::PHYSIC_OBJECT)),
				value("PHYSIC_SHELL_HOLDER", u64(ESPATIAL_TYPE::PHYSIC_SHELL_HOLDER)),
				value("PHYSIC_OBJECT_DESTR", u64(ESPATIAL_TYPE::PHYSIC_OBJECT_DESTR)),
				value("PHYSIC_OBJECT_BRKBL", u64(ESPATIAL_TYPE::PHYSIC_OBJECT_BRKBL)),
				value("PHYSIC_MOVEMENT", u64(ESPATIAL_TYPE::PHYSIC_MOVEMENT)),
				value("INV_BOX", u64(ESPATIAL_TYPE::INV_BOX)),
				value("AI_DOOR", u64(ESPATIAL_TYPE::AI_DOOR)),
				value("LIGHT_LAMP", u64(ESPATIAL_TYPE::LIGHT_LAMP)),
				value("LEVEL_CHANGER", u64(ESPATIAL_TYPE::LEVEL_CHANGER)),
				value("SPACE_RESTRICTOR", u64(ESPATIAL_TYPE::SPACE_RESTRICTOR)),
				value("ANOMALY_ZONE", u64(ESPATIAL_TYPE::ANOMALY_ZONE)),
				value("SIM_FACTION", u64(ESPATIAL_TYPE::SIM_FACTION)),
				value("SMART_TERRAIN", u64(ESPATIAL_TYPE::SMART_TERRAIN)),
				value("CAMP_ZONE", u64(ESPATIAL_TYPE::CAMP_ZONE)),
				value("SMART_COVER", u64(ESPATIAL_TYPE::SMART_COVER)),
				value("ANOMAL_ZONE_LOGIC", u64(ESPATIAL_TYPE::ANOMAL_ZONE_LOGIC))
			],

		// obsolete\deprecated
		def("object_by_id",						get_object_by_id),
#ifdef DEBUG
		def("debug_object",						get_object_by_name),
		def("debug_actor",						tpfGetActor),
		def("check_object",						check_object),
#endif
		def("set_time_factor_single", set_time_factor_single), // FNAS
		
		def("search_online_objects_by_sphere", &GetOnlineGameObjectsBySphereSpatial, return_stl_iterator),
		def("search_online_objects_by_obb_box", &GetOnlineGameObjectsByObbBoxSpatial, return_stl_iterator),

		def("is_exists_named_stash_string_vector", &IsExistsNamedStashStringVector),
		def("get_named_stash_string_vector", &GetNamedStashStringVector),
		def("set_named_stash_string_vector", &SetNamedStashStringVector),
		def("remove_named_stash_string_vector", &RemoveNamedStashStringVector),
		def("remove_all_named_stash_string_vectors", &RemoveAllNamedStashStringVectors),

		def("get_weather",						get_weather),
		def("set_weather",						set_weather),
		def("set_weather_fx",					set_weather_fx),
		def("set_past_weather", set_past_wdesc),
		def("set_next_weather", set_next_wdesc),
		def("get_weather_game_time", get_weather_game_time),
		def("get_past_wdesc_execution_time", get_past_wdesc_execution_time),
		def("get_next_wdesc_execution_time", get_next_wdesc_execution_time),
		def("get_past_weather", get_past_wdesc),
		def("get_next_weather", get_next_wdesc),
		def("enable_rain", +[](bool Value) 
			{
				g_pGamePersistent->Environment().eff_Rain->Enable(false);
				g_pGamePersistent->Environment().eff_Thunderbolt->Enable(false);
			}
		),

		def("get_fog_distance",					get_fog_distance),
		def("GetActorMaterialPairName",			GetActorMaterialPairName),
		def("CallGameOver",						CallGameOver),

		def("start_weather_fx_from_time",		start_weather_fx_from_time),
		def("is_wfx_playing",					is_wfx_playing),
		def("get_wfx_time",						get_wfx_time),
		def("stop_weather_fx",					stop_weather_fx),

		def("environment",						environment),
		
		def("set_time_factor",					set_time_factor),
		def("get_time_factor",					get_time_factor),

		def("set_global_time_factor", &set_global_time_factor),
		def("get_global_time_factor", &get_global_time_factor),

		def("set_game_difficulty",				set_game_difficulty),
		def("get_game_difficulty",				get_game_difficulty),
		
		def("get_time_days",					get_time_days),
		def("get_time_hours",					get_time_hours),
		def("get_time_minutes",					get_time_minutes),
		def("change_game_time",					change_game_time),
		def("set_game_date_time", set_game_date_time), // runtime set new date with time

		def("high_cover_in_direction",			high_cover_in_direction),
		def("low_cover_in_direction",			low_cover_in_direction),
		def("vertex_in_direction",				vertex_in_direction),
		def("rain_factor",						rain_factor),
		def("patrol_path_exists",				patrol_path_exists),
		def("vertex_position",					vertex_position),
		def("name",								get_name),
		def("prefetch_sound",					prefetch_sound),

		def("client_spawn_manager",				get_client_spawn_manager),

		def("map_add_object_spot_ser",			map_add_object_spot_ser),
		def("map_add_object_spot",				map_add_object_spot),
//-		def("map_add_object_spot_complex",		map_add_object_spot_complex),
		def("map_remove_object_spot",			map_remove_object_spot),
		def("map_has_object_spot",				map_has_object_spot),
		def("map_change_spot_hint",				map_change_spot_hint),
		def("map_manager",						get_map_manager),

		def("start_stop_menu", start_stop_menu),
		def("add_dialog_to_render",				add_dialog_to_render),
		def("remove_dialog_to_render",			remove_dialog_to_render),
		def("main_input_receiver",				main_input_receiver), // for compatibility
		def("hide_indicators",					hide_indicators),
		def("hide_indicators_safe",				hide_indicators_safe),

		def("show_indicators",					show_indicators),
		def("show_weapon",						show_weapon),
		def("add_call",							((void (*) (const luabind::functor<bool> &,const luabind::functor<void> &)) &add_call)),
		def("add_call",							((void (*) (const luabind::object &,const luabind::functor<bool> &,const luabind::functor<void> &)) &add_call)),
		def("add_call",							((void (*) (const luabind::object &, const char*, const char*)) &add_call)),
		def("remove_call",						((void (*) (const luabind::functor<bool> &,const luabind::functor<void> &)) &remove_call)),
		def("remove_call",						((void (*) (const luabind::object &,const luabind::functor<bool> &,const luabind::functor<void> &)) &remove_call)),
		def("remove_call",						((void (*) (const luabind::object &, const char*, const char*)) &remove_call)),
		def("remove_calls_for_object",			remove_calls_for_object),
		def("present",							is_level_present),
		def("disable_input",					disable_input),
		def("enable_input",						enable_input),
		def("disable_mouse_move",				disable_mouse_move),
		def("enable_mouse_move",				enable_mouse_move),
		def("spawn_phantom",					spawn_phantom),

		def("get_bounding_volume",				get_bounding_volume),

		def("iterate_sounds",					&iterate_sounds1),
		def("iterate_sounds",					&iterate_sounds2),
		def("physics_world",					&physics_world_scripted),
		def("get_snd_volume",					&get_snd_volume),
		def("set_snd_volume",					&set_snd_volume),
		def("is_inventory_volume_enabled",		&is_inventory_volume_enabled),
		def("set_inventory_volume_enabled",		&set_inventory_volume_enabled),
		def("add_cam_effector",					&add_cam_effector),
		def("add_cam_effector2",				&add_cam_effector2),
		def("add_cam_effector2",				&add_cam_effector_without_fov),
		def("remove_cam_effector",				&remove_cam_effector),
		def("add_pp_effector",					&add_pp_effector),
		def("set_pp_effector_factor",			&set_pp_effector_factor),
		def("set_pp_effector_factor",			&set_pp_effector_factor2),
		def("remove_pp_effector",				&remove_pp_effector),
		def("get_compass_direction",			&get_compass_direction),

		def("add_complex_effector",				&add_complex_effector),
		def("remove_complex_effector",			&remove_complex_effector),
		
		def("valid_vertex_id", valid_vertex_id),
		def("is_accessible_vertex_id", is_accessible_vertex_id),
		def("disable_vertex", disable_vertex),
		def("enable_vertex", enable_vertex),
		def("vertex_id",						&vertex_id),

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
		def("send", &g_send2), //allow the ability to send netpacket to level
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
		def("spawn_anomaly", &spawn_anomaly),
		def("valid_vertex", &valid_vertex),
		def("is_ui_shown", &IsUIShown),
		def("is_actor_burned", &IsActorBurned),
		def("indicators_shown", &IndicatorsShown),
		def("inventory_shown", &InventoryShown),
		def("pickup_mode", &IsPickupMode),
		// TODO Guns: Drombeys to all: not impl
		def("electronics_break", &ElectronicsBreak),
		def("electronics_restore", &IsElectronicsRestore),
		def("electronics_reset", &ElectronicsReset),
		def("electronics_apply", &IsElectronicsApply),
		def("get_parameter_upgraded_int", &GetParameterUpgradedInt),
		def("valid_saved_game_int", &ValidSavedGameInt),
		def("is_tactical_hud", &IsTacticalHud),

		// launch SAM
		def("launch_sam", &launch_sam),
		
		// Dynamic wallmarks switch
		def("switch_wallmark", &switch_wallmark),

		// new for fmp
		def("get_object_by_client", &get_object_by_client),
		def("get_local_player_id", &get_local_player_id),
		def("get_g_actor_id", &get_g_actor_id),

		def("ws_element_set_text", &ws_element_set_text),
		def("ws_element_show", &ws_element_show)
	],
	
	module(L,"nearest")
	[
		def("set",						&level_nearest::Set),
		def("size",						&level_nearest::Size),
		def("get",						&level_nearest::Get)
	];

	module(L, "animslot")
	[
		def("play_cutscene", &CCutsceneManager::PlayCutscene),
		def("stop_current_cutscene", &CCutsceneManager::FinishCurrentCutscene)
	];

	module(L)
		[
			class_<SCutsceneObjectElement>("SCutsceneObjectElement")
				.def("set_all_bones_visibility", &SCutsceneObjectElement::SetAllBonesVisibility)
				.def("set_bone_visibility", &SCutsceneObjectElement::SetBoneVisibility)
				.def("set_parent", &SCutsceneObjectElement::SetParent)
				.def("set_anim_to_play", &SCutsceneObjectElement::SetAnimToPlay)
				.def("set_on_finish_func", &SCutsceneObjectElement::SetOnFinishFunc)
				.def("get_bone_id", &SCutsceneObjectElement::GetBoneID)
				.def("set_bones_weapon", &SCutsceneObjectElement::SetBonesWeapon),
			class_<CCutsceneItem>("CCutsceneItem")
				.def("create_object_element", &CCutsceneItem::CreateObjectElement)
				.def("set_pivot_object", &CCutsceneItem::SetPivotObject)
		];

	module(L, "player_hud")
	[
		def("show_legs", &show_legs)
	];

	module(L, "actor_stats")
	[
		def("add_points", &add_actor_points),
		def("add_points_str", &add_actor_points_str),
		def("get_points", &get_actor_points),
		def("add_to_ranking", &add_human_to_top_list),
		def("remove_from_ranking", &remove_human_from_top_list),
		def("get_actor_ranking", &get_actor_ranking)
	];

	module(L)
	[
	   class_<CRayPick>("ray_pick")
	   .def(								constructor<>())
	   .def(								constructor<Fvector&, Fvector&, float, collide::rq_target, CScriptGameObject*>())
	   .def("set_position",					&CRayPick::set_position)
	   .def("set_direction",				&CRayPick::set_direction)
	   .def("set_range",					&CRayPick::set_range)
	   .def("set_flags",					&CRayPick::set_flags)
	   .def("set_ignore_object",			&CRayPick::set_ignore_object)
	   .def("query",						&CRayPick::query)
	   .def("get_result",					&CRayPick::get_result)
	   .def("get_object",					&CRayPick::get_object)
	   .def("get_distance",					&CRayPick::get_distance)
	   .def("get_element",					&CRayPick::get_element)	
	   .def("get_material",					&CRayPick::get_material),
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
		def("command_line",						&command_line),
		def("IsGameTypeSingle",					&IsGameTypeSingle),
		def("IsDynamicMusic",					&IsDynamicMusic),
		def("render_get_dx_level",				&render_get_dx_level),
		def("IsImportantSave",					&IsImportantSave),
		def("IsDedicated",						&is_dedicated),
		def("OnClient",							&OnClient),
		def("OnServer",							&OnServer),
		def("EngineBuildId",					&get_build_id),
		def("action_repeaters",					&ActionRepeaters)
	];

	module(L,"relation_registry")
	[
		def("community_goodwill",				&g_community_goodwill),
		def("set_community_goodwill",			&g_set_community_goodwill),
		def("change_community_goodwill",		&g_change_community_goodwill),
		
		def("community_relation",				&g_get_community_relation),
		def("set_community_relation",			&g_set_community_relation),
		def("get_general_goodwill_between",		&g_get_general_goodwill_between)
	];
	
	module(L, "script_events")
	[
		def("send_to_server", &send_script_event_to_server),
		def("send_to_client", &send_script_event_to_client),
		def("send_broadcast", &send_script_event_broadcast),

		def("get_last_client_event", &get_last_client_event),
		def("pop_last_client_event", &pop_last_client_event),
		def("get_size_client_events", &get_size_client_events),

		def("get_last_server_event", &get_last_server_event),
		def("pop_last_server_event", &pop_last_server_event),
		def("get_front_server_event", &get_front_server_event),
		def("pop_front_server_event", &pop_front_server_event),
		def("get_size_server_events", &get_size_server_events)
	];

	module(L,"game")
	[
		class_< xrTime >("CTime")
			.enum_("date_format")
			[
				value("DateToDay",		int(InventoryUtilities::edpDateToDay)),
				value("DateToMonth",	int(InventoryUtilities::edpDateToMonth)),
				value("DateToYear",		int(InventoryUtilities::edpDateToYear))
			]
			.enum_("time_format")
			[
				value("TimeToHours",	int(InventoryUtilities::etpTimeToHours)),
				value("TimeToMinutes",	int(InventoryUtilities::etpTimeToMinutes)),
				value("TimeToSeconds",	int(InventoryUtilities::etpTimeToSeconds)),
				value("TimeToMilisecs",	int(InventoryUtilities::etpTimeToMilisecs))
			]
			.def(						constructor<>()				)
			.def(						constructor<const xrTime&>())
			.def(const_self <			xrTime()					)
			.def(const_self <=			xrTime()					)
			.def(const_self >			xrTime()					)
			.def(const_self >=			xrTime()					)
			.def(const_self ==			xrTime()					)
			.def(self +					xrTime()					)
			.def(self -					xrTime()					)

			.def("diffSec"				,&xrTime::diffSec_script)
			.def("add"					,&xrTime::add_script)
			.def("sub"					,&xrTime::sub_script)

			.def("save"					,&xrTime::Save)
			.def("load"					,&xrTime::Load)

			.def("setHMS"				,&xrTime::setHMS)
			.def("setHMSms"				,&xrTime::setHMSms)
			.def("set"					,&xrTime::set)
			.def("get"					,&xrTime::get, out_value<2>() + out_value<3>() + out_value<4>() + out_value<5>() + out_value<6>() + out_value<7>() + out_value<8>())
			.def("dateToString"			,&xrTime::dateToString)
			.def("timeToString"			,&xrTime::timeToString)
			.def("Serialize", &ctime_serialize),
			// declarations
			def("time",					get_time),
			def("get_game_time",		get_time_struct),
//			def("get_surge_time",	Game::get_surge_time),
//			def("get_object_by_name",Game::get_object_by_name),
		
			def("start_tutorial",		&start_tutorial),
			def("stop_tutorial",		&stop_tutorial),
			def("has_active_tutorial",	&has_active_tutotial),
			def("active_tutorial_name", &tutorial_name),
			def("translate_string",		&translate_string),
			def("current_language",		&current_language),
			def("reload_language", &ReloadLanguage),
			def("world2ui", &World2Ui),
			def("jump_level", &jump_level)
	];
}
