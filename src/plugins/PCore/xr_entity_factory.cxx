#include <algorithm>
#include "xr_entity_factory.h"
#include "xr_entity_script.h"
#include "xr_entity_zenobian.h"
#include "xr_file_system.h"
#include "xr_ini_file.h"
#include "xr_utils.h"
#include "xr_entity_la.h"

using namespace xray_re;

class factory_item_base {
public:
				factory_item_base(const char* clsid);
	virtual			~factory_item_base();
	virtual cse_abstract*	create() = 0;
	const xr_clsid&		clsid() const;
protected:
	xr_clsid		m_clsid;
};

inline factory_item_base::factory_item_base(const char* clsid): m_clsid(clsid) {}
inline factory_item_base::~factory_item_base() {}

inline const xr_clsid& factory_item_base::clsid() const { return m_clsid; }

template<class T> class factory_item: public factory_item_base {
public:
				factory_item(const char* clsid);
	virtual cse_abstract*	create();
};

template<class T> inline factory_item<T>::factory_item(const char* clsid):
	factory_item_base(clsid) {}

template<class T> cse_abstract* factory_item<T>::create()
{
	T* p = new T;
	p->clsid() = clsid();
	return p;
}

class xr_entity_factory {
public:
			xr_entity_factory();
			~xr_entity_factory();
	cse_abstract*	create(const char* name);
	xr_clsid*		get_entity_clsid(const char* name);
	void		load_system_ini(const char* game_config);
private:
	void		init();

private:
	const char*			m_game_config;
	xr_ini_file*			m_system_ini;
	std::vector<factory_item_base*>	m_clsids;
};

xr_entity_factory::xr_entity_factory(): m_system_ini(0)
{
	init();
}

xr_entity_factory::~xr_entity_factory()
{
	delete m_system_ini;
	delete_elements(m_clsids);
}

static xr_entity_factory g_entity_factory;

struct clsid_pred { bool operator()(const factory_item_base* l, const factory_item_base* r) const {
	return l->clsid() < r->clsid();
}};

void xr_entity_factory::init()
{
	// engine classes
	m_clsids.push_back(new factory_item<cse_alife_graph_point>("AI_GRAPH"));
	m_clsids.push_back(new factory_item<cse_alife_online_offline_group>("ON_OFF_G"));
	m_clsids.push_back(new factory_item<se_actor>("O_ACTOR"));	// was cse_alife_creature_actor in SoC
//	m_clsids.push_back(new factory_item<cse_spectator>("SPECT"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_FLESH"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_HIMER"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_DOG_R"));
	m_clsids.push_back(new factory_item<cse_alife_human_stalker>("AI_STL"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_BLOOD"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_BOAR"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_DOG_B"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_DOG_P"));
	m_clsids.push_back(new factory_item<cse_alife_psy_dog_phantom>("AI_DOG_F"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_BURER"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_GIANT"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_CONTR"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_POLTR"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_ZOM"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_FRACT"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_SNORK"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_CAT"));
	m_clsids.push_back(new factory_item<cse_alife_monster_base>("AI_TUSH"));
	m_clsids.push_back(new factory_item<cse_alife_creature_phantom>("AI_PHANT"));
	m_clsids.push_back(new factory_item<cse_alife_trader>("AI_TRADE"));
	m_clsids.push_back(new factory_item<cse_alife_creature_crow>("AI_CROW"));
	m_clsids.push_back(new factory_item<cse_alife_car>("C_NIVA"));
	m_clsids.push_back(new factory_item<cse_alife_helicopter>("C_HLCPTR"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_MBALL"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_BDROP"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_NEEDL"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_BAST"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_BGRAV"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_DUMMY"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_ZUDA"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_THORN"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_FBALL"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_EBALL"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_RHAIR"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_GALAN"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("ARTEFACT"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_WMAGAZ"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined_w_gl>("W_WMAGGL"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined_w_gl>("W_FN2000"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined_w_gl>("W_AK74"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_LR300"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_HPSA"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_PM"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_FORT"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_BINOC"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_shotgun>("W_SHOTGN"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_SVD"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_SVU"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_RPG7"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_VAL"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_VINT"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_WALTHR"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined>("W_USP45"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_magazined_w_gl>("W_GROZA"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon>("W_KNIFE"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_shotgun>("W_BM16"));
	m_clsids.push_back(new factory_item<cse_alife_item_weapon_shotgun>("W_RG6"));
	m_clsids.push_back(new factory_item<cse_alife_item_ammo>("AMMO"));
	m_clsids.push_back(new factory_item<cse_alife_item_ammo>("A_VOG25"));
	m_clsids.push_back(new factory_item<cse_alife_item_ammo>("A_OG7B"));
	m_clsids.push_back(new factory_item<cse_alife_item_ammo>("A_M209"));
	m_clsids.push_back(new factory_item<cse_alife_item>("W_SCOPE"));
	m_clsids.push_back(new factory_item<cse_alife_item>("W_SILENC"));
	m_clsids.push_back(new factory_item<cse_alife_item>("W_GLAUNC"));
	m_clsids.push_back(new factory_item<cse_alife_item_bolt>("II_BOLT"));
	m_clsids.push_back(new factory_item<cse_alife_item>("II_MEDKI"));
	m_clsids.push_back(new factory_item<cse_alife_item>("II_BANDG"));
	m_clsids.push_back(new factory_item<cse_alife_item>("II_ANTIR"));
	m_clsids.push_back(new factory_item<cse_alife_item>("II_FOOD"));
	m_clsids.push_back(new factory_item<cse_alife_item>("II_BOTTL"));
	m_clsids.push_back(new factory_item<cse_alife_item_explosive>("II_EXPLO"));
	m_clsids.push_back(new factory_item<cse_alife_item_document>("II_DOC"));
	m_clsids.push_back(new factory_item<cse_alife_item>("II_ATTCH"));
	m_clsids.push_back(new factory_item<cse_alife_item_custom_outfit>("EQU_SCIE"));
	m_clsids.push_back(new factory_item<cse_alife_item_custom_outfit>("EQU_STLK"));
	m_clsids.push_back(new factory_item<cse_alife_item_custom_outfit>("EQU_MLTR"));
	m_clsids.push_back(new factory_item<cse_alife_item_custom_outfit>("EQU_EXO"));
	m_clsids.push_back(new factory_item<cse_alife_item_grenade>("G_F1"));
	m_clsids.push_back(new factory_item<cse_alife_item_grenade>("G_RGD5"));
//	m_clsids.push_back(new factory_item<cse_temporary>("G_RPG7"));
//	m_clsids.push_back(new factory_item<cse_temporary>("G_FAKE"));
	m_clsids.push_back(new factory_item<cse_alife_item>("MP_PLBAG"));
	m_clsids.push_back(new factory_item<cse_alife_custom_zone>("Z_ZONE"));
	m_clsids.push_back(new factory_item<cse_alife_anomalous_zone>("Z_MBALD"));
	m_clsids.push_back(new factory_item<cse_alife_anomalous_zone>("Z_MINCER"));
	m_clsids.push_back(new factory_item<cse_alife_anomalous_zone>("Z_ACIDF"));
	m_clsids.push_back(new factory_item<cse_alife_anomalous_zone>("Z_GALANT"));
	m_clsids.push_back(new factory_item<cse_alife_anomalous_zone>("Z_RADIO"));
	m_clsids.push_back(new factory_item<cse_alife_zone_visual>("Z_BFUZZ"));
	m_clsids.push_back(new factory_item<cse_alife_zone_visual>("Z_RUSTYH"));
	m_clsids.push_back(new factory_item<cse_alife_anomalous_zone>("Z_DEAD"));
	m_clsids.push_back(new factory_item<cse_alife_space_restrictor>("SCRIPTZN"));
	m_clsids.push_back(new factory_item<cse_alife_smart_zone>("SMRTZONE"));
	m_clsids.push_back(new factory_item<cse_alife_team_base_zone>("Z_TEAMBS"));
	m_clsids.push_back(new factory_item<cse_alife_torrid_zone>("Z_TORRID"));
	m_clsids.push_back(new factory_item<cse_alife_space_restrictor>("SPACE_RS"));
	m_clsids.push_back(new factory_item<cse_alife_zone_visual>("Z_AMEBA"));
	m_clsids.push_back(new factory_item<cse_alife_anomalous_zone>("Z_NOGRAV"));
	m_clsids.push_back(new factory_item<cse_alife_item_detector>("D_SIMDET"));
	m_clsids.push_back(new factory_item<cse_alife_item_torch>("D_TORCH"));
	m_clsids.push_back(new factory_item<cse_alife_item_pda>("D_PDA"));
	m_clsids.push_back(new factory_item<cse_alife_object_projector>("O_SEARCH"));
	m_clsids.push_back(new factory_item<cse_alife_mounted_weapon>("W_MOUNTD"));
	m_clsids.push_back(new factory_item<cse_alife_stationary_mgun>("W_STMGUN"));
	m_clsids.push_back(new factory_item<cse_alife_object_hanging_lamp>("O_HLAMP"));
	m_clsids.push_back(new factory_item<cse_alife_object_physic>("O_PHYSIC"));
	m_clsids.push_back(new factory_item<cse_alife_dynamic_object_visual>("SCRPTOBJ"));
	m_clsids.push_back(new factory_item<cse_alife_object_breakable>("O_BRKBL"));
	m_clsids.push_back(new factory_item<cse_alife_object_climable>("O_CLMBL"));
	m_clsids.push_back(new factory_item<cse_alife_ph_skeleton_object>("P_SKELET"));
	m_clsids.push_back(new factory_item<cse_alife_object_physic>("P_DSTRBL"));
	m_clsids.push_back(new factory_item<cse_inventory_box>("O_INVBOX"));
	// Clear Sky (including build 3120) engine classes
	m_clsids.push_back(new factory_item<cse_alife_item_detector>("D_ELITE"));
	m_clsids.push_back(new factory_item<cse_alife_item_detector>("D_ADVANC"));
	m_clsids.push_back(new factory_item<cse_alife_item>("D_FLARE"));
	m_clsids.push_back(new factory_item<cse_alife_item>("II_BTTCH"));
	m_clsids.push_back(new factory_item<cse_alife_item>("NW_ATTCH"));
	m_clsids.push_back(new factory_item<cse_alife_anomalous_zone>("Z_CFIRE"));
	m_clsids.push_back(new factory_item<cse_alife_item_artefact>("AF_CTA"));

	// CoP new engine classes
	m_clsids.push_back(new factory_item<cse_alife_object_physic>("O_DSTR_S"));
	m_clsids.push_back(new factory_item<cse_alife_object_hanging_lamp>("SO_HLAMP"));
	m_clsids.push_back(new factory_item<cse_alife_item_grenade>("G_F1_S"));
	m_clsids.push_back(new factory_item<cse_alife_item_grenade>("G_RGD5_S"));
	m_clsids.push_back(new factory_item<cse_alife_item_pda>("S_PDA"));
	m_clsids.push_back(new factory_item<cse_alife_item>("S_FOOD"));
	m_clsids.push_back(new factory_item<cse_alife_item>("WP_GLAUN"));
	m_clsids.push_back(new factory_item<cse_alife_item>("WP_SILEN"));
	m_clsids.push_back(new factory_item<cse_alife_item_ammo>("AMMO_S"));
	m_clsids.push_back(new factory_item<cse_alife_item_ammo>("S_VOG25"));
	m_clsids.push_back(new factory_item<cse_alife_item_ammo>("S_M209"));
	m_clsids.push_back(new factory_item<cse_alife_item_ammo>("S_OG7B"));
	m_clsids.push_back(new factory_item<cse_alife_item_helmet>("E_HLMET"));
	m_clsids.push_back(new factory_item<cse_alife_item_explosive>("S_EXPLO"));
	m_clsids.push_back(new factory_item<cse_alife_item_detector>("DET_ELIT"));

	// script classes
	m_clsids.push_back(new factory_item<se_level_changer>("LVL_CHNG"));	// was cse_alife_level_changer in pre-3502 builds
	m_clsids.push_back(new factory_item<se_smart_terrain>("SMRTTRRN"));
	m_clsids.push_back(new factory_item<se_respawn>("RE_SPAWN"));
//	m_clsids.push_back(new factory_item<cse_switcher>("O_SWITCH"));
	m_clsids.push_back(new factory_item<se_stalker>("AI_STL_S"));
	m_clsids.push_back(new factory_item<se_trader>("AI_TRD_S"));
	m_clsids.push_back(new factory_item<se_heli>("C_HLCP_S"));
	m_clsids.push_back(new factory_item<se_restrictor>("SPC_RS_S"));
	m_clsids.push_back(new factory_item<se_physic>("O_PHYS_S"));
	m_clsids.push_back(new factory_item<se_artefact>("SCRPTART"));
	m_clsids.push_back(new factory_item<se_car>("SCRPTCAR"));
	m_clsids.push_back(new factory_item<se_monster>("SM_BLOOD"));
	m_clsids.push_back(new factory_item<se_monster>("SM_BOARW"));
	m_clsids.push_back(new factory_item<se_monster>("SM_DOG_S"));
	m_clsids.push_back(new factory_item<se_monster>("SM_FLESH"));
	m_clsids.push_back(new factory_item<se_monster>("SM_P_DOG"));
	m_clsids.push_back(new factory_item<se_monster>("SM_BURER"));
	m_clsids.push_back(new factory_item<se_monster>("SM_CAT_S"));
	m_clsids.push_back(new factory_item<se_monster>("SM_CHIMS"));
	m_clsids.push_back(new factory_item<se_monster>("SM_CONTR"));
	m_clsids.push_back(new factory_item<se_monster>("SM_IZLOM"));
	m_clsids.push_back(new factory_item<se_monster>("SM_POLTR"));
	m_clsids.push_back(new factory_item<se_monster>("SM_GIANT"));
	m_clsids.push_back(new factory_item<se_monster>("SM_ZOMBI"));
	m_clsids.push_back(new factory_item<se_monster>("SM_SNORK"));
	m_clsids.push_back(new factory_item<se_monster>("SM_TUSHK"));
	m_clsids.push_back(new factory_item<se_monster>("SM_DOG_P"));
	m_clsids.push_back(new factory_item<se_monster>("SM_DOG_F"));
	m_clsids.push_back(new factory_item<se_item_torch>("TORCH_S"));
	m_clsids.push_back(new factory_item<se_outfit>("E_STLK"));
	m_clsids.push_back(new factory_item<se_item>("WP_SCOPE"));
	m_clsids.push_back(new factory_item<se_weapon_magazined_w_gl>("WP_AK74"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_LR300"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_BINOC"));
	m_clsids.push_back(new factory_item<se_weapon_shotgun>("WP_BM16"));
	m_clsids.push_back(new factory_item<se_weapon_magazined_w_gl>("WP_GROZA"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_SVD"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_HPSA"));
	m_clsids.push_back(new factory_item<se_weapon>("WP_KNIFE"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_PM"));
	m_clsids.push_back(new factory_item<se_weapon_shotgun>("WP_RG6"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_RPG7"));
	m_clsids.push_back(new factory_item<se_weapon_shotgun>("WP_SHOTG"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_MAGAZ"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_SVU"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_USP45"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_VAL"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_VINT"));
	m_clsids.push_back(new factory_item<se_weapon_magazined>("WP_WALTH"));
	m_clsids.push_back(new factory_item<se_zone_visual>("ZS_BFUZZ"));
	m_clsids.push_back(new factory_item<se_zone_anom>("ZS_MBALD"));
	m_clsids.push_back(new factory_item<se_zone_anom>("ZS_GALAN"));
	m_clsids.push_back(new factory_item<se_zone_anom>("ZS_MINCE"));
	// clear sky script classes
	m_clsids.push_back(new factory_item<se_actor>("S_ACTOR"));			// + CoP
	m_clsids.push_back(new factory_item<se_sim_faction>("SFACTION"));
	m_clsids.push_back(new factory_item<se_smart_cover>("SMRT_C_S"));	// + CoP

	// ZENOBIAN mod script classes
	m_clsids.push_back(new factory_item<se_anomaly_field>("ANOMFLD1"));
	m_clsids.push_back(new factory_item<se_turret_mgun>("TURRETMG"));
	m_clsids.push_back(new factory_item<se_zone_anom>("Z_MINES"));
	
	// CoP new script classes
	m_clsids.push_back(new factory_item<se_invbox>("S_INVBOX"));
	m_clsids.push_back(new factory_item<se_zone_anom>("ZS_RADIO"));
	m_clsids.push_back(new factory_item<se_zone_torrid>("ZS_TORRD"));
	m_clsids.push_back(new factory_item<se_weapon_shotgun>("WP_ASHTG"));

	//LA new script classes
	m_clsids.push_back(new factory_item<se_monster>("SM_KARLO"));
	m_clsids.push_back(new factory_item<se_monster>("SM_RAT"));
	m_clsids.push_back(new factory_item<se_shelter>("LA_SHELT"));
	m_clsids.push_back(new factory_item<se_item>("IS_ATTCH"));
	m_clsids.push_back(new factory_item<se_zone_anom>("ZS_NOGRA"));
	m_clsids.push_back(new factory_item<se_zone_visual>("ZS_RUSTY"));
	m_clsids.push_back(new factory_item<se_safe>("LA_PHSAF"));
	m_clsids.push_back(new factory_item<se_safe>("LA_SAFE"));
	m_clsids.push_back(new factory_item<cse_turret_mgun>("W_TURRET"));

	// prepare for bisection
	std::sort(m_clsids.begin(), m_clsids.end(), clsid_pred());
}

struct clsid_pred2 {
	const xr_clsid clsid;
	explicit clsid_pred2(const xr_clsid& _clsid): clsid(_clsid) {}
	bool operator()(const factory_item_base* p) const {
		return p->clsid() < clsid;
	}
};

cse_abstract* xr_entity_factory::create(const char* name)
{
	if (m_system_ini == 0)
		m_system_ini = new xr_ini_file(PA_GAME_CONFIG, "system.ltx");

	if (!m_system_ini->section_exist(name)) {
		msg("can't find entity %s", name);
		return 0;
	}

	xr_clsid clsid(m_system_ini->r_clsid(name, "class"));
	std::vector<factory_item_base*>::iterator it = lower_bound_if(
			m_clsids.begin(), m_clsids.end(), clsid_pred2(clsid));
	if (it != m_clsids.end() && (*it)->clsid() == clsid)
		return (*it)->create();

	char clsid_name[9] = {};
	clsid.get(clsid_name);
	msg("can't create entity %s, %s", name, clsid_name);
	return 0;
}

xr_clsid* xr_entity_factory::get_entity_clsid(const char* name)
{
	if (m_system_ini == 0)
		m_system_ini = new xr_ini_file(PA_GAME_CONFIG, "system.ltx");
  
	if (m_system_ini->section_exist(name)) {
		xr_clsid clsid(m_system_ini->r_clsid(name, "class"));
		std::vector<factory_item_base*>::iterator it = lower_bound_if(
				m_clsids.begin(), m_clsids.end(), clsid_pred2(clsid));
		if (it != m_clsids.end() && (*it)->clsid() == clsid)
		{
			xr_clsid* temp = &clsid;
			return temp;
		}
	}

	return NULL;
}

void xr_entity_factory::load_system_ini(const char* game_config)
{
	delete m_system_ini;
	m_system_ini = new xr_ini_file(game_config, "system.ltx");
}

cse_abstract* xray_re::create_entity(const char* name)
{
	return g_entity_factory.create(name);
}

void xray_re::load_system_ini(const char* game_config)
{
	g_entity_factory.load_system_ini(game_config);
}

xr_clsid* xray_re::get_entity_clsid(const char *name)
{
	return g_entity_factory.get_entity_clsid(name);
}
