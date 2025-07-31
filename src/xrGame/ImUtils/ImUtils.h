#pragma once

#include "../xrCore/clsid.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"
#include <clsid_game.h>

struct clsid_manager
{
	void add_mp_stuff(CLASS_ID id);
	bool is_mp_stuff(CLASS_ID id);

	void add_item(CLASS_ID id);
	bool is_item(CLASS_ID id);

	void add_outfit(CLASS_ID id);
	bool is_outfit(CLASS_ID id);

	void add_ammo(CLASS_ID id);
	bool is_ammo(CLASS_ID id);

	void add_weapon(CLASS_ID id);
	bool is_weapon(CLASS_ID id);

	void add_monster(CLASS_ID id);
	bool is_monster(CLASS_ID id);

	void add_addon(CLASS_ID id);
	bool is_addon(CLASS_ID id);

	void add_artefact(CLASS_ID id);
	bool is_artefact(CLASS_ID id);

	void add_vehicle(CLASS_ID id);
	bool is_vehicle(CLASS_ID id);

	const xr_set<CLASS_ID>& get_items(void) const { return items; }
	const xr_set<CLASS_ID>& get_outfits(void) const { return outfits; }
	const xr_set<CLASS_ID>& get_ammo(void) const { return ammo; }
	const xr_set<CLASS_ID>& get_monsters(void) const { return monsters; }
	const xr_set<CLASS_ID>& get_weapons(void) const { return weapons; }
	const xr_set<CLASS_ID>& get_addons(void) const { return addons; }
	const xr_set<CLASS_ID>& get_artefacts(void) const { return artefacts; }
	const xr_set<CLASS_ID>& get_vehicles(void) const { return vehicles; }
	const xr_set<CLASS_ID>& get_mp_stuffs(void) const { return mp_stuffs; }

	const char* translateCLSID(CLASS_ID id);

	// reminder: information took from class_registrator.script because it overloads existed classes (clsids)
	CLASS_ID artefact_s = TEXT2CLSID("SCRPTART");
	CLASS_ID artefact = TEXT2CLSID("ARTEFACT");

	//CLASS_ID car = TEXT2CLSID("SCRPTCAR");
	CLASS_ID car = TEXT2CLSID("C_NIVA");
	CLASS_ID stalker = TEXT2CLSID("AI_STL_S");
	CLASS_ID smart_terrain = TEXT2CLSID("SMRTTRRN");
	CLASS_ID smart_cover = TEXT2CLSID("SMRT_C_S");
	CLASS_ID level_changer = TEXT2CLSID("LVL_CHNG");
	CLASS_ID sim_squad_scripted = TEXT2CLSID("ON_OFF_S");

	CLASS_ID outfit = TEXT2CLSID("E_STLK");
	CLASS_ID helmet = TEXT2CLSID("E_HLMET");

	CLASS_ID monster_bloodsucker = TEXT2CLSID("SM_BLOOD");
	CLASS_ID monster_boar = TEXT2CLSID("SM_BOARW");
	CLASS_ID monster_dog = TEXT2CLSID("SM_DOG_S");
	CLASS_ID monster_flesh = TEXT2CLSID("SM_FLESH");
	CLASS_ID monster_pseudodog = TEXT2CLSID("SM_P_DOG");
	CLASS_ID monster_burer = TEXT2CLSID("SM_BURER");
	CLASS_ID monster_cat = TEXT2CLSID("SM_CAT_S");
	CLASS_ID monster_chimera = TEXT2CLSID("SM_CHIMS");
	CLASS_ID monster_controller = TEXT2CLSID("SM_CONTR");
	CLASS_ID monster_izlom = TEXT2CLSID("SM_IZLOM");
	CLASS_ID monster_poltergeist = TEXT2CLSID("SM_POLTR");
	CLASS_ID monster_pseudogigant = TEXT2CLSID("SM_GIANT");
	CLASS_ID monster_zombie = TEXT2CLSID("SM_ZOMBI");
	CLASS_ID monster_snork = TEXT2CLSID("SM_SNORK");
	CLASS_ID monster_tushkano = TEXT2CLSID("SM_TUSHK");
	CLASS_ID monster_psydog = TEXT2CLSID("SM_DOG_P");
	CLASS_ID monster_psydogphantom = TEXT2CLSID("SM_DOG_F");

	CLASS_ID weapon_binocular = TEXT2CLSID("WP_BINOC");
	CLASS_ID weapon_knife = TEXT2CLSID("WP_KNIFE");
	CLASS_ID weapon_bm16 = TEXT2CLSID("WP_BM16");
	CLASS_ID weapon_groza = TEXT2CLSID("WP_GROZA");
	CLASS_ID weapon_svd = TEXT2CLSID("WP_SVD");
	CLASS_ID weapon_ak74 = TEXT2CLSID("WP_AK74");
	CLASS_ID weapon_lr300 = TEXT2CLSID("WP_LR300");
	CLASS_ID weapon_hpsa = TEXT2CLSID("WP_HPSA");
	CLASS_ID weapon_pm = TEXT2CLSID("WP_PM");
	CLASS_ID weapon_rg6 = TEXT2CLSID("WP_RG6");
	CLASS_ID weapon_rpg7 = TEXT2CLSID("WP_RPG7");
	CLASS_ID weapon_shotgun = TEXT2CLSID("WP_SHOTG");
	CLASS_ID weapon_autoshotgun = TEXT2CLSID("WP_ASHTG");
	CLASS_ID weapon_svu = TEXT2CLSID("WP_SVU");
	CLASS_ID weapon_usp45 = TEXT2CLSID("WP_USP45");
	CLASS_ID weapon_val = TEXT2CLSID("WP_VAL");
	CLASS_ID weapon_vintorez = TEXT2CLSID("WP_VINT");
	CLASS_ID weapon_walther = TEXT2CLSID("WP_WALTH");
	CLASS_ID weapon_magazine = TEXT2CLSID("WP_MAGAZ");
	CLASS_ID weapon_stationary_machine_gun = TEXT2CLSID("W_STMGUN");

	CLASS_ID ammo_base = TEXT2CLSID("AMMO_S");
	CLASS_ID ammo_vog25 = TEXT2CLSID("S_VOG25");
	CLASS_ID ammo_og7b = TEXT2CLSID("S_OG7B");
	CLASS_ID ammo_m209 = TEXT2CLSID("S_M209");
	CLASS_ID ammo_f1 = TEXT2CLSID("G_F1_S");
	CLASS_ID ammo_rgd5 = TEXT2CLSID("G_RGD5_S");

	CLASS_ID addon_scope = TEXT2CLSID("WP_SCOPE");
	CLASS_ID addon_silen = TEXT2CLSID("WP_SILEN");
	CLASS_ID addon_glaun = TEXT2CLSID("WP_GLAUN");

	CLASS_ID item_torch = TEXT2CLSID("TORCH_S");
	CLASS_ID item_detector_scientific = TEXT2CLSID("DET_SCIE");
	CLASS_ID item_detector_elite = TEXT2CLSID("DET_ELIT");
	CLASS_ID item_detector_advanced = TEXT2CLSID("DET_ADVA");
	CLASS_ID item_detector_simple = TEXT2CLSID("DET_SIMP");
	CLASS_ID item_pda = TEXT2CLSID("S_PDA");
	CLASS_ID item_d_pda = TEXT2CLSID("D_PDA");
	CLASS_ID item_ii_attch = TEXT2CLSID("II_ATTCH");
	CLASS_ID item_medkit = TEXT2CLSID("S_MEDKI");
	CLASS_ID item_bandage = TEXT2CLSID("S_BANDG");
	CLASS_ID item_antirad = TEXT2CLSID("S_ANTIR");
	CLASS_ID item_food = TEXT2CLSID("S_FOOD");
	CLASS_ID item_bottle = TEXT2CLSID("S_BOTTL");

	CLASS_ID mp_out_scientific = CLSID_EQUIPMENT_SCIENTIFIC;
	CLASS_ID mp_out_stalker = CLSID_EQUIPMENT_STALKER;
	CLASS_ID mp_out_military = CLSID_EQUIPMENT_MILITARY;
	CLASS_ID mp_out_exo = CLSID_EQUIPMENT_EXO;
	CLASS_ID mp_helmet = CLSID_EQUIPMENT_HELMET;
	CLASS_ID mp_weapon_fn2000 = CLSID_OBJECT_W_FN2000;
	CLASS_ID mp_weapon_binocular = CLSID_OBJECT_W_BINOCULAR;
	CLASS_ID mp_weapon_knife = CLSID_OBJECT_W_KNIFE;
	CLASS_ID mp_weapon_bm16 = CLSID_OBJECT_W_BM16;
	CLASS_ID mp_weapon_groza = CLSID_OBJECT_W_GROZA;
	CLASS_ID mp_weapon_svd = CLSID_OBJECT_W_SVD;
	CLASS_ID mp_weapon_ak74 = CLSID_OBJECT_W_AK74;
	CLASS_ID mp_weapon_lr300 = CLSID_OBJECT_W_LR300;
	CLASS_ID mp_weapon_hpsa = CLSID_OBJECT_W_HPSA;
	CLASS_ID mp_weapon_pm = CLSID_OBJECT_W_PM;
	CLASS_ID mp_weapon_fort = CLSID_OBJECT_W_FORT;
	CLASS_ID mp_weapon_rg6 = CLSID_OBJECT_W_RG6;
	CLASS_ID mp_weapon_rpg7 = CLSID_OBJECT_W_RPG7;
	CLASS_ID mp_weapon_shotgun = CLSID_OBJECT_W_SHOTGUN;
	CLASS_ID mp_weapon_svu = CLSID_OBJECT_W_SVU;
	CLASS_ID mp_weapon_usp45 = CLSID_OBJECT_W_USP45;
	CLASS_ID mp_weapon_val = CLSID_OBJECT_W_VAL;
	CLASS_ID mp_weapon_vintorez = CLSID_OBJECT_W_VINTOREZ;
	CLASS_ID mp_weapon_walther = CLSID_OBJECT_W_WALTHER;
	CLASS_ID mp_weapon_magazine = CLSID_OBJECT_W_MAGAZINED;
	CLASS_ID mp_weapon_magazine_gl = CLSID_OBJECT_W_MAGAZWGL;

	CLASS_ID mp_ammo_base = CLSID_OBJECT_AMMO;
	CLASS_ID mp_ammo_og7b = CLSID_OBJECT_A_OG7B;
	CLASS_ID mp_ammo_m209 = CLSID_OBJECT_A_M209;
	CLASS_ID mp_ammo_vog25 = CLSID_OBJECT_A_VOG25;
	CLASS_ID mp_f1 = CLSID_GRENADE_F1;
	CLASS_ID mp_rgd5 = CLSID_GRENADE_RGD5;
	//CLASS_ID mp_rpg7 = CLSID_OBJECT_G_RPG7;
	CLASS_ID mp_addon_scope = CLSID_OBJECT_W_SCOPE;
	CLASS_ID mp_addon_silen = CLSID_OBJECT_W_SILENCER;
	CLASS_ID mp_addon_glaun = CLSID_OBJECT_W_GLAUNCHER;

	CLASS_ID mp_art_mercury_ball = CLSID_AF_MERCURY_BALL;
	CLASS_ID mp_art_black_drops = CLSID_AF_BLACKDROPS;
	CLASS_ID mp_art_needles = CLSID_AF_NEEDLES;
	CLASS_ID mp_art_bast_artefact = CLSID_AF_BAST;
	CLASS_ID mp_art_gravi_black = CLSID_AF_BLACK_GRAVI;
	CLASS_ID mp_art_dummy = CLSID_AF_DUMMY;
	CLASS_ID mp_art_zuda = CLSID_AF_ZUDA;
	CLASS_ID mp_art_thorn = CLSID_AF_THORN;
	CLASS_ID mp_art_faded_ball = CLSID_AF_FADED_BALL;
	CLASS_ID mp_art_electric_ball = CLSID_AF_ELECTRIC_BALL;
	CLASS_ID mp_art_rusty_hair = CLSID_AF_RUSTY_HAIR;
	CLASS_ID mp_art_galantine = CLSID_AF_GALANTINE;
	CLASS_ID mp_art_gravi = CLSID_AF_GRAVI;
	CLASS_ID mp_art_cta = CLSID_AF_CTA;

private:
	xr_set<CLASS_ID> weapons;
	xr_set<CLASS_ID> monsters;
	xr_set<CLASS_ID> zones;
	xr_set<CLASS_ID> items;
	xr_set<CLASS_ID> outfits;
	xr_set<CLASS_ID> ammo;
	xr_set<CLASS_ID> addons;
	xr_set<CLASS_ID> artefacts;
	xr_set<CLASS_ID> vehicles;
	xr_set<CLASS_ID> mp_stuffs;
};

enum eSelectedType {
	kSelectedType_All,
	kSelectedType_SmartTerrain,
	kSelectedType_SmartCover,
	kSelectedType_LevelChanger,
	kSelectedType_Artefact,
	kSelectedType_Stalker,
	kSelectedType_Car,
	kSelectedType_Monster_All,
	kSelectedType_Monster_BloodSucker,
	kSelectedType_Monster_Boar,
	kSelectedType_Monster_Dog,
	kSelectedType_Monster_Flesh,
	kSelectedType_Monster_PseudoDog,
	kSelectedType_Monster_Burer,
	kSelectedType_Monster_Cat,
	kSelectedType_Monster_Chimera,
	kSelectedType_Monster_Controller,
	kSelectedType_Monster_Izlom,
	kSelectedType_Monster_Poltergeist,
	kSelectedType_Monster_PseudoGigant,
	kSelectedType_Monster_Zombie,
	kSelectedType_Monster_Snork,
	kSelectedType_Monster_Tushkano,
	kSelectedType_Monster_PsyDog,
	kSelectedType_Monster_PsyDogPhantom,
	kSelectedType_Weapon_All,
	kSelectedType_Weapon_Binocular,
	kSelectedType_Weapon_Knife,
	kSelectedType_Weapon_BM16,
	kSelectedType_Weapon_Groza,
	kSelectedType_Weapon_SVD,
	kSelectedType_Weapon_AK74,
	kSelectedType_Weapon_LR300,
	kSelectedType_Weapon_HPSA,
	kSelectedType_Weapon_PM,
	kSelectedType_Weapon_RG6,
	kSelectedType_Weapon_RPG7,
	kSelectedType_Weapon_Shotgun,
	kSelectedType_Weapon_AutoShotgun,
	kSelectedType_Weapon_SVU,
	kSelectedType_Weapon_USP45,
	kSelectedType_Weapon_VAL,
	kSelectedType_Weapon_VINTOREZ,
	kSelectedType_Weapon_WALTHER,
	kSelectedType_Weapon_Magazine,
	kSelectedType_Weapon_StationaryMachineGun,
	kSelectedType_Count
};

// todo: statistics must be counting on Level and xr_offline side (when an object adds or deletes)!!!! Don't calculate it dynamically due to clipping feature for optimization of iteration (you're unable to do that fast, because you can't iterate through whole array)
struct CImGuiGameSearchManager {

	bool is_initialized = false;
	bool show_alive_creatures = {};
	int selected_type = {};
	const char* pTranslatedLabel_SmartCover{};
	const char* pTranslatedLabel_SmartTerrain{};
	const char* pTranslatedLabel_Stalker{};
	const char* pTranslatedLabel_Car{};
	const char* pTranslatedLabel_LevelChanger{};
	const char* pTranslatedLabel_Artefact{};
	xr_hash_map<eSelectedType, CLASS_ID> type_to_class;
	xr_hash_map<CLASS_ID, eSelectedType> class_to_type;
	char search_string[256] = {};
	const char* combo_items[(eSelectedType::kSelectedType_Count)] = {};
	int counts[(eSelectedType::kSelectedType_Count)]{};
	char category_names[(eSelectedType::kSelectedType_Count)][32] = {};

	eSelectedType convertCLSIDToType(CLASS_ID id);
	const char* convertTypeToString(int type);
	bool valid(CLASS_ID id);
	void count(CLASS_ID id);
	void init();


private:
	// pre-caching naming for fast accessing and reducing requests to StringTable manager, it is slow...
	void initTranslatedLabels();
	const char* getDefaultNameOfSelectedType(eSelectedType type);
	const char* getTranslatedString(eSelectedType type);
};

constexpr float kGeneralAlphaLevelForImGuiWindows = 0.5f;

void InitSections();
void InitImGuiCLSIDInGame();
void InitImGuiSearchInGame();

void RenderTimeManagerWindow();
void RenderSpawnManagerWindow();
void RenderWeaponManagerWindow();
void RenderSearchManagerWindow();

void RenderToolsOMFEditorWindow();

void DestroySpawnManagerWindow();

void RegisterImGuiInGame();
void execute_console_command_deferred(CConsole* c, LPCSTR string_to_execute);

extern clsid_manager* g_pClsidManager;
extern CImGuiGameSearchManager imgui_search_manager;
