#pragma once

#include "../xrCore/clsid.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"
#include <clsid_game.h>

struct STextureParams;

constexpr ImVec4 kAuthorTextColor = ImVec4(0.063f, 0.788f, 0.196f, 1.0f);
constexpr ImVec4 kContributorsTextColor = ImVec4(0.306f, 0.745f, 0.878f, 1.0f);


struct clsid_manager
{
	void add_mp_stuff(CLASS_ID id);
	bool is_mp_stuff(CLASS_ID id);

	void add_item(CLASS_ID id);
	bool is_item(CLASS_ID id);

	void add_item_used(CLASS_ID id);
	bool is_item_used(CLASS_ID id);

	void add_device(CLASS_ID id);
	bool is_device(CLASS_ID id);

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

	void add_dynamic_object(CLASS_ID id);
	bool is_dynamic_object(CLASS_ID id);

	void add_explo(CLASS_ID id);
	bool is_explo(CLASS_ID id);

	void add_npc(CLASS_ID id);
	bool is_npc(CLASS_ID id);

	void add_anomaly(CLASS_ID id);
	bool is_anomaly(CLASS_ID id);

	bool is_squad(CLASS_ID id);
	void add_squad(CLASS_ID id);

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
	CLASS_ID monster_anomal_pseudogigant = TEXT2CLSID("SM_GIG_A");
	CLASS_ID monster_zombie = TEXT2CLSID("SM_ZOMBI");
	CLASS_ID monster_snork = TEXT2CLSID("SM_SNORK");
	CLASS_ID monster_tushkano = TEXT2CLSID("SM_TUSHK");
	CLASS_ID monster_psydog = TEXT2CLSID("SM_DOG_P");
	CLASS_ID monster_psydogphantom = TEXT2CLSID("SM_DOG_F");
	CLASS_ID monster_crow = TEXT2CLSID("AI_CROW");

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
	CLASS_ID weapon_flamethrower = CLSID_OBJECT_W_FLAMETHROWER;

	CLASS_ID ammo_base = TEXT2CLSID("AMMO_S");
	CLASS_ID ammo_vog25 = TEXT2CLSID("S_VOG25");
	CLASS_ID ammo_og7b = TEXT2CLSID("S_OG7B");
	CLASS_ID ammo_m209 = TEXT2CLSID("S_M209");
	CLASS_ID ammo_f1 = TEXT2CLSID("G_F1_S");
	CLASS_ID ammo_rgd5 = TEXT2CLSID("G_RGD5_S");
	CLASS_ID ammo_flame_canister = CLSID_OBJECT_FLAME_CANISTER;

	CLASS_ID addon_scope = TEXT2CLSID("WP_SCOPE");
	CLASS_ID addon_silen = TEXT2CLSID("WP_SILEN");
	CLASS_ID addon_glaun = TEXT2CLSID("WP_GLAUN");

	CLASS_ID item_torch = TEXT2CLSID("TORCH_S");
	CLASS_ID item_pda = TEXT2CLSID("S_PDA");
	CLASS_ID item_d_pda = TEXT2CLSID("D_PDA");
	CLASS_ID item_ii_attch = TEXT2CLSID("II_ATTCH");
	CLASS_ID item_medkit = TEXT2CLSID("S_MEDKI");
	CLASS_ID item_bandage = TEXT2CLSID("S_BANDG");
	CLASS_ID item_antirad = TEXT2CLSID("S_ANTIR");
	CLASS_ID item_bottle = TEXT2CLSID("S_BOTTL");

	CLASS_ID item_ii_explo = TEXT2CLSID("II_EXPLO");
	CLASS_ID item_ii_doc = TEXT2CLSID("II_DOC");
	CLASS_ID item_ii_bttch = TEXT2CLSID("II_BTTCH");
	CLASS_ID item_nw_attch = TEXT2CLSID("NW_ATTCH");
	CLASS_ID item_ii_bolt = CLSID_IITEM_BOLT;

	// Items used
	CLASS_ID item_food = TEXT2CLSID("S_FOOD");
	CLASS_ID item_ii_antir = CLSID_IITEM_ANTIRAD;
	CLASS_ID item_ii_medki = CLSID_IITEM_MEDKIT;
	CLASS_ID item_ii_bandg = CLSID_IITEM_BANDAGE;
	CLASS_ID item_ii_food = CLSID_IITEM_FOOD;
	CLASS_ID item_ii_bottl = CLSID_IITEM_BOTTLE;

	// Detectors
	CLASS_ID item_detector_scientific = TEXT2CLSID("DET_SCIE");
	CLASS_ID item_detector_elite = TEXT2CLSID("DET_ELIT");
	CLASS_ID item_detector_advanced = TEXT2CLSID("DET_ADVA");
	CLASS_ID item_detector_simple = TEXT2CLSID("DET_SIMP");
	CLASS_ID item_d_elite = CLSID_DETECTOR_ELITE;
	CLASS_ID item_d_scientific = CLSID_DETECTOR_SCIENTIFIC;
	CLASS_ID item_d_advanc = CLSID_DETECTOR_ADVANCED;
	CLASS_ID item_d_flare = TEXT2CLSID("D_FLARE");
	CLASS_ID item_d_simple = CLSID_DETECTOR_SIMPLE;
	CLASS_ID item_d_smetr = TEXT2CLSID("D_DSMETR");
	CLASS_ID item_d_custom = TEXT2CLSID("D_CUSTOM");

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

	// Dynamic objects
	CLASS_ID do_dstr_s = TEXT2CLSID("O_DSTR_S");
	CLASS_ID o_physic_s = TEXT2CLSID("O_PHYS_S");
	CLASS_ID do_object_item_std = CLSID_OBJECT_ITEM_STD;
	CLASS_ID do_object_breakable = CLSID_OBJECT_BREAKABLE;
	CLASS_ID do_object_climable = CLSID_OBJECT_CLIMABLE;
	CLASS_ID do_object_holder_ent = CLSID_OBJECT_HOLDER_ENT;
	CLASS_ID do_ph_skeleton_object = CLSID_PH_SKELETON_OBJECT;
	CLASS_ID do_object_physic = CLSID_OBJECT_PHYSIC;
	CLASS_ID do_physics_destr = CLSID_PHYSICS_DESTROYABLE;
	CLASS_ID do_invbox = CLSID_INVENTORY_BOX;
	CLASS_ID s_invbox = TEXT2CLSID("S_INVBOX");

	// Explo
	CLASS_ID item_s_explo = TEXT2CLSID("S_EXPLO");

	CLASS_ID zs_bfuzz = TEXT2CLSID("ZS_BFUZZ");
	CLASS_ID zs_galan = TEXT2CLSID("ZS_GALAN");
	CLASS_ID zs_mbald = TEXT2CLSID("ZS_MBALD");
	CLASS_ID zs_mince = TEXT2CLSID("ZS_MINCE");
	CLASS_ID zs_radio = TEXT2CLSID("ZS_RADIO");
	CLASS_ID zs_torrd = TEXT2CLSID("ZS_TORRD");
	CLASS_ID z_cfire = TEXT2CLSID("Z_CFIRE");
	CLASS_ID z_mbald = TEXT2CLSID("Z_MBALD");
	CLASS_ID z_nograv = TEXT2CLSID("Z_NOGRAV");
	CLASS_ID z_radio = TEXT2CLSID("Z_RADIO");
	CLASS_ID z_teambs = TEXT2CLSID("Z_TEAMBS");

private:
	xr_set<CLASS_ID> weapons;
	xr_set<CLASS_ID> monsters;
	xr_set<CLASS_ID> zones;
	xr_set<CLASS_ID> items;
	xr_set<CLASS_ID> items_used;
	xr_set<CLASS_ID> devices;
	xr_set<CLASS_ID> outfits;
	xr_set<CLASS_ID> ammo;
	xr_set<CLASS_ID> addons;
	xr_set<CLASS_ID> artefacts;
	xr_set<CLASS_ID> vehicles;
	xr_set<CLASS_ID> mp_stuffs;
	xr_set<CLASS_ID> dynamic_objects;
	xr_set<CLASS_ID> explosives;
	xr_set<CLASS_ID> npc_list;
	xr_set<CLASS_ID> anomalies;
	xr_set<CLASS_ID> squads;
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
	kSelectedType_Monster_AnomalPseudoGigant,
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

struct CHudAdjustManager
{
	bool is_initialized;
};

template <typename T>
class ThreadSafeQueue {
private:
	std::queue<T> data_queue;
	mutable std::mutex mut;
	std::condition_variable cond;

public:
	void push(T value) {
		std::lock_guard<std::mutex> lock(mut); // Acquire lock
		data_queue.push(std::move(value));     // Push item
		cond.notify_one();                     // Notify one waiting consumer
	} // Lock is automatically released here

	T pop() {
		std::unique_lock<std::mutex> lock(mut); // Acquire lock
		// Wait until queue is not empty, condition variable releases lock while waiting
		cond.wait(lock, [this] { return !data_queue.empty(); });
		T value = std::move(data_queue.front()); // Get item
		data_queue.pop();                        // Remove item
		return value;
	} // Lock is automatically released here

	bool empty() const {
		std::lock_guard<std::mutex> lock(mut);
		return data_queue.empty();
	}
};

struct CImGuiTextureEditor
{
	enum eAnalyzedStatus
	{
		kTooLongPath = 1 << 1,
		kHasTHM = 1 << 2,
		kTHMIsNotValid = 1 << 3,
		kDimensionsNotPowerOf2 = 1 << 4,
		kNoMipMaps = 1 << 5,
		kIgnoreTHM = 1 << 6,
		kInvalidMetadata = 1 << 7
	};

	enum class eFilterQueryType : u32
	{
		kSearch,
		kInvalidFirst,
		/// @brief means we don't clear filter_query and just apply filtering for existed set of entries
		kInvalidFirstExisted,
		kNoFilter,
		kInvalid = u32(-1)
	};

	// don't store metadata only on when selected
	struct STextureEntry
	{
		bool is_valid() const
		{
			return analyze_status_result_flags != 0 && (((analyze_status_result_flags & eAnalyzedStatus::kHasTHM) == eAnalyzedStatus::kHasTHM) || ((analyze_status_result_flags & eAnalyzedStatus::kIgnoreTHM) == eAnalyzedStatus::kIgnoreTHM)) && (
				(
					(analyze_status_result_flags & eAnalyzedStatus::kTooLongPath) == eAnalyzedStatus::kTooLongPath ||
					(analyze_status_result_flags & eAnalyzedStatus::kTHMIsNotValid) == eAnalyzedStatus::kTHMIsNotValid ||
					(analyze_status_result_flags & eAnalyzedStatus::kDimensionsNotPowerOf2) == eAnalyzedStatus::kDimensionsNotPowerOf2 ||
					(analyze_status_result_flags & eAnalyzedStatus::kNoMipMaps) == eAnalyzedStatus::kNoMipMaps
					) == false)
				;
		}

		u32 analyze_status_result_flags = 0;
		/// @brief only filename with extension
		string_path filename;
		/// @brief no filename
		string_path subpath;
		/// @brief full absolute path to file with extension
		string_path path;
	};

	// todo: serialize to binary and read before init...
	struct SUserSettings
	{
		bool show_invalid_first = false;
		bool show_only_dds_and_thm = false;
		bool treat_nomipmap_as_invalid = false;
		bool treat_notpowerof2dimensions_as_invalid = false;
		bool treat_nothasthm_as_invalid = false;
	};

	// todo: if it can be generalized then remove from CImGuiTextureEditor scope and make it 'publically' accessible
	struct SImGuiWindowState
	{
		bool canApply = false;
		bool    wasDocked = false;
		bool    isCentralNode = false;

		ImVec2  pos = { FLT_MAX, FLT_MAX };
		ImVec2  size = { 0,0 };
		ImGuiDir dockDir = ImGuiDir_None;

		void Capture(const char* windowName);
		void Apply(const char* windowName);
	};

	bool is_init = false;
	// written on wt side
	bool is_all_analyzed = false;
	bool is_settings_read = false;
	bool is_settings_write = false;

	bool is_filter_processing = false;

	bool is_selected_metadata_loaded = false;
	bool is_selected_thm_data_loaded = false;
	bool is_selected_preview_loaded = false;

	bool is_settings_applied = false;

	bool is_metadata_tooltip_loaded = false;
	bool is_preview_tooltip_image_loaded = false;
	bool is_preview_tooltip_image_load_started = false;
	u8 search_frame_count = 0;

	RHITextureMetadata selected_metadata;
	RHITextureMetadata tooltip_metadata;
	SImGuiWindowState last_window_selected_state;
	u32 selected_index = u32(-1);
	u32 hovered_tooltip_index = u32(-1);
	IRHISurface* pTexturePreview = nullptr;
	IRHIShaderResourceView* pTexturePreviewSRV = nullptr;
	IRHISurface* pTextureSelected = nullptr;
	IRHIShaderResourceView* pTextureSelectedSRV = nullptr;
	STextureParams* pTHMSelected = nullptr;

	std::atomic<u32> current_analyzed_count = 0;
	std::atomic<u32> total_textures_in_folder = 0;
	std::atomic<u32> total_thm_in_folder = 0;
	std::atomic<u32> total_unable_to_classify_files_in_folder = 0;
	std::atomic<u32> total_seq_in_folder = 0;
	std::atomic<u32> total_png_in_folder = 0;
	std::atomic<u32> total_svg_in_folder = 0;
	std::atomic<u32> total_bmp_in_folder = 0;
	std::atomic<u32> total_ogm_in_folder = 0;
	std::atomic<u32> total_ini_in_folder = 0;
	std::atomic<u32> total_other_in_folder = 0;
	std::atomic<u32> total_files_in_folder = 0;

	std::atomic<u32> valid_count = 0;
	u32 invalid_by_filenamelength = 0;
	u32 invalid_by_thm = 0;

	SUserSettings settings;

	std::string_view wt_current_analyzing_texture;
	std::string_view current_tooltip_texture_filename;
	std::string path_to_texture_folder;

	xr_concurrent_vector<STextureEntry> textures;
	xr_vector<u32> filter_query;

	string_path window_selected_name;
	string_path search_input_buffer;
};

constexpr float kGeneralAlphaLevelForImGuiWindows = 0.5f;

enum class eImGuiEditorType : u32
{
	kTextureEditor,
	kOMFEditor,
	kQuestEditor,
	kPPEEditor,
	kNoEditor,
	kInvalid = u32(-1)
};

enum class eRequestType_TextureEditor : u32
{
	kReadSettings,
	kWriteSettings,
	kReadAll,
	kLoadTooltipPreview,
	kLoadTooltipMetadata,
	kLoadMetadataOfSelected,
	kLoadPreviewOfSelected,
	kLoadTHMOfSelected,
	kDeselectCurrentSelected,
	kFilterQuery,
	kShutdown
};

enum class eRequestType_QuestEditor : u32
{
	kReadSettings,
	kWriteSettings,
	kLoadCurrentQuests,
	kDeselectCurrentSelectedOrHideWindow,
	kShutdown
};

enum class eRequestType_OMFEditor : u32
{
	kReadSettings,
	kWriteSettings,
	kLoadFile,
	kDeselectCurrentSelectedOrHideWindow,
	kShutdown
};

enum class eRequestType_PPEditor : u32
{
	kReadSettings,
	kWriteSettings,
	kLoadFile,
	kLoadTexturePreview,
	kDeselectCurrentSelectedOrHideWindow,
	kShutdown
};

struct SRequestData
{
	u32 editor_type = static_cast<u32>(eImGuiEditorType::kInvalid);
	u32 request_type = 0;
	u32 payload = 0;
};

struct CImGuiRequestManager
{
	xr_task_group requests;
};

#define IXRAY_MAX_IMGUI_REQUESTS_COUNT 8


// todo: implement viewer
/// @brief \~english if enabled then you will have in-game viewer but right now it is not implemented feature
#define IXRAY_OMF_EDITOR_ENABLE_VIEWER 0
#define IXRAY_OMF_EDITOR_TAB_GAME 1
#define IXRAY_OMF_EDITOR_TAB_EDITOR 1

/// @brief \~english if enabled 'bone renaming' section won't exist and you can directly rename from bone list  otherwise you have to rename only through 'bone renaming' section
#define IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING 1
#define IXRAY_OMF_EDITOR_TAB_HELP 1

#define IXRAY_PPE_EDITOR_TAB_GAME 1
#define IXRAY_PPE_EDITOR_TAB_EDITOR 1
#define IXRAY_PPE_EDITOR_TAB_HELP 1

// todo: implement viewer
/// @brief \~english if enabled the editor tab of the PPE editor gets a 'Preview' column for a curve viewer (not implemented yet)
#define IXRAY_PPE_EDITOR_PREVIEW 0

// shared message box helper for in-game editors (implementation in ImUtils.cpp)
enum class _eMessageBoxStatus
{
	kSuccess,
	kWarning,
	kError,
	kYesOrNo
};

int ShowMessageBox(_eMessageBoxStatus status, std::string_view title, std::string_view message);

// shared help-manual helpers for in-game editors
inline void ImGuiEditorUI_HelpBullet(const char* text)
{
	ImGui::Bullet();
	ImGui::TextWrapped("%s", text);
}

inline void ImGuiEditorUI_HelpSection(const char* name, const char* description)
{
	if (ImGui::CollapsingHeader(name))
	{
		ImGui::TextWrapped("%s", description);
	}
}

/* INIT */
void InitSections();
void CollectCars();
void InitImGuiCLSIDInGame();
void InitImGuiSearchInGame();
void InitImGuiHudAdjustInGame();
void InitImGuiInGameInputReceiver();
void InitImGuiQuestEditor();

/* RENDER */
void RenderTimeManagerWindow();
void RenderSpawnManagerWindow();
void RenderWeaponManagerWindow();
void RenderSearchManagerWindow();
void RenderHUDAdjustManager();
void RenderToolsOMFEditorWindow();
void RenderCarConfigEditor();
void RenderToolsInputManagerWindow();
void RenderToolsRenderDebugSVGStorageViewerWindow();
void RenderTextureEditor();
void RenderQuestEditor();
void RenderPPEEditor();
void Render3rdAdjust();
void RenderDemoRecordEditorWindow();

/* MISCELLANEOUS */

void DestroySpawnManagerWindow();

/* WORKER THREAD of Tools */

// note1: real and only one worker thread is named as AllEditorsAndTools_WorkerThread

// note2: other functions need to use prefix _WorkerThread only to identify threaded execution of these function, so the difference between real function that used in std::thread and others is that others accept ime_request_t alias for handling incoming request from worker thread

// note3: each editor (if it is needed) defines somewhere (preferably in their own class/struct definitions) a request enum that will describe tasks that their own workload need to handle (see TextureEditor implementation as example)
void AllEditors_SendRequest(const SRequestData& req);
void AllEditors_ExecuteRequest(const SRequestData& req);

void AllEditors_OnPressed(int key);
void AllEditors_OnReleased(int key);

void TextureEditor_OnPressed(int key);
void TextureEditor_OnReleased(int key);

void SpawnManager_OnPressed(int key);
void SpawnManager_OnReleased(int key);

void QuestEditor_OnPressed(int key);
void QuestEditor_OnReleased(int key);

void OMFEditor_OnPressed(int key);
void OMFEditor_OnReleased(int key);

void PPEEditor_OnPressed(int key);
void PPEEditor_OnReleased(int key);

void RequestHandler_TextureEditor(const SRequestData& req);
void RequestHandler_QuestEditor(const SRequestData& req);
void RequestHandler_OMFEditor(const SRequestData& req);
void RequestHandler_PPEEditor(const SRequestData& req);

void RegisterImGuiInGame();
void execute_console_command_deferred(CConsole* c, const char* string_to_execute);

extern clsid_manager* g_pClsidManager;
extern CImGuiGameSearchManager imgui_search_manager;
extern CHudAdjustManager imgui_hud_adjust_manager;
extern CImGuiTextureEditor g_imgui_texture_editor;
extern CImGuiRequestManager g_imgui_editor_request_manager;

template<typename T, std::size_t N>
IC void AllEditors_SendRequests_Sequential(const xr_array<T, N>& reqs)
{
	g_imgui_editor_request_manager.requests.run([reqs]()
		{
			for (const SRequestData& req : reqs)
			{
				AllEditors_ExecuteRequest(req);
			}
		});
}

void AllEditors_Shutdown();