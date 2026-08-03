#include "StdAfx.h"
#include "../Actor.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../Weapon.h"
#include "../ui/UIInventoryUtilities.h"
#include "ImUtils.h"
#include "../WeaponKnife.h"

enum
{
	inv_cost = 0,
	ammo_mag_size,
	inv_weight,
	fire_distance,
	bullet_speed,
	rpm,
	hit_impulse,
	upgrade_disp_base,
	upgrade_disp_vel_factor,
	upgrade_disp_accel_factor,
	upgrade_disp_crouch,
	upgrade_disp_crouch_no_acc,
	fire_dispersion_condition_factor,
	fire_dispersion_base,
	control_inertion_factor,
	crosshair_inertion,
	cam_return,
	cam_relax_speed,
	cam_relax_speed_ai,
	cam_dispersion,
	cam_dispersion_inc,
	cam_dispersion_frac,
	cam_max_angle,
	cam_max_angle_horz,
	cam_step_angle_horz,
	hit_power,
	hit_power_critical,
	inv_scale,
	inv_grid_x,
	inv_grid_y,
	inv_grid_width,
	inv_grid_height,
	silencer_x,
	silencer_y,
	
	hit_impulse_2,
	hit_power_2,
	hit_power_critical_2,
	splash1_direction,
	splash2_direction,
	splash1_dist,
	splash2_dist,
	splash1_radius,
	splash2_radius,
	splash1_hits_count,
	splash1_pervictim_hcount,
	splash2_hits_count,
	splash_hit_divide_factor,

	max_count
};

struct
{
	bool init{};

	bool can_show[max_count]{};

	bool modal_icon_selection_window{};

	u16 weapon_id{ u16(-1) };

	int current_slot{ NO_ACTIVE_SLOT };

	int inv_cost{};
	int cfg_inv_cost{};

	int ammo_mag_size{};
	int cfg_ammo_mag_size{};

	float inv_weight{};
	float cfg_inv_weight{};

	float fire_distance{};
	float cfg_fire_distance{};

	float bullet_speed{};
	float cfg_bullet_speed{};

	float rpm{};
	float cfg_rpm{};

	float hit_impulse{};
	float cfg_hit_impulse{};

	float upgrade_disp_base{};
	float cfg_upgrade_disp_base{};

	float upgrade_disp_vel_factor{};
	float cfg_upgrade_disp_vel_factor{};

	float upgrade_disp_accel_factor{};
	float cfg_upgrade_disp_accel_factor{};

	float upgrade_disp_crouch{};
	float cfg_upgrade_disp_crouch{};

	float upgrade_disp_crouch_no_acc{};
	float cfg_upgrade_disp_crouch_no_acc{};

	float fire_dispersion_condition_factor{};
	float cfg_fire_dispersion_condition_factor{};

	float fire_dispersion_base{};
	float cfg_fire_dispersion_base{};

	float control_inertion_factor{};
	float cfg_control_inertion_factor{};

	float crosshair_inertion{};
	float cfg_crosshair_inertion{};

	float cam_return{};
	float cfg_cam_return{};

	float cam_relax_speed{};
	float cfg_cam_relax_speed{};

	float cam_relax_speed_ai{};
	float cfg_cam_relax_speed_ai{};

	float cam_dispersion{};
	float cfg_cam_dispersion{};

	float cam_dispersion_inc{};
	float cfg_cam_dispersion_inc{};

	float cam_dispersion_frac{};
	float cfg_cam_dispersion_frac{};

	float cam_max_angle{};
	float cfg_cam_max_angle{};

	float cam_max_angle_horz{};
	float cfg_cam_max_angle_horz{};

	float cam_step_angle_horz{};
	float cfg_cam_step_angle_horz{};

	float inv_scale {};
	float cfg_inv_scale {};

	int inv_grid_width{};
	u32 cfg_inv_grid_width{};

	int inv_grid_height{};
	u32 cfg_inv_grid_height{};

	int inv_grid_x{};
	u32 cfg_inv_grid_x{};

	int inv_grid_y{};
	u32 cfg_inv_grid_y{};

	int silencer_x{};
	int cfg_silencer_x{};

	int silencer_y{};
	int cfg_silencer_y{};

	size_t icons_count{};

	Fvector4 hit_power;
	Fvector4 cfg_hit_power;

	Fvector4 hit_power_critical;
	Fvector4 cfg_hit_power_critical;

	float hit_impulse_2{};
	float cfg_hit_impulse_2{};

	Fvector4 hit_power_2;
	Fvector4 cfg_hit_power_2;

	Fvector4 hit_power_critical_2;
	Fvector4 cfg_hit_power_critical_2;

	Fvector3 splash1_direction;
	Fvector3 cfg_splash1_direction;

	Fvector3 splash2_direction;
	Fvector3 cfg_splash2_direction;

	float splash1_dist{};
	float cfg_splash1_dist{};

	float splash2_dist{};
	float cfg_splash2_dist{};

	float splash1_radius{};
	float cfg_splash1_radius{};

	float splash2_radius{};
	float cfg_splash2_radius{};

	int splash1_hits_count{};
	u32 cfg_splash1_hits_count{};

	int splash1_pervictim_hcount{};
	u32 cfg_splash1_pervictim_hcount{};

	int splash2_hits_count{};
	u32 cfg_splash2_hits_count{};

	float splash_hit_divide_factor{};
	float cfg_splash_hit_divide_factor{};

	IRender_interface::SurfaceParams ui_icons;

	struct WeaponIcon
	{
		float inv_scale{};
		u32 inv_grid_x{};
		u32 inv_grid_y{};
		u32 inv_grid_width{};
		u32 inv_grid_height{};
		const char* p_section_name{};
	};

	// id for string table
	string128 inv_name{};
	// id for string table
	string128 inv_short_name{};


	WeaponIcon icons[1024]{};
	void Reset()
	{
		for (int i = 0; i < max_count; i++)
		{
			can_show[i] = false;
		}
	}
}
imgui_weapon_manager;


void RenderWeaponManagerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WeaponManager)])
	{
		return;
	}

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	auto draw_item = [](CInventoryItem* pItem, int slot_type) {
		if (pItem)
		{
			CShootingObject* pSO = dynamic_cast<CShootingObject*>(pItem);
			CWeapon* pWeapon = pItem->cast_weapon();
			CWeaponKnife* pKnife = pItem->cast_weapon_knife();

			if (imgui_weapon_manager.current_slot != slot_type)
				imgui_weapon_manager.init = false;

			if (imgui_weapon_manager.weapon_id != pItem->object_id())
				imgui_weapon_manager.init = false;

			if (Render)
			{
				const auto surface = READ_IF_EXISTS(pSettings, r_string, pItem->m_section_id, "icons_texture", "ui\\ui_icon_equipment");
				imgui_weapon_manager.ui_icons = Render->getSurface(surface);
			}

			if (!imgui_weapon_manager.init)
			{
				// clear bool flags that line_exist checking for correct init/uninit cycle
				imgui_weapon_manager.Reset();

				imgui_weapon_manager.current_slot = slot_type;
				imgui_weapon_manager.weapon_id = pItem->object_id();

				imgui_weapon_manager.inv_cost = pItem->Cost();
				imgui_weapon_manager.inv_weight = pItem->Weight();
				imgui_weapon_manager.control_inertion_factor = pItem->GetControlInertionFactor();
				imgui_weapon_manager.inv_scale = READ_IF_EXISTS(pSettings, r_float, pItem->m_section_id, "inv_scale", 1.0f);;
				imgui_weapon_manager.inv_grid_x = pItem->GetInvGridRect().x1;
				imgui_weapon_manager.inv_grid_y = pItem->GetInvGridRect().y1;
				imgui_weapon_manager.inv_grid_width = pItem->GetInvGridRect().x2;
				imgui_weapon_manager.inv_grid_height = pItem->GetInvGridRect().y2;

				if (pSO)
				{
					imgui_weapon_manager.hit_impulse = pSO->getHitImpulse();
					imgui_weapon_manager.hit_power = pSO->getHitPower();
					imgui_weapon_manager.hit_power_critical = pSO->getHitPowerCritical();
				}
				if (pKnife)
				{
					imgui_weapon_manager.splash1_direction = pKnife->GetHit1SplashDir();
					imgui_weapon_manager.splash2_direction = pKnife->GetHit2SplashDir();
					imgui_weapon_manager.splash1_dist = pKnife->GetHit1Dist();
					imgui_weapon_manager.splash2_dist = pKnife->GetHit2Dist();
					imgui_weapon_manager.splash1_radius = pKnife->GetHit1SplashRadius();
					imgui_weapon_manager.splash2_radius = pKnife->GetHit2SplashRadius();
					imgui_weapon_manager.splash1_hits_count = pKnife->GetSplash1HitsCount();
					imgui_weapon_manager.splash1_pervictim_hcount = pKnife->GetSplash1PerVictimsHCount();
					imgui_weapon_manager.splash2_hits_count = pKnife->GetSplash2HitsCount();
					imgui_weapon_manager.splash_hit_divide_factor = pKnife->GetNextHitDivideFactor();
				}
				if (pSO && !pKnife)
				{
					imgui_weapon_manager.fire_distance = pSO->getFireDistance();
					imgui_weapon_manager.bullet_speed = pSO->getStartBulletSpeed();
					imgui_weapon_manager.rpm = pSO->getRPM();
					imgui_weapon_manager.fire_dispersion_base = pSO->getFireDispersionBase();
				}

				if (pWeapon && !pKnife)
				{
					imgui_weapon_manager.ammo_mag_size = pWeapon->GetAmmoMagSize();
					imgui_weapon_manager.crosshair_inertion = pWeapon->GetCrosshairInertion();
					imgui_weapon_manager.upgrade_disp_base = pWeapon->Get_PDM_Base();
					imgui_weapon_manager.upgrade_disp_vel_factor = pWeapon->Get_PDM_Vel_F();
					imgui_weapon_manager.upgrade_disp_accel_factor = pWeapon->Get_PDM_Accel_F();
					imgui_weapon_manager.upgrade_disp_crouch = pWeapon->Get_PDM_Crouch();
					imgui_weapon_manager.upgrade_disp_crouch_no_acc = pWeapon->Get_PDM_Crouch_NA();
					imgui_weapon_manager.fire_dispersion_condition_factor = pWeapon->getFireDispersionConditionFactor();
					imgui_weapon_manager.silencer_x = pWeapon->GetSilencerX();
					imgui_weapon_manager.silencer_y = pWeapon->GetSilencerY();
				}

				// icons
				imgui_weapon_manager.icons_count = 0;
				ZeroMemory(imgui_weapon_manager.icons, sizeof(imgui_weapon_manager.icons));
				CInifile::Root& sections = pSettings->sections();
				for (CInifile::Sect& pSection : sections)
				{
					// todo: temp because of korzyna need to replace to g_pClsidManager
					xr_string_view name = pSection.Name.c_str();
					if (!name.empty())
					{
						size_t index = name.find("wpn_");
						if (index != xr_string_view::npos && index == 0)
						{
							if (pSection.line_exist("inv_grid_x") && pSection.line_exist("inv_grid_y") && pSection.line_exist("inv_grid_width") && pSection.line_exist("inv_grid_height"))
							{
								auto& icon = imgui_weapon_manager.icons[imgui_weapon_manager.icons_count];
								icon.inv_scale = READ_IF_EXISTS(pSettings, r_float, name.data(), "inv_scale", 1.0f);
								icon.inv_grid_x = pSettings->r_u32(name.data(), "inv_grid_x");
								icon.inv_grid_y = pSettings->r_u32(name.data(), "inv_grid_y");
								icon.inv_grid_width = pSettings->r_u32(name.data(), "inv_grid_width");
								icon.inv_grid_height = pSettings->r_u32(name.data(), "inv_grid_height");
								icon.p_section_name = name.data();
								++imgui_weapon_manager.icons_count;
							}
						}
					}
				}

				// defaults
				if (pSettings)
				{
					if (pSettings->section_exist(pItem->m_section_id.c_str()))
					{
						const char* pSectionName = pItem->m_section_id.c_str();

						if (pSettings->line_exist(pSectionName, "cost"))
						{
							imgui_weapon_manager.can_show[inv_cost] = true;
							imgui_weapon_manager.cfg_inv_cost = pSettings->r_u32(pSectionName, "cost");
						}

						if (pSettings->line_exist(pSectionName, "inv_weight"))
						{
							imgui_weapon_manager.can_show[inv_weight] = true;
							imgui_weapon_manager.cfg_inv_weight = pSettings->r_float(pSectionName, "inv_weight");
						}

						if (pSettings->line_exist(pSectionName, "fire_distance"))
						{
							imgui_weapon_manager.can_show[fire_distance] = true;
							imgui_weapon_manager.cfg_fire_distance = pSettings->r_float(pSectionName, "fire_distance");
						}

						if (pSettings->line_exist(pSectionName, "bullet_speed"))
						{
							imgui_weapon_manager.can_show[bullet_speed] = true;
							imgui_weapon_manager.cfg_bullet_speed = pSettings->r_float(pSectionName, "bullet_speed");
						}

						if (pSettings->line_exist(pSectionName, "rpm"))
						{
							imgui_weapon_manager.can_show[rpm] = true;
							imgui_weapon_manager.cfg_rpm = pSettings->r_float(pSectionName, "rpm");
							imgui_weapon_manager.cfg_rpm = 60.0f / imgui_weapon_manager.cfg_rpm;
						}

						if (pSettings->line_exist(pSectionName, "hit_impulse"))
						{
							imgui_weapon_manager.can_show[hit_impulse] = true;
							imgui_weapon_manager.cfg_hit_impulse = pSettings->r_float(pSectionName, "hit_impulse");
						}

						if (pSettings->line_exist(pSectionName, "hit_power"))
						{
							imgui_weapon_manager.can_show[hit_power] = true;
							imgui_weapon_manager.cfg_hit_power = pSettings->r_fvector4(pSectionName, "hit_power");
						}

						if (pSettings->line_exist(pSectionName, "hit_power_critical"))
						{
							imgui_weapon_manager.can_show[hit_power_critical] = true;
							imgui_weapon_manager.cfg_hit_power_critical = pSettings->r_fvector4(pSectionName, "hit_power_critical");
						}
						
						if (pSettings->line_exist(pSectionName, "hit_impulse_2"))
						{
							imgui_weapon_manager.can_show[hit_impulse_2] = true;
							imgui_weapon_manager.cfg_hit_impulse_2 = pSettings->r_float(pSectionName, "hit_impulse_2");
						}

						if (pSettings->line_exist(pSectionName, "hit_power_2"))
						{
							imgui_weapon_manager.can_show[hit_power_2] = true;
							imgui_weapon_manager.cfg_hit_power_2 = pSettings->r_fvector4(pSectionName, "hit_power_2");
						}

						if (pSettings->line_exist(pSectionName, "hit_power_critical_2"))
						{
							imgui_weapon_manager.can_show[hit_power_critical_2] = true;
							imgui_weapon_manager.cfg_hit_power_critical_2 = pSettings->r_fvector4(pSectionName, "hit_power_critical_2");
						}

						if (pSettings->line_exist(pSectionName, "splash1_direction"))
						{
							imgui_weapon_manager.can_show[splash1_direction] = true;
							imgui_weapon_manager.cfg_splash1_direction = pSettings->r_fvector3(pSectionName, "splash1_direction");
						}

						if (pSettings->line_exist(pSectionName, "splash2_direction"))
						{
							imgui_weapon_manager.can_show[splash2_direction] = true;
							imgui_weapon_manager.cfg_splash2_direction = pSettings->r_fvector3(pSectionName, "splash2_direction");
						}

						// DO NOT FIX TYPOS!!!!!! THOSE ARE INTENTIONAL
						if (pSettings->line_exist(pSectionName, "spash1_dist"))
						{
							imgui_weapon_manager.can_show[splash1_dist] = true;
							imgui_weapon_manager.cfg_splash1_dist = pSettings->r_float(pSectionName, "spash1_dist");
						}

						if (pSettings->line_exist(pSectionName, "spash2_dist"))
						{
							imgui_weapon_manager.can_show[splash2_dist] = true;
							imgui_weapon_manager.cfg_splash2_dist = pSettings->r_float(pSectionName, "spash2_dist");
						}

						if (pSettings->line_exist(pSectionName, "spash1_radius"))
						{
							imgui_weapon_manager.can_show[splash1_radius] = true;
							imgui_weapon_manager.cfg_splash1_radius = pSettings->r_float(pSectionName, "spash1_radius");
						}

						if (pSettings->line_exist(pSectionName, "spash2_radius"))
						{
							imgui_weapon_manager.can_show[splash2_radius] = true;
							imgui_weapon_manager.cfg_splash2_radius = pSettings->r_float(pSectionName, "spash2_radius");
						}

						if (pSettings->line_exist(pSectionName, "splash1_hits_count"))
						{
							imgui_weapon_manager.can_show[splash1_hits_count] = true;
							imgui_weapon_manager.cfg_splash1_hits_count = pSettings->r_u32(pSectionName, "splash1_hits_count");
						}

						if (pSettings->line_exist(pSectionName, "splash1_pervictim_hcount"))
						{
							imgui_weapon_manager.can_show[splash1_pervictim_hcount] = true;
							imgui_weapon_manager.cfg_splash1_pervictim_hcount = pSettings->r_u32(pSectionName, "splash1_pervictim_hcount");
						}

						if (pSettings->line_exist(pSectionName, "splash2_hits_count"))
						{
							imgui_weapon_manager.can_show[splash2_hits_count] = true;
							imgui_weapon_manager.cfg_splash2_hits_count = pSettings->r_u32(pSectionName, "splash2_hits_count");
						}

						if (pSettings->line_exist(pSectionName, "splash_hit_divide_factor"))
						{
							imgui_weapon_manager.can_show[splash_hit_divide_factor] = true;
							imgui_weapon_manager.cfg_splash_hit_divide_factor = pSettings->r_float(pSectionName, "splash_hit_divide_factor");
						}

						if (pSettings->line_exist(pSectionName, "ammo_mag_size"))
						{
							imgui_weapon_manager.can_show[ammo_mag_size] = true;
							imgui_weapon_manager.cfg_ammo_mag_size = pSettings->r_u32(pSectionName, "ammo_mag_size");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_accel_factor"))
						{
							imgui_weapon_manager.can_show[upgrade_disp_accel_factor] = true;
							imgui_weapon_manager.cfg_upgrade_disp_accel_factor = pSettings->r_float(pSectionName, "PDM_disp_accel_factor");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_base"))
						{
							imgui_weapon_manager.can_show[upgrade_disp_base] = true;
							imgui_weapon_manager.cfg_upgrade_disp_base = pSettings->r_float(pSectionName, "PDM_disp_base");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_crouch"))
						{
							imgui_weapon_manager.can_show[upgrade_disp_crouch] = true;
							imgui_weapon_manager.cfg_upgrade_disp_crouch = pSettings->r_float(pSectionName, "PDM_disp_crouch");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_crouch_no_acc"))
						{
							imgui_weapon_manager.can_show[upgrade_disp_crouch_no_acc] = true;
							imgui_weapon_manager.cfg_upgrade_disp_crouch_no_acc = pSettings->r_float(pSectionName, "PDM_disp_crouch_no_acc");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_vel_factor"))
						{
							imgui_weapon_manager.can_show[upgrade_disp_vel_factor] = true;
							imgui_weapon_manager.cfg_upgrade_disp_vel_factor = pSettings->r_float(pSectionName, "PDM_disp_vel_factor");
						}

						if (pSettings->line_exist(pSectionName, "cam_return"))
						{
							imgui_weapon_manager.can_show[cam_return] = true;
							imgui_weapon_manager.cfg_cam_return = pSettings->r_float(pSectionName, "cam_return");
						}

						if (pSettings->line_exist(pSectionName, "cam_relax_speed"))
						{
							imgui_weapon_manager.can_show[cam_relax_speed] = true;
							imgui_weapon_manager.cfg_cam_relax_speed = pSettings->r_float(pSectionName, "cam_relax_speed");
						}

						if (pSettings->line_exist(pSectionName, "cam_relax_speed_ai"))
						{
							imgui_weapon_manager.can_show[cam_relax_speed_ai] = true;
							imgui_weapon_manager.cfg_cam_relax_speed_ai = pSettings->r_float(pSectionName, "cam_relax_speed_ai");
						}

						if (pSettings->line_exist(pSectionName, "cam_dispersion"))
						{
							imgui_weapon_manager.can_show[cam_dispersion] = true;
							imgui_weapon_manager.cfg_cam_dispersion = pSettings->r_float(pSectionName, "cam_dispersion");
						}

						if (pSettings->line_exist(pSectionName, "cam_dispersion_inc"))
						{
							imgui_weapon_manager.can_show[cam_dispersion_inc] = true;
							imgui_weapon_manager.cfg_cam_dispersion_inc = pSettings->r_float(pSectionName, "cam_dispersion_inc");
						}

						if (pSettings->line_exist(pSectionName, "cam_dispersion_frac"))
						{
							imgui_weapon_manager.can_show[cam_dispersion_frac] = true;
							imgui_weapon_manager.cfg_cam_dispersion_frac = pSettings->r_float(pSectionName, "cam_dispersion_frac");
						}

						if (pSettings->line_exist(pSectionName, "cam_max_angle"))
						{
							imgui_weapon_manager.can_show[cam_max_angle] = true;
							imgui_weapon_manager.cfg_cam_max_angle = pSettings->r_float(pSectionName, "cam_max_angle");
						}

						if (pSettings->line_exist(pSectionName, "cam_max_angle_horz"))
						{
							imgui_weapon_manager.can_show[cam_max_angle_horz] = true;
							imgui_weapon_manager.cfg_cam_max_angle_horz = pSettings->r_float(pSectionName, "cam_max_angle_horz");
						}

						if (pSettings->line_exist(pSectionName, "cam_step_angle_horz"))
						{
							imgui_weapon_manager.can_show[cam_step_angle_horz] = true;
							imgui_weapon_manager.cfg_cam_step_angle_horz = pSettings->r_float(pSectionName, "cam_step_angle_horz");
						}

						if (pSettings->line_exist(pSectionName, "fire_dispersion_base"))
						{
							imgui_weapon_manager.can_show[fire_dispersion_base] = true;
							imgui_weapon_manager.cfg_fire_dispersion_base = pSettings->r_float(pSectionName, "fire_dispersion_base");
						}

						if (pSettings->line_exist(pSectionName, "control_inertion_factor"))
						{
							imgui_weapon_manager.can_show[control_inertion_factor] = true;
							imgui_weapon_manager.cfg_control_inertion_factor = pSettings->r_float(pSectionName, "control_inertion_factor");
						}

						if (pSettings->line_exist(pSectionName, "crosshair_inertion"))
						{
							imgui_weapon_manager.can_show[crosshair_inertion] = true;
							imgui_weapon_manager.cfg_crosshair_inertion = pSettings->r_float(pSectionName, "crosshair_inertion");
						}

						if (pSettings->line_exist(pSectionName, "fire_dispersion_condition_factor"))
						{
							imgui_weapon_manager.can_show[fire_dispersion_condition_factor] = true;
							imgui_weapon_manager.cfg_fire_dispersion_condition_factor = pSettings->r_float(pSectionName, "fire_dispersion_condition_factor");
						}

						if (pSettings->line_exist(pSectionName, "inv_scale"))
						{
							imgui_weapon_manager.can_show[inv_scale] = true;
							imgui_weapon_manager.cfg_inv_scale = pSettings->r_u32(pSectionName, "inv_scale");
						}
						else
						{
							imgui_weapon_manager.cfg_inv_scale = 1.0f;
						}

						if (pSettings->line_exist(pSectionName, "inv_grid_x"))
						{
							imgui_weapon_manager.can_show[inv_grid_x] = true;
							imgui_weapon_manager.cfg_inv_grid_x = pSettings->r_u32(pSectionName, "inv_grid_x");
						}

						if (pSettings->line_exist(pSectionName, "inv_grid_y"))
						{
							imgui_weapon_manager.can_show[inv_grid_y] = true;
							imgui_weapon_manager.cfg_inv_grid_y = pSettings->r_u32(pSectionName, "inv_grid_y");
						}

						if (pSettings->line_exist(pSectionName, "inv_grid_width"))
						{
							imgui_weapon_manager.can_show[inv_grid_width] = true;
							imgui_weapon_manager.cfg_inv_grid_width = pSettings->r_u32(pSectionName, "inv_grid_width");
						}

						if (pSettings->line_exist(pSectionName, "inv_grid_height"))
						{
							imgui_weapon_manager.can_show[inv_grid_height] = true;
							imgui_weapon_manager.cfg_inv_grid_height = pSettings->r_u32(pSectionName, "inv_grid_height");
						}

						if (pSettings->line_exist(pSectionName, "silencer_x"))
						{
							if (pWeapon)
							{
								imgui_weapon_manager.can_show[silencer_x] = true;
								imgui_weapon_manager.cfg_silencer_x = pSettings->r_s32(pSectionName, "silencer_x");
							}
						}

						if (pSettings->line_exist(pSectionName, "silencer_y"))
						{
							if (pWeapon)
							{
								imgui_weapon_manager.can_show[silencer_y] = true;
								imgui_weapon_manager.cfg_silencer_y = pSettings->r_s32(pSectionName, "silencer_y");
							}
						}
					}
				}

				imgui_weapon_manager.init = true;
			}

			if (ImGui::CollapsingHeader("Information"))
			{
				ImGui::Text("section name: [%s]", pItem->m_section_id.c_str());

				if (ImGui::TreeNode("Inventory"))
				{
					ImGui::Text("Cost: %d", pItem->Cost());
					ImGui::Text("Weight: %f", pItem->Weight());
					ImGui::Text("Name: [%s]", Platform::ANSI_TO_UTF8(pItem->NameItem()).c_str());
					ImGui::Text("Short name: [%s]", Platform::ANSI_TO_UTF8(pItem->NameShort()).c_str());
					ImGui::TextWrapped("Description: [%s]", Platform::ANSI_TO_UTF8(pItem->ItemDescription().c_str()).c_str());

					if (ImGui::TreeNode("Icon"))
					{
						ImGui::Text("Grid X: %d", pItem->GetInvGridRect().x1);
						ImGui::Text("Grid Y: %d", pItem->GetInvGridRect().y1);
						ImGui::Text("Grid Width: %d", pItem->GetInvGridRect().x2);
						ImGui::Text("Grid Height: %d", pItem->GetInvGridRect().y2);

						if (imgui_weapon_manager.ui_icons.Surface != nullptr)
						{
							float scaleIcon = imgui_weapon_manager.inv_scale;
							float x = imgui_weapon_manager.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
							float y = imgui_weapon_manager.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
							float w = imgui_weapon_manager.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
							float h = imgui_weapon_manager.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);

							ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.f, 0.f, 0.f, 0.f));
							ImGui::PushStyleColor(ImGuiCol_ButtonActive, ImVec4(0.f, 0.f, 0.f, 0.f));
							ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImVec4(0.f, 0.f, 0.f, 0.f));

							ImGui::ImageButton("WeaponIconInWeaponManager", imgui_weapon_manager.ui_icons.Surface, { w,h }, { x / imgui_weapon_manager.ui_icons.w, y / imgui_weapon_manager.ui_icons.h }, { (x + w) / imgui_weapon_manager.ui_icons.w, (y + h) / imgui_weapon_manager.ui_icons.h });

							ImGui::PopStyleColor(3);
						}

						if (ImGui::TreeNode("Addons"))
						{
							if (pWeapon)
							{
								ImGui::Text("Silencer X: %d", pWeapon->GetSilencerX());
								ImGui::Text("Silencer Y: %d", pWeapon->GetSilencerY());
							}
							ImGui::TreePop();
						}
						ImGui::TreePop();
					}
					ImGui::TreePop();
				}

				if (pSO && !pKnife)
				{
					if (ImGui::TreeNode("Ballistic"))
					{
						ImGui::Text("Fire distance: %.4f", pSO->getFireDistance());
						ImGui::Text("Bullet speed: %.4f", pSO->getStartBulletSpeed());
						ImGui::Text("RPM: %.4f", pSO->getRPM());
						ImGui::TreePop();
					}
				}
				if (pSO)
				{
					if (ImGui::TreeNode("Hit"))
					{
						ImGui::Text("Hit impulse: %.4f", pSO->getHitImpulse());
						const auto& hit_power = pSO->getHitPower();
						ImGui::Text("Hit power: %.4f %.4f %.4f %.4f", hit_power.x, hit_power.y, hit_power.z, hit_power.w);
						const auto& hit_power_critical = pSO->getHitPowerCritical();
						ImGui::Text("Hit power critical: %.4f %.4f %.4f %.4f", hit_power_critical.x, hit_power_critical.y, hit_power_critical.z, hit_power_critical.w);
						if (pKnife)
						{
							ImGui::Text("Hit impulse 2: %.4f", pKnife->getHitImpulse_2());
							const auto& hit_power_2 = pKnife->getHitPower_2();
							ImGui::Text("Hit power 2: %.4f %.4f %.4f %.4f", hit_power_2.x, hit_power_2.y, hit_power_2.z, hit_power_2.w);
							const auto& hit_power_critical_2 = pKnife->getHitPowerCritical_2();
							ImGui::Text("Hit power critical 2: %.4f %.4f %.4f %.4f", hit_power_critical_2.x, hit_power_critical_2.y, hit_power_critical_2.z, hit_power_critical_2.w);
						}
						ImGui::TreePop();
					}
				}

				if (pWeapon && pSO && !pKnife)
				{
					if (ImGui::TreeNode("Ammunition"))
					{
						ImGui::Text("Magazine size: %d", pWeapon->GetAmmoMagSize());

						xr_string ammos = "Ammo: ";
						for (const auto& str : pWeapon->getAmmoTypes())
						{
							ammos += str.c_str();
							ammos += ',';
						}

						ammos.erase(ammos.rfind(','));

						ImGui::TextWrapped(ammos.c_str());
						ImGui::TreePop();
					}

					if (ImGui::TreeNode("Dispersion"))
					{
						ImGui::Text("Fire dispersion base: %.4f", pSO->getFireDispersionBase());
						ImGui::Text("Control inertion factor: %.4f", pItem->GetControlInertionFactor());
						ImGui::Text("Crosshair inertion: %.4f", pWeapon->GetCrosshairInertion());
						ImGui::Text("Upgrade dispersion base: %.4f", pWeapon->Get_PDM_Base());
						ImGui::Text("Upgrade dispersion velocity factor: %.4f", pWeapon->Get_PDM_Vel_F());
						ImGui::Text("Upgrade dispersion acceleration factor: %.4f", pWeapon->Get_PDM_Accel_F());
						ImGui::Text("Upgrade dispersion crouch: %.4f", pWeapon->Get_PDM_Crouch());
						ImGui::Text("Upgrade dispersion crouch no acceleration: %.4f", pWeapon->Get_PDM_Crouch_NA());
						ImGui::Text("Dispersion factor when weapon is damaged/broken: %.4f", pWeapon->getFireDispersionConditionFactor());
						ImGui::TreePop();
					}

					if (ImGui::TreeNode("Recoil"))
					{
						const auto& cam_recoil = pWeapon->getCameraRecoil();
						ImGui::Text("Camera return: %s", cam_recoil.ReturnMode ? "enabled" : "disabled");
						ImGui::Text("Camera relax speed: %.4f", cam_recoil.RelaxSpeed);
						ImGui::Text("Camera relax speed ai: %.4f", cam_recoil.RelaxSpeed_AI);
						ImGui::Text("Camera dispersion: %.4f", cam_recoil.Dispersion);
						ImGui::Text("Camera dispersion inc: %.4f", cam_recoil.DispersionInc);
						ImGui::Text("Camera dispersion frac: %.4f", cam_recoil.DispersionFrac);
						if (ImGui::BeginItemTooltip())
						{
							ImGui::SetItemTooltip("Where gun will be pointed that described by law cam_dispersion*cam_dispersion_frac +- cam_dispersion*(1-cam_dispersion_frac)");
							ImGui::EndTooltip();
						}

						ImGui::Text("Camera max angle vertical: %.4f", cam_recoil.MaxAngleVert);
						ImGui::Text("Camera max angle horizontal: %.4f", cam_recoil.MaxAngleHorz);
						ImGui::Text("Camera step angle horizontal: %.4f", cam_recoil.StepAngleHorz);

						ImGui::SeparatorText("Zoom");
						const auto& zoom_cam_recoil = pWeapon->getCameraZoomRecoil();
						ImGui::Text("Zoom camera relax speed: %.4f", zoom_cam_recoil.RelaxSpeed);
						ImGui::Text("Zoom camera relax speed ai: %.4f", zoom_cam_recoil.RelaxSpeed_AI);
						ImGui::Text("Zoom cam dispersion: %.4f", zoom_cam_recoil.Dispersion);
						ImGui::Text("Zoom cam dispersion inc: %.4f", zoom_cam_recoil.DispersionInc);
						ImGui::Text("Zoom cam dispersion frac: %.4f", zoom_cam_recoil.DispersionFrac);
						if (ImGui::BeginItemTooltip())
						{
							ImGui::SetItemTooltip("Where gun will be pointed that described by law cam_dispersion*cam_dispersion_frac +- cam_dispersion*(1-cam_dispersion_frac)");
							ImGui::EndTooltip();
						}
						ImGui::Text("Zoom cam max angle vertical: %.4f", zoom_cam_recoil.MaxAngleVert);
						ImGui::Text("Zoom cam max angle horizontal: %.4f", zoom_cam_recoil.MaxAngleHorz);
						ImGui::Text("Zoom step angle horizontal: %.4f", zoom_cam_recoil.StepAngleHorz);
						ImGui::TreePop();
					}
				}
				if (pKnife)
				{
					if (ImGui::TreeNode("Knife params"))
					{
						ImGui::Text("Splash direction 1: %.4f %.4f %4.f", pKnife->GetHit1SplashDir().x, pKnife->GetHit1SplashDir().y, pKnife->GetHit1SplashDir().z);
						ImGui::Text("Splash distance 1: %.4f", pKnife->GetHit1Dist());
						ImGui::Text("Splash radius 1: %.4f", pKnife->GetHit1SplashRadius());
						ImGui::Text("Splash hits count 1: %u", pKnife->GetSplash1HitsCount());
						ImGui::Text("Splash hits per victim: %u", pKnife->GetSplash1PerVictimsHCount());
						ImGui::Text("Splash direction 2: %.4f %.4f %4.f", pKnife->GetHit2SplashDir().x, pKnife->GetHit2SplashDir().y, pKnife->GetHit2SplashDir().z);
						ImGui::Text("Splash distance 2: %.4f", pKnife->GetHit2Dist());
						ImGui::Text("Splash radius 2: %.4f", pKnife->GetHit2SplashRadius());
						ImGui::Text("Splash hits count 2: %u", pKnife->GetSplash2HitsCount());
						ImGui::Text("Splash next hit divide factor: %.4f", pKnife->GetNextHitDivideFactor());
						ImGui::TreePop();
					}
				}
			}

			if (imgui_weapon_manager.init)
			{
				constexpr const char* pModalIconSelectionName = "Select Icon...##Editing";

				if (imgui_weapon_manager.modal_icon_selection_window && !ImGui::IsPopupOpen(pModalIconSelectionName))
					ImGui::OpenPopup(pModalIconSelectionName);

				if (ImGui::BeginPopupModal(pModalIconSelectionName, &imgui_weapon_manager.modal_icon_selection_window, ImGuiWindowFlags_AlwaysAutoResize))
				{
					constexpr int kWeaponManagerTableColumnSize = 5;

					static_assert(kWeaponManagerTableColumnSize > 0 && "specify positive number");

					if (ImGui::BeginTable("icons##Editing_WeaponManager", kWeaponManagerTableColumnSize, ImGuiTableFlags_Borders))
					{
						int row_max = std::ceil(imgui_weapon_manager.icons_count / kWeaponManagerTableColumnSize);
						R_ASSERT(row_max > 0 && "something is wrong");

						for (int row = 0; row < row_max; ++row)
						{
							ImGui::TableNextRow();
							for (int column = 0; column < kWeaponManagerTableColumnSize; ++column)
							{
								int current_icon_index = row * kWeaponManagerTableColumnSize + column;

								// overflow, but we need to round up for iterating through all items, imagine size of 213 elements and our column is equal to 5 so rows are 213/5=42.6 => roundup() = 43 but 43 * 5 = 215 and then it is 213 leading to overflow... This is okay :))
								if (current_icon_index < imgui_weapon_manager.icons_count)
								{
									ImGui::TableSetColumnIndex(column);
									const auto& icon = imgui_weapon_manager.icons[current_icon_index];
									float scaleIcon = icon.inv_scale;
									float x = icon.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
									float y = icon.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
									float w = icon.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
									float h = icon.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);

									string64 button_name{};
									xr_sprintf(button_name, sizeof(button_name), "%s%d", "WeaponIconButton_", current_icon_index);

									bool is_pressed_icon = ImGui::ImageButton(button_name, imgui_weapon_manager.ui_icons.Surface, { w,h }, { x / imgui_weapon_manager.ui_icons.w, y / imgui_weapon_manager.ui_icons.h }, { (x + w) / imgui_weapon_manager.ui_icons.w, (y + h) / imgui_weapon_manager.ui_icons.h });

									if (ImGui::BeginItemTooltip())
									{
										if (icon.p_section_name)
										{
											ImGui::Text("[%s]", icon.p_section_name);
										}
										ImGui::Text("Grid X: %d", icon.inv_grid_x);
										ImGui::Text("Grid Y: %d", icon.inv_grid_y);
										ImGui::Text("Grid Width: %d", icon.inv_grid_width);
										ImGui::Text("Grid Height: %d", icon.inv_grid_height);

										ImGui::EndTooltip();
									}

									if (is_pressed_icon)
									{
										imgui_weapon_manager.inv_grid_x = icon.inv_grid_x;
										imgui_weapon_manager.inv_grid_y = icon.inv_grid_y;
										imgui_weapon_manager.inv_grid_width = icon.inv_grid_width;
										imgui_weapon_manager.inv_grid_height = icon.inv_grid_height;

										if (pItem)
										{
											pItem->SetInvGridRect(icon.inv_grid_x, icon.inv_grid_y, icon.inv_grid_width, icon.inv_grid_height);
										}

										imgui_weapon_manager.modal_icon_selection_window = false;
										ImGui::CloseCurrentPopup();
									}
								}
							}
						}

						ImGui::EndTable();
					}

					ImGui::EndPopup();
				}

				if (ImGui::CollapsingHeader("Editing"))
				{
					ImGui::Text("section name: [%s]", pItem->m_section_id.c_str());
					if (ImGui::Button("Reset to defaults"))
					{
						imgui_weapon_manager.inv_cost = imgui_weapon_manager.cfg_inv_cost;
						imgui_weapon_manager.inv_weight = imgui_weapon_manager.cfg_inv_weight;
						imgui_weapon_manager.ammo_mag_size = imgui_weapon_manager.cfg_ammo_mag_size;
						imgui_weapon_manager.fire_distance = imgui_weapon_manager.cfg_fire_distance;
						imgui_weapon_manager.bullet_speed = imgui_weapon_manager.cfg_bullet_speed;
						imgui_weapon_manager.rpm = imgui_weapon_manager.cfg_rpm;
						imgui_weapon_manager.hit_impulse = imgui_weapon_manager.cfg_hit_impulse;
						imgui_weapon_manager.hit_power = imgui_weapon_manager.hit_power;
						imgui_weapon_manager.hit_power_critical = imgui_weapon_manager.hit_power_critical;
						imgui_weapon_manager.upgrade_disp_accel_factor = imgui_weapon_manager.cfg_upgrade_disp_accel_factor;
						imgui_weapon_manager.upgrade_disp_base = imgui_weapon_manager.cfg_upgrade_disp_base;
						imgui_weapon_manager.upgrade_disp_crouch = imgui_weapon_manager.cfg_upgrade_disp_crouch;
						imgui_weapon_manager.upgrade_disp_crouch_no_acc = imgui_weapon_manager.cfg_upgrade_disp_crouch_no_acc;
						imgui_weapon_manager.upgrade_disp_vel_factor = imgui_weapon_manager.cfg_upgrade_disp_vel_factor;
						imgui_weapon_manager.fire_dispersion_condition_factor = imgui_weapon_manager.cfg_fire_dispersion_condition_factor;
						imgui_weapon_manager.inv_scale = imgui_weapon_manager.cfg_inv_scale;
						imgui_weapon_manager.inv_grid_height = imgui_weapon_manager.cfg_inv_grid_height;
						imgui_weapon_manager.inv_grid_width = imgui_weapon_manager.cfg_inv_grid_width;
						imgui_weapon_manager.inv_grid_x = imgui_weapon_manager.cfg_inv_grid_x;
						imgui_weapon_manager.inv_grid_y = imgui_weapon_manager.cfg_inv_grid_y;
						imgui_weapon_manager.silencer_x = imgui_weapon_manager.cfg_silencer_x;
						imgui_weapon_manager.silencer_y = imgui_weapon_manager.cfg_silencer_y;
						imgui_weapon_manager.splash1_direction = imgui_weapon_manager.cfg_splash1_direction;
						imgui_weapon_manager.splash2_direction = imgui_weapon_manager.cfg_splash2_direction;
						imgui_weapon_manager.splash1_dist = imgui_weapon_manager.cfg_splash1_dist;
						imgui_weapon_manager.splash2_dist = imgui_weapon_manager.cfg_splash2_dist;
						imgui_weapon_manager.splash1_radius = imgui_weapon_manager.cfg_splash1_radius;
						imgui_weapon_manager.splash2_radius = imgui_weapon_manager.cfg_splash2_radius;
						imgui_weapon_manager.splash1_hits_count = imgui_weapon_manager.cfg_splash1_hits_count;
						imgui_weapon_manager.splash1_pervictim_hcount = imgui_weapon_manager.cfg_splash1_pervictim_hcount;
						imgui_weapon_manager.splash2_hits_count = imgui_weapon_manager.cfg_splash1_hits_count;
						imgui_weapon_manager.splash_hit_divide_factor = imgui_weapon_manager.cfg_splash_hit_divide_factor;

						if (pItem)
						{
							pItem->setCost(imgui_weapon_manager.inv_cost);
							pItem->setWeight(imgui_weapon_manager.inv_weight);
							pItem->SetInvGridRect(imgui_weapon_manager.inv_grid_x, imgui_weapon_manager.inv_grid_y, imgui_weapon_manager.inv_grid_width, imgui_weapon_manager.inv_grid_height);
						}

						if (pSO && !pKnife)
						{
							pSO->setFireDistance(imgui_weapon_manager.fire_distance);
							pSO->setStartBulletSpeed(imgui_weapon_manager.bullet_speed);
							pSO->setRPM(imgui_weapon_manager.rpm);
							pSO->setHitImpulse(imgui_weapon_manager.hit_impulse);
							pSO->setHitPower(imgui_weapon_manager.hit_power);
							pSO->setHitPowerCritical(imgui_weapon_manager.hit_power_critical);
						}
						if (pKnife)
						{
							pKnife->SetHit1SplashDir(imgui_weapon_manager.splash1_direction);
							pKnife->SetHit1Dist(imgui_weapon_manager.splash1_dist);
							pKnife->SetHit1SplashRadius(imgui_weapon_manager.splash1_radius);
							pKnife->SetSplash1HitsCount(imgui_weapon_manager.splash1_hits_count);
							pKnife->SetSplash1PerVictimsHCount(imgui_weapon_manager.splash1_pervictim_hcount);
							pKnife->SetHit2SplashDir(imgui_weapon_manager.splash2_direction);
							pKnife->SetHit2Dist(imgui_weapon_manager.splash2_dist);
							pKnife->SetHit2SplashRadius(imgui_weapon_manager.splash2_radius);
							pKnife->SetSplash2HitsCount(imgui_weapon_manager.splash2_hits_count);
							pKnife->SetNextHitDivideFactor(imgui_weapon_manager.splash_hit_divide_factor);
						}
						if (pWeapon && !pKnife)
						{
							pWeapon->SetAmmoMagSize(imgui_weapon_manager.ammo_mag_size);

							pWeapon->Set_PDM_Base(imgui_weapon_manager.upgrade_disp_base);
							pWeapon->Set_PDM_Accel_F(imgui_weapon_manager.upgrade_disp_accel_factor);
							pWeapon->Set_PDM_Crouch(imgui_weapon_manager.upgrade_disp_crouch);
							pWeapon->Set_PDM_Crouch_NA(imgui_weapon_manager.upgrade_disp_crouch_no_acc);
							pWeapon->Set_PDM_Vel_F(imgui_weapon_manager.upgrade_disp_vel_factor);

							pWeapon->setFireDispersionConditionFactor(imgui_weapon_manager.fire_dispersion_condition_factor);
						}
					}

					// remove this button for now
					/*
					ImGui::SameLine();

					if (ImGui::Button("Save"))
					{

					}*/
					ImGuiSliderFlags flags = ImGuiSliderFlags_AlwaysClamp | ImGuiSliderFlags_Logarithmic;

					if (ImGui::TreeNode("Inventory##Editing"))
					{
						if (imgui_weapon_manager.can_show[inv_cost])
						{
							if (ImGui::SliderInt("Cost##Editing", &imgui_weapon_manager.inv_cost, 0, 100000, "%d", flags))
							{
								pItem->setCost(imgui_weapon_manager.inv_cost);
							}
						}

						if (imgui_weapon_manager.can_show[inv_weight])
						{
							if (ImGui::SliderFloat("Weight##Editing", &imgui_weapon_manager.inv_weight, 0.0f, 100000.0f, "%.3f", flags))
							{
								pItem->setWeight(imgui_weapon_manager.inv_weight);
							}
						}

						if (ImGui::TreeNode("Icon##Editing"))
						{
							if (imgui_weapon_manager.can_show[inv_grid_height] && imgui_weapon_manager.can_show[inv_grid_width] && imgui_weapon_manager.can_show[inv_grid_x] && imgui_weapon_manager.can_show[inv_grid_y])
							{
								if (ImGui::SliderInt("Grid X##Editing", &imgui_weapon_manager.inv_grid_x, 0, 16000, "%d", flags))
								{
									const Irect& rect = pItem->GetInvGridRect();
									pItem->SetInvGridRect(imgui_weapon_manager.inv_grid_x, rect.y1, rect.x2, rect.y2);
								}

								if (ImGui::SliderInt("Grid Y##Editing", &imgui_weapon_manager.inv_grid_y, 0, 16000, "%d", flags))
								{
									const Irect& rect = pItem->GetInvGridRect();
									pItem->SetInvGridRect(rect.x1, imgui_weapon_manager.inv_grid_y, rect.x2, rect.y2);
								}

								if (ImGui::SliderInt("Grid Width##Editing", &imgui_weapon_manager.inv_grid_width, 0, 16000, "%d", flags))
								{
									const Irect& rect = pItem->GetInvGridRect();
									pItem->SetInvGridRect(rect.x1, rect.y1, imgui_weapon_manager.inv_grid_width, rect.y2);
								}

								if (ImGui::SliderInt("Grid Height##Editing", &imgui_weapon_manager.inv_grid_height, 0, 16000, "%d", flags))
								{
									const Irect& rect = pItem->GetInvGridRect();
									pItem->SetInvGridRect(rect.x1, rect.y1, rect.x2, imgui_weapon_manager.inv_grid_height);
								}

								if (imgui_weapon_manager.ui_icons.Surface)
								{
									float scaleIcon = imgui_weapon_manager.inv_scale;
									float x = imgui_weapon_manager.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
									float y = imgui_weapon_manager.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
									float w = imgui_weapon_manager.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
									float h = imgui_weapon_manager.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);

									bool is_pressed = ImGui::ImageButton("WeaponIconInWeaponManager##Editing", imgui_weapon_manager.ui_icons.Surface, { w,h }, { x / imgui_weapon_manager.ui_icons.w, y / imgui_weapon_manager.ui_icons.h }, { (x + w) / imgui_weapon_manager.ui_icons.w, (y + h) / imgui_weapon_manager.ui_icons.h });

									if (is_pressed)
									{
										imgui_weapon_manager.modal_icon_selection_window = true;
									}
								}
							}

							if (ImGui::TreeNode("Addons"))
							{
								if (imgui_weapon_manager.can_show[silencer_x])
								{
									if (ImGui::SliderInt("Silencer X##Editing", &imgui_weapon_manager.silencer_x, -4096, 4096, "%d", flags))
									{
										if (pWeapon && !pKnife)
										{
											pWeapon->SetSilencerX(imgui_weapon_manager.silencer_x);
										}
									}
								}
								if (imgui_weapon_manager.can_show[silencer_y])
								{
									if (ImGui::SliderInt("Silencer Y##Editing", &imgui_weapon_manager.silencer_y, -4096, 4096, "%d", flags))
									{
										if (pWeapon && !pKnife)
										{
											pWeapon->SetSilencerY(imgui_weapon_manager.silencer_y);
										}
									}
								}

								ImGui::TreePop();
							}

							ImGui::TreePop();
						}


						ImGui::TreePop();
					}

					if (pSO && !pKnife)
					{
						if (ImGui::TreeNode("Ballistic##Editing"))
						{
							if (imgui_weapon_manager.can_show[fire_distance])
							{
								if (ImGui::SliderFloat("Fire distance##Editing", &imgui_weapon_manager.fire_distance, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setFireDistance(imgui_weapon_manager.fire_distance);
								}
							}

							if (imgui_weapon_manager.can_show[bullet_speed])
							{
								if (ImGui::SliderFloat("Bullet speed##Editing", &imgui_weapon_manager.bullet_speed, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setStartBulletSpeed(imgui_weapon_manager.bullet_speed);
								}
							}

							if (imgui_weapon_manager.can_show[rpm])
							{
								if (ImGui::SliderFloat("RPM##Editing", &imgui_weapon_manager.rpm, 0.001f, 100.0f, "%.3f", flags))
								{
									pSO->setRPM(imgui_weapon_manager.rpm);
								}
							}

							ImGui::TreePop();
						}
					}
					if (pSO)
					{
						if (ImGui::TreeNode("Hit##Editing"))
						{
							if (imgui_weapon_manager.can_show[hit_impulse])
							{
								if (ImGui::SliderFloat("Hit impulse##Editing", &imgui_weapon_manager.hit_impulse, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setHitImpulse(imgui_weapon_manager.hit_impulse);
								}
							}

							if (imgui_weapon_manager.can_show[hit_power])
							{
								if (ImGui::SliderFloat4("Hit power##Editing", &imgui_weapon_manager.hit_power.x, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setHitPower(imgui_weapon_manager.hit_power);
								}
							}

							if (imgui_weapon_manager.can_show[hit_power_critical])
							{
								if (ImGui::SliderFloat4("Hit power critical##Editing", &imgui_weapon_manager.hit_power_critical.x, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setHitPowerCritical(imgui_weapon_manager.hit_power_critical);
								}
							}

							if (pKnife)
							{
								if (imgui_weapon_manager.can_show[hit_impulse_2])
								{
									if (ImGui::SliderFloat("Hit impulse 2##Editing", &imgui_weapon_manager.hit_impulse_2, 0.0f, 10000.0f, "%.3f", flags))
									{
										pKnife->setHitImpulse_2(imgui_weapon_manager.hit_impulse_2);
									}
								}

								if (imgui_weapon_manager.can_show[hit_power_2])
								{
									if (ImGui::SliderFloat4("Hit power 2##Editing", &imgui_weapon_manager.hit_power_2.x, 0.0f, 10000.0f, "%.3f", flags))
									{
										pKnife->setHitPower_2(imgui_weapon_manager.hit_power_2);
									}
								}

								if (imgui_weapon_manager.can_show[hit_power_critical_2])
								{
									if (ImGui::SliderFloat4("Hit power critical 2##Editing", &imgui_weapon_manager.hit_power_critical_2.x, 0.0f, 10000.0f, "%.3f", flags))
									{
										pKnife->setHitPowerCritical_2(imgui_weapon_manager.hit_power_critical_2);
									}
								}
							}
							ImGui::TreePop();
						}
					}

					if (pWeapon && !pKnife)
					{
						if (ImGui::TreeNode("Ammunition##Editing"))
						{
							if (imgui_weapon_manager.can_show[ammo_mag_size])
							{
								if (ImGui::SliderInt("Magazine size##Editing", &imgui_weapon_manager.ammo_mag_size, 0, 10000, "%d", flags))
								{
									pWeapon->SetAmmoMagSize(imgui_weapon_manager.ammo_mag_size);
								}
							}

							// todo: implement changing calibers and calibers section names
							// add dropdown menu with parsed all ammos that game supports

							ImGui::TreePop();
						}

						if (ImGui::TreeNode("Dispersion##Editing"))
						{
							if (imgui_weapon_manager.can_show[fire_dispersion_base])
							{
								if (ImGui::SliderFloat("Fire dispersion base", &imgui_weapon_manager.fire_dispersion_base, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->setFireDispersionBase(imgui_weapon_manager.fire_dispersion_base);
								}
							}

							if (imgui_weapon_manager.can_show[control_inertion_factor])
							{
								if (ImGui::SliderFloat("Control inertion factor", &imgui_weapon_manager.control_inertion_factor, 0.0f, 100.0f, "%.3f", flags))
								{
									pItem->setControlInertionFactor(imgui_weapon_manager.control_inertion_factor);
								}
							}

							if (imgui_weapon_manager.can_show[crosshair_inertion])
							{
								if (ImGui::SliderFloat("Crosshair inertion", &imgui_weapon_manager.crosshair_inertion, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->setCrosshairInertion(imgui_weapon_manager.crosshair_inertion);
								}
							}

							if (imgui_weapon_manager.upgrade_disp_base)
							{
								if (ImGui::SliderFloat("Upgrade dispersion base", &imgui_weapon_manager.upgrade_disp_base, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->Set_PDM_Base(imgui_weapon_manager.upgrade_disp_base);
								}
							}

							if (imgui_weapon_manager.can_show[upgrade_disp_vel_factor])
							{
								if (ImGui::SliderFloat("Upgrade dispersion velocity factor", &imgui_weapon_manager.upgrade_disp_vel_factor, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->Set_PDM_Vel_F(imgui_weapon_manager.upgrade_disp_vel_factor);
								}
							}

							if (imgui_weapon_manager.can_show[upgrade_disp_accel_factor])
							{
								if (ImGui::SliderFloat("Upgrade dispersion acceleration factor", &imgui_weapon_manager.upgrade_disp_accel_factor, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->Set_PDM_Accel_F(imgui_weapon_manager.upgrade_disp_accel_factor);
								}
							}

							if (imgui_weapon_manager.can_show[upgrade_disp_crouch])
							{
								if (ImGui::SliderFloat("Upgrade dispersion crouch", &imgui_weapon_manager.upgrade_disp_crouch, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->Set_PDM_Crouch(imgui_weapon_manager.upgrade_disp_crouch);
								}
							}

							if (imgui_weapon_manager.cfg_upgrade_disp_crouch_no_acc)
							{
								if (ImGui::SliderFloat("Upgrade dispersion crouch no acceleration", &imgui_weapon_manager.upgrade_disp_crouch_no_acc, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->Set_PDM_Crouch_NA(imgui_weapon_manager.upgrade_disp_crouch_no_acc);
								}
							}

							if (imgui_weapon_manager.can_show[fire_dispersion_condition_factor])
							{
								if (ImGui::SliderFloat("Dispersion factor when weapon is damaged/broken", &imgui_weapon_manager.fire_dispersion_condition_factor, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->setFireDispersionConditionFactor(imgui_weapon_manager.fire_dispersion_condition_factor);
								}
							}

							ImGui::TreePop();
						}
					}
					if (pKnife)
					{
						if (ImGui::TreeNode("Knife params##Editing"))
						{
							if (imgui_weapon_manager.can_show[splash1_direction])
							{
								if (ImGui::SliderFloat3("Splash direction 1##Editing", &imgui_weapon_manager.splash1_direction.x, 0.0f, 100.0f, "%.3f", flags))
								{
									pKnife->SetHit1SplashDir(imgui_weapon_manager.splash1_direction);
								}
							}
							if (imgui_weapon_manager.can_show[splash1_dist])
							{
								if (ImGui::SliderFloat("Splash distance 1##Editing", &imgui_weapon_manager.splash1_dist, 0.0f, 1000.0f, "%.3f", flags))
								{
									pKnife->SetHit1Dist(imgui_weapon_manager.splash1_dist);
								}
							}
							if (imgui_weapon_manager.can_show[splash1_radius])
							{
								if (ImGui::SliderFloat("Splash radius 1##Editing", &imgui_weapon_manager.splash1_radius, 0.0f, 1000.0f, "%.3f", flags))
								{
									pKnife->SetHit1SplashRadius(imgui_weapon_manager.splash1_radius);
								}
							}
							if (imgui_weapon_manager.can_show[splash1_hits_count])
							{
								if (ImGui::SliderInt("Splash hits count 1##Editing", &imgui_weapon_manager.splash1_hits_count, 0, 1000, "%d", flags))
								{
									pKnife->SetSplash1HitsCount(imgui_weapon_manager.splash1_hits_count);
								}
							}
							if (imgui_weapon_manager.can_show[splash1_pervictim_hcount])
							{
								if (ImGui::SliderInt("Splash hits per victim##Editing", &imgui_weapon_manager.splash1_pervictim_hcount, 0, 1000, "%d", flags))
								{
									pKnife->SetSplash1PerVictimsHCount(imgui_weapon_manager.splash1_pervictim_hcount);
								}
							}
							if (imgui_weapon_manager.can_show[splash2_direction])
							{
								if (ImGui::SliderFloat3("Splash direction 2##Editing", &imgui_weapon_manager.splash2_direction.x, 0.0f, 100.0f, "%.3f", flags))
								{
									pKnife->SetHit2SplashDir(imgui_weapon_manager.splash2_direction);
								}
							}
							if (imgui_weapon_manager.can_show[splash2_dist])
							{
								if (ImGui::SliderFloat("Splash distance 2##Editing", &imgui_weapon_manager.splash2_dist, 0.0f, 1000.0f, "%.3f", flags))
								{
									pKnife->SetHit2Dist(imgui_weapon_manager.splash2_dist);
								}
							}
							if (imgui_weapon_manager.can_show[splash2_radius])
							{
								if (ImGui::SliderFloat("Splash radius 2##Editing", &imgui_weapon_manager.splash2_radius, 0.0f, 1000.0f, "%.3f", flags))
								{
									pKnife->SetHit2SplashRadius(imgui_weapon_manager.splash2_radius);
								}
							}
							if (imgui_weapon_manager.can_show[splash2_hits_count])
							{
								if (ImGui::SliderInt("Splash hits count 2##Editing", &imgui_weapon_manager.splash2_hits_count, 0, 1000, "%d", flags))
								{
									pKnife->SetSplash2HitsCount(imgui_weapon_manager.splash2_hits_count);
								}
							}
							if (imgui_weapon_manager.can_show[splash_hit_divide_factor])
							{
								if (ImGui::SliderFloat("Splash hit divide factor##Editing", &imgui_weapon_manager.splash_hit_divide_factor, 0.0f, 1000.0f, "%.3f", flags))
								{
									pKnife->SetNextHitDivideFactor(imgui_weapon_manager.splash_hit_divide_factor);
								}
							}
							ImGui::TreePop();
						}
					}
				}
			}
		}
		};

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));
	if (ImGui::Begin("Weapon Manager", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WeaponManager)]))
	{
		if (ImGui::BeginTabBar("##TB_InGameWeaponManager"))
		{
			CActor* pActor = Level().CurrentEntity() != nullptr ? Level().CurrentEntity()->cast_actor() : nullptr;

			xr_string slot1_tab_name{ "Knife (KNIFE_SLOT) - " };
			xr_string slot2_tab_name{ "Slot 2 (INV_SLOT_2) - " };
			xr_string slot3_tab_name{ "Slot 3 (INV_SLOT_3) - " };
			xr_string slot4_tab_name{ "Pistol (PISTOL_SLOT_NEW) - " };

			if (pActor)
			{
				CInventoryItem* pItemInSlot1 = pActor->inventory().ItemFromSlot(KNIFE_SLOT);
				CInventoryItem* pItemInSlot2 = pActor->inventory().ItemFromSlot(INV_SLOT_2);
				CInventoryItem* pItemInSlot3 = pActor->inventory().ItemFromSlot(INV_SLOT_3);
				CInventoryItem* pItemInSlot4 = pActor->inventory().ItemFromSlot(PISTOL_SLOT_NEW);

				if (pItemInSlot1)
				{
					slot1_tab_name += pItemInSlot1->m_section_id.c_str();
				}
				slot1_tab_name += "##TB_InGameWeaponManager";

				if (pItemInSlot2)
				{
					slot2_tab_name += pItemInSlot2->m_section_id.c_str();
				}
				slot2_tab_name += "##TB_InGameWeaponManager";

				if (pItemInSlot3)
				{
					slot3_tab_name += pItemInSlot3->m_section_id.c_str();
				}
				slot3_tab_name += "##TB_InGameWeaponManager";

				if (pItemInSlot4)
				{
					slot4_tab_name += pItemInSlot4->m_section_id.c_str();
				}
				slot4_tab_name += "##TB_InGameWeaponManager";
			}

			if (ImGui::BeginTabItem(slot1_tab_name.c_str()))
			{
				CInventoryItem* pItem = pActor->inventory().ItemFromSlot(KNIFE_SLOT);
				draw_item(pItem, KNIFE_SLOT);

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem(slot2_tab_name.c_str()))
			{
				CInventoryItem* pItem = pActor->inventory().ItemFromSlot(INV_SLOT_2);
				draw_item(pItem, INV_SLOT_2);

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem(slot3_tab_name.c_str()))
			{
				CInventoryItem* pItem = pActor->inventory().ItemFromSlot(INV_SLOT_3);
				draw_item(pItem, INV_SLOT_3);

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem(slot4_tab_name.c_str()))
			{
				CInventoryItem* pItem = pActor->inventory().ItemFromSlot(PISTOL_SLOT_NEW);
				draw_item(pItem, PISTOL_SLOT_NEW);

				ImGui::EndTabItem();
			}

			ImGui::EndTabBar();
		}
	}

	ImGui::End();
	ImGui::PopStyleColor(1);
}
