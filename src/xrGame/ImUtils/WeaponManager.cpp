#include "stdafx.h"
#include "../Actor.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../Weapon.h"
#include "../ui/UIInventoryUtilities.h"

struct
{
	bool init{};

	bool can_show_inv_cost{};
	bool can_show_ammo_mag_size{};
	bool can_show_inv_weight{};
	bool can_show_fire_distance{};
	bool can_show_bullet_speed{};
	bool can_show_rpm{};
	bool can_show_hit_impulse{};
	bool can_show_upgrade_disp_base{};
	bool can_show_upgrade_disp_vel_factor{};
	bool can_show_upgrade_disp_accel_factor{};
	bool can_show_upgrade_disp_crouch{};
	bool can_show_upgrade_disp_crouch_no_acc{};
	bool can_show_fire_dispersion_condition_factor{};
	bool can_show_fire_dispersion_base{};
	bool can_show_control_inertion_factor{};
	bool can_show_crosshair_inertion{};
	bool can_show_cam_return{};
	bool can_show_cam_relax_speed{};
	bool can_show_cam_relax_speed_ai{};
	bool can_show_cam_dispersion{};
	bool can_show_cam_dispersion_inc{};
	bool can_show_cam_dispersion_frac{};
	bool can_show_cam_max_angle{};
	bool can_show_cam_max_angle_horz{};
	bool can_show_cam_step_angle_horz{};
	bool can_show_hit_power{};
	bool can_show_hit_power_critical{};
	bool can_show_inv_grid_x{};
	bool can_show_inv_grid_y{};
	bool can_show_inv_grid_width{};
	bool can_show_inv_grid_height{};
	bool can_show_silencer_x{};
	bool can_show_silencer_y{};

	bool can_show_modal_icon_selection_window{};

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

	IRender_interface::SurfaceParams ui_icons;

	struct WeaponIcon
	{
		u32 inv_grid_x{};
		u32 inv_grid_y{};
		u32 inv_grid_width{};
		u32 inv_grid_height{};
		const char* p_section_name{};
	};

	// id for string table
	char inv_name[128]{};
	// id for string table
	char inv_short_name[128]{};


	WeaponIcon icons[1024]{};
}
imgui_weapon_manager;


void RenderWeaponManagerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WeaponManager)])
		return;

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	auto draw_item = [](CInventoryItem* pItem, int slot_type) {
		if (pItem)
		{
			CShootingObject* pSO = dynamic_cast<CShootingObject*>(pItem);
			CWeapon* pWeapon = dynamic_cast<CWeapon*>(pItem);

			if (imgui_weapon_manager.current_slot != slot_type)
				imgui_weapon_manager.init = false;

			if (imgui_weapon_manager.weapon_id != pItem->object_id())
				imgui_weapon_manager.init = false;

			if (!imgui_weapon_manager.init)
			{
				// clear bool flags that line_exist checking for correct init/uninit cycle
				memset((&imgui_weapon_manager.init + sizeof(imgui_weapon_manager.init)), 0, 33);

				imgui_weapon_manager.current_slot = slot_type;
				imgui_weapon_manager.weapon_id = pItem->object_id();

				imgui_weapon_manager.inv_cost = pItem->Cost();
				imgui_weapon_manager.inv_weight = pItem->Weight();
				imgui_weapon_manager.control_inertion_factor = pItem->GetControlInertionFactor();
				imgui_weapon_manager.inv_grid_x = pItem->GetInvGridRect().x1;
				imgui_weapon_manager.inv_grid_y = pItem->GetInvGridRect().y1;
				imgui_weapon_manager.inv_grid_width = pItem->GetInvGridRect().x2;
				imgui_weapon_manager.inv_grid_height = pItem->GetInvGridRect().y2;
				
				if (Render)
				{
					imgui_weapon_manager.ui_icons = Render->getSurface("ui\\ui_icon_equipment");
				}

				if (pSO)
				{
					imgui_weapon_manager.fire_distance = pSO->getFireDistance();
					imgui_weapon_manager.bullet_speed = pSO->getStartBulletSpeed();
					imgui_weapon_manager.rpm = pSO->getRPM();
					imgui_weapon_manager.hit_impulse = pSO->getHitImpulse();
					imgui_weapon_manager.hit_power = pSO->getHitPower();
					imgui_weapon_manager.hit_power_critical = pSO->getHitPowerCritical();
					imgui_weapon_manager.fire_dispersion_base = pSO->getFireDispersionBase();
				}

				if (pWeapon)
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
				memset(imgui_weapon_manager.icons, 0, sizeof(imgui_weapon_manager.icons));

				for (const auto& pSection : pSettings->sections())
				{
					if (pSection)
					{
						// todo: temp because of korzyna need to replace to g_pClsidManager

						std::string_view name = pSection->Name.c_str();

						if (!name.empty())
						{
							size_t index = name.find("wpn_");
							if (index != std::string_view::npos && index == 0)
							{
								if (pSection->line_exist("inv_grid_x") && pSection->line_exist("inv_grid_y") && pSection->line_exist("inv_grid_width") && pSection->line_exist("inv_grid_height"))
								{
									auto& icon = imgui_weapon_manager.icons[imgui_weapon_manager.icons_count];
									icon.inv_grid_x = pSettings->r_u32(pSection->Name, "inv_grid_x");
									icon.inv_grid_y = pSettings->r_u32(pSection->Name, "inv_grid_y");
									icon.inv_grid_width = pSettings->r_u32(pSection->Name, "inv_grid_width");
									icon.inv_grid_height = pSettings->r_u32(pSection->Name, "inv_grid_height");
									icon.p_section_name = pSection->Name.c_str();
									++imgui_weapon_manager.icons_count;
								}
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
							imgui_weapon_manager.can_show_inv_cost = true;
							imgui_weapon_manager.cfg_inv_cost = pSettings->r_u32(pSectionName, "cost");
						}

						if (pSettings->line_exist(pSectionName, "inv_weight"))
						{
							imgui_weapon_manager.can_show_inv_weight = true;
							imgui_weapon_manager.cfg_inv_weight = pSettings->r_float(pSectionName, "inv_weight");
						}

						if (pSettings->line_exist(pSectionName, "fire_distance"))
						{
							imgui_weapon_manager.can_show_fire_distance = true;
							imgui_weapon_manager.cfg_fire_distance = pSettings->r_float(pSectionName, "fire_distance");
						}

						if (pSettings->line_exist(pSectionName, "bullet_speed"))
						{
							imgui_weapon_manager.can_show_bullet_speed = true;
							imgui_weapon_manager.cfg_bullet_speed = pSettings->r_float(pSectionName, "bullet_speed");
						}

						if (pSettings->line_exist(pSectionName, "rpm"))
						{
							imgui_weapon_manager.can_show_rpm = true;
							imgui_weapon_manager.cfg_rpm = pSettings->r_float(pSectionName, "rpm");
							imgui_weapon_manager.cfg_rpm = 60.0f / imgui_weapon_manager.cfg_rpm;
						}

						if (pSettings->line_exist(pSectionName, "hit_impulse"))
						{
							imgui_weapon_manager.can_show_hit_impulse = true;
							imgui_weapon_manager.cfg_hit_impulse = pSettings->r_float(pSectionName, "hit_impulse");
						}

						if (pSettings->line_exist(pSectionName, "hit_power"))
						{
							imgui_weapon_manager.can_show_hit_power = true;
							imgui_weapon_manager.cfg_hit_power = pSettings->r_fvector4(pSectionName, "hit_power");
						}

						if (pSettings->line_exist(pSectionName, "hit_power_critical"))
						{
							imgui_weapon_manager.can_show_hit_power_critical = true;
							imgui_weapon_manager.cfg_hit_power_critical = pSettings->r_fvector4(pSectionName, "hit_power_critical");
						}

						if (pSettings->line_exist(pSectionName, "ammo_mag_size"))
						{
							imgui_weapon_manager.can_show_ammo_mag_size = true;
							imgui_weapon_manager.cfg_ammo_mag_size = pSettings->r_u32(pSectionName, "ammo_mag_size");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_accel_factor"))
						{
							imgui_weapon_manager.can_show_upgrade_disp_accel_factor = true;
							imgui_weapon_manager.cfg_upgrade_disp_accel_factor = pSettings->r_float(pSectionName, "PDM_disp_accel_factor");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_base"))
						{
							imgui_weapon_manager.can_show_upgrade_disp_base = true;
							imgui_weapon_manager.cfg_upgrade_disp_base = pSettings->r_float(pSectionName, "PDM_disp_base");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_crouch"))
						{
							imgui_weapon_manager.can_show_upgrade_disp_crouch = true;
							imgui_weapon_manager.cfg_upgrade_disp_crouch = pSettings->r_float(pSectionName, "PDM_disp_crouch");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_crouch_no_acc"))
						{
							imgui_weapon_manager.can_show_upgrade_disp_crouch_no_acc = true;
							imgui_weapon_manager.cfg_upgrade_disp_crouch_no_acc = pSettings->r_float(pSectionName, "PDM_disp_crouch_no_acc");
						}

						if (pSettings->line_exist(pSectionName, "PDM_disp_vel_factor"))
						{
							imgui_weapon_manager.can_show_upgrade_disp_vel_factor = true;
							imgui_weapon_manager.cfg_upgrade_disp_vel_factor
								= pSettings->r_float(pSectionName, "PDM_disp_vel_factor");
						}

						if (pSettings->line_exist(pSectionName, "cam_return"))
						{
							imgui_weapon_manager.can_show_cam_return = true;
							imgui_weapon_manager.cfg_cam_return = pSettings->r_float(pSectionName, "cam_return");
						}

						if (pSettings->line_exist(pSectionName, "cam_relax_speed"))
						{
							imgui_weapon_manager.can_show_cam_relax_speed = true;
							imgui_weapon_manager.cfg_cam_relax_speed = pSettings->r_float(pSectionName, "cam_relax_speed");
						}

						if (pSettings->line_exist(pSectionName, "cam_relax_speed_ai"))
						{
							imgui_weapon_manager.can_show_cam_relax_speed_ai = true;
							imgui_weapon_manager.cfg_cam_relax_speed_ai = pSettings->r_float(pSectionName, "cam_relax_speed_ai");
						}

						if (pSettings->line_exist(pSectionName, "cam_dispersion"))
						{
							imgui_weapon_manager.can_show_cam_dispersion = true;
							imgui_weapon_manager.cfg_cam_dispersion = pSettings->r_float(pSectionName, "cam_dispersion");
						}

						if (pSettings->line_exist(pSectionName, "cam_dispersion_inc"))
						{
							imgui_weapon_manager.can_show_cam_dispersion_inc = true;
							imgui_weapon_manager.cfg_cam_dispersion_inc = pSettings->r_float(pSectionName, "cam_dispersion_inc");
						}


						if (pSettings->line_exist(pSectionName, "cam_dispersion_frac"))
						{
							imgui_weapon_manager.can_show_cam_dispersion_frac = true;
							imgui_weapon_manager.cfg_cam_dispersion_frac = pSettings->r_float(pSectionName, "cam_dispersion_frac");
						}


						if (pSettings->line_exist(pSectionName, "cam_max_angle"))
						{
							imgui_weapon_manager.can_show_cam_max_angle = true;
							imgui_weapon_manager.cfg_cam_max_angle = pSettings->r_float(pSectionName, "cam_max_angle");
						}


						if (pSettings->line_exist(pSectionName, "cam_max_angle_horz"))
						{
							imgui_weapon_manager.can_show_cam_max_angle_horz = true;
							imgui_weapon_manager.cfg_cam_max_angle_horz = pSettings->r_float(pSectionName, "cam_max_angle_horz");
						}


						if (pSettings->line_exist(pSectionName, "cam_step_angle_horz"))
						{
							imgui_weapon_manager.can_show_cam_step_angle_horz = true;
							imgui_weapon_manager.cfg_cam_step_angle_horz = pSettings->r_float(pSectionName, "cam_step_angle_horz");
						}



						if (pSettings->line_exist(pSectionName, "fire_dispersion_base"))
						{
							imgui_weapon_manager.can_show_fire_dispersion_base = true;
							imgui_weapon_manager.cfg_fire_dispersion_base = pSettings->r_float(pSectionName, "fire_dispersion_base");
						}


						if (pSettings->line_exist(pSectionName, "control_inertion_factor"))
						{
							imgui_weapon_manager.can_show_control_inertion_factor = true;
							imgui_weapon_manager.cfg_control_inertion_factor = pSettings->r_float(pSectionName, "control_inertion_factor");
						}


						if (pSettings->line_exist(pSectionName, "crosshair_inertion"))
						{
							imgui_weapon_manager.can_show_crosshair_inertion = true;
							imgui_weapon_manager.cfg_crosshair_inertion = pSettings->r_float(pSectionName, "crosshair_inertion");
						}


						if (pSettings->line_exist(pSectionName, "fire_dispersion_condition_factor"))
						{
							imgui_weapon_manager.can_show_fire_dispersion_condition_factor = true;
							imgui_weapon_manager.cfg_fire_dispersion_condition_factor = pSettings->r_float(pSectionName, "fire_dispersion_condition_factor");
						}

						if (pSettings->line_exist(pSectionName, "inv_grid_x"))
						{
							imgui_weapon_manager.can_show_inv_grid_x = true;
							imgui_weapon_manager.cfg_inv_grid_x = pSettings->r_u32(pSectionName, "inv_grid_x");
						}

						if (pSettings->line_exist(pSectionName, "inv_grid_y"))
						{
							imgui_weapon_manager.can_show_inv_grid_y = true;
							imgui_weapon_manager.cfg_inv_grid_y = pSettings->r_u32(pSectionName, "inv_grid_y");
						}

						if (pSettings->line_exist(pSectionName, "inv_grid_width"))
						{
							imgui_weapon_manager.can_show_inv_grid_width = true;
							imgui_weapon_manager.cfg_inv_grid_width = pSettings->r_u32(pSectionName, "inv_grid_width");
						}

						if (pSettings->line_exist(pSectionName, "inv_grid_height"))
						{
							imgui_weapon_manager.can_show_inv_grid_height = true;
							imgui_weapon_manager.cfg_inv_grid_height = pSettings->r_u32(pSectionName, "inv_grid_height");
						}

						if (pSettings->line_exist(pSectionName, "silencer_x"))
						{
							if (pWeapon)
							{
								imgui_weapon_manager.can_show_silencer_x = true;
								imgui_weapon_manager.cfg_silencer_x = pSettings->r_s32(pSectionName, "silencer_x");
							}
						}

						if (pSettings->line_exist(pSectionName, "silencer_y"))
						{
							if (pWeapon)
							{
								imgui_weapon_manager.can_show_silencer_y = true;
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
							float x = imgui_weapon_manager.inv_grid_x * INV_GRID_WIDTH(isHQIcons);
							float y = imgui_weapon_manager.inv_grid_y * INV_GRID_HEIGHT(isHQIcons);
							float w = imgui_weapon_manager.inv_grid_width * INV_GRID_WIDTH(isHQIcons);
							float h = imgui_weapon_manager.inv_grid_height * INV_GRID_HEIGHT(isHQIcons);

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



				if (pSO)
				{
					if (ImGui::TreeNode("Ballistic"))
					{
						ImGui::Text("Fire distance: %.4f", pSO->getFireDistance());
						ImGui::Text("Bullet speed: %.4f", pSO->getStartBulletSpeed());
						ImGui::Text("RPM: %.4f", pSO->getRPM());
						ImGui::TreePop();
					}


					if (ImGui::TreeNode("Hit"))
					{
						ImGui::Text("Hit impulse: %.4f", pSO->getHitImpulse());
						const auto& hit_power = pSO->getHitPower();
						ImGui::Text("Hit power: %.4f %.4f %.4f %.4f", hit_power.x, hit_power.y, hit_power.z, hit_power.z, hit_power.w);
						const auto& hit_power_critical = pSO->getHitPowerCritical();
						ImGui::Text("Hit power critical: %.4f %.4f %.4f %.4f", hit_power_critical.x, hit_power_critical.y, hit_power_critical.z, hit_power_critical.w);
						ImGui::TreePop();
					}
				}

				if (pWeapon && pSO)
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
			}

			if (imgui_weapon_manager.init)
			{
				constexpr const char* pModalIconSelectionName = "Select Icon...##Editing";

				if (imgui_weapon_manager.can_show_modal_icon_selection_window && !ImGui::IsPopupOpen(pModalIconSelectionName))
					ImGui::OpenPopup(pModalIconSelectionName);

				if (ImGui::BeginPopupModal(pModalIconSelectionName, &imgui_weapon_manager.can_show_modal_icon_selection_window, ImGuiWindowFlags_AlwaysAutoResize))
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

									float x = icon.inv_grid_x * INV_GRID_WIDTH(isHQIcons);
									float y = icon.inv_grid_y * INV_GRID_HEIGHT(isHQIcons);
									float w = icon.inv_grid_width * INV_GRID_WIDTH(isHQIcons);
									float h = icon.inv_grid_height * INV_GRID_HEIGHT(isHQIcons);

									char button_name[64]{};
									sprintf_s(button_name, sizeof(button_name), "%s%d", "WeaponIconButton_", current_icon_index);

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

										imgui_weapon_manager.can_show_modal_icon_selection_window = false;
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
						imgui_weapon_manager.upgrade_disp_crouch
							= imgui_weapon_manager.cfg_upgrade_disp_crouch;
						imgui_weapon_manager.upgrade_disp_crouch_no_acc = imgui_weapon_manager.cfg_upgrade_disp_crouch_no_acc;
						imgui_weapon_manager.upgrade_disp_vel_factor = imgui_weapon_manager.cfg_upgrade_disp_vel_factor;
						imgui_weapon_manager.fire_dispersion_condition_factor = imgui_weapon_manager.cfg_fire_dispersion_condition_factor;
						imgui_weapon_manager.inv_grid_height = imgui_weapon_manager.cfg_inv_grid_height;
						imgui_weapon_manager.inv_grid_width = imgui_weapon_manager.cfg_inv_grid_width;
						imgui_weapon_manager.inv_grid_x = imgui_weapon_manager.cfg_inv_grid_x;
						imgui_weapon_manager.inv_grid_y = imgui_weapon_manager.cfg_inv_grid_y;
						imgui_weapon_manager.silencer_x = imgui_weapon_manager.cfg_silencer_x;
						imgui_weapon_manager.silencer_y = imgui_weapon_manager.cfg_silencer_y;

						if (pItem)
						{
							pItem->setCost(imgui_weapon_manager.inv_cost);
							pItem->setWeight(imgui_weapon_manager.inv_weight);
							pItem->SetInvGridRect(imgui_weapon_manager.inv_grid_x, imgui_weapon_manager.inv_grid_y, imgui_weapon_manager.inv_grid_width, imgui_weapon_manager.inv_grid_height);
						}

						if (pSO)
						{
							pSO->setFireDistance(imgui_weapon_manager.fire_distance);
							pSO->setStartBulletSpeed(imgui_weapon_manager.bullet_speed);
							pSO->setRPM(imgui_weapon_manager.rpm);
							pSO->setHitImpulse(imgui_weapon_manager.hit_impulse);
							pSO->setHitPower(imgui_weapon_manager.hit_power);
							pSO->setHitPowerCritical(imgui_weapon_manager.hit_power_critical);
						}

						if (pWeapon)
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

					ImGui::SameLine();

					if (ImGui::Button("Save"))
					{

					}
					ImGuiSliderFlags flags = ImGuiSliderFlags_AlwaysClamp | ImGuiSliderFlags_Logarithmic;

					if (ImGui::TreeNode("Inventory##Editing"))
					{
						if (imgui_weapon_manager.can_show_inv_cost)
						{
							if (ImGui::SliderInt("Cost##Editing", &imgui_weapon_manager.inv_cost, 0, 100000, "%d", flags))
							{
								pItem->setCost(imgui_weapon_manager.inv_cost);
							}
						}

						if (imgui_weapon_manager.can_show_inv_weight)
						{
							if (ImGui::SliderFloat("Weight##Editing", &imgui_weapon_manager.inv_weight, 0.0f, 100000.0f, "%.3f", flags))
							{
								pItem->setWeight(imgui_weapon_manager.inv_weight);
							}
						}

						if (ImGui::TreeNode("Icon##Editing"))
						{
							if (imgui_weapon_manager.can_show_inv_grid_height && imgui_weapon_manager.can_show_inv_grid_width && imgui_weapon_manager.can_show_inv_grid_x && imgui_weapon_manager.can_show_inv_grid_y)
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
									float x = imgui_weapon_manager.inv_grid_x * INV_GRID_WIDTH(isHQIcons);
									float y = imgui_weapon_manager.inv_grid_y * INV_GRID_HEIGHT(isHQIcons);
									float w = imgui_weapon_manager.inv_grid_width * INV_GRID_WIDTH(isHQIcons);
									float h = imgui_weapon_manager.inv_grid_height * INV_GRID_HEIGHT(isHQIcons);

									bool is_pressed = ImGui::ImageButton("WeaponIconInWeaponManager##Editing", imgui_weapon_manager.ui_icons.Surface, { w,h }, { x / imgui_weapon_manager.ui_icons.w, y / imgui_weapon_manager.ui_icons.h }, { (x + w) / imgui_weapon_manager.ui_icons.w, (y + h) / imgui_weapon_manager.ui_icons.h });

									if (is_pressed)
									{
										imgui_weapon_manager.can_show_modal_icon_selection_window = true;
									}
								}
							}

							if (ImGui::TreeNode("Addons"))
							{
								if (imgui_weapon_manager.can_show_silencer_x)
								{
									if (ImGui::SliderInt("Silencer X##Editing", &imgui_weapon_manager.silencer_x, -4096, 4096, "%d", flags))
									{
										if (pWeapon)
										{
											pWeapon->SetSilencerX(imgui_weapon_manager.silencer_x);
										}
									}
								}
								if (imgui_weapon_manager.can_show_silencer_y)
								{
									if (ImGui::SliderInt("Silencer Y##Editing", &imgui_weapon_manager.silencer_y, -4096, 4096, "%d", flags))
									{
										if (pWeapon)
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

					if (pSO)
					{
						if (ImGui::TreeNode("Ballistic##Editing"))
						{
							if (imgui_weapon_manager.can_show_fire_distance)
							{
								if (ImGui::SliderFloat("Fire distance##Editing", &imgui_weapon_manager.fire_distance, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setFireDistance(imgui_weapon_manager.fire_distance);
								}
							}

							if (imgui_weapon_manager.can_show_bullet_speed)
							{
								if (ImGui::SliderFloat("Bullet speed##Editing", &imgui_weapon_manager.bullet_speed, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setStartBulletSpeed(imgui_weapon_manager.bullet_speed);
								}
							}

							if (imgui_weapon_manager.can_show_rpm)
							{
								if (ImGui::SliderFloat("RPM##Editing", &imgui_weapon_manager.rpm, 0.001f, 100.0f, "%.3f", flags))
								{
									pSO->setRPM(imgui_weapon_manager.rpm);
								}
							}

							ImGui::TreePop();
						}

						if (ImGui::TreeNode("Hit##Editing"))
						{
							if (imgui_weapon_manager.can_show_hit_impulse)
							{
								if (ImGui::SliderFloat("Hit impulse##Editing", &imgui_weapon_manager.hit_impulse, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setHitImpulse(imgui_weapon_manager.hit_impulse);
								}
							}

							if (imgui_weapon_manager.can_show_hit_power)
							{
								if (ImGui::SliderFloat4("Hit power##Editing", &imgui_weapon_manager.hit_power.x, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setHitPower(imgui_weapon_manager.hit_power);
								}
							}

							if (imgui_weapon_manager.can_show_hit_power_critical)
							{
								if (ImGui::SliderFloat4("Hit power critical##Editing", &imgui_weapon_manager.hit_power_critical.x, 0.0f, 10000.0f, "%.3f", flags))
								{
									pSO->setHitPowerCritical(imgui_weapon_manager.hit_power_critical);
								}
							}

							ImGui::TreePop();
						}
					}

					if (pWeapon)
					{
						if (ImGui::TreeNode("Ammunition##Editing"))
						{
							if (imgui_weapon_manager.can_show_ammo_mag_size)
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
							if (imgui_weapon_manager.can_show_fire_dispersion_base)
							{
								if (ImGui::SliderFloat("Fire dispersion base", &imgui_weapon_manager.fire_dispersion_base, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->setFireDispersionBase(imgui_weapon_manager.fire_dispersion_base);
								}
							}

							if (imgui_weapon_manager.can_show_control_inertion_factor)
							{
								if (ImGui::SliderFloat("Control inertion factor", &imgui_weapon_manager.control_inertion_factor, 0.0f, 100.0f, "%.3f", flags))
								{
									pItem->setControlInertionFactor(imgui_weapon_manager.control_inertion_factor);
								}
							}

							if (imgui_weapon_manager.can_show_crosshair_inertion)
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

							if (imgui_weapon_manager.can_show_upgrade_disp_vel_factor)
							{
								if (ImGui::SliderFloat("Upgrade dispersion velocity factor", &imgui_weapon_manager.upgrade_disp_vel_factor, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->Set_PDM_Vel_F(imgui_weapon_manager.upgrade_disp_vel_factor);
								}
							}

							if (imgui_weapon_manager.can_show_upgrade_disp_accel_factor)
							{
								if (ImGui::SliderFloat("Upgrade dispersion acceleration factor", &imgui_weapon_manager.upgrade_disp_accel_factor, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->Set_PDM_Accel_F(imgui_weapon_manager.upgrade_disp_accel_factor);
								}
							}

							if (imgui_weapon_manager.can_show_upgrade_disp_crouch)
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

							if (imgui_weapon_manager.can_show_fire_dispersion_condition_factor)
							{
								if (ImGui::SliderFloat("Dispersion factor when weapon is damaged/broken", &imgui_weapon_manager.fire_dispersion_condition_factor, 0.0f, 100.0f, "%.3f", flags))
								{
									pWeapon->setFireDispersionConditionFactor(imgui_weapon_manager.fire_dispersion_condition_factor);
								}
							}

							ImGui::TreePop();
						}
					}
				}
			}
		}
		};

	if (ImGui::Begin("Weapon Manager", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WeaponManager)]))
	{
		if (ImGui::BeginTabBar("##TB_InGameWeaponManager"))
		{
			CActor* pActor = smart_cast<CActor*>(Level().CurrentEntity());

			char slot2_tab_name[128]{ "Slot 2 (INV_SLOT_2) - " };
			char slot3_tab_name[128]{ "Slot 3 (INV_SLOT_3) - " };

			if (pActor)
			{
				CInventoryItem* pItemInSlot2 = pActor->inventory().ItemFromSlot(INV_SLOT_2);
				CInventoryItem* pItemInSlot3 = pActor->inventory().ItemFromSlot(INV_SLOT_3);

				if (pItemInSlot2)
				{
					memcpy_s(&slot2_tab_name[0] + strlen(slot2_tab_name), sizeof(slot2_tab_name), pItemInSlot2->m_section_id.c_str(), pItemInSlot2->m_section_id.size());
				}
				memcpy_s(&slot2_tab_name[0] + strlen(slot2_tab_name), sizeof(slot2_tab_name), "##TB_InGameWeaponManager", sizeof("##TB_InGameWeaponManager"));

				if (pItemInSlot3)
				{
					memcpy_s(&slot3_tab_name[0] + strlen(slot3_tab_name), sizeof(slot3_tab_name), pItemInSlot3->m_section_id.c_str(), pItemInSlot3->m_section_id.size());
				}
				memcpy_s(&slot3_tab_name[0] + strlen(slot3_tab_name), sizeof(slot3_tab_name), "##TB_InGameWeaponManager", sizeof("##TB_InGameWeaponManager"));
			}

			if (ImGui::BeginTabItem(slot2_tab_name))
			{
				CInventoryItem* pItem = pActor->inventory().ItemFromSlot(INV_SLOT_2);
				draw_item(pItem, INV_SLOT_2);


				ImGui::EndTabItem();
			}


			if (ImGui::BeginTabItem(slot3_tab_name))
			{

				CInventoryItem* pItem = pActor->inventory().ItemFromSlot(INV_SLOT_3);
				draw_item(pItem, INV_SLOT_3);


				ImGui::EndTabItem();
			}

			ImGui::EndTabBar();
		}


		ImGui::End();
	}
}
