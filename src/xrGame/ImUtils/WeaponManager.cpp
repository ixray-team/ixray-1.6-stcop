#include "stdafx.h"
#include "../Actor.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../Weapon.h"

struct
{
	bool init{};

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

	Fvector4 hit_power;
	Fvector4 cfg_hit_power;

	Fvector4 hit_power_critical;
	Fvector4 cfg_hit_power_critical;

	// id for string table
	char inv_name[128]{};
	// id for string table
	char inv_short_name[128]{};

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

	auto draw_item = [](CInventoryItem* pItem) {
		if (pItem)
		{
			CShootingObject* pSO = dynamic_cast<CShootingObject*>(pItem);
			CWeapon* pWeapon = dynamic_cast<CWeapon*>(pItem);

			if (!imgui_weapon_manager.init)
			{
				imgui_weapon_manager.inv_cost = pItem->Cost();
				imgui_weapon_manager.inv_weight = pItem->Weight();

				if (pSO)
				{
					imgui_weapon_manager.fire_distance = pSO->getFireDistance();
					imgui_weapon_manager.bullet_speed = pSO->getStartBulletSpeed();
					imgui_weapon_manager.rpm = pSO->getRPM();
					imgui_weapon_manager.hit_impulse = pSO->getHitImpulse();
					imgui_weapon_manager.hit_power = pSO->getHitPower();
					imgui_weapon_manager.hit_power_critical = pSO->getHitPowerCritical();
				}

				if (pWeapon)
				{
					imgui_weapon_manager.ammo_mag_size = pWeapon->GetAmmoMagSize();
				}


				// defaults

				if (pSettings)
				{
					if (pSettings->section_exist(pItem->m_section_id.c_str()))
					{
						const char* pSectionName = pItem->m_section_id.c_str();

						imgui_weapon_manager.cfg_inv_cost = pSettings->r_u32(pSectionName, "cost");
						imgui_weapon_manager.cfg_inv_weight = pSettings->r_float(pSectionName, "inv_weight");

						imgui_weapon_manager.cfg_fire_distance = pSettings->r_float(pSectionName, "fire_distance");
						imgui_weapon_manager.cfg_bullet_speed = pSettings->r_float(pSectionName, "bullet_speed");
						imgui_weapon_manager.cfg_rpm = pSettings->r_float(pSectionName, "rpm");
						imgui_weapon_manager.cfg_rpm = 60.0f / imgui_weapon_manager.cfg_rpm;
						imgui_weapon_manager.cfg_hit_impulse = pSettings->r_float(pSectionName, "hit_impulse");
						imgui_weapon_manager.cfg_hit_power = pSettings->r_fvector4(pSectionName, "hit_power");
						imgui_weapon_manager.cfg_hit_power_critical = pSettings->r_fvector4(pSectionName, "hit_power_critical");
						imgui_weapon_manager.cfg_ammo_mag_size = pSettings->r_u32(pSectionName, "ammo_mag_size");
						imgui_weapon_manager.cfg_upgrade_disp_accel_factor = pSettings->r_float(pSectionName, "PDM_disp_accel_factor");
						imgui_weapon_manager.cfg_upgrade_disp_base = pSettings->r_float(pSectionName, "PDM_disp_base");
						imgui_weapon_manager.cfg_upgrade_disp_crouch = pSettings->r_float(pSectionName, "PDM_disp_crouch");
						imgui_weapon_manager.cfg_upgrade_disp_crouch_no_acc = pSettings->r_float(pSectionName, "PDM_disp_crouch_no_acc");
						imgui_weapon_manager.cfg_upgrade_disp_vel_factor
							= pSettings->r_float(pSectionName, "PDM_disp_vel_factor");
						imgui_weapon_manager.cfg_cam_return = pSettings->r_float(pSectionName, "cam_return");
						imgui_weapon_manager.cfg_cam_relax_speed = pSettings->r_float(pSectionName, "cam_relax_speed");
						imgui_weapon_manager.cfg_cam_relax_speed_ai = pSettings->r_float(pSectionName, "cam_relax_speed_ai");
						imgui_weapon_manager.cfg_cam_dispersion = pSettings->r_float(pSectionName, "cam_dispersion");
						imgui_weapon_manager.cfg_cam_dispersion_inc = pSettings->r_float(pSectionName, "cam_dispersion_inc");
						imgui_weapon_manager.cfg_cam_dispersion_frac = pSettings->r_float(pSectionName, "cam_dispersion_frac");
						imgui_weapon_manager.cfg_cam_max_angle = pSettings->r_float(pSectionName, "cam_max_angle");
						imgui_weapon_manager.cfg_cam_max_angle_horz = pSettings->r_float(pSectionName, "cam_max_angle_horz");
						imgui_weapon_manager.cfg_cam_step_angle_horz = pSettings->r_float(pSectionName, "cam_step_angle_horz");
						imgui_weapon_manager.cfg_fire_dispersion_base = pSettings->r_float(pSectionName, "fire_dispersion_base");
						imgui_weapon_manager.cfg_control_inertion_factor = pSettings->r_float(pSectionName, "control_inertion_factor");
						imgui_weapon_manager.cfg_crosshair_inertion = pSettings->r_float(pSectionName, "crosshair_inertion");
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

						if (pItem)
						{
							pItem->setCost(imgui_weapon_manager.inv_cost);
							pItem->setWeight(imgui_weapon_manager.inv_weight);
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

					if (ImGui::TreeNode("Inventory##Editing"))
					{
						if (ImGui::SliderInt("Cost##Editing", &imgui_weapon_manager.inv_cost, 0, 100000))
						{
							pItem->setCost(imgui_weapon_manager.inv_cost);
						}

						if (ImGui::SliderFloat("Weight##Editing", &imgui_weapon_manager.inv_weight, 0.0f, 100000.0f))
						{
							pItem->setWeight(imgui_weapon_manager.inv_weight);
						}

						ImGui::TreePop();
					}

					if (pSO)
					{
						if (ImGui::TreeNode("Ballistic##Editing"))
						{
							if (ImGui::SliderFloat("Fire distance##Editing", &imgui_weapon_manager.fire_distance, 0.0f, 10000.0f))
							{
								pSO->setFireDistance(imgui_weapon_manager.fire_distance);
							}

							if (ImGui::SliderFloat("Bullet speed##Editing", &imgui_weapon_manager.bullet_speed, 0.0f, 10000.0f))
							{
								pSO->setStartBulletSpeed(imgui_weapon_manager.bullet_speed);
							}

							if (ImGui::SliderFloat("RPM##Editing", &imgui_weapon_manager.rpm, 0.001f, 100.0f, "%.3f", ImGuiSliderFlags_AlwaysClamp))
							{
								pSO->setRPM(imgui_weapon_manager.rpm);
							}

							ImGui::TreePop();
						}

						if (ImGui::TreeNode("Hit##Editing"))
						{
							if (ImGui::SliderFloat("Hit impulse##Editing", &imgui_weapon_manager.hit_impulse, 0.0f, 10000.0f))
							{
								pSO->setHitImpulse(imgui_weapon_manager.hit_impulse);
							}

							if (ImGui::SliderFloat4("Hit power##Editing", &imgui_weapon_manager.hit_power.x, 0.0f, 10000.0f))
							{
								pSO->setHitPower(imgui_weapon_manager.hit_power);
							}

							if (ImGui::SliderFloat4("Hit power critical##Editing", &imgui_weapon_manager.hit_power_critical.x, 0.0f, 10000.0f))
							{
								pSO->setHitPowerCritical(imgui_weapon_manager.hit_power_critical);
							}

							ImGui::TreePop();
						}
					}

					if (pWeapon)
					{
						if (ImGui::TreeNode("Ammunition##Editing"))
						{
							if (ImGui::SliderInt("Magazine size##Editing", &imgui_weapon_manager.ammo_mag_size, 0, 10000))
							{
								pWeapon->SetAmmoMagSize(imgui_weapon_manager.ammo_mag_size);
							}

							// todo: implement changing calibers and calibers section names
							// add dropdown menu with parsed all ammos that game supports

							ImGui::TreePop();
						}

						if (ImGui::TreeNode("Dispersion"))
						{
							if (ImGui::SliderFloat("Fire dispersion base: %.4f", &imgui_weapon_manager.fire_dispersion_base, 0.0f, 100.0f))
							{
								pWeapon->setFireDispersionBase(imgui_weapon_manager.fire_dispersion_base);
							}


							if (ImGui::SliderFloat("Control inertion factor: %.4f", &imgui_weapon_manager.control_inertion_factor, 0.0f, 100.0f))
							{
								pItem->setControlInertionFactor(imgui_weapon_manager.control_inertion_factor);
							}

							if (ImGui::SliderFloat("Crosshair inertion: %.4f", &imgui_weapon_manager.crosshair_inertion, 0.0f, 100.0f))
							{
								pWeapon->setCrosshairInertion(imgui_weapon_manager.crosshair_inertion);
							}

							if (ImGui::SliderFloat("Upgrade dispersion base: %.4f", &imgui_weapon_manager.upgrade_disp_base, 0.0f, 100.0f))
							{
								pWeapon->Set_PDM_Base(imgui_weapon_manager.upgrade_disp_base);
							}

							if (ImGui::SliderFloat("Upgrade dispersion velocity factor: %.4f", &imgui_weapon_manager.upgrade_disp_vel_factor, 0.0f, 100.0f))
							{
								pWeapon->Set_PDM_Vel_F(imgui_weapon_manager.upgrade_disp_vel_factor);
							}

							if (ImGui::SliderFloat("Upgrade dispersion acceleration factor: %.4f", &imgui_weapon_manager.upgrade_disp_accel_factor, 0.0f, 100.0f))
							{
								pWeapon->Set_PDM_Accel_F(imgui_weapon_manager.upgrade_disp_accel_factor);
							}

							if (ImGui::SliderFloat("Upgrade dispersion crouch: %.4f", &imgui_weapon_manager.upgrade_disp_crouch, 0.0f, 100.0f))
							{
								pWeapon->Set_PDM_Crouch(imgui_weapon_manager.upgrade_disp_crouch);
							}

							if (ImGui::SliderFloat("Upgrade dispersion crouch no acceleration: %.4f", &imgui_weapon_manager.upgrade_disp_crouch_no_acc, 0.0f, 100.0f))
							{
								pWeapon->Set_PDM_Crouch_NA(imgui_weapon_manager.upgrade_disp_crouch_no_acc);
							}

							if (ImGui::SliderFloat("Dispersion factor when weapon is damaged/broken: %.4f", &imgui_weapon_manager.fire_dispersion_condition_factor, 0.0f, 100.0f))
							{
								pWeapon->setFireDispersionConditionFactor(imgui_weapon_manager.fire_dispersion_condition_factor);
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
			if (ImGui::BeginTabItem("Slot 2 (INV_SLOT_2)##TB_InGameWeaponManager"))
			{
				CActor* pActor = smart_cast<CActor*>(Level().CurrentEntity());

				if (pActor)
				{
					CInventoryItem* pItem = pActor->inventory().ItemFromSlot(INV_SLOT_2);

					draw_item(pItem);
				}

				ImGui::EndTabItem();
			}


			ImGui::EndTabBar();
		}


		ImGui::End();
	}
}
