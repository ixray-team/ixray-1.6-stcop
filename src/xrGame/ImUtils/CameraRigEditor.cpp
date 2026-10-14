#include "StdAfx.h"
#include "ImUtils.h"
#include "../Actor.h"
#include "../ActorEffector.h"
#include "../CameraRig.h"
#include "../HudItem.h"
#include "../Inventory.h"
#include "../Weapon.h"
#include "../player_hud.h"

static const struct
{
	const char*		Key;
	const char8_t*	Text;
}
g_camera_rig_descriptions[] =
{
	{ "hud_ik_share", u8"Доля покачивания оружия (инерция поворота, смещения hud_move_* с наклонами/прыжком/приземлением, movement_layer_*), которую руки догоняют через IK; остальное двигает весь HUD. 0 = старое жёсткое поведение" },
	{ "lean_hud_counter", u8"Доля крена от наклона Q/E, снимаемая с оружия (0 = оружие кренится вместе с камерой, меньше 0 = доп. крен); руки догоняют через IK" },
	{ "left_hand_ik_follow", u8"с, как быстро хват левой руки догоняет свою анимированную позу после включения IK хвата; 0 = хват замирает при включении IK" },
	{ "left_hand_ik_blend", u8"с, за сколько левая рука подлетает к хвату (после перезарядки, доставания...) и отпускает его; 0 = сразу. Для оружия: left_hand_ik_blend в hud-секции" },
	{ "left_hand_finger_tail", u8"Доля подлёта руки к хвату в его конце, за которую пальцы из замороженной в начале подлёта позы переходят в хват (позу из анимации); 0 = пальцы всё время идут по анимации. Для оружия: left_hand_finger_tail в hud-секции" },
	{ "left_hand_attach_blend", u8"с, плавность прикрепления левой руки к оружию и её отпускания (left_hand_attach_motions). Для оружия: left_hand_attach_blend в hud-секции" },

	{ "turn_sway_smooth", u8"с, сглаживание покачивания от поворота мыши (неровные рывки мыши трясут оружие в руках); 0 = выкл." },
	{ "inertia_speed", u8"Общая скорость пружин hud_move_* и покачивания от взгляда" },
	{ "inertia_damping", u8"Затухание пружины смещений hud_move_*: меньше 1 = перелёт и успокоение, 1 = без перелёта. Для оружия: inertia_damping в hud-секции" },
	{ "mass_speed_exponent", u8"Насколько hud_mass замедляет инерцию: скорость / масса^степень (0 = масса не влияет, 0.5 = сильно). hud_mass задаётся в hud-секции оружия" },
	{ "sway_damping_ratio", u8"Затухание пружин покачивания HUD от взгляда и движения: меньше 1 = перелёт, 1 = без перелёта" },

	{ "hud_inertialization", u8"с, модели рук и оружия переключают анимации сразу, а скачок позы гаснет примерно за это время (как в CoD); 0 = старый плавный переход" },
	{ "idle_fade", u8"с, плавный переход рук и оружия в anm_idle* (после выстрела, перезарядки, доставания...); 0 = как в анимациях. Для оружия: idle_fade в hud-секции" },
	{ "additive_blend_in", u8"с, плавность включения аддитивных анимаций (additive_motions). Для оружия: additive_blend_in в hud-секции" },
	{ "additive_blend_out", u8"с, плавность выключения аддитивных анимаций (additive_motions). Для оружия: additive_blend_out в hud-секции" },

	{ "aim_reload_motion", u8"Доля движения оружия из анимации перезарядки в прицеле (1 = как нарисовано); руки догоняют через IK. Для оружия: aim_reload_motion в hud-секции" },
	{ "aim_reload_blend", u8"с, плавность включения и выключения aim_reload_motion" },
	{ "aim_reload_tail", u8"Доля анимации перезарядки в её конце, за которую гашение снимается, чтобы она пришла в свою последнюю (idle) позу" },

	{ "shot_anim_variation", u8"Каждый выстрел масштабирует движение оружия из анимации выстрела по каждой оси на 1 ± это значение. Для оружия: shot_anim_variation в hud-секции" },
	{ "shot_anim_twist", u8"Каждый выстрел переводит до этой доли подброса ствола в рыскание и крен, в случайную сторону. Для оружия: shot_anim_twist в hud-секции" },
	{ "shot_anim_blend", u8"с, плавность включения и выключения вариации вокруг стрельбы" },
	{ "shot_anim_tail", u8"Доля анимации выстрела в её конце, за которую вариация и гашение в прицеле снимаются, чтобы она пришла в свою последнюю (idle) позу" },
	{ "aim_shot_motion", u8"В прицеле: доля движения оружия из анимации выстрела, чтобы выстрел от бедра смотрелся как прицельный. Для оружия: aim_shot_motion в hud-секции" },

	{ "cam_shoot_variation", u8"Разброс силы камеры выстрела по каждой оси (0.5 = от 50% до 150%); 0 = выкл. Для оружия: cam_shoot_variation в hud-секции" },
	{ "cam_shoot_mirror", u8"Камера выстрела случайно уходит влево или вправо (рыскание и крен), подброс всегда вверх. Для оружия: cam_shoot_mirror в hud-секции" },
	{ "cam_shoot_speed_variation", u8"Разброс скорости камеры выстрела (0.1 = ±10%), удары в очереди идут не в одном ритме. Для оружия: cam_shoot_speed_variation в hud-секции" },
	{ "shot_fov_kick", u8"Градусы, на сколько каждый выстрел на миг расширяет угол обзора; 0 = выкл. Для оружия: rig_fov_kick в hud-секции" },
	{ "aim_fov_kick_scale", u8"Доля толчка угла обзора в прицеле (в оптике он ещё уменьшается вместе с приближением)" },
	{ "fov_stiffness", u8"Жёсткость пружины угла обзора (переходы зума и толчок от выстрела): больше = быстрее" },
	{ "fov_damping", u8"Затухание пружины угла обзора: меньше = больше перелёт после толчка" },

	{ "motion_blend_in", u8"с, плавность включения камеры из hud-анимаций (перезарядка, доставание...)" },
	{ "motion_blend_out", u8"с, плавность выключения камеры из hud-анимаций" },
	{ "action_blend_in", u8"с, плавность включения камер действий (вход/выход из прицела, safemode)" },
	{ "action_blend_out", u8"с, плавность выключения камер действий" },
	{ "shot_blend_in", u8"с, плавность включения камеры выстрела (cam_shoot); 0 = сразу" },
	{ "shot_blend_out", u8"с, плавность выключения камеры выстрела" },
	{ "move_blend_in", u8"с, плавность включения камер движения актора (прыжок, присед...)" },
	{ "move_blend_out", u8"с, плавность выключения камер движения актора" },

	{ "punch", u8"Градусы, подброс камеры при выстреле: x = вверх, y = рыскание (в случайную сторону), z = крен (в случайную сторону). Для оружия: rig_punch в hud-секции" },
	{ "punch_stiffness", u8"Жёсткость пружины подброса камеры: больше = резче удар и быстрее возврат" },
	{ "punch_damping", u8"Затухание пружины подброса камеры: меньше = больше перелёт" },
	{ "punch_hud_scale", u8"Доля подброса камеры, которая применяется к камере HUD (1 = оружие уходит вместе с камерой)" },
	{ "aim_punch_scale", u8"Множитель подброса камеры в прицеле" },

	{ "kick_pos", u8"м, отдача оружия в руках при выстреле: смещение HUD (z меньше 0 = назад). Делится на sqrt(hud_mass). Для оружия: rig_kick_pos в hud-секции" },
	{ "kick_rot", u8"Градусы, отдача оружия в руках: x = подброс, y = рыскание, z = крен (y и z в случайную сторону). Делится на sqrt(hud_mass). Для оружия: rig_kick_rot в hud-секции" },
	{ "kick_stiffness", u8"Жёсткость пружины отдачи оружия: больше = резче удар и быстрее возврат" },
	{ "kick_damping", u8"Затухание пружины отдачи оружия: меньше = больше перелёт" },
	{ "aim_kick_scale", u8"Множитель отдачи оружия и тряски от выстрела в прицеле" },

	{ "shot_trauma", u8"Тряска, добавляемая каждым выстрелом (0..1; сила тряски = квадрат накопленного значения). Для оружия: rig_shot_trauma в hud-секции" },
	{ "hit_trauma", u8"Тряска от попадания по актору, умножается на силу удара" },
	{ "explosion_trauma", u8"Тряска от взрыва, умножается на силу взрыва" },
	{ "trauma_decay", u8"На сколько в секунду спадает накопленная тряска" },
	{ "shake_frequency", u8"Частота шума тряски: больше = мельче и быстрее" },
	{ "shake_hud_scale", u8"Доля тряски камеры, которая применяется к HUD" },
	{ "shake_rot", u8"Градусы, амплитуда тряски при полной силе: x = тангаж, y = рыскание, z = крен" },
	{ "shake_pos", u8"м, амплитуда смещения камеры при полной силе тряски" },

	{ "land_min_speed", u8"м/с, скорость приземления, с которой начинается удар камеры" },
	{ "land_max_speed", u8"м/с, скорость приземления, при которой удар камеры максимальный" },
	{ "land_pitch", u8"Градусы, кивок камеры вниз при приземлении" },
	{ "land_roll", u8"Градусы, крен камеры при приземлении (в случайную сторону)" },
	{ "land_height", u8"м, проседание камеры при приземлении" },
	{ "land_trauma", u8"Тряска при приземлении на максимальной скорости" },
	{ "land_hud_scale", u8"Во сколько раз HUD двигается при приземлении сильнее камеры (1 = так же)" },
	{ "land_stiffness", u8"Жёсткость пружины приземления: больше = резче и короче" },
	{ "land_damping", u8"Затухание пружины приземления: меньше = больше пружинит" },

	{ "strafe_roll", u8"Градусы крена камеры на 1 м/с бокового движения" },
	{ "look_roll", u8"Градусы крена камеры от скорости поворота взгляда" },
	{ "roll_max", u8"Градусы, предел крена от движения и взгляда" },
	{ "aim_roll_scale", u8"Множитель крена в прицеле" },
	{ "roll_stiffness", u8"Жёсткость пружины крена: больше = быстрее догоняет" },
	{ "roll_damping", u8"Затухание пружины крена: меньше = больше перелёт" },
};

static const char* CameraRigDescription(const char* key)
{
	for (const auto& it : g_camera_rig_descriptions)
	{
		if (xr_strcmp(it.Key, key) == 0)
		{
			return reinterpret_cast<const char*>(it.Text);
		}
	}

	return nullptr;
}

static void CameraRigApply()
{
	if (g_player_hud != nullptr)
	{
		g_player_hud->ReloadRigParams();
	}

	CActor* actor = Actor();
	if (actor == nullptr)
	{
		return;
	}

	actor->Cameras().Rig().ResetWeaponCache();
	for (PIItem item : actor->inventory().m_all)
	{
		if (CWeapon* weapon = item != nullptr ? item->cast_weapon() : nullptr)
		{
			weapon->LoadCamShotParams();
		}
	}
}

static shared_str CameraRigActiveHudSection()
{
	CActor* actor = Actor();
	PIItem active = actor != nullptr ? actor->inventory().ActiveItem() : nullptr;
	CHudItem* hud_item = active != nullptr ? active->cast_hud_item() : nullptr;
	return hud_item != nullptr ? hud_item->HudSection() : shared_str();
}

static void CameraRigFormatValue(const SCameraRigParam& param, string256& out)
{
	if (param.Flag != nullptr)
	{
		xr_strcpy(out, *param.Flag ? "true" : "false");
		return;
	}

	float value[3] = {};
	for (u32 i = 0; i < param.Count; i++)
	{
		value[i] = param.Degrees ? rad2deg(param.Value[i]) : param.Value[i];
	}

	if (param.Count == 3)
	{
		xr_sprintf(out, "%g, %g, %g", value[0], value[1], value[2]);
	}
	else
	{
		xr_sprintf(out, "%g", value[0]);
	}
}

static void CameraRigCopyToClipboard()
{
	u32 count = 0;
	const SCameraRigParam* params = CCameraRig::ParamList(count);

	xr_string text = "[camera_rig]\r\nenabled = true\r\n";
	const char* group = nullptr;
	for (u32 i = 0; i < count; i++)
	{
		if (group == nullptr || xr_strcmp(group, params[i].Group) != 0)
		{
			group = params[i].Group;
			text += "\r\n; ";
			text += group;
			text += "\r\n";
		}

		string256 value;
		CameraRigFormatValue(params[i], value);
		text += params[i].Key;
		text += " = ";
		text += value;
		text += "\r\n";
	}

	ImGui::SetClipboardText(text.c_str());
}

static void CameraRigSave()
{
	string_path path = {};
	FS.update_path(path, "$app_data_root$", "camera_rig\\saved.ltx");

	CInifile file(path, false, false, false);
	file.w_bool("camera_rig", "enabled", true);

	u32 count = 0;
	const SCameraRigParam* params = CCameraRig::ParamList(count);
	for (u32 i = 0; i < count; i++)
	{
		string256 value;
		CameraRigFormatValue(params[i], value);
		file.w_string("camera_rig", params[i].Key, value);
	}

	if (file.save_as(path))
	{
		Msg("Camera rig saved to [%s]", path);
	}
	else
	{
		Msg("! Can't save camera rig to [%s]", path);
	}
}

static bool CameraRigDrawParam(const SCameraRigParam& param, const shared_str& hud_sect)
{
	bool changed = false;
	ImGui::PushID(param.Key);
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * (param.Count == 3 ? 18.f : 8.f));

	if (param.Flag != nullptr)
	{
		changed = ImGui::Checkbox(param.Key, param.Flag);
	}
	else
	{
		float value[3] = {};
		for (u32 i = 0; i < param.Count; i++)
		{
			value[i] = param.Degrees ? rad2deg(param.Value[i]) : param.Value[i];
		}

		const bool edited = param.Count == 3 ? ImGui::DragFloat3(param.Key, value, param.Speed, 0.f, 0.f, "%.4f") : ImGui::DragFloat(param.Key, value, param.Speed, 0.f, 0.f, "%.4f");
		if (edited)
		{
			for (u32 i = 0; i < param.Count; i++)
			{
				param.Value[i] = param.Degrees ? deg2rad(value[i]) : value[i];
			}

			changed = true;
		}
	}

	if (const char* description = CameraRigDescription(param.Key))
	{
		if (ImGui::BeginItemTooltip())
		{
			ImGui::PushTextWrapPos(ImGui::GetFontSize() * 30.f);
			ImGui::TextUnformatted(description);
			ImGui::PopTextWrapPos();
			ImGui::EndTooltip();
		}
	}

	if (param.HudKey != nullptr && hud_sect.size() && pSettings->line_exist(hud_sect, param.HudKey))
	{
		ImGui::SameLine();
		ImGui::TextColored(ImVec4(1.f, 0.8f, 0.2f, 1.f), "[%s = %s]", param.HudKey, pSettings->r_string(hud_sect, param.HudKey));
		ImGui::SetItemTooltip("Overridden in hud section [%s]", hud_sect.c_str());
	}

	ImGui::PopID();
	return changed;
}

void RenderCameraRigEditor()
{
	bool& open = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CameraRig)];
	if (!open)
	{
		return;
	}

	if (!CCameraRig::Enabled() || !READ_IF_EXISTS(pSettings, r_bool, "camera_rig", "enabled", false))
	{
		open = false;
		return;
	}

	if (!ImGui::Begin("Camera Rig", &open))
	{
		ImGui::End();
		return;
	}

	bool changed = false;
	if (ImGui::Button("Reload config"))
	{
		CCameraRig::Reload();
		changed = true;
	}

	ImGui::SameLine();
	if (ImGui::Button("Copy LTX"))
	{
		CameraRigCopyToClipboard();
	}

	ImGui::SameLine();
	if (ImGui::Button("Save"))
	{
		CameraRigSave();
	}

	ImGui::SetItemTooltip("$app_data_root$\\camera_rig\\saved.ltx");

	const shared_str hud_sect = CameraRigActiveHudSection();
	ImGui::Text("Hud section: %s", hud_sect.size() ? hud_sect.c_str() : "-");

	ImGui::Separator();

	u32 count = 0;
	const SCameraRigParam* params = CCameraRig::ParamList(count);

	const char* group = nullptr;
	bool group_open = false;
	for (u32 i = 0; i < count; i++)
	{
		const SCameraRigParam& param = params[i];
		if (group == nullptr || xr_strcmp(group, param.Group) != 0)
		{
			group = param.Group;
			const char* reset_label = "Reset";
			const float reset_x = ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x - ImGui::CalcTextSize(reset_label).x - ImGui::GetStyle().FramePadding.x * 2.f;
			group_open = ImGui::CollapsingHeader(group, ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_AllowOverlap);

			ImGui::PushID(group);
			ImGui::SameLine(reset_x);
			if (ImGui::SmallButton(reset_label))
			{
				CCameraRig::ReloadGroup(group);
				changed = true;
			}

			ImGui::SetItemTooltip("Reset [%s] to config values", group);
			ImGui::PopID();
		}

		if (group_open)
		{
			changed |= CameraRigDrawParam(param, hud_sect);
		}
	}

	static bool refresh_preview = false;
	if (changed)
	{
		CameraRigApply();
		refresh_preview = true;
	}

	if (refresh_preview && !ImGui::IsAnyItemActive())
	{
		refresh_preview = false;
		HudMotionPlayerRefresh();
	}

	ImGui::End();
}
