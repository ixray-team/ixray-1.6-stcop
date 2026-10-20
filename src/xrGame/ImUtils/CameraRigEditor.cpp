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
	{ "left_hand_ik_follow", u8"с, как быстро хват левой руки догоняет свою анимированную позу после включения IK хвата; 0 = хват замирает при включении IK" },
	{ "left_hand_ik_blend", u8"с, за сколько левая рука подлетает к хвату (после перезарядки, доставания...) и отпускает его; 0 = сразу. Для оружия: left_hand_ik_blend в hud-секции" },
	{ "left_hand_finger_grip", u8"Сила сдвига пальцев левой руки к хвату рукоятки по метке mm_ik_fingers: пальцы двигаются как в анимации, но вокруг позы хвата (1 = база точно в хвате, 0.5 = посередине, 0 = выкл.). Для оружия: left_hand_finger_grip в hud-секции" },
	{ "left_hand_attach_blend", u8"с, плавность прикрепления левой руки к оружию и её отпускания (left_hand_attach_motions). Для оружия: left_hand_attach_blend в hud-секции" },

	{ "turn_sway_smooth", u8"с, сглаживание смещения HUD от наклона взгляда вверх-вниз (inertion_pitch_offset_*); ванильный сдвиг от поворота (inertion_origin_offset) с camera rig выключен, его заменяет look_sway_pos; 0 = выкл." },
	{ "inertia_speed", u8"Общая скорость пружин hud_move_*" },
	{ "inertia_damping", u8"Затухание пружин смещений hud_move_*: меньше 1 = перелёт и успокоение, 1 = без перелёта. Для оружия: inertia_damping в hud-секции" },
	{ "look_sway_pos", u8"м на 1 рад/с скорости поворота взгляда: x = оружие отстаёт вбок при повороте, y = вниз при взгляде вверх, z = назад при любом повороте. Поворот ствола от взгляда задают hud_*_inertia_k. Пружина: look_sway_inertia и look_sway_damping. Для оружия: look_sway_pos в hud-секции" },
	{ "aim_sway_scale", u8"Множитель look_sway_pos в прицеле. Для оружия: aim_sway_scale в hud-секции" },
	{ "look_sway_inertia", u8"с, мягкость реакции оружия на поворот взгляда (поворот ствола hud_*_inertia_k и смещение look_sway_pos): меньше = резче и короче, растёт с hud_mass; в прицеле делится на hud_zoom_inertia_speed / hud_inertia_speed; 0 = без инерции. Для оружия: look_sway_inertia в hud-секции" },
	{ "look_sway_damping", u8"Затухание пружины поворота взгляда: меньше 1 = перелёт и раскачка после остановки поворота, 1 = без перелёта. Для оружия: look_sway_damping в hud-секции" },

	{ "hud_inertialization", u8"с, модели рук и оружия переключают анимации сразу, а скачок позы гаснет примерно за это время (как в CoD); 0 = старый плавный переход" },
	{ "idle_fade", u8"с, плавный переход рук и оружия в anm_idle* (после выстрела, перезарядки, доставания...); 0 = как в анимациях. Для оружия: idle_fade в hud-секции" },
	{ "additive_blend_in", u8"с, плавность включения аддитивных анимаций (additive_motions). Для оружия: additive_blend_in в hud-секции" },
	{ "additive_blend_out", u8"с, плавность выключения аддитивных анимаций (additive_motions). Для оружия: additive_blend_out в hud-секции" },
	{ "aim_blend_inertia", u8"с, инерциализация при входе в прицел и выходе из него (anim_aim_start / anim_aim_end): скачок позы при прерывании (нажал и сразу отпустил) плавно гасится за это время; 0 = выкл. Для оружия: aim_blend_inertia в hud-секции" },

	{ "sprint_pose_inertia", u8"с, мягкость перехода между позой спринта и сейфмодом (уход сейфмода при входе в спринт, возврат при выходе); 0 = без инерции. Для оружия: sprint_pose_inertia в hud-секции" },
	{ "sprint_left_grip_range", u8"Насколько далеко левая рука в анимации спринта (left_hand_free_motions) может отойти от хвата, оставаясь прижатой к оружию: 1 = 10 см или 30°; рядом с хватом рука сидит точно на оружии, дальше становится свободной; 0 = рука всегда свободна. Для оружия: sprint_left_grip_range в hud-секции" },
	{ "sprint_left_cancel_return", u8"с, за сколько левая рука возвращается на рукоять после отмены sprint_start (из позы, где она была в момент отмены); работает вместе с sprint_left_grip_range; 0 = сразу. Для оружия: sprint_left_cancel_return в hud-секции" },
	{ "safemode_damping", u8"Затухание пружины смещения сейфмода (вход и выход, длительность = safemode_rotate_time) и перехода между позой спринта и сейфмодом: меньше 1 = перелёт и доводка, 1 = без перелёта. Для оружия: safemode_damping в hud-секции или секции оружия" },

	{ "aim_reload_motion", u8"Доля движения оружия из анимации перезарядки в прицеле (1 = как нарисовано); руки догоняют через IK. Для оружия: aim_reload_motion в hud-секции" },
	{ "aim_reload_blend", u8"с, плавность включения и выключения aim_reload_motion" },
	{ "aim_reload_tail", u8"Доля анимации перезарядки в её конце, за которую гашение снимается, чтобы она пришла в свою последнюю (idle) позу" },
	{ "aim_reload_hud_offset_pos", u8"м, смещение худа при перезарядке в прицеле (x вправо, y вверх, z от камеры); 0, 0, 0 = выкл. Для оружия: aim_reload_hud_offset_pos в hud-секции" },
	{ "aim_reload_hud_offset_rot", u8"Градусы, наклон худа при перезарядке в прицеле (x, y, z); 0, 0, 0 = выкл. Для оружия: aim_reload_hud_offset_rot в hud-секции" },
	{ "aim_reload_zoom_out", u8"Доля приближения прицела, которая снимается при перезарядке в прицеле (FOV отдаляется); 0 = выкл., 1 = FOV как без прицела. Для оружия: aim_reload_zoom_out в hud-секции" },
	{ "aim_reload_pose_inertia", u8"с, мягкость, с которой худ смещается и FOV отдаляется при перезарядке в прицеле; 0 = без сглаживания. Для оружия: aim_reload_pose_inertia в hud-секции" },
	{ "aim_reload_pose_inertia_out", u8"с, то же, но для возврата в конце перезарядки, а также для толчка вперёд (вход и возврат) и возврата опускания по y (меньше = резче); 0 = без сглаживания. Для оружия: aim_reload_pose_inertia_out в hud-секции" },
	{ "aim_reload_hud_push_pos", u8"м, резкий толчок оружия при перезарядке в прицеле (z = вперёд), отдельно от плавного смещения; 0, 0, 0 = выкл. Для оружия: aim_reload_hud_push_pos в hud-секции" },

	{ "left_hand_sway_look", u8"Градусы поворота свободной левой кисти (не прикреплённой к оружию, например в броске болта и гранат) вокруг запястья на 1 рад/с поворота взгляда: x = кивок от взгляда вверх-вниз, y = рыскание от поворота вбок, z = крен от поворота вбок (минус = наоборот). 0, 0, 0 = выкл. Для оружия: left_hand_sway_look в hud-секции" },
	{ "left_hand_sway_bob", u8"Градусы поворота свободной левой кисти на 1 см покачивания шагов (movement_layer_*): x = кивок от покачивания вверх-вниз, y = рыскание и z = крен от покачивания вбок. Для оружия: left_hand_sway_bob в hud-секции" },
	{ "left_hand_sway_jump", u8"Градусы, поза свободной левой кисти в прыжке: x = кивок вниз, y = рыскание, z = крен. Кисть уходит в неё при отрыве от земли, держит весь подъём и возвращается; при приземлении уходит в обратную сторону (сила по скорости падения). Скорость = hand_sway_inertia. 0, 0, 0 = выкл. Для оружия: left_hand_sway_jump в hud-секции" },
	{ "right_hand_sway_look", u8"Градусы поворота правой кисти вместе с предметом в ней (нож, болт, граната) вокруг запястья на 1 рад/с поворота взгляда: x = кивок от взгляда вверх-вниз, y = рыскание и z = крен от поворота вбок. HUD при поворотах вращается вокруг камеры и предмет в руке только смещается, это добавляет ему свой поворот. 0, 0, 0 = выкл. Для оружия: right_hand_sway_look в hud-секции" },
	{ "right_hand_sway_jump", u8"Градусы, поза правой кисти с предметом в прыжке, при приземлении в обратную сторону (как left_hand_sway_jump): x = кивок вниз, y = рыскание, z = крен. Для оружия: right_hand_sway_jump в hud-секции" },
	{ "left_hand_follow_lag", u8"с, свободная левая рука (нож, болт, гранаты) догоняет движение HUD на своей пружине с этим временем и перелётом hand_sway_damping: инерцию HUD она не копирует, а живо отстаёт и доводится по-своему; 0 = рука двигается вместе с HUD. Для оружия: left_hand_follow_lag в hud-секции" },
	{ "left_hand_follow_turn", u8"Доля инерции поворота камеры (поворот от hud_*_inertia_k и сдвиг look_sway_pos), которую повторяет свободная левая рука: 1 = как HUD, 0 = рука её не повторяет. Для оружия: left_hand_follow_turn в hud-секции" },
	{ "left_hand_follow_move", u8"Доля смещений hud_move_* (прыжок, падение, приземление, стрейф, присед), которую повторяет свободная левая рука, по осям: x = вбок, y = вверх-вниз, z = вперёд-назад; 1 = как HUD, 0 = рука по этой оси остаётся на месте. Не для прицела. Для оружия: left_hand_follow_move в hud-секции" },
	{ "left_hand_follow_move_rot", u8"То же для поворотов из hud_move_*: 1 = кисть наклоняется как HUD, 0 = не наклоняется. Для оружия: left_hand_follow_move_rot в hud-секции" },
	{ "left_hand_bob_pos", u8"Своё покачивание шагов свободной левой руки: множитель смещений movement_layer_* по осям x, y, z (1 = как HUD, -1 = в другую сторону, 0 = рука не качается). Для оружия: left_hand_bob_pos в hud-секции" },
	{ "left_hand_bob_rot", u8"То же для поворотов из movement_layer_* (x, y, z; 1 = как HUD). Для оружия: left_hand_bob_rot в hud-секции" },
	{ "hand_sway_inertia", u8"с, мягкость пружины поворота кистей (left_hand_sway_*, right_hand_sway_*) и скорость входа и выхода из позы прыжка (больше = ленивее); 0 = без инерции. Для оружия: hand_sway_inertia в hud-секции" },
	{ "hand_sway_damping", u8"Затухание пружины поворота кистей и отставания левой руки (left_hand_follow_lag): меньше 1 = перелёт и раскачка, 1 = без перелёта. Для оружия: hand_sway_damping в hud-секции" },

	{ "crouch_hud_offset_pos", u8"м, смещение худа в присяде (x вправо, y вверх, z от камеры); 0, 0, 0 = выкл. Не в прицеле. Для оружия: crouch_hud_offset_pos в hud-секции" },
	{ "crouch_hud_offset_rot", u8"Градусы, наклон худа в присяде (x, y, z; z = завал вбок, минус = влево); 0, 0, 0 = выкл. Для оружия: crouch_hud_offset_rot в hud-секции" },
	{ "crouch_pose_inertia", u8"с, мягкость входа в позу присяда и выхода из неё; 0 = без сглаживания. Для оружия: crouch_pose_inertia в hud-секции" },
	{ "crouch_pose_inertia_out", u8"с, мягкость выхода из позы присяда (при вставании и прицеле), меньше = быстрее; меньше 0 = как crouch_pose_inertia. Для оружия: crouch_pose_inertia_out в hud-секции" },
	{ "crouch_pose_delay", u8"с, пауза после приседания, прежде чем оружие начнёт уходить в позу присяда; при вставании и прицеле возврат без паузы. Для оружия: crouch_pose_delay в hud-секции" },
	{ "walk_hud_offset_pos", u8"м, смещение худа после долгой ходьбы (оружие расслабленно опускается); 0, 0, 0 = выкл. Для оружия: walk_hud_offset_pos в hud-секции" },
	{ "walk_hud_offset_rot", u8"Градусы, наклон худа после долгой ходьбы (x, y, z); 0, 0, 0 = выкл. Для оружия: walk_hud_offset_rot в hud-секции" },
	{ "walk_pose_delay", u8"с, сколько нужно идти без остановки, прицела, спринта и действий оружием (только idle), чтобы оружие перешло в позу ходьбы. Для оружия: walk_pose_delay в hud-секции" },
	{ "walk_pose_inertia", u8"с, мягкость перехода в позу ходьбы (больше = медленнее и ленивее). Для оружия: walk_pose_inertia в hud-секции" },
	{ "walk_pose_inertia_out", u8"с, как быстро оружие возвращается из позы ходьбы при остановке, прицеле, выстреле и т. п. (меньше = резче). Для оружия: walk_pose_inertia_out в hud-секции" },
	{ "carry_pose_damping", u8"Затухание пружин позы присяда и ходьбы: меньше 1 = небольшой перелёт и доводка (живее), 1 = без перелёта. Для оружия: carry_pose_damping в hud-секции" },

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
	{ "sprint_blend_in", u8"с, плавность включения камер спринта (camera_effects/weapon/<имя анимации sprint_start или sprint_end>.anm)" },
	{ "sprint_blend_out", u8"с, плавность выключения камер спринта" },

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

static xr_map<shared_str, xr_map<shared_str, Fvector>> g_camera_rig_hud_values;

static float* CameraRigHudField(attachable_hud_item* item, const char* key, u32& count)
{
	count = 0;
	return item != nullptr ? item->RigField(key, count) : nullptr;
}

static attachable_hud_item* CameraRigActiveHudItem(const shared_str& hud_sect)
{
	attachable_hud_item* item = g_player_hud != nullptr ? g_player_hud->attached_item(0) : nullptr;
	return item != nullptr && hud_sect.size() && item->m_sect_name == hud_sect ? item : nullptr;
}

static void CameraRigApplyHudValues(attachable_hud_item* item)
{
	const auto values = item != nullptr ? g_camera_rig_hud_values.find(item->m_sect_name) : g_camera_rig_hud_values.end();
	if (values == g_camera_rig_hud_values.end())
	{
		return;
	}

	for (const auto& [key, value] : values->second)
	{
		u32 count = 0;
		if (float* field = CameraRigHudField(item, key.c_str(), count))
		{
			for (u32 i = 0; i < count; i++)
			{
				field[i] = value[i];
			}
		}
	}
}

static void CameraRigApply()
{
	if (g_player_hud != nullptr)
	{
		g_player_hud->ReloadRigParams();
		CameraRigApplyHudValues(g_player_hud->attached_item(0));
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

	for (const auto& [sect, values] : g_camera_rig_hud_values)
	{
		for (const auto& [key, value] : values)
		{
			const SCameraRigParam* param = CCameraRig::FindHudParam(key.c_str());
			if (param != nullptr && param->Count == 3)
			{
				file.w_fvector3(sect.c_str(), key.c_str(), value);
			}
			else
			{
				file.w_float(sect.c_str(), key.c_str(), value.x);
			}
		}
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
		u32 field_count = 0;
		if (float* field = CameraRigHudField(CameraRigActiveHudItem(hud_sect), param.HudKey, field_count))
		{
			ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.f, 0.8f, 0.2f, 1.f));
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * (field_count == 3 ? 18.f : 8.f));
			const bool edited = field_count == 3 ? ImGui::DragFloat3("##hud", field, param.Speed, 0.f, 0.f, "%.4f") : ImGui::DragFloat("##hud", field, param.Speed, 0.f, 0.f, "%.4f");
			if (edited)
			{
				Fvector& value = g_camera_rig_hud_values[hud_sect][param.HudKey];
				value.set(field[0], field_count == 3 ? field[1] : 0.f, field_count == 3 ? field[2] : 0.f);
				changed = true;
			}

			ImGui::PopStyleColor();
			ImGui::SetItemTooltip("[%s] %s: this weapon uses this value, the global one is ignored", hud_sect.c_str(), param.HudKey);
		}
		else
		{
			ImGui::TextColored(ImVec4(1.f, 0.8f, 0.2f, 1.f), "[%s = %s]", param.HudKey, pSettings->r_string(hud_sect, param.HudKey));
			ImGui::SetItemTooltip("Overridden in hud section [%s]", hud_sect.c_str());
		}
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
	CameraRigApplyHudValues(CameraRigActiveHudItem(hud_sect));
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
