#include "StdAfx.h"
#include "ImUtils.h"
#include "../Actor.h"
#include "../ActorEffector.h"
#include "../CameraRig.h"
#include "../HudItem.h"
#include "../Inventory.h"
#include "../player_hud.h"
#include "../../Include/xrRender/animation_blend.h"
#include "../../xrEngine/SkeletonMotions.h"

static struct
{
	int			Selected = -1;
	CHudItem*	Item = nullptr;
	shared_str	Motion;
	shared_str	Camera;
	MotionID	ArmsMotion;
	MotionID	ItemMotion;
	float		OmfSpeed = 1.f;
	float		BaseSpeed = 1.f;
	float		ItemBaseSpeed = 1.f;
	float		Speed = 1.f;
	float		PrevTime = 0.f;
	bool		Playing = false;
	bool		Paused = false;
	bool		Keys = true;
}
imgui_hud_motion_player;

template <typename Func>
static CBlend* HudMotionPlayerFind(IKinematicsAnimated* model, MotionID motion, const Func& func)
{
	if (model == nullptr || !motion.valid())
	{
		return nullptr;
	}

	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		for (u32 i = 0, count = model->LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = model->LL_PartBlend(part, i);
			if (blend != nullptr && blend->motionID == motion && blend->blend_state() != CBlend::eFREE_SLOT && func(*blend))
			{
				return blend;
			}
		}
	}

	return nullptr;
}

static CBlend* HudMotionPlayerBlend(IKinematicsAnimated* model, MotionID motion)
{
	return HudMotionPlayerFind(model, motion, [](CBlend&) { return true; });
}

template <typename Func>
static void HudMotionPlayerForEach(IKinematicsAnimated* model, MotionID motion, const Func& func)
{
	HudMotionPlayerFind(model, motion, [&func](CBlend& blend)
	{
		func(blend);
		return false;
	});
}

static IKinematicsAnimated* HudMotionPlayerArmsModel(attachable_hud_item* item)
{
	return item->m_model_combined ? item->m_model->dcast_PKinematicsAnimated() : g_player_hud->GetModel();
}

static IKinematicsAnimated* HudMotionPlayerItemModel(attachable_hud_item* item)
{
	return item->m_model_combined ? nullptr : item->m_model->dcast_PKinematicsAnimated();
}

static bool HudMotionPlayerIsIdle(const shared_str& motion)
{
	return strstr(motion.c_str(), "anm_idle") == motion.c_str();
}

static bool HudMotionPlayerWeaponGrip(const shared_str& motion)
{
	return HudMotionPlayerIsIdle(motion) || strstr(motion.c_str(), "anm_shot") != nullptr || strstr(motion.c_str(), "anm_shoot") != nullptr;
}

static void HudMotionPlayerSetTime(attachable_hud_item* item, float normalized)
{
	auto& state = imgui_hud_motion_player;
	HudMotionPlayerForEach(HudMotionPlayerArmsModel(item), state.ArmsMotion, [normalized](CBlend& blend)
	{
		blend.timeCurrent = normalized * blend.timeTotal;
	});
	HudMotionPlayerForEach(HudMotionPlayerItemModel(item), state.ItemMotion, [normalized](CBlend& blend)
	{
		blend.timeCurrent = normalized * blend.timeTotal;
	});
}

static void HudMotionPlayerApplyPlayback(attachable_hud_item* item)
{
	auto& state = imgui_hud_motion_player;
	const bool playing = !state.Paused;
	const float speed = state.Speed;
	const float item_speed = state.BaseSpeed > EPS ? state.ItemBaseSpeed * state.Speed / state.BaseSpeed : state.Speed;

	HudMotionPlayerForEach(HudMotionPlayerArmsModel(item), state.ArmsMotion, [playing, speed](CBlend& blend)
	{
		blend.playing = playing;
		blend.speed = speed;
	});
	HudMotionPlayerForEach(HudMotionPlayerItemModel(item), state.ItemMotion, [playing, item_speed](CBlend& blend)
	{
		blend.playing = playing;
		blend.speed = item_speed;
	});
}

static void HudMotionPlayerSyncCamera(const CBlend& blend)
{
	auto& state = imgui_hud_motion_player;
	CActor* actor = Actor();
	if (actor == nullptr || state.Camera.size() == 0 || !CCameraRig::Enabled())
	{
		return;
	}

	const float omf = state.OmfSpeed > EPS ? state.OmfSpeed : 1.f;
	actor->Cameras().Rig().PreviewAnim(state.Camera.c_str(), crcMotion, blend.timeCurrent / omf, state.Paused ? 0.f : state.Speed / omf);
}

static void HudMotionPlayerStop()
{
	auto& state = imgui_hud_motion_player;
	state.Playing = false;
	state.Paused = false;
	state.ArmsMotion = MotionID();
	state.ItemMotion = MotionID();
	state.Camera = nullptr;

	if (g_player_hud != nullptr)
	{
		g_player_hud->PreviewGripBlocked = false;
	}
}

static void HudMotionPlayerIdle(CHudItem* hud_item)
{
	HudMotionPlayerStop();
	hud_item->PlayAnimIdle();
}

static void HudMotionPlayerPlay(CHudItem* hud_item, attachable_hud_item* item, const shared_str& motion, bool paused, EHudMixType mix = EHudMixType::eMixHands)
{
	auto& state = imgui_hud_motion_player;
	const player_hud_motion* anm = item->m_hand_motions.find_motion(motion);
	if (anm == nullptr || anm->m_animations.empty())
	{
		return;
	}

	if (hud_item->PlayHUDMotion_noCB(motion, mix, true) == 0)
	{
		return;
	}

	const motion_descr& descr = anm->m_animations[0];
	state.Item = hud_item;
	state.Motion = motion;
	state.Camera = shared_str().printf("camera_effects\\weapon\\%s.anm", descr.name.c_str());
	state.ArmsMotion = descr.mid;
	state.ItemMotion = MotionID();
	state.Playing = true;
	state.Paused = paused;
	state.PrevTime = 0.f;

	IKinematicsAnimated* arms = HudMotionPlayerArmsModel(item);
	CMotionDef* def = arms->LL_GetMotionDef(descr.mid);
	state.OmfSpeed = def != nullptr ? def->Speed() : 1.f;
	const CBlend* blend = HudMotionPlayerBlend(arms, descr.mid);
	state.BaseSpeed = blend != nullptr ? blend->speed : state.OmfSpeed;
	state.ItemBaseSpeed = state.BaseSpeed;
	state.Speed = state.BaseSpeed;

	if (IKinematicsAnimated* item_model = HudMotionPlayerItemModel(item))
	{
		const u32 count = item_model->LL_PartBlendsCount(0);
		if (count > 0)
		{
			const CBlend* item_blend = item_model->LL_PartBlend(0, count - 1);
			state.ItemMotion = item_blend->motionID;
			state.ItemBaseSpeed = item_blend->speed;
		}
	}

	g_player_hud->PreviewGripBlocked = !HudMotionPlayerWeaponGrip(motion);
	HudMotionPlayerApplyPlayback(item);
}

static float HudMotionPlayerMarkTime(float motion_time, float speed)
{
	return speed <= EPS || fsimilar(speed, 1.f) ? motion_time : motion_time / speed;
}

const player_hud_motion* HudMotionPlayerSelectedAnim(attachable_hud_item* item)
{
	const auto& state = imgui_hud_motion_player;
	const xr_vector<player_hud_motion>& motions = item->m_hand_motions.m_anims;
	if (state.Item != item->m_parent_hud_item || state.Selected < 0 || state.Selected >= int(motions.size()) || motions[state.Selected].m_animations.empty())
	{
		return nullptr;
	}

	return &motions[state.Selected];
}

bool& HudMotionPlayerKeys()
{
	return imgui_hud_motion_player.Keys;
}

void RenderHudMotionPlayerSpeed(attachable_hud_item* item)
{
	auto& state = imgui_hud_motion_player;
	const CBlend* blend = state.Playing && state.Item == item->m_parent_hud_item ? HudMotionPlayerBlend(HudMotionPlayerArmsModel(item), state.ArmsMotion) : nullptr;

	ImGui::BeginDisabled(blend == nullptr);
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 5.f);
	if (ImGui::DragFloat("Speed", &state.Speed, 0.01f, 0.001f, 100.f, "%.3f"))
	{
		state.Speed = std::max(state.Speed, 0.001f);
		HudMotionPlayerApplyPlayback(item);
	}

	ImGui::SetItemTooltip("Playback speed of this motion; the default is the OMF speed multiplied by the config speed");
	ImGui::SameLine();
	if (ImGui::SmallButton("Reset"))
	{
		state.Speed = state.BaseSpeed;
		HudMotionPlayerApplyPlayback(item);
	}
	ImGui::EndDisabled();
}

bool HudMotionPlayerCurrentMarkTime(attachable_hud_item* item, IKinematicsAnimated* model, MotionID motion, float& time)
{
	const auto& state = imgui_hud_motion_player;
	const CBlend* blend = state.Playing && state.Item == item->m_parent_hud_item ? HudMotionPlayerBlend(model, motion) : nullptr;
	CMotionDef* def = blend != nullptr ? model->LL_GetMotionDef(motion) : nullptr;
	if (def == nullptr)
	{
		return false;
	}

	time = HudMotionPlayerMarkTime(blend->timeCurrent, def->Speed());
	return true;
}

void HudMotionPlayerRefresh()
{
	auto& state = imgui_hud_motion_player;
	CActor* actor = Actor();
	PIItem active = actor != nullptr ? actor->inventory().ActiveItem() : nullptr;
	CHudItem* hud_item = active != nullptr ? active->cast_hud_item() : nullptr;
	if (!state.Playing || hud_item == nullptr || hud_item != state.Item || g_player_hud == nullptr)
	{
		return;
	}

	attachable_hud_item* item = hud_item->HudItemData();
	const CBlend* blend = item != nullptr ? HudMotionPlayerBlend(HudMotionPlayerArmsModel(item), state.ArmsMotion) : nullptr;
	if (blend == nullptr)
	{
		return;
	}

	const float normalized = blend->timeTotal > 0.f ? blend->timeCurrent / blend->timeTotal : 0.f;
	const bool paused = state.Paused;
	const float speed = state.Speed;
	const shared_str motion = state.Motion;

	HudMotionPlayerPlay(hud_item, item, motion, paused, EHudMixType::eNoMix);
	state.Speed = speed;
	HudMotionPlayerApplyPlayback(item);
	HudMotionPlayerSetTime(item, normalized);
}

void HudMotionPlayerClose()
{
	if (imgui_hud_motion_player.Playing)
	{
		HudMotionPlayerStop();
	}
}

static void HudMotionPlayerDrawMarks(attachable_hud_item* item, const ImVec2& pos, float width, float total_seconds)
{
	const CMotionDef* def = HudMotionPlayerArmsModel(item)->LL_GetMotionDef(imgui_hud_motion_player.ArmsMotion);
	if (def == nullptr || def->marks.empty())
	{
		return;
	}

	const ImGuiStyle& style = ImGui::GetStyle();
	const float height = ImGui::GetFrameHeight();
	const float padding = 2.f;
	const float usable = width - padding * 2.f - style.GrabMinSize;
	const float start = pos.x + padding + style.GrabMinSize * 0.5f;
	const float radius = ImGui::GetFontSize() * 0.18f;
	const float y = pos.y + height - radius - 1.f;
	const ImVec2 mouse = ImGui::GetIO().MousePos;
	ImDrawList* draw = ImGui::GetWindowDrawList();

	xr_string hovered;
	for (const motion_marks& marks : def->marks)
	{
		for (const motion_marks::interval& interval : marks.intervals)
		{
			const float x = start + usable * std::clamp(interval.first / total_seconds, 0.f, 1.f);
			draw->AddCircleFilled(ImVec2(x, y), radius, IM_COL32(230, 40, 40, 255));

			if (std::abs(mouse.x - x) <= radius * 2.f && mouse.y >= pos.y && mouse.y <= pos.y + height)
			{
				if (!hovered.empty())
				{
					hovered += "\n";
				}

				hovered += marks.name.c_str();
			}
		}
	}

	if (!hovered.empty() && !ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		ImGui::SetTooltip("%s", hovered.c_str());
	}
}

void RenderHudMotionPlayer()
{
	auto& state = imgui_hud_motion_player;

	CActor* actor = Actor();
	PIItem active = actor != nullptr ? actor->inventory().ActiveItem() : nullptr;
	CHudItem* hud_item = active != nullptr ? active->cast_hud_item() : nullptr;
	attachable_hud_item* item = hud_item != nullptr && g_player_hud != nullptr ? hud_item->HudItemData() : nullptr;

	if (item == nullptr)
	{
		if (state.Playing)
		{
			HudMotionPlayerStop();
		}

		ImGui::TextDisabled("No active HUD item");
		return;
	}

	if (state.Item != hud_item)
	{
		HudMotionPlayerStop();
		state.Item = hud_item;
		state.Selected = -1;
	}

	const xr_vector<player_hud_motion>& motions = item->m_hand_motions.m_anims;
	if (state.Selected >= int(motions.size()))
	{
		state.Selected = -1;
	}

	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 14.f);
	if (ImGui::BeginCombo("##motion", state.Selected >= 0 ? motions[state.Selected].m_alias_name.c_str() : "-"))
	{
		for (int i = 0; i < int(motions.size()); ++i)
		{
			const bool selected = i == state.Selected;
			if (ImGui::Selectable(motions[i].m_alias_name.c_str(), selected))
			{
				state.Selected = i;
				HudMotionPlayerPlay(hud_item, item, motions[i].m_alias_name, true);
			}

			if (selected)
			{
				ImGui::SetItemDefaultFocus();
			}
		}

		ImGui::EndCombo();
	}

	ImGui::SameLine();
	ImGui::BeginDisabled(state.Selected < 0);
	if (ImGui::Button(state.Playing && !state.Paused ? "Pause" : "Play"))
	{
		const shared_str& motion = motions[state.Selected].m_alias_name;
		CBlend* current = state.Playing && state.Motion == motion ? HudMotionPlayerBlend(HudMotionPlayerArmsModel(item), state.ArmsMotion) : nullptr;
		if (current == nullptr || state.Paused && current->timeCurrent >= current->timeTotal - END_EPS - EPS_L)
		{
			const float speed = current != nullptr ? state.Speed : -1.f;
			HudMotionPlayerPlay(hud_item, item, motion, false);
			if (speed > 0.f)
			{
				state.Speed = speed;
				HudMotionPlayerApplyPlayback(item);
			}
		}
		else
		{
			state.Paused = !state.Paused;
			HudMotionPlayerApplyPlayback(item);
		}
	}
	ImGui::EndDisabled();

	ImGui::SameLine();
	if (ImGui::Button("Idle"))
	{
		HudMotionPlayerIdle(hud_item);
	}

	ImGui::SetItemTooltip("Play the idle motion now");
	ImGui::SameLine();
	if (ImGui::RadioButton("Keys", state.Keys))
	{
		state.Keys = true;
	}

	ImGui::SameLine();
	if (ImGui::RadioButton("Seconds", !state.Keys))
	{
		state.Keys = false;
	}

	CBlend* blend = state.Playing ? HudMotionPlayerBlend(HudMotionPlayerArmsModel(item), state.ArmsMotion) : nullptr;
	if (state.Playing && blend == nullptr)
	{
		HudMotionPlayerStop();
	}

	ImGui::BeginDisabled(blend == nullptr);
	const float total = blend != nullptr ? blend->timeTotal : 0.f;
	const float total_keys = total * SAMPLE_FPS;
	float key = blend != nullptr ? blend->timeCurrent * SAMPLE_FPS : 0.f;

	const float seconds = HudMotionPlayerMarkTime(key / SAMPLE_FPS, state.OmfSpeed);
	const float total_seconds = HudMotionPlayerMarkTime(total, state.OmfSpeed);

	const shared_str position = state.Keys ? shared_str().printf("%.3f s", seconds) : shared_str().printf("%.1f key", key);
	const float position_width = ImGui::CalcTextSize(state.Keys ? "000.000 s" : "0000.0 key").x;

	const bool step_back = ImGui::ArrowButton("##step_back", ImGuiDir_Left);
	ImGui::SetItemTooltip("Previous key");
	ImGui::SameLine();
	const ImVec2 slider_pos = ImGui::GetCursorScreenPos();
	const float slider_width = ImGui::GetContentRegionAvail().x - ImGui::GetFrameHeight() - position_width - ImGui::GetStyle().ItemSpacing.x * 2.f;
	ImGui::SetNextItemWidth(slider_width);
	float value = state.Keys ? key : seconds;
	const float value_max = state.Keys ? total_keys : total_seconds;
	const shared_str format = state.Keys ? shared_str().printf("%%.1f / %.0f", total_keys) : shared_str().printf("%%.3f / %.3f s", total_seconds);
	const bool scrubbed = ImGui::SliderFloat("##position", &value, 0.f, std::max(value_max, EPS_L), format.c_str());
	ImGui::SameLine();
	const bool step_forward = ImGui::ArrowButton("##step_forward", ImGuiDir_Right);
	ImGui::SetItemTooltip("Next key");
	ImGui::SameLine();
	ImGui::TextUnformatted(position.c_str());

	if (scrubbed || step_back || step_forward)
	{
		if (!state.Paused)
		{
			state.Paused = true;
			HudMotionPlayerApplyPlayback(item);
		}

		if (scrubbed)
		{
			HudMotionPlayerSetTime(item, value_max > 0.f ? std::clamp(value / value_max, 0.f, 1.f) : 0.f);
		}
		else
		{
			const float stepped = std::clamp(step_back ? ceilf(key - EPS_L) - 1.f : floorf(key + EPS_L) + 1.f, 0.f, total_keys);
			HudMotionPlayerSetTime(item, total_keys > 0.f ? stepped / total_keys : 0.f);
		}
	}

	if (blend != nullptr && total_seconds > 0.f)
	{
		HudMotionPlayerDrawMarks(item, slider_pos, slider_width, total_seconds);
	}

	if (state.Keys)
	{
		ImGui::Text("Mark key: %.2f", seconds * SAMPLE_FPS);
	}
	else
	{
		ImGui::Text("Mark time: %.3f s", seconds);
	}

	ImGui::SetItemTooltip("Value for the motion mark at this frame: key / OMF speed (the key itself at speed 1)");

	ImGui::EndDisabled();

	if (blend != nullptr)
	{
		HudMotionPlayerSyncCamera(*blend);
	}

	if (blend != nullptr && !state.Paused)
	{
		const bool finished = blend->timeCurrent >= blend->timeTotal - END_EPS - EPS_L || blend->timeCurrent < state.PrevTime - EPS_L;
		if (finished && !HudMotionPlayerIsIdle(state.Motion))
		{
			HudMotionPlayerIdle(hud_item);
			blend = nullptr;
		}
	}

	state.PrevTime = blend != nullptr ? blend->timeCurrent : 0.f;
}
