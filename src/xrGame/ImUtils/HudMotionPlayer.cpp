#include "StdAfx.h"
#include "ImUtils.h"
#include "../Actor.h"
#include "../CameraRig.h"
#include "../HudItem.h"
#include "../Inventory.h"
#include "../player_hud.h"
#include "../../Include/xrRender/animation_blend.h"
#include "../../xrEngine/SkeletonMotions.h"

static struct
{
	bool		Open = false;
	int			Selected = -1;
	CHudItem*	Item = nullptr;
	shared_str	Motion;
	MotionID	ArmsMotion;
	MotionID	ItemMotion;
	float		PrevTime = 0.f;
	float		MarkTime = -1.f;
	bool		Playing = false;
	bool		Paused = false;
	bool		ReleaseGrip = true;
	bool		ReturnToIdle = true;
	float		TimeScale = 1.f;
	float		SavedTimeScale = 1.f;
	bool		TimeScaled = false;
}
imgui_hud_motion_player;

void HudMotionPlayerOpen()
{
	imgui_hud_motion_player.Open = true;
}

static CBlend* HudMotionPlayerBlend(IKinematicsAnimated* model, MotionID motion)
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
			if (blend != nullptr && blend->motionID == motion && blend->blend_state() != CBlend::eFREE_SLOT)
			{
				return blend;
			}
		}
	}

	return nullptr;
}

template <typename Func>
static void HudMotionPlayerForEach(IKinematicsAnimated* model, MotionID motion, const Func& func)
{
	if (model == nullptr || !motion.valid())
	{
		return;
	}

	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		for (u32 i = 0, count = model->LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = model->LL_PartBlend(part, i);
			if (blend != nullptr && blend->motionID == motion && blend->blend_state() != CBlend::eFREE_SLOT)
			{
				func(*blend);
			}
		}
	}
}

static IKinematicsAnimated* HudMotionPlayerArmsModel(attachable_hud_item* item)
{
	return item->m_model_combined ? item->m_model->dcast_PKinematicsAnimated() : g_player_hud->GetModel();
}

static IKinematicsAnimated* HudMotionPlayerItemModel(attachable_hud_item* item)
{
	return item->m_model_combined ? nullptr : item->m_model->dcast_PKinematicsAnimated();
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

static void HudMotionPlayerSetPlaying(attachable_hud_item* item, bool playing)
{
	auto& state = imgui_hud_motion_player;
	HudMotionPlayerForEach(HudMotionPlayerArmsModel(item), state.ArmsMotion, [playing](CBlend& blend)
	{
		blend.playing = playing;
	});
	HudMotionPlayerForEach(HudMotionPlayerItemModel(item), state.ItemMotion, [playing](CBlend& blend)
	{
		blend.playing = playing;
	});
}

static void HudMotionPlayerStop()
{
	auto& state = imgui_hud_motion_player;
	state.Playing = false;
	state.Paused = false;
	state.ArmsMotion = MotionID();
	state.ItemMotion = MotionID();

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

static void HudMotionPlayerPlay(CHudItem* hud_item, attachable_hud_item* item, const shared_str& motion, bool paused)
{
	auto& state = imgui_hud_motion_player;
	const player_hud_motion* anm = item->m_hand_motions.find_motion(motion);
	if (anm == nullptr || anm->m_animations.empty())
	{
		return;
	}

	if (hud_item->PlayHUDMotion_noCB(motion, EHudMixType::eMixHands, true) == 0)
	{
		return;
	}

	state.Item = hud_item;
	state.Motion = motion;
	state.ArmsMotion = anm->m_animations[0].mid;
	state.ItemMotion = MotionID();
	state.Playing = true;
	state.Paused = paused;
	state.PrevTime = 0.f;
	state.MarkTime = -1.f;

	if (IKinematicsAnimated* item_model = HudMotionPlayerItemModel(item))
	{
		const u32 count = item_model->LL_PartBlendsCount(0);
		if (count > 0)
		{
			state.ItemMotion = item_model->LL_PartBlend(0, count - 1)->motionID;
		}
	}

	const bool idle = strstr(motion.c_str(), "anm_idle") == motion.c_str();
	g_player_hud->PreviewGripBlocked = state.ReleaseGrip && !idle;

	if (paused)
	{
		HudMotionPlayerSetPlaying(item, false);
	}
}

static void HudMotionPlayerMarks(attachable_hud_item* item, const CBlend& blend)
{
	auto& state = imgui_hud_motion_player;
	const CMotionDef* def = HudMotionPlayerArmsModel(item)->LL_GetMotionDef(state.ArmsMotion);
	if (def == nullptr || def->marks.empty())
	{
		state.MarkTime = -1.f;
		return;
	}

	const float time = blend.timeCurrent / (blend.speed > EPS ? blend.speed : 1.f);
	float prev = state.MarkTime;
	if (prev >= 0.f && time < prev - EPS_L)
	{
		g_player_hud->OnHudMotionStarted(state.Motion);
		prev = -1.f;
	}

	if (prev < 0.f || time > prev)
	{
		for (const motion_marks& marks : def->marks)
		{
			for (const motion_marks::interval& interval : marks.intervals)
			{
				if ((prev < 0.f ? interval.first >= 0.f : interval.first > prev) && interval.first <= time)
				{
					g_player_hud->OnHudMotionMark(item, marks.name);
				}
			}
		}
	}

	state.MarkTime = time;
}

static void HudMotionPlayerSetTimeScale(float scale)
{
	auto& state = imgui_hud_motion_player;
	if (!state.TimeScaled)
	{
		state.SavedTimeScale = Device.time_factor();
		state.TimeScaled = true;
	}

	state.TimeScale = scale;
	Device.time_factor(scale);
}

static void HudMotionPlayerRestoreTimeScale()
{
	auto& state = imgui_hud_motion_player;
	if (state.TimeScaled)
	{
		Device.time_factor(state.SavedTimeScale);
		state.TimeScaled = false;
	}

	state.TimeScale = 1.f;
}

void RenderHudMotionPlayer()
{
	auto& state = imgui_hud_motion_player;
	if (!state.Open)
	{
		if (state.Playing || state.TimeScaled)
		{
			HudMotionPlayerStop();
			HudMotionPlayerRestoreTimeScale();
		}

		return;
	}

	if (!ImGui::Begin("HUD Motion Player", &state.Open))
	{
		ImGui::End();
		return;
	}

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
		ImGui::End();
		return;
	}

	if (state.Item != hud_item)
	{
		HudMotionPlayerStop();
		state.Item = hud_item;
		state.Selected = -1;
	}

	ImGui::Text("Item: %s", item->m_sect_name.c_str());

	const xr_vector<player_hud_motion>& motions = item->m_hand_motions.m_anims;
	if (state.Selected >= int(motions.size()))
	{
		state.Selected = -1;
	}

	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 18.f);
	if (ImGui::BeginCombo("Motion", state.Selected >= 0 ? motions[state.Selected].m_alias_name.c_str() : "-"))
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

	ImGui::BeginDisabled(state.Selected < 0);
	if (ImGui::Button(state.Playing && !state.Paused ? "Pause" : "Play"))
	{
		const shared_str& motion = motions[state.Selected].m_alias_name;
		CBlend* current = state.Playing && state.Motion == motion ? HudMotionPlayerBlend(HudMotionPlayerArmsModel(item), state.ArmsMotion) : nullptr;
		if (current == nullptr || state.Paused && current->timeCurrent >= current->timeTotal - END_EPS - EPS_L)
		{
			HudMotionPlayerPlay(hud_item, item, motion, false);
		}
		else
		{
			state.Paused = !state.Paused;
			HudMotionPlayerSetPlaying(item, !state.Paused);
		}
	}
	ImGui::EndDisabled();

	ImGui::SameLine();
	ImGui::BeginDisabled(state.Selected < 0);
	if (ImGui::Button("Restart"))
	{
		HudMotionPlayerPlay(hud_item, item, motions[state.Selected].m_alias_name, false);
	}
	ImGui::EndDisabled();

	ImGui::SameLine();
	if (ImGui::Button("Idle"))
	{
		HudMotionPlayerIdle(hud_item);
	}

	ImGui::SetItemTooltip("Play the idle motion now, the left hand flies to the grip");

	ImGui::Checkbox("Release grip during motion", &state.ReleaseGrip);
	ImGui::SetItemTooltip("Left hand grip IK is off while a non idle motion plays, as in reload or draw");
	ImGui::Checkbox("Return to idle at end", &state.ReturnToIdle);

	CBlend* blend = state.Playing ? HudMotionPlayerBlend(HudMotionPlayerArmsModel(item), state.ArmsMotion) : nullptr;
	if (state.Playing && blend == nullptr)
	{
		HudMotionPlayerStop();
	}

	float time = blend != nullptr ? blend->timeCurrent : 0.f;
	const float total = blend != nullptr ? blend->timeTotal : 0.f;

	ImGui::BeginDisabled(blend == nullptr);
	ImGui::SetNextItemWidth(-ImGui::GetFontSize() * 4.f);
	if (ImGui::SliderFloat("Time", &time, 0.f, std::max(total, EPS_L), "%.3f s"))
	{
		if (!state.Paused)
		{
			state.Paused = true;
			HudMotionPlayerSetPlaying(item, false);
		}

		HudMotionPlayerSetTime(item, total > 0.f ? std::clamp(time / total, 0.f, 1.f) : 0.f);
	}
	ImGui::EndDisabled();

	if (blend != nullptr)
	{
		HudMotionPlayerMarks(item, *blend);
	}

	if (blend != nullptr && !state.Paused)
	{
		const bool finished = blend->timeCurrent >= blend->timeTotal - END_EPS - EPS_L || blend->timeCurrent < state.PrevTime - EPS_L;
		const bool idle = strstr(state.Motion.c_str(), "anm_idle") == state.Motion.c_str();
		if (finished && state.ReturnToIdle && !idle)
		{
			HudMotionPlayerIdle(hud_item);
			blend = nullptr;
		}
	}

	state.PrevTime = blend != nullptr ? blend->timeCurrent : 0.f;

	float scale = state.TimeScale;
	ImGui::SetNextItemWidth(-ImGui::GetFontSize() * 4.f);
	if (ImGui::SliderFloat("Speed", &scale, 0.02f, 1.f, "%.2f", ImGuiSliderFlags_Logarithmic))
	{
		HudMotionPlayerSetTimeScale(scale);
	}

	ImGui::SetItemTooltip("Game time scale, slows the whole game");
	ImGui::SameLine();
	if (ImGui::SmallButton("1x"))
	{
		HudMotionPlayerRestoreTimeScale();
	}

	ImGui::Separator();
	const float grip = g_player_hud->LeftGripWeight();
	ImGui::ProgressBar(grip, ImVec2(-ImGui::GetFontSize() * 4.f, 0.f), shared_str().printf("%.2f", grip).c_str());
	ImGui::SameLine();
	ImGui::TextUnformatted("Grip");
	ImGui::Text("Fingers: %s", g_player_hud->LeftGripFingersFrozen() ? "frozen" : "animation");

	ImGui::End();
}
