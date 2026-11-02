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
	MotionID	AdditiveMotion;
	MotionID	AdditiveReference;
	shared_str	AdditiveName;
	shared_str	AdditiveReferenceName;
	string64	AdditiveFilter = {};
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

constexpr float HudMotionPlayerAdditiveEnterTime = 0.1f;
constexpr float HudMotionPlayerAdditiveExitTime = 0.3f;

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
	CBlend* active = HudMotionPlayerFind(model, motion, [](CBlend& blend) { return blend.blend_state() != CBlend::eFalloff; });
	return active != nullptr ? active : HudMotionPlayerFind(model, motion, [](CBlend&) { return true; });
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

static void HudMotionPlayerSmoothAdditiveExit(IKinematicsAnimated* Arms)
{
	constexpr float Rate = 1.f / HudMotionPlayerAdditiveExitTime;
	for (u16 Part = 0, Count = Arms->partitions().count(); Part < Count; ++Part)
	{
		for (u32 i = 0, Blends = Arms->LL_PartBlendsCount(Part); i < Blends; ++i)
		{
			CBlend* Blend = Arms->LL_PartBlend(Part, i);
			if (Blend == nullptr || Blend->channel != ADDITIVE_CHANNEL || Blend->blend_state() == CBlend::eFREE_SLOT)
			{
				continue;
			}

			if (Blend->blend_state() == CBlend::eFalloff)
			{
				Blend->blendFalloff = Rate;
			}
			else
			{
				Blend->blendAmount = EPS_S;
				Blend->blendAccrue = Rate;
			}
		}
	}
}

static void HudMotionPlayerClearAdditive()
{
	auto& state = imgui_hud_motion_player;
	state.AdditiveMotion = MotionID();
	state.AdditiveReference = MotionID();
	state.AdditiveName = nullptr;
	state.AdditiveReferenceName = nullptr;
}

static void HudMotionPlayerIdle(CHudItem* hud_item)
{
	auto& state = imgui_hud_motion_player;
	const bool additive = state.Playing && state.AdditiveMotion.valid() && state.ArmsMotion == state.AdditiveMotion;
	HudMotionPlayerStop();
	hud_item->PlayAnimIdle();

	if (additive && g_player_hud != nullptr && g_player_hud->GetModel() != nullptr)
	{
		HudMotionPlayerSmoothAdditiveExit(g_player_hud->GetModel());
	}
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

static void HudMotionPlayerPlayAdditive(CHudItem* hud_item, attachable_hud_item* item, bool paused)
{
	auto& state = imgui_hud_motion_player;
	IKinematicsAnimated* Arms = g_player_hud->GetModel();
	CMotionDef* Def = Arms != nullptr && !item->m_model_combined && state.AdditiveMotion.valid() ? Arms->LL_GetMotionDef(state.AdditiveMotion) : nullptr;
	const shared_str Base = hud_item->SetCurrentStateAnimation(hud_item->SetCurrentIdleAnimation());
	if (Def == nullptr || !hud_item->HudAnimationExist(Base))
	{
		return;
	}

	hud_item->PlayHUDMotion_noCB(Base, EHudMixType::eMixHands, true);

	constexpr float Rate = 1.f / HudMotionPlayerAdditiveEnterTime;
	for (u16 Part = 0, Count = Arms->partitions().count(); Part < Count; ++Part)
	{
		Arms->LL_FadeCycle(Part, Rate, 1 << ADDITIVE_CHANNEL);
		if (CBlend* Blend = Arms->LL_PlayCycle(Part, state.AdditiveMotion, false, 1.f, Rate, Def->Speed(), true, nullptr, nullptr, ADDITIVE_CHANNEL))
		{
			Blend->additive_ref = state.AdditiveReference;
			Blend->fall_at_end = false;
		}
	}

	item->AdditivePlaying = true;
	item->AdditiveOverlay = false;
	item->RetargetPlaying = false;
	item->RetargetLayerPlaying = false;

	state.Item = hud_item;
	state.Motion = state.AdditiveName;
	state.Camera = nullptr;
	state.ArmsMotion = state.AdditiveMotion;
	state.ItemMotion = MotionID();
	state.Playing = true;
	state.Paused = paused;
	state.PrevTime = 0.f;
	state.OmfSpeed = Def->Speed();
	state.BaseSpeed = state.OmfSpeed;
	state.ItemBaseSpeed = state.OmfSpeed;
	state.Speed = state.OmfSpeed;

	g_player_hud->PreviewGripBlocked = false;
	HudMotionPlayerApplyPlayback(item);
}

static void HudMotionPlayerStart(CHudItem* hud_item, attachable_hud_item* item, const shared_str& motion, bool paused, EHudMixType mix = EHudMixType::eMixHands)
{
	if (imgui_hud_motion_player.AdditiveMotion.valid())
	{
		HudMotionPlayerPlayAdditive(hud_item, item, paused);
	}
	else
	{
		HudMotionPlayerPlay(hud_item, item, motion, paused, mix);
	}
}

static xr_vector<motion_descr> HudMotionPlayerConfigMotions(attachable_hud_item* Item, IKinematicsAnimated* Model, const char* Filter)
{
	xr_vector<motion_descr> Motions;
	const auto Add = [&](const shared_str& Name, MotionID Motion)
	{
		const bool Listed = std::any_of(Motions.begin(), Motions.end(), [&](const motion_descr& It) { return It.mid == Motion; });
		if (!Listed && Motion.valid() && (Filter[0] == 0 || strstr(Name.c_str(), Filter) != nullptr))
		{
			Motions.push_back({ Motion, Name });
		}
	};

	for (const player_hud_motion& Anm : Item->m_hand_motions.m_anims)
	{
		for (const motion_descr& Descr : Anm.m_animations)
		{
			Add(Descr.name, Descr.mid);
		}
	}

	for (const auto& [Alias, Layer] : Item->RetargetLayers)
	{
		Add(Layer.Motion, Model->ID_Cycle_Safe(Layer.Motion));
		if (Layer.Reference.size() != 0)
		{
			Add(Layer.Reference, Model->ID_Cycle_Safe(Layer.Reference));
		}
	}

	std::sort(Motions.begin(), Motions.end(), [](const motion_descr& A, const motion_descr& B) { return xr_strcmp(A.name, B.name) < 0; });
	return Motions;
}

static bool HudMotionPlayerAdditiveCombo(const char* Id, attachable_hud_item* Item, IKinematicsAnimated* Model, shared_str& Name, MotionID& Motion, const char* None)
{
	auto& state = imgui_hud_motion_player;
	bool Changed = false;
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 14.f);
	if (!ImGui::BeginCombo(Id, Name.size() != 0 ? Name.c_str() : None))
	{
		return false;
	}

	ImGui::SetNextItemWidth(-FLT_MIN);
	ImGui::InputTextWithHint("##filter", "Filter", state.AdditiveFilter, sizeof(state.AdditiveFilter));
	if (ImGui::Selectable(None, Name.size() == 0))
	{
		Name = nullptr;
		Motion = MotionID();
		Changed = true;
	}

	for (const motion_descr& Descr : HudMotionPlayerConfigMotions(Item, Model, state.AdditiveFilter))
	{
		ImGui::PushID(int(Descr.mid.val));
		if (ImGui::Selectable(Descr.name.c_str(), Descr.mid == Motion))
		{
			Name = Descr.name;
			Motion = Descr.mid;
			Changed = true;
		}

		ImGui::PopID();
	}

	ImGui::EndCombo();
	return Changed;
}

static void HudMotionPlayerApplyLayerReference(attachable_hud_item* Item)
{
	auto& state = imgui_hud_motion_player;
	for (const auto& [Alias, Layer] : Item->RetargetLayers)
	{
		if (Layer.Motion == state.AdditiveName && Layer.Reference.size() != 0)
		{
			state.AdditiveReferenceName = Layer.Reference;
			state.AdditiveReference = g_player_hud->GetModel()->ID_Cycle_Safe(Layer.Reference);
			return;
		}
	}
}

static void RenderHudMotionPlayerAdditive(CHudItem* HudItem, attachable_hud_item* Item)
{
	auto& state = imgui_hud_motion_player;
	IKinematicsAnimated* Model = g_player_hud->GetModel();
	if (HudMotionPlayerAdditiveCombo("Additive##motion", Item, Model, state.AdditiveName, state.AdditiveMotion, "-##additive"))
	{
		state.Selected = -1;
		if (state.AdditiveMotion.valid())
		{
			HudMotionPlayerApplyLayerReference(Item);
			HudMotionPlayerPlayAdditive(HudItem, Item, true);
		}
		else
		{
			HudMotionPlayerIdle(HudItem);
		}
	}

	ImGui::SetItemTooltip("Hands motions from this weapon's hud config and retarget sets, played additively over the current idle");
	if (HudMotionPlayerAdditiveCombo("Reference##additive", Item, Model, state.AdditiveReferenceName, state.AdditiveReference, "First frame##reference") && state.AdditiveMotion.valid())
	{
		HudMotionPlayerPlayAdditive(HudItem, Item, state.Paused);
	}

	ImGui::SetItemTooltip("Pose the additive delta is taken from: the first frame of the motion, or another motion's first frame (same as retarget_reference)");
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

bool HudMotionPlayerSelectedAdditive(attachable_hud_item* item, motion_descr& motion)
{
	const auto& state = imgui_hud_motion_player;
	if (state.Item != item->m_parent_hud_item || !state.AdditiveMotion.valid())
	{
		return false;
	}

	motion.mid = state.AdditiveMotion;
	motion.name = state.AdditiveName;
	return true;
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

	HudMotionPlayerStart(hud_item, item, motion, paused, EHudMixType::eNoMix);
	state.Speed = speed;
	HudMotionPlayerApplyPlayback(item);
	HudMotionPlayerSetTime(item, normalized);
}

void HudMotionPlayerClose()
{
	auto& state = imgui_hud_motion_player;
	if (!state.Playing)
	{
		return;
	}

	CActor* actor = state.AdditiveMotion.valid() ? Actor() : nullptr;
	PIItem active = actor != nullptr ? actor->inventory().ActiveItem() : nullptr;
	CHudItem* hud_item = active != nullptr ? active->cast_hud_item() : nullptr;
	if (hud_item != nullptr && hud_item == state.Item && hud_item->HudItemData() != nullptr)
	{
		HudMotionPlayerIdle(hud_item);
	}
	else
	{
		HudMotionPlayerStop();
	}
}

ImU32 HudMotionPlayerMarkColor(const shared_str& name)
{
	static constexpr ImU32 colors[] =
	{
		IM_COL32(235, 70, 70, 255),
		IM_COL32(80, 170, 255, 255),
		IM_COL32(110, 215, 90, 255),
		IM_COL32(245, 185, 50, 255),
		IM_COL32(195, 115, 245, 255),
		IM_COL32(60, 215, 205, 255),
		IM_COL32(245, 125, 195, 255),
		IM_COL32(240, 140, 60, 255),
		IM_COL32(170, 200, 90, 255),
		IM_COL32(150, 150, 255, 255),
	};

	static xr_hash_map<shared_str, u32> indices;
	const u32 index = indices.try_emplace(name, u32(indices.size())).first->second;
	return colors[index % std::size(colors)];
}

bool HudMotionPlayerSeekMark(attachable_hud_item* item, IKinematicsAnimated* model, MotionID motion, float mark_time)
{
	auto& state = imgui_hud_motion_player;
	const xr_vector<player_hud_motion>& motions = item->m_hand_motions.m_anims;
	const bool additive = state.AdditiveMotion.valid();
	if (state.Item != item->m_parent_hud_item || (!additive && (state.Selected < 0 || state.Selected >= int(motions.size()))))
	{
		return false;
	}

	if (!state.Playing)
	{
		HudMotionPlayerStart(item->m_parent_hud_item, item, additive ? state.AdditiveName : motions[state.Selected].m_alias_name, true);
	}

	const CBlend* blend = HudMotionPlayerBlend(model, motion);
	CMotionDef* def = blend != nullptr ? model->LL_GetMotionDef(motion) : nullptr;
	if (def == nullptr || blend->timeTotal <= EPS)
	{
		return false;
	}

	if (!state.Paused)
	{
		state.Paused = true;
		HudMotionPlayerApplyPlayback(item);
	}

	const float speed = def->Speed();
	const float time = speed <= EPS || fsimilar(speed, 1.f) ? mark_time : mark_time * speed;
	HudMotionPlayerSetTime(item, std::clamp(time / blend->timeTotal, 0.f, 1.f));
	return true;
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
	const auto hover = [&](float x, const motion_marks& marks, const char* edge)
	{
		if (std::abs(mouse.x - x) <= radius * 2.f && mouse.y >= pos.y && mouse.y <= pos.y + height)
		{
			if (!hovered.empty())
			{
				hovered += "\n";
			}

			hovered += marks.name.c_str();
			hovered += edge;
		}
	};

	for (const motion_marks& marks : def->marks)
	{
		const ImU32 color = HudMotionPlayerMarkColor(marks.name);
		for (const motion_marks::interval& interval : marks.intervals)
		{
			const float x = start + usable * std::clamp(interval.first / total_seconds, 0.f, 1.f);
			draw->AddCircleFilled(ImVec2(x, y), radius, color);
			hover(x, marks, " (start)");

			if (interval.second > interval.first + EPS_L && interval.second < total_seconds - EPS_L)
			{
				const float end = start + usable * std::clamp(interval.second / total_seconds, 0.f, 1.f);
				draw->AddCircle(ImVec2(end, y), radius, color, 0, 1.5f);
				hover(end, marks, " (end)");
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
		HudMotionPlayerClearAdditive();
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
				HudMotionPlayerClearAdditive();
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
	const bool additive = state.AdditiveMotion.valid();
	ImGui::BeginDisabled(state.Selected < 0 && !additive);
	if (ImGui::Button(state.Playing && !state.Paused ? "Pause" : "Play"))
	{
		const shared_str motion = additive ? state.AdditiveName : motions[state.Selected].m_alias_name;
		CBlend* current = state.Playing && state.Motion == motion ? HudMotionPlayerBlend(HudMotionPlayerArmsModel(item), state.ArmsMotion) : nullptr;
		if (current == nullptr || state.Paused && current->timeCurrent >= current->timeTotal - END_EPS - EPS_L)
		{
			const float speed = current != nullptr ? state.Speed : -1.f;
			HudMotionPlayerStart(hud_item, item, motion, false);
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

	if (!item->m_model_combined)
	{
		RenderHudMotionPlayerAdditive(hud_item, item);
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
