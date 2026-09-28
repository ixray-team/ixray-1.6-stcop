#include "StdAfx.h"
#include "../Actor.h"
#include "ImUtils.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "imgui_internal.h"

constexpr ImVec4 kAnimationListSelected = ImVec4(0.16f, 0.36f, 0.62f, 0.85f);
constexpr ImVec4 kAnimationListHovered = ImVec4(0.12f, 0.26f, 0.45f, 0.70f);
constexpr ImVec4 kAnimationListActive = ImVec4(0.20f, 0.44f, 0.74f, 0.90f);

CBlend* PlayMotionByParts(IKinematicsAnimated* sa, MotionID motion_ID, bool bMixIn, PlayCallback Callback, LPVOID CallbackParam);

static IKinematicsAnimated* ActorKinematics()
{
	if (g_actor == nullptr || g_actor->Visual() == nullptr)
		return nullptr;

	return g_actor->Visual()->dcast_PKinematicsAnimated();
}

static bool IsCompatible(IKinematics* source, IKinematics* target)
{
	if (source->LL_BoneCount() != target->LL_BoneCount())
		return false;

	for (const auto& bone : *source->LL_Bones())
	{
		if (target->LL_BoneID(bone.first) == BI_NONE)
			return false;
	}

	return true;
}

static bool IsOmfPath(const shared_str& source)
{
	u32 length = source.size();

	return length > 4 && !_stricmp(source.c_str() + length - 4, ".omf");
}

static void GetFavoritesPath(string_path& path)
{
	FS.update_path(path, "$app_data_root$", "actor_animations\\favorites.ltx");
}

static bool SelectableWrapped(const char* label, bool selected)
{
	ImVec2 position = ImGui::GetCursorScreenPos();
	float width = ImGui::GetContentRegionAvail().x;
	ImVec2 text_size = ImGui::CalcTextSize(label, nullptr, false, width);
	ImVec2 end = ImVec2(position.x + width, position.y + text_size.y);
	bool hovered = ImGui::IsWindowHovered() && ImGui::IsMouseHoveringRect(position, end);
	bool clicked = hovered && ImGui::IsMouseClicked(ImGuiMouseButton_Left);

	if (selected || hovered)
	{
		ImGui::GetWindowDrawList()->AddRectFilled(position, end, ImGui::GetColorU32(selected ? ImGuiCol_Header : ImGuiCol_HeaderHovered));
	}

	ImGui::PushTextWrapPos(ImGui::GetCursorPos().x + width);
	ImGui::TextUnformatted(label);
	ImGui::PopTextWrapPos();

	return clicked;
}

static void DrawStar(ImVec2 center, float radius, bool filled, bool hovered)
{
	ImDrawList* draw_list = ImGui::GetWindowDrawList();

	center.x = ImFloor(center.x);
	center.y = ImFloor(center.y);

	ImU32 color = 0;

	if (filled)
	{
		color = hovered ? IM_COL32(255, 235, 80, 255) : IM_COL32(255, 215, 0, 255);
	}
	else
	{
		color = hovered ? IM_COL32(255, 235, 80, 255) : ImGui::GetColorU32(ImGuiCol_Text);
	}

	constexpr int point_count = 5;
	constexpr float inner_radius_factor = 0.38196601125f;

	ImVec2 vertices[point_count * 2];
	const float start_angle = -IM_PI * 0.5f;

	for (int index = 0; index < IM_ARRAYSIZE(vertices); ++index)
	{
		float current_radius = (index & 1) ? radius * inner_radius_factor : radius;
		float angle = start_angle + IM_PI * index / point_count;

		vertices[index] =
		{
			center.x + cosf(angle) * current_radius,
			center.y + sinf(angle) * current_radius
		};
	}

	float thickness = radius < 10.0f ? 1.0f : 2.0f;

	if (filled)
	{
		draw_list->AddConvexPolyFilled(vertices, IM_ARRAYSIZE(vertices), color);
	}

	draw_list->AddPolyline(vertices, IM_ARRAYSIZE(vertices), color, ImDrawFlags_Closed, thickness);
}

static float FavoriteButtonWidth()
{
	return ImGui::GetFontSize() * 0.5f + 4.0f;
}

static bool FavoriteStarButton(bool favorite)
{
	float font_size = ImGui::GetFontSize();
	float star_size = font_size * 0.25f;
	float button_width = star_size * 2.0f + 4.0f;
	float button_height = font_size * 0.8f;

	ImGui::SameLine(0.0f, 0.0f);
	ImGui::SetCursorPosX(ImGui::GetWindowContentRegionMax().x - button_width - ImGui::GetStyle().ItemSpacing.x);
	ImGui::SetCursorPosY(ImGui::GetCursorPosY() + (ImGui::GetTextLineHeight() - button_height) * 0.5f);

	ImVec2 cursor_screen_pos = ImGui::GetCursorScreenPos();
	ImVec2 center = ImVec2(cursor_screen_pos.x + star_size + 2.0f, cursor_screen_pos.y + button_height * 0.5f);

	ImGui::InvisibleButton("##actor_anim_favorite", ImVec2(button_width, button_height));

	bool hovered = ImGui::IsItemHovered();
	DrawStar(center, star_size, favorite, hovered);

	if (hovered)
	{
		ImGui::SetTooltip(favorite ? "Remove from favorites" : "Add to favorites");
	}

	return ImGui::IsItemClicked();
}

static const char* MotionName(IKinematicsAnimated* kinematics, MotionID motion)
{
	if (!motion.valid() || motion.slot >= kinematics->LL_MotionsSlotCount())
		return "none";

	shared_motions motions = kinematics->LL_MotionsSlot(motion.slot);

	if (accel_map* map = motions.motion_map())
	{
		for (const auto& [name, index] : *map)
		{
			if (index == motion.idx)
				return name.c_str();
		}
	}

	return "unknown";
}

static CBlend* FindBlend(IKinematicsAnimated* kinematics, MotionID motion)
{
	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		for (u32 i = 0, count = kinematics->LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = kinematics->LL_PartBlend(part, i);

			if (blend && blend->motionID == motion && blend->blend_state() == CBlend::eAccrue)
				return blend;
		}
	}

	return nullptr;
}

template <typename Func>
static void ForEachBlend(IKinematicsAnimated* kinematics, MotionID motion, const Func& func)
{
	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		for (u32 i = 0, count = kinematics->LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = kinematics->LL_PartBlend(part, i);

			if (blend && blend->motionID == motion && blend->blend_state() == CBlend::eAccrue)
				func(*blend);
		}
	}
}

static void SetBlendTime(IKinematicsAnimated* kinematics, MotionID motion, float time)
{
	ForEachBlend(kinematics, motion, [time](CBlend& blend)
	{
		blend.timeCurrent = time;
	});
}

static void SetBlendPlaying(IKinematicsAnimated* kinematics, MotionID motion, bool playing)
{
	ForEachBlend(kinematics, motion, [playing](CBlend& blend)
	{
		blend.playing = playing;
	});
}

static void SetBlendLoop(IKinematicsAnimated* kinematics, MotionID motion, bool loop)
{
	ForEachBlend(kinematics, motion, [loop](CBlend& blend)
	{
		blend.stop_at_end = !loop;
	});
}

static void SetBlendSpeed(IKinematicsAnimated* kinematics, MotionID motion, float speed)
{
	ForEachBlend(kinematics, motion, [speed](CBlend& blend)
	{
		blend.speed = speed;
	});
}

static bool Timeline(const char* id, float& time, float total, const CMotionDef* motion_def)
{
	float width = std::max(ImGui::GetContentRegionAvail().x, 1.0f);
	constexpr float height = 72.0f;
	ImVec2 origin = ImGui::GetCursorScreenPos();
	ImVec2 size = ImVec2(width, height);
	ImVec2 end = ImVec2(origin.x + size.x, origin.y + size.y);

	ImGui::InvisibleButton(id, size);

	bool changed = false;

	if (total > 0.0f && ImGui::IsItemActive())
	{
		float normalized = std::max(0.0f, std::min(1.0f, (ImGui::GetIO().MousePos.x - origin.x) / width));
		time = normalized * total;
		changed = true;
	}

	ImDrawList* draw = ImGui::GetWindowDrawList();
	const ImU32 color_background = IM_COL32(22, 22, 24, 255);
	const ImU32 color_elapsed = IM_COL32(52, 78, 52, 140);
	const ImU32 color_mark = IM_COL32(78, 121, 176, 110);
	const ImU32 color_tick_minor = IM_COL32(58, 58, 62, 255);
	const ImU32 color_tick_major = IM_COL32(105, 105, 112, 255);
	const ImU32 color_text = IM_COL32(180, 180, 186, 255);
	const ImU32 color_playhead = IM_COL32(222, 170, 64, 255);
	const ImU32 color_border = IM_COL32(80, 80, 86, 255);

	draw->AddRectFilled(origin, end, color_background);

	if (total > 0.0f)
	{
		float pixels_per_second = width / total;
		float played_x = origin.x + std::max(0.0f, std::min(total, time)) * pixels_per_second;

		draw->AddRectFilled(ImVec2(origin.x + 1.0f, origin.y + 1.0f), ImVec2(played_x, origin.y + height - 1.0f), color_elapsed);

		if (motion_def)
		{
			for (const motion_marks& mark : motion_def->marks)
			{
				for (const motion_marks::interval& interval : mark.intervals)
				{
					float x0 = origin.x + std::max(0.0f, interval.first) * pixels_per_second;
					float x1 = origin.x + std::max(0.0f, interval.second) * pixels_per_second;

					draw->AddRectFilled(ImVec2(x0, origin.y + 1.0f), ImVec2(std::max(x1, x0 + 1.0f), origin.y + height - 1.0f), color_mark);
				}
			}
		}

		const float steps[] = { 0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1.0f, 2.0f, 5.0f, 10.0f, 30.0f, 60.0f };
		float step = steps[IM_ARRAYSIZE(steps) - 1];

		for (float candidate : steps)
		{
			if (candidate * pixels_per_second >= 64.0f)
			{
				step = candidate;
				break;
			}
		}

		int major_count = std::max(1, iFloor(total / step + 0.5f));
		string32 label = {};

		for (int major = 0; major <= major_count; ++major)
		{
			float major_time = major * step;
			float major_x = origin.x + major_time * pixels_per_second;

			draw->AddLine(ImVec2(major_x, origin.y + 1.0f), ImVec2(major_x, origin.y + height - 1.0f), color_tick_major);

			if (major < major_count)
			{
				xr_sprintf(label, step >= 1.0f ? "%.1f" : "%.2f", major_time);
				draw->AddText(ImVec2(major_x + 3.0f, origin.y + 3.0f), color_text, label);

				for (int minor = 1; minor < 5; ++minor)
				{
					float minor_time = major_time + step * minor / 5.0f;

					if (minor_time > total)
						break;

					float minor_x = origin.x + minor_time * pixels_per_second;
					draw->AddLine(ImVec2(minor_x, origin.y + height - 10.0f), ImVec2(minor_x, origin.y + height - 1.0f), color_tick_minor);
				}
			}
		}

		draw->AddLine(ImVec2(played_x, origin.y), ImVec2(played_x, origin.y + height), color_playhead, 2.0f);
		draw->AddTriangleFilled(ImVec2(played_x - 5.0f, origin.y), ImVec2(played_x + 5.0f, origin.y), ImVec2(played_x, origin.y + 7.0f), color_playhead);

		if (ImGui::IsItemHovered())
		{
			float hover_time = std::max(0.0f, std::min(total, (ImGui::GetIO().MousePos.x - origin.x) / width * total));
			ImGui::SetTooltip("Time: %.3f s\nFrame: %d", hover_time, iFloor(hover_time / SAMPLE_SPF + 0.5f));
		}
	}
	else
	{
		const char* message = "no animation";
		ImVec2 message_size = ImGui::CalcTextSize(message);
		draw->AddText(ImVec2(origin.x + (width - message_size.x) * 0.5f, origin.y + (height - message_size.y) * 0.5f), color_text, message);
	}

	draw->AddRect(origin, end, color_border);

	return changed;
}

void CActorAnimationManager::reload()
{
	sources.clear();
	selected_source = -1;
	played_motion.invalidate();
	played_name = "";
	played_fx = false;

	if (g_actor == nullptr)
		return;

	CActor* actor = Actor();
	IKinematicsAnimated* actor_animations = actor->Visual() ? actor->Visual()->dcast_PKinematicsAnimated() : nullptr;
	IKinematics* actor_skeleton = actor_animations ? actor_animations->dcast_PKinematics() : nullptr;

	if (!actor_animations || !actor_skeleton)
		return;

	xr_set<shared_str> processed;

	auto collect = [&](IKinematicsAnimated* kinematics)
	{
		IKinematics* skeleton = kinematics ? kinematics->dcast_PKinematics() : nullptr;

		if (!skeleton || !IsCompatible(skeleton, actor_skeleton))
			return;

		for (u16 slot = 0, count = kinematics->LL_MotionsSlotCount(); slot < count; ++slot)
		{
			shared_motions motions = kinematics->LL_MotionsSlot(slot);

			if (motions.motion_map() == nullptr || processed.contains(motions.id()))
				continue;

			if (!IsOmfPath(motions.id()) && kinematics != actor_animations)
				continue;

			processed.insert(motions.id());
			SSource& source = sources.emplace_back();
			source.id = motions.id();

			if (accel_map* cycles = motions.cycle())
			{
				for (const auto& key : *cycles | std::views::keys)
				{
					source.animations.push_back({.name = key, .fx = false });
				}
			}

			if (accel_map* effects = motions.fx())
			{
				for (const auto& key : *effects | std::views::keys)
				{
					source.animations.push_back({.name = key, .fx = true });
				}
			}
		}
	};

	collect(actor_animations);

	if (g_pGameLevel)
	{
		for (u32 i = 0; i < g_pGameLevel->Objects.o_count(); ++i)
		{
			CObject* object = g_pGameLevel->Objects.o_get_by_iterator(i);

			if (object && object->Visual())
			{
				collect(object->Visual()->dcast_PKinematicsAnimated());
			}
		}
	}

	if (!sources.empty())
	{
		selected_source = 0;
	}

	load_favorites();
}

bool CActorAnimationManager::ensure_source(IKinematicsAnimated* kinematics, const shared_str& source)
{
	for (u16 slot = 0, count = kinematics->LL_MotionsSlotCount(); slot < count; ++slot)
	{
		if (kinematics->LL_MotionsSlot(slot).id() == source)
		{
			return true;
		}
	}

	if (!IsOmfPath(source))
	{
		return false;
	}

	string_path path = {};
	xr_strcpy(path, source.c_str());
	path[source.size() - 4] = '\0';

	kinematics->append_motion_from_path(Actor()->cNameSect().c_str(), path);

	return true;
}

const CActorAnimationManager::SSource* CActorAnimationManager::find_source(const shared_str& source) const
{
	for (const SSource& entry : sources)
	{
		if (entry.id == source)
		{
			return &entry;
		}
	}

	return nullptr;
}

const CActorAnimationManager::SAnimation* CActorAnimationManager::find_animation(const SSource& source, const shared_str& name, bool fx) const
{
	for (const SAnimation& animation : source.animations)
	{
		if (animation.fx == fx && animation.name == name)
		{
			return &animation;
		}
	}

	return nullptr;
}

bool CActorAnimationManager::is_favorite(const shared_str& source, const shared_str& name) const
{
	for (const SFavorite& favorite : favorites)
	{
		if (favorite.source == source && favorite.name == name)
		{
			return true;
		}
	}

	return false;
}

void CActorAnimationManager::add_favorite(const shared_str& source, const SAnimation& animation)
{
	if (is_favorite(source, animation.name))
	{
		return;
	}

	favorites.push_back({ .source = source, .name = animation.name, .fx = animation.fx });
	save_favorites();
}

void CActorAnimationManager::remove_favorite(const shared_str& source, const shared_str& name)
{
	auto end = std::remove_if(favorites.begin(), favorites.end(), [&](const SFavorite& favorite)
	{
		return favorite.source == source && favorite.name == name;
	});

	if (end == favorites.end())
	{
		return;
	}

	favorites.erase(end, favorites.end());
	save_favorites();
}

void CActorAnimationManager::load_favorites()
{
	favorites.clear();

	string_path path = {};
	GetFavoritesPath(path);

	if (!FS.exist(path))
	{
		return;
	}

	CInifile ini(path, true, true, false);

	for (const CInifile::Sect& section : ini.sections())
	{
		const SSource* source = find_source(section.Name);

		if (source == nullptr)
		{
			continue;
		}

		for (const auto& line : section.Data)
		{
			bool fx = !xr_strcmp(line.second, "fx");

			if (find_animation(*source, line.first, fx))
			{
				favorites.push_back({ .source = section.Name, .name = line.first, .fx = fx });
			}
		}
	}
}

void CActorAnimationManager::save_favorites()
{
	string_path path = {};
	GetFavoritesPath(path);

	CInifile ini(path, false, true, true);
	ini.set_override_names(true);

	for (const SFavorite& favorite : favorites)
	{
		ini.w_string(favorite.source.c_str(), favorite.name.c_str(), favorite.fx ? "fx" : "cycle");
	}
}

void CActorAnimationManager::play(const SSource& source, const SAnimation& animation)
{
	if (g_actor == nullptr)
	{
		return;
	}

	IKinematicsAnimated* kinematics = ActorKinematics();

	if (!kinematics)
	{
		return;
	}

	set_override(true);

	if (!ensure_source(kinematics, source.id))
	{
		return;
	}

	MotionID motion = animation.fx ? kinematics->ID_FX_Safe(animation.name.c_str()) : kinematics->ID_Cycle_Safe(animation.name.c_str());

	if (!motion.valid())
	{
		return;
	}

	if (animation.fx)
	{
		kinematics->PlayFX(motion, 1.0f);
	}
	else
	{
		PlayMotionByParts(kinematics, motion, mix, nullptr, nullptr);
		SetBlendLoop(kinematics, motion, loop);
	}

	SetBlendSpeed(kinematics, motion, speed);

	played_motion = motion;
	played_name = animation.name;
	played_fx = animation.fx;
}

void CActorAnimationManager::play_pause()
{
	IKinematicsAnimated* kinematics = ActorKinematics();

	if (!kinematics || !played_motion.valid())
	{
		return;
	}

	CBlend* blend = FindBlend(kinematics, played_motion);

	if (!blend)
	{
		return;
	}

	bool at_end = blend->timeTotal > 0.f && blend->timeCurrent >= blend->timeTotal - END_EPS;

	if (blend->playing && !at_end)
	{
		SetBlendPlaying(kinematics, played_motion, false);
		return;
	}

	if (at_end)
	{
		SetBlendTime(kinematics, played_motion, 0.f);
	}

	SetBlendPlaying(kinematics, played_motion, true);
}

void CActorAnimationManager::frame_step(s32 direction)
{
	IKinematicsAnimated* kinematics = ActorKinematics();

	if (!kinematics || !played_motion.valid())
	{
		return;
	}

	CBlend* blend = FindBlend(kinematics, played_motion);

	if (!blend)
	{
		return;
	}

	SetBlendPlaying(kinematics, played_motion, false);
	SetBlendTime(kinematics, played_motion, std::max(0.0f, std::min(blend->timeTotal, blend->timeCurrent + direction * SAMPLE_SPF)));
}

void CActorAnimationManager::stop()
{
	IKinematicsAnimated* kinematics = ActorKinematics();

	if (!kinematics || !played_motion.valid())
	{
		return;
	}

	SetBlendPlaying(kinematics, played_motion, false);
	SetBlendTime(kinematics, played_motion, 0.f);
}

void CActorAnimationManager::set_override(bool value)
{
	override_mode = value;

	if (g_actor == nullptr)
	{
		return;
	}

	CActor* actor = Actor();
	actor->m_bAnimOverride = value;
	actor->m_current_legs.invalidate();
	actor->m_current_torso.invalidate();
	actor->m_current_head.invalidate();
}

void CActorAnimationManager::draw()
{
	if (g_actor == nullptr)
	{
		return;
	}

	if (level != g_pGameLevel)
	{
		level = g_pGameLevel;
		reload();
	}

	CActor* actor = Actor();
	IKinematicsAnimated* kinematics = actor->Visual() ? actor->Visual()->dcast_PKinematicsAnimated() : nullptr;

	if (!kinematics)
	{
		return;
	}

	if (actor->m_bAnimOverride != override_mode)
	{
		set_override(override_mode);
	}

	if (ImGui::Checkbox("Override", &override_mode))
	{
		set_override(override_mode);
	}

	ImGui::SameLine();

	if (ImGui::Button("Reload"))
	{
		reload();
	}

	ImGui::SeparatorText("Current");

	auto current_row = [&](const char* label, MotionID motion)
	{
		if (!motion.valid())
		{
			ImGui::Text("%s: none", label);
			return;
		}

		string256 text = {};
		xr_sprintf(text, "%s: %s", label, MotionName(kinematics, motion));

		if (ImGui::Selectable(text, played_motion == motion))
		{
			played_motion = motion;
			played_name = MotionName(kinematics, motion);
			played_fx = false;
		}
	};

	current_row("Legs", actor->m_current_legs);
	current_row("Torso", actor->m_current_torso);
	current_row("Head", actor->m_current_head);

	ImGui::SeparatorText("Player");

	CBlend* blend = played_motion.valid() ? FindBlend(kinematics, played_motion) : nullptr;
	float total = blend ? blend->timeTotal : 0.0f;
	bool at_end = blend && blend->timeTotal > 0.0f && blend->timeCurrent >= blend->timeTotal - END_EPS;

	if (played_motion.valid())
	{
		ImGui::Text("Animation: %s%s", MotionName(kinematics, played_motion), played_fx ? " [fx]" : "");
	}
	else
	{
		ImGui::Text("Animation: none");
	}

	ImGui::BeginDisabled(blend == nullptr);

	if (ImGui::Button(blend && blend->playing && !at_end ? "Pause" : "Play"))
	{
		play_pause();
	}

	ImGui::SameLine();

	if (ImGui::Button("Stop"))
	{
		stop();
	}

	ImGui::SameLine();

	if (ImGui::Button("|<"))
	{
		SetBlendPlaying(kinematics, played_motion, false);
		SetBlendTime(kinematics, played_motion, 0.0f);
	}

	ImGui::SameLine();

	if (ImGui::Button("<|"))
	{
		frame_step(-1);
	}

	ImGui::SameLine();

	if (ImGui::Button("|>"))
	{
		frame_step(1);
	}

	ImGui::SameLine();

	if (ImGui::Button(">|"))
	{
		SetBlendPlaying(kinematics, played_motion, false);
		SetBlendTime(kinematics, played_motion, total);
	}

	ImGui::SameLine();

	if (ImGui::Checkbox("Loop", &loop))
	{
		SetBlendLoop(kinematics, played_motion, loop);
	}

	ImGui::SameLine();
	ImGui::SetNextItemWidth(140.0f);

	if (ImGui::DragFloat("Speed", &speed, .01f, -4.f, 4.f, "%.2f"))
	{
		SetBlendSpeed(kinematics, played_motion, speed);
	}

	float time = blend ? blend->timeCurrent : 0.f;

	if (Timeline("##actor_anim_timeline", time, total, played_motion.valid() ? kinematics->LL_GetMotionDef(played_motion) : nullptr))
	{
		SetBlendPlaying(kinematics, played_motion, false);
		SetBlendTime(kinematics, played_motion, time);
	}

	if (blend)
	{
		u32 frame = (u32)iFloor(time / SAMPLE_SPF + .5f);
		u32 frames = (u32)iFloor(total / SAMPLE_SPF + .5f);
		ImGui::Text("Frame: %u / %u    Time: %.3f / %.3f s", frame, frames, time, total);
	}

	ImGui::EndDisabled();

	ImGui::PushStyleColor(ImGuiCol_Header, kAnimationListSelected);
	ImGui::PushStyleColor(ImGuiCol_HeaderHovered, kAnimationListHovered);
	ImGui::PushStyleColor(ImGuiCol_HeaderActive, kAnimationListActive);

	ImGui::SeparatorText("Animations");
	ImGui::InputTextWithHint("##actor_anim_filter", "filter", filter, IM_ARRAYSIZE(filter));

	constexpr float list_height = 320.f;

	ImGui::BeginChild("##actor_anim_sources", ImVec2(280.0f, list_height), true);

	string_path label = {};
	xr_sprintf(label, "Favorites (%u)", (u32)favorites.size());

	if (SelectableWrapped(label, selected_source < 0))
	{
		selected_source = -1;
	}

	for (s32 i = 0; i < (s32)sources.size(); ++i)
	{
		xr_sprintf(label, "%s (%u)", sources[i].id.c_str(), (u32)sources[i].animations.size());

		if (SelectableWrapped(label, selected_source == i))
		{
			selected_source = i;
		}
	}

	ImGui::EndChild();
	ImGui::SameLine();
	ImGui::BeginChild("##actor_anim_motions", ImVec2(0.0f, list_height), true);

	if (selected_source < 0)
	{
		xr_vector<shared_str> groups;

		for (const SFavorite& favorite : favorites)
		{
			bool found = false;

			for (const shared_str& group : groups)
			{
				if (group == favorite.source)
				{
					found = true;
					break;
				}
			}

			if (!found)
			{
				groups.push_back(favorite.source);
			}
		}

		shared_str pending_source;
		shared_str pending_name;
		bool pending_remove = false;

		for (const shared_str& group : groups)
		{
			u32 count = 0;
			bool visible = false;

			for (const SFavorite& favorite : favorites)
			{
				if (favorite.source != group)
				{
					continue;
				}

				++count;

				if (!filter[0] || strstr(favorite.name.c_str(), filter) || strstr(favorite.source.c_str(), filter))
				{
					visible = true;
				}
			}

			if (!visible)
			{
				continue;
			}

			ImGui::PushID(group.c_str());

			xr_sprintf(label, "%s (%u)", group.c_str(), count);

			if (ImGui::TreeNodeEx(label, ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth))
			{
				for (s32 i = 0; i < (s32)favorites.size(); ++i)
				{
					const SFavorite& favorite = favorites[i];

					if (favorite.source != group)
					{
						continue;
					}

					if (filter[0] && !strstr(favorite.name.c_str(), filter) && !strstr(favorite.source.c_str(), filter))
					{
						continue;
					}

					ImGui::PushID(i);

					xr_sprintf(label, "%s%s", favorite.name.c_str(), favorite.fx ? " [fx]" : "");

					ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;

					if (played_name == favorite.name)
					{
						flags |= ImGuiTreeNodeFlags_Selected;
					}

					ImGui::TreeNodeEx(label, flags);

					if (ImGui::IsItemClicked())
					{
						if (const SSource* source = find_source(favorite.source))
						{
							if (const SAnimation* animation = find_animation(*source, favorite.name, favorite.fx))
							{
								play(*source, *animation);
							}
						}
					}

					if (ImGui::BeginPopupContextItem("##actor_anim_favorite_context"))
					{
						if (ImGui::MenuItem("Remove from favorites"))
						{
							pending_source = favorite.source;
							pending_name = favorite.name;
							pending_remove = true;
						}

						ImGui::EndPopup();
					}

					if (FavoriteStarButton(true))
					{
						pending_source = favorite.source;
						pending_name = favorite.name;
						pending_remove = true;
					}

					ImGui::PopID();
				}

				ImGui::TreePop();
			}

			ImGui::PopID();
		}

		if (pending_remove)
		{
			remove_favorite(pending_source, pending_name);
		}
	}
	else if (selected_source < (s32)sources.size())
	{
		const SSource& source = sources[selected_source];

		for (s32 i = 0; i < (s32)source.animations.size(); ++i)
		{
			const SAnimation& animation = source.animations[i];

			if (filter[0] && !strstr(animation.name.c_str(), filter))
			{
				continue;
			}

			ImGui::PushID(i);

			bool favorite = is_favorite(source.id, animation.name);
			float selectable_width = std::max(1.0f, ImGui::GetContentRegionAvail().x - FavoriteButtonWidth() - ImGui::GetStyle().ItemSpacing.x * 1.5f);

			xr_sprintf(label, "%s%s", animation.name.c_str(), animation.fx ? " [fx]" : "");

			if (ImGui::Selectable(label, played_name == animation.name, 0, ImVec2(selectable_width, 0.0f)))
			{
				play(source, animation);
			}

			if (ImGui::BeginPopupContextItem("##actor_anim_context"))
			{
				if (favorite)
				{
					if (ImGui::MenuItem("Remove from favorites"))
					{
						remove_favorite(source.id, animation.name);
					}
				}
				else if (ImGui::MenuItem("Add to favorites"))
				{
					add_favorite(source.id, animation);
				}

				ImGui::EndPopup();
			}

			if (FavoriteStarButton(favorite))
			{
				if (favorite)
				{
					remove_favorite(source.id, animation.name);
				}
				else
				{
					add_favorite(source.id, animation);
				}
			}

			ImGui::PopID();
		}
	}

	ImGui::EndChild();

	ImGui::PopStyleColor(3);
}

void RenderActorAnimationManager()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_ActorAnimations)])
	{
		if (imgui_actor_animation_manager.override_mode)
		{
			imgui_actor_animation_manager.set_override(false);
		}

		return;
	}

	if (!g_pGameLevel)
	{
		imgui_actor_animation_manager.override_mode = false;
		return;
	}

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.f, 0.f, 0.f, kGeneralAlphaLevelForImGuiWindows));

	if (!ImGui::Begin("Actor Animations", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_ActorAnimations)]))
	{
		ImGui::End();
		ImGui::PopStyleColor(1);
		return;
	}

	imgui_actor_animation_manager.draw();

	ImGui::End();
	ImGui::PopStyleColor(1);
}
