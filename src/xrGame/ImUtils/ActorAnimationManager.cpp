#include "StdAfx.h"
#include "../Actor.h"
#include "ImUtils.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "imgui_internal.h"

constexpr ImVec4 kAnimationListSelected = ImVec4(0.16f, 0.36f, 0.62f, 0.85f);
constexpr ImVec4 kAnimationListHovered = ImVec4(0.12f, 0.26f, 0.45f, 0.70f);
constexpr ImVec4 kAnimationListActive = ImVec4(0.20f, 0.44f, 0.74f, 0.90f);

struct SAnimationDragPayload
{
	string_path source;
	string_path name;
	bool fx = false;
};

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

static bool ContainsFilter(const char* text, const char* filter)
{
	if (filter[0] == '\0')
	{
		return true;
	}

	size_t length = xr_strlen(filter);

	for (const char* position = text; *position; ++position)
	{
		if (_strnicmp(position, filter, length) == 0)
		{
			return true;
		}
	}

	return false;
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
	played_source = "";
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

void CActorAnimationManager::PlaylistCallback(CBlend* blend)
{
	if (blend == nullptr || blend->CallbackParam == nullptr)
	{
		return;
	}

	CActorAnimationManager* manager = static_cast<CActorAnimationManager*>(blend->CallbackParam);
	manager->playlist_advance = true;
}

void CActorAnimationManager::play_playlist(s32 index)
{
	if (index < 0 || index >= (s32)playlist.size())
	{
		stop_playlist();
		return;
	}

	const SPlaylistEntry& entry = playlist[index];
	playlist_index = index;

	const SSource* source = find_source(entry.source);

	if (source != nullptr)
	{
		if (const SAnimation* animation = find_animation(*source, entry.name, entry.fx))
		{
			play(*source, *animation, true);
		}
	}

	playlist_advance = false;

	if (played_name != entry.name)
	{
		playlist_advance = true;
	}

	if (entry.fx)
	{
		IKinematicsAnimated* kinematics = ActorKinematics();

		if (kinematics && played_motion.valid())
		{
			playlist_fx_deadline = Device.fTimeGlobal + kinematics->get_animation_length(played_motion);
		}
	}
}

void CActorAnimationManager::stop_playlist()
{
	playlist_index = -1;
	playlist_advance = false;
}

void CActorAnimationManager::move_playlist(s32 from, s32 to)
{
	if (from == to || from < 0 || from >= (s32)playlist.size() || to < 0 || to > (s32)playlist.size())
	{
		return;
	}

	s32 insert_at = to;

	if (insert_at > from)
	{
		--insert_at;
	}

	SPlaylistEntry entry = playlist[from];
	playlist.erase(playlist.begin() + from);
	playlist.insert(playlist.begin() + insert_at, entry);

	if (playlist_index == from)
	{
		playlist_index = insert_at;
	}
	else
	{
		if (from < playlist_index)
		{
			--playlist_index;
		}

		if (insert_at <= playlist_index)
		{
			++playlist_index;
		}
	}
}

void CActorAnimationManager::remove_playlist(s32 index)
{
	if (index < 0 || index >= (s32)playlist.size())
	{
		return;
	}

	playlist.erase(playlist.begin() + index);

	if (playlist_index == index)
	{
		playlist_index = -1;
	}
	else if (playlist_index > index)
	{
		--playlist_index;
	}

	if (playlist_selected >= (s32)playlist.size())
	{
		playlist_selected = (s32)playlist.size() - 1;
	}
}

void CActorAnimationManager::play(const SSource& source, const SAnimation& animation, bool playlist)
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

	if (!playlist)
	{
		stop_playlist();
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
		PlayMotionByParts(kinematics, motion, mix, playlist ? PlaylistCallback : nullptr, playlist ? this : nullptr);
		SetBlendLoop(kinematics, motion, playlist ? false : loop);
	}

	SetBlendSpeed(kinematics, motion, speed);

	played_motion = motion;
	played_name = animation.name;
	played_source = source.id;
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
		ImGui::TextDisabled("No actor in the level");
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
		ImGui::TextDisabled("Actor has no animated visual");
		return;
	}

	if (actor->m_bAnimOverride != override_mode)
	{
		set_override(override_mode);
	}

	if (ImGui::Checkbox("Override actor animation", &override_mode))
	{
		set_override(override_mode);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Block the game's animation state machine while this panel controls the actor");
	}

	ImGui::SameLine();

	if (ImGui::Button("Reload library"))
	{
		reload();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Rebuild the animation library from the level objects");
	}

	if (ImGui::CollapsingHeader("Current state"))
	{
		auto current_row = [&](const char* label, MotionID motion)
		{
			if (!motion.valid())
			{
				ImGui::TextDisabled("%s: none", label);
				return;
			}

			string256 text = {};
			xr_sprintf(text, "%s: %s", label, MotionName(kinematics, motion));

			if (ImGui::Selectable(text, played_motion == motion))
			{
				played_motion = motion;
				played_name = MotionName(kinematics, motion);
				played_source = "";
				played_fx = false;
			}

			if (ImGui::IsItemHovered())
			{
				ImGui::SetTooltip("Inspect this animation in the player");
			}
		};

		current_row("Legs", actor->m_current_legs);
		current_row("Torso", actor->m_current_torso);
		current_row("Head", actor->m_current_head);
	}

	ImGui::SeparatorText("Player");

	CBlend* blend = played_motion.valid() ? FindBlend(kinematics, played_motion) : nullptr;
	float total = blend ? blend->timeTotal : 0.0f;
	bool at_end = blend && blend->timeTotal > 0.0f && blend->timeCurrent >= blend->timeTotal - END_EPS;

	if (played_motion.valid())
	{
		ImGui::Text("Animation: %s%s", MotionName(kinematics, played_motion), played_fx ? " [fx]" : "");

		if (played_source.size() > 0)
		{
			ImGui::SameLine();
			ImGui::TextDisabled("(%s)", played_source.c_str());
		}
	}
	else
	{
		ImGui::TextDisabled("Animation: none — pick one from the library below");
	}

	if (!ImGui::GetIO().WantTextInput && ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && blend != nullptr)
	{
		if (ImGui::IsKeyPressed(ImGuiKey_Space, false))
		{
			play_pause();
		}

		if (ImGui::IsKeyPressed(ImGuiKey_LeftArrow))
		{
			frame_step(-1);
		}

		if (ImGui::IsKeyPressed(ImGuiKey_RightArrow))
		{
			frame_step(1);
		}

		if (ImGui::IsKeyPressed(ImGuiKey_Home))
		{
			SetBlendPlaying(kinematics, played_motion, false);
			SetBlendTime(kinematics, played_motion, 0.0f);
		}

		if (ImGui::IsKeyPressed(ImGuiKey_End))
		{
			SetBlendPlaying(kinematics, played_motion, false);
			SetBlendTime(kinematics, played_motion, total);
		}
	}

	ImGui::BeginDisabled(blend == nullptr);

	if (ImGui::Button(blend && blend->playing && !at_end ? "Pause" : "Play"))
	{
		play_pause();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Play / pause (Space)");
	}

	ImGui::SameLine();

	if (ImGui::Button("Stop"))
	{
		stop_playlist();
		stop();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Stop and rewind (also stops the playlist)");
	}

	ImGui::SameLine();

	if (ImGui::Button("|<"))
	{
		SetBlendPlaying(kinematics, played_motion, false);
		SetBlendTime(kinematics, played_motion, 0.0f);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("First frame (Home)");
	}

	ImGui::SameLine();

	if (ImGui::Button("<|"))
	{
		frame_step(-1);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Previous frame (Left arrow)");
	}

	ImGui::SameLine();

	if (ImGui::Button("|>"))
	{
		frame_step(1);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Next frame (Right arrow)");
	}

	ImGui::SameLine();

	if (ImGui::Button(">|"))
	{
		SetBlendPlaying(kinematics, played_motion, false);
		SetBlendTime(kinematics, played_motion, total);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Last frame (End)");
	}

	ImGui::SameLine();

	if (ImGui::Checkbox("Loop", &loop))
	{
		SetBlendLoop(kinematics, played_motion, loop);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Loop the selected animation");
	}

	ImGui::SameLine();
	ImGui::SetNextItemWidth(140.0f);

	if (ImGui::DragFloat("Speed", &speed, .01f, -4.f, 4.f, "%.2f"))
	{
		SetBlendSpeed(kinematics, played_motion, speed);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Playback speed multiplier");
	}

	float time = blend ? blend->timeCurrent : 0.f;

	if (Timeline("##actor_anim_timeline", time, total, played_motion.valid() ? kinematics->LL_GetMotionDef(played_motion) : nullptr))
	{
		SetBlendPlaying(kinematics, played_motion, false);
		SetBlendTime(kinematics, played_motion, time);
	}

	if (ImGui::IsItemHovered() || ImGui::IsItemActive())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
	}

	if (blend)
	{
		u32 frame = (u32)iFloor(time / SAMPLE_SPF + .5f);
		u32 frames = (u32)iFloor(total / SAMPLE_SPF + .5f);
		ImGui::Text("Frame: %u / %u    Time: %.3f / %.3f s", frame, frames, time, total);
	}
	else
	{
		ImGui::TextDisabled("No animation loaded");
	}

	ImGui::EndDisabled();

	ImGui::PushStyleColor(ImGuiCol_Header, kAnimationListSelected);
	ImGui::PushStyleColor(ImGuiCol_HeaderHovered, kAnimationListHovered);
	ImGui::PushStyleColor(ImGuiCol_HeaderActive, kAnimationListActive);

	ImGui::SeparatorText("Library");

	float filter_button_width = ImGui::GetFrameHeight() + ImGui::GetStyle().ItemSpacing.x;
	float filter_width = std::max(120.0f, ImGui::GetContentRegionAvail().x - filter_button_width);
	ImGui::SetNextItemWidth(filter_width);
	ImGui::InputTextWithHint("##actor_anim_filter", "Search animations and sources", filter, IM_ARRAYSIZE(filter));

	if (filter[0] != '\0')
	{
		ImGui::SameLine();

		if (ImGui::Button("X##actor_anim_filter_clear"))
		{
			filter[0] = '\0';
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Clear search");
		}
	}

	string_path label = {};

	auto animation_row = [&](const SSource& source, const SAnimation& animation, bool show_source)
	{
		ImGui::PushID(&animation);

		bool favorite = is_favorite(source.id, animation.name);
		float selectable_width = std::max(1.0f, ImGui::GetContentRegionAvail().x - FavoriteButtonWidth() - ImGui::GetStyle().ItemSpacing.x * 1.5f);

		if (show_source)
		{
			xr_sprintf(label, "%s  (%s)%s", animation.name.c_str(), source.id.c_str(), animation.fx ? " [fx]" : "");
		}
		else
		{
			xr_sprintf(label, "%s%s", animation.name.c_str(), animation.fx ? " [fx]" : "");
		}

		if (ImGui::Selectable(label, played_name == animation.name && played_source == source.id, 0, ImVec2(selectable_width, 0.0f)))
		{
			play(source, animation);
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s\n%s", animation.name.c_str(), source.id.c_str());
		}

		if (ImGui::BeginPopupContextItem("##actor_anim_context"))
		{
			if (ImGui::MenuItem("Add to playlist"))
			{
				playlist.push_back({ .source = source.id, .name = animation.name, .fx = animation.fx });
				playlist_selected = (s32)playlist.size() - 1;
			}

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

		if (ImGui::BeginDragDropSource())
		{
			SAnimationDragPayload payload = {};
			xr_strcpy(payload.source, source.id.c_str());
			xr_strcpy(payload.name, animation.name.c_str());
			payload.fx = animation.fx;

			ImGui::SetDragDropPayload("ACTOR_ANIMATION", &payload, sizeof(payload));
			ImGui::TextUnformatted(label);
			ImGui::EndDragDropSource();
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
	};

	float browser_height = std::max(150.0f, ImGui::GetContentRegionAvail().y * 0.5f);
	float browser_width = ImGui::GetContentRegionAvail().x;
	constexpr float splitter_width = 6.0f;
	float max_left_width = std::max(80.0f, browser_width - 160.0f - splitter_width);
	float left_width = std::clamp(browser_width * browser_split, 80.0f, max_left_width);

	ImGui::BeginChild("##actor_anim_browser_sources", ImVec2(left_width, browser_height), true);

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

	if (sources.empty() && favorites.empty())
	{
		ImGui::TextDisabled("No animation sets on the level");
	}

	ImGui::EndChild();

	ImGui::SameLine(0.0f, 0.0f);

	ImGui::InvisibleButton("##actor_anim_browser_splitter", ImVec2(splitter_width, browser_height));

	if (ImGui::IsItemActive())
	{
		browser_split += ImGui::GetIO().MouseDelta.x / std::max(1.0f, browser_width);
		browser_split = std::clamp(browser_split, 0.1f, 0.9f);
	}

	if (ImGui::IsItemHovered() || ImGui::IsItemActive())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
	}

	ImVec2 splitter_min = ImGui::GetItemRectMin();
	ImVec2 splitter_max = ImGui::GetItemRectMax();
	ImGui::GetWindowDrawList()->AddLine(ImVec2(splitter_min.x + splitter_width * 0.5f, splitter_min.y), ImVec2(splitter_min.x + splitter_width * 0.5f, splitter_max.y), ImGui::GetColorU32(ImGui::IsItemHovered() || ImGui::IsItemActive() ? ImGuiCol_SeparatorHovered : ImGuiCol_Separator), 1.0f);

	ImGui::SameLine(0.0f, 0.0f);

	ImGui::BeginChild("##actor_anim_browser_motions", ImVec2(0.0f, browser_height), true);

	if (filter[0] != '\0')
	{
		bool any = false;

		for (const SSource& source : sources)
		{
			for (const SAnimation& animation : source.animations)
			{
				if (!ContainsFilter(animation.name.c_str(), filter) && !ContainsFilter(source.id.c_str(), filter))
				{
					continue;
				}

				any = true;
				animation_row(source, animation, true);
			}
		}

		if (!any)
		{
			ImGui::TextDisabled("Nothing found");
		}
	}
	else if (selected_source < 0)
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

				if (ContainsFilter(favorite.name.c_str(), filter) || ContainsFilter(favorite.source.c_str(), filter))
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

					if (!ContainsFilter(favorite.name.c_str(), filter) && !ContainsFilter(favorite.source.c_str(), filter))
					{
						continue;
					}

					ImGui::PushID(i);

					xr_sprintf(label, "%s%s", favorite.name.c_str(), favorite.fx ? " [fx]" : "");

					ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;

					if (played_name == favorite.name && played_source == favorite.source)
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
						if (ImGui::MenuItem("Add to playlist"))
						{
							playlist.push_back({ .source = favorite.source, .name = favorite.name, .fx = favorite.fx });
							playlist_selected = (s32)playlist.size() - 1;
						}

						if (ImGui::MenuItem("Remove from favorites"))
						{
							pending_source = favorite.source;
							pending_name = favorite.name;
							pending_remove = true;
						}

						ImGui::EndPopup();
					}

					if (ImGui::BeginDragDropSource())
					{
						SAnimationDragPayload payload = {};
						xr_strcpy(payload.source, favorite.source.c_str());
						xr_strcpy(payload.name, favorite.name.c_str());
						payload.fx = favorite.fx;

						ImGui::SetDragDropPayload("ACTOR_ANIMATION", &payload, sizeof(payload));
						ImGui::TextUnformatted(label);
						ImGui::EndDragDropSource();
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

		for (const SAnimation& animation : source.animations)
		{
			animation_row(source, animation, false);
		}
	}

	ImGui::EndChild();

	ImGui::SeparatorText("Playlist");

	if (playlist_index >= 0)
	{
		bool finished = playlist_advance;
		playlist_advance = false;

		if (!finished && playlist[playlist_index].fx && Device.fTimeGlobal >= playlist_fx_deadline)
		{
			finished = true;
		}

		if (!finished && !playlist[playlist_index].fx && FindBlend(kinematics, played_motion) == nullptr)
		{
			finished = true;
		}

		if (finished)
		{
			if (playlist_index + 1 < (s32)playlist.size())
			{
				play_playlist(playlist_index + 1);
			}
			else if (playlist_loop)
			{
				play_playlist(0);
			}
			else
			{
				stop_playlist();
			}
		}
	}

	ImGui::BeginDisabled(playlist.empty());

	if (ImGui::Button("Play##playlist"))
	{
		play_playlist(playlist_selected >= 0 ? playlist_selected : 0);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Play the playlist from the selected entry");
	}

	ImGui::EndDisabled();

	ImGui::SameLine();

	ImGui::BeginDisabled(playlist_index < 0);

	if (ImGui::Button("Stop##playlist"))
	{
		stop_playlist();
		stop();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Stop the playlist");
	}

	ImGui::EndDisabled();

	ImGui::SameLine();

	ImGui::BeginDisabled(playlist.empty());

	if (ImGui::Button("Clear"))
	{
		stop_playlist();
		playlist.clear();
		playlist_selected = -1;
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Remove all playlist entries");
	}

	ImGui::EndDisabled();

	ImGui::SameLine();
	ImGui::Text("Entries: %u", (u32)playlist.size());

	ImGui::SameLine();
	ImGui::Checkbox("Loop##playlist", &playlist_loop);

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Restart the playlist after the last entry");
	}

	float playlist_height = std::max(90.0f, ImGui::GetContentRegionAvail().y - ImGui::GetStyle().ItemSpacing.y);

	ImGui::BeginChild("##actor_anim_playlist", ImVec2(0.0f, playlist_height), true);

	if (ImGui::BeginDragDropTarget())
	{
		if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload("ACTOR_ANIMATION"))
		{
			const SAnimationDragPayload* animation = (const SAnimationDragPayload*)payload->Data;
			playlist.push_back({ .source = animation->source, .name = animation->name, .fx = animation->fx });
			playlist_selected = (s32)playlist.size() - 1;
		}
		else if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload("ACTOR_ANIMATION_INDEX"))
		{
			s32 from = *(const s32*)payload->Data;
			move_playlist(from, (s32)playlist.size());
		}

		ImGui::EndDragDropTarget();
	}

	if (playlist.empty())
	{
		ImGui::TextDisabled("Drag animations here from the library");
	}

	for (s32 i = 0; i < (s32)playlist.size(); ++i)
	{
		const SPlaylistEntry& entry = playlist[i];

		ImGui::PushID(i);

		xr_sprintf(label, "%d. %s%s", i + 1, entry.name.c_str(), entry.fx ? " [fx]" : "");

		if (ImGui::Selectable(label, playlist_selected == i))
		{
			playlist_selected = i;
		}

		if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
		{
			play_playlist(i);
		}

		if (playlist_index == i)
		{
			ImVec2 rect_min = ImGui::GetItemRectMin();
			ImVec2 rect_max = ImGui::GetItemRectMax();
			ImGui::GetWindowDrawList()->AddText(ImVec2(rect_max.x + 8.0f, rect_min.y), IM_COL32(120, 255, 120, 255), "<< playing");
		}

		if (ImGui::BeginDragDropSource())
		{
			ImGui::SetDragDropPayload("ACTOR_ANIMATION_INDEX", &i, sizeof(i));
			ImGui::TextUnformatted(label);
			ImGui::EndDragDropSource();
		}

		if (ImGui::BeginDragDropTarget())
		{
			if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload("ACTOR_ANIMATION_INDEX"))
			{
				s32 from = *(const s32*)payload->Data;
				move_playlist(from, i);
			}
			else if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload("ACTOR_ANIMATION"))
			{
				const SAnimationDragPayload* animation = (const SAnimationDragPayload*)payload->Data;
				playlist.insert(playlist.begin() + i, { .source = animation->source, .name = animation->name, .fx = animation->fx });
				playlist_selected = i;

				if (playlist_index >= i)
				{
					++playlist_index;
				}
			}

			ImGui::EndDragDropTarget();
		}

		if (ImGui::BeginPopupContextItem("##actor_anim_playlist_context"))
		{
			if (ImGui::MenuItem("Play from here"))
			{
				play_playlist(i);
			}

			if (ImGui::MenuItem("Remove"))
			{
				remove_playlist(i);
				ImGui::EndPopup();
				ImGui::PopID();
				break;
			}

			ImGui::EndPopup();
		}

		ImGui::PopID();
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

		imgui_actor_animation_manager.stop_playlist();
		return;
	}

	if (!g_pGameLevel)
	{
		imgui_actor_animation_manager.override_mode = false;
		imgui_actor_animation_manager.stop_playlist();
		return;
	}

	ImGui::SetNextWindowSize(ImVec2(820.0f, 700.0f), ImGuiCond_FirstUseEver);
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
