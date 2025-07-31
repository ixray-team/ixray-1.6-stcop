#pragma once

// Animation clip structure
struct AnimClipItem 
{
	MotionID mid;
	void clear() { mid.invalidate(); }
	bool valid() const { return mid.valid(); }
};

class TClipMaker;
class CAnimationClip
{
public:
	AnimClipItem animItems[4];
	shared_str name;
	float start_time;
	float length;
	int idx;
	TClipMaker* owner;

	CAnimationClip(const char* n, TClipMaker* own) : name(n), owner(own), start_time(0.0f), length(2.0f), idx(-1) {}
	CAnimationClip(TClipMaker* own) : owner(own), start_time(0.0f), length(2.0f), idx(-1) {}

	float Length() const { return length; }
	const float& StartTime() const { return start_time; }

	int PWidthUI();
	int PLeftUI();
	int PRightUI() { return PLeftUI() + PWidthUI(); }

	void SetCycle(MotionID mid, u16 part_id, u8 part_count)
	{
		for (int k = 0; k < part_count; ++k) {
			if (k == part_id || part_id == BI_NONE)
				animItems[k].mid = mid;
			else
				animItems[k].clear();
		}
	}
};

class TClipMaker:
	public IEditorWnd
{
public:
	// Window state
	bool is_docked = false;

	// Clip management
	std::vector<CAnimationClip*> clips;
	CAnimationClip* sel_clip = nullptr;
	u32 play_clip = 0;
	float m_CurrentPlayTime = 0.0f;
	float m_TotalLength = 0.0f;
	float m_Zoom = 24.0f;

	// UI state
	bool show_clip_props = true;
	bool show_anim_select = true;

	// Animation player state
	bool is_playing = false;
	bool is_looped = false;

	// Colors
	ImVec4 CLIP_INACTIVE_COLOR = ImVec4(0.41f, 0.41f, 0.41f, 1.0f); // 0x00686868
	ImVec4 CLIP_ACTIVE_COLOR = ImVec4(0.63f, 0.63f, 0.63f, 1.0f);   // 0x00A1A1A1
	ImVec4 CLIP_ACTIVE_DRAG_COLOR = ImVec4(1.0f, 1.0f, 1.0f, 1.0f); // 0x00FFFFFF
	ImVec4 BP_INACTIVE_COLOR = ImVec4(0.41f, 0.41f, 0.41f, 1.0f);   // 0x00686868
	ImVec4 BP_ACTIVE_COLOR = ImVec4(0.63f, 0.63f, 0.63f, 1.0f);    // 0x00A1A1A1
	ImVec4 BP_ACTIVE_DRAG_COLOR = ImVec4(1.0f, 1.0f, 1.0f, 1.0f);  // 0x00FFFFFF

	// Drag and drop state
	int drag_obj = 0xFFFF;
	bool g_resizing = false;
	int g_X_prev = 0;
	int g_X_dx = 0;

	// Bone parts
	xr_string bp_names[4] = { "-", "-", "-", "-" };

public:
	virtual void Draw() override
	{
		if (!bOpen) return;

		ImGui::SetNextWindowSize(ImVec2(735, 579), ImGuiCond_FirstUseEver);
		ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.27f, 0.27f, 0.28f, 1.0f)); // 6908265

		if (ImGui::Begin("Clip Maker", &bOpen,
			ImGuiWindowFlags_NoScrollbar |
			ImGuiWindowFlags_MenuBar |
			(is_docked ? 0 : ImGuiWindowFlags_NoDocking))) {

			if (ImGui::BeginMenuBar()) {
				if (ImGui::BeginMenu("File")) {
					if (ImGui::MenuItem("Load Clips")) LoadClips();
					if (ImGui::MenuItem("Save Clips")) SaveClips();
					ImGui::EndMenu();
				}
				ImGui::EndMenuBar();
			}

			RenderMainLayout();
		}
		ImGui::End();
		ImGui::PopStyleColor();
	}

	void ShowEditor(CKinematicsAnimated* O);

	void HideEditor() 
	{
		bOpen = false;
		Clear();
	}

private:
	void RenderMainLayout()
	{
		ImGui::BeginChild("MainLayout", ImVec2(0, 0), false, ImGuiWindowFlags_NoScrollbar);

		// Left panel (129px wide)
		ImGui::BeginChild("LeftPanel", ImVec2(170, 0), true);
		RenderLeftPanel();
		ImGui::EndChild();

		ImGui::SameLine();

		// Right panel (remaining space)
		ImGui::BeginChild("RightPanel", ImVec2(0, 0), true);
		RenderRightPanel();
		ImGui::EndChild();

		ImGui::EndChild();
	}

	void RenderLeftPanel()
	{
		// Clips header
		ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.74f, 0.57f, 0.29f, 1.0f)); // 12698049
		ImGui::Text("Clips:");
		ImGui::PopStyleColor();

		// Bone parts labels
		ImGui::Text("Bone part");
		ImGui::Separator();

		// Bone parts info
		for (int i = 0; i < 4; i++) {
			ImGui::Text("%s", bp_names[i].c_str());
		}

		// Time display

		if (ImGui::Button("Trash", ImVec2(0, 18))) {
			if (!clips.empty()) {
				if (ImGui::IsKeyDown(ImGuiMod_Ctrl)) {
					Clear();
				}
				else {
					RemoveClip(sel_clip);
				}
			}
		}

		ImGui::SameLine();
		ImGui::Text("Time: %.2f", m_CurrentPlayTime);

		// Trash button

		// Transport controls
		if (ImGui::Button("<<", ImVec2(22, 17))) ebPrevClipClick();
		ImGui::SameLine();
		if (ImGui::Button("|>", ImVec2(22, 17))) ebPlayClick();
		ImGui::SameLine();
		if (ImGui::Button("|>[]", ImVec2(26, 17))) ebPlayCycleClick();
		ImGui::SameLine();
		if (ImGui::Button("[]", ImVec2(26, 17))) ebStopClick();
		ImGui::SameLine();
		if (ImGui::Button(">>", ImVec2(22, 17))) ebNextClipClick();

		// Clip list
		ImGui::BeginChild("ClipList", ImVec2(0, -63), true);
		for (auto& clip : clips) {
			if (ImGui::Selectable(clip->name.c_str(), sel_clip == clip)) {
				SelectClip(clip);
			}
		}
		ImGui::EndChild();

		// Clip management buttons
		if (ImGui::Button("Insert", ImVec2(42, 15))) ebInsertClipClick();
		ImGui::SameLine();
		if (ImGui::Button("Append", ImVec2(42, 15))) ebAppendClipClick();
		if (ImGui::Button("Load", ImVec2(42, 15))) ebLoadClipsClick();
		ImGui::SameLine();
		if (ImGui::Button("Save", ImVec2(42, 15))) ebSaveClipsClick();
		if (ImGui::Button("Sync", ImVec2(42, 15))) ebSyncClick();
		ImGui::SameLine();
		if (ImGui::Button("Clear", ImVec2(42, 15))) ebClearClick();
	}
	void ebInsertClipClick() {};
	void ebAppendClipClick() {};
	void ebLoadClipsClick() {};
	void ebSaveClipsClick() {};

	void RenderRightPanel() {
		// Base panel (timeline) - 137px tall
		ImGui::BeginChild("BasePanel", ImVec2(0, 137), true);
		RenderTimeline();
		ImGui::EndChild();

		// Splitter
		float height = ImGui::GetContentRegionAvail().y;
		ImGui::BeginChild("RightSplit", ImVec2(0, height), false);

		// Clip properties
		if (show_clip_props) {
			ImGui::BeginChild("ClipProps", ImVec2(368, 0), true);
			RenderClipProperties();
			ImGui::EndChild();

			ImGui::SameLine();
		}

		// Animation selector
		if (show_anim_select) {
			ImGui::BeginChild("AnimSelect", ImVec2(227, 0), true);
			RenderAnimationSelector();
			ImGui::EndChild();
		}

		ImGui::EndChild();
	}

	void RenderTimeline() {
		ImDrawList* draw_list = ImGui::GetWindowDrawList();
		ImVec2 p = ImGui::GetCursorScreenPos();
		ImVec2 size = ImGui::GetContentRegionAvail();

		// Draw timeline background
		draw_list->AddRectFilled(p, ImVec2(p.x + size.x, p.y + size.y),
			ImGui::ColorConvertFloat4ToU32(ImVec4(0.39f, 0.39f, 0.40f, 1.0f))); // 6316128

		// Draw clips
		for (auto& clip : clips) {
			ImVec2 clip_min(p.x + clip->PLeftUI(), p.y);
			ImVec2 clip_max(p.x + clip->PRightUI(), p.y + size.y);

			ImU32 color = (clip == sel_clip) ?
				((drag_obj == -1) ?
					ImGui::ColorConvertFloat4ToU32(CLIP_ACTIVE_COLOR) :
					ImGui::ColorConvertFloat4ToU32(CLIP_ACTIVE_DRAG_COLOR)) :
				ImGui::ColorConvertFloat4ToU32(CLIP_INACTIVE_COLOR);

			draw_list->AddRectFilled(clip_min, clip_max, color);
			draw_list->AddRect(clip_min, clip_max, IM_COL32_BLACK);

			// Clip name
			ImVec2 text_pos(clip_min.x + 2, clip_min.y + 2);
			draw_list->AddText(text_pos, IM_COL32_BLACK, clip->name.c_str());
		}

		// Draw play head if playing
		if (is_playing) {
			float play_pos = p.x + m_CurrentPlayTime * m_Zoom;
			draw_list->AddLine(
				ImVec2(play_pos, p.y),
				ImVec2(play_pos, p.y + size.y),
				IM_COL32(255, 0, 0, 255), 3.0f);
		}

		// Handle clip selection
		if (ImGui::IsWindowHovered() && ImGui::IsMouseClicked(0)) {
			float mouse_x = ImGui::GetMousePos().x - p.x;
			SelectClip(FindClip(static_cast<int>(mouse_x)));
		}
	}

	void RenderClipProperties()
	{
		m_ClipProps.Draw();
	}

	void RenderAnimationSelector() 
	{
		m_ObjectItems.Draw();
	}

	// Clip management functions
	void InsertClip() {
		std::string name = "clip_" + std::to_string(clips.size());
		CAnimationClip* clip = new CAnimationClip(name.c_str(), this);
		clip->start_time = sel_clip ? sel_clip->StartTime() - 0.001f : 0.0f;
		clips.push_back(clip);
		UpdateClips(true, false);
		SelectClip(clip);
	}

	void AppendClip() {
		std::string name = "clip_" + std::to_string(clips.size());
		CAnimationClip* clip = new CAnimationClip(name.c_str(), this);
		clip->start_time = sel_clip ? sel_clip->StartTime() + sel_clip->Length() - 0.001f : 0.0f;
		clips.push_back(clip);
		UpdateClips(true, false);
		SelectClip(clip);
	}

	void LoadClips();
	void SaveClips();

	void RemoveClip(CAnimationClip* clip) {
		if (!clip) return;

		Stop();
		auto it = std::find(clips.begin(), clips.end(), clip);
		if (it != clips.end()) {
			auto p_it = it;
			p_it++;
			if ((p_it == clips.end()) && (clips.size() > 1)) {
				p_it = it;
				p_it--;
			}

			CAnimationClip* C = (p_it == clips.end()) ? nullptr : *p_it;
			delete* it;
			clips.erase(it);
			SelectClip(C);
			UpdateClips();
		}
	}

	void Clear() {
		Stop();
		for (auto& clip : clips) {
			delete clip;
		}
		clips.clear();
		sel_clip = nullptr;
		UpdateClips(true);
	}

	// Clip selection and navigation
	void SelectClip(CAnimationClip* clip) {
		if (sel_clip != clip) {
			sel_clip = clip;
			RepaintClips();
			UpdateProperties();
		}
	}

	CAnimationClip* FindClip(int x) {
		return FindClip(static_cast<float>(x) / m_Zoom);
	}

	CAnimationClip* FindClip(float t) {
		if (clips.empty()) return nullptr;

		auto it = std::upper_bound(clips.begin(), clips.end(), t,
			[](float val, CAnimationClip* clip) { return val < clip->start_time; });

		if (it != clips.begin()) it--;
		return *it;
	}

	// Transport controls
	void ebPrevClipClick() {
		if (sel_clip) {
			auto it = std::find(clips.begin(), clips.end(), sel_clip);
			if (it != clips.begin()) {
				it--;
				SelectClip(*it);
			}
		}
	}

	void ebNextClipClick() {
		if (sel_clip) {
			auto it = std::find(clips.begin(), clips.end(), sel_clip);
			if (it != clips.end()) {
				it++;
				if (it != clips.end()) {
					SelectClip(*it);
				}
			}
		}
	}

	void ebPlayClick() {
		Play(false);
	}

	void ebPlayCycleClick() {
		Play(true);
	}

	void ebStopClick() {
		Stop();
	}

	void ebSyncClick() {
		// TODO: Implement sync functionality
	}

	void ebClearClick() {
		Clear();
	}

	// Playback control
	void Play(bool bLoop) {
		if (sel_clip) {
			is_playing = true;
			is_looped = bLoop;
			play_clip = sel_clip->idx;
			m_CurrentPlayTime = sel_clip->start_time;
			PlayAnimation(sel_clip);
		}
	}

	void Stop() {
		if (is_playing) {
			is_playing = false;
			m_CurrentPlayTime = 0.0f;
			RepaintClips();
			// TODO: Stop animation playback
		}
	}

	void PlayAnimation(CAnimationClip* clip) {
		if (!clip) return;
		// TODO: Implement animation playback
	}

	// Update functions
	void UpdateClips(bool bForced = false, bool bRepaint = true);
	void RepaintClips(bool bForced = false);

	void UpdateProperties(bool bForced = false);

private:
	CKinematicsAnimated* RenderObject;
	UIItemListForm m_ObjectItems;
	UIItemListForm m_ClipList;
	UIPropertiesForm m_ClipProps;
};

// Global instance
extern TClipMaker* g_clip_maker;