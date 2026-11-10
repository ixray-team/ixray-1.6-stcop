#include "StdAfx.h"
#include "CinemaEditor.h"
#include "../ImUtils.h"
#include "../../../xrEngine/ImGuiManager.h"
#include "../../../xrEngine/Autotest.h"
#include "../../Cinema/CinemaSceneIo.h"
#include "../../Cinema/CinemaCamera.h"
#include "../../../xrEngine/Environment.h"
#include "../../../xrEngine/IGame_Persistent.h"
#include "../../../xrEngine/GameMtlLib.h"

namespace Cinema::EditorUi
{
constexpr const char* ModalTitles[11] = { "", "Open scene", "Import v4 scene", "Save scene as", "Unsaved changes", "", "Save preset", "Delete preset", "", "Render frames", "Save as animation" };
constexpr const char* SettingsFolder = "cinema_editor\\";
constexpr const char* SettingsFile = "cinema_editor\\editor.ltx";
constexpr size_t MaxSettingsSize = 64u << 10;
constexpr const char* AddLabels[7] = { "Add actor", "", "Add light", "Add particle", "Add decal", "Add weather", "Add post" };
xr_unique_ptr<Editor> g_Editor;

bool ScenePath(string_path& Out, const char* Alias, const shared_str& LevelName, const xr_string& File)
{
	string_path Folder;
	Io::SceneFolder(Folder, Alias, LevelName);
	if (xr_strlen(Folder) + File.size() + 5 > sizeof(Out))
	{
		return false;
	}
	xr_sprintf(Out, "%s%s.ltx", Folder, File.c_str());
	return true;
}

bool IsPresetBefore(const ActorPreset& A, const ActorPreset& B)
{
	return A.Name < B.Name;
}

void PutPreset(Object& Target, const ActorPreset& Preset)
{
	const Fvector Position = Target.Actor.Position, Rotation = Target.Actor.Rotation;
	Target.Actor = Preset.Setup;
	Target.Actor.Position = Position;
	Target.Actor.Rotation = Rotation;
	std::erase_if(Target.Lanes, [&Target](const Lane& Track) { return Track.Id == Param::LayerWeight && Track.Layer > Target.Actor.Layers.size(); });
	Target.Name = Target.IsNameLocked ? Target.Name : ModelName(Preset.Setup.Visual);
}
}

namespace Cinema
{
Editor::Editor()
{
	LoadSettings();
}

void Editor::Draw(Player& Core, bool& IsOpen)
{
	WasOpen = true;
	BeginFrame(Core);
	if (Job.IsActive)
	{
		StepRender(Core);
		EndFrame(Core);
		return;
	}
	DrawGizmo(Core);
	DrawTangents(Core);
	DrawIkHandles(Core);
	DrawPathKeys(Core);
	UpdateFly(Core);
	DrawViewCube(Core);
	Pick(Core);
	PickColor(Core);
	PickFocus(Core);
	DrawHelpers(Core);
	DrawGhosts(Core);
	DrawFrameLines(Core);
	DrawBones(Core);
	DrawViewportPopup(Core);
	DrawTimeline(Core, IsOpen);
	DrawToolbar(Core);
	DrawInspector(Core);
	DrawPresets();
	DrawShots(Core);
	DrawGraph(Core);
	HandleShortcuts(Core);
	DrawModals(Core);
	EndFrame(Core);
}

void Editor::DrawHidden(const Player& Core)
{
	Overlay = ImGui::GetForegroundDrawList();
	DrawFrameLines(Core);
	Overlay = nullptr;
}

void Editor::Close(Player& Core)
{
	if (Core.HasRequests())
	{
		return;
	}
	if (Job.IsActive)
	{
		StopRender(Core, "The editor was closed.");
	}
	BeginFrame(Core);
	Drag = DragKind::None;
	RenameId = 0;
	Undo.End(Core.Data);
	Latches.clear();
	Core.SetAudition(0, shared_str(), shared_str());
	Core.Soloed.clear();
	ReleasePreview();
	Core.HasUnsavedChanges = Undo.Serial() != SavedSerial || IsLayoutDirty;
	CImGuiManager::Instance().SetMouseLook(false);
	IsEditorCamera = IsFlying = IsFlyArmed = IsPanning = IsGizmoActive = IsPickArmed = false;
	Core.IsLookingThrough = true;
	Core.SetEditorView(false, false, Eye, Fidentity);
	WasOpen = false;
}

void Editor::BeginFrame(Player& Core)
{
	Core.IsDofPreview = Settings.DofPreview;
	if (SeenGeneration != Core.Generation)
	{
		SeenGeneration = Core.Generation;
		SceneName.clear();
		SceneFile.clear();
		Modal = ModalKind::None;
		IsLibraryBuilt = false;
		ResetScene(true);
	}
	if (EyeLevel != Core.Data.LevelName)
	{
		EyeLevel = Core.Data.LevelName;
		IsEyeValid = IsEditorCamera = false;
	}
	std::erase_if(Core.Soloed, [&Core](u32 Id) { return !Find(Core.Data, Id); });
	const Object* Rig = FindCamera(Core.Data);
	const xr_vector<PoseKey>* Keys = Rig && TangentBlock >= 0 && TangentBlock < static_cast<s32>(Rig->Camera.Blocks.size()) ? &Rig->Camera.Blocks[TangentBlock].Keys : nullptr;
	const u32 Count = Keys ? static_cast<u32>(Keys->size()) : 0;
	if (Count != TangentCount || TangentKey < 0 || TangentKey >= static_cast<s32>(Count))
	{
		const auto Found = Keys ? std::find_if(Keys->begin(), Keys->end(), [this](const PoseKey& Item) { return std::fabs(Item.Time - TangentTime) < 1e-4f; }) : xr_vector<PoseKey>::const_iterator();
		TangentKey = Keys && Found != Keys->end() ? static_cast<s32>(Found - Keys->begin()) : -1;
	}
	TangentTime = TangentKey >= 0 ? (*Keys)[TangentKey].Time : TangentTime;
	TangentCount = Count;
	EndStep(Core);
}

void Editor::SetTangentKey(const Player& Core, s32 Block, s32 Key)
{
	const Object* Rig = FindCamera(Core.Data);
	const bool IsValid = Rig && Block >= 0 && Block < static_cast<s32>(Rig->Camera.Blocks.size()) && Key >= 0 && Key < static_cast<s32>(Rig->Camera.Blocks[Block].Keys.size());
	TangentBlock = IsValid ? Block : -1;
	TangentKey = IsValid ? Key : -1;
	TangentCount = IsValid ? static_cast<u32>(Rig->Camera.Blocks[Block].Keys.size()) : 0;
	TangentTime = IsValid ? Rig->Camera.Blocks[Block].Keys[Key].Time : 0.0f;
}

void Editor::EndFrame(Player& Core)
{
	for (u32 Index = 0; Undo.IsOpen() && Index < Latches.size(); ++Index)
	{
		Record(Core, Latches[Index]);
	}
	EndStep(Core);
	if (UndoRequest && Drag == DragKind::None && !Undo.IsOpen())
	{
		UndoRedo(Core, UndoRequest > 0);
		UndoRequest = 0;
	}
	if (!IsChooserOpen || Core.IsPlaying)
	{
		Core.SetAudition(0, shared_str(), shared_str());
	}
	IsChooserOpen = false;
	Core.HasUnsavedChanges = Undo.Serial() != SavedSerial || IsLayoutDirty;
	if (Settings != SavedSettings && !IsFlying && !ImGui::IsAnyItemActive())
	{
		SaveSettings();
	}
}

void Editor::EndStep(Player& Core)
{
	if (Undo.IsOpen() && Drag == DragKind::None && !IsGizmoActive && !IsTangentActive && !IsIkActive && !IsPathKeyActive && !ImGui::IsAnyItemActive() && !Core.HasRequests())
	{
		u8 Used = 0;
		Object* Rig = FindCamera(Core.Data);
		for (u32 Index = 0; Rig && Index < Rig->Camera.Shakes.size(); ++Index)
		{
			Used = static_cast<u8>(Used | (1u << Rig->Camera.Shakes[Index].Row));
		}

		if (Rig && (Rig->Camera.OverrideRows & ~Used))
		{
			Undo.Touch(Core.Data, Rig->Id);
			Rig->Camera.OverrideRows &= Used;
		}
		Undo.End(Core.Data);

		PruneSelection(Core.Data);
	}
	if (!Undo.IsOpen())
	{
		Latches.clear();
	}
}

void Editor::ResetScene(bool IsSaved)
{
	Undo.Clear();
	SavedSerial = IsSaved ? Undo.Serial() : 0;
	IsLayoutDirty = false;
	Selected.clear();
	SelectedKeys.clear();
	Slots.clear();
	Expanded.clear();
	Anchor = SelectedFolder = RenameId = FocusObject = 0;
	FocusBlock = DragMarker = HoverMarker = PopupMarker = RenameMarker = TangentBlock = TangentKey = -1;
	FocusLayer = PopupLayer = DragLayer = 0;
	UndoRequest = 0;
	Focus = FocusKind::Objects;
	Drag = DragKind::None;
	IsGizmoActive = IsPickingColor = IsPickingFocus = IsTangentActive = false;
}

void Editor::HandleShortcuts(Player& Core)
{
	const ImGuiID Active = ImGui::GetActiveID();
	if (!CImGuiManager::Instance().IsCapturingInputs() || ImGui::GetIO().WantTextInput || (Active && Active != CanvasId) || (ImGui::IsWindowFocused(ImGuiFocusedFlags_AnyWindow) && !IsOwnWindowFocused))
	{
		return;
	}
	const float Rate = static_cast<float>(Core.Data.Header.FrameRate);
	const float Frame = std::round(Core.Time * Rate);
	if (ImGui::IsKeyPressed(ImGuiKey_Space, false) && !IsFlyArmed)
	{
		PlayPause(Core, ImGui::GetIO().KeyShift);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_Home, false))
	{
		SeekBoth(Core, 0.0f);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_End, false))
	{
		SeekBoth(Core, ContentEnd(Core.Data));
	}
	if (ImGui::IsKeyPressed(ImGuiKey_LeftArrow))
	{
		Core.Seek((Frame - 1.0f) / Rate);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_RightArrow))
	{
		Core.Seek((Frame + 1.0f) / Rate);
	}
	if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_Z))
	{
		UndoRequest = -1;
	}
	if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_Y) || ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiMod_Shift | ImGuiKey_Z))
	{
		UndoRequest = 1;
	}
	if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_S))
	{
		Save(Core);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_Delete, false) && Drag == DragKind::None)
	{
		DeleteSelection(Core);
	}
	const bool IsIdle = Drag == DragKind::None && !IsGizmoActive && !IsFlying;
	if (IsIdle && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_C))
	{
		Copy(Core, Copied);
	}
	if (IsIdle && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_V))
	{
		Paste(Core, Copied, Core.Time);
	}
	if (IsIdle && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_D))
	{
		Duplicate(Core);
	}
	if (IsIdle && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_L))
	{
		LoopSelection(Core);
	}
	if (IsIdle && !IsFlyArmed && ImGui::IsKeyChordPressed(ImGuiKey_K))
	{
		IsAutoKey = !IsAutoKey;
	}
	if (IsIdle && !IsFlyArmed && ImGui::IsKeyChordPressed(ImGuiKey_B))
	{
		IsPosing = !IsPosing;
	}
	const ImGuiKey Tools[4] = { ImGuiKey_W, ImGuiKey_E, ImGuiKey_R, ImGuiKey_Q };
	for (u32 Index = 0; Index < 4; ++Index)
	{
		if (IsIdle && !IsFlyArmed && !IsGraphFocused && ImGui::IsKeyChordPressed(Tools[Index]))
		{
			Gizmo = Index == 3 ? GizmoMode::Universal : static_cast<GizmoMode>(Index);
		}
	}
	if (IsIdle && !IsFlyArmed && !IsGraphFocused && ImGui::IsKeyChordPressed(ImGuiKey_X))
	{
		bool& IsLocal = GizmoItems.size() > 1 ? IsGizmoLocalMulti : IsGizmoLocalSingle;
		IsLocal = !IsLocal;
	}
	if (IsIdle && !IsFlyArmed && !IsGraphFocused && ImGui::IsKeyChordPressed(ImGuiKey_V))
	{
		if (Core.IsLookingThrough)
		{
			UseEditorCamera(Core);
		}
		else
		{
			Core.IsLookingThrough = true;
		}
	}
	if (IsIdle && !IsFlyArmed && ImGui::IsKeyChordPressed(ImGuiKey_G))
	{
		Settings.ShowSafe = !Settings.ShowSafe;
	}
	if (IsIdle && !IsFlyArmed && !IsGraphFocused && ImGui::IsKeyChordPressed(ImGuiKey_F))
	{
		IsPosing && !SelectedBones.empty() ? KeyBones(Core) : KeyPlayhead(Core);
	}
	float Lo = 0.0f, Hi = 0.0f;
	if (IsIdle && !IsFlyArmed && !IsGraphFocused && ImGui::IsKeyChordPressed(ImGuiKey_Z))
	{
		if (!SelectionSpan(Core, Lo, Hi))
		{
			Lo = Core.HasLoopRange ? Core.LoopStart : 0.0f;
			Hi = Core.HasLoopRange ? Core.LoopEnd : std::max(ContentEnd(Core.Data), 1.0f);
		}
		ZoomTo(Lo, Hi);
	}
	if (IsIdle && !IsFlyArmed && (ImGui::IsKeyChordPressed(ImGuiKey_Equal) || ImGui::IsKeyChordPressed(ImGuiKey_KeypadAdd) || ImGui::IsKeyChordPressed(ImGuiMod_Shift | ImGuiKey_Equal)))
	{
		ZoomAround(1.25f, Core.Time);
	}
	if (IsIdle && !IsFlyArmed && (ImGui::IsKeyChordPressed(ImGuiKey_Minus) || ImGui::IsKeyChordPressed(ImGuiKey_KeypadSubtract)))
	{
		ZoomAround(0.8f, Core.Time);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_F2, false) && (SelectedFolder || Selected.size() == 1))
	{
		StartRename(Core.Data, SelectedFolder ? SelectedFolder : Selected[0], SelectedFolder != 0);
	}
}

void Editor::UndoRedo(Player& Core, bool IsRedo)
{
	if (Drag == DragKind::None && (IsRedo ? Undo.Redo(Core.Data, Core.LoopStart, Core.LoopEnd) : Undo.Undo(Core.Data, Core.LoopStart, Core.LoopEnd)))
	{
		SelectedKeys.clear();
		PruneSelection(Core.Data);
		const Object* Focused = Find(Core.Data, FocusObject);
		if (Focused && Focused->Type == ObjectType::Actor)
		{
			FocusBlock = -1;
			FocusLayer = 0;
			Focus = Focus == FocusKind::Block ? FocusKind::Objects : Focus;
		}
		SetStatus(false, "%s %s", IsRedo ? "Redo" : "Undo", IsRedo ? Undo.UndoLabel() : Undo.RedoLabel());
	}
}

void Editor::LoadSettings()
{
	string_path Folder, Path;
	FS.update_path(Folder, Io::LegacyAlias, EditorUi::SettingsFolder);
	FS.rescan_path(Folder, false, false);
	FS.update_path(Path, Io::LegacyAlias, EditorUi::SettingsFile);
	const CReaderGuarded File(FS.exist(Path) ? FS.r_open(Path) : nullptr);
	xr_string Error = "the file is larger than 64 KiB";
	if (!File)
	{
		return;
	}
	if (static_cast<size_t>(File->length()) > EditorUi::MaxSettingsSize || !Io::PreScan(*File, Error))
	{
		Msg("! Cinema editor: %s is ignored: %s", Path, Error.c_str());
		return;
	}
	File->seek(0);
	const CInifile Ini(&*File, nullptr);
	Io::Warnings Notes;
	const auto ReadFloat = [&Ini, &Notes](const char* Line, float& Value, float Lo, float Hi)
	{
		float Read = Value;
		Io::ReadFloats(Ini, "editor", Line, &Read, 1, Notes);
		Value = Sanitize(Read, Lo, Hi, Value);
	};
	ReadFloat("fly_speed", Settings.FlySpeed, 0.25f, 64.0f);
	ReadFloat("move_step", Settings.MoveStep, 0.01f, 10.0f);
	ReadFloat("rotate_step", Settings.RotateStep, 0.5f, 180.0f);
	ReadFloat("scale_step", Settings.ScaleStep, 0.01f, 1.0f);
	ReadFloat("range_step", Settings.RangeStep, 0.01f, 10.0f);
	ReadFloat("header_width", Settings.HeaderWidth, MinHeaderWidth, MaxHeaderWidth);
	ReadFloat("opacity", Settings.Opacity, 0.1f, 1.0f);
	ReadFloat("look_sensitivity", Settings.LookSensitivity, 0.1f, 4.0f);
	ReadFloat("onion_count", Settings.OnionCount, 1.0f, 5.0f);
	ReadFloat("onion_step", Settings.OnionStep, 1.0f, 30.0f);
	Settings.ShowOnion = Io::ReadBool(Ini, "editor", "onion", Settings.ShowOnion, Notes);
	Settings.ShowPaths = Io::ReadBool(Ini, "editor", "paths", Settings.ShowPaths, Notes);
	Settings.ShowSafe = Io::ReadBool(Ini, "editor", "safe_areas", Settings.ShowSafe, Notes);
	Settings.ShowThirds = Io::ReadBool(Ini, "editor", "thirds", Settings.ShowThirds, Notes);
	ReadFloat("frame_aspect", Settings.FrameRatio, 0.0f, 8.0f);
	ReadFloat("frame_mask", Settings.FrameMask, 0.0f, 1.0f);
	Settings.FrameRatio = std::round(Settings.FrameRatio);
	ReadFloat("fly_smoothing", Settings.FlySmoothing, 0.0f, 0.5f);
	ReadFloat("record_smoothing", Settings.RecordSmoothing, 0.0f, 0.5f);
	ReadFloat("render_warmup", Settings.RenderWarmup, 0.0f, 30.0f);
	ReadFloat("row_height", Settings.DefaultRow, 1.0f, MaxRowSize);
	ReadFloat("lane_height", Settings.DefaultLane, 1.0f, MaxLaneSize);
	Settings.IsSnapping = Io::ReadBool(Ini, "editor", "snap", Settings.IsSnapping, Notes);
	Settings.SnapStep = static_cast<u8>(Io::ReadU32(Ini, "editor", "snap_step", 6, Notes));
	Settings.ShowHelpers = Io::ReadBool(Ini, "editor", "helpers", Settings.ShowHelpers, Notes);
	Settings.ShowHints = Io::ReadBool(Ini, "editor", "hints", Settings.ShowHints, Notes);
	Settings.IsGizmoSnapping = Io::ReadBool(Ini, "editor", "gizmo_snap", Settings.IsGizmoSnapping, Notes);
	Settings.DofPreview = Io::ReadBool(Ini, "editor", "dof_preview", Settings.DofPreview, Notes);
	Settings.IsFollowing = Io::ReadBool(Ini, "editor", "follow", Settings.IsFollowing, Notes);
	Settings.ShowInspector = Io::ReadBool(Ini, "editor", "inspector", Settings.ShowInspector, Notes);
	Settings.ShowPresets = Io::ReadBool(Ini, "editor", "presets", Settings.ShowPresets, Notes);
	Settings.ShowShots = Io::ReadBool(Ini, "editor", "shots", Settings.ShowShots, Notes);
	Settings.ShowGraph = Io::ReadBool(Ini, "editor", "graph", Settings.ShowGraph, Notes);
	for (const xr_string& Line : Notes.Lines)
	{
		Msg("! Cinema editor: %s: %s", Path, Line.c_str());
	}
	SavedSettings = Settings;
}

void Editor::SaveSettings()
{
	SavedSettings = Settings;
	CInifile Ini(nullptr, false, false, false);
	Ini.w_bool("editor", "snap", Settings.IsSnapping);
	Ini.w_u32("editor", "snap_step", Settings.SnapStep);
	Ini.w_float("editor", "fly_speed", Settings.FlySpeed);
	Ini.w_float("editor", "move_step", Settings.MoveStep);
	Ini.w_float("editor", "rotate_step", Settings.RotateStep);
	Ini.w_float("editor", "scale_step", Settings.ScaleStep);
	Ini.w_float("editor", "range_step", Settings.RangeStep);
	Ini.w_float("editor", "header_width", Settings.HeaderWidth);
	Ini.w_float("editor", "opacity", Settings.Opacity);
	Ini.w_float("editor", "look_sensitivity", Settings.LookSensitivity);
	Ini.w_float("editor", "fly_smoothing", Settings.FlySmoothing);
	Ini.w_float("editor", "record_smoothing", Settings.RecordSmoothing);
	Ini.w_float("editor", "render_warmup", Settings.RenderWarmup);
	Ini.w_float("editor", "row_height", Settings.DefaultRow);
	Ini.w_float("editor", "lane_height", Settings.DefaultLane);
	Ini.w_bool("editor", "helpers", Settings.ShowHelpers);
	Ini.w_bool("editor", "hints", Settings.ShowHints);
	Ini.w_bool("editor", "gizmo_snap", Settings.IsGizmoSnapping);
	Ini.w_bool("editor", "onion", Settings.ShowOnion);
	Ini.w_bool("editor", "paths", Settings.ShowPaths);
	Ini.w_bool("editor", "safe_areas", Settings.ShowSafe);
	Ini.w_bool("editor", "thirds", Settings.ShowThirds);
	Ini.w_float("editor", "frame_aspect", Settings.FrameRatio);
	Ini.w_float("editor", "frame_mask", Settings.FrameMask);
	Ini.w_float("editor", "onion_count", Settings.OnionCount);
	Ini.w_float("editor", "onion_step", Settings.OnionStep);
	Ini.w_bool("editor", "dof_preview", Settings.DofPreview);
	Ini.w_bool("editor", "follow", Settings.IsFollowing);
	Ini.w_bool("editor", "inspector", Settings.ShowInspector);
	Ini.w_bool("editor", "presets", Settings.ShowPresets);
	Ini.w_bool("editor", "shots", Settings.ShowShots);
	Ini.w_bool("editor", "graph", Settings.ShowGraph);
	string_path Path;
	xr_string Error;
	FS.update_path(Path, Io::LegacyAlias, EditorUi::SettingsFile);
	if (!Io::SaveIni(Ini, Path, Error))
	{
		SetStatus(true, "%s", Error.c_str());
	}
}

void Editor::DrawMenuBar(Player& Core)
{
	if (!ImGui::BeginMenuBar())
	{
		return;
	}
	if (ImGui::BeginMenu("File"))
	{
		if (ImGui::MenuItem("New"))
		{
			RequestScene(Core, PendingKind::New, false);
		}
		if (ImGui::MenuItem("Open..."))
		{
			RequestScene(Core, PendingKind::Open, false);
		}
		if (ImGui::MenuItem("Import v4..."))
		{
			RequestScene(Core, PendingKind::Import, false);
		}
		ImGui::Separator();
		if (ImGui::MenuItem("Save", "Ctrl+S"))
		{
			Save(Core);
		}
		if (ImGui::MenuItem("Save as..."))
		{
			ShowModal(Core, ModalKind::SaveAs);
		}
		ImGui::Separator();
		if (ImGui::MenuItem("Render frames...", nullptr, false, FindCamera(Core.Data) != nullptr))
		{
			RenderAspect = static_cast<s32>(Settings.FrameRatio);
			Modal = ModalKind::Render;
		}
		ImGui::EndMenu();
	}
	if (ImGui::BeginMenu("Edit"))
	{
		const char* UndoLabel = Undo.UndoLabel();
		const char* RedoLabel = Undo.RedoLabel();
		string128 Text;
		xr_sprintf(Text, "Undo %s###Undo", UndoLabel ? UndoLabel : "");
		if (ImGui::MenuItem(Text, "Ctrl+Z", false, UndoLabel != nullptr))
		{
			UndoRequest = -1;
		}
		xr_sprintf(Text, "Redo %s###Redo", RedoLabel ? RedoLabel : "");
		if (ImGui::MenuItem(Text, "Ctrl+Y", false, RedoLabel != nullptr))
		{
			UndoRequest = 1;
		}
		ImGui::Separator();
		const bool CanCopy = (Focus == FocusKind::Keys && !SelectedKeys.empty()) || Focus == FocusKind::Block || !Selected.empty();
		if (ImGui::MenuItem("Copy", "Ctrl+C", false, CanCopy))
		{
			Copy(Core, Copied);
		}
		if (ImGui::MenuItem("Paste", "Ctrl+V", false, Copied.Kind != ClipKind::None))
		{
			Paste(Core, Copied, Core.Time);
		}
		if (ImGui::MenuItem("Duplicate", "Ctrl+D", false, CanCopy))
		{
			Duplicate(Core);
		}
		if (ImGui::MenuItem("Delete", "Delete", false, SelectedFolder || !Selected.empty()))
		{
			DeleteSelection(Core);
		}
		ImGui::Separator();
		if (ImGui::MenuItem("Add marker"))
		{
			AddMarker(Core, Core.Time);
		}
		for (u32 Index = 0; Index < 2; ++Index)
		{
			if (ImGui::MenuItem(Index ? "Remove time..." : "Insert time..."))
			{
				IsRemovingTime = Index != 0;
				Modal = ModalKind::ShiftTime;
			}
		}
		ImGui::EndMenu();
	}
	if (ImGui::BeginMenu("Add"))
	{
		LoadPresets();
		for (u32 Index = 0; Index < std::size(AddChoices); ++Index)
		{
			if (ImGui::MenuItem(AddChoices[Index], nullptr, false, Index != 1 || !Presets.empty()))
			{
				PopupTime = Core.Time;
				PopupRequest = "##Add";
				PopupKind = AddKinds[Index];
				IsPopupChoosing = IsChooserFresh = true;
			}
		}
		if (ImGui::MenuItem("Light"))
		{
			AddLight(Core, Core.Time, Fvector().mad(Device.vCameraPosition, Device.vCameraDirection, 3.0f));
		}
		if (ImGui::MenuItem("Weather"))
		{
			AddWeather(Core, Core.Time);
		}
		if (ImGui::MenuItem("Post"))
		{
			AddPost(Core, Core.Time);
		}
		if (ImGui::MenuItem("Camera move"))
		{
			AddCameraMove(Core, Core.Time);
		}
		if (ImGui::MenuItem("Folder"))
		{
			AddFolder(Core);
		}
		ImGui::EndMenu();
	}
	if (ImGui::BeginMenu("View"))
	{
		if (ImGui::MenuItem("Game camera", nullptr, !IsEditorCamera))
		{
			IsEditorCamera = false;
		}
		if (ImGui::MenuItem("Editor camera", nullptr, IsEditorCamera))
		{
			UseEditorCamera(Core);
		}
		if (ImGui::MenuItem("Reset FOV", nullptr, false, EyeFov > 0.0f))
		{
			EyeFov = 0.0f;
		}
		if (ImGui::MenuItem("Reset roll", nullptr, false, Roll != 0.0f))
		{
			Roll = 0.0f;
		}
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 8.0f);
		ImGui::SliderFloat("Look sensitivity", &Settings.LookSensitivity, 0.1f, 4.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp | ImGuiSliderFlags_Logarithmic);
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 8.0f);
		ImGui::SliderFloat("Fly smoothing", &Settings.FlySmoothing, 0.0f, 0.5f, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 8.0f);
		ImGui::SliderFloat("Record smoothing", &Settings.RecordSmoothing, 0.0f, 0.5f, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
		ImGui::Separator();
		ImGui::MenuItem("Helpers", nullptr, &Settings.ShowHelpers);
		ImGui::MenuItem("Hints", nullptr, &Settings.ShowHints);
		ImGui::MenuItem("DOF preview", nullptr, &Settings.DofPreview);
		ImGui::MenuItem("Inspector", nullptr, &Settings.ShowInspector);
		ImGui::MenuItem("Presets", nullptr, &Settings.ShowPresets);
		ImGui::MenuItem("Shots & Markers", nullptr, &Settings.ShowShots);
		ImGui::MenuItem("Graph Editor", nullptr, &Settings.ShowGraph);
		ImGui::Separator();
		ImGui::MenuItem("Onion skin", nullptr, &Settings.ShowOnion);
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 8.0f);
		ImGui::SliderFloat("Onion count", &Settings.OnionCount, 1.0f, 5.0f, "%.0f", ImGuiSliderFlags_AlwaysClamp);
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 8.0f);
		ImGui::SliderFloat("Onion step", &Settings.OnionStep, 1.0f, 30.0f, "%.0f frames", ImGuiSliderFlags_AlwaysClamp);
		ImGui::MenuItem("Motion paths", nullptr, &Settings.ShowPaths);
		ImGui::MenuItem("Bones", "B", &IsPosing);
		ImGui::Separator();
		s32 Ratio = static_cast<s32>(Settings.FrameRatio);
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 8.0f);
		if (ImGui::Combo("Frame lines", &Ratio, "Off\0" "2.39:1\0" "21:9\0" "2:1\0" "1.85:1\0" "16:9\0" "4:3\0" "1:1\0" "9:16\0"))
		{
			Settings.FrameRatio = static_cast<float>(Ratio);
		}
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 8.0f);
		ImGui::SliderFloat("Frame mask", &Settings.FrameMask, 0.0f, 1.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
		ImGui::MenuItem("Safe areas", "G", &Settings.ShowSafe);
		ImGui::MenuItem("Thirds", nullptr, &Settings.ShowThirds);
		ImGui::EndMenu();
	}
	ImGui::EndMenuBar();
}

void Editor::DrawModals(Player& Core)
{
	if (Modal != ModalKind::None && !ImGui::IsPopupOpen("###CinemaModal"))
	{
		ImGui::OpenPopup("###CinemaModal");
	}
	string32 Code;
	string64 Shift;
	string256 Title;
	Timecode(Code, Core.Time, Core.Data.Header.FrameRate);
	xr_sprintf(Shift, "%s time at %s", IsRemovingTime ? "Remove" : "Insert", Code);
	xr_sprintf(Title, "%.128s###CinemaModal", Modal == ModalKind::Message ? MessageTitle.c_str() : Modal == ModalKind::ShiftTime ? Shift : EditorUi::ModalTitles[static_cast<u32>(Modal)]);
	if (!ImGui::BeginPopupModal(Title, nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		return;
	}
	if (Modal == ModalKind::Message)
	{
		ImGui::SetNextWindowSizeConstraints(ImVec2(560.0f, 0.0f), ImVec2(560.0f, 16.0f * ImGui::GetTextLineHeightWithSpacing()));
		ImGui::BeginChild("##Lines", ImVec2(560.0f, 0.0f), ImGuiChildFlags_Borders | ImGuiChildFlags_AutoResizeY);
		ImGui::PushTextWrapPos(0.0f);
		for (const xr_string& Line : MessageLines)
		{
			ImGui::TextUnformatted(Line.c_str());
		}
		ImGui::PopTextWrapPos();
		ImGui::EndChild();
		if (ImGui::Button("OK"))
		{
			Modal = ModalKind::None;
		}
	}
	else if (Modal == ModalKind::Discard)
	{
		ImGui::TextUnformatted("The scene has unsaved changes. Discard them?");
		if (ImGui::Button("Discard"))
		{
			RequestScene(Core, Pending, true);
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			Modal = ModalKind::None;
		}
	}
	else if (Modal == ModalKind::Render)
	{
		DrawRenderModal(Core);
	}
	else if (Modal == ModalKind::SaveMotion)
	{
		DrawMotionModal(Core);
	}
	else if (Modal == ModalKind::ShiftTime)
	{
		const float Rate = static_cast<float>(Core.Data.Header.FrameRate);
		ImGui::SetNextItemWidth(160.0f);
		TimeField("Amount", ShiftAmount, MinLength, MaxTime, Core.Data.Header.FrameRate);
		if (ImGui::Button(IsRemovingTime ? "Remove" : "Insert"))
		{
			const float Frames = std::max(std::round(ShiftAmount * Rate), 1.0f);
			Modal = ModalKind::None;
			ShiftTime(Core, Core.Time, (IsRemovingTime ? -Frames : Frames) / Rate);
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			Modal = ModalKind::None;
		}
	}
	else if (Modal == ModalKind::DeletePreset)
	{
		const bool IsListed = PresetSelected >= 0 && PresetSelected < static_cast<s32>(Presets.size());
		ImGui::Text("Delete preset \"%s\"?", IsListed ? Presets[PresetSelected].Name.c_str() : "");
		if (ImGui::Button("Delete") && IsListed)
		{
			xr_vector<ActorPreset> Next = Presets;
			const xr_string Name = Next[PresetSelected].Name;
			Next.erase(Next.begin() + PresetSelected);
			PresetSelected = -1;
			Modal = ModalKind::None;
			StorePresets(std::move(Next), Name);
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			Modal = ModalKind::None;
		}
	}
	else if (Modal == ModalKind::SaveAs || Modal == ModalKind::SavePreset)
	{
		const bool IsPreset = Modal == ModalKind::SavePreset;
		const s32 Taken = IsPreset ? FindPreset(NameBuffer) : -1;
		const bool IsTaken = IsPreset ? Taken >= 0 : IsNameTaken;
		const char* Problem = !IsPreset ? NameError.c_str() : !NameBuffer[0] ? "Enter a preset name." : xr_strlen(NameBuffer) > MaxNameBytes ? "The name is longer than 127 bytes." : "";
		if (ImGui::IsWindowAppearing())
		{
			ImGui::SetKeyboardFocusHere();
		}
		ImGui::PushStyleColor(ImGuiCol_Text, IsTaken ? ErrorColor : ImGui::GetStyleColorVec4(ImGuiCol_Text));
		ImGui::SetNextItemWidth(360.0f);
		if (ImGui::InputText("##Name", NameBuffer, sizeof(NameBuffer)) && !IsPreset)
		{
			CheckName();
		}
		ImGui::PopStyleColor();
		ImGui::TextColored(ErrorColor, "%s", !NameBuffer[0] ? "" : Problem[0] ? Problem : !IsTaken ? "" : IsPreset ? "Replaces the existing preset." : "Replaces the existing scene.");
		ImGui::BeginDisabled(Problem[0] != 0);
		const bool IsSaving = ImGui::Button("Save");
		if (IsSaving && !IsPreset)
		{
			SaveScene(Core, NameFile);
		}
		if (IsSaving && IsPreset)
		{
			xr_vector<ActorPreset> Next = Presets;
			PresetDraft.Name = NameBuffer;
			Taken < 0 ? Next.push_back(PresetDraft) : (void)(Next[Taken] = PresetDraft);
			Modal = ModalKind::None;
			if (StorePresets(std::move(Next), {}))
			{
				PresetSelected = FindPreset(NameBuffer);
				SetStatus(false, "Saved preset %s", NameBuffer);
			}
		}
		ImGui::EndDisabled();
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			Modal = ModalKind::None;
		}
	}
	else if (Modal != ModalKind::None)
	{
		const bool IsImport = Modal == ModalKind::Import;
		bool ShouldOpen = false;
		if (ImGui::BeginListBox("##Scenes", ImVec2(420.0f, 12.0f * ImGui::GetTextLineHeightWithSpacing())))
		{
			if (FileList.empty())
			{
				ImGui::TextDisabled("No scenes on this level");
			}
			for (s32 Index = 0; Index < static_cast<s32>(FileList.size()); ++Index)
			{
				if (ImGui::Selectable(FileList[Index].c_str(), FileSelected == Index, ImGuiSelectableFlags_AllowDoubleClick))
				{
					FileSelected = Index;
					ShouldOpen = ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left);
				}
			}
			ImGui::EndListBox();
		}
		ImGui::BeginDisabled(FileSelected < 0);
		ShouldOpen = ImGui::Button(IsImport ? "Import" : "Open") || ShouldOpen;
		ImGui::EndDisabled();
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			Modal = ModalKind::None;
		}
		if (ShouldOpen && FileSelected >= 0)
		{
			const bool IsUnsaved = HasUnsavedFile && FileSelected + 1 == static_cast<s32>(FileList.size());
			string_path Path = {};
			if (IsUnsaved)
			{
				Io::UnsavedPath(Path, Core.Data.LevelName);
			}
			else if (!EditorUi::ScenePath(Path, IsImport ? Io::LegacyAlias : Io::SceneAlias, Core.Data.LevelName, Platform::UTF8_to_CP1251(FileList[FileSelected])))
			{
				Path[0] = 0;
			}
			OpenScene(Core, Path, FileList[FileSelected], !IsImport && !IsUnsaved);
		}
	}
	if (Modal == ModalKind::None)
	{
		ImGui::CloseCurrentPopup();
	}
	ImGui::EndPopup();
}

void Editor::RequestScene(Player& Core, PendingKind Kind, bool IsConfirmed)
{
	if (Core.HasUnsavedChanges && !IsConfirmed)
	{
		Pending = Kind;
		Modal = ModalKind::Discard;
		return;
	}
	Modal = ModalKind::None;
	if (Kind == PendingKind::New)
	{
		Core.Replace(Scene {});
		SetStatus(false, "New scene");
		return;
	}
	ShowModal(Core, Kind == PendingKind::Open ? ModalKind::Open : ModalKind::Import);
}

void Editor::DrawRenderModal(Player& Core)
{
	const bool HasRange = Core.HasLoopRange && Core.LoopEnd > Core.LoopStart;
	RenderRange = HasRange ? RenderRange : 1;
	ImGui::BeginDisabled(!HasRange);
	ImGui::RadioButton("Loop brace", &RenderRange, 0);
	ImGui::EndDisabled();
	ImGui::SameLine();
	ImGui::RadioButton("Whole scene", &RenderRange, 1);
	ImGui::SetNextItemWidth(160.0f);
	ImGui::Combo("Frame rate", &RenderRate, "Scene\0" "24\0" "25\0" "30\0" "60\0");
	ImGui::SetNextItemWidth(160.0f);
	ImGui::Combo("Aspect", &RenderAspect, "Window\0" "2.39:1\0" "21:9\0" "2:1\0" "1.85:1\0" "16:9\0" "4:3\0" "1:1\0" "9:16\0");
	ImGui::SetNextItemWidth(160.0f);
	ImGui::Combo("Output", &RenderOutput, "TGA frames\0" "AVI video (MJPEG)\0");
	ImGui::SetNextItemWidth(160.0f);
	ImGui::SliderFloat("Cut warm-up", &Settings.RenderWarmup, 0.0f, 30.0f, "%.0f frames", ImGuiSliderFlags_AlwaysClamp);
	ImGui::SetItemTooltip("Frames rendered and thrown away after every camera cut, so textures, exposure and anti-aliasing settle before the frame is written");
	ImGui::TextDisabled("Output goes to $app_data_root$\\cinema_renders\\%s\\", SceneName.empty() ? "untitled" : SceneName.c_str());
	if (ImGui::Button("Render"))
	{
		Modal = ModalKind::None;
		StartRender(Core);
	}
	ImGui::SameLine();
	if (ImGui::Button("Cancel"))
	{
		Modal = ModalKind::None;
	}
}

void Editor::DrawMotionModal(Player& Core)
{
	const Object* Target = Find(Core.Data, MotionObject);
	const ActorRuntime* Runtime = Core.FindActor(MotionObject);
	if (!Target || !Runtime)
	{
		Modal = ModalKind::None;
		return;
	}
	float First = FLT_MAX, Last = -FLT_MAX;
	for (const Lane& Item : Target->Lanes)
	{
		for (const Curve& Part : Item.Curves)
		{
			if (IsBoneParam(Item.Id) && Item.Layer / MaxPoseBones == MotionRow && !Part.Keys.empty())
			{
				First = std::min(First, Part.Keys.front().Time);
				Last = std::max(Last, Part.Keys.back().Time);
			}
		}
	}
	const bool HasKeys = Last > First, HasBrace = Core.HasLoopRange && Core.LoopEnd > Core.LoopStart;
	MotionRange = (MotionRange == 0 && !HasKeys) || (MotionRange == 1 && !HasBrace) ? 2 : MotionRange;
	if (ImGui::IsWindowAppearing())
	{
		ImGui::SetKeyboardFocusHere();
	}
	ImGui::SetNextItemWidth(360.0f);
	ImGui::InputTextWithHint("##Name", "Name", NameBuffer, 64);
	ImGui::BeginDisabled(!HasKeys);
	ImGui::RadioButton("Pose keys", &MotionRange, 0);
	ImGui::EndDisabled();
	ImGui::SameLine();
	ImGui::BeginDisabled(!HasBrace);
	ImGui::RadioButton("Loop brace", &MotionRange, 1);
	ImGui::EndDisabled();
	ImGui::SameLine();
	ImGui::RadioButton("Whole clip", &MotionRange, 2);
	ImGui::RadioButton("Pose only", &MotionSource, 0);
	ImGui::SameLine();
	ImGui::RadioButton("Final result", &MotionSource, 1);
	const float Start = Target->Span.Start;
	const float From = MotionRange == 0 ? Start + First : MotionRange == 1 ? Core.LoopStart : Start, To = MotionRange == 0 ? Start + Last : MotionRange == 1 ? Core.LoopEnd : Start + Target->Span.Length;
	const u32 Frames = static_cast<u32>(std::clamp((To - From) * Omf::MotionRate, 0.0f, 1000000.0f)) + 1;
	const size_t Length = xr_strlen(NameBuffer);
	const bool IsNamed = Length > 0 && std::all_of(NameBuffer, NameBuffer + Length, [](char Symbol) { return (Symbol >= 'a' && Symbol <= 'z') || (Symbol >= '0' && Symbol <= '9') || Symbol == '_'; });
	string_path File, Full;
	xr_sprintf(File, "cinema\\motions\\%s.omf", IsNamed ? NameBuffer : "");
	const char* Problem = !IsNamed ? "Use a-z, 0-9 and _ only." : FS.exist(Full, "$app_data_root$", File) ? "An animation with this name exists." : Frames < 2 ? "The range is empty." : Frames > Omf::MaxMotionFrames ? "The range is longer than 10 minutes." : Target->Actor.Packs.size() >= MaxPacks ? "The actor has no free pack slot." : "";
	ImGui::TextColored(ErrorColor, "%s", Length ? Problem : "");
	ImGui::TextDisabled("Saved to $app_data_root$\\cinema\\motions\\");
	ImGui::BeginDisabled(Problem[0] != 0);
	if (ImGui::Button("Save"))
	{
		xr_vector<shared_str> Bones;
		xr_vector<Fquaternion> Turns;
		xr_vector<Fvector> Shifts;
		for (u16 Bone = 0; Bone < Runtime->BoneCount(); ++Bone)
		{
			Bones.push_back(Runtime->BoneName(Bone));
		}
		IWriter* Writer = !Bones.empty() && Core.BakeMotion(MotionObject, From, Frames, MotionSource == 0, Turns, Shifts) ? FS.w_open("$app_data_root$", File) : nullptr;
		const bool IsWritten = Writer && Writer->valid();
		if (IsWritten)
		{
			CMemoryWriter Output;
			Omf::WriteMotion(NameBuffer, Bones, Turns, Shifts, Output);
			Writer->w(Output.pointer(), Output.size());
			Edit(Core, MotionObject, "Save as animation")->Actor.Packs.push_back(File);
		}
		if (Writer)
		{
			FS.w_close(Writer, true);
		}
		SetStatus(!IsWritten, IsWritten ? "Saved animation %s" : "Could not save animation %s", NameBuffer);
		Modal = ModalKind::None;
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	if (ImGui::Button("Cancel"))
	{
		Modal = ModalKind::None;
	}
}

void Editor::StartRender(Player& Core)
{
	constexpr double Rates[5] = { 0.0, 24.0, 25.0, 30.0, 60.0 };
	const bool IsBrace = RenderRange == 0 && Core.HasLoopRange && Core.LoopEnd > Core.LoopStart;
	const double Begin = IsBrace ? Core.LoopStart : 0.0, End = IsBrace ? Core.LoopEnd : ContentEnd(Core.Data);
	const double Rate = RenderRate > 0 && RenderRate < 5 ? Rates[RenderRate] : static_cast<double>(Core.Data.Header.FrameRate);
	string_path Sub;
	xr_sprintf(Sub, "cinema_renders\\%s\\", SceneName.empty() ? "untitled" : SceneName.c_str());
	Job = {};
	FS.update_path(Job.Folder, "$app_data_root$", Sub);
	Job.Begin = Begin;
	Job.Rate = Rate;
	Job.Count = static_cast<u32>(std::clamp(std::ceil((End - Begin) * Rate - 1e-3), 1.0, 999999.0));
	Job.Aspect = FrameAspects[std::clamp(RenderAspect, 0, 8)];
	Job.IsVideo = RenderOutput == 1;
	xr_sprintf(Job.Video, "%s%s.avi", Job.Folder, SceneName.empty() ? "untitled" : SceneName.c_str());
	if (Job.IsVideo && !Autotest::OpenVideo(Job.Video, Rate))
	{
		SetStatus(true, "Could not create %s", Job.Video);
		return;
	}
	Job.WasLooping = Core.IsLooping;
	Job.IsActive = true;
	Core.IsLooping = false;
	IsEditorCamera = IsFlying = IsFlyArmed = IsPanning = IsGizmoActive = IsPickArmed = false;
	CImGuiManager::Instance().SetMouseLook(false);
	Core.IsLookingThrough = true;
	Core.SetEditorView(false, false, Eye, Fidentity);
	Core.Play();
	Device.FixedDelta = static_cast<float>(1.0 / Rate);
}

void Editor::StepRender(Player& Core)
{
	const s32 Status = Autotest::FrameStatus();
	Job.Index += Job.HasRequest && Status == 0 ? 1 : 0;
	const char* Problem = Job.HasRequest && Status < 0 ? "A frame could not be written." : !g_pGameLevel ? "The level was unloaded." : Device.Paused() ? "The game is paused." : nullptr;
	Job.HasRequest = Job.HasRequest && Status == 1;
	ImGui::SetNextWindowPos(ImGui::GetMainViewport()->GetCenter(), ImGuiCond_Always, ImVec2(0.5f, 0.5f));
	ImGui::Begin("Rendering frames###CinemaRender", nullptr, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoSavedSettings);
	ImGui::Text(Job.Warm ? "Frame %u of %u (warming up)" : "Frame %u of %u", std::min(Job.Index + 1, Job.Count), Job.Count);
	ImGui::ProgressBar(static_cast<float>(Job.Index) / static_cast<float>(Job.Count), ImVec2(240.0f, 0.0f));
	const bool IsCancelled = ImGui::Button("Cancel") || ImGui::IsKeyPressed(ImGuiKey_Escape, false);
	ImGui::End();
	if (Problem || IsCancelled || Job.Index >= Job.Count)
	{
		StopRender(Core, Problem ? Problem : IsCancelled ? "Rendering cancelled." : nullptr);
		return;
	}
	if (!Job.HasRequest && Job.WarmIndex != Job.Index)
	{
		const float Shown = static_cast<float>(Job.Begin + (static_cast<double>(Job.Index) - 1.0) / Job.Rate);
		Job.WarmIndex = Job.Index;
		Job.Warm = Job.Index && Core.IsCutBetween(Shown - static_cast<float>(1.0 / Job.Rate), Shown) ? static_cast<u32>(Settings.RenderWarmup) : 0;
	}
	const bool IsWarming = !Job.HasRequest && Job.Warm > 0;
	if (IsWarming)
	{
		--Job.Warm;
	}
	else if (!Job.HasRequest)
	{
		string_path Path;
		xr_sprintf(Path, "%sframe_%06u.tga", Job.Folder, Job.Index);
		Autotest::RequestFrame(Path, Job.Aspect);
		Job.HasRequest = true;
	}
	Core.IsPlaying = true;
	Core.Time = static_cast<float>(Job.Begin + (static_cast<double>(Job.Index) - (IsWarming ? 2.0 : 1.0)) / Job.Rate);
}

void Editor::StopRender(Player& Core, const char* Problem)
{
	Device.FixedDelta = 0.0f;
	Core.Pause();
	Core.IsLooping = Job.WasLooping;
	Job.IsActive = false;
	const u32 Parts = Job.IsVideo ? Autotest::CloseVideo() : 0;
	SetStatus(Problem != nullptr, Parts > 1 ? "%s %u frames written to %s and %u more parts" : "%s %u frames written to %s", Problem ? Problem : "Done:", Job.Index, Job.IsVideo ? Job.Video : Job.Folder, Parts - 1);
}

void Editor::ShowModal(const Player& Core, ModalKind Kind)
{
	Io::ListScenes(Kind == ModalKind::Import ? Io::LegacyAlias : Io::SceneAlias, Core.Data.LevelName, FileList);
	string_path Folder, Unsaved;
	FS.update_path(Folder, Io::LegacyAlias, "cinema_editor\\");
	FS.rescan_path(Folder, false, false);
	Io::UnsavedPath(Unsaved, Core.Data.LevelName);
	HasUnsavedFile = Kind == ModalKind::Open && FS.exist(Unsaved);
	if (HasUnsavedFile)
	{
		FileList.push_back(xr_string("unsaved_") + Core.Data.LevelName.c_str() + " (recovered)");
	}
	FileSelected = -1;
	xr_sprintf(NameBuffer, "%s", SceneName.c_str());
	CheckName();
	Modal = Kind;
}

void Editor::CheckName()
{
	Io::CleanName(NameBuffer, NameFile, NameError);
	IsNameTaken = NameError.empty() && NameFile != SceneFile && std::find(FileList.begin(), FileList.end(), Platform::ANSI_TO_UTF8(NameFile)) != FileList.end();
}

void Editor::OpenScene(Player& Core, const char* Path, const xr_string& Name, bool IsNamed)
{
	Scene Incoming;
	Incoming.LevelName = Core.Data.LevelName;
	Io::LoadResult Outcome = Path[0] ? Io::Load(Path, Incoming) : Io::LoadResult { .Error = "The scene path is too long." };
	if (!Outcome.IsOk)
	{
		SetStatus(true, "%s", Outcome.Error.c_str());
		Message("Cannot open the scene", { Outcome.Error });
		return;
	}
	Core.Replace(std::move(Incoming));
	SeenGeneration = Core.Generation;
	const bool IsNative = Outcome.SourceVersion == FormatVersion;
	for (const Object& Item : Core.Data.Objects)
	{
		if (!IsNative && Item.Type == ObjectType::Actor && !Item.Span.IsLengthLocked)
		{
			Core.RequestFit(Item.Id);
		}
	}
	IsNamed = IsNamed && IsNative;
	ResetScene(IsNamed);
	SceneName = IsNamed ? Name : xr_string();
	SceneFile = IsNamed ? Platform::UTF8_to_CP1251(Name) : xr_string();
	SetStatus(false, "%s %s", IsNative ? "Opened" : "Imported", Name.c_str());
	Modal = ModalKind::None;
	xr_vector<xr_string>& Lines = Outcome.Notes.Lines;
	if (Outcome.Notes.Dropped)
	{
		string64 More;
		xr_sprintf(More, "%u more", Outcome.Notes.Dropped);
		Lines.emplace_back(More);
	}
	if (!Lines.empty())
	{
		Message("Warnings", std::move(Lines));
	}
}

void Editor::Save(Player& Core)
{
	SceneFile.empty() ? ShowModal(Core, ModalKind::SaveAs) : SaveScene(Core, SceneFile);
}

void Editor::SaveScene(const Player& Core, const xr_string& File)
{
	string_path Path;
	xr_string Error = "The scene path is too long.";
	if (!EditorUi::ScenePath(Path, Io::SceneAlias, Core.Data.LevelName, File) || !Io::Save(Core.Data, Path, Error))
	{
		SetStatus(true, "%s", Error.c_str());
		Message("Cannot save the scene", { Error });
		return;
	}
	SavedSerial = Undo.IsOpen() ? 0 : Undo.Serial();
	IsLayoutDirty = false;
	SceneFile = File;
	SceneName = Platform::ANSI_TO_UTF8(SceneFile);
	SetStatus(false, "Saved %s", SceneName.c_str());
	Modal = ModalKind::None;
}

void Editor::Message(const char* Title, xr_vector<xr_string> Lines)
{
	MessageTitle = Title;
	MessageLines = std::move(Lines);
	Modal = ModalKind::Message;
}

void Editor::SetStatus(bool IsError, const char* Format, ...)
{
	string512 Text;
	va_list Args;
	va_start(Args, Format);
	std::vsnprintf(Text, sizeof(Text), Format, Args);
	va_end(Args);
	StatusText = Text;
	IsStatusError = IsError;
	Msg(IsError ? "! Cinema editor: %s" : "Cinema editor: %s", Text);
}

Object* Editor::Edit(Player& Core, u32 Id, const char* Label)
{
	Undo.Begin(Label);
	Undo.Touch(Core.Data, Id);
	return Find(Core.Data, Id);
}

Object* Editor::CreateObject(Player& Core, ObjectType Type, float Start)
{
	Scene& Data = Core.Data;
	if (Data.Objects.size() >= MaxObjects)
	{
		SetStatus(true, "The scene already has %u objects.", MaxObjects);
		return nullptr;
	}
	const u32 Row = NextRow(Data, 0);
	Undo.Touch(Data, Data.NextId);
	Object& Item = Data.Objects.emplace_back();
	Item.Id = Data.NextId++;
	Item.Type = Type;
	Item.Color = NextColor(Data.Header);
	Item.Row = Row;
	Item.Span.Start = Sanitize(Start, 0.0f, MaxTime - Item.Span.Length, 0.0f);
	Selected.assign(1, Item.Id);
	Anchor = Item.Id;
	SelectedFolder = 0;
	Focus = FocusKind::Objects;
	return &Item;
}

Object* Editor::AddObject(Player& Core, ObjectType Type, const shared_str& Source, float Start, const Fvector& Position, const Fvector& Rotation)
{
	Undo.Begin(EditorUi::AddLabels[static_cast<u32>(Type)]);
	Object* Item = CreateObject(Core, Type, Start);
	if (Item)
	{
		(Type == ObjectType::Particle ? Item->Particle.Effect : Type == ObjectType::Decal ? Item->Decal.Texture : Item->Actor.Visual) = Source;
		*PlacementOf(*Item, 0) = Position;
		*PlacementOf(*Item, 1) = Rotation;
		Item->Name = DefaultName(*Item);
	}
	return Item;
}

void Editor::AddLight(Player& Core, float Start, const Fvector& Position)
{
	const float End = ContentEnd(Core.Data);
	float Heading = 0.0f, Tilt = 0.0f;
	Device.vCameraDirection.getHP(Heading, Tilt);
	Object* Item = AddObject(Core, ObjectType::Light, shared_str(), Start, Position, Fvector().set(rad2deg(Heading), rad2deg(Tilt), 0.0f));
	if (Item)
	{
		Item->Span.Length = std::min(End - Item->Span.Start >= 1.0f ? End - Item->Span.Start : 5.0f, MaxTime - Item->Span.Start);
	}
}

void Editor::AddWeather(Player& Core, float Start)
{
	Undo.Begin(EditorUi::AddLabels[static_cast<u32>(ObjectType::Weather)]);
	Object* Item = CreateObject(Core, ObjectType::Weather, Start);
	if (!Item || !g_pGamePersistent)
	{
		return;
	}
	CEnvironment& Env = g_pGamePersistent->Environment();
	Item->Weather.Cycle = Env.GetWeather();
	Item->Weather.TimeOfDay = std::clamp(Env.GetGameTime() / 3600.0f, 0.0f, 24.0f);
	Item->Weather.Mask = 1;
	Item->Span.Length = std::min(10.0f, MaxTime - Item->Span.Start);
	Item->Name = DefaultName(*Item);
}

void Editor::AddPost(Player& Core, float Start)
{
	Undo.Begin(EditorUi::AddLabels[static_cast<u32>(ObjectType::Post)]);
	Object* Item = CreateObject(Core, ObjectType::Post, Start);
	if (Item)
	{
		Item->Span.Length = std::min(5.0f, MaxTime - Item->Span.Start);
		Item->Name = DefaultName(*Item);
	}
}

void Editor::LoadPresets()
{
	if (IsPresetsLoaded)
	{
		return;
	}
	IsPresetsLoaded = true;
	string_path Folder, Paths[2];
	FS.update_path(Folder, Io::LegacyAlias, EditorUi::SettingsFolder);
	FS.rescan_path(Folder, false, false);
	FS.update_path(Paths[0], Io::LegacyAlias, Io::PresetFile);
	FS.update_path(Paths[1], Io::LegacyAlias, Io::LegacyPresetFile);
	xr_vector<ActorPreset> Lists[2];
	xr_vector<xr_string> Lines;
	bool IsRead[2] = { true, true };
	for (u32 Index = 0; Index < 2; ++Index)
	{
		Io::LoadResult Result = FS.exist(Paths[Index]) ? Io::LoadPresets(Paths[Index], Lists[Index], HiddenPresets) : Io::LoadResult { .IsOk = true };
		IsRead[Index] = Result.IsOk;
		if (!Result.IsOk || !Result.Notes.Lines.empty())
		{
			Lines.push_back(Result.IsOk ? xr_string(Paths[Index]) : Result.Error);
		}
		Lines.insert(Lines.end(), Result.Notes.Lines.begin(), Result.Notes.Lines.end());
		if (Result.Notes.Dropped)
		{
			string64 More;
			xr_sprintf(More, "%u more", Result.Notes.Dropped);
			Lines.emplace_back(More);
		}
	}
	IsPresetsReadOnly = !IsRead[0];
	if (IsPresetsReadOnly)
	{
		Lines.emplace_back("Presets are read-only until the game restarts, so the file is not overwritten.");
	}
	for (ActorPreset& Item : Lists[1])
	{
		Item.IsOld = true;
		OldPresetNames.push_back(Item.Name);
		const bool IsHidden = std::find(HiddenPresets.begin(), HiddenPresets.end(), Item.Name) != HiddenPresets.end();
		if (!IsHidden && std::none_of(Lists[0].begin(), Lists[0].end(), [&](const ActorPreset& Other) { return Other.Name == Item.Name; }))
		{
			Lists[0].push_back(std::move(Item));
		}
	}
	if (IsRead[1])
	{
		std::erase_if(HiddenPresets, [this](const xr_string& Name) { return std::find(OldPresetNames.begin(), OldPresetNames.end(), Name) == OldPresetNames.end(); });
	}
	std::sort(Lists[0].begin(), Lists[0].end(), EditorUi::IsPresetBefore);
	Presets = std::move(Lists[0]);
	for (const ActorPreset& Item : Presets)
	{
		PresetNames.emplace_back(Item.Name.c_str());
	}
	IsFilterDirty = true;
	if (!Lines.empty())
	{
		Message("Presets", std::move(Lines));
	}
}

bool Editor::StorePresets(xr_vector<ActorPreset>&& Next, const xr_string& Removed)
{
	xr_vector<xr_string> Hidden = HiddenPresets;
	const bool IsOldName = std::find(OldPresetNames.begin(), OldPresetNames.end(), Removed) != OldPresetNames.end();
	if (IsOldName && std::find(Hidden.begin(), Hidden.end(), Removed) == Hidden.end())
	{
		Hidden.push_back(Removed);
	}
	std::sort(Next.begin(), Next.end(), EditorUi::IsPresetBefore);
	string_path Path;
	xr_string Error = "The presets file could not be read, so presets are read-only until the game restarts.";
	FS.update_path(Path, Io::LegacyAlias, Io::PresetFile);
	if (IsPresetsReadOnly || !Io::SavePresets(Next, Hidden, Path, Error))
	{
		SetStatus(true, "%s", Error.c_str());
		Message("Cannot save the presets", { Error });
		return false;
	}
	Presets = std::move(Next);
	HiddenPresets = std::move(Hidden);
	PresetNames.clear();
	for (const ActorPreset& Item : Presets)
	{
		PresetNames.emplace_back(Item.Name.c_str());
	}
	IsFilterDirty = true;
	return true;
}

s32 Editor::FindPreset(const char* Name) const
{
	const auto Found = std::find_if(Presets.begin(), Presets.end(), [Name](const ActorPreset& Item) { return Item.Name == Name; });
	return Found == Presets.end() ? -1 : static_cast<s32>(Found - Presets.begin());
}

void Editor::ApplyPreset(Player& Core, const xr_vector<u32>& Ids, const char* Name)
{
	const s32 Found = FindPreset(Name);
	if (Found < 0 || !Omf::VisualExists(Presets[Found].Setup.Visual.c_str()))
	{
		SetStatus(true, "The preset model is missing.");
		return;
	}
	for (u32 Id : Ids)
	{
		Object* Item = Edit(Core, Id, "Apply preset");
		EditorUi::PutPreset(*Item, Presets[Found]);
		Refit(Core, *Item);
	}
}

void Editor::AddActorFromPreset(Player& Core, const char* Name, float Start, const Fvector& Position, const Fvector& Rotation)
{
	const s32 Found = FindPreset(Name);
	if (Found < 0 || !Omf::VisualExists(Presets[Found].Setup.Visual.c_str()))
	{
		SetStatus(true, "The preset model is missing.");
		return;
	}
	if (Object* Item = AddObject(Core, ObjectType::Actor, Presets[Found].Setup.Visual, Start, Position, Rotation))
	{
		EditorUi::PutPreset(*Item, Presets[Found]);
		Refit(Core, *Item);
	}
}

void Editor::SpawnPose(Fvector& Position, Fvector& Rotation)
{
	Fvector Normal;
	Position.mad(Device.vCameraPosition, Device.vCameraDirection, 3.0f);
	StaticRay(Position, Fvector().set(0.0f, -1.0f, 0.0f), 5.0f, Position, Normal);
	Rotation = FacingRotation(Position, nullptr);
}

bool Editor::StaticRay(const Fvector& Origin, const Fvector& Direction, float Range, Fvector& Point, Fvector& Normal)
{
	if (!g_pGameLevel)
	{
		return false;
	}
	collide::rq_result Hit;
	Hit.set(nullptr, Range, -1);
	collide::rq_results Results;
	const auto Accept = [](collide::rq_result& Result, void* Data)
	{
		const SGameMtl* Material = GMLib.GetMaterialByIdx(g_pGameLevel->ObjectSpace.GetStaticTris()[Result.element].material);
		if (Material && (Material->Flags.is(SGameMtl::flPassable) || Material->Flags.is(SGameMtl::flActorObstacle)))
		{
			return true;
		}
		*static_cast<collide::rq_result*>(Data) = Result;
		return false;
	};
	g_pGameLevel->ObjectSpace.RayQuery(Results, collide::ray_defs(Origin, Direction, Range, CDB::OPT_FULL_TEST, collide::rqtStatic), Accept, &Hit, nullptr, nullptr);
	if (Hit.element < 0)
	{
		return false;
	}
	const CDB::TRI& Triangle = g_pGameLevel->ObjectSpace.GetStaticTris()[Hit.element];
	const xr_vector<Fvector>& Vertices = g_pGameLevel->ObjectSpace.GetStaticVerts();
	Normal.mknormal(Vertices[Triangle.verts[0]], Vertices[Triangle.verts[1]], Vertices[Triangle.verts[2]]);
	Normal.mul(Normal.dotproduct(Direction) > 0.0f ? -1.0f : 1.0f);
	Point.mad(Origin, Direction, Hit.range);
	return true;
}

void Editor::Refit(Player& Core, const Object& Target)
{
	if (Target.Type == ObjectType::Actor && !Target.Span.IsLengthLocked)
	{
		Core.RequestFit(Target.Id);
	}
}

void Editor::RemoveBlock(Player& Core, u32 Id, s32 Block, u8 Layer)
{
	const Object* Target = Find(Core.Data, Id);
	const bool IsCamera = Target && Target->Type == ObjectType::Camera;
	if (Target && !IsCamera && Layer > Target->Actor.Layers.size())
	{
		return;
	}
	const s32 Count = !Target ? 0 : static_cast<s32>(IsCamera ? Target->Camera.Blocks.size() : BlocksOf(Target->Actor, Layer).size());
	if (IsCamera && Block >= ShakeBase && HasOwner(*Target, Block))
	{
		xr_vector<ShakeClip>& Shakes = Edit(Core, Id, "Remove shake")->Camera.Shakes;
		Shakes.erase(Shakes.begin() + (Block - ShakeBase));
		SelectedKeys.clear();
		FocusBlock = -1;
		Focus = FocusKind::Objects;
		return;
	}
	if (Block < 0 || Block >= Count)
	{
		return;
	}
	if (!IsCamera && Target->Actor.IsRelative)
	{
		SetStatus(true, "%s", FrozenClips);
		return;
	}
	Object& Item = *Edit(Core, Id, "Remove block");
	if (IsCamera)
	{
		PinShakes(Item);
		Item.Camera.Blocks.erase(Item.Camera.Blocks.begin() + Block);
		for (ShakePin& Pin : Pins)
		{
			Pin.Block = Pin.Block == Block ? -2 : Pin.Block - (Pin.Block > Block ? 1 : 0);
		}
		FollowShakes(Item);
		SelectedKeys.clear();
		TangentBlock = TangentKey = -1;
	}
	else
	{
		BlocksOf(Item.Actor, Layer).erase(BlocksOf(Item.Actor, Layer).begin() + Block);
	}
	Refit(Core, Item);
	FocusObject = Id;
	FocusLayer = Layer;
	FocusBlock = std::min(Block, Count - 2);
	Focus = FocusBlock < 0 ? FocusKind::Objects : FocusKind::Block;
}

void Editor::AddLayer(Player& Core, u32 Id)
{
	const Object* Target = Find(Core.Data, Id);
	if (!Target || Target->Type != ObjectType::Actor || Target->Actor.Layers.size() >= MaxActorLayers)
	{
		SetStatus(true, "An actor has at most %u animation rows.", MaxActorLayers + 1);
		return;
	}
	Edit(Core, Id, "Add row")->Actor.Layers.push_back(NewRow(Core.FindActor(Id)));
	const Object& Item = *Find(Core.Data, Id);
	FocusObject = Id;
	FocusLayer = static_cast<u8>(Item.Actor.Layers.size());
	FocusBlock = -1;
}

void Editor::RemoveLayer(Player& Core, u32 Id, u8 Layer)
{
	const Object* Target = Find(Core.Data, Id);
	if (!Target || Target->Type != ObjectType::Actor || Target->Actor.Layers.empty() || Layer > Target->Actor.Layers.size())
	{
		return;
	}
	Object& Item = *Edit(Core, Id, "Remove row");
	if (!Layer)
	{
		Item.Actor.Base = std::move(Item.Actor.Layers.front());
	}
	Item.Actor.Layers.erase(Item.Actor.Layers.begin() + (Layer ? Layer - 1 : 0));
	const auto IsWeight = [Id, Layer](u32 Owner, Param Kind, u8 At) { return Owner == Id && Kind == Param::LayerWeight && At == Layer; };
	std::erase_if(Item.Lanes, [&](const Lane& Track) { return IsWeight(Id, Track.Id, Track.Layer); });
	std::erase_if(Slots, [&](const LaneSlot& Slot) { return IsWeight(Slot.ObjectId, Slot.Id, Slot.Layer); });
	std::erase_if(Core.Data.Sizes, [&](const RowSize& Size) { return Size.ObjectId == Id && (Size.Kind == SizeSub ? Size.Layer == Layer : IsWeight(Id, Size.Id, Size.Layer)); });
	for (Lane& Track : Item.Lanes)
	{
		Track.Layer -= Track.Id == Param::LayerWeight && Track.Layer > Layer ? 1 : 0;
	}
	for (LaneSlot& Slot : Slots)
	{
		Slot.Layer -= Slot.ObjectId == Id && Slot.Id == Param::LayerWeight && Slot.Layer > Layer ? 1 : 0;
	}
	for (RowSize& Size : Core.Data.Sizes)
	{
		const bool IsShifted = Size.ObjectId == Id && (Size.Kind == SizeSub || Size.Id == Param::LayerWeight) && Size.Layer > Layer;
		Size.Layer -= IsShifted ? 1 : 0;
		IsLayoutDirty = IsLayoutDirty || IsShifted;
	}
	SelectedKeys.clear();
	if (FocusObject == Id && FocusLayer >= Layer)
	{
		FocusBlock = FocusLayer == Layer ? -1 : FocusBlock;
		FocusLayer = static_cast<u8>(FocusLayer == Layer ? 0 : FocusLayer - 1);
	}
	Refit(Core, Item);
}

void Editor::SetRowMode(Player& Core, u32 Id, u8 Row, bool IsAdd)
{
	const Object* Target = Find(Core.Data, Id);
	if (!Target || Row > Target->Actor.Layers.size())
	{
		return;
	}
	if (IsAdd && AddRows(Target->Actor) >= MaxAddRows)
	{
		SetStatus(true, "At most %u animation rows can add.", MaxAddRows);
		return;
	}
	RowOf(Edit(Core, Id, IsAdd ? "Add row mode" : "Override row mode")->Actor, Row).Mode = IsAdd ? RowMode::Add : RowMode::Override;
}

void Editor::AddAnimationAt(Player& Core, u32 Id, u8 Row, float Time, const shared_str& Source, const shared_str& Name)
{
	const Object* Target = Find(Core.Data, Id);
	const ActorRuntime* Runtime = Core.FindActor(Id);
	if (!Target || Target->Type != ObjectType::Actor)
	{
		return;
	}
	if (Target->Actor.IsRelative || !Runtime)
	{
		SetStatus(true, "%s", FrozenClips);
		return;
	}
	Object& Item = *Edit(Core, Id, "Add animation");
	ActorBlock Added;
	Added.Source = Source;
	Added.Name = Name;
	Added.Start = std::clamp(Time - Item.Span.Start, 0.0f, MaxTime);
	const u8 Into = PlaceRow(Item, *Runtime, static_cast<u8>(std::min<size_t>(Row, Item.Actor.Layers.size())), Added, true, true);
	if (Into > MaxActorLayers)
	{
		SetStatus(true, "Every animation row is taken there.");
		return;
	}
	const xr_vector<ActorBlock>& Placed = BlocksOf(Item.Actor, Into);
	FocusObject = Id;
	FocusLayer = Into;
	FocusBlock = static_cast<s32>(std::find(Placed.begin(), Placed.end(), Added) - Placed.begin());
	Focus = FocusKind::Block;
}

void Editor::AddFolder(Player& Core)
{
	Scene& Data = Core.Data;
	if (Data.Header.Folders.size() >= MaxFolders)
	{
		SetStatus(true, "The scene already has %u folders.", MaxFolders);
		return;
	}
	Undo.Begin("Add folder");
	Undo.TouchHeader(Data);
	const u32 Row = NextRow(Data, 0);
	Folder& Item = Data.Header.Folders.emplace_back();
	Item.Id = Data.NextId++;
	Item.Row = Row;
	Item.Name = "Folder";
	Selected.clear();
	SelectedFolder = Item.Id;
	StartRename(Data, Item.Id, true);
}

void Editor::AddCameraMove(Player& Core, float Time, const PoseKey* Pose)
{
	xr_vector<CameraBlock> Shots(1);
	PoseKey First;
	ViewPose(Core, false, First);
	First = Pose ? *Pose : First;
	Cameras::SetPoseAt(Shots[0], 0.0f, First.Position, First.Rotation, First.Mode, 1e-4f);
	Shots[0].Fov = Core.View() == ViewSource::Editor ? EyeFov : 0.0f;
	Undo.Begin("Add camera move");
	InsertShots(Core, Sanitize(Time, 0.0f, MaxTime, 0.0f), Shots, true);
}

void Editor::AddShake(Player& Core, float Time, s32 Row)
{
	const Object* Rig = FindCamera(Core.Data);
	const float Local = Rig ? std::clamp(Time - Rig->Span.Start, 0.0f, MaxTime - ShakeClip {}.Length) : 0.0f;
	const u8 First = Row >= 0 ? static_cast<u8>(Row) : 0, Last = Row >= 0 ? static_cast<u8>(Row + 1) : static_cast<u8>(MaxShakeRows);
	u8 Free = First;
	while (Rig && Free < Last && !Cameras::IsShakeFree(Rig->Camera, u32(-1), Free, Local, ShakeClip {}.Length, MaxShakeClips, true))
	{
		++Free;
	}
	const char* Problem = !Rig ? "Add a camera move first." : Rig->Camera.Shakes.size() >= MaxShakeClips ? "The camera already has 256 shakes." : Free == Last ? "No shake row is free there." : nullptr;
	if (Problem)
	{
		SetStatus(true, "%s", Problem);
		return;
	}
	xr_vector<ShakeClip>& Shakes = Edit(Core, Rig->Id, "Add shake")->Camera.Shakes;
	ShakeClip& Clip = Shakes.emplace_back();
	Clip.Start = Local;
	Clip.Row = Free;
	FocusObject = Rig->Id;
	FocusBlock = ShakeBase + static_cast<s32>(Shakes.size() - 1);
	Focus = FocusKind::Block;
}

void Editor::PinShakes(const Object& Rig)
{
	Pins.clear();
	for (const ShakeClip& Clip : Rig.Camera.Shakes)
	{
		const s32 Block = Cameras::ShakeOwner(Rig.Camera, Clip);
		Pins.push_back({ Block, Clip.Start - (Block < 0 ? 0.0f : Cameras::BlockStart(Rig.Camera, static_cast<u32>(Block))), Clip.Length, Clip.FadeIn, Clip.FadeOut, Clip.Row });
	}
}

void Editor::FollowShakes(Object& Rig, s32 Stretched, float Factor)
{
	xr_vector<ShakeClip>& Shakes = Rig.Camera.Shakes;
	ShakeMoved.assign(Shakes.size(), 0);
	for (u32 Index = static_cast<u32>(std::min(Pins.size(), Shakes.size())); Index-- > 0;)
	{
		const ShakePin& Pin = Pins[Index];
		ShakeClip& Clip = Shakes[Index];
		if (Pin.Block < -1)
		{
			Shakes.erase(Shakes.begin() + Index);
			ShakeMoved.erase(ShakeMoved.begin() + Index);
			continue;
		}
		const float Scale = Stretched >= 0 && Pin.Block == Stretched ? Factor : 1.0f, Was = Clip.Start;
		Clip.Start = std::clamp((Pin.Block < 0 ? 0.0f : Cameras::BlockStart(Rig.Camera, static_cast<u32>(Pin.Block))) + Pin.Offset * Scale, 0.0f, MaxTime);
		Clip.Length = std::clamp(Pin.Length * Scale, MinLength, MaxTime);
		Clip.FadeIn = std::min(Pin.FadeIn * Scale, MaxTime);
		Clip.FadeOut = std::min(Pin.FadeOut * Scale, MaxTime);
		ShakeMoved[Index] = Clip.Start != Was || Clip.Row != Pin.Row || Scale != 1.0f;
		SetShakeRow(Clip, Pin.Row);
	}
	if (Cameras::SettleShakes(Rig.Camera, ShakeMoved) && Drag == DragKind::None && !ImGui::IsAnyItemActive())
	{
		SetStatus(true, "Every shake row is taken there; a shake overlaps another.");
	}
}

void Editor::Crossfade(Player& Core, s32 Block, bool IsOn)
{
	const Object* Rig = FindCamera(Core.Data);
	if (!Rig || Block <= 0 || Block >= static_cast<s32>(Rig->Camera.Blocks.size()))
	{
		return;
	}
	const CameraBlock& Previous = Rig->Camera.Blocks[Block - 1];
	const CameraBlock& Shot = Rig->Camera.Blocks[Block];
	const float Overlap = Previous.Start + Previous.Length - Shot.Start;
	const float Length = Shot.Start - Previous.Start + (IsOn ? std::min(LastBlend, Shot.Length - MinLength) : 0.0f);
	if (!(Length >= MinLength) || !IsClipFree(Rig->Camera, Block - 1, Previous.Start, Length))
	{
		SetStatus(true, "There is no room for a crossfade there.");
		return;
	}
	LastBlend = !IsOn && Overlap > 0.0f ? Overlap : LastBlend;
	Edit(Core, Rig->Id, IsOn ? "Crossfade" : "Remove crossfade")->Camera.Blocks[Block - 1].Length = Length;
}

bool Editor::IsClipFree(const CameraData& Data, s32 Skip, float Start, float Length)
{
	SpanScratch.clear();
	bool IsPlaced = false;
	for (u32 Index = 0; Index < Data.Blocks.size(); ++Index)
	{
		const CameraBlock& Item = Data.Blocks[Index];
		if (!IsPlaced && Start < Item.Start)
		{
			SpanScratch.push_back({ Start, Length });
			IsPlaced = true;
		}
		if (static_cast<s32>(Index) != Skip)
		{
			SpanScratch.push_back({ Item.Start, Item.Length });
		}
	}
	if (!IsPlaced)
	{
		SpanScratch.push_back({ Start, Length });
	}
	return Cameras::IsTrackValid(SpanScratch);
}

float Editor::FitClip(const CameraData& Data, s32 Skip, float Start, float End, u8 Edge)
{
	const float Length = End - Start, Wanted = Edge == 2 ? End : Start;
	float Best = -1.0f;
	const auto Try = [&](float Value)
	{
		const float From = Edge == 2 ? Start : Edge == 1 ? Value : std::clamp(Value, 0.0f, std::max(MaxTime - Length, 0.0f));
		const float To = Edge == 2 ? Value : Edge == 1 ? End : From + Length;
		const float Picked = Edge == 2 ? To : From;
		if ((Best < 0.0f || std::fabs(Picked - Wanted) < std::fabs(Best - Wanted)) && IsClipFree(Data, Skip, From, To - From))
		{
			Best = Picked;
		}
	};
	Try(Wanted);
	for (u32 Index = 0; Index < Data.Blocks.size(); ++Index)
	{
		const CameraBlock& Item = Data.Blocks[Index];
		if (static_cast<s32>(Index) != Skip)
		{
			Try(Edge == 0 ? Item.Start - Length : Item.Start);
			Try(Item.Start + Item.Length);
		}
	}
	return Best;
}

void Editor::TrimHead(CameraBlock& Shot, const CameraBlock& Base, float Delta) const
{
	Shot.Start = Base.Start + Delta;
	Shot.Length = Base.Length - Delta;
	const float PathDelta = Cameras::PathTime(Base, Delta);
	for (Lane& Track : Shot.Lanes)
	{
		const float Cut = Track.Id <= Param::Rotation ? PathDelta : Delta;
		for (Curve& Part : Track.Curves)
		{
			const auto Kept = std::lower_bound(Part.Keys.begin(), Part.Keys.end(), Cut, [](const Key& Item, float Moment) { return Item.Time < Moment; });
			if (Cut > 0.0f && Kept != Part.Keys.begin())
			{
				const float Value = Curves::Value(Part, Cut, 0.0f);
				const Interp Mode = (Kept - 1)->Mode;
				*(Kept - 1) = Key { Cut, Value, Mode };
				Part.Keys.erase(Part.Keys.begin(), Kept - 1);
			}
			for (Key& Item : Part.Keys)
			{
				Item.Time -= Cut;
			}
			Part.Gen.Phase += Delta * Part.Gen.Frequency;
		}
	}
	Cameras::Rebuild(Shot);
}

s32 Editor::SortShot(Object& Rig, s32 Block)
{
	xr_vector<CameraBlock>& Blocks = Rig.Camera.Blocks;
	const auto Swap = [&](s32 At)
	{
		std::swap(Blocks[At], Blocks[At + 1]);
		const auto Remap = [At](auto& Block) { Block = static_cast<std::decay_t<decltype(Block)>>(Block == At ? At + 1 : Block == At + 1 ? At : Block); };
		for (ShakePin& Pin : Pins)
		{
			Remap(Pin.Block);
		}
		for (KeyRef& Ref : SelectedKeys)
		{
			if (Ref.ObjectId == Rig.Id)
			{
				Remap(Ref.Block);
			}
		}
		Remap(PickFocusBlock);
		TangentBlock = TangentKey = -1;
	};
	while (Block > 0 && Blocks[Block - 1].Start > Blocks[Block].Start)
	{
		Swap(--Block);
	}
	while (Block + 1 < static_cast<s32>(Blocks.size()) && Blocks[Block + 1].Start < Blocks[Block].Start)
	{
		Swap(Block++);
	}
	return Block;
}

bool Editor::InsertShots(Player& Core, float Time, xr_vector<CameraBlock>& Shots, bool ShouldFit)
{
	Object* Rig = FindCamera(Core.Data);
	if (!Rig || Shots.empty())
	{
		SetStatus(true, "The scene has no camera track.");
		return false;
	}
	xr_vector<CameraBlock>& Blocks = Rig->Camera.Blocks;
	Time = std::clamp(Time, 0.0f, MaxTime - MinLength);
	if (ShouldFit)
	{
		for (u32 Step = 0, Inside = 0; Step < Blocks.size() && (Inside = static_cast<u32>(Cameras::BlockAt(Rig->Camera, Time))) < Blocks.size(); ++Step)
		{
			Time = std::max(Time, Blocks[Inside].Start + Blocks[Inside].Length);
		}
		const auto Next = std::lower_bound(Blocks.begin(), Blocks.end(), Time, [](const CameraBlock& Item, float Moment) { return Item.Start < Moment; });
		Shots[0].Length = std::min(Shots[0].Length, (Next == Blocks.end() ? MaxTime : Next->Start) - Time);
	}
	SpanScratch.clear();
	for (const CameraBlock& Item : Blocks)
	{
		SpanScratch.push_back({ Item.Start, Item.Length });
	}
	for (const CameraBlock& Shot : Shots)
	{
		SpanScratch.push_back({ Time + Shot.Start, Shot.Length });
	}
	std::sort(SpanScratch.begin(), SpanScratch.end(), [](const Cameras::ClipSpan& A, const Cameras::ClipSpan& B) { return A.Start < B.Start; });
	const bool IsShort = ShouldFit && Shots[0].Length < 1.0f / static_cast<float>(Core.Data.Header.FrameRate);
	const char* Problem = IsShort ? "There is no room for a camera clip there." : Blocks.size() + Shots.size() > MaxBlocks ? "The camera would have more than 256 clips." : !Cameras::IsTrackValid(SpanScratch) ? "Camera clips there would lie inside another, overlap two others or pass 60:00:00." : nullptr;
	if (Problem)
	{
		SetStatus(true, "%s", Problem);
		return false;
	}
	Undo.Touch(Core.Data, Rig->Id);
	PinShakes(*Rig);
	InsertedShots.clear();
	for (CameraBlock& Shot : Shots)
	{
		Shot.Start += Time;
		const s32 At = static_cast<s32>(std::upper_bound(Blocks.begin(), Blocks.end(), Shot.Start, [](float Moment, const CameraBlock& Item) { return Moment < Item.Start; }) - Blocks.begin());
		Blocks.insert(Blocks.begin() + At, Shot);
		for (s32& Index : InsertedShots)
		{
			Index += Index >= At ? 1 : 0;
		}
		for (ShakePin& Pin : Pins)
		{
			Pin.Block += Pin.Block >= At ? 1 : 0;
		}
		InsertedShots.push_back(At);
	}
	FollowShakes(*Rig);
	Selected.assign(1, Rig->Id);
	Anchor = FocusObject = Rig->Id;
	SelectedFolder = 0;
	FocusBlock = InsertedShots[0];
	Focus = FocusKind::Block;
	SelectedKeys.clear();
	TangentBlock = TangentKey = -1;
	return true;
}

void Editor::AddMarker(Player& Core, float Time)
{
	SceneHeader& Header = Core.Data.Header;
	Time = std::clamp(Time, 0.0f, MaxTime);
	if (Header.Markers.size() >= MaxMarkers || !IsMarkerFree(Header, Time))
	{
		SetStatus(true, Header.Markers.size() >= MaxMarkers ? "The scene already has 256 markers." : "Another marker is on that frame.");
		return;
	}
	Undo.Begin("Add marker");
	Undo.TouchHeader(Core.Data);
	string32 Name;
	xr_sprintf(Name, "Marker %u", static_cast<u32>(Header.Markers.size() + 1));
	const auto At = std::upper_bound(Header.Markers.begin(), Header.Markers.end(), Time, [](float Moment, const Marker& Item) { return Moment < Item.Time; });
	Header.Markers.insert(At, Marker { Time, Name });
}

void Editor::RemoveMarker(Player& Core, s32 Index)
{
	xr_vector<Marker>& Markers = Core.Data.Header.Markers;
	if (Index >= 0 && Index < static_cast<s32>(Markers.size()))
	{
		Undo.Begin("Remove marker");
		Undo.TouchHeader(Core.Data);
		Markers.erase(Markers.begin() + Index);
	}
}

bool Editor::Copy(Player& Core, Clipboard& Out)
{
	const Scene& Data = Core.Data;
	const Object* Focused = Find(Data, FocusObject);
	const bool IsCamera = Focused && Focused->Type == ObjectType::Camera;
	const u8 Layer = IsCamera ? 0 : FocusLayer;
	const bool HasBlock = Focus == FocusKind::Block && Focused && FocusBlock >= 0 && (IsCamera || Layer <= Focused->Actor.Layers.size()) && FocusBlock < static_cast<s32>(IsCamera ? Focused->Camera.Blocks.size() : BlocksOf(Focused->Actor, Layer).size());
	const Object* Rig = FindCamera(Data);
	const bool HasRig = Rig && IsSelected(Rig->Id) && !Rig->Camera.Blocks.empty();
	const bool HasObjects = std::any_of(Selected.begin(), Selected.end(), [&Data](u32 Id) { const Object* Item = Find(Data, Id); return Item && Item->Type != ObjectType::Camera; });
	const bool IsShake = Focus == FocusKind::Block && IsCamera && FocusBlock >= ShakeBase && HasOwner(*Focused, FocusBlock);
	const ClipKind Kind = Focus == FocusKind::Keys && !SelectedKeys.empty() ? ClipKind::Keys : IsShake ? ClipKind::Shakes : HasBlock ? (IsCamera ? ClipKind::CameraBlocks : ClipKind::ActorBlocks) : HasObjects ? ClipKind::Objects : HasRig ? ClipKind::CameraBlocks : ClipKind::None;
	if (Kind == ClipKind::None)
	{
		SetStatus(true, "Select objects to copy.");
		return false;
	}
	Out = Clipboard {};
	Out.Kind = Kind;
	Out.Generation = Core.Generation;
	Out.Origin = MaxTime;
	if (Kind == ClipKind::Keys)
	{
		const Object* First = Find(Data, SelectedKeys[0].ObjectId);
		Out.Owner = First ? First->Type : ObjectType::Actor;
		for (const KeyRef& Ref : SelectedKeys)
		{
			const Object* Owner = Find(Data, Ref.ObjectId);
			const Lane* Found = Owner && Owner->Type == Out.Owner && HasOwner(*Owner, Ref.Block) ? Curves::FindLane(LaneList(*Owner, Ref.Block), Ref.Id, Ref.Layer) : nullptr;
			if (Found && Ref.Index < Found->Curves[Ref.Component].Keys.size())
			{
				Key& Item = Out.Keys.emplace_back(ClipKey { Ref.ObjectId, Ref.Id, Ref.Layer, Ref.Component, Found->Curves[Ref.Component].Keys[Ref.Index] }).Item;
				Item.Time += TargetStart(*Owner, Ref.Block);
				Out.Origin = std::min(Out.Origin, Item.Time);
				Out.Extent = std::max(Out.Extent, Item.Time);
			}
		}
		for (ClipKey& Entry : Out.Keys)
		{
			Entry.Item.Time -= Out.Origin;
		}
	}
	else if (Kind == ClipKind::Shakes)
	{
		Out.Shakes.push_back(Focused->Camera.Shakes[FocusBlock - ShakeBase]);
		Out.Origin = Focused->Span.Start + Out.Shakes[0].Start;
		Out.Extent = Out.Origin + Out.Shakes[0].Length;
		Out.Shakes[0].Start = 0.0f;
	}
	else if (Kind == ClipKind::ActorBlocks)
	{
		const ActorBlock& Block = BlocksOf(Focused->Actor, Layer)[FocusBlock];
		Out.Blocks.push_back(Block);
		if (std::find(Focused->Actor.Packs.begin(), Focused->Actor.Packs.end(), Block.Source) != Focused->Actor.Packs.end())
		{
			Out.Packs.push_back(Block.Source);
		}
	}
	else if (Kind == ClipKind::CameraBlocks)
	{
		const Object& Owner = HasBlock ? *Focused : *Rig;
		const s32 First = HasBlock ? FocusBlock : 0, Last = HasBlock ? FocusBlock : static_cast<s32>(Owner.Camera.Blocks.size()) - 1;
		Out.Shots.assign(Owner.Camera.Blocks.begin() + First, Owner.Camera.Blocks.begin() + Last + 1);
		Out.Origin = TargetStart(Owner, First);
		Out.Extent = 0.0f;
		for (CameraBlock& Shot : Out.Shots)
		{
			Out.Extent = std::max(Out.Extent, Owner.Span.Start + Shot.Start + Shot.Length);
			Shot.Start = std::max(Owner.Span.Start + Shot.Start - Out.Origin, 0.0f);
		}
		for (const ShakeClip& Clip : Owner.Camera.Shakes)
		{
			const s32 Anchor = Cameras::ShakeOwner(Owner.Camera, Clip);
			if (Anchor >= First && Anchor <= Last)
			{
				Out.Shakes.push_back(Clip);
				Out.Shakes.back().Start = std::max(Owner.Span.Start + Clip.Start - Out.Origin, 0.0f);
			}
		}
	}
	else
	{
		for (const Object& Item : Data.Objects)
		{
			if (Item.Type != ObjectType::Camera && IsSelected(Item.Id))
			{
				Out.Objects.push_back(Item);
				Out.Origin = std::min(Out.Origin, Item.Span.Start);
				Out.Extent = std::max(Out.Extent, ClipEnd(Item));
			}
		}
		if (HasRig)
		{
			SetStatus(false, "The camera is copied by its blocks.");
		}
	}
	Out.Extent = std::max(Out.Extent - Out.Origin, 0.0f);
	return true;
}

void Editor::Paste(Player& Core, const Clipboard& Source, float Time)
{
	Undo.Begin("Paste");
	if (Source.Kind == ClipKind::Objects)
	{
		PasteObjects(Core, Source, Time);
		return;
	}
	if (Source.Kind == ClipKind::ActorBlocks)
	{
		PasteBlocks(Core, Source, 0);
		return;
	}
	if (Source.Kind == ClipKind::Keys)
	{
		PasteKeys(Core, Source, Time, false);
		return;
	}
	Object* Owner = FindCamera(Core.Data);
	if (Source.Kind == ClipKind::Shakes && (!Owner || Owner->Camera.Shakes.size() >= MaxShakeClips))
	{
		SetStatus(true, "%s", Owner ? "The camera already has 256 shakes." : "Add a camera move first.");
		return;
	}
	if (Source.Kind == ClipKind::Shakes)
	{
		Undo.Touch(Core.Data, Owner->Id);
		Owner->Camera.Shakes.push_back(Source.Shakes[0]);
		Owner->Camera.Shakes.back().Start = std::clamp(Time - Owner->Span.Start, 0.0f, MaxTime);
		FocusObject = Owner->Id;
		FocusBlock = ShakeBase + static_cast<s32>(Owner->Camera.Shakes.size() - 1);
		Focus = FocusKind::Block;
		ShakeMoved.assign(Owner->Camera.Shakes.size(), 0);
		ShakeMoved.back() = 1;
		if (Cameras::SettleShakes(Owner->Camera, ShakeMoved, true))

		{
			SetStatus(true, "Every shake row is taken there; a shake overlaps another.");
		}
		return;
	}
	xr_vector<CameraBlock> Shots = Source.Shots;
	for (CameraBlock& Shot : Shots)
	{
		Shot.Attach.Parent = Shot.Attach.LookAt = AttachLink {};
	}
	if (Source.Kind != ClipKind::CameraBlocks || !InsertShots(Core, Time, Shots, false))
	{
		return;
	}
	IdMap.clear();
	Object& Rig = *FindCamera(Core.Data);
	for (u32 Index = 0; Index < Shots.size(); ++Index)
	{
		Relink(Core.Data, Rig.Id, Source.Shots[Index].Attach, Rig.Camera.Blocks[InsertedShots[Index]].Attach, Source.Generation == Core.Generation);
		Rig.Camera.Blocks[InsertedShots[Index]].FocusTarget = Source.Generation == Core.Generation ? Source.Shots[Index].FocusTarget : AttachLink {};
	}
	const float Base = Shots[0].Start - Source.Shots[0].Start;
	for (u32 Index = 0; Index < Source.Shakes.size() && Rig.Camera.Shakes.size() < MaxShakeClips; ++Index)
	{
		Rig.Camera.Shakes.push_back(Source.Shakes[Index]);
		Rig.Camera.Shakes.back().Start = std::min(Rig.Camera.Shakes.back().Start + Base, MaxTime);
	}
	if (Cameras::SettleShakes(Rig.Camera))
	{
		SetStatus(true, "Every shake row is taken there; a shake overlaps another.");
	}
}

void Editor::PasteObjects(Player& Core, const Clipboard& Source, float Time)
{
	Scene& Data = Core.Data;
	IdMap.clear();
	Selected.clear();
	for (const Object& Item : Source.Objects)
	{
		if (Data.Objects.size() >= MaxObjects)
		{
			SetStatus(true, "%u objects were not pasted.", static_cast<u32>(Source.Objects.size() - IdMap.size()));
			break;
		}
		Undo.Touch(Data, Data.NextId);
		Object& Added = Data.Objects.emplace_back(Item);
		Added.Id = Data.NextId++;
		Added.Color = NextColor(Data.Header);
		Added.Span.Start = Sanitize(Time + Item.Span.Start - Source.Origin, 0.0f, MaxTime - Item.Span.Length, 0.0f);
		Added.FolderId = Source.Generation == Core.Generation && FindFolder(Data, Item.FolderId) ? Item.FolderId : 0;
		Added.Attach.Parent = Added.Attach.LookAt = AttachLink {};
		IdMap.emplace_back(Item.Id, Added.Id);
		Selected.push_back(Added.Id);
	}
	const size_t First = Data.Objects.size() - IdMap.size();
	for (u32 Index = 0; Index < IdMap.size(); ++Index)
	{
		Relink(Data, IdMap[Index].second, Source.Objects[Index].Attach, Data.Objects[First + Index].Attach, Source.Generation == Core.Generation);
	}
	Anchor = Selected.empty() ? Anchor : Selected[0];
	SelectedFolder = 0;
	Focus = FocusKind::Objects;
}

void Editor::PasteBlocks(Player& Core, const Clipboard& Source, u32 OnlyId)
{
	u32 Targets = 0;
	for (Object& Item : Core.Data.Objects)
	{
		ActorData& Setup = Item.Actor;
		if (Item.Type != ObjectType::Actor || (OnlyId ? Item.Id != OnlyId : !IsSelected(Item.Id) && (!Selected.empty() || Item.Id != FocusObject)))
		{
			continue;
		}
		u8 Layer = 0;
		const s32 Focused = FocusedBlock(Core, Item, &Layer);
		xr_vector<ActorBlock>& Blocks = BlocksOf(Setup, Layer);
		const s32 After = std::clamp(Focused, -1, static_cast<s32>(Blocks.size()) - 1);
		const ActorRuntime* Runtime = Core.FindActor(Item.Id);
		if (Setup.IsRelative || !Runtime || !Runtime->IsComplete() || Source.Blocks.empty())
		{
			SetStatus(true, "%s", FrozenClips);
			continue;
		}
		const float Anchor = After < 0 ? std::max(Core.Time - Item.Span.Start, 0.0f) : TimeAt(Item, *Runtime, AreaAt(Item, *Runtime, Item.Span.Start + Blocks[After].Start) + Runtime->BlockDuration(Blocks[After])) - Item.Span.Start;
		Undo.Touch(Core.Data, Item.Id);
		u8 Into = Layer;
		s32 Pasted = -1;
		for (const ActorBlock& Copy : Source.Blocks)
		{
			ActorBlock Placed = Copy;
			Placed.Start = std::clamp(Anchor + Copy.Start - Source.Blocks[0].Start, 0.0f, MaxTime);
			const u8 Row = PlaceRow(Item, *Runtime, Layer, Placed, true, true);
			if (Row > MaxActorLayers)
			{
				SetStatus(true, "Every animation row is taken there.");
				continue;
			}
			Into = Pasted < 0 ? Row : Into;
			Pasted = Pasted < 0 ? static_cast<s32>(std::find(BlocksOf(Setup, Row).begin(), BlocksOf(Setup, Row).end(), Placed) - BlocksOf(Setup, Row).begin()) : Pasted;
		}
		for (const shared_str& Pack : Source.Packs)
		{
			if (Setup.Packs.size() < MaxPacks && std::find(Setup.Packs.begin(), Setup.Packs.end(), Pack) == Setup.Packs.end())
			{
				Setup.Packs.push_back(Pack);
			}
		}
		Refit(Core, Item);
		FocusObject = Targets ? FocusObject : Item.Id;
		FocusLayer = Targets ? FocusLayer : Into;
		FocusBlock = Targets++ ? FocusBlock : Pasted;
		Focus = FocusKind::Block;
	}
	if (!Targets)
	{
		SetStatus(true, "Select the actors to paste the block into.");
	}
}

void Editor::PasteKeys(Player& Core, const Clipboard& Source, float Time, bool IsSameOwners)
{
	Scene& Data = Core.Data;
	const float HalfFrame = 0.5f / static_cast<float>(Data.Header.FrameRate);
	u32 Skipped = 0;
	bool HasTarget = false;
	const auto Walk = [&](bool IsWriting)
	{
		for (Object& Target : Data.Objects)
		{
			if (Target.Type != Source.Owner || (!IsSameOwners && !IsSelected(Target.Id)))
			{
				continue;
			}
			for (const ClipKey& Entry : Source.Keys)
			{
				if (IsSameOwners && Entry.ObjectId != Target.Id)
				{
					continue;
				}
				HasTarget = true;
				const float At = Time + Entry.Item.Time - Target.Span.Start;
				const s32 Block = Target.Type == ObjectType::Camera ? CameraOwnerAt(Target, Entry.Id, Entry.Layer, At) : -1;
				const float Local = At - (Block < 0 ? 0.0f : TargetStart(Target, Block) - Target.Span.Start);
				const bool IsInside = Target.Type == ObjectType::Camera ? Block >= 0 && StaticField(Target, Block, Entry.Id, Entry.Layer, 0) : At >= 0.0f && At <= Target.Span.Length && StaticField(Target, -1, Entry.Id, Entry.Layer, 0);
				const Lane* Found = IsInside ? Curves::FindLane(LaneList(Target, Block), Entry.Id, Entry.Layer) : nullptr;
				if (!IsWriting)
				{
					const u32 Index = Found ? Curves::KeyAt(Found->Curves[Entry.Component], Local, 0.0f) : u32(-1);
					if (Index != u32(-1))
					{
						SelectedKeys.push_back({ Target.Id, static_cast<s16>(Block), Entry.Id, Entry.Layer, Entry.Component, Index });
					}
					continue;
				}
				if (!IsInside || (!Found && LaneList(Target, Block).size() >= MaxLanes))
				{
					++Skipped;
					continue;
				}
				Undo.Touch(Data, Target.Id);
				UseGameFov(Target, Block, Entry.Id);
				Curve& Part = Curves::EnsureLane(LaneList(Target, Block), Entry.Id, Entry.Layer).Curves[Entry.Component];
				Key Placed = Entry.Item;
				Placed.Time = Local;
				Placed.Value = std::clamp(Placed.Value, Info(Entry.Id).Min, Info(Entry.Id).Max);
				Curves::PlaceKey(Part, Placed, HalfFrame);
				Skipped += Curves::KeyAt(Part, Local, 0.0f) == u32(-1);
				if (Entry.Id == Param::TimeScale)
				{
					Refit(Core, Target);
				}
			}
		}
	};
	Walk(true);
	if (!HasTarget)
	{
		SetStatus(true, "Select the objects to paste keys into.");
		return;
	}
	SelectedKeys.clear();
	Walk(false);
	std::sort(SelectedKeys.begin(), SelectedKeys.end());
	SelectedKeys.erase(std::unique(SelectedKeys.begin(), SelectedKeys.end()), SelectedKeys.end());
	Focus = SelectedKeys.empty() ? FocusKind::Objects : FocusKind::Keys;
	if (Skipped)
	{
		SetStatus(true, "%u keys could not be pasted.", Skipped);
	}
}

void Editor::Relink(Scene& Data, u32 Owner, const Attachment& Wanted, Attachment& Out, bool IsSameScene)
{
	const AttachLink* From[2] = { &Wanted.Parent, &Wanted.LookAt };
	AttachLink* To[2] = { &Out.Parent, &Out.LookAt };
	for (u32 Index = 0; Index < 2; ++Index)
	{
		const auto Mapped = std::find_if(IdMap.begin(), IdMap.end(), [&](const std::pair<u32, u32>& Pair) { return Pair.first == From[Index]->ObjectId; });
		AttachLink Link = *From[Index];
		Link.ObjectId = Mapped != IdMap.end() ? Mapped->second : IsSameScene ? Link.ObjectId : 0;
		const bool IsAllowed = Link.ObjectId && CanAttach(Data, Owner, Link.ObjectId);
		*To[Index] = IsAllowed ? Link : AttachLink {};
		if (Link.ObjectId && !IsAllowed)
		{
			SetStatus(true, "Links that would close a loop or a chain deeper than %u were dropped.", MaxAttachDepth);
		}
	}
}

void Editor::Duplicate(Player& Core)
{
	if (!Copy(Core, Scratch))
	{
		return;
	}
	if (Scratch.Kind == ClipKind::CameraBlocks && Focus != FocusKind::Block)
	{
		SetStatus(true, "Duplicate a camera block instead.");
		return;
	}
	Undo.Begin("Duplicate");
	if (Scratch.Kind == ClipKind::ActorBlocks)
	{
		PasteBlocks(Core, Scratch, FocusObject);
		return;
	}
	if (Scratch.Kind == ClipKind::Keys)
	{
		PasteKeys(Core, Scratch, Scratch.Origin + Scratch.Extent + 1.0f / static_cast<float>(Core.Data.Header.FrameRate), true);
		return;
	}
	Paste(Core, Scratch, Scratch.Origin + Scratch.Extent);
}

void Editor::KeyFromView(Player& Core, float Time, bool IsEye)
{
	const Object* Found = FindCamera(Core.Data);
	PoseKey Pose;
	ViewPose(Core, IsEye, Pose);
	const s32 At = Found ? Cameras::BlockAt(Found->Camera, Time - Found->Span.Start) : -1;
	const BlockCache* Cache = At >= 0 ? Core.FindCameraBlock(static_cast<u32>(At)) : nullptr;
	if (!Cache)
	{
		AddCameraMove(Core, Time, &Pose);
		return;
	}
	Object& Rig = *Edit(Core, Found->Id, "Key from view");
	const float Frame = 1.0f / static_cast<float>(Core.Data.Header.FrameRate);
	ToSpace(Cache->Frames, Pose);
	CameraBlock& Shot = Rig.Camera.Blocks[At];
	const float Inside = Time - Rig.Span.Start - Shot.Start;
	PutPose(Shot, *Cache, Inside, 0.5f * Frame, Pose);
	const Lane* FovLane = Curves::FindLane(Shot.Lanes, Param::Fov, 0);
	const float Shown = FovLane ? Curves::Value(FovLane->Curves[0], Inside, Shot.Fov) : Shot.Fov;
	if ((IsEye || Core.View() == ViewSource::Editor) && EyeFov > 0.0f && std::fabs(EyeFov - (IsEye ? FlyFov : Shown > 0.0f ? Shown : GameFov())) > 0.01f)
	{
		UseGameFov(Rig, At, Param::Fov);
		Curves::SetKey(Curves::EnsureLane(Shot.Lanes, Param::Fov, 0).Curves[0], Inside, EyeFov, 0.5f * Frame);
	}
	FocusObject = Rig.Id;
	FocusBlock = At;
	Focus = FocusKind::Block;
}

void Editor::HoverLane(u32 ObjectId, const LaneSlot& Slot)
{
	if (ImGui::IsItemHovered())
	{
		HoverLaneObject = ObjectId;
		HoverLaneId = Slot.Id;
		HoverLaneLayer = Slot.Layer;
	}
}

void Editor::KeyPlayhead(Player& Core)
{
	constexpr Param Keyed[7][3] = { { Param::Position, Param::Rotation, Param::Scale }, { Param::Count, Param::Count, Param::Count }, { Param::Position, Param::Rotation, Param::Count }, { Param::Position, Param::Rotation, Param::Size }, { Param::Position, Param::Rotation, Param::Count }, { Param::Count, Param::Count, Param::Count }, { Param::Count, Param::Count, Param::Count } };
	const Object* Hovered = Find(Core.Data, HoverLaneObject);
	if (Hovered)
	{
		const s32 Block = Hovered->Type == ObjectType::Camera ? CameraOwnerAt(*Hovered, HoverLaneId, HoverLaneLayer, Core.Time - Hovered->Span.Start) : -1;
		KeyIds.assign(1, Hovered->Id);
		if (Hovered->Type != ObjectType::Camera || Block >= 0)
		{
			ToggleKey(Core, KeyIds, Block, HoverLaneId, HoverLaneLayer, true);
		}
		return;
	}
	for (u32 Index = 0; Index < Selected.size(); ++Index)
	{
		const Object* Target = Find(Core.Data, Selected[Index]);
		const bool IsCamera = Target && Target->Type == ObjectType::Camera;
		if (IsCamera && Core.View() == ViewSource::Global)
		{
			SetStatus(true, "Fly or open the editor camera to key the camera from the view.");
		}
		else if (IsCamera)
		{
			KeyFromView(Core, Core.Time, false);
		}
		for (u32 Slot = 0; Target && !IsCamera && Slot < 3; ++Slot)
		{
			const Param Id = Keyed[static_cast<u32>(Target->Type)][Slot];
			KeyIds.assign(1, Target->Id);
			if (Id != Param::Count && IsParamShown(*Target, Id))

			{
				ToggleKey(Core, KeyIds, -1, Id, 0, true);
			}
		}
	}
}

void Editor::ViewPose(const Player& Core, bool IsEye, PoseKey& Pose) const

{
	const bool IsEditorView = IsEye || Core.View() == ViewSource::Editor;
	Fmatrix View = Fidentity;
	View.i = Device.vCameraRight;
	View.j = Device.vCameraTop;
	View.k = Device.vCameraDirection;
	View.getHPB(Pose.Rotation);
	Pose.Rotation = IsEditorView ? Fvector().set(Yaw, Pitch, Roll) : Pose.Rotation;
	Pose.Rotation.mul(rad2deg(1.0f));
	Pose.Position = IsEditorView ? Eye : Device.vCameraPosition;
}

const BlockCache* Editor::PoseAt(const Player& Core, const CameraBlock& Shot, s32 Block, float Local, PoseKey& Pose, u32& At) const
{
	const BlockCache* Cache = Core.FindCameraBlock(static_cast<u32>(Block));
	At = Cache ? PoseKeyAt(Shot, *Cache, Local, 0.5f / static_cast<float>(Core.Data.Header.FrameRate)) : u32(-1);
	if (At != u32(-1))
	{
		Pose = Shot.Keys[At];
		return Cache;
	}
	ViewPose(Core, false, Pose);
	if (Cache && !Shot.Keys.empty())
	{
		Fmatrix Turn = Fidentity;
		Cameras::PathPose(Shot, Cameras::PathTime(Shot, *Cache, Local), Pose.Position, Turn);
		Turn.getHPB(Pose.Rotation);
		Pose.Rotation.mul(rad2deg(1.0f));
	}
	else if (Cache)
	{
		ToSpace(Cache->Frames, Pose);
	}
	return Cache;
}

void Editor::ToSpace(const LinkFrames& Frames, PoseKey& Pose)
{
	Fmatrix Turn;
	Turn.setHPB(deg2rad(Pose.Rotation.x), deg2rad(Pose.Rotation.y), deg2rad(Pose.Rotation.z));
	Turn.c = Pose.Position;
	Spaces::Decompose(Frames, Turn);
	Pose.Position = Turn.c;
	Turn.getHPB(Pose.Rotation);
	Pose.Rotation.mul(rad2deg(1.0f));
}

void Editor::PutPose(CameraBlock& Shot, const BlockCache& Cache, float Local, float HalfFrame, PoseKey Pose)
{
	const u32 At = PoseKeyAt(Shot, Cache, Local, HalfFrame);
	if (At == u32(-1) && Shot.Keys.size() >= MaxPoseKeys)
	{
		SetStatus(true, "The block already has %u pose keys.", MaxPoseKeys);
		return;
	}
	const float Time = At != u32(-1) ? Shot.Keys[At].Time : std::clamp(Cameras::PathTime(Shot, Cache, Local), 0.0f, MaxTime);
	const auto Next = std::upper_bound(Shot.Keys.begin(), Shot.Keys.end(), Time, [](float Moment, const PoseKey& Item) { return Moment < Item.Time; });
	const Interp Mode = At != u32(-1) ? Shot.Keys[At].Mode : Next != Shot.Keys.begin() ? (Next - 1)->Mode : Next != Shot.Keys.end() ? Next->Mode : Interp::EaseInOut;
	Cameras::SetPoseAt(Shot, Time, Pose.Position, Pose.Rotation, Mode, HalfFrame);
}

u32 Editor::PoseKeyAt(const CameraBlock& Shot, const BlockCache& Cache, float Local, float HalfFrame)
{
	const float Lo = Cameras::PathTime(Shot, Cache, Local - HalfFrame), Hi = Cameras::PathTime(Shot, Cache, Local + HalfFrame);
	const auto Found = std::lower_bound(Shot.Keys.begin(), Shot.Keys.end(), Lo, [](const PoseKey& Item, float Moment) { return Item.Time < Moment; });
	return Found != Shot.Keys.end() && Found->Time <= Hi ? static_cast<u32>(Found - Shot.Keys.begin()) : u32(-1);
}

void Editor::RemovePose(Player& Core, s32 Block, u32 Index)
{
	const Object* Rig = FindCamera(Core.Data);
	if (!Rig || Block < 0 || Block >= static_cast<s32>(Rig->Camera.Blocks.size()) || Index >= Rig->Camera.Blocks[Block].Keys.size())
	{
		return;
	}
	if (Rig->Camera.Blocks[Block].Keys.size() == 1)
	{
		SetStatus(true, "A shot keeps its last pose key.");
		return;
	}
	const float Time = Rig->Camera.Blocks[Block].Keys[Index].Time;
	Cameras::RemovePoseAt(Edit(Core, Rig->Id, "Remove pose key")->Camera.Blocks[Block], Time, 1e-4f);
}

void Editor::UseGameFov(Object& Target, s32 Block, Param Id)
{
	if (Block >= 0 && Id == Param::Fov && !(Target.Camera.Blocks[Block].Fov > 0.0f))
	{
		Target.Camera.Blocks[Block].Fov = GameFov();
	}
}

void Editor::SetMuted(Player& Core, u32 Id, bool IsMuted)
{
	const bool IsGroup = IsSelected(Id);
	Undo.Begin(IsMuted ? "Mute" : "Unmute");
	for (Object& Item : Core.Data.Objects)
	{
		if (Item.Id == Id || (IsGroup && IsSelected(Item.Id)))
		{
			Undo.Touch(Core.Data, Item.Id);
			Item.IsMuted = IsMuted;
		}
	}
}

void Editor::ResetLength(Player& Core)
{
	Undo.Begin("Reset length");
	for (Object& Item : Core.Data.Objects)
	{
		if (Item.Type == ObjectType::Actor && IsSelected(Item.Id))
		{
			Undo.Touch(Core.Data, Item.Id);
			Item.Span.IsLengthLocked = false;
			Core.RequestFit(Item.Id);
		}
	}
}

void Editor::DeleteSelection(Player& Core)
{
	Scene& Data = Core.Data;
	if (HoverPose >= 0)
	{
		RemovePose(Core, HoverBlock, static_cast<u32>(HoverPose));
		return;
	}
	if (HoverMarker >= 0)
	{
		RemoveMarker(Core, HoverMarker);
		return;
	}
	if (Focus == FocusKind::Keys)
	{
		RemoveKeys(Core);
		return;
	}
	if (Focus == FocusKind::Block)
	{
		Focus = FocusKind::Objects;
		RemoveBlock(Core, FocusObject, FocusBlock, FocusLayer);
		return;
	}
	if (SelectedFolder)
	{
		Undo.Begin("Delete folder");
		Undo.TouchHeader(Data);
		u32 Row = NextRow(Data, 0);
		for (Object& Item : Data.Objects)
		{
			if (Item.FolderId == SelectedFolder)
			{
				Undo.Touch(Data, Item.Id);
				Item.FolderId = 0;
				Item.Row = Row++;
			}
		}
		std::erase_if(Data.Header.Folders, [this](const Folder& Item) { return Item.Id == SelectedFolder; });
		SelectedFolder = 0;
		return;
	}
	if (Selected.empty())
	{
		return;
	}
	const Object* Rig = FindCamera(Data);
	std::erase_if(Selected, [Rig](u32 Id) { return Rig && Id == Rig->Id; });
	if (Selected.empty())
	{
		SetStatus(true, "The camera track cannot be deleted.");
		return;
	}
	Undo.Begin(Selected.size() > 1 ? "Delete objects" : "Delete object");
	for (u32 Id : Selected)
	{
		Undo.Touch(Data, Id);
	}
	std::erase_if(Data.Objects, [this](const Object& Item) { return IsSelected(Item.Id); });
	Selected.clear();
}

void Editor::Select(u32 Id, bool IsCtrl, bool IsShift)
{
	SelectedFolder = 0;
	const auto RowOf = [this, IsShift](u32 Target) { return IsShift ? std::find_if(Rows.begin(), Rows.end(), [Target](const RowItem& Item) { return !Item.IsFolder && Item.Id == Target; }) : Rows.end(); };
	auto From = RowOf(Anchor), To = RowOf(Id);
	if (From != Rows.end() && To != Rows.end())
	{
		if (From > To)
		{
			std::swap(From, To);
		}
		Selected.clear();
		for (++To; From != To; ++From)
		{
			if (!From->IsFolder)
			{
				Selected.push_back(From->Id);
			}
		}
		return;
	}
	const auto Found = std::find(Selected.begin(), Selected.end(), Id);
	Anchor = Id;
	if (!IsCtrl)
	{
		Selected.assign(1, Id);
		return;
	}
	Found == Selected.end() ? Selected.push_back(Id) : (void)Selected.erase(Found);
}

bool Editor::IsSelected(u32 Id) const
{
	return std::find(Selected.begin(), Selected.end(), Id) != Selected.end();
}

void Editor::PruneSelection(const Scene& Data)
{
	std::erase_if(Selected, [&Data](u32 Id) { return !Find(Data, Id); });
	std::erase_if(SelectedKeys, [&Data](const KeyRef& Ref)
	{
		const Object* Owner = Find(Data, Ref.ObjectId);
		const xr_vector<Lane>* Lanes = Owner && HasOwner(*Owner, Ref.Block) ? &LaneList(*Owner, Ref.Block) : nullptr;
		const Lane* Found = Lanes ? Curves::FindLane(*Lanes, Ref.Id, Ref.Layer) : nullptr;
		return !Found || Ref.Index >= Found->Curves[Ref.Component].Keys.size() || !IsParamShown(*Owner, Ref.Id);
	});
	Focus = Focus == FocusKind::Keys && SelectedKeys.empty() ? FocusKind::Objects : Focus;
	const s32 MarkerCount = static_cast<s32>(Data.Header.Markers.size());
	for (s32* Slot : { &HoverMarker, &PopupMarker, &RenameMarker })
	{
		*Slot = *Slot < MarkerCount ? *Slot : -1;
	}
	SelectedFolder = std::any_of(Data.Header.Folders.begin(), Data.Header.Folders.end(), [this](const Folder& Item) { return Item.Id == SelectedFolder; }) ? SelectedFolder : 0;
}

void Editor::StartRename(const Scene& Data, u32 Id, bool IsFolder)
{
	const Object* Target = IsFolder ? nullptr : Find(Data, Id);
	const auto Group = std::find_if(Data.Header.Folders.begin(), Data.Header.Folders.end(), [Id](const Folder& Item) { return Item.Id == Id; });
	if (!Target && (!IsFolder || Group == Data.Header.Folders.end()))
	{
		return;
	}
	xr_sprintf(RenameBuffer, "%.127s", Target ? Target->Name.c_str() : Group->Name.c_str());
	RenameId = Id;
	RenameMarker = -1;
	IsRenameFolder = IsFolder;
	RenameFrames = 3;
}

void Editor::CommitRename(Player& Core)
{
	xr_vector<Marker>& Markers = Core.Data.Header.Markers;
	if (RenameMarker >= 0)
	{
		if (RenameMarker < static_cast<s32>(Markers.size()) && Markers[RenameMarker].Name != RenameBuffer)
		{
			Undo.Begin("Rename marker");
			Undo.TouchHeader(Core.Data);
			Markers[RenameMarker].Name = RenameBuffer;
		}
		return;
	}
	if (IsRenameFolder)
	{
		Folder* Group = FindFolder(Core.Data, RenameId);
		if (Group && Group->Name != RenameBuffer)
		{
			Undo.Begin("Rename");
			Undo.TouchHeader(Core.Data);
			Group->Name = RenameBuffer;
		}
		return;
	}
	const Object* Target = Find(Core.Data, RenameId);
	if (Target && Target->Name != RenameBuffer)
	{
		Object* Item = Edit(Core, RenameId, "Rename");
		Item->Name = RenameBuffer;
		Item->IsNameLocked = true;
	}
}

Folder* Editor::FindFolder(Scene& Data, u32 Id)
{
	const auto Found = std::find_if(Data.Header.Folders.begin(), Data.Header.Folders.end(), [Id](const Folder& Item) { return Item.Id == Id; });
	return Found == Data.Header.Folders.end() ? nullptr : &*Found;
}

u32 Editor::NextRow(const Scene& Data, u32 FolderId)
{
	u32 Next = 0;
	for (const Object& Item : Data.Objects)
	{
		Next = Item.FolderId == FolderId ? std::max(Next, Item.Row + 1) : Next;
	}
	for (const Folder& Item : Data.Header.Folders)
	{
		Next = std::max(Next, FolderId ? 0u : Item.Row + 1);
	}
	return Next;
}
}

void RenderCinemaEditor()
{
	bool& IsOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaEditor)];
	xr_unique_ptr<Cinema::Editor>& Instance = Cinema::EditorUi::g_Editor;
	if (!IsOpen)
	{
		Cinema::Player* Core = Cinema::FindPlayer();
		if (Instance && Instance->WasOpen && Core)
		{
			Instance->Close(*Core);
		}
		return;
	}
	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, Instance ? Instance->Opacity() : kGeneralAlphaLevelForImGuiWindows));
	if (g_pGameLevel && g_pGameLevel->bReady)
	{
		if (!Instance)
		{
			Instance = xr_make_unique<Cinema::Editor>();
		}
		Instance->Draw(Cinema::EnsurePlayer(), IsOpen);
	}
	else
	{
		ImGui::Begin("Cinema Editor###CinemaNoLevel", &IsOpen, ImGuiWindowFlags_AlwaysAutoResize);
		ImGui::TextUnformatted("Load a level");
		ImGui::End();
	}
	ImGui::PopStyleColor();
}

void CinemaEditor_DrawHidden()
{
	const Cinema::Player* Core = Cinema::FindPlayer();
	if (Cinema::EditorUi::g_Editor && Core && g_pGameLevel && g_pGameLevel->bReady && Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaEditor)])
	{
		Cinema::EditorUi::g_Editor->DrawHidden(*Core);
	}
}

void CinemaEditor_Shutdown()
{
	CImGuiManager::Instance().SetMouseLook(false);
	Cinema::EditorUi::g_Editor.reset();
}
