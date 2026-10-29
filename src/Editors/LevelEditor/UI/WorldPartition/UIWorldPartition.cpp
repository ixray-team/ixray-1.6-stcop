#include "stdafx.h"
#include "IconsFontAwesome6.h"

UIWorldPartition* UIWorldPartition::Form = nullptr;

UIWorldPartition::UIWorldPartition()
{
	ViewX = 0.0f;
	ViewZ = 0.0f;
	Zoom = 1.0f;
	CanvasMin = ImVec2(0, 0);
	CanvasSize = ImVec2(1, 1);
	NeedFit = true;
	RectSelecting = false;
	RectStart = ImVec2(0, 0);
	EditCellSize = GWorldPartition.GetCellSize();
}

UIWorldPartition::~UIWorldPartition()
{
}

void UIWorldPartition::Draw()
{
	PROF_EVENT("UIWorldPartition::Draw")
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(300, 300));

	if (!ImGui::Begin("World Partition", &bOpen))
	{
		ImGui::PopStyleVar(1);
		ImGui::End();
		return;
	}

	IsDocked = ImGui::IsWindowDocked();
	IsFocused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);

	DrawToolbar();
	DrawCanvas();

	u32 Disabled = 0;
	for (const auto& [Key, Info] : GWorldPartition.GetCells())
	{
		Disabled += GWorldPartition.IsCellDisabled(Key) ? 1 : 0;
	}
	const u32 Statics = GWorldPartition.GetStaticCount();
	ImGui::TextDisabled("%u cells, %u disabled, %u selected | static objects loaded %u/%u", (u32)GWorldPartition.GetCells().size(), Disabled, (u32)SelectedCells.size(), Statics - GWorldPartition.GetPendingCount(), Statics);

	ImGui::PopStyleVar(1);
	ImGui::End();
}

void UIWorldPartition::DrawToolbar()
{
	ImGui::SetNextItemWidth(90.0f);
	// Follows the loaded level unless the field is being edited
	if (!EditingCellSize)
		EditCellSize = GWorldPartition.GetCellSize();
	ImGui::InputFloat("##CellSize", &EditCellSize, 0, 0, "%.0f m");
	EditingCellSize = ImGui::IsItemActive();
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		GWorldPartition.SetCellSize(EditCellSize);
		EditCellSize = GWorldPartition.GetCellSize();
		SelectedCells.clear();
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Cell size. Changing it enables all cells.");
	}

	ImGui::SameLine();
	ImGui::BeginDisabled(SelectedCells.empty());
	if (ImGui::Button(ICON_FA_EYE_SLASH " Disable"))
		SetSelectedDisabled(true);
	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_EYE " Enable"))
		SetSelectedDisabled(false);
	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_CROSSHAIRS " Isolate"))
		IsolateSelected();
	ImGui::EndDisabled();

	ImGui::SameLine();
	ImGui::BeginDisabled(!GWorldPartition.HasDisabledCells());
	if (ImGui::Button("Enable all"))
		GWorldPartition.EnableAll();
	ImGui::EndDisabled();

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_EXPAND " Fit"))
		FitView();

	bool Enabled = GWorldPartition.IsEnabled();
	if (ImGui::Checkbox("Enabled", &Enabled))
		GWorldPartition.SetEnabled(Enabled);
	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Off: every object loads, cells are ignored and large meshes are culled by the camera view.\nOn: cells and streaming apply, large meshes hide outside the streaming radius.");
	}
	ImGui::SameLine();

	ImGui::BeginDisabled(!Enabled);
	// Streaming of static object references
	ImGui::Checkbox("Unload disabled", &GWorldPartition.UnloadDisabled);
	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Static objects in disabled cells release their library objects.");
	}

	ImGui::SameLine();
	ImGui::Checkbox("Stream around camera", &GWorldPartition.StreamAroundCamera);

	ImGui::SameLine();
	ImGui::BeginDisabled(!GWorldPartition.StreamAroundCamera);
	ImGui::SetNextItemWidth(90.0f);
	ImGui::DragFloat("##Radius", &GWorldPartition.StreamRadius, 5.0f, 25.0f, 5000.0f, "%.0f m");
	ImGui::EndDisabled();

	ImGui::SameLine();
	ImGui::BeginDisabled(GWorldPartition.GetPendingCount() == 0);
	if (ImGui::Button(ICON_FA_DOWNLOAD " Load all"))
	{
		// Turning streaming off keeps the loaded objects from streaming out again
		GWorldPartition.UnloadDisabled = false;
		GWorldPartition.StreamAroundCamera = false;
		GWorldPartition.StreamInAll();
	}
	ImGui::EndDisabled();

	// Enabled
	ImGui::EndDisabled();
}

void UIWorldPartition::FitView()
{
	const Fbox& Bounds = GWorldPartition.GetBounds();
	if (!GWorldPartition.HasBounds())
	{
		ViewX = 0.0f;
		ViewZ = 0.0f;
		Zoom = 1.0f;
		return;
	}

	ViewX = (Bounds.min.x + Bounds.max.x) * 0.5f;
	ViewZ = (Bounds.min.z + Bounds.max.z) * 0.5f;

	const float Width = std::max(Bounds.max.x - Bounds.min.x, 1.0f);
	const float Depth = std::max(Bounds.max.z - Bounds.min.z, 1.0f);
	Zoom = std::min(CanvasSize.x / Width, CanvasSize.y / Depth) * 0.9f;
}

ImVec2 UIWorldPartition::WorldToScreen(float X, float Z) const
{
	return ImVec2(CanvasMin.x + CanvasSize.x * 0.5f + (X - ViewX) * Zoom, CanvasMin.y + CanvasSize.y * 0.5f - (Z - ViewZ) * Zoom);
}

void UIWorldPartition::ScreenToWorld(const ImVec2& Pos, float& X, float& Z) const
{
	X = ViewX + (Pos.x - CanvasMin.x - CanvasSize.x * 0.5f) / Zoom;
	Z = ViewZ - (Pos.y - CanvasMin.y - CanvasSize.y * 0.5f) / Zoom;
}

u64 UIWorldPartition::CellAtScreen(const ImVec2& Pos) const
{
	Fvector World;
	ScreenToWorld(Pos, World.x, World.z);
	World.y = 0.0f;

	s32 X, Z;
	GWorldPartition.CellOf(World, X, Z);
	return CWorldPartition::MakeKey(X, Z);
}

void UIWorldPartition::DrawCanvas()
{
	CanvasMin = ImGui::GetCursorScreenPos();
	CanvasSize = ImGui::GetContentRegionAvail();
	CanvasSize.y -= ImGui::GetFrameHeightWithSpacing();
	CanvasSize.x = std::max(CanvasSize.x, 50.0f);
	CanvasSize.y = std::max(CanvasSize.y, 50.0f);

	if (NeedFit && GWorldPartition.HasBounds())
	{
		FitView();
		NeedFit = false;
	}

	ImGui::InvisibleButton("Canvas", CanvasSize, ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight | ImGuiButtonFlags_MouseButtonMiddle);
	const bool Hovered = ImGui::IsItemHovered();
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	const ImVec2 CanvasMax(CanvasMin.x + CanvasSize.x, CanvasMin.y + CanvasSize.y);

	// Pan with the middle button, zoom with the wheel around the cursor
	if (ImGui::IsItemActive() && ImGui::IsMouseDragging(ImGuiMouseButton_Middle, 0.0f))
	{
		const ImVec2 Delta = ImGui::GetIO().MouseDelta;
		ViewX -= Delta.x / Zoom;
		ViewZ += Delta.y / Zoom;
	}
	if (Hovered && ImGui::GetIO().MouseWheel != 0.0f)
	{
		float BeforeX, BeforeZ;
		ScreenToWorld(Mouse, BeforeX, BeforeZ);
		Zoom = std::clamp(Zoom * powf(1.2f, ImGui::GetIO().MouseWheel), 0.0005f, 100.0f);
		float AfterX, AfterZ;
		ScreenToWorld(Mouse, AfterX, AfterZ);
		ViewX += BeforeX - AfterX;
		ViewZ += BeforeZ - AfterZ;
	}

	// Left click selects a cell, left drag selects a rectangle
	if (Hovered && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		RectSelecting = true;
		RectStart = Mouse;
	}
	if (RectSelecting && ImGui::IsMouseReleased(ImGuiMouseButton_Left))
	{
		RectSelecting = false;
		if (!ImGui::GetIO().KeyCtrl)
			SelectedCells.clear();

		const u64 First = CellAtScreen(RectStart);
		const u64 Last = CellAtScreen(Mouse);
		const s32 MinX = std::min(CWorldPartition::KeyX(First), CWorldPartition::KeyX(Last));
		const s32 MaxX = std::max(CWorldPartition::KeyX(First), CWorldPartition::KeyX(Last));
		const s32 MinZ = std::min(CWorldPartition::KeyZ(First), CWorldPartition::KeyZ(Last));
		const s32 MaxZ = std::max(CWorldPartition::KeyZ(First), CWorldPartition::KeyZ(Last));

		if (First == Last && ImGui::GetIO().KeyCtrl && SelectedCells.contains(First))
		{
			SelectedCells.erase(First);
		}
		else
		{
			for (s32 X = MinX; X <= MaxX; X++)
			{
				for (s32 Z = MinZ; Z <= MaxZ; Z++)
					SelectedCells.insert(CWorldPartition::MakeKey(X, Z));
			}
		}
	}
	if (Hovered && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		FocusCell(CellAtScreen(Mouse));
	}

	if (Hovered && ImGui::IsMouseReleased(ImGuiMouseButton_Right))
	{
		const u64 Key = CellAtScreen(Mouse);
		if (!SelectedCells.contains(Key))
		{
			SelectedCells.clear();
			SelectedCells.insert(Key);
		}
		ImGui::OpenPopup("CellContext");
	}
	if (ImGui::BeginPopup("CellContext"))
	{
		DrawContextMenu();
		ImGui::EndPopup();
	}

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	DrawList->PushClipRect(CanvasMin, CanvasMax, true);
	DrawList->AddRectFilled(CanvasMin, CanvasMax, ImGui::GetColorU32(ImGuiCol_FrameBg));

	const float CellSize = GWorldPartition.GetCellSize();
	const float CellPixels = CellSize * Zoom;

	// Grid lines when cells are big enough to tell apart
	if (CellPixels > 6.0f)
	{
		float MinX, MaxZ, MaxX, MinZ;
		ScreenToWorld(CanvasMin, MinX, MaxZ);
		ScreenToWorld(CanvasMax, MaxX, MinZ);

		const ImU32 GridColor = ImGui::GetColorU32(ImGuiCol_Border, 0.5f);
		for (float X = floorf(MinX / CellSize) * CellSize; X <= MaxX; X += CellSize)
			DrawList->AddLine(WorldToScreen(X, MinZ), WorldToScreen(X, MaxZ), GridColor);
		for (float Z = floorf(MinZ / CellSize) * CellSize; Z <= MaxZ; Z += CellSize)
			DrawList->AddLine(WorldToScreen(MinX, Z), WorldToScreen(MaxX, Z), GridColor);
	}

	u32 MaxObjects = 1;
	for (const auto& [Key, Info] : GWorldPartition.GetCells())
		MaxObjects = std::max(MaxObjects, Info.Objects);

	auto CellRect = [&](u64 Key, ImVec2& Min, ImVec2& Max)
	{
		const s32 X = CWorldPartition::KeyX(Key);
		const s32 Z = CWorldPartition::KeyZ(Key);
		Min = WorldToScreen(X * CellSize, (Z + 1) * CellSize);
		Max = WorldToScreen((X + 1) * CellSize, Z * CellSize);
	};

	// Occupied cells: brighter means more objects, red means disabled
	for (const auto& [Key, Info] : GWorldPartition.GetCells())
	{
		ImVec2 Min, Max;
		CellRect(Key, Min, Max);

		const float Density = 0.25f + 0.65f * sqrtf(float(Info.Objects) / float(MaxObjects));
		// Red: disabled, yellow: streamed out by the camera radius, green: loaded
		ImU32 Color = IM_COL32(70, 140, 90, int(255 * Density));
		if (GWorldPartition.IsCellDisabled(Key))
			Color = IM_COL32(170, 60, 60, 200);
		else if (GWorldPartition.IsCellStreamedOut(Key) || Info.Unloaded > 0)
			Color = IM_COL32(210, 190, 40, int(255 * Density));
		DrawList->AddRectFilled(Min, Max, Color);
	}

	for (u64 Key : SelectedCells)
	{
		ImVec2 Min, Max;
		CellRect(Key, Min, Max);
		DrawList->AddRectFilled(Min, Max, IM_COL32(120, 180, 255, 50));
		DrawList->AddRect(Min, Max, IM_COL32(120, 180, 255, 255), 0.0f, 0, 2.0f);
	}

	if (RectSelecting && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
	{
		DrawList->AddRectFilled(RectStart, Mouse, IM_COL32(100, 150, 255, 40));
		DrawList->AddRect(RectStart, Mouse, IM_COL32(100, 150, 255, 200));
	}

	// Camera position
	const ImVec2 Camera = WorldToScreen(EDevice->vCameraPosition.x, EDevice->vCameraPosition.z);
	DrawList->AddCircleFilled(Camera, 5.0f, IM_COL32(80, 170, 255, 255));
	DrawList->AddCircle(Camera, 8.0f, IM_COL32(80, 170, 255, 160), 0, 1.5f);

	// Streaming radius
	if (GWorldPartition.StreamAroundCamera)
	{
		DrawList->AddCircle(Camera, GWorldPartition.StreamRadius * Zoom, IM_COL32(210, 190, 40, 200), 64, 1.5f);
	}

	DrawList->PopClipRect();

	if (Hovered && !RectSelecting)
	{
		const u64 Key = CellAtScreen(Mouse);
		const auto It = GWorldPartition.GetCells().find(Key);
		const u32 Objects = It != GWorldPartition.GetCells().end() ? It->second.Objects : 0;
		const u32 Unloaded = It != GWorldPartition.GetCells().end() ? It->second.Unloaded : 0;
		ImGui::SetTooltip("Cell %d, %d\n%u objects, %u unloaded%s", CWorldPartition::KeyX(Key), CWorldPartition::KeyZ(Key), Objects, Unloaded, GWorldPartition.IsCellDisabled(Key) ? "\nDisabled" : "");
	}
}

void UIWorldPartition::DrawContextMenu()
{
	if (ImGui::MenuItem(ICON_FA_EYE_SLASH " Disable"))
		SetSelectedDisabled(true);
	if (ImGui::MenuItem(ICON_FA_EYE " Enable"))
		SetSelectedDisabled(false);
	if (ImGui::MenuItem(ICON_FA_CROSSHAIRS " Isolate"))
		IsolateSelected();

	ImGui::Separator();
	if (ImGui::MenuItem("Focus camera", nullptr, false, SelectedCells.size() == 1))
		FocusCell(*SelectedCells.begin());
}

void UIWorldPartition::SetSelectedDisabled(bool Disabled)
{
	for (u64 Key : SelectedCells)
		GWorldPartition.SetCellDisabled(Key, Disabled);
}

void UIWorldPartition::IsolateSelected()
{
	for (const auto& [Key, Info] : GWorldPartition.GetCells())
		GWorldPartition.SetCellDisabled(Key, !SelectedCells.contains(Key));
}

void UIWorldPartition::FocusCell(u64 Key)
{
	Fbox Box;
	GWorldPartition.CellBox(CWorldPartition::KeyX(Key), CWorldPartition::KeyZ(Key), Box);
	EContext.UI->CurrentView().m_Camera.ZoomExtents(Box);
	EContext.UI->RedrawScene();
}

void UIWorldPartition::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
			// Belongs to the scene workspace only
			if (EContext.UI->ActiveTabIndex != 0)
			{
				return;
			}

			Form->Draw();
		}
		else
		{
			xr_delete(Form);
		}
	}
}

void UIWorldPartition::Show()
{
	if (Form == nullptr)
	{
		Form = new UIWorldPartition();
		Form->SetStateName("world_partition");
		Form->bOpen = true;
	}
}

void UIWorldPartition::Close()
{
	if (Form != nullptr)
	{
		Form->bOpen = false;
	}
	xr_delete(Form);
}
