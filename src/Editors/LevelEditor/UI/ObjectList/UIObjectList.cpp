#include "stdafx.h"
#include "../../Renderer/Tiramisu/TiramisuEditorNativeScene.h"

UIObjectList* UIObjectList::Form = nullptr;

UIObjectList::UIObjectList():
	m_Root("")
{
	m_Mode = M_Visible;
	m_Filter[0] = 0;
}

UIObjectList::~UIObjectList()
{
}

void UIObjectList::Draw()
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(400, 400));

	if (!ImGui::Begin("Object List", &bOpen))
	{
		ImGui::PopStyleVar(1);
		ImGui::End();
		return;
	}

	IsDocked = ImGui::IsWindowDocked();
	IsFocused = ImGui::IsWindowFocused();

	if (GetEditorNativeSceneDocument().IsOpen())
	{
		DrawNativeObjects();
		ImGui::PopStyleVar(1);
		ImGui::End();
		return;
	}

	ImGui::BeginGroup();
	if (ImGui::RadioButton("All", m_Mode == M_All))
	{
		m_Mode = M_All;
		m_Root.ClearSelcted();
	}
	ImGui::SameLine();
	if (ImGui::RadioButton("Visible", m_Mode == M_Visible))
	{
		m_Mode = M_Visible;
		m_Root.ClearSelcted();
	}
	ImGui::SameLine();
	if (ImGui::RadioButton("Invisible", m_Mode == M_Inbvisible))
	{
		m_Mode = M_Inbvisible;
		m_Root.ClearSelcted();
	}

	ImGui::SameLine();

	const float Avail = ImGui::GetContentRegionAvail().x;
	const float Spacing = ImGui::GetStyle().ItemSpacing.x;
	const float BtnWidth = (Avail - Spacing * 2) / 3.0f;

	if (ImGui::Button("Focus", ImVec2(BtnWidth, 0)))
	{
		for (UITreeItem* Item : m_Root.Items)
		{
			UIObjectListItem* RItem = (UIObjectListItem*)Item;
			if (RItem->Object->Selected())
			{
				RItem->Object->Select(true);
				Fbox bb;
				if (RItem->Object->GetBox(bb))
					UI->CurrentView().m_Camera.ZoomExtents(bb);

				UI->RedrawScene();
				break;
			}
		}
	}

	ImGui::SameLine();
	if (ImGui::Button("Show", ImVec2(BtnWidth, 0)))
	{
		for (UITreeItem* Item : m_Root.Items)
		{
			UIObjectListItem* RItem = (UIObjectListItem*)Item;
			if (RItem->bIsSelected)
				RItem->Object->Show(true);
		}
	}

	ImGui::SameLine();
	if (ImGui::Button("Hide", ImVec2(BtnWidth, 0)))
	{
		for (UITreeItem* Item : m_Root.Items)
		{
			UIObjectListItem* RItem = (UIObjectListItem*)Item;
			if (RItem->bIsSelected)
				RItem->Object->Show(false);
		}
	}

	ImGui::Separator();

	DrawObjects();
	ImGui::SetNextItemWidth(-1);
	if (ImGui::InputTextWithHint("##value", "Search...", m_Filter, sizeof(m_Filter)))
	{
		m_Root.ClearSelcted();
	}

	if (GUIManager->SearchIcon)
	{
		ImVec2 IconSize = { 12,12 };

		ImGui::SameLine();
		ImVec2 cursorPos = ImGui::GetCursorPos();
		ImGui::SetCursorPos(ImVec2(cursorPos.x - IconSize.x - 10.f, 1 + cursorPos.y + (IconSize.y / 4)));

		ImGui::Image(GUIManager->SearchIcon, IconSize);
	}

	ImGui::EndGroup();

	ImGui::PopStyleVar(1);
	ImGui::End();
}

void UIObjectList::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
			Form->BeginDraw();
			Form->Draw();
			Form->EndDraw();
		}
		else
		{
			xr_delete(Form);
		}
	}

}

void UIObjectList::Show()
{
	if (Form == nullptr)
	{
		Form = new UIObjectList();
	}

	if (!Form->bOpen)
	{
		Refresh();
	}
}

void UIObjectList::Close()
{
	xr_delete(Form);
}

void UIObjectList::Refresh()
{
	if (Form == nullptr)
		return;

	xrCriticalSectionGuard guard(Form->LoaderCS);

	Form->m_Root = UIObjectListItem("");
	if (GetEditorNativeSceneDocument().IsOpen())
	{
		Form->m_LastNativeSelected.clear();
		Form->m_LastSelected = nullptr;
		return;
	}

	Form->m_cur_cls = LTools->CurrentClassID();
	for (SceneToolsMapPairIt it = Scene->FirstTool(); it != Scene->LastTool(); ++it)
	{
		ESceneCustomOTool* ot = smart_cast<ESceneCustomOTool*>(it->second);
		if (ot && ((Form->m_cur_cls == OBJCLASS_DUMMY) || (it->first == Form->m_cur_cls)))
		{
			if (it->first == OBJCLASS_DUMMY)
				continue;

			ObjectList lst = ot->GetObjects();
			size_t Index = 0;

			lst.sort([](CCustomObject* A, CCustomObject* B)
				{
					if (A->GetName() == nullptr || A->GetName()[0] == 0)
						return false;
					if (B->GetName() == nullptr || B->GetName()[0] == 0)
						return true;

					return NaturalCompare(A->GetName(), B->GetName());
				}
			);

			for (CCustomObject* Obj : lst)
			{
				if (Obj->GetName() == 0 || Obj->GetName()[0] == 0)
				{
					continue;
				}
				else
				{
					UIObjectListItem* Item = static_cast<UIObjectListItem*>(Form->m_Root.AppendItem(Obj->GetName(), 0));
					VERIFY(Item);

					Item->bIsSelected = Obj->Selected();
					Item->Object = Obj;
				}
			}
		}
	}
	Form->m_LastSelected = nullptr;
}

void UIObjectList::DrawNativeObjects()
{
	TiramisuEditorNativeSceneDocument& Document =
		GetEditorNativeSceneDocument();
	const Tiramisu::Scene::FResolvedRenderScene* Scene =
		Document.GetScene();
	if (!Scene)
		return;

	struct FNativeObjectRow
	{
		xr_string Id;
		xr_string Name;
		xr_string Type;
		xr_string Asset;
		bool Visible = true;
	};

	ImGui::BeginGroup();
	if (ImGui::RadioButton("All", m_Mode == M_All))
		m_Mode = M_All;
	ImGui::SameLine();
	if (ImGui::RadioButton("Visible", m_Mode == M_Visible))
		m_Mode = M_Visible;
	ImGui::SameLine();
	if (ImGui::RadioButton("Invisible", m_Mode == M_Inbvisible))
		m_Mode = M_Inbvisible;

	ImGui::SetNextItemWidth(-1);
	if (ImGui::InputTextWithHint(
			"##NativeObjectSearch", "Search native objects...",
			m_Filter, sizeof(m_Filter)))
	{
		m_LastNativeSelected.clear();
	}

	xr_vector<FNativeObjectRow> Objects;
	Objects.reserve(Scene->Scene.StaticMeshComponents.size() +
		Scene->Scene.LightComponents.size());
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		Scene->Scene.StaticMeshComponents)
	{
		if (m_Mode == M_Visible && !Component.Visible)
			continue;
		if (m_Mode == M_Inbvisible && Component.Visible)
			continue;
		if (m_Filter[0] &&
			!std::strstr(Component.Name.c_str(), m_Filter) &&
			!std::strstr(Component.StaticMesh.c_str(), m_Filter) &&
			!std::strstr("StaticMesh", m_Filter))
		{
			continue;
		}
		Objects.push_back({Component.Id, Component.Name, "StaticMesh",
			Component.StaticMesh, Component.Visible});
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		Scene->Scene.LightComponents)
	{
		if (m_Mode == M_Visible && !Light.Visible)
			continue;
		if (m_Mode == M_Inbvisible && Light.Visible)
			continue;
		xr_string Type;
		switch (Light.Type)
		{
		case Tiramisu::Scene::ELightType::Directional:
			Type = "Directional Light";
			break;
		case Tiramisu::Scene::ELightType::Point:
			Type = "Point Light";
			break;
		case Tiramisu::Scene::ELightType::Spot:
			Type = "Spot Light";
			break;
		}
		if (m_Filter[0] &&
			!std::strstr(Light.Name.c_str(), m_Filter) &&
			!std::strstr(Type.c_str(), m_Filter))
		{
			continue;
		}
		Objects.push_back(
			{Light.Id, Light.Name, std::move(Type), {}, Light.Visible});
	}
	std::ranges::sort(Objects,
		[](const FNativeObjectRow& Left,
			const FNativeObjectRow& Right)
		{
			return NaturalCompare(
				Left.Name.c_str(), Right.Name.c_str());
		});

	ImGui::BeginDisabled(!Document.IsEditableRenderScene());
	if (ImGui::BeginCombo("##AddNativeObject", "Add object"))
	{
		const auto AddLight =
			[&](const char* Label,
				const Tiramisu::Scene::ELightType Type)
			{
				if (!ImGui::Selectable(Label))
					return;
				xr_string Diagnostic;
				const xr_array<float, 16> Transform =
					Tiramisu::Scene::FLightComponent{}.LocalToWorld;
				if (!Document.AddLightComponent(
						Type, Transform, Diagnostic))
				{
					Msg("! Native scene add light: %s",
						Diagnostic.c_str());
					UI->SetStatus(
						"Cannot add native light. See log.");
				}
				else
				{
					m_LastNativeSelected.clear();
					UI->RedrawScene();
				}
			};
		AddLight("Point Light", Tiramisu::Scene::ELightType::Point);
		AddLight("Spot Light", Tiramisu::Scene::ELightType::Spot);
		AddLight("Directional Light",
			Tiramisu::Scene::ELightType::Directional);
		ImGui::EndCombo();
	}
	ImGui::EndDisabled();

	const float Available = ImGui::GetContentRegionAvail().x;
	const float Spacing = ImGui::GetStyle().ItemSpacing.x;
	const float ButtonWidth = (Available - Spacing * 4.0f) / 5.0f;
	ImGui::BeginDisabled(Document.GetSelectionCount() == 0);
	if (ImGui::Button("Focus", {ButtonWidth, 0.0f}))
		ExecCommand(COMMAND_ZOOM_EXTENTS, true);
	ImGui::EndDisabled();
	ImGui::SameLine();
	if (ImGui::Button("All", {ButtonWidth, 0.0f}))
	{
		Document.SelectAll();
		UI->RedrawScene();
	}
	ImGui::SameLine();
	if (ImGui::Button("None", {ButtonWidth, 0.0f}))
	{
		Document.ClearSelection();
		UI->RedrawScene();
	}
	ImGui::SameLine();
	ImGui::BeginDisabled(!Document.IsEditableRenderScene() ||
		Document.GetSelectionCount() == 0);
	if (ImGui::Button("Show", {ButtonWidth, 0.0f}))
	{
		(void)Document.SetSelectedComponentsVisibility(true);
		UI->RedrawScene();
	}
	ImGui::SameLine();
	if (ImGui::Button("Hide", {ButtonWidth, 0.0f}))
	{
		(void)Document.SetSelectedComponentsVisibility(false);
		UI->RedrawScene();
	}
	ImGui::EndDisabled();

	ImGui::TextDisabled("%zu objects, %zu selected",
		Objects.size(), Document.GetSelectionCount());
	const ImGuiTableFlags Flags =
		ImGuiTableFlags_BordersV |
		ImGuiTableFlags_BordersOuterH |
		ImGuiTableFlags_Resizable |
		ImGuiTableFlags_RowBg |
		ImGuiTableFlags_ScrollY;
	if (ImGui::BeginTable("native_objects", 4, Flags,
			ImVec2(0, 0)))
	{
		ImGui::TableSetupScrollFreeze(0, 1);
		ImGui::TableSetupColumn(
			"Name", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn(
			"Type", ImGuiTableColumnFlags_WidthFixed);
		ImGui::TableSetupColumn(
			"Asset", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn(
			"State", ImGuiTableColumnFlags_WidthFixed);
		ImGui::TableHeadersRow();

		ImGuiListClipper Clipper;
		Clipper.Begin(static_cast<int>(Objects.size()));
		while (Clipper.Step())
		{
			for (int Index = Clipper.DisplayStart;
				Index < Clipper.DisplayEnd; ++Index)
			{
				const FNativeObjectRow& Object =
					Objects[static_cast<size_t>(Index)];
				ImGui::PushID(Object.Id.c_str());
				ImGui::TableNextRow();
				ImGui::TableNextColumn();
				const bool Selected =
					Document.IsComponentSelected(Object.Id);
				if (ImGui::Selectable(Object.Name.c_str(), Selected,
						ImGuiSelectableFlags_SpanAllColumns))
				{
					if (ImGui::GetIO().KeyShift &&
						!m_LastNativeSelected.empty())
					{
						auto Last = std::ranges::find(
							Objects, m_LastNativeSelected,
							[](const FNativeObjectRow& Item)
							{
								return Item.Id;
							});
						if (Last != Objects.end())
						{
							const size_t LastIndex =
								static_cast<size_t>(
									std::distance(Objects.begin(), Last));
							const size_t First =
								std::min<size_t>(
									LastIndex, Index);
							const size_t LastRange =
								std::max<size_t>(
									LastIndex, Index);
							xr_vector<xr_string> Range;
							Range.reserve(LastRange - First + 1);
							for (size_t RangeIndex = First;
								RangeIndex <= LastRange; ++RangeIndex)
							{
								Range.push_back(
									Objects[RangeIndex].Id);
							}
							(void)Document.SelectComponents(Range,
								ImGui::GetIO().KeyCtrl
									? EEditorNativeSceneSelectionMode::Add
									: EEditorNativeSceneSelectionMode::Replace);
						}
						else
						{
							(void)Document.SelectObject(
								Tiramisu::Scene::StableSceneIdHash(
									Object.Id),
								EEditorNativeSceneSelectionMode::Replace);
						}
					}
					else
					{
						(void)Document.SelectObject(
							Tiramisu::Scene::StableSceneIdHash(
								Object.Id),
							ImGui::GetIO().KeyCtrl
								? EEditorNativeSceneSelectionMode::Toggle
								: EEditorNativeSceneSelectionMode::Replace);
					}
					m_LastNativeSelected = Object.Id;
					UI->RedrawScene();
				}
				ImGui::TableNextColumn();
				ImGui::TextUnformatted(Object.Type.c_str());
				ImGui::TableNextColumn();
				ImGui::TextDisabled("%s",
					Object.Asset.empty() ? "-" : Object.Asset.c_str());
				ImGui::TableNextColumn();
				ImGui::TextUnformatted(
					Object.Visible ? "Visible" : "Hidden");
				ImGui::PopID();
			}
		}
		ImGui::EndTable();
	}
	ImGui::EndGroup();
}

void UIObjectList::DrawObjects()
{
	if (LTools->CurrentClassID() != m_cur_cls)
		Refresh();

	xrCriticalSectionGuard guard(Form->LoaderCS);
	static ImGuiTableFlags flags = ImGuiTableFlags_BordersV | ImGuiTableFlags_BordersOuterH | ImGuiTableFlags_Resizable | ImGuiTableFlags_RowBg | ImGuiTableFlags_NoBordersInBody| ImGuiTableFlags_ScrollY;

	if (ImGui::BeginTable("objects", 1, flags, ImVec2(0, -ImGui::GetFrameHeight() - 4)))
	{
		//IsDocked = ImGui::IsWindowDocked();
		IsFocused = IsDocked || ImGui::IsWindowFocused();

		ImGui::TableSetupScrollFreeze(1, 1);
		ImGui::TableSetupColumn("Label", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableHeadersRow();
		m_Root.DrawRoot();
		ImGui::EndTable();
	}
}


