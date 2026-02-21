#include "stdafx.h"
UIDOTool::UIDOTool()
{
	m_DOShuffle = false;
}

UIDOTool::~UIDOTool()
{
}

void UIDOTool::Draw()
{
	ImGuiStyle& Style = ImGui::GetStyle();

	ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(2, 2));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(2, 2));

	float fullWidth = ImGui::GetContentRegionAvail().x;
	float buttonWidth = (fullWidth - Style.ItemSpacing.x) * 0.5f;

	auto Button2 = [&](const char* label, std::function<void()> fn)
	{
		if (ImGui::Button(label, ImVec2(buttonWidth, 0)))
		{
			fn();
		}
	};

	Button2("First Init", [&]
	{
		if (DM->Initialize())
			Scene->UndoSave();
	});

	ImGui::SameLine();

	Button2("Reinit All", [&]
	{
		if (DM->Reinitialize())
			Scene->UndoSave();
	});

	Button2("Reinit Objects Only", [&]
	{
		if (DM->UpdateObjects(true, false))
			Scene->UndoSave();
	});

	ImGui::SameLine();

	Button2("Reinit Selected Slot Objects", [&]
	{
		if (DM->UpdateObjects(false, true))
			Scene->UndoSave();
	});

	ImGui::Separator();

	Button2("Clear Slots", [&]
	{
		if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to reset slots?") == mrYes)
		{
			DM->ClearSlots();
			Scene->UndoSave();
		}
	});

	ImGui::SameLine();

	Button2("Clear Details", [&]
	{
		if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to clear details?") == mrYes)
		{
			ExecCommand(COMMAND_UPDATE_PROPERTIES);
			DM->Clear();
			Scene->UndoSave();
		}
	});

	ImGui::Separator();

	Button2("Object List", [&]
	{
		m_DOShuffle = true;
		UIDOShuffle::Show(DM);
	});

	ImGui::SameLine();

	Button2("Update Renderer", [&]
	{
		DM->InvalidateCache();
		Scene->UndoSave();
	});

	ImGui::Separator();

	ImGui::Text("Base Texture");

	shared_str StrBaseTextureName = "<none>";
	if (DM->m_Base.name.size() > 0)
	{
		StrBaseTextureName = DM->m_Base.name;
	}

	if (ImGui::Button(*StrBaseTextureName, ImVec2(-1, 0)))
	{
		UIChooseForm::SelectItem(smTexture, 1);
		IsChooseDraw = true;
	}

	if (IsChooseDraw)
	{
		xr_string str;
		bool ok;
		if (UIChooseForm::GetResult(ok, str))
		{
			if (ok)
			{
				DM->m_Base.name = str.c_str();
				DM->OnBaseTextureChange(nullptr);
			}

			IsChooseDraw = false;
		}

		UIChooseForm::Update();
	}

	HandleDragDrop();

	ImGui::PopStyleVar(3);
}

void UIDOTool::HandleDragDrop()
{
	const ImGuiPayload* payload = ImGui::GetDragDropPayload();

	if (payload && ImGui::IsMouseDragging(ImGuiMouseButton_Left) && GUIManager->DnDType == EDragDropType::Details)
	{
		ImDrawList* draw_list = ImGui::GetWindowDrawList();
		ImVec2 p_min = ImGui::GetItemRectMin();
		ImVec2 p_max = ImGui::GetItemRectMax();
		draw_list->AddRectFilled(p_min, p_max, IM_COL32(50, 50, 70, 100));
		draw_list->AddRect(p_min, p_max, IM_COL32(100, 180, 255, 255));
	}

	if (!ImGui::BeginDragDropTarget())
		return;

	auto ImData = ImGui::AcceptDragDropPayload("TEST#dti");

	if (ImData == nullptr)
	{
		ImGui::EndDragDropTarget();
		return;
	}

	struct DragDropData
	{
		xr_string FileName;
	} Data = *(DragDropData*)ImData->Data;


	if (Data.FileName.ends_with(".dti"))
	{
		m_DOShuffle = true; 
		UIDOShuffle::Show(DM);
		UIDOShuffle::LoadFromStream(Data.FileName);
	}

	ImGui::EndDragDropTarget();
}

void UIDOTool::OnDrawUI()
{
	if (m_DOShuffle)
	{
		if (UIDOShuffle::GetResult())
		{
			m_DOShuffle = false;
		}
		UIDOShuffle::Update();
	}
}
