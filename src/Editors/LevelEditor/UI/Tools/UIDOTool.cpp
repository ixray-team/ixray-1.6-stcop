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
	float ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Affect in D3D"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;

			if (XRay::ImGui::Button("First Init", { SizeX, 0 }))
				if (DM->Initialize()) Scene->UndoSave();
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Reinit All", { SizeX, 0 }))
				if (DM->Reinitialize()) Scene->UndoSave();

			if (XRay::ImGui::Button("Reinit Objects Only", { SizeX, 0 }))
				if (DM->UpdateObjects(true, false)) Scene->UndoSave();
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Reinit Selected Slot Objects", { SizeX, 0 }))
				if (DM->UpdateObjects(false, true)) Scene->UndoSave();

			XRay::ImGui::Separator();

			if (XRay::ImGui::Button("Clear Slots", { SizeX, 0 }))
				if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to reset slots?") == mrYes)
				{
					DM->ClearSlots();
					Scene->UndoSave();
				}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Clear Details", { SizeX, 0 }))
				if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to clear details?") == mrYes)
				{
					ExecCommand(COMMAND_UPDATE_PROPERTIES);
					DM->Clear();
					Scene->UndoSave();
				}

			XRay::ImGui::Separator();

			if (XRay::ImGui::Button("Object List", { SizeX, 0 }))
			{
				m_DOShuffle = true;
				UIDOShuffle::Show(DM);
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Update Renderer", { SizeX, 0 }))
			{
				DM->InvalidateCache();
				Scene->UndoSave();
			}

			XRay::ImGui::Separator();

			XRay::ImGui::TextFramed("Base Texture");
			ImGui::SameLine(0, ItemSpacingX);

			shared_str StrBaseTextureName = "<none>";
			if (DM->m_Base.name.size() > 0)
			{
				StrBaseTextureName = DM->m_Base.name;
			}

			if (XRay::ImGui::Button(*StrBaseTextureName, ImVec2(-0.01, 0)))
			{
				UIChooseForm::SelectItem(smTexture, 1);
				IsChooseDraw = true;
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
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
