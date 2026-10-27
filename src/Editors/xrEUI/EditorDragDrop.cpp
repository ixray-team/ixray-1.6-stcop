#include "stdafx.h"
#include "EditorDragDrop.h"

static const char* GetPayloadName(EDragDropType Type)
{
	switch (Type)
	{
	case EDragDropType::Folder:			return "ASSET_FOLDER";
	case EDragDropType::File:			return "ASSET_FILE";
	case EDragDropType::Viewport:		return "ASSET_VIEWPORT";
	case EDragDropType::RandomAppend:	return "ASSET_RANDOM_APPEND";
	case EDragDropType::Details:		return "ASSET_DETAILS";
	case EDragDropType::Logic:			return "ASSET_LOGIC";
	default:							return "ASSET_NONE";
	}
}

static void DrawHighlight()
{
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	const ImVec2 Min = ImGui::GetItemRectMin();
	const ImVec2 Max = ImGui::GetItemRectMax();
	DrawList->AddRectFilled(Min, Max, IM_COL32(50, 50, 70, 100));
	DrawList->AddRect(Min, Max, IM_COL32(100, 180, 255, 255));
}

void EditorDragDrop::SetPayload(EDragDropType Type, const xr_string& Value)
{
	ImGui::SetDragDropPayload(GetPayloadName(Type), Value.c_str(), Value.size() + 1);
}

const char* EditorDragDrop::Accept(EDragDropType Type, ImGuiDragDropFlags Flags, bool* IsDelivery)
{
	const ImGuiPayload* Payload = ImGui::AcceptDragDropPayload(GetPayloadName(Type), Flags);
	if (Payload == nullptr || Payload->Data == nullptr || Payload->DataSize <= 0)
	{
		return nullptr;
	}

	if (IsDelivery != nullptr)
	{
		*IsDelivery = Payload->IsDelivery();
	}
	return (const char*)Payload->Data;
}

bool EditorDragDrop::IsDragging()
{
	return ImGui::GetDragDropPayload() != nullptr && ImGui::IsMouseDragging(ImGuiMouseButton_Left);
}

bool EditorDragDrop::IsDragging(EDragDropType Type)
{
	const ImGuiPayload* Payload = ImGui::GetDragDropPayload();
	return Payload != nullptr && Payload->IsDataType(GetPayloadName(Type)) && ImGui::IsMouseDragging(ImGuiMouseButton_Left);
}

void EditorDragDrop::HighlightTarget(EDragDropType Type)
{
	if (IsDragging(Type))
	{
		DrawHighlight();
	}
}

void EditorDragDrop::HighlightTarget()
{
	if (IsDragging())
	{
		DrawHighlight();
	}
}
