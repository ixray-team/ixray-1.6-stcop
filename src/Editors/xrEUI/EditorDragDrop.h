#pragma once

// Editor asset drag and drop: the payload is a null-terminated string, one payload name per EDragDropType
namespace EditorDragDrop
{
	XREUI_API void SetPayload(EDragDropType Type, const xr_string& Value);

	// nullptr for another payload type
	XREUI_API const char* Accept(EDragDropType Type, ImGuiDragDropFlags Flags = 0, bool* IsDelivery = nullptr);

	XREUI_API bool IsDragging(EDragDropType Type);
	XREUI_API bool IsDragging();

	XREUI_API void HighlightTarget(EDragDropType Type);
	XREUI_API void HighlightTarget();
}
