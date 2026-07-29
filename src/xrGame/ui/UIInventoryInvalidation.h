#pragma once

class CUICellItem;
class CUIDragDropListEx;

// Single authority for inventory UI cell lifetime / stale-pointer safety
// Call sites: ClearAll, drag end, slot/list item removal, bag/quick-slot rebuild
namespace UIInventoryInvalidation
{
	// Detach PIItem binding and disable destructor callbacks that touch game objects
	// Safe to call multiple times. Does not delete the widget
	void PrepareCellForDestroy(CUICellItem* cell);

	// PrepareCellForDestroy + xr_delete. Nulls the caller pointer
	void DestroyCell(CUICellItem*& cell);

	// Drop static drag ghost (deferred destroy) if any
	void EndDragSession();

	// Before wiping list cells: end drag, bump owner-generation token, notify observers, clear selection
	void BeginListContentReset(CUIDragDropListEx& list);
}
