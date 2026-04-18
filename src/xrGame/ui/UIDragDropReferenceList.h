#pragma once
#include "UIDragDropListEx.h"
#include "../../xrUI/Widgets/UI3dStatic.h"
#include "../../xrServerEntities/inventory_space.h"
class CInventoryOwner;

class CUIDragDropReferenceList final : public CUIDragDropListEx
{
private:
	typedef CUIDragDropListEx inherited;
	typedef xr_vector<CUI3dStatic*> ITEMS_REFERENCES_VEC;
	typedef ITEMS_REFERENCES_VEC::iterator ITEMS_REFERENCES_VEC_IT;
	ITEMS_REFERENCES_VEC m_references;

public:
	CUIDragDropReferenceList();
	virtual ~CUIDragDropReferenceList();
	virtual void SetItem(CUICellItem* itm);
	virtual bool SetItem(CUICellItem* itm, Fvector2 abs_pos);
	virtual void SetItem(CUICellItem* itm, Ivector2 cell_pos);
	bool			SetItemAtQuickSlotCell	(CUICellItem* itm, u8 slotIndex);
	virtual CUICellItem* RemoveItem(CUICellItem* itm, bool force_root);

	void Initialize();
	CUICellContainer* GetContainer() {return m_container;};
	void LoadItemTexture(const char* section, Ivector2 cell_pos);
	void ReloadReferences(CInventoryOwner* pActor);

	virtual void 	OnItemDBClick		(CUIWindow* w, void* pData);
	virtual void 	OnItemDrop			(CUIWindow* w, void* pData);

	virtual CUIWindow* ui_cast_window() { return this; }
};