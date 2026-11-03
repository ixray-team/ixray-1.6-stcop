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
	virtual ~CUIDragDropReferenceList() override;
	virtual void SetItem(CUICellItem* itm) override;
	virtual bool SetItem(CUICellItem* itm, Fvector2 abs_pos) override;
	virtual void SetItem(CUICellItem* itm, Ivector2 cell_pos) override;
	bool			SetItemAtQuickSlotCell	(CUICellItem* itm, u8 slotIndex);
	virtual CUICellItem* RemoveItem(CUICellItem* itm, bool force_root) override;

	void Initialize();
	CUICellContainer* GetContainer() {return m_container;};
	void LoadItemTexture(const char* section, Ivector2 cell_pos);
	void ReloadReferences(CInventoryOwner* pActor);

	virtual void 	OnItemDBClick		(CUIWindow* w, void* pData) override;
	virtual void 	OnItemDrop			(CUIWindow* w, void* pData) override;

	virtual CUIWindow* ui_cast_window() override { return this; }
};