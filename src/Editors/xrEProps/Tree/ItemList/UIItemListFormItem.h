#pragma once
class UIItemListFormItem :public UITreeItem
{
public:
	UIItemListFormItem(shared_str Name);
	virtual ~UIItemListFormItem();

	bool bIsFavorite;

	class UIItemListForm* Form;
	int Index;
	ListItem* Object;
	shared_str Text;
	bool bSelected;
	void Draw();
	void DrawRoot();
	void Sort();
	void ClearSelection();
	void Selected(int Start, int End);
	void SelectedToFavorite(bool Favorite);

	void CheckFavorited();
	void SetFavorite(bool bFavorite);
	void FillFavorited(xr_vector< ListItem*>& selected);
	void CheckFavorited(xr_vector< ListItem*>& selected);
protected:
	virtual UITreeItem* CreateItem(shared_str Name);
	bool m_bIsMixed;
};
