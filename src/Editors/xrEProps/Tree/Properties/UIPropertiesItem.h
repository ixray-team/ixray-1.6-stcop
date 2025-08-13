#pragma once
class XREPROPS_API UIPropertiesItem :
	public UITreeItem
{
public:
	UIPropertiesItem(shared_str Name, UIPropertiesForm* PropertiesFrom);
	virtual ~UIPropertiesItem();
	void Draw();
	void DrawRoot();
	void DrawItem();
	void DrawProp();
	void SetSelect();
	void SetUnselect();

public:
	PropItem* PItem;
	UIPropertiesForm* PropertiesFrom;
	bool IsTexture = false;

protected:
	virtual UITreeItem* CreateItem(shared_str Name);

private:
	void RemoveMixed();
	bool IsSelect = false;
};