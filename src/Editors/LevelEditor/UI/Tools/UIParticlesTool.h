#pragma once
class UIParticlesTool :public UIToolCustom
{
public:
	UIParticlesTool();
	virtual ~UIParticlesTool();
	virtual void Draw();

	void DrawObjectsList();

	IC const char* Current() const
	{
		return m_Current.empty() ? nullptr : m_Current.c_str();
	}
private:
	void SelByRef(bool flag);
	void OnItemFocused(ListItem* item);
	UIItemListForm* m_ParticlesList;
	xr_string m_Current;
};
