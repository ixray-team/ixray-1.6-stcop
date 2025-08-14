#pragma once
class XREPROPS_API UIPropertiesForm :
	public IEditorWnd
{
	friend class UIPropertiesItem;
	xr_atomic_bool bAsyncUpdated = true;

public:
	UIPropertiesForm();
	virtual ~UIPropertiesForm();
	virtual void Draw();
	void AssignItems(PropItemVec& items);
	PropItem* FindItem(const char* path);
	UIPropertiesItem* FindPropItem(const char* path);
	PropItem* FindItemOfName(shared_str name);
	void ClearProperties();
	IC void SetReadOnly(bool enable) { m_Flags.set(plReadOnly, enable); }
	IC bool IsModified() { return m_bModified; }

	void setModified(bool val)
	{
		m_bModified = val;
	}

	IC bool Empty() { return m_Items.size() == 0; }
	void SetModifiedEvent(TOnModifiedEvent modif = 0) { OnModifiedEvent = modif; }
	void SetFitMode(bool Mode) { IsFitMode = Mode; }

public:
	void AssignItemsAsync(PropItemVec items);

	enum
	{
		plReadOnly = (1 << 0),
	};
	Flags32 m_Flags;
	IC bool IsReadOnly()const { return m_Flags.is(plReadOnly); }

private:
	PropItemVec m_Items;
	PropItem* m_EditChooseValue;
	PropItem* m_EditTextureValue;
	PropItem* m_EditShortcutValue;
	TOnModifiedEvent OnModifiedEvent;
	UIPropertiesItem m_Root;

	PropItem* m_EditTextValue = nullptr;
	PropItem* m_EditGameTypeValue;
	char* m_EditTextValueData;
	int m_EditTextValueDataSize;
	GameTypeChooser m_EditGameTypeChooser;
	bool m_bModified;
	bool IsFitMode = false;

private:
	void DrawEditText();
	int  DrawEditText_Callback(ImGuiInputTextCallbackData* data);
	void DrawEditGameType();
	void Modified() { m_bModified = true; if (!OnModifiedEvent.empty()) OnModifiedEvent(); /* m_bModified = false; */ }
};