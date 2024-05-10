#pragma once
class XREPROPS_API UIPropertiesForm :public XrUI
{
	friend class UIPropertiesItem;
public:
	UIPropertiesForm();
	virtual ~UIPropertiesForm();
	virtual void Draw();
	void AssignItems(PropItemVec& items);
	PropItem* FindItem(const char* path);
	PropItem* FindItemOfName(shared_str name);
	void ClearProperties();
	IC void SetReadOnly(bool enable) { m_Flags.set(plReadOnly, enable); }
	IC bool IsModified() { return m_bModified;}
	IC bool Empty() { return m_Items.size() == 0; }
	void SetModifiedEvent(TOnModifiedEvent modif = 0) { OnModifiedEvent = modif; }
public:
	enum {
		plReadOnly = (1 << 0),
	};
	Flags32 m_Flags;
	IC bool IsReadOnly()const { return m_Flags.is(plReadOnly); }
private:
	PropItemVec m_Items;
	PropItem* m_EditChooseValue;
	PropItem* m_EditTextureValue;
	PropItem* m_EditShortcutValue;
private:
	TOnModifiedEvent 	OnModifiedEvent;
private:
	PropItem* m_EditTextValue;
	char* m_EditTextValueData;
	int m_EditTextValueDataSize;
	void DrawEditText();
	int  DrawEditText_Callback(ImGuiInputTextCallbackData* data);
private:
	GameTypeChooser m_EditGameTypeChooser;
	PropItem* m_EditGameTypeValue;
	void DrawEditGameType();
	bool m_bModified;
	void Modified() { m_bModified = true; if (!OnModifiedEvent.empty()) OnModifiedEvent(); }
private:
	UIPropertiesItem m_Root;

/*	virtual void DrawNode(Node* pNode);
	virtual void DrawItem(Node* pNode);
	virtual void DrawItem(const char*name,PropItem* Node);
	virtual bool IsDrawFloder(Node* Node);
	virtual void DrawAfterFloderNode(bool is_open, Node* Node = 0);
private:
	
	Node m_GeneralNode;


private:
	void RemoveMixed(Node* Node);*/
};

