#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIXml;
class CUIStatic;
class UIHint;
class CUIScrollView;
class CUIStackPanel;

class CUIAchievements final :
	public CUIWindow
{
	typedef CUIWindow inherited;

	enum class ELayout : u8
	{
		Legacy = 0,
		SingleStack = 1,
		NestedStack = 2,
	};

public:
	struct SLayoutDefaults final
	{
		float iconWidth = 0.f;
		float iconHeight = 0.f;
		float nameWidth = 0.f;
		float nameHeight = 0.f;
		float descrWidth = 0.f;
		float descrHeight = 0.f;
		float hintWidth = 0.f;
		float hintHeight = 0.f;
	};

private:
	CUIScrollView* m_parent = nullptr;
	CUIStatic* m_name = nullptr;
	CUIStatic* m_descr = nullptr;
	CUIStatic* m_icon = nullptr;
	UIHint* m_hint = nullptr;
	CUIStackPanel* m_itemStack = nullptr;
	CUIStackPanel* m_rowStack = nullptr;
	CUIStackPanel* m_textStack = nullptr;
	ELayout m_layout = ELayout::Legacy;
	float m_itemStackSpacing = 0.f;
	float m_rowStackSpacing = 0.f;
	float m_textStackSpacing = 0.f;
	SLayoutDefaults m_layoutDefaults = {};
	string128 m_functor_str = {};
	bool m_repeat = false;

public:
	CUIAchievements(CUIScrollView* parent);
	virtual ~CUIAchievements();

	void init_from_xml(CUIXml& xml);
	void Update();

	void SetName(const char* name);
	void SetDescription(const char* desc);
	void SetHint(const char* hint);
	void SetIcon(const char* icon);
	void SetFunctor(const char* func);
	void SetRepeatable(bool repeat);

	virtual void DrawHint();
	virtual void Reset();

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	bool ParentHasMe();

private:
	void initLegacyFromXml(CUIXml& xml);
	bool initStackedFromXml(CUIXml& xml);
	void createAchievementStatics(CUIXml& xml, CUIWindow* nameParent, CUIWindow* descrParent, CUIWindow* iconParent);
	void applyLayoutDefaults(CUIXml& xml);
	void applyStaticLayoutSize(CUIXml& xml, const char* path, CUIStatic* wnd, float defaultWidth, float defaultHeight);
	void applyHintLayoutSize(CUIXml& xml);
	void updateStackedLayoutHeight();
	float readStackSpacing(CUIXml& xml, const char* path) const;
};
