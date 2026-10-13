#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../EntityCondition.h"
#include "UIItemParam.h"

class CUIXml;
class CUIStatic;
class UIBoosterInfoItem;

class CUIBoosterInfo final : public CUIWindow
{
public:
					CUIBoosterInfo		();
	virtual			~CUIBoosterInfo		();
			void	InitFromXml				(CUIXml& xml);
			void	SetInfo					(const shared_str& section);

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	UIBoosterInfoItem*	m_booster_items[eBoostExplImmunity];
	UIBoosterInfoItem*	m_booster_satiety;
	UIBoosterInfoItem*	m_booster_thirst;
	UIBoosterInfoItem*	m_booster_sleepiness;
	UIBoosterInfoItem*	m_booster_intoxication;
	UIBoosterInfoItem*	m_booster_anabiotic;
	UIBoosterInfoItem*	m_booster_time;

	CUIStatic*			m_Prop_line = nullptr;

}; // class CUIBoosterInfo

// -----------------------------------

class UIBoosterInfoItem final : public UIItemParam
{
public:
	UIBoosterInfoItem() = default;
	~UIBoosterInfoItem() override = default;

	void Init(CUIXml& Xml, const char* Section)
	{
		UIItemParam::Init(Xml, Section);
	}

	void SetValue(float Value)
	{
		UIItemParam::SetValue(Value, false);
	}
};
