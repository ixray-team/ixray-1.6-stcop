#pragma once

#include "../../xrUI/Widgets/UIStatic.h"
#include "KillMessageStruct.h"

class CUIXml;
class CUIGameLog;

class CUIMoneyIndicator final : public CUIWindow
{
public:
						CUIMoneyIndicator		();
	virtual ~CUIMoneyIndicator		() override;
	virtual void 		Update					() override;
			void 		InitFromXML				(CUIXml& xml_doc);
			void 		SetMoneyAmount			(const char* money);
			void 		SetMoneyChange			(const char* money);
			void 		AddBonusMoney			(KillMessageStruct& msg);

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
	CUIStatic			m_back;
	CUIStatic			m_money_amount;
	CUIStatic			m_money_change;
	CUIGameLog*			m_pBonusMoney;
};