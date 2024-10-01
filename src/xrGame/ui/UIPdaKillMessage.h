#pragma once

#include "KillMessageStruct.h"
#include "../../xrUI/Widgets/UIStatic.h"

class CUIPdaKillMessage : public CUIColorAnimConrollerContainer 
{
	typedef CUIColorAnimConrollerContainer inherited;
public:
				CUIPdaKillMessage	();

			void Init				(KillMessageStruct& msg, CGameFont* F);
protected:
			float InitText(CUITextWnd& refStatic, float x, PlayerInfo& info);
			float InitIcon(CUIStatic& refStatic, float x, IconInfo& info);

    CUITextWnd	m_victim_name;
	CUIStatic	m_initiator;
	CUITextWnd	m_killer_name;
	CUIStatic	m_ext_info;
};
