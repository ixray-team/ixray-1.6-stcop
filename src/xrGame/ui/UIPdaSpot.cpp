#include "StdAfx.h"
#include "UIPdaSpot.h"
#include "PdaUiSound.h"
//#include <dinput.h>
#include "Level.h"
#include "map_manager.h"
#include "map_location.h"
#include "../../xrUI/Widgets/UIEditBox.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../xrEngine/string_table.h"
#include "../../xrUI/UICursor.h"
#include "../HudPdaAnimator.h"

CUIPdaSpot::CUIPdaSpot()
{
	m_mainWnd = false;
	m_levelName = nullptr;
	m_position = Fvector();

	m_spotID = u16(-1);

	m_spotType = pSettings->read_if_exists<LPCSTR>("user_spots", "spot_type", "treasure");
	// FFx0001 override spot section from global config
	if (pGameGlobals->section_exist("pda_map") && pGameGlobals->line_exist("pda_map", "hand_spot_icon_xml_section"))
	{
		const shared_str override_spot_name = pGameGlobals->r_string("pda_map", "hand_spot_icon_xml_section");

		if (override_spot_name.c_str() != nullptr) {
			m_spotType = override_spot_name;
		}
	}

	last_cursor_pos.set(UI_BASE_WIDTH / 2.f, UI_BASE_HEIGHT / 2.f);
	InitControls();
}

CUIPdaSpot::~CUIPdaSpot()
{
}

void CUIPdaSpot::Init(u16 spot_id, const char* level_name, Fvector pos, bool main_wnd)
{
	m_mainWnd = main_wnd;
	m_levelName = level_name;
	m_position = pos;

	m_spotID = !m_mainWnd ? spot_id : u16(-1);

	if (!m_mainWnd)
	{
		CMapLocation* ml = Level().MapManager().GetMapLocation(m_spotType, m_spotID);
		if (!ml) return;
		m_editBox->SetText(ml->GetHint());
		ml->HighlightSpot(true, Fcolor().set(255.f, 36.f, 0.f, 255.f));
	}
}

void CUIPdaSpot::InitControls()
{
	SetWndPos(Fvector2().set(0.0f, 0.0f));
	SetWndSize(Fvector2().set(UI_BASE_WIDTH, UI_BASE_HEIGHT));

	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, "pda_spot.xml");

	m_background = UIHelper::CreateStatic(uiXml, "background", this);
	m_editBox = UIHelper::CreateEditBox(uiXml, "spot_name_edit", this);

	m_btn_ok = UIHelper::Create3tButton(uiXml, "btn_apply", this);
	Register(m_btn_ok);
	AddCallback(m_btn_ok, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIPdaSpot::OnApply));

	m_btn_cancel = UIHelper::Create3tButton(uiXml, "btn_cancel", this);
	Register(m_btn_cancel);
	AddCallback(m_btn_cancel, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIPdaSpot::OnExit));
}

void CUIPdaSpot::OnAdd(CUIWindow* ui, void* d)
{
	CMapLocation* ml = Level().MapManager().AddUserLocation(m_spotType, m_levelName, m_position);
	ml->SetHint(m_editBox->GetText());
	ml->SetSerializable(true);

	Exit();
}

void CUIPdaSpot::OnApply(CUIWindow* ui, void* d)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::Confirm);
	}

	if (m_mainWnd)
	{
		OnAdd(ui, d);
		return;
	}

	CMapLocation* ml = Level().MapManager().GetMapLocation(m_spotType, m_spotID);
	if (!ml)
		return;

	if (m_editBox->GetText() != ml->GetHint())
		ml->SetHint(m_editBox->GetText());

	Exit();
}

void CUIPdaSpot::OnExit(CUIWindow* w, void* d)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::Cancel);
	}
	Exit();
}

void CUIPdaSpot::Exit()
{
	if (!m_mainWnd)
	{
		CMapLocation* ml = Level().MapManager().GetMapLocation(m_spotType, m_spotID);
		if (!ml) return;
		ml->HighlightSpot(false, Fcolor().set(0.f, 0.f, 0.f, 0.f));
	}

	m_mainWnd = false;
	m_levelName = nullptr;
	m_position = Fvector();
	m_spotID = u16(-1);

	m_editBox->ClearText();

	HideDialog();
}

bool CUIPdaSpot::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	switch (dik)
	{
	case SDL_SCANCODE_ESCAPE:
	{
		if (IsShown())
		{
			Exit();
			return true;
		}
	}break;
	}

	return base_class::OnKeyboardAction(dik, keyboard_action);
}

void CUIPdaSpot::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

static u32 pda_spot_render_frame = 0;

void CUIPdaSpot::Draw()
{
	if (pda_spot_render_frame == Device.dwFrame)
	{
		return;
	}

	pda_spot_render_frame = Device.dwFrame;

	base_class::Draw();
}

void CUIPdaSpot::ResetCursor()
{
	if (!last_cursor_pos.similar({0.f, 0.f}))
	{
		GetUICursor().SetUICursorPosition(last_cursor_pos);
	}
}

bool CUIPdaSpot::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	CObject* current_entity = Level().CurrentControlEntity();
	CHudPdaAnimator* pda_animator = current_entity != nullptr ? current_entity->cast_actor()->HudAnimator()->PdaAnimator() : nullptr;
	if (pda_animator != nullptr)
	{
		pda_animator->OnMouseAction(x, y, mouse_action);
	}
	return base_class::OnMouseAction(x, y, mouse_action);
}
