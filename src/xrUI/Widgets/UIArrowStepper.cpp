////////////////////////////////////////////////////////////////////////////
//	Module 		: UIArrowStepper.cpp
//	Created 	: 29.01.2025
//	Modified 	: 02.06.2025
//	IXRay port	: 02.06.2025
//	Author		: Konstantin Tarasov
//	Description : Element for step-by-step value adjustment using arrows or direct clicks
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"

#include "UIArrowStepper.h"
#include "UI3tButton.h"
#include "UITextureMaster.h"
#include "UIXmlInit.h"
#include "UIFontDefines.h"
#include "UI_IB_Static.h"
#include "UIInteractiveBackground.h"
#include "../../xrEngine/xr_input.h"
#include "../string_table.h"
#include <sstream> // for std::ostringstream
#include <iomanip> // for std::setprecision

#define DEF_CONTROL_HEIGHT		16.0f

CUIArrowStepper::CUIArrowStepper()
	: m_f_min(0),
	  m_f_max(1),
	  m_f_val(0),
	  m_f_opt_backup_value(0),
	 m_f_step(0.01f),
	m_b_invert(false),
	m_mode(eStepperModeFloat),
	m_tokens(nullptr),
	m_i_num_of_signs(1)
{	
	m_TextVal						= nullptr;
	m_FrameLine						= nullptr;
	m_LeftBtn						= nullptr;
	m_RightBtn						= nullptr;
	m_b_mouse_capturer				= false;
}

bool CUIArrowStepper::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	CUIWindow::OnMouseAction(x, y, mouse_action);

	switch (mouse_action)
	{
		case WINDOW_MOUSE_WHEEL_DOWN:
		case WINDOW_LBUTTON_DOWN:
		{
			if (m_FrameLine && m_FrameLine->CursorOverWindow())
			{
				ChangeValue(true);
			}break;
		}

		case WINDOW_MOUSE_WHEEL_UP:
		case WINDOW_RBUTTON_DOWN:
		{
			if (m_FrameLine && m_FrameLine->CursorOverWindow())
			{
				ChangeValue(false);
			}break;
		}
	}
	return true;
}

void CUIArrowStepper::InitArrowStepper(Fvector2 pos, Fvector2 size)
{
	CUIXml xml_doc;
	xml_doc.Load					(CONFIG_PATH, UI_PATH, "backend\\arrowstepper.xml");

	LPCSTR nodevalue_track			= xml_doc.Read("track_texture_name", 0, "ui_inGame2_opt_slider_bar");
	LPCSTR left_btn_texture_name	= xml_doc.Read("left_btn_texture_name", 0, "ui_date_bt_left");
	LPCSTR right_btn_texture_name	= xml_doc.Read("right_btn_texture_name", 0, "ui_date_bt_right");

	float							item_height;
	float							item_width;

	Fvector2 TrySize				= size;
	float ib_pos					= xml_doc.ReadFlt("ib_pos", 0, 30.0f);
	TrySize.x						-= ib_pos;
	m_FrameLine						= new CUI_IB_FrameLineWnd();
	m_FrameLine->InitIB				(Fvector2().set(ib_pos / 2.f, pos.y), TrySize);

	m_FrameLine->InitState			(S_Enabled, nodevalue_track);
	m_FrameLine->InitState			(S_Disabled, nodevalue_track);
	AttachChild						(m_FrameLine);

	m_LeftBtn						= new CUI3tButton();
	AttachChild						(m_LeftBtn);
	m_LeftBtn->InitButton			(Fvector2().set(0.f, 0.f), Fvector2().set(ib_pos / 2.f, GetHeight()));
	m_LeftBtn->InitTexture			(left_btn_texture_name);
//	m_LeftBtn->SetStretch			(true);

	m_RightBtn						= new CUI3tButton();
	AttachChild						(m_RightBtn);
	m_RightBtn->InitButton			(Fvector2().set(TrySize.x + (ib_pos / 2.f), 0.f), Fvector2().set(ib_pos / 2.f, GetHeight()));
	m_RightBtn->InitTexture			(right_btn_texture_name);
//	m_RightBtn->SetStretch			(true);
	m_TextVal						= new CUIStatic();
	AttachChild						(m_TextVal);
	m_TextVal->SetWndPos(Fvector2().set(0.f, 0.f));
	m_TextVal->SetWndSize(Fvector2().set(size.x, size.y));
	m_TextVal->TextItemControl()->SetTextAlignment		(CGameFont::alCenter);
	m_TextVal->TextItemControl()->SetVTextAlignment	(valCenter);
	CGameFont* static_font			= UI().Font().GetFont(LETTERICA16_FONT_NAME);
	u32 font_color					= color_rgba(255, 255, 255, 255);
	CUIXmlInit::InitFont			(xml_doc, "static_font", 0, font_color, static_font);
	m_TextVal->SetFont				(static_font);
	m_TextVal->TextItemControl()->SetTextColor(font_color);
	m_TextVal->SetAutoDelete		(true);

	m_FrameLine->SetCurrentState(S_Enabled);

	UpdateText();
}	

void CUIArrowStepper::Draw()
{
	m_FrameLine->Draw();
	for (WINDOW_LIST_it it = m_ChildWndList.begin(); m_ChildWndList.end() != it; ++it)
	{
		if ((*it))
			(*it)->Draw();
	}
}

void CUIArrowStepper::Show(bool status)
{
	if (status)
		UpdateText();
}

void CUIArrowStepper::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWindow::SendMessage(pWnd, msg, pData);

	switch (msg)
	{
	case BUTTON_CLICKED:
	{
		if (m_LeftBtn && m_LeftBtn->CursorOverWindow())
		{
			ChangeValue(false);
		}
		else if (m_RightBtn && m_RightBtn->CursorOverWindow())
		{
			ChangeValue(true);
		}break;
	}
	default: break;
	}
}

// ‘орматирование текущего значени€
static std::string FormatFloatWithStep(float value, int num_of_signs)
{
	// ¬ычисл€ем множитель на основе количества знаков после зап€той
	float multiplier = std::pow(10.0f, num_of_signs);

	// ќкругл€ем значение с учетом заданной точности
	float rounded_value = std::round(value * multiplier) / multiplier;

	// ѕровер€ем, €вл€етс€ ли округлЄнное значение целым
	if (std::fabs(std::floor(rounded_value) - rounded_value) < 0.00001f)
	{
		return std::to_string(static_cast<int>(rounded_value)); // ѕреобразуем в строку как целое число
	}

	// ≈сли дробна€ часть есть, форматируем с указанной точностью
	std::ostringstream oss;
	oss << std::fixed << std::setprecision(num_of_signs) << rounded_value;
	return oss.str();
}

void CUIArrowStepper::Update()
{
	CUIWindow::Update();

	if (m_b_mouse_capturer)
	{
		if (!pInput->iGetAsyncBtnState(0))
			m_b_mouse_capturer = false;
	}
}

void CUIArrowStepper::UpdateText()
{
	std::string out_str = "";
	switch (m_mode)
	{
		case eStepperModeInt:
		{
			out_str = std::to_string(m_i_val);
		}break;
		case eStepperModeFloat:
		{
			out_str = FormatFloatWithStep(m_f_val, m_i_num_of_signs);
		}break;
		case eStepperModeToken:
		{
			xr_token* tok = GetOptToken();
			LPCSTR cur_val = get_token_name(tok, m_i_val - 1);
			out_str = *CStringTable().translate(cur_val);
		}break;
		case eStepperModeBool:
		{
			out_str = m_i_val == m_i_min ? *CStringTable().translate("st_track_opt_off") : *CStringTable().translate("st_track_opt_on");
		}break;
	}
	m_TextVal->SetText(out_str.c_str());
}

void CUIArrowStepper::SetCurrentOptValue()
{
	CUIOptionsItem::SetCurrentOptValue();
	if (IsTokenMode())
	{
		LPCSTR val = GetOptStringValue();
		for (xr_token* tok = GetOptToken(); tok->name; ++tok)
		{
			if (stricmp(tok->name, val) == 0)
			{
				m_i_val = tok->id + 1;
				break;
			}
		}
	}
	else
	{
		if (IsFltMode())
		{
			float minn = 0.f;
			float maxx = 0.f;

			GetOptFloatValue(m_f_val, minn, maxx);

			m_f_min = minn;
			m_f_max = maxx;

			// clamp current value to bounds
			clamp(m_f_val, m_f_min, m_f_max);
		}
		else // for bool and int mode it will be the same
		{
			int minn = 0;
			int maxx = 0;

			GetOptIntegerValue(m_i_val, minn, maxx);

			m_i_min = minn;
			m_i_max = maxx;

			// clamp current value to bounds
			clamp(m_i_val, m_i_min, m_i_max);
		}
	}

	UpdateText			();
}

void CUIArrowStepper::SaveOptValue()
{
	CUIOptionsItem::SaveOptValue	();
	if (IsTokenMode())
	{
		if (strcmp("not_an_option", GetEntry()))
		{
			xr_token* tok = GetOptToken();
			LPCSTR cur_val = get_token_name(tok, m_i_val - 1);
			SaveOptStringValue(cur_val);
		}
	}
	else
	{
		if (IsFltMode())
			SaveOptFloatValue(m_f_val);
		else
			SaveOptIntegerValue(m_i_val);
	}
}

bool CUIArrowStepper::IsChangedOptValue() const
{
	if (IsFltMode())
	{
		return !fsimilar(m_f_opt_backup_value, m_f_val);
	}
	else
	{
		return (m_i_opt_backup_value != m_i_val);
	}
}

void CUIArrowStepper::SaveBackUpOptValue()
{
	CUIOptionsItem::SaveBackUpOptValue();

	if (IsFltMode())
		m_f_opt_backup_value		= m_f_val;
	else
		m_i_opt_backup_value		= m_i_val;
}

void CUIArrowStepper::UndoOptValue()
{
	if (IsFltMode())
		m_f_val			= m_f_opt_backup_value;
	else
		m_i_val			= m_i_opt_backup_value;

	UpdateText			();
	CUIOptionsItem::UndoOptValue();
}

void CUIArrowStepper::SetStep(float step)
{
	if (IsFltMode())
		m_f_step	= step;
	else
		m_i_step	= iFloor(step);
}

void CUIArrowStepper::Enable(bool status)
{
	m_bIsEnabled				= status;
	m_FrameLine->SetCurrentState(m_bIsEnabled ? S_Enabled : S_Disabled);
	m_TextVal->Enable			(m_bIsEnabled);
}

void CUIArrowStepper::SetTokenValues(xr_token* tokens)
{
	m_tokens = tokens;

	int count = 0;
	if (m_tokens)
	{
		for (xr_token* tok = m_tokens; tok->name; ++tok)
		{
			++count;
		}
	}

	if (count > 0)
	{
		m_i_min = 1;
		m_i_max = count;
	}

	LPCSTR current_value = GetOptStringValue();
	m_i_val = m_i_min;

	if (!GetInvert())
	{
		for (int i = 0; i < count; ++i)
		{
			if (stricmp(m_tokens[i].name, current_value) == 0)
			{
				m_i_val = i + 1;
				break;
			}
		}
	}
	else
	{
		for (int i = count - 1; i >= 0; --i)
		{
			if (stricmp(m_tokens[i].name, current_value) == 0)
			{
				m_i_val = i + 1;
				break;
			}
		}
	}

	clamp(m_i_val, m_i_min, m_i_max);

	VERIFY(m_mode == eStepperModeToken);
}

void CUIArrowStepper::OnMessage(LPCSTR message)
{
	if (0 == xr_strcmp(message, "set_default_value"))
	{
		if (IsFltMode())
			m_f_val = m_f_min + (m_f_max - m_f_min) / 2.0f;
		else
			m_i_val = m_i_min + iFloor((m_i_max - m_i_min) / 2.0f);

		UpdateText();
	}
}

bool CUIArrowStepper::GetCheck() const
{
	VERIFY(!IsFltMode());
	return !!m_i_val;
}

void CUIArrowStepper::SetCheck(bool b)
{
	VERIFY(!IsFltMode());
	m_i_val = (b) ? m_i_max : m_i_min;
}

void CUIArrowStepper::SetOptIBounds(int imin, int imax)
{
	m_i_min					= imin;
	m_i_max					= imax;
	if (m_i_val < m_i_min || m_i_val > m_i_max)
	{
		clamp					(m_i_val, m_i_min, m_i_max);
		OnChangedOptValue	();
		GetMessageTarget()->SendMessage(this, TRACK_VALUE_CHANGED, &m_i_val);
	}
}

void CUIArrowStepper::SetOptFBounds(float fmin, float fmax)
{
	m_f_min					= fmin;
	m_f_max					= fmax;
	if (m_f_val < m_f_min || m_f_val > m_f_max)
	{
		clamp				(m_f_val, m_f_min, m_f_max);
		OnChangedOptValue	();
		GetMessageTarget()->SendMessage(this, TRACK_VALUE_CHANGED, &m_f_val);
	}
}

void CUIArrowStepper::ChangeValue(bool bAdd)
{
	if (IsFltMode())
	{
		if (bAdd)
		{
			m_f_val += GetInvert() ? -m_f_step : m_f_step;
		}
		else
		{
			m_f_val -= GetInvert() ? -m_f_step : m_f_step;
		}
	}
	else
	{
		if (bAdd)
		{
			if (IsIntMode())
				m_i_val += GetInvert() ? -m_i_step : m_i_step;
			else
				m_i_val += GetInvert() ? -1 : 1;
		}
		else
		{
			if (IsIntMode())
				m_i_val -= GetInvert() ? -m_i_step : m_i_step;
			else
				m_i_val -= GetInvert() ? -1 : 1;
		}
	}
	ChangeOnEnd(bAdd);
	UpdateText();
}

void CUIArrowStepper::ChangeOnEnd(bool bRight)
{
	if (IsFltMode())
	{
		if (bRight)
		{
			if (m_f_val > m_f_max)
				m_f_val = m_f_min;
		}
		else
		{
			if (m_f_val < m_f_min)
				m_f_val = m_f_max;
		}
	}
	else
	{
		if (IsTokenMode())
		{
			if (!GetInvert())
			{
				if (bRight)
				{
					if (m_i_val > m_i_max)
						m_i_val = m_i_min;
				}
				else
				{
					if (m_i_val < m_i_min)
						m_i_val = m_i_max;
				}
			}
			else
			{
				if (bRight)
				{
					if (m_i_val < m_i_min)
						m_i_val = m_i_max;
				}
				else
				{
					if (m_i_val > m_i_max)
						m_i_val = m_i_min;
				}
			}
		}
		else
		{
			if (bRight)
			{
				if (m_i_val > m_i_max)
					m_i_val = m_i_min;
			}
			else
			{
				if (m_i_val < m_i_min)
					m_i_val = m_i_max;
			}
		}
	}
}
