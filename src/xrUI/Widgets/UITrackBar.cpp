#include "StdAfx.h"

#include "UITrackBar.h"
#include "UI3tButton.h"
#include "UITextureMaster.h"
#include "UIXmlInit.h"
#include "../../xrEngine/xr_input.h"
#include "../string_table.h"
#include <sstream> // for std::ostringstream
#include <iomanip> // for std::setprecision

#define DEF_CONTROL_HEIGHT		16.0f

CUITrackBar::CUITrackBar()
	: m_f_min(0),
	  m_f_max(1),
	  m_f_val(0),
	  m_f_opt_backup_value(0),
	 m_f_step(0.01f),
	m_b_invert(false),
	m_mode(eTrackBarModeFloat),
	m_tokens(nullptr),
	m_i_num_of_signs(1),
	m_bDrawValue(false)
{	
	m_pSlider						= new CUI3tButton();
	AttachChild						(m_pSlider);
	m_pSlider->SetAutoDelete		(true);
	m_b_mouse_capturer				= false;
}

bool CUITrackBar::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	CUIWindow::OnMouseAction(x, y, mouse_action);

	switch (mouse_action)
	{
	case WINDOW_MOUSE_MOVE:
		{
			if(m_bCursorOverWindow && m_b_mouse_capturer)
			{
				if (pInput->iGetAsyncBtnState(0))
					UpdatePosRelativeToMouse();
			}
		}break;
	case WINDOW_LBUTTON_DOWN:
		{
			m_b_mouse_capturer = m_bCursorOverWindow;
			if(m_b_mouse_capturer)
				UpdatePosRelativeToMouse();
		}break;

	case WINDOW_LBUTTON_UP:
		{
			m_b_mouse_capturer = false;
		}
		break;
	case WINDOW_MOUSE_WHEEL_UP:
		{
			if (IsFltMode())
			{
				m_f_val -= GetInvert() ? -m_f_step : m_f_step;
				clamp(m_f_val, m_f_min, m_f_max);
			}
			else
			{
				if (IsIntMode())
					m_i_val -= GetInvert() ? -m_i_step : m_i_step;
				else
					m_i_val -= GetInvert() ? -1 : 1; // for bool and token it will be 1 always
				clamp(m_i_val, m_i_min, m_i_max);
			}
			GetMessageTarget()->SendMessage(this, BUTTON_CLICKED, nullptr);
			UpdatePos			();
			OnChangedOptValue	();
		}
		break;
	case WINDOW_MOUSE_WHEEL_DOWN:
		{
			if (IsFltMode())
			{
				m_f_val += GetInvert() ? -m_f_step : m_f_step;
				clamp(m_f_val, m_f_min, m_f_max);
			}
			else
			{
				if (IsIntMode())
					m_i_val += GetInvert() ? -m_i_step : m_i_step;
				else
					m_i_val += GetInvert() ? -1 : 1;  // for bool and token it will be 1 always
				clamp(m_i_val, m_i_min, m_i_max);
			}
			GetMessageTarget()->SendMessage(this, BUTTON_CLICKED, nullptr);
			UpdatePos();
			OnChangedOptValue	();
		}
		break;
	};
	return true;
}

void CUITrackBar::InitTrackBar(Fvector2 pos, Fvector2 size)
{
	const float EditBoxSize = 20.f * UI().get_current_kx();

	CUIXml xml_doc;
	xml_doc.Load			(CONFIG_PATH, UI_PATH, "backend\\trackbar.xml");

	LPCSTR nodevalue_button = xml_doc.Read("button_texture_name", 0, "ui_inGame2_opt_slider_box");
	LPCSTR nodevalue_track	= xml_doc.Read("track_texture_name", 0, "ui_inGame2_opt_slider_bar");
	float size_custom		= xml_doc.ReadFlt("size", 0, 1.0f);

	float					item_height;
	float					item_width;

	Fvector2 TrySize = size;

	InitIB(pos, TrySize);

	InitState				(S_Enabled, nodevalue_track);
	InitState				(S_Disabled, nodevalue_track);
	string128				name_button_e;
	xr_sprintf				(name_button_e, "%s%s", nodevalue_button, "_e");
	item_width				= CUITextureMaster::GetTextureWidth(name_button_e);
	item_height				= CUITextureMaster::GetTextureHeight(name_button_e);

	item_width				*= UI().get_current_kx();

	item_width				*= size_custom;
	item_height				*= size_custom;

	m_pSlider->InitButton	(Fvector2().set(0.0f, 0.0f), Fvector2().set(item_width, item_height));			//size
	m_pSlider->InitTexture	(nodevalue_button);
	m_pSlider->m_background->SetStretchTexture(xml_doc.ReadInt("stretch", 0, TRUE));

	if (m_bDrawValue)
	{
		m_pSlider->AddStatic();
		m_pSlider->SetStaticColorChanging(true);
		CUIStatic* pUIStatic = m_pSlider->GetBtnStatic();
		pUIStatic->TextItemControl()->SetTextComplexMode(false);
		pUIStatic->SetWndSize(Fvector2().set(item_width, item_height));
		pUIStatic->SetWndPos(Fvector2().set(0.f, IsTokenMode() ? item_height : 0.f));
		pUIStatic->TextItemControl()->SetTextAlignment(ETextAlignment::alCenter);
		pUIStatic->TextItemControl()->SetVTextAlignment(EVTextAlignment::valCenter);
	}

	SetCurrentState(S_Enabled);

	UpdateText();
}	

void CUITrackBar::Draw()
{
	CUI_IB_FrameLineWnd::Draw();
	m_pSlider->Draw();
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

void CUITrackBar::Update()
{
	CUIWindow::Update();

	if (m_b_mouse_capturer)
	{
		if (!pInput->iGetAsyncBtnState(0))
			m_b_mouse_capturer = false;
	}
}

void CUITrackBar::UpdateText()
{
	CUIStatic* pUIStatic = m_pSlider->GetBtnStatic();
	std::string out_str = "";
	if (pUIStatic && m_bDrawValue)
	{
		switch (m_mode)
		{
			case eTrackBarModeInt:
			{
				out_str = std::to_string(m_i_val);
			}break;
			case eTrackBarModeFloat:
			{
				out_str = FormatFloatWithStep(m_f_val, m_i_num_of_signs);
			}break;
			case eTrackBarModeToken:
			{
				xr_token* tok = GetOptToken();
				LPCSTR cur_val = get_token_name(tok, m_i_val - 1);
				out_str = *CStringTable().translate(cur_val);
			}break;
			case eTrackBarModeBool:
			{
				out_str = m_i_val == m_i_min ? *CStringTable().translate("st_track_opt_off") : *CStringTable().translate("st_track_opt_on");
			}break;
		}
		pUIStatic->SetText(out_str.c_str());
	}
}

void CUITrackBar::SetCurrentOptValue()
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

	UpdatePos			();
}

void CUITrackBar::SaveOptValue()
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

bool CUITrackBar::IsChangedOptValue() const
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

void CUITrackBar::SaveBackUpOptValue()
{
	CUIOptionsItem::SaveBackUpOptValue();

	if (IsFltMode())
		m_f_opt_backup_value		= m_f_val;
	else
		m_i_opt_backup_value		= m_i_val;
}

void CUITrackBar::UndoOptValue()
{
	if (IsFltMode())
		m_f_val			= m_f_opt_backup_value;
	else
		m_i_val			= m_i_opt_backup_value;

	UpdatePos			();
	CUIOptionsItem::UndoOptValue();
}

void CUITrackBar::SetStep(float step)
{
	if (IsFltMode())
		m_f_step	= step;
	else
		m_i_step	= iFloor(step);
}

void CUITrackBar::Enable(bool status)
{
	m_bIsEnabled				= status;
	SetCurrentState				(m_bIsEnabled?S_Enabled:S_Disabled);
	m_pSlider->Enable			(m_bIsEnabled);
}

void CUITrackBar::UpdatePosRelativeToMouse()
{
	float _bkf		= 0.0f;
	int _bki		= 0;
	if (IsFltMode())
	{
		_bkf = m_f_val;
	}
	else
	{
		_bki = m_i_val;
	}


	float btn_width				= m_pSlider->GetWidth();
	float window_width			= GetWidth();
	float fpos					= cursor_pos.x;

	if (GetInvert())
		fpos					= window_width - fpos;

	if (fpos < btn_width / 2)
		fpos = btn_width / 2;
	else if (fpos > window_width - btn_width / 2)
		fpos = window_width - btn_width/2;

	float __fval;
	float __fmax	= (IsFltMode()) ? m_f_max : (float)m_i_max;
	float __fmin	= (IsFltMode()) ? m_f_min : (float)m_i_min;
	float __fstep	= (IsFltMode()) ? m_f_step : (IsIntMode()) ? (float)m_i_step : 1.f;

	__fval		= (__fmax - __fmin) * (fpos - btn_width / 2) / (window_width - btn_width) + __fmin;

	float _d	= (__fval - __fmin);
	
	float _v	= _d / __fstep;
	int _vi		= iFloor(_v);
	float _vf	= __fstep * _vi;

	if (_d - _vf > __fstep / 2.0f)
		_vf		+= __fstep;

	__fval		= __fmin + _vf;
	
	clamp		(__fval, __fmin, __fmax);

	if (IsFltMode())
		m_f_val	= __fval;
	else
		m_i_val	= iFloor(__fval);
	

	bool b_ch = false;
	if (IsTokenMode())
	{
		b_ch = !fsimilar(_bki - 1, m_i_val - 1);
		GetMessageTarget()->SendMessage(this, TRACK_VALUE_CHANGED, &m_i_val - 1);
	}
	else
	{
		if (IsFltMode())
		{
			b_ch = !fsimilar(_bkf, m_f_val);
			GetMessageTarget()->SendMessage(this, TRACK_VALUE_CHANGED, &m_f_val);
		}
		else
		{
			b_ch = (_bki != m_i_val);
			GetMessageTarget()->SendMessage(this, TRACK_VALUE_CHANGED, &m_i_val);
		}
	}

	if (b_ch)
		GetMessageTarget()->SendMessage(this, BUTTON_CLICKED, nullptr);

	UpdatePos			();
	OnChangedOptValue	();
}

void CUITrackBar::SetTokenValues(xr_token* tokens)
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

	VERIFY(m_mode == eTrackBarModeToken);
}

void CUITrackBar::UpdatePos()
{
#ifdef DEBUG
	if (IsFltMode())
		R_ASSERT2(m_f_val >= m_f_min && m_f_val <= m_f_max, "CUITrackBar::UpdatePos() - m_val >= m_min && m_val <= m_max" );
	else
		R_ASSERT2(m_i_val >= m_i_min && m_i_val <= m_i_max, "CUITrackBar::UpdatePos() - m_val >= m_min && m_val <= m_max" );
#endif

	float btn_width				= m_pSlider->GetWidth();
	float window_width			= GetWidth();
	float free_space			= window_width - btn_width;
	Fvector2 pos				= m_pSlider->GetWndPos();
	
	float __fval	= (IsFltMode()) ? m_f_val : (float)m_i_val;
	float __fmax	= (IsFltMode()) ? m_f_max : (float)m_i_max;
	float __fmin	= (IsFltMode()) ? m_f_min : (float)m_i_min;


	pos.x						= (__fval - __fmin) * free_space / (__fmax - __fmin);
	if (GetInvert())
		pos.x					= free_space - pos.x;

	m_pSlider->SetWndPos		(pos);

	UpdateText();
}

void CUITrackBar::OnMessage(LPCSTR message)
{
	if (0 == xr_strcmp(message, "set_default_value"))
	{
		if (IsFltMode())
			m_f_val = m_f_min + (m_f_max - m_f_min) / 2.0f;
		else
			m_i_val = m_i_min + iFloor((m_i_max - m_i_min) / 2.0f);

		UpdatePos();
	}
}

bool CUITrackBar::GetCheck() const
{
	VERIFY(!IsFltMode());
	return !!m_i_val;
}

void CUITrackBar::SetCheck(bool b)
{
	VERIFY(!IsFltMode());
	m_i_val = (b) ? m_i_max : m_i_min;
}

void CUITrackBar::SetOptIBounds(int imin, int imax)
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

void CUITrackBar::SetOptFBounds(float fmin, float fmax)
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
