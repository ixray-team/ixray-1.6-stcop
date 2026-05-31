#include "StdAfx.h"
#include "UIMainIngameWnd.h"
#include "UIMotionIcon.h"
#include "../../xrCore/_color.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrEngine/CustomHUD.h"
#include "../game_cl_single.h"

const char* MOTION_ICON_XML = "motion_icon.xml";
static const float OVERLAY_LUMINOSITY_SMOOTH_SPEED = 4.5f;
static const float OVERLAY_NOISE_SMOOTH_SPEED = 5.5f;

CUIMotionIcon* g_pMotionIcon = nullptr;

CUIMotionIcon::CUIMotionIcon()
{
	m_current_state = stLast;
	g_pMotionIcon	= this;
	m_bchanged		= true;
	m_luminosity	= 0.0f;
	m_cur_pos		= 0.0f;

	m_power_progress = nullptr;
	m_luminosity_progress_bar = nullptr;
	m_noise_progress_bar = nullptr;
	m_luminosity_progress_shape = nullptr;
	m_noise_progress_shape = nullptr;
	_luminosityOverlay = nullptr;
	_noiseOverlay = nullptr;
	_luminosityOverlayBaseColor = 0;
	_noiseOverlayBaseColor = 0;
	_luminosityNormalized = 0.f;
	_noiseNormalized = 0.f;
	_luminosityOverlayCur = 0.f;
	_noiseOverlayCur = 0.f;
}

CUIMotionIcon::~CUIMotionIcon()
{
	g_pMotionIcon	= nullptr;
}

void CUIMotionIcon::ResetVisibility()
{
	m_npc_visibility.clear	();
	m_luminosity			= 0.0f;
	m_bchanged				= true;
	_luminosityOverlayCur	= 0.f;
	_noiseOverlayCur		= 0.f;
}

bool CUIMotionIcon::Init(Frect const& zonemap_rect, bool useCompassBar)
{
	CUIXml						uiXml;
	uiXml.Load					(CONFIG_PATH, UI_PATH, MOTION_ICON_XML);

	CUIXmlInit					xml_init;

	if (uiXml.NavigateToNode("window", 0))
	{
		xml_init.InitWindow(uiXml, "window", 0, this);
	}
	else
	{
		m_independent = xml_init.InitStatic(uiXml, "background", 0, this);
	}

	Fvector2					sz;
	Fvector2					pos;

    if (!m_independent)
    {
        const float rel_sz = uiXml.ReadAttribFlt("window", 0, "rel_size", 1.0f);

        zonemap_rect.getsize(sz);
        pos.set(sz.x / 2.0f, sz.y / 2.0f);

        SetWndSize(sz);
        SetWndPos(pos);

        float k = UI().get_current_kx();
        sz.mul(rel_sz * k);
    }

    if (uiXml.NavigateToNode("power_progress", 0))
        m_power_progress = UIHelper::CreateProgressBar(uiXml, "power_progress", this);

    bool useLuminosityOverlay = uiXml.NavigateToNode("luminosity_overlay", 0);
    bool useNoiseOverlay = uiXml.NavigateToNode("noise_overlay", 0);

    if (m_independent)
    {
        if (!useLuminosityOverlay)
            m_luminosity_progress_bar = UIHelper::CreateProgressBar(uiXml, "luminosity_progress", this);
        if (!useNoiseOverlay)
            m_noise_progress_bar = UIHelper::CreateProgressBar(uiXml, "noise_progress", this);
    }
    else if (!useCompassBar)
    {
        if (!useLuminosityOverlay && !m_luminosity_progress_bar)
        {
            m_luminosity_progress_shape = UIHelper::CreateProgressShape(uiXml, "luminosity_progress", this);
            if (m_luminosity_progress_shape)
            {
                m_luminosity_progress_shape->SetWndSize(sz);
                m_luminosity_progress_shape->SetWndPos(pos);
            }
        }
        if (!useNoiseOverlay && !m_noise_progress_bar)
        {
            m_noise_progress_shape = UIHelper::CreateProgressShape(uiXml, "noise_progress", this);
            if (m_noise_progress_shape)
            {
                m_noise_progress_shape->SetWndSize(sz);
                m_noise_progress_shape->SetWndPos(pos);
            }
        }
    }
    CUIStatic* state = nullptr;

    if (uiXml.NavigateToNode("state_normal", 0))
    {
        state = UIHelper::CreateStatic(uiXml, "state_normal", this);
        m_states[stNormal] = state;
        state->Show(false);
    }

    if (uiXml.NavigateToNode("state_crouch", 0))
    {
        state = UIHelper::CreateStatic(uiXml, "state_crouch", this);
        m_states[stCrouch] = state;
        state->Show(false);
    }

    if (uiXml.NavigateToNode("state_creep", 0))
    {
        state = UIHelper::CreateStatic(uiXml, "state_creep", this);
        m_states[stCreep] = state;
        state->Show(false);
    }

    if (uiXml.NavigateToNode("state_climb", 0))
    {
        state = UIHelper::CreateStatic(uiXml, "state_climb", this);
        m_states[stClimb] = state;
        state->Show(false);
    }

    if (uiXml.NavigateToNode("state_run", 0))
    { 
        state = UIHelper::CreateStatic(uiXml, "state_run", this);
        m_states[stRun] = state;
        state->Show(false);
    }

    if (uiXml.NavigateToNode("state_sprint", 0))
    {
		state = UIHelper::CreateStatic(uiXml, "state_sprint", this);
        m_states[stSprint] = state;
        state->Show(false);
    }

    ShowState(stNormal);

    if (useLuminosityOverlay && !useCompassBar)
    {
        _luminosityOverlay = UIHelper::CreateStatic(uiXml, "luminosity_overlay", this, false);
        if (_luminosityOverlay)
        {
            _luminosityOverlayBaseColor = _luminosityOverlay->GetTextureColor();
            _luminosityOverlay->SetTextureColor(subst_alpha(_luminosityOverlayBaseColor, 0));
        }
    }
    if (useNoiseOverlay && !useCompassBar)
    {
        _noiseOverlay = UIHelper::CreateStatic(uiXml, "noise_overlay", this, false);
        if (_noiseOverlay)
        {
            _noiseOverlayBaseColor = _noiseOverlay->GetTextureColor();
            _noiseOverlay->SetTextureColor(subst_alpha(_noiseOverlayBaseColor, 0));
        }
    }

    return m_independent;
}

void CUIMotionIcon::ShowState(EState state)
{
	if (m_current_state == state)
		return;

	if (m_current_state != stLast)
	{
		CUIStatic* curState = m_states[m_current_state];
		if (curState)
		{
			curState->Show(false);
			curState->Enable(false);
		}
	}
	CUIStatic* newState = m_states[state];
	if (newState)
	{
		newState->Show(true);
		newState->Enable(true);
	}

	m_current_state = state;
}

void CUIMotionIcon::SetPower(float newPos)
{
	if (m_power_progress)
		m_power_progress->SetProgressPos(newPos);
}

void CUIMotionIcon::SetNoise(float newPos)
{
	if (!IsGameTypeSingleCompatible())
		return;

	if (m_noise_progress_shape)
	{
		float pos = newPos;
		pos = clampr(pos, 0.f, 100.f);
		m_noise_progress_shape->SetPos(pos / 100.f);
		_noiseNormalized = pos / 100.f;
	}
	else if (m_noise_progress_bar)
	{
		float pos = newPos;
		float rmin = m_noise_progress_bar->GetRange_min();
		float rmax = m_noise_progress_bar->GetRange_max();
		pos = clampr(pos, rmin, rmax);
		m_noise_progress_bar->SetProgressPos(pos);
		_noiseNormalized = (rmax > rmin) ? (pos - rmin) / (rmax - rmin) : 0.f;
	}
	else
	{
		_noiseNormalized = clampr(newPos / 100.f, 0.f, 1.f);
	}
}

void CUIMotionIcon::SetLuminosity(float newPos)
{
	if (!IsGameTypeSingleCompatible())
		return;

	if (m_luminosity_progress_shape)
	{
		m_luminosity = newPos;
	}
	else if (m_luminosity_progress_bar)
	{
		newPos = clampr(newPos, m_luminosity_progress_bar->GetRange_min(), m_luminosity_progress_bar->GetRange_max());
		m_luminosity = newPos;
	}
	if (_luminosityOverlay != nullptr && m_luminosity_progress_shape == nullptr && m_luminosity_progress_bar == nullptr)
		_luminosityNormalized = clampr(newPos, 0.f, 1.f);
}

float CUIMotionIcon::GetLuminosityNormalized() const
{
	if (_luminosityOverlay != nullptr)
	{
		return _luminosityOverlayCur;
	}

	if (m_luminosity_progress_shape)
	{
		return m_cur_pos / 100.f;
	}

	if (m_luminosity_progress_bar)
	{
		const float rmin = m_luminosity_progress_bar->GetRange_min();
		const float rmax = m_luminosity_progress_bar->GetRange_max();
		return (rmax > rmin) ? (m_cur_pos - rmin) / (rmax - rmin) : 0.f;
	}

	return 0.f;
}

void CUIMotionIcon::Draw()
{
	const static bool disableMotionIcon = EngineExternal()[EEngineExternalUI::DisableMotionIcon];
	const static bool noHUDonMaster = EngineExternal()[EEngineExternalUI::DisableHudRenderingOnMaster];
	bool renderHUD = noHUDonMaster ? g_SingleGameDifficulty < egdVeteran : true;
	bool showMotionIcon = m_independent ? true : psHUD_Flags.test(HUD_MINIMAP);
	if (!disableMotionIcon && renderHUD && showMotionIcon)
		inherited::Draw();
}

void CUIMotionIcon::Update()
{
	if (!IsGameTypeSingleCompatible())
	{
		inherited::Update();
		return;
	}
	if (m_bchanged)
	{
		m_bchanged = false;
		if (!m_npc_visibility.empty())
		{
			std::sort(m_npc_visibility.begin(), m_npc_visibility.end());
			SetLuminosity(m_npc_visibility.back().value);
		}
		else
			SetLuminosity(0.f);
	}
	inherited::Update();

	if (m_luminosity_progress_shape)
	{
		if (m_cur_pos != m_luminosity)
		{
			const float _diff = std::abs(m_luminosity - m_cur_pos);
			if (m_luminosity > m_cur_pos)
				m_cur_pos += _diff * Device.fTimeDelta;
			else
				m_cur_pos -= _diff * Device.fTimeDelta;
			clamp(m_cur_pos, 0.f, 100.f);
			m_luminosity_progress_shape->SetPos(m_cur_pos / 100.f);
		}
	}
	else if (m_luminosity_progress_bar)
	{
		const float len = m_luminosity_progress_bar->GetRange_max() - m_luminosity_progress_bar->GetRange_min();
		m_cur_pos = m_luminosity_progress_bar->GetProgressPos();
		if (m_cur_pos != m_luminosity)
		{
			const float _diff = std::abs(m_luminosity - m_cur_pos);
			if (m_luminosity > m_cur_pos)
				m_cur_pos += std::min(len * Device.fTimeDelta, _diff);
			else
				m_cur_pos -= std::min(len * Device.fTimeDelta, _diff);
			clamp(m_cur_pos, m_luminosity_progress_bar->GetRange_min(), m_luminosity_progress_bar->GetRange_max());
			m_luminosity_progress_bar->SetProgressPos(m_cur_pos);
		}
	}

	float normLum = 0.f;
	if (m_luminosity_progress_shape)
		normLum = m_cur_pos / 100.f;
	else if (m_luminosity_progress_bar)
	{
		float rmin = m_luminosity_progress_bar->GetRange_min();
		float rmax = m_luminosity_progress_bar->GetRange_max();
		normLum = (rmax > rmin) ? (m_cur_pos - rmin) / (rmax - rmin) : 0.f;
	}
	else if (_luminosityOverlay != nullptr)
		normLum = _luminosityNormalized;

	if (_luminosityOverlay != nullptr)
	{
		float diff = std::abs(normLum - _luminosityOverlayCur);
		if (normLum > _luminosityOverlayCur)
			_luminosityOverlayCur += diff * Device.fTimeDelta * OVERLAY_LUMINOSITY_SMOOTH_SPEED;
		else
			_luminosityOverlayCur -= diff * Device.fTimeDelta * OVERLAY_LUMINOSITY_SMOOTH_SPEED;
		clamp(_luminosityOverlayCur, 0.f, 1.f);
		u32 maxA = color_get_A(_luminosityOverlayBaseColor);
		u32 alpha = (u32)clampr(iFloor(_luminosityOverlayCur * float(maxA)), 0, 255);
		_luminosityOverlay->SetTextureColor(subst_alpha(_luminosityOverlayBaseColor, alpha));
	}
	if (_noiseOverlay != nullptr)
	{
		float diff = std::abs(_noiseNormalized - _noiseOverlayCur);
		if (_noiseNormalized > _noiseOverlayCur)
			_noiseOverlayCur += diff * Device.fTimeDelta * OVERLAY_NOISE_SMOOTH_SPEED;
		else
			_noiseOverlayCur -= diff * Device.fTimeDelta * OVERLAY_NOISE_SMOOTH_SPEED;
		clamp(_noiseOverlayCur, 0.f, 1.f);
		u32 maxA = color_get_A(_noiseOverlayBaseColor);
		u32 alpha = (u32)clampr(iFloor(_noiseOverlayCur * float(maxA)), 0, 255);
		_noiseOverlay->SetTextureColor(subst_alpha(_noiseOverlayBaseColor, alpha));
	}
}

void SetActorVisibility		(u16 who_id, float value)
{
	if(!IsGameTypeSingleCompatible())
		return;

	if(g_pMotionIcon)
		g_pMotionIcon->SetActorVisibility(who_id, value);
}

void CUIMotionIcon::SetActorVisibility		(u16 who_id, float value)
{
    if (m_luminosity_progress_shape)
    {
        clamp(value, 0.f, 1.f);
        value *= 100.f;
    }
    else if (m_luminosity_progress_bar)
    {
        float v = float(m_luminosity_progress_bar->GetRange_max() - m_luminosity_progress_bar->GetRange_min());
        value *= v;
        value += m_luminosity_progress_bar->GetRange_min();
    }

    auto it = std::find(m_npc_visibility.begin(), m_npc_visibility.end(), who_id);

	if(it==m_npc_visibility.end() && value!=0)
	{
		m_npc_visibility.resize	(m_npc_visibility.size()+1);
		_npc_visibility& v		= m_npc_visibility.back();
		v.id					= who_id;
		v.value					= value;
	}
	else if( fis_zero(value) )
	{
		if (it!=m_npc_visibility.end())
			m_npc_visibility.erase(it);
	}
	else
	{
		(*it).value	= value;
	}

	m_bchanged = true;
}
