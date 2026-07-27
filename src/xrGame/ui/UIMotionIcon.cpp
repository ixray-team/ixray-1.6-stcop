#include "StdAfx.h"
#include "UIMainIngameWnd.h"
#include "UIMotionIcon.h"
#include "UINavigationOwnership.h"
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
	_compassBackground = nullptr;
}

CUIMotionIcon::~CUIMotionIcon()
{
	g_pMotionIcon = nullptr;
	m_states.clear();
	_compassLayoutFrame = nullptr;
}

void CUIMotionIcon::ResetVisibility()
{
	m_npc_visibility.clear	();
	m_luminosity			= 0.0f;
	m_bchanged				= true;
	_luminosityOverlayCur	= 0.f;
	_noiseOverlayCur		= 0.f;
	_contextualAlpha		= 0.f;
	if (_compassContextualFade)
	{
		ApplyCompassContextualAlpha(0.f);
	}
	if (_minimapContextualFade && _luminosityOverlay)
	{
		ApplyMinimapLuminosityOverlayAlpha(0.f);
	}
}

void CUIMotionIcon::LoadContextualFadeSettings(CUIXml& uiXml, const char* path, bool& contextualFadeOut)
{
	if (!uiXml.NavigateToNode(path, 0))
	{
		return;
	}

	contextualFadeOut = uiXml.ReadAttribInt(path, 0, "contextual_fade", contextualFadeOut ? 1 : 0) != 0;
	_fadeInSpeed = std::max(uiXml.ReadAttribFlt(path, 0, "fade_in_speed", _fadeInSpeed), 0.1f);
	_fadeOutSpeed = std::max(uiXml.ReadAttribFlt(path, 0, "fade_out_speed", _fadeOutSpeed), 0.1f);
	_minVisibleAlpha = clampr(uiXml.ReadAttribFlt(path, 0, "min_visible_alpha", _minVisibleAlpha), 0.0f, 1.0f);
	_visibilityThreshold = uiXml.ReadAttribFlt(path, 0, "visibility_threshold", _visibilityThreshold);
}

void CUIMotionIcon::InitMinimapLuminosityOverlay(CUIXml& uiXml)
{
	if (_luminosityOverlay || !uiXml.NavigateToNode("luminosity_overlay", 0))
	{
		return;
	}

	_luminosityOverlay = UIHelper::CreateStatic(uiXml, "luminosity_overlay", this, false);
	if (!_luminosityOverlay)
	{
		return;
	}

	_luminosityOverlayBaseColor = _luminosityOverlay->GetTextureColor();
	_luminosityOverlay->SetTextureColor(subst_alpha(_luminosityOverlayBaseColor, 0));
	LoadContextualFadeSettings(uiXml, "minimap_layout", _minimapContextualFade);
}

void CUIMotionIcon::EnsureCompassLayout(CUIXml& uiXml)
{
	if (_compassLayoutFrame || !uiXml.NavigateToNode("compass_layout", 0))
	{
		return;
	}

	CUIXmlInit xml_init;
	const char* layoutPath = "compass_layout";

	_compassLayoutFrame = new CUIWindow();
	_compassLayoutFrame->SetAutoDelete(true);
	xml_init.InitWindow(uiXml, layoutPath, 0, _compassLayoutFrame);

	_compassLayoutPos.x = uiXml.ReadAttribFlt(layoutPath, 0, "x", 0.0f);
	_compassLayoutPos.y = uiXml.ReadAttribFlt(layoutPath, 0, "y", 1.0f);
	_compassLayoutSize.x = uiXml.ReadAttribFlt(layoutPath, 0, "width", 1.0f);
	_compassLayoutSize.y = uiXml.ReadAttribFlt(layoutPath, 0, "height", 0.893f);
	_compassLayoutRelative = _compassLayoutSize.x <= 1.0f && _compassLayoutSize.y <= 1.0f &&
		_compassLayoutPos.x <= 1.0f;

	const shared_str layoutAlign = uiXml.ReadAttrib(layoutPath, 0, "align", "");
	_compassLayoutAlignCenter = (_compassLayoutFrame->GetAlignment() == waCenter) ||
		(layoutAlign.size() > 0 && strchr(layoutAlign.c_str(), 'c'));

	LoadContextualFadeSettings(uiXml, "compass_layout", _compassContextualFade);

	if (!_compassBackground && uiXml.NavigateToNode("background", 0))
	{
		_compassBackground = UIHelper::CreateStatic(uiXml, "background", this);
		if (_compassBackground)
		{
			_compassBackground->SetWndPos(Fvector2().set(0.0f, 0.0f));
			_compassBackground->SetWndSize(GetWndSize());
			_compassBackgroundBaseColor = _compassBackground->GetTextureColor();
		}
	}
}

void CUIMotionIcon::ApplyNavigationPresentation(bool useCompassBar, CUIXml* uiXml, Fvector2 const* overlaySize, Fvector2 const* overlayPos)
{
	CUIXml localXml;
	CUIXml* xml = uiXml;
	if (!xml)
	{
		localXml.Load(CONFIG_PATH, UI_PATH, MOTION_ICON_XML);
		xml = &localXml;
	}

	_contextualAlpha = 0.f;

	if (useCompassBar)
	{
		EnsureCompassLayout(*xml);
		_compassModeActive = (_compassLayoutFrame != nullptr || _compassBackground != nullptr);
		_noiseNormalized = 0.f;
		_noiseOverlayCur = 0.f;
		SetMinimapOverlayVisibility(false);
		SetCompassOverlayVisibility(true);
		if (_compassContextualFade)
		{
			ApplyCompassContextualAlpha(0.f);
		}
		return;
	}

	_compassModeActive = false;
	SetCompassOverlayVisibility(false);

	Fvector2 sz = overlaySize ? *overlaySize : GetWndSize();
	Fvector2 pos = overlayPos ? *overlayPos : Fvector2().set(sz.x / 2.0f, sz.y / 2.0f);
	EnsureMinimapOverlays(*xml, sz, pos);
	SetMinimapOverlayVisibility(true);
	if (_minimapContextualFade && _luminosityOverlay)
	{
		ApplyMinimapLuminosityOverlayAlpha(0.f);
	}
}

void CUIMotionIcon::SetNavigationPresentation(bool useCompassBar)
{
	ApplyNavigationPresentation(useCompassBar);
}

float CUIMotionIcon::UpdateContextualFadeAlpha(float alpha, bool isVisible) const
{
	const float speed = std::max(isVisible ? _fadeInSpeed : _fadeOutSpeed, 1.0f);
	const float target = isVisible ? 1.0f : 0.0f;
	const float delta = target - alpha;
	const float t = clampr(Device.fTimeDelta * speed, 0.0f, 1.0f);
	const float smoothT = 1.0f - (1.0f - t) * (1.0f - t);
	return clampr(alpha + delta * smoothT, 0.0f, 1.0f);
}

bool CUIMotionIcon::IsContextuallyNeeded() const
{
	if (!m_npc_visibility.empty())
	{
		return true;
	}

	float luminosityNorm = 0.f;
	if (m_luminosity_progress_bar)
	{
		const float rmin = m_luminosity_progress_bar->GetRange_min();
		const float rmax = m_luminosity_progress_bar->GetRange_max();
		luminosityNorm = (rmax > rmin) ? (m_cur_pos - rmin) / (rmax - rmin) : 0.f;
	}
	else if (m_luminosity_progress_shape)
	{
		luminosityNorm = m_cur_pos / 100.f;
	}
	else if (_luminosityOverlay != nullptr)
	{
		luminosityNorm = _luminosityNormalized;
	}
	else
	{
		luminosityNorm = m_luminosity > 1.f ? m_luminosity / 100.f : m_luminosity;
	}

	if (luminosityNorm > _visibilityThreshold)
	{
		return true;
	}

	if (_compassModeActive)
	{
		return false;
	}

	return _noiseNormalized > _visibilityThreshold;
}

void CUIMotionIcon::ApplyCompassContextualAlpha(float alpha)
{
	if (!_compassBackground)
	{
		return;
	}

	const u32 baseColor = _compassBackgroundBaseColor;
	const u32 channelAlpha = (u32)clampr(iFloor(float(color_get_A(baseColor)) * alpha), 0, 255);
	_compassBackground->SetTextureColor(subst_alpha(baseColor, channelAlpha));
}

void CUIMotionIcon::ApplyMinimapLuminosityOverlayAlpha(float contextualAlpha)
{
	if (!_luminosityOverlay)
	{
		return;
	}

	const u32 maxA = color_get_A(_luminosityOverlayBaseColor);
	const float intensity = _minimapContextualFade ? _luminosityOverlayCur : 1.f;
	const u32 alpha = (u32)clampr(iFloor(contextualAlpha * intensity * float(maxA)), 0, 255);
	_luminosityOverlay->SetTextureColor(subst_alpha(_luminosityOverlayBaseColor, alpha));
}

bool CUIMotionIcon::Init(Frect const& zonemap_rect, bool useCompassBar, bool useCompassLayout)
{
	CUIXml						uiXml;
	uiXml.Load					(CONFIG_PATH, UI_PATH, MOTION_ICON_XML);

	CUIXmlInit					xml_init;

	const bool hasCompassLayoutNode = useCompassLayout && uiXml.NavigateToNode("compass_layout", 0);
	const bool bootCompassLayout = useCompassBar && hasCompassLayoutNode;

	if (uiXml.NavigateToNode("window", 0) && !bootCompassLayout)
	{
		xml_init.InitWindow(uiXml, "window", 0, this);
	}
	else if (!bootCompassLayout)
	{
		m_independent = xml_init.InitStatic(uiXml, "background", 0, this);
	}

	Fvector2					sz;
	Fvector2					pos;

    if (bootCompassLayout)
    {
        EnsureCompassLayout(uiXml);
        _contextualAlpha = 0.f;
        _compassModeActive = true;
    }
    else if (!useCompassBar)
    {
        LoadContextualFadeSettings(uiXml, "minimap_layout", _minimapContextualFade);
    }
    else if (useCompassBar && useCompassLayout && !hasCompassLayoutNode)
    {
        zonemap_rect.getsize(sz);
        SetWndSize(sz);
        SetWndPos(Fvector2().set(0.0f, 0.0f));
        _contextualAlpha = 0.f;
    }
    else if (!m_independent)
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
        if (!useLuminosityOverlay && uiXml.NavigateToNode("luminosity_progress", 0))
            m_luminosity_progress_bar = UIHelper::CreateProgressBar(uiXml, "luminosity_progress", this);
        if (!useNoiseOverlay && uiXml.NavigateToNode("noise_progress", 0))
            m_noise_progress_bar = UIHelper::CreateProgressBar(uiXml, "noise_progress", this);
    }
    else if (bootCompassLayout)
    {
        SetMinimapOverlayVisibility(false);
    }
    else if (!useCompassBar)
    {
        if (!useLuminosityOverlay && !m_luminosity_progress_bar)
        {
            if (uiXml.NavigateToNode("luminosity_progress", 0))
            {
                m_luminosity_progress_shape = UIHelper::CreateProgressShape(uiXml, "luminosity_progress", this);
                if (m_luminosity_progress_shape)
                {
                    m_luminosity_progress_shape->SetWndSize(sz);
                    m_luminosity_progress_shape->SetWndPos(pos);
                }
            }
        }
        if (!useNoiseOverlay && !m_noise_progress_bar)
        {
            if (uiXml.NavigateToNode("noise_progress", 0))
            {
                m_noise_progress_shape = UIHelper::CreateProgressShape(uiXml, "noise_progress", this);
                if (m_noise_progress_shape)
                {
                    m_noise_progress_shape->SetWndSize(sz);
                    m_noise_progress_shape->SetWndPos(pos);
                }
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

    if (!useCompassBar)
    {
        InitMinimapLuminosityOverlay(uiXml);
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

    if (_compassContextualFade)
    {
        ApplyCompassContextualAlpha(0.f);
    }
    if (_minimapContextualFade && _luminosityOverlay)
    {
        ApplyMinimapLuminosityOverlayAlpha(0.f);
    }

    return m_independent;
}

void CUIMotionIcon::EnsureMinimapOverlays(CUIXml& uiXml, Fvector2 const& sz, Fvector2 const& pos)
{
	const bool useLuminosityOverlay = uiXml.NavigateToNode("luminosity_overlay", 0);
    const bool useNoiseOverlay = uiXml.NavigateToNode("noise_overlay", 0);

    if (!useLuminosityOverlay && !m_luminosity_progress_bar)
    {
        if (!m_luminosity_progress_shape && uiXml.NavigateToNode("luminosity_progress", 0))
            m_luminosity_progress_shape = UIHelper::CreateProgressShape(uiXml, "luminosity_progress", this);
        if (m_luminosity_progress_shape)
        {
            m_luminosity_progress_shape->SetWndSize(sz);
            m_luminosity_progress_shape->SetWndPos(pos);
        }
    }

    if (!useNoiseOverlay && !m_noise_progress_bar)
    {
        if (!m_noise_progress_shape && uiXml.NavigateToNode("noise_progress", 0))
            m_noise_progress_shape = UIHelper::CreateProgressShape(uiXml, "noise_progress", this);
        if (m_noise_progress_shape)
        {
            m_noise_progress_shape->SetWndSize(sz);
            m_noise_progress_shape->SetWndPos(pos);
        }
    }

    if (useNoiseOverlay && !_noiseOverlay)
    {
        _noiseOverlay = UIHelper::CreateStatic(uiXml, "noise_overlay", this, false);
        if (_noiseOverlay)
        {
            _noiseOverlayBaseColor = _noiseOverlay->GetTextureColor();
            _noiseOverlay->SetTextureColor(subst_alpha(_noiseOverlayBaseColor, 0));
        }
    }

    InitMinimapLuminosityOverlay(uiXml);
}

void CUIMotionIcon::SetMinimapOverlayVisibility(bool visible)
{
    auto toggle = [visible](auto* widget)
    {
        if (widget)
        {
            widget->Show(visible);
            widget->Enable(visible);
        }
    };

    toggle(m_luminosity_progress_shape);
    toggle(m_noise_progress_shape);
    toggle(m_luminosity_progress_bar);
    toggle(m_noise_progress_bar);
    toggle(_luminosityOverlay);
    toggle(_noiseOverlay);
}

void CUIMotionIcon::SetCompassOverlayVisibility(bool visible)
{
    if (_compassBackground)
    {
        _compassBackground->Show(visible);
        _compassBackground->Enable(visible);
    }

    if (!visible && _compassContextualFade)
    {
        ApplyCompassContextualAlpha(0.f);
    }
}

void CUIMotionIcon::ApplyNavigationHost(CUIWindow* attachParent, Frect const& hostRect, bool useCompassBar)
{
    if (!attachParent)
        return;

    if (!m_independent)
    {
        CUIXml uiXml;
        uiXml.Load(CONFIG_PATH, UI_PATH, MOTION_ICON_XML);
        const float rel_sz = uiXml.ReadAttribFlt("window", 0, "rel_size", 1.0f);

        Fvector2 sz;
        Fvector2 pos;
        hostRect.getsize(sz);
        pos.set(sz.x / 2.0f, sz.y / 2.0f);
        SetWndSize(sz);
        SetWndPos(pos);

        const float k = UI().get_current_kx();
        sz.mul(rel_sz * k);

        ApplyNavigationPresentation(useCompassBar, &uiXml, &sz, &pos);
    }

    UINavigationOwnership::ReparentOwned(attachParent, this);
}

CUIWindow* CUIMotionIcon::CompassLayoutFrame() const
{
    return _compassLayoutFrame;
}

void CUIMotionIcon::ApplyCompassLayout(CUIWindow* compassBar)
{
    if (!compassBar)
    {
        return;
    }

    if (!_compassLayoutFrame)
    {
        CUIXml uiXml;
        uiXml.Load(CONFIG_PATH, UI_PATH, MOTION_ICON_XML);
        EnsureCompassLayout(uiXml);
    }

    if (!_compassLayoutFrame)
    {
        return;
    }

    Fvector2 size = _compassLayoutSize;
    Fvector2 pos = _compassLayoutPos;

    if (_compassLayoutRelative)
    {
        const float compassWidth = compassBar->GetWidth();
        const float compassHeight = compassBar->GetHeight();
        size.set(_compassLayoutSize.x * compassWidth, _compassLayoutSize.y * compassHeight);

        if (_compassLayoutPos.y >= 1.0f)
        {
            pos.y = compassHeight + (_compassLayoutPos.y - 1.0f) * compassHeight;
        }
        else
        {
            pos.y = _compassLayoutPos.y * compassHeight;
        }

        if (_compassLayoutAlignCenter)
        {
            pos.x = (compassWidth - size.x) * 0.5f + _compassLayoutPos.x * compassWidth;
        }
        else
        {
            pos.x = _compassLayoutPos.x * compassWidth;
        }
    }

    _compassLayoutFrame->SetAlignment(waNone);
    _compassLayoutFrame->SetWndSize(size);
    _compassLayoutFrame->SetWndPos(pos);

    SetWndSize(size);
    SetWndPos(Fvector2().set(0.0f, 0.0f));

    if (_compassBackground)
    {
        _compassBackground->SetWndPos(Fvector2().set(0.0f, 0.0f));
        _compassBackground->SetWndSize(size);
    }

    ApplyNavigationPresentation(true);
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

	if (_compassModeActive)
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
	else
	{
		m_luminosity = newPos;
	}

	if (_luminosityOverlay != nullptr && m_luminosity_progress_shape == nullptr && m_luminosity_progress_bar == nullptr)
	{
		_luminosityNormalized = newPos > 1.f ? newPos / 100.f : newPos;
		_luminosityNormalized = clampr(_luminosityNormalized, 0.f, 1.f);
	}
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
	{
		if (_compassModeActive && _compassContextualFade && _contextualAlpha <= _minVisibleAlpha)
		{
			return;
		}
		inherited::Draw();
	}
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

		if (_minimapContextualFade && !_compassModeActive)
		{
			_contextualAlpha = UpdateContextualFadeAlpha(_contextualAlpha, IsContextuallyNeeded());
			ApplyMinimapLuminosityOverlayAlpha(_contextualAlpha);
		}
		else if (!_compassModeActive)
		{
			const u32 maxA = color_get_A(_luminosityOverlayBaseColor);
			const u32 alpha = (u32)clampr(iFloor(_luminosityOverlayCur * float(maxA)), 0, 255);
			_luminosityOverlay->SetTextureColor(subst_alpha(_luminosityOverlayBaseColor, alpha));
		}
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

	if (_compassModeActive && _compassContextualFade)
	{
		const bool isNeeded = IsContextuallyNeeded();
		_contextualAlpha = UpdateContextualFadeAlpha(_contextualAlpha, isNeeded);
		ApplyCompassContextualAlpha(_contextualAlpha);
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
