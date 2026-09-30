#include "StdAfx.h"
#include "UIMainIngameWnd.h"
#include "UIMotionIcon.h"
#include "UINavigationOwnership.h"
#include "../../xrCore/_color.h"
#include "../../xrCore/Collision/ISpatial.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/device.h"
#include "../Actor.h"
#include "../ActorCondition.h"
#include "../Level.h"
#include "../space_restrictor.h"
#include "../game_cl_single.h"
#include <cmath>

const char* MOTION_ICON_XML = "motion_icon.xml";
static const float OVERLAY_LUMINOSITY_SMOOTH_SPEED = 4.5f;
static const float OVERLAY_NOISE_SMOOTH_SPEED = 5.5f;

namespace
{
	enum class EMotionStatusState : u8
	{
		None = 0,
		Enemy,
		Anomaly,
		SafeZone,
	};

	u32 ColorFromFloat4(const Fvector4& c)
	{
		return color_rgba_f(c.x, c.y, c.z, c.w);
	}

	Fvector4 LerpColor(const Fvector4& a, const Fvector4& b, float t)
	{
		Fvector4 out;
		out.x = a.x + (b.x - a.x) * t;
		out.y = a.y + (b.y - a.y) * t;
		out.z = a.z + (b.z - a.z) * t;
		out.w = a.w + (b.w - a.w) * t;
		return out;
	}

	bool ParseBoolToken(const char* value, bool defaultValue)
	{
		if (!value || !*value)
		{
			return defaultValue;
		}
		if (!_stricmp(value, "true") || !_stricmp(value, "on") || !_stricmp(value, "yes"))
		{
			return true;
		}
		if (!_stricmp(value, "false") || !_stricmp(value, "off") || !_stricmp(value, "no"))
		{
			return false;
		}
		return atoi(value) != 0;
	}

	bool TryReadIniBool(LPCSTR section, LPCSTR key, bool& outValue)
	{
		if (!pSettings || !pSettings->section_exist(section) || !pSettings->line_exist(section, key))
		{
			return false;
		}
		outValue = ParseBoolToken(pSettings->r_string(section, key), outValue);
		return true;
	}

	bool TryReadIniFlt(LPCSTR section, LPCSTR key, float& outValue)
	{
		if (!pSettings || !pSettings->section_exist(section) || !pSettings->line_exist(section, key))
		{
			return false;
		}
		outValue = pSettings->r_float(section, key);
		return true;
	}

	bool ParseIniColorValue(LPCSTR value, Fvector4& outColor)
	{
		if (!value || !*value)
		{
			return false;
		}

		const u32 count = _GetItemCount(value);
		if (count < 3)
		{
			return false;
		}

		string256 token;
		float r = 0.0f;
		float g = 0.0f;
		float b = 0.0f;
		float a = outColor.w;
		_GetItem(value, 0, token); r = float(atof(token));
		_GetItem(value, 1, token); g = float(atof(token));
		_GetItem(value, 2, token); b = float(atof(token));
		const bool hasAlpha = count >= 4;
		if (hasAlpha)
		{
			_GetItem(value, 3, token);
			a = float(atof(token));
		}

		const float maxRgb = std::max(r, std::max(g, b));
		const bool byteMode = (maxRgb > 1.0f) || (hasAlpha && a > 1.0f);
		if (byteMode)
		{
			if (!hasAlpha)
			{
				a = 255.0f;
			}
			outColor.set(
				clampr(r, 0.0f, 255.0f) / 255.0f,
				clampr(g, 0.0f, 255.0f) / 255.0f,
				clampr(b, 0.0f, 255.0f) / 255.0f,
				clampr(a, 0.0f, 255.0f) / 255.0f);
		}
		else
		{
			outColor.set(
				clampr(r, 0.0f, 1.0f),
				clampr(g, 0.0f, 1.0f),
				clampr(b, 0.0f, 1.0f),
				clampr(a, 0.0f, 1.0f));
		}
		return true;
	}

	bool TryReadIniColor(LPCSTR section, LPCSTR key, Fvector4& outColor)
	{
		if (!pSettings || !pSettings->section_exist(section) || !pSettings->line_exist(section, key))
		{
			return false;
		}
		return ParseIniColorValue(pSettings->r_string_wb(section, key).c_str(), outColor);
	}

	bool RestrictorContains(CSpaceRestrictor* restrictor, const Fvector& pos)
	{
		if (!restrictor)
		{
			return false;
		}

		Fsphere probe;
		probe.P = pos;
		probe.R = EPS_L;
		return restrictor->inside(probe);
	}

	CSpaceRestrictor* FindNamedRestrictor(const shared_str& zoneName)
	{
		if (!zoneName.size())
		{
			return nullptr;
		}

		CObject* object = Level().Objects.FindObjectByName(zoneName);
		if (!object || object->getDestroy())
		{
			return nullptr;
		}

		CGameObject* gameObject = object->cast_game_object();
		if (!gameObject)
		{
			return nullptr;
		}

		return gameObject->cast_restrictor();
	}

	bool IsInsideCampZone(const Fvector& pos, float maxDistance)
	{
		if (maxDistance <= 0.0f || !g_SpatialSpace)
		{
			return false;
		}

		static xr_vector<ISpatialShared> nearest;
		nearest.clear();
		nearest.reserve(16);
		g_SpatialSpace->q_sphere(nearest, 0, ESPATIAL_TYPE::CAMP_ZONE, pos, maxDistance);

		for (ISpatialShared& spatial : nearest)
		{
			if (!spatial.get())
			{
				continue;
			}

			CObject* object = spatial->dcast_CObject();
			if (!object || object->getDestroy())
			{
				continue;
			}

			CGameObject* gameObject = object->cast_game_object();
			if (!gameObject)
			{
				continue;
			}

			if (RestrictorContains(gameObject->cast_restrictor(), pos))
			{
				return true;
			}
		}

		return false;
	}

	bool IsInsideNamedSafeZones(const Fvector& pos, const xr_vector<shared_str>& zoneNames)
	{
		for (const shared_str& zoneName : zoneNames)
		{
			if (RestrictorContains(FindNamedRestrictor(zoneName), pos))
			{
				return true;
			}
		}
		return false;
	}

	void ParseSafeZoneNames(LPCSTR value, xr_vector<shared_str>& outNames)
	{
		outNames.clear();
		if (!value || !*value)
		{
			return;
		}

		string4096 buffer;
		xr_strcpy(buffer, value);

		char* context = nullptr;
		for (char* token = strtok_s(buffer, ",; \t\r\n", &context); token; token = strtok_s(nullptr, ",; \t\r\n", &context))
		{
			if (token && *token)
			{
				outNames.push_back(shared_str(token));
			}
		}
	}

	bool TryReadIniSafeZones(LPCSTR section, LPCSTR key, xr_vector<shared_str>& outNames)
	{
		if (!pSettings || !pSettings->section_exist(section) || !pSettings->line_exist(section, key))
		{
			return false;
		}
		ParseSafeZoneNames(pSettings->r_string_wb(section, key).c_str(), outNames);
		return true;
	}
}

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
	_statusEnabled = false;
	_statusIcon = nullptr;
	_statusState = 0;
	_statusPulsePhase = 0.f;
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

	LoadStatusSettings(uiXml);
	EnsureStatusIcon(uiXml);
}

void CUIMotionIcon::LoadStatusSettings(CUIXml& uiXml)
{
	_statusEnabled = false;
	_statusSafeZoneNames.clear();
	_statusUseCampSafeZones = false;

	if (pSettings && pSettings->section_exist("motion_icon"))
	{
		TryReadIniBool("motion_icon", "enabled", _statusEnabled);
		TryReadIniColor("motion_icon", "enemy_color", _statusEnemyColor);
		TryReadIniColor("motion_icon", "anomaly_color", _statusAnomalyColor);
		TryReadIniColor("motion_icon", "safe_color", _statusSafeColor);
		TryReadIniColor("motion_icon", "default_color", _statusNoneColor);
		TryReadIniFlt("motion_icon", "enemy_intensity", _statusEnemyIntensity);
		TryReadIniFlt("motion_icon", "anomaly_intensity", _statusAnomalyIntensity);
		TryReadIniFlt("motion_icon", "safe_intensity", _statusSafeIntensity);
		TryReadIniFlt("motion_icon", "color_transition_speed", _statusColorSpeed);
		TryReadIniFlt("motion_icon", "pulse_amplitude", _statusPulseAmplitude);
		TryReadIniFlt("motion_icon", "pulse_speed", _statusPulseSpeed);
		TryReadIniFlt("motion_icon", "enemy_threshold", _statusEnemyThreshold);
		TryReadIniFlt("motion_icon", "anomaly_threshold", _statusAnomalyThreshold);
		TryReadIniFlt("motion_icon", "max_safe_distance", _statusMaxSafeDistance);
		TryReadIniBool("motion_icon", "pulse_enemy", _statusPulseEnemy);
		TryReadIniBool("motion_icon", "pulse_anomaly", _statusPulseAnomaly);
		TryReadIniBool("motion_icon", "pulse_safe", _statusPulseSafe);
		TryReadIniSafeZones("motion_icon", "safe_zones", _statusSafeZoneNames);

		if (pSettings->line_exist("motion_icon", "safe_zone_source"))
		{
			shared_str source = pSettings->r_string_wb("motion_icon", "safe_zone_source");
			LPCSTR value = source.c_str();
			if (value && (!_stricmp(value, "camp") || !_stricmp(value, "camp_zone")))
			{
				_statusUseCampSafeZones = true;
				_statusSafeZoneNames.clear();
			}
		}

		if (_statusMaxSafeDistance < 0.0f)
		{
			_statusMaxSafeDistance = 0.0f;
		}

		Msg("- motion_icon: safe_zones=%u camp=%d",
			(u32)_statusSafeZoneNames.size(),
			_statusUseCampSafeZones ? 1 : 0);
	}

	const char* statusPaths[] = { "status_icon", "background" };
	for (const char* path : statusPaths)
	{
		if (!uiXml.NavigateToNode(path, 0))
		{
			continue;
		}
		if (uiXml.ReadAttribInt(path, 0, "status_tint", 0) != 0 ||
			uiXml.ReadAttribInt(path, 0, "status_enabled", 0) != 0)
		{
			_statusEnabled = true;
		}
	}
}

void CUIMotionIcon::EnsureStatusIcon(CUIXml& uiXml)
{
	if (_statusIcon || !_statusEnabled)
	{
		return;
	}

	if (uiXml.NavigateToNode("status_icon", 0))
	{
		_statusIcon = UIHelper::CreateStatic(uiXml, "status_icon", this, false);
		if (_statusIcon)
		{
			_statusCurrentColor = _statusNoneColor;
			_statusCurrentColor.w = 0.0f;
			_statusTargetColor = _statusCurrentColor;
			_statusIcon->SetTextureColor(ColorFromFloat4(_statusCurrentColor));
			return;
		}
	}

	if (_compassBackground)
	{
		_statusCurrentColor = _statusNoneColor;
		_statusCurrentColor.w = 0.0f;
		_statusTargetColor = _statusCurrentColor;
	}
}

CUIStatic* CUIMotionIcon::StatusTintTarget() const
{
	if (_statusIcon)
	{
		return _statusIcon;
	}
	if (_statusEnabled)
	{
		return _compassBackground;
	}
	return nullptr;
}

void CUIMotionIcon::ApplyStatusTintColor(float contextualAlpha)
{
	CUIStatic* target = StatusTintTarget();
	if (!target)
	{
		return;
	}

	Fvector4 color = _statusCurrentColor;
	color.w = clampr(color.w * clampr(contextualAlpha, 0.0f, 1.0f), 0.0f, 1.0f);
	target->SetTextureColor(ColorFromFloat4(color));
	target->Show(color.w > 0.01f);
}

void CUIMotionIcon::UpdateStatusGlow()
{
	if (!_compassModeActive || !_statusEnabled || !StatusTintTarget())
	{
		return;
	}

	EMotionStatusState state = EMotionStatusState::None;
	float intensity = 0.0f;

	const float enemyThreat = GetThreatNormalized();
	if (enemyThreat > _statusEnemyThreshold)
	{
		state = EMotionStatusState::Enemy;
		intensity = clampr(enemyThreat, 0.0f, 1.0f);
	}
	else
	{
		CActor* actor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
		if (actor)
		{
			const float zoneDanger = clampr(actor->conditions().GetZoneDanger(), 0.0f, 1.0f);
			if (zoneDanger > _statusAnomalyThreshold)
			{
				state = EMotionStatusState::Anomaly;
				intensity = zoneDanger;
			}
			else if (IsStatusSafeZone(actor->Position()))
			{
				state = EMotionStatusState::SafeZone;
				intensity = 1.0f;
			}
		}
	}

	_statusState = u8(state);

	Fvector4 styleColor = _statusNoneColor;
	float styleIntensity = 0.0f;
	bool pulse = false;
	switch (state)
	{
	case EMotionStatusState::Enemy:
		styleColor = _statusEnemyColor;
		styleIntensity = _statusEnemyIntensity;
		pulse = _statusPulseEnemy;
		break;
	case EMotionStatusState::Anomaly:
		styleColor = _statusAnomalyColor;
		styleIntensity = _statusAnomalyIntensity;
		pulse = _statusPulseAnomaly;
		break;
	case EMotionStatusState::SafeZone:
		styleColor = _statusSafeColor;
		styleIntensity = _statusSafeIntensity;
		pulse = _statusPulseSafe;
		break;
	default:
		break;
	}

	Fvector4 desired = styleColor;
	desired.w = clampr(styleColor.w * clampr(styleIntensity * intensity, 0.0f, 1.0f), 0.0f, 1.0f);
	if (intensity <= 0.0f)
	{
		desired.w = 0.0f;
	}

	if (pulse && intensity > 0.0f && _statusPulseAmplitude > 0.0f)
	{
		_statusPulsePhase += Device.fTimeDelta * std::max(_statusPulseSpeed, 0.0f);
		desired.w = clampr(desired.w * (1.0f + _statusPulseAmplitude * std::sin(_statusPulsePhase)), 0.0f, 1.0f);
	}
	else
	{
		_statusPulsePhase = 0.0f;
	}

	_statusTargetColor = desired;
	const float t = clampr(Device.fTimeDelta * std::max(_statusColorSpeed, 0.0f), 0.0f, 1.0f);
	_statusCurrentColor = LerpColor(_statusCurrentColor, _statusTargetColor, t);

	const float contextualAlpha = (_compassModeActive && _compassContextualFade) ? _contextualAlpha : 1.0f;
	ApplyStatusTintColor(contextualAlpha);
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

	if (_statusEnabled)
	{
		if (GetThreatNormalized() > _statusEnemyThreshold)
		{
			return true;
		}

		CActor* actor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
		if (actor)
		{
			if (actor->conditions().GetZoneDanger() > _statusAnomalyThreshold)
			{
				return true;
			}
			if (IsStatusSafeZone(actor->Position()))
			{
				return true;
			}
		}
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
	if (_statusEnabled && StatusTintTarget())
	{
		ApplyStatusTintColor(alpha);
		return;
	}

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

	LoadStatusSettings(uiXml);

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
        EnsureStatusIcon(uiXml);
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
    auto toggle = [visible](CUIStatic* widget)
    {
        if (widget)
        {
            widget->Show(visible);
            widget->Enable(visible);
        }
    };

    toggle(_compassBackground);
    toggle(_statusIcon);

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

	UpdateStatusGlow();
}

void SetActorVisibility		(u16 who_id, float value)
{
	if(!IsGameTypeSingleCompatible())
		return;

	if(g_pMotionIcon)
		g_pMotionIcon->SetActorVisibility(who_id, value);
}

float CUIMotionIcon::GetThreatNormalized() const
{
	float maxValue = 0.0f;
	for (const _npc_visibility& entry : m_npc_visibility)
	{
		if (entry.value > maxValue)
		{
			maxValue = entry.value;
		}
	}

	if (maxValue <= 0.0f)
	{
		return 0.0f;
	}

	if (m_luminosity_progress_shape)
	{
		return clampr(maxValue / 100.0f, 0.0f, 1.0f);
	}

	if (m_luminosity_progress_bar)
	{
		const float rmin = m_luminosity_progress_bar->GetRange_min();
		const float rmax = m_luminosity_progress_bar->GetRange_max();
		if (rmax <= rmin)
		{
			return 0.0f;
		}
		return clampr((maxValue - rmin) / (rmax - rmin), 0.0f, 1.0f);
	}

	return clampr(maxValue > 1.0f ? maxValue / 100.0f : maxValue, 0.0f, 1.0f);
}

bool CUIMotionIcon::IsStatusSafeZone(const Fvector& pos) const
{
	if (!_statusSafeZoneNames.empty())
	{
		return IsInsideNamedSafeZones(pos, _statusSafeZoneNames);
	}
	if (_statusUseCampSafeZones)
	{
		return IsInsideCampZone(pos, _statusMaxSafeDistance);
	}
	return false;
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
