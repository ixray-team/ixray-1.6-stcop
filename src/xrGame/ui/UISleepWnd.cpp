#include "StdAfx.h"
#include "UISleepWnd.h"

#include "pch_script.h"
#include "Actor.h"
#include "ActorCondition.h"
#include "ActorEffector.h"
#include "Level.h"
#include "PostprocessAnimator.h"
#include "alife_simulator.h"
#include "alife_time_manager.h"
#include "game_sv_single.h"
#include "GamePersistent.h"
#include "script_game_object.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"
#include "../../xrEngine/date_time.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UITrackBar.h"
#include "../../xrUI/Widgets/UIActionRepeaters.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "UIMessageBoxEx.h"
#include "UIInventoryUtilities.h"

namespace
{
constexpr LPCSTR kParams = "sleep_params";
constexpr LPCSTR kPreset = "sleep_preset_btn";
constexpr LPCSTR kStrip = "sleep_hours_strip";

u32 LevelHours()
{
	u32 y = 0, mo = 0, d = 0, h = 0, mi = 0, s = 0, ms = 0;
	split_time(Level().GetGameTime(), y, mo, d, h, mi, s, ms);
	return h;
}

void ChangeGameTimeHours(u32 hours)
{
	auto* game = Level().Server && Level().Server->game ? Level().Server->game->cast_game_sv_single() : nullptr;
	if (!game || !ai().get_alife()) return;
	const u32 secs = hours * 3600;
	g_pGamePersistent->Environment().ChangeGameTime(static_cast<float>(secs));
	game->alife().time_manager().change_game_time(secs * 1000);
}

shared_str AttrStr(CUIXml& xml, LPCSTR path, LPCSTR a, const shared_str& fb)
{
	LPCSTR v = xml.ReadAttrib(path, 0, a, nullptr);
	return (v && v[0]) ? shared_str(v) : fb;
}
bool AttrBool(CUIXml& xml, LPCSTR path, LPCSTR a, bool fb) { return xml.ReadAttribInt(path, 0, a, fb ? 1 : 0) != 0; }
LPCSTR ResolveFmt(const shared_str& key) { return (!key || key.size() == 0) ? "%s" : *g_pStringTable->translate(key); }

bool LuaPcall(lua_State* L, int argc, int nres)
{
	if (lua_pcall(L, argc, nres, 0) == 0) return true;
	if (const char* err = lua_tostring(L, -1)) Msg("! [sleep] Lua error: %s", err);
	lua_pop(L, 1);
	return false;
}

int PushMgr(lua_State* L, LPCSTR module, LPCSTR getter)
{
	if (!L || !g_pGameLevel || !Level().game) return 0;
	const luabind::object globals = luabind::get_globals(L);
	const luabind::object mod = globals[module];
	if (luabind::get_type(mod) != LUA_TTABLE) return 0;
	const luabind::object getterObj = mod[getter];
	if (luabind::get_type(getterObj) != LUA_TFUNCTION) return 0;
	getterObj.pushvalue();
	if (!LuaPcall(L, 0, 1) || lua_isnil(L, -1)) { lua_pop(L, 1); return 0; }
	return lua_gettop(L);
}

void SetBool(lua_State* L, int tbl, LPCSTR f, bool v) { lua_pushboolean(L, v ? 1 : 0); lua_setfield(L, tbl, f); }
bool GetBool(lua_State* L, int tbl, LPCSTR f, bool def = false)
{
	lua_getfield(L, tbl, f);
	const bool v = lua_isboolean(L, -1) ? (lua_toboolean(L, -1) != 0) : def;
	lua_pop(L, 1);
	return v;
}
shared_str GetStr(lua_State* L, int tbl, LPCSTR f)
{
	lua_getfield(L, tbl, f);
	shared_str v;
	if (lua_isstring(L, -1)) v = lua_tostring(L, -1);
	lua_pop(L, 1);
	return v;
}

void ForceWeather(lua_State* L, int mgr)
{
	lua_getfield(L, mgr, "forced_weather_change");
	if (!lua_isfunction(L, -1)) { lua_pop(L, 1); return; }
	lua_pushvalue(L, mgr);
	LuaPcall(L, 1, 0);
}

void CallXrEffect(LPCSTR fn)
{
	CActor* actor = Actor();
	CGameObject* go = actor ? actor->cast_game_object() : nullptr;
	CScriptGameObject* ao = go ? go->lua_game_object() : nullptr;
	if (!ao) return;
	luabind::functor<void> f;
	if (!ai().script_engine().functor(fn, f)) { Msg("! [sleep] Lua functor not found: %s", fn); return; }
	f(ao, luabind::object(), luabind::object());
}

void ScriptCb(LPCSTR name, int hours)
{
	lua_State* L = ai().script_engine().lua();
	if (!L) return;
	lua_getglobal(L, "SendScriptCallback");
	if (!lua_isfunction(L, -1)) { lua_pop(L, 1); return; }
	lua_pushstring(L, name);
	lua_pushnumber(L, hours);
	LuaPcall(L, 2, 0);
}

void AddPP(LPCSTR fn, int id)
{
	if (CActor* a = Actor())
	{
		auto* pp = new CPostprocessAnimator(id, false);
		pp->Load(fn);
		a->Cameras().AddPPEffector(pp);
	}
}

void GiveTutorialSleep()
{
	if (CActor* a = Actor())
	{
		a->GiveInfoPortion("tutorial_sleep");
		a->DisableInfoPortion("sleep_active");
	}
}

void CreateSleepCover(CUIXml& xml, CUIWindow* parent)
{
	if (!xml.NavigateToNode("static_cover", 0)) return;
	LPCSTR coverTex = xml.Read("static_cover:texture", 0, nullptr);
	bool nine = false;
	if (coverTex && coverTex[0])
	{
		string256 probe; xr_sprintf(probe, "%s_lt", coverTex);
		nine = CUITextureMaster::ItemExist(probe);
	}
	CUIWindow* cover = nine ? UIHelper::CreateFrameWindow(xml, "static_cover", parent, false) : nullptr;
	if (!cover) cover = UIHelper::CreateStatic(xml, "static_cover", parent, false);
	if (cover) cover->Enable(false);
}

void SetupPanorama(shared_str tex, CUIStatic* s0, CUIStatic* s1, Frect& outTex, Fvector2& outDisp, float& hourStep, int hoursN)
{
	if (!CUITextureMaster::ItemExist(tex))
	{
		Msg("! [sleep] panorama texture [%s] missing, fallback to ui_inGame2_sky_panorama", tex.c_str());
		tex = "ui_inGame2_sky_panorama";
	}
	const Frect pr = CUITextureMaster::GetTextureRect(tex);
	outTex.set(pr.x1, pr.y1, pr.x2, pr.y2);
	outDisp.set(s0 ? s0->GetWidth() : pr.width(), s0 ? s0->GetHeight() : pr.height());
	hourStep = outDisp.x / static_cast<float>(hoursN);
	for (CUIStatic* st : { s0, s1 })
	{
		if (!st) continue;
		st->InitTexture(*tex);
		st->SetStretchTexture(true);
		st->SetWndSize(outDisp);
	}
}

void LoadSleepSounds(CUIXml& xml, ref_sound snd[], u8 count)
{
	static constexpr LPCSTR keys[] = {
		"snd_open", "snd_close", "snd_sleep", "snd_cancel", "snd_track", "snd_preset", "snd_warning"
	};
	for (u8 i = 0, n = std::min(count, u8(std::size(keys))); i < n; ++i)
		if (LPCSTR name = xml.ReadAttrib(kParams, 0, keys[i], nullptr); name && name[0])
			snd[i].create(name, st_Effect, sg_SourceType);
}

void PlaceTrackSlider(CUITrackBar* track, float hours, int minH, int maxH)
{
	if (!track) return;
	CUI3tButton* slider = track->GetSlider();
	if (!slider) return;
	float t = (hours - float(minH)) / std::max(0.001f, float(maxH - minH));
	clamp(t, 0.f, 1.f);
	if (track->GetInvert()) t = 1.f - t;
	Fvector2 pos = slider->GetWndPos();
	pos.x = t * (track->GetWidth() - slider->GetWidth());
	slider->SetWndPos(pos);
}

void ApplySleepWeatherAfterRest()
{
	lua_State* L = ai().script_engine().lua();
	if (!L) return;
	auto markSurge = [&](int surge) { SetBool(L, surge, "time_forwarded", true); return GetBool(L, surge, "started", false); };

	if (const int weather = PushMgr(L, "level_weathers", "get_weather_manager"))
	{
		ForceWeather(L, weather);
		lua_getfield(L, weather, "weather_fx");
		const bool hasFx = !lua_isnil(L, -1);
		lua_pop(L, 1);
		bool surgeOn = false;
		if (const int surge = PushMgr(L, "surge_manager", "get_surge_manager"))
		{ surgeOn = markSurge(surge); lua_pop(L, 1); }
		if (surgeOn && hasFx)
		{
			if (g_pGamePersistent) g_pGamePersistent->Environment().StopWFX();
			ForceWeather(L, weather);
		}
		lua_pop(L, 1);
	}
	else if (const int surge = PushMgr(L, "surge_manager", "get_surge_manager"))
	{ markSurge(surge); lua_pop(L, 1); }
}

float PositiveMod(float v, float n)
{
	v = fmodf(v, n);
	return v < 0.f ? v + n : v;
}
} // namespace

class CSleepCamEffectorCB final : public CAnimatorCamEffector
{
	CUISleepWnd* m_owner = nullptr;
	bool m_fired = false;
public:
	explicit CSleepCamEffectorCB(CUISleepWnd* o) : m_owner(o) {}
	bool Valid() override
	{
		const bool active = CAnimatorCamEffector::Valid();
		if (!active && m_owner && !m_fired) { m_fired = true; m_owner->OnCamEffectorDone(); }
		return active;
	}
};

static void AddCamCB(CUISleepWnd* owner, LPCSTR fn, int id)
{
	CActor* a = Actor();
	if (!a || !owner) return;
	auto* e = new CSleepCamEffectorCB(owner);
	e->SetType(static_cast<ECamEffectorType>(id));
	e->SetCyclic(false);
	e->Start(fn);
	a->Cameras().AddCamEffector(e);
}

void CUISleepWnd::PlaySnd(EUiSound id)
{
	if (m_snd[id]._p) m_snd[id].play_no_feedback(nullptr, sm_2D);
}

void CUISleepWnd::LoadSleepParams(CUIXml& xml)
{
	m_params = SleepParams{};
	if (!xml.NavigateToNode(kParams, 0))
	{
		m_paramsBase = m_params;
		return;
	}
	auto& p = m_params;
	auto str = [&](LPCSTR a, shared_str& dst) { dst = AttrStr(xml, kParams, a, dst); };
	auto bol = [&](LPCSTR a, bool fb) { return AttrBool(xml, kParams, a, fb); };
	auto i32 = [&](LPCSTR a, int fb) { return xml.ReadAttribInt(kParams, 0, a, fb); };
	auto flt = [&](LPCSTR a, float fb) { return xml.ReadAttribFlt(kParams, 0, a, fb); };

	str("panorama_texture", p.panoramaTexture);
	p.panoramaHours = std::max(1, i32("panorama_hours", p.panoramaHours));
	if (LPCSTR bind = xml.ReadAttrib(kParams, 0, "panorama_bind", nullptr); bind && bind[0])
		p.panoramaBindWake = (0 == xr_strcmp(bind, "wake"));
	p.panoramaSmoothSpeed = flt("panorama_smooth_speed", p.panoramaSmoothSpeed);
	if (LPCSTR fit = xml.ReadAttrib(kParams, 0, "panorama_scale", nullptr); fit && fit[0])
		p.panoramaNativeScale = (0 == xr_strcmp(fit, "native"));
	p.panoramaTexScale = std::max(1.f, flt("panorama_tex_scale", p.panoramaTexScale));
	p.panoramaWrap = bol("panorama_wrap", true);
	str("hour_label_suffix", p.hourLabelSuffix);
	str("warning_box_template", p.warningBoxTemplate);
	str("warning_bleeding", p.warningBleeding);
	str("warning_radiation", p.warningRadiation);
	str("warning_both", p.warningBoth);
	p.allowSleepWithBleeding = bol("allow_sleep_with_bleeding", false);
	p.minHours = std::max(1, i32("min_hours", p.minHours));
	p.maxHours = i32("max_hours", p.maxHours);
	if (p.maxHours < p.minHours) p.maxHours = p.minHours;
	str("cam_anm", p.camAnm);
	str("pp_effector", p.ppEffector);
	p.camId = i32("cam_id", p.camId);
	p.ppId = i32("pp_id", p.ppId);
	p.markerMinX = flt("marker_min_x", p.markerMinX);
	p.restorePower = flt("restore_power", p.restorePower);
	p.muteMusic = bol("mute_music", true);
	p.muteEffects = bol("mute_effects", true);
	p.presetSpacing = flt("preset_spacing", p.presetSpacing);
	p.presetsConfirm = bol("presets_confirm", false);
	str("fmt_time_now", p.fmtTimeNow);
	str("fmt_sleep_duration", p.fmtSleepDuration);
	str("fmt_wake_time", p.fmtWakeTime);
	if (LPCSTR sep = xml.ReadAttrib(kParams, 0, "time_separator", nullptr); sep && sep[0]) p.timeSeparator = sep[0];
	if (LPCSTR prec = xml.ReadAttrib(kParams, 0, "time_precision", nullptr))
		p.timePrecision = (0 == xr_strcmp(prec, "hours")) ? 0 : (0 == xr_strcmp(prec, "seconds") ? 2 : 1);
	p.trackSmoothSpeed = flt("track_smooth_speed", p.trackSmoothSpeed);
	m_paramsBase = m_params;
}

CUISleepWnd::CUISleepWnd()
{
	m_bWorkInPause = true;
	CUIXml xml;
	if (xml.Load(CONFIG_PATH, UI_PATH, "ui_sleep_dialog.xml")) Init(xml);
}

CUISleepWnd::~CUISleepWnd()
{
	ActionRepeaters()->UnregisterOwner(this);
	for (auto& s : m_snd) s.destroy();
}

void CUISleepWnd::Init(CUIXml& xml)
{
	SetWndPos(Fvector2().set(0.f, 0.f));
	SetWndSize(Fvector2().set(UI_BASE_WIDTH, UI_BASE_HEIGHT));
	LoadSleepParams(xml);
	LoadSleepSounds(xml, m_snd, eSndCount);

	m_background = UIHelper::CreateStatic(xml, "background", this);
	m_sleepStatic = UIHelper::CreateStatic(xml, "sleep_static", m_background, true, 0);
	m_sleepStatic2 = UIHelper::CreateStatic(xml, "sleep_static", m_background, true, 0);
	CreateSleepCover(xml, m_background);

	m_marker = UIHelper::CreateStatic(xml, "st_marker", m_background);
	if (m_marker) m_markerBase = m_marker->GetWndPos();
	BuildHourLabels(xml);

	m_timeTrack = UIHelper::CreateTrackBar(xml, "time_track", m_background);
	Register(m_timeTrack);
	if (m_timeTrack) m_trackVisual = float(m_timeTrack->GetIValue());
	m_trackTarget = m_trackVisual;

	auto bindBtn = [&](LPCSTR node, void (CUISleepWnd::*cb)(CUIWindow*, void*))
	{
		CUI3tButton* btn = UIHelper::Create3tButton(xml, node, m_background);
		Register(btn);
		AddCallback(btn, BUTTON_CLICKED, CUIWndCallback::void_function(this, cb));
	};
	bindBtn("btn_sleep", &CUISleepWnd::OnButtonSleep);
	bindBtn("btn_cancel", &CUISleepWnd::OnButtonCancel);

	m_stTimeNow = UIHelper::CreateStatic(xml, "st_time_now", m_background, false);
	m_stSleepDuration = UIHelper::CreateStatic(xml, "st_sleep_duration", m_background, false);
	m_stWakeTime = UIHelper::CreateStatic(xml, "st_wake_time", m_background, false);
	auto overrideFmt = [&](LPCSTR node, shared_str& dst)
	{
		if (!xml.NavigateToNode(node, 0)) return;
		if (LPCSTR fmt = xml.ReadAttrib(node, 0, "format", nullptr); fmt && fmt[0]) dst = fmt;
	};
	overrideFmt("st_time_now", m_params.fmtTimeNow);
	overrideFmt("st_sleep_duration", m_params.fmtSleepDuration);
	overrideFmt("st_wake_time", m_params.fmtWakeTime);

	m_warningBox = new CUIMessageBoxEx();
	m_warningBox->SetAutoDelete(true);
	AttachChild(m_warningBox);
	m_warningBox->Show(false);
	Register(m_warningBox);
	AddCallback(m_warningBox, MESSAGE_BOX_OK_CLICKED, CUIWndCallback::void_function(this, &CUISleepWnd::OnMessageBoxOk));

	if (xml.NavigateToNode(kPreset, 0))
	{
		m_hasPresetTpl = true;
		m_presetTplPos.set(xml.ReadAttribFlt(kPreset, 0, "x", 0.f), xml.ReadAttribFlt(kPreset, 0, "y", 0.f));
		m_presetTplSize.set(xml.ReadAttribFlt(kPreset, 0, "width", 60.f), xml.ReadAttribFlt(kPreset, 0, "height", 20.f));
	}

	SetupPanorama(m_params.panoramaTexture, m_sleepStatic, m_sleepStatic2, m_panoTex, m_panoDisplay, m_hourStepPx, m_params.panoramaHours);
	if (m_sleepStatic)
		m_panoBasePos = m_sleepStatic->GetWndPos();
	ApplyTrackBounds();
	ActionRepeaters()->Register(this, kUI_LEFT);
	ActionRepeaters()->Register(this, kUI_RIGHT);
}

void CUISleepWnd::BuildHourLabels(CUIXml& xml)
{
	m_hourLabels.clear();
	if (!m_background) return;

	if (xml.NavigateToNode(kStrip, 0))
	{
		auto flt = [&](LPCSTR a, float fb) { return xml.ReadAttribFlt(kStrip, 0, a, fb); };
		int count = xml.ReadAttribInt(kStrip, 0, "count", 24);
		clamp(count, 1, 24);
		const float x = flt("x", 0.f), y = flt("y", 0.f), w = flt("width", 0.f), h = flt("height", 15.f);
		const float pad = flt("padding", 0.f), padL = flt("pad_left", pad), padR = flt("pad_right", pad);
		const float spacing = flt("spacing", 0.f);
		float labelW = flt("label_width", 0.f);
		const float usable = std::max(0.f, w - padL - padR);
		if (labelW <= 0.f)
			labelW = std::max(1.f, count > 1 ? (usable - spacing * float(count - 1)) / float(count) : usable);

		m_hourLabels.reserve(u32(count));
		for (int i = 0; i < count; ++i)
		{
			auto* st = new CUIStatic();
			st->SetAutoDelete(true);
			m_background->AttachChild(st);
			CUIXmlInit::InitStatic(xml, kStrip, 0, st);
			st->SetWndPos(Fvector2().set(x + padL + float(i) * (labelW + spacing), y));
			st->SetWndSize(Fvector2().set(labelW, h));
			m_hourLabels.push_back(st);
		}
		return;
	}

	m_hourLabels.resize(24, nullptr);
	for (int i = 0; i < 24; ++i)
	{
		string32 node; xr_sprintf(node, "sleep_st_%d", i + 1);
		m_hourLabels[i] = UIHelper::CreateStatic(xml, node, m_background, false);
	}
}

void CUISleepWnd::ApplyTrackBounds()
{
	if (!m_timeTrack) return;
	m_timeTrack->SetOptIBounds(m_params.minHours, m_params.maxHours);
	const int cur = m_timeTrack->GetIValue();
	if (cur < m_params.minHours || cur > m_params.maxHours) m_timeTrack->SetIValue(m_params.minHours);
	SnapTrack(float(m_timeTrack->GetIValue()));
}

void CUISleepWnd::ApplySessionOverrides()
{
	m_params = m_paramsBase;
	if (m_session.hasHoursRange)
	{
		m_params.minHours = std::max(1, m_session.minHours);
		m_params.maxHours = std::max(m_params.minHours, m_session.maxHours);
	}
	if (m_session.hasAllowBleeding)
		m_params.allowSleepWithBleeding = m_session.allowSleepWithBleeding;
	if (m_session.hasRestorePower)
		m_params.restorePower = m_session.restorePower;
	if (m_session.hasMute)
	{
		m_params.muteMusic = m_session.muteMusic;
		m_params.muteEffects = m_session.muteEffects;
	}
	ApplyTrackBounds();
	RebuildPresetButtons();
}

void CUISleepWnd::ClearSessionFlag(bool& flag)
{
	if (!flag) return;
	flag = false;
	ApplySessionOverrides();
}

void CUISleepWnd::SetSleepHoursRange(int minHours, int maxHours)
{
	m_session.hasHoursRange = true;
	m_session.minHours = minHours;
	m_session.maxHours = maxHours;
	ApplySessionOverrides();
}
void CUISleepWnd::ClearSleepHoursRange() { ClearSessionFlag(m_session.hasHoursRange); }

void CUISleepWnd::SetSleepAllowBleeding(bool allow)
{
	m_session.hasAllowBleeding = true;
	m_session.allowSleepWithBleeding = allow;
	ApplySessionOverrides();
}
void CUISleepWnd::ClearSleepAllowBleeding() { ClearSessionFlag(m_session.hasAllowBleeding); }

void CUISleepWnd::SetSleepRestorePower(float power)
{
	m_session.hasRestorePower = true;
	m_session.restorePower = power;
	ApplySessionOverrides();
}
void CUISleepWnd::ClearSleepRestorePower() { ClearSessionFlag(m_session.hasRestorePower); }

void CUISleepWnd::SetSleepMute(bool muteMusic, bool muteEffects)
{
	m_session.hasMute = true;
	m_session.muteMusic = muteMusic;
	m_session.muteEffects = muteEffects;
	ApplySessionOverrides();
}
void CUISleepWnd::ClearSleepMute() { ClearSessionFlag(m_session.hasMute); }

void CUISleepWnd::ClearSleepSessionOverrides()
{
	m_session = SleepSessionOverrides{};
	ApplySessionOverrides();
}

void CUISleepWnd::SetSleepBlocked(bool blocked, LPCSTR warningText)
{
	m_sleepBlocked = blocked;
	m_sleepBlockedWarning = (warningText && warningText[0]) ? shared_str(warningText) : shared_str{};
}

void CUISleepWnd::SetHourPresets(xr_vector<int> hours)
{
	m_presetHours = std::move(hours);
	RebuildPresetButtons();
}

void CUISleepWnd::RebuildPresetButtons()
{
	for (auto& e : m_presets)
	{
		if (!e.btn) continue;
		m_background->DetachChild(e.btn);
		xr_delete(e.btn);
	}
	m_presets.clear();
	if (!m_hasPresetTpl || m_presetHours.empty() || !m_background) return;

	CUIXml xml;
	const bool hasXml = xml.Load(CONFIG_PATH, UI_PATH, "ui_sleep_dialog.xml") && xml.NavigateToNode(kPreset, 0);
	float curX = m_presetTplPos.x;
	LPCSTR suffix = *g_pStringTable->translate(m_params.hourLabelSuffix);

	for (int h : m_presetHours)
	{
		if (h < m_params.minHours || h > m_params.maxHours) continue;
		auto* btn = new CUI3tButton();
		btn->SetAutoDelete(false);
		m_background->AttachChild(btn);
		if (hasXml) CUIXmlInit::Init3tButton(xml, kPreset, 0, btn);
		else btn->InitButton(m_presetTplPos, m_presetTplSize);
		btn->SetWndPos(Fvector2().set(curX, m_presetTplPos.y));
		btn->SetWndSize(m_presetTplSize);
		string32 label; xr_sprintf(label, "%d%s", h, suffix);
		btn->TextItemControl()->SetText(label);
		Register(btn);
		AddCallback(btn, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUISleepWnd::OnPresetClicked));
		m_presets.push_back({ btn, h });
		curX += m_presetTplSize.x + m_params.presetSpacing;
	}
}

void CUISleepWnd::ShowSleepDialog()
{
	if (!HasInitializedLayout()) { Msg("! [sleep] ShowSleepDialog skipped: no layout"); return; }
	m_timeTrack->SetCurrentOptValue();
	SnapTrack(float(m_timeTrack->GetIValue()));
	TestAndShow();
	ActionRepeaters()->ResetAll(this);
}

void CUISleepWnd::ShowSleepDialog(int hours)
{
	if (!HasInitializedLayout()) return;
	SetSelectedHours(hours, true);
	TestAndShow();
	ActionRepeaters()->ResetAll(this);
}

void CUISleepWnd::HideSleepDialog()
{
	if (!IsShown()) return;
	PlaySnd(m_snd[eSndCancel]._p ? eSndCancel : eSndClose);
	HideDialog();
	GiveTutorialSleep();
	ClearSleepSessionOverrides();
}

bool CUISleepWnd::ConfirmSleep()
{
	if (!IsShown()) return false;
	OnButtonSleep(nullptr, nullptr);
	return true;
}

void CUISleepWnd::ForceSleep(int hours)
{
	if (!HasInitializedLayout() || m_camPhase != 0) return;
	SetSelectedHours(hours, true);
	if (IsShown()) HideDialog();
	OnConfirmSleep();
}

bool CUISleepWnd::AbortSleep()
{
	const u8 phase = m_camPhase;
	if (IsShown())
	{
		HideSleepDialog();
		ScriptCb("actor_on_sleep_aborted", 0);
		return true;
	}
	if (phase == 0) return false;

	m_camPhase = 0;
	RemoveSleepEffectors();

	if (phase == 1)
	{
		RestoreSleepAudio();
		CallXrEffect("xr_effects.enable_ui");
		if (CActor* a = Actor())
		{
			GiveTutorialSleep();
			a->DisableInfoPortion("actor_is_sleeping");
		}
		ClearSleepSessionOverrides();
		ScriptCb("actor_on_sleep_aborted", 1);
		return true;
	}

	WakeUp();
	ScriptCb("actor_on_sleep_aborted", 2);
	return true;
}

bool CUISleepWnd::IsActorSleeping() const
{
	if (m_camPhase != 0) return true;
	if (CActor* a = Actor())
		return a->HasInfo("actor_is_sleeping");
	return false;
}

void CUISleepWnd::ShowWarning(const shared_str& text)
{
	if (!m_warningBox || !text || text.size() == 0) return;
	PlaySnd(eSndWarning);
	m_warningBox->InitMessageBox(*m_params.warningBoxTemplate);
	m_warningBox->SetText(*text);
	m_warningBox->ShowDialog(true);
}

bool CUISleepWnd::CanSleepNow(int hours, shared_str& outWarning) const
{
	outWarning = shared_str{};
	if (m_sleepBlocked)
	{
		outWarning = m_sleepBlockedWarning;
		return false;
	}

	lua_State* L = ai().script_engine().lua();
	if (!L) return true;

	lua_createtable(L, 0, 2);
	const int flags = lua_gettop(L);
	SetBool(L, flags, "allow", true);
	lua_pushstring(L, "");
	lua_setfield(L, flags, "warning_text");

	lua_getglobal(L, "SendScriptCallback");
	if (!lua_isfunction(L, -1))
	{
		lua_pop(L, 2);
		return true;
	}
	lua_pushstring(L, "actor_on_can_sleep");
	lua_pushnumber(L, hours);
	lua_pushvalue(L, flags);
	if (!LuaPcall(L, 3, 0))
	{
		lua_pop(L, 1);
		return true;
	}

	const bool allow = GetBool(L, flags, "allow", true);
	outWarning = GetStr(L, flags, "warning_text");
	lua_pop(L, 1);
	return allow;
}

void CUISleepWnd::InitializeLayout()
{
	const u32 cur = LevelHours();
	LPCSTR suffix = *g_pStringTable->translate(m_params.hourLabelSuffix);
	for (u32 i = 0; i < m_hourLabels.size(); ++i)
	{
		CUIStatic* st = m_hourLabels[i];
		if (!st || !st->TextItemControl()) continue;
		string64 label; xr_sprintf(label, "%d%s", (cur + i + 1) % 24, suffix);
		st->TextItemControl()->SetText(label);
	}
	UpdatePanorama(true);
	UpdateMarker();
	m_lastTimeInfoHours = -1;
	UpdateTimeInfo();
}

void CUISleepWnd::UpdateTimeInfo()
{
	if (!m_stTimeNow && !m_stSleepDuration && !m_stWakeTime) return;
	const int hours = SelectedHours();
	if (hours == m_lastTimeInfoHours) return;
	m_lastTimeInfoHours = hours;

	using namespace InventoryUtilities;
	const auto prec = static_cast<ETimePrecision>(m_params.timePrecision);
	const u64 now = Level().GetGameTime();
	const u64 wake = now + u64(hours) * 3600ull * 1000ull;
	string256 buf;
	auto setSt = [&](CUIStatic* st, LPCSTR text) { if (st) st->TextItemControl()->SetText(text); };
	if (m_stTimeNow)
	{
		xr_sprintf(buf, ResolveFmt(m_params.fmtTimeNow), *GetTimeAsString(now, prec, m_params.timeSeparator, true));
		setSt(m_stTimeNow, buf);
	}
	if (m_stSleepDuration)
	{
		xr_sprintf(buf, ResolveFmt(m_params.fmtSleepDuration), hours, *g_pStringTable->translate(m_params.hourLabelSuffix));
		setSt(m_stSleepDuration, buf);
	}
	if (m_stWakeTime)
	{
		xr_sprintf(buf, ResolveFmt(m_params.fmtWakeTime), *GetTimeAsString(wake, prec, m_params.timeSeparator, true));
		setSt(m_stWakeTime, buf);
	}
}

float CUISleepWnd::WrapHoursDelta(float from, float to, float hoursN)
{
	float delta = to - from;
	if (delta > hoursN * 0.5f) delta -= hoursN;
	if (delta < -hoursN * 0.5f) delta += hoursN;
	return delta;
}

float CUISleepWnd::PanoramaTargetHours() const
{
	const float hoursN = static_cast<float>(m_params.panoramaHours);
	float hours = static_cast<float>(LevelHours());
	if (m_params.panoramaBindWake)
		hours += static_cast<float>(SelectedHours());
	return PositiveMod(hours, hoursN);
}

void CUISleepWnd::SetPanoPanel(CUIStatic* st, float x, float w, float h, float u0, float v0, float u1, float v1, bool show)
{
	if (!st) return;
	st->Show(show);
	if (!show) return;
	st->SetStretchTexture(true);
	st->SetTextureOffset(0.f, 0.f);
	st->SetTextureColor(color_argb(255, 255, 255, 255));
	st->SetWndPos(Fvector2().set(m_panoBasePos.x + x, m_panoBasePos.y));
	st->SetWndSize(Fvector2().set(std::max(0.f, w), h));
	st->SetTextureRect(Frect().set(u0, v0, u1, v1));
}

void CUISleepWnd::ApplyPanoramaHours(float hoursMod)
{
	if (!m_sleepStatic) return;

	const float hoursN = static_cast<float>(std::max(1, m_params.panoramaHours));
	const bool native = m_params.panoramaNativeScale;
	const float texScale = native ? std::max(1.f, m_params.panoramaTexScale) : 1.f;
	const float logicalW = m_panoTex.width() / texScale;
	const float logicalH = m_panoTex.height() / texScale;
	const float dispW = m_panoDisplay.x;
	const float dispH = m_panoDisplay.y;
	const float drawH = native ? std::min(logicalH, dispH) : dispH;
	const float cropUi = native ? (logicalH - drawH) : 0.f;
	const float srcY1 = native ? (m_panoTex.y1 + cropUi * 0.5f * texScale) : m_panoTex.y1;
	const float srcY2 = native ? (srcY1 + drawH * texScale) : m_panoTex.y2;

	// Native clamp: single blit inside [0 .. logicalW-dispW]
	if (native && !m_params.panoramaWrap)
	{
		const float span = std::max(0.f, logicalW - dispW);
		const float offset = (span > 0.f) ? (span / hoursN * hoursMod) : 0.f;
		SetPanoPanel(m_sleepStatic, 0.f, dispW, drawH,
			m_panoTex.x1 + offset * texScale, srcY1,
			m_panoTex.x1 + (offset + dispW) * texScale, srcY2, true);
		SetPanoPanel(m_sleepStatic2, 0.f, 0.f, drawH, 0.f, 0.f, 0.f, 0.f, false);
		return;
	}

	if (!m_sleepStatic2)
	{
		const float span = std::max(0.f, logicalW - dispW);
		const float offset = std::min(logicalW / hoursN * hoursMod, span);
		SetPanoPanel(m_sleepStatic, 0.f, dispW, drawH,
			m_panoTex.x1 + offset * texScale, srcY1,
			m_panoTex.x1 + (offset + dispW) * texScale, srcY2, true);
		return;
	}

	float leftW = 0.f, rightW = 0.f;
	float leftU0 = 0.f, leftU1 = 0.f, rightU0 = 0.f, rightU1 = 0.f;

	if (native)
	{
		float offset = logicalW / hoursN * hoursMod;
		clamp(offset, 0.f, logicalW);
		leftW = std::min(logicalW - offset, dispW);
		rightW = dispW - leftW;
		leftU0 = m_panoTex.x1 + offset * texScale;
		leftU1 = m_panoTex.x1 + (offset + leftW) * texScale;
		rightU0 = m_panoTex.x1;
		rightU1 = m_panoTex.x1 + rightW * texScale;
	}
	else
	{
		// Legacy CoP: remaining atlas stretched into left pane, head into right pane.
		float texDelta = m_panoTex.width() / hoursN * hoursMod;
		float dispDelta = m_hourStepPx * hoursMod;
		clamp(texDelta, 0.f, m_panoTex.width());
		clamp(dispDelta, 0.f, dispW);
		leftW = dispW - dispDelta;
		rightW = dispDelta;
		leftU0 = m_panoTex.x1 + texDelta;
		leftU1 = m_panoTex.x2;
		rightU0 = m_panoTex.x1;
		rightU1 = m_panoTex.x1 + texDelta;
	}

	constexpr float kJoinOverlap = 1.f;
	const bool both = leftW > 0.5f && rightW > 0.5f;
	const float leftDrawW = both ? std::min(dispW, leftW + kJoinOverlap) : leftW;
	if (native && both)
		leftU1 = leftU0 + leftDrawW * texScale;

	SetPanoPanel(m_sleepStatic, 0.f, leftDrawW, drawH, leftU0, srcY1, leftU1, srcY2, leftW > 0.5f);
	SetPanoPanel(m_sleepStatic2, leftW, rightW, drawH, rightU0, srcY1, rightU1, srcY2, rightW > 0.5f);
}

void CUISleepWnd::UpdatePanorama(bool instant)
{
	if (!m_sleepStatic) return;

	const float hoursN = static_cast<float>(std::max(1, m_params.panoramaHours));
	const float target = PanoramaTargetHours();

	if (instant || m_params.panoramaSmoothSpeed <= 0.f)
	{
		m_panoScrollHours = target;
	}
	else
	{
		const float curMod = PositiveMod(m_panoScrollHours, hoursN);
		const float delta = WrapHoursDelta(curMod, target, hoursN);
		m_panoScrollHours += delta * (1.f - expf(-m_params.panoramaSmoothSpeed * Device.fTimeDelta));
		const float nextMod = PositiveMod(m_panoScrollHours, hoursN);
		if (fabsf(WrapHoursDelta(nextMod, target, hoursN)) < 0.002f)
			m_panoScrollHours += WrapHoursDelta(nextMod, target, hoursN);
	}

	ApplyPanoramaHours(PositiveMod(m_panoScrollHours, hoursN));
}

void CUISleepWnd::UpdateMarker()
{
	if (!m_marker || !m_timeTrack) return;
	CUI3tButton* slider = m_timeTrack->GetSlider();
	if (!slider) return;
	const float sliderCenter = m_timeTrack->GetWndPos().x + slider->GetWndPos().x + slider->GetWidth() * 0.5f;
	float x = std::max(m_params.markerMinX, sliderCenter - m_marker->GetWidth() * 0.5f);
	m_marker->SetWndPos(Fvector2().set(x, m_markerBase.y));
}

void CUISleepWnd::TestAndShow()
{
	bool bleed = false, rad = false;
	if (CActor* a = Actor())
	{
		bleed = !m_params.allowSleepWithBleeding && a->conditions().BleedingSpeed() > 0.f;
		rad = a->conditions().GetRadiation() > 0.f;
	}
	InitializeLayout();

	const shared_str warn = (bleed && rad) ? m_params.warningBoth
		: bleed ? m_params.warningBleeding
		: rad ? m_params.warningRadiation
		: shared_str{};

	if (warn && warn.size() > 0)
	{
		ShowWarning(warn);
		return;
	}

	shared_str canWarn;
	if (!CanSleepNow(SelectedHours(), canWarn))
	{
		if (canWarn && canWarn.size() > 0)
			ShowWarning(canWarn);
		return;
	}

	PlaySnd(eSndOpen);
	ShowDialog(true);
}

void CUISleepWnd::OnConfirmSleep()
{
	CActor* actor = Actor();
	if (!actor) return;

	CallXrEffect("xr_effects.disable_ui");
	m_camPhase = 1;
	AddCamCB(this, *m_params.camAnm, m_params.camId);
	AddPP(*m_params.ppEffector, m_params.ppId);
	actor->GiveInfoPortion("actor_is_sleeping");

	m_savedMusic = psSoundVMusic;
	m_savedEffects = psSoundVEffects;
	if (m_params.muteMusic) psSoundVMusic = 0.f;
	if (m_params.muteEffects) psSoundVEffects = 0.f;

	if (lua_State* L = ai().script_engine().lua())
		if (const int mgr = PushMgr(L, "surge_manager", "get_surge_manager"))
		{ SetBool(L, mgr, "skip_message", false); lua_pop(L, 1); }
}

void CUISleepWnd::OnCamEffectorDone()
{
	if (m_camPhase == 1) { m_camPhase = 2; OnCamPhase1Done(); }
	else if (m_camPhase == 2) { m_camPhase = 0; WakeUp(); }
}

void CUISleepWnd::OnCamPhase1Done()
{
	const int hours = SelectedHours();
	ScriptCb("actor_on_before_sleep", hours);
	AddCamCB(this, *m_params.camAnm, m_params.camId);
	ChangeGameTimeHours(u32(hours));
	ApplySleepWeatherAfterRest();
	if (CActor* a = Actor()) a->conditions().SetPower(m_params.restorePower);
	Msg("dream_callback: time forwarded on [%d]", hours);
	ScriptCb("actor_on_sleep", hours);
}

void CUISleepWnd::WakeUp()
{
	CallXrEffect("xr_effects.enable_ui");
	RestoreSleepAudio();
	if (CActor* a = Actor())
	{
		GiveTutorialSleep();
		a->DisableInfoPortion("actor_is_sleeping");
	}
	ClearSleepSessionOverrides();
}

void CUISleepWnd::RestoreSleepAudio()
{
	if (m_params.muteMusic) psSoundVMusic = m_savedMusic;
	if (m_params.muteEffects) psSoundVEffects = m_savedEffects;
	m_savedMusic = m_savedEffects = 0.f;
}

void CUISleepWnd::RemoveSleepEffectors()
{
	if (CActor* a = Actor())
	{
		a->Cameras().RemoveCamEffector(static_cast<ECamEffectorType>(m_params.camId));
		a->Cameras().RemovePPEffector(static_cast<EEffectorPPType>(m_params.ppId));
	}
}

int CUISleepWnd::SelectedHours() const
{
	if (!m_timeTrack) return 1;
	const int v = m_trackSmoothActive ? iFloor(m_trackVisual + 0.5f) : m_timeTrack->GetIValue();
	return clampr(v, m_params.minHours, m_params.maxHours);
}

void CUISleepWnd::SnapTrack(float hours)
{
	m_trackVisual = m_trackTarget = hours;
	m_trackSmoothActive = false;
	PlaceTrackSlider(m_timeTrack, hours, m_params.minHours, m_params.maxHours);
}

void CUISleepWnd::SetSelectedHours(int hours, bool instant)
{
	if (!m_timeTrack) return;
	clamp(hours, m_params.minHours, m_params.maxHours);
	m_trackTarget = float(hours);
	if (instant || m_params.trackSmoothSpeed <= 0.f)
	{
		m_timeTrack->SetIValue(hours);
		SnapTrack(float(hours));
		return;
	}
	m_trackSmoothActive = true;
}

void CUISleepWnd::StepTrack(bool right)
{
	if (!m_timeTrack) return;
	const int next = SelectedHours() + (right ? 1 : -1);
	if (next < m_params.minHours || next > m_params.maxHours) return;
	SetSelectedHours(next, false);
	PlaySnd(eSndTrack);
}

void CUISleepWnd::UpdateTrackSmooth()
{
	if (!m_timeTrack) return;

	if (m_timeTrack->IsMouseCapturing())
	{
		m_trackVisual = m_trackTarget = float(m_timeTrack->GetIValue());
		m_trackSmoothActive = false;
		m_trackWasCapturing = true;
		return;
	}

	if (m_trackWasCapturing)
	{
		m_trackWasCapturing = false;
		SnapTrack(float(m_timeTrack->GetIValue()));
	}

	if (m_params.trackSmoothSpeed <= 0.f || !m_trackSmoothActive)
	{
		m_trackVisual = m_trackTarget = float(m_timeTrack->GetIValue());
		return;
	}

	m_trackVisual += (m_trackTarget - m_trackVisual) * (1.f - expf(-m_params.trackSmoothSpeed * Device.fTimeDelta));
	if (fabsf(m_trackTarget - m_trackVisual) < 0.02f)
	{
		m_trackVisual = m_trackTarget;
		m_trackSmoothActive = false;
		m_timeTrack->SetIValue(iFloor(m_trackTarget + 0.5f));
	}
	PlaceTrackSlider(m_timeTrack, m_trackVisual, m_params.minHours, m_params.maxHours);
}

void CUISleepWnd::Update()
{
	CUIDialogWnd::Update();
	UpdateTrackSmooth();
	if (IsShown())
		UpdatePanorama(false);
	UpdateMarker();
	UpdateTimeInfo();
}

void CUISleepWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

bool CUISleepWnd::HandleUiAction(int action, bool gamepad, bool hold)
{
	if (gamepad && !m_timeTrack) return false;
	if (hold && action != kUI_LEFT && action != kUI_RIGHT) return false;

	switch (action)
	{
	case kUI_BACK: OnButtonCancel(nullptr, nullptr); return true;
	case kUI_ACCEPT: OnButtonSleep(nullptr, nullptr); return true;
	case kUI_LEFT:
	case kUI_RIGHT:
	{
		const bool right = (action == kUI_RIGHT);
		if (!gamepad) { StepTrack(right); return true; }
		const auto act = right ? kUI_RIGHT : kUI_LEFT;
		const auto opp = right ? kUI_LEFT : kUI_RIGHT;
		if (hold)
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, act) && !any_binded_key_for_action_pressed_c(opp))
				StepTrack(right);
			return true;
		}
		if (!any_binded_key_for_action_pressed_c(opp)) StepTrack(right);
		ActionRepeaters()->SetActionStarted(this, act);
		return true;
	}
	default: return false;
	}
}

bool CUISleepWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if (keyboard_action == WINDOW_KEY_PRESSED)
	{
		if (HandleUiAction(get_binded_action(dik, agUIGeneral))) return true;
		if (is_binded(kQUIT, dik)) { OnButtonCancel(nullptr, nullptr); return true; }
	}
	return CUIDialogWnd::OnKeyboardAction(dik, keyboard_action);
}

void CUISleepWnd::OnButtonSleep(CUIWindow*, void*)
{
	PlaySnd(eSndSleep); HideDialog(); OnConfirmSleep();
}

void CUISleepWnd::OnButtonCancel(CUIWindow*, void*)
{
	HideSleepDialog();
}

void CUISleepWnd::OnMessageBoxOk(CUIWindow*, void*)
{
	GiveTutorialSleep();
	ClearSleepSessionOverrides();
}

void CUISleepWnd::OnPresetClicked(CUIWindow* w, void*)
{
	for (const auto& e : m_presets)
	{
		if (e.btn != w) continue;
		PlaySnd(eSndPreset);
		SetSelectedHours(e.hours);
		m_lastTimeInfoHours = -1;
		UpdateMarker();
		UpdateTimeInfo();
		if (m_params.presetsConfirm) { HideDialog(); OnConfirmSleep(); }
		return;
	}
}

bool CUISleepWnd::OnGamepadKeyAction(int id, EUIMessages action)
{
	if (!m_timeTrack) return CUIDialogWnd::OnGamepadKeyAction(id, action);
	if (CUIDialogWnd::OnGamepadKeyAction(id, action)) return true;
	if (action != WINDOW_KEY_PRESSED) return false;
	return HandleUiAction(get_binded_action(id, agUIGeneral), true, false);
}

bool CUISleepWnd::OnGamepadKeyHold(int id)
{
	if (!m_timeTrack) return CUIDialogWnd::OnGamepadKeyHold(id);
	return HandleUiAction(get_binded_action(id, agUIGeneral), true, true);
}
