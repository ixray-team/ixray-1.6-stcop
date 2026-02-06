#include "StdAfx.h"
#include <algorithm>
#include "UICompassBar.h"
#include "../Level.h"
#include "../map_manager.h"
#include "../map_location.h"
#include "../map_location_defs.h"
#include "../Actor.h"
#include "../../xrEngine/Device.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../xrUI/Widgets/UILines.h"
#include "../../xrEngine/GameFont.h"
#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "../../xrCore/vector.h"

CUICompassBar::CUICompassBar()
    : _background(nullptr),
      _layerBg(nullptr),
      _strip(nullptr),
      _layerFg(nullptr),
      _activeMarker(nullptr),
      _activeDistText(nullptr),
      _activeTargetLoc(nullptr),
      _lastActiveLoc(nullptr),
      _activeTargetCurX(0.0f),
      _fov(_kDefaultFovDeg * _kPi / 180.0f),
      _stripWidth(0.0f),
      _stripTexWidth(_kDefaultStripTexWidth),
      _stripTexLoop(true),
      _collectSpotsTimer(0.0f)
{
    _cfg.stripY = 0.0f;
    _cfg.markersY = 0.0f;
    _cfg.activePadding = 8.0f;
    _cfg.smoothingSpeed = 10.0f;
    _cfg.distY = -18.0f;
    _cfg.activeOffsetX = 0.0f;
    _cfg.activeOffsetY = 0.0f;
    _cfg.textOffsetX = 38.0f;
    _cfg.textOffsetY = 10.0f;
}

CUICompassBar::~CUICompassBar()
{
    _poolSpots.clear();
    _poolSpotTextureNames.clear();
}

void CUICompassBar::Init()
{
    CUIXml uiXml;
    uiXml.Load(CONFIG_PATH, UI_PATH, "compass_bar.xml");
    CUIXmlInit xmlInit;
    if (!uiXml.NavigateToNode("compass_bar", 0))
    {
        Msg("! CUICompassBar::Init: node 'compass_bar' not found in compass_bar.xml");
        return;
    }
    InitWindowAndBackground(uiXml, xmlInit);
    InitLayoutFromXml(uiXml);

    _layerBg = new CUIWindow();
    _layerBg->SetAutoDelete(true);
    _layerBg->SetWndSize(GetWndSize());
    _layerBg->SetWndPos(Fvector2().set(0.0f, 0.0f));
    AttachChild(_layerBg);

    InitCompassDial(uiXml, xmlInit);

    _layerFg = new CUIWindow();
    _layerFg->SetAutoDelete(true);
    _layerFg->SetWndSize(GetWndSize());
    _layerFg->SetWndPos(Fvector2().set(0.0f, 0.0f));
    AttachChild(_layerFg);

    InitActiveTargetWidgets(uiXml);
    InitStripVectorIcon(uiXml);
}

void CUICompassBar::InitWindowAndBackground(CUIXml& uiXml, CUIXmlInit& xmlInit)
{
    xmlInit.InitWindow(uiXml, "compass_bar", 0, this);
    _background = UIHelper::CreateStatic(uiXml, "compass_bar:background", this);
    if (!_background)
    {
        _background = new CUIStatic();
        _background->SetAutoDelete(true);
        _background->SetWndSize(GetWndSize());
        AttachChild(_background);
    }
    else
    {
        _background->SetWndSize(GetWndSize());
        _background->SetWndPos(Fvector2().set(0.0f, 0.0f));
    }
}

void CUICompassBar::InitLayoutFromXml(CUIXml& uiXml)
{
    float fovDeg = uiXml.ReadAttribFlt("compass_bar", 0, "fov_angle", _kDefaultFovDeg);
    _fov = fovDeg * _kPi / 180.0f;
    ParseSpots(uiXml, "compass_bar:spots");

    const char* layoutPath = "compass_bar:active_target";
    if (!uiXml.NavigateToNode(layoutPath, 0))
    {
        layoutPath = "compass_bar:layout";
    }
    if (uiXml.NavigateToNode(layoutPath, 0))
    {
        _cfg.stripY = uiXml.ReadAttribFlt(layoutPath, 0, "strip_y", 0.0f);
        _cfg.markersY = uiXml.ReadAttribFlt(layoutPath, 0, "markers_y", 0.0f);
        _cfg.distY = uiXml.ReadAttribFlt(layoutPath, 0, "dist_y", -18.0f);
        _cfg.smoothingSpeed = uiXml.ReadAttribFlt(layoutPath, 0, "smoothing_speed", 10.0f);
        _cfg.activePadding = uiXml.ReadAttribFlt(layoutPath, 0, "active_target_padding", 8.0f);
        _cfg.activeOffsetX = uiXml.ReadAttribFlt(layoutPath, 0, "offset_x", 0.0f);
        _cfg.activeOffsetY = uiXml.ReadAttribFlt(layoutPath, 0, "offset_y", 0.0f);
        _cfg.textOffsetX = uiXml.ReadAttribFlt(layoutPath, 0, "text_offset_x", 38.0f);
        _cfg.textOffsetY = uiXml.ReadAttribFlt(layoutPath, 0, "text_offset_y", 10.0f);
    }
}

void CUICompassBar::ParseSpots(CUIXml& uiXml, const char* path)
{
    _spotCfg.show = true;
    _spotCfg.offsetX = 0.0f;
    _spotCfg.offsetY = 0.0f;
    _spotCfg.align = 1;
    _spotCfg.spotWidth = 14.0f;
    _spotCfg.spotHeight = 17.0f;
    _spotCfg.maxDistance = -1.0f;
    if (!uiXml.NavigateToNode(path, 0))
    {
        return;
    }
    _spotCfg.show = uiXml.ReadAttribInt(path, 0, "show", 1) != 0;
    _spotCfg.offsetX = uiXml.ReadAttribFlt(path, 0, "x", 0.0f);
    _spotCfg.offsetY = uiXml.ReadAttribFlt(path, 0, "y", 0.0f);
    _spotCfg.align = ParseAlign(uiXml.ReadAttrib(path, 0, "align", "c"));
    _spotCfg.maxDistance = uiXml.ReadAttribFlt(path, 0, "max_distance", -1.0f);
    _spotCfg.collectInterval = uiXml.ReadAttribFlt(path, 0, "collect_interval", 0.1f);
    if (_spotCfg.collectInterval <= 0.0f)
    {
        _spotCfg.collectInterval = 0.1f;
    }
    string_path tmplPath;
    xr_strconcat(tmplPath, path, ":spot_template");
    if (uiXml.NavigateToNode(tmplPath, 0))
    {
        _spotCfg.spotWidth = uiXml.ReadAttribFlt(tmplPath, 0, "width", 14.0f);
        _spotCfg.spotHeight = uiXml.ReadAttribFlt(tmplPath, 0, "height", 17.0f);
    }
}

void CUICompassBar::InitCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit)
{
    const char* stripPath = "compass_bar:strip";
    const char* cardinalsPath = "compass_bar:cardinal_points";
    if (uiXml.NavigateToNode("compass_bar:compass_dial", 0))
    {
        stripPath = "compass_bar:compass_dial:strip";
        cardinalsPath = "compass_bar:compass_dial:cardinal_points";
    }
    _strip = UIHelper::CreateStatic(uiXml, stripPath, this);
    _stripXmlPath = stripPath;
    _stripTexWidth = uiXml.ReadAttribFlt(stripPath, 0, "tex_width", _kDefaultStripTexWidth);
    _stripTexLoop = uiXml.ReadAttribInt(stripPath, 0, "tex_loop", 1) != 0;
    _stripSvgPath = nullptr;
    string_path svgNodePath;
    xr_strconcat(svgNodePath, stripPath, ":svg");
    if (uiXml.NavigateToNode(svgNodePath, 0))
    {
        LPCSTR svgText = uiXml.Read(svgNodePath, 0, "");
        if (svgText && *svgText)
        {
            _stripSvgPath = svgText;
        }
    }
    if (_strip)
    {
        _stripWidth = _strip->GetWidth();
    }
    float defY = 0.0f;
    float defW = 16.0f;
    float defH = 14.0f;
    if (uiXml.NavigateToNode(cardinalsPath, 0))
    {
        defY = uiXml.ReadAttribFlt(cardinalsPath, 0, "y", 0.0f);
        defW = uiXml.ReadAttribFlt(cardinalsPath, 0, "width", 16.0f);
        defH = uiXml.ReadAttribFlt(cardinalsPath, 0, "height", 14.0f);
    }
    const char* dirs[] = { "n", "e", "s", "w", "ne", "se", "sw", "nw" };
    _cardinals.clear();
    _cardinals.reserve(8);
    for (const char* d : dirs)
    {
        string_path nodePath;
        xr_sprintf(nodePath, "%s:%s", cardinalsPath, d);
        if (uiXml.NavigateToNode(nodePath, 0))
        {
            CUIStatic* st = InitCardinalStatic(uiXml, xmlInit, cardinalsPath, d, defY, defW, defH);
            if (st)
            {
                AttachChild(st);
                _cardinals.push_back(st);
            }
        }
    }
}

CUIStatic* CUICompassBar::InitCardinalStatic(CUIXml& uiXml, CUIXmlInit& xmlInit, const char* cardinalsPath,
    const char* directionNode, float defaultY, float defaultW, float defaultH)
{
    string_path childPath;
    string_path defaultTextPath;
    string_path childTextPath;
    xr_strconcat(childPath, cardinalsPath, ":", directionNode);
    xr_strconcat(defaultTextPath, cardinalsPath, ":text");
    xr_strconcat(childTextPath, childPath, ":text");

    float y = uiXml.ReadAttribFlt(childPath, 0, "y", defaultY);
    float w = uiXml.ReadAttribFlt(childPath, 0, "width", defaultW);
    float h = uiXml.ReadAttribFlt(childPath, 0, "height", defaultH);

    CUIStatic* st = new CUIStatic();
    st->SetAutoDelete(true);
    st->SetWndPos(Fvector2().set(0.0f, y));
    st->SetWndSize(Fvector2().set(w, h));

    if (uiXml.NavigateToNode(defaultTextPath, 0))
    {
        xmlInit.InitText(uiXml, defaultTextPath, 0, st);
    }
    if (uiXml.NavigateToNode(childTextPath, 0))
    {
        xmlInit.InitText(uiXml, childTextPath, 0, st);
    }
    else
    {
        LPCSTR caption = uiXml.Read(childPath, 0, nullptr);
        if (caption && *caption)
        {
            st->SetText(caption);
        }
        if (uiXml.ReadAttrib(childPath, 0, "color", nullptr))
        {
            st->SetTextColor(CUIXmlInit::GetColor(uiXml, childPath, 0, 0xFFFFFFFF));
        }
        LPCSTR alignStr = uiXml.ReadAttrib(childPath, 0, "align", nullptr);
        if (alignStr && st->TextItemControl())
        {
            if (alignStr[0] == 'l' || alignStr[0] == 'L')
            {
                st->TextItemControl()->SetTextAlignment(CGameFont::alLeft);
            }
            else if (alignStr[0] == 'r' || alignStr[0] == 'R')
            {
                st->TextItemControl()->SetTextAlignment(CGameFont::alRight);
            }
            else
            {
                st->TextItemControl()->SetTextAlignment(CGameFont::alCenter);
            }
        }
    }
    return st;
}

void CUICompassBar::InitActiveTargetWidgets(CUIXml& uiXml)
{
    _activeMarkerFallbackTexture = uiXml.Read("compass_bar:active_target:marker:texture", 0,
        "ui_inGame2_hint_wnd_main_window");
    if (uiXml.NavigateToNode("compass_bar:active_target:distance_text", 0))
    {
        _activeDistText = UIHelper::CreateStatic(uiXml, "compass_bar:active_target:distance_text", this, false);
        if (_activeDistText)
        {
            _activeDistText->SetAutoDelete(false);
        }
    }
    if (uiXml.NavigateToNode("compass_bar:active_target:marker", 0))
    {
        _activeMarker = UIHelper::CreateStatic(uiXml, "compass_bar:active_target:marker", this, false);
        if (_activeMarker)
        {
            _activeMarker->SetAutoDelete(false);
        }
    }
}

void CUICompassBar::InitStripVectorIcon(CUIXml& uiXml)
{
    if (!_strip)
    {
        return;
    }
    LPCSTR pSVG = _stripSvgPath.size() > 0 ? _stripSvgPath.c_str() : nullptr;
    if (!pSVG && _strip->isSVGPresented())
    {
        pSVG = _strip->getSVGFilename(uiXml, _stripXmlPath.c_str(), 0);
    }
    if (!pSVG || !*pSVG)
    {
        return;
    }
    const float fW = _stripTexWidth > 0.0f ? _stripTexWidth : _kDefaultStripTexWidth;
    const float fH = _strip->GetHeight();
    const ui_shader& shader = UI().GetVectorShader(pSVG, fW, fH);
    const Frect uvRect = UI().GetVectorUV(pSVG, fW, fH);
    if (!shader || !shader->inited())
    {
        return;
    }
    _strip->SetShader(shader);
    _strip->SetTextureRect(uvRect);
}

u8 CUICompassBar::ParseAlign(const char* alignStr)
{
    if (!alignStr || !*alignStr)
    {
        return 1;
    }
    if (alignStr[0] == 'l' || alignStr[0] == 'L')
    {
        return 0;
    }
    if (alignStr[0] == 'r' || alignStr[0] == 'R')
    {
        return 2;
    }
    return 1;
}

bool CUICompassBar::ProjectToStrip(const Fvector& targetPos, const Fvector& actorPos, float camHeading,
    float& outX, bool clampToEdges) const
{
    Fvector2 dir;
    dir.set(targetPos.x - actorPos.x, targetPos.z - actorPos.z);
    if (dir.square_magnitude() < 0.01f)
    {
        outX = 0.0f;
        return true;
    }
    float targetYaw = dir.getH();
    float delta = angle_normalize_signed(targetYaw - camHeading);
    float halfFov = _fov * 0.5f;
    if (!clampToEdges)
    {
        if (delta < -halfFov || delta > halfFov)
        {
            return false;
        }
    }
    else
    {
        if (delta < -halfFov)
        {
            delta = -halfFov;
        }
        else if (delta > halfFov)
        {
            delta = halfFov;
        }
    }
    float halfW = _stripWidth * 0.5f;
    outX = (delta / halfFov) * halfW;
    return true;
}

void CUICompassBar::UpdateStrip(float heading)
{
    if (!_strip)
    {
        return;
    }
    const float uvCenter = (heading + _kPi) / (2.0f * _kPi);
    const float stripTexW = _stripTexWidth > 0.0f ? _stripTexWidth : _kDefaultStripTexWidth;
    const float w = _strip->GetWidth();
    const float kx = UI().get_current_kx();
    const float winW = w / kx;
    float u = uvCenter * stripTexW - winW * 0.5f;
    if (_stripTexLoop)
    {
        u = fmodf(u, stripTexW);
        if (u < 0.0f)
        {
            u += stripTexW;
        }
    }
    else
    {
        u = clampr(u, 0.0f, stripTexW - winW);
    }
    Frect rect;
    rect.lt.set(u, 0.0f);
    rect.rb.set(u + winW, _strip->GetHeight());
    _strip->SetTextureRect(rect);
}

void CUICompassBar::UpdateCardinals(float heading)
{
    if (!_strip || _cardinals.empty())
    {
        return;
    }
    const float angles[] = { 0.0f, 0.5f * _kPi, _kPi, -0.5f * _kPi,
        _kPi * 0.25f, _kPi * 0.75f, -_kPi * 0.75f, -_kPi * 0.25f };
    SCompassStripGeometry geom = GetStripGeometry();
    Fvector actorPos = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->Position() : Fvector().set(0, 0, 0);
    for (size_t i = 0; i < _cardinals.size() && i < 8; ++i)
    {
        CUIStatic* st = _cardinals[i];
        if (!st)
        {
            continue;
        }
        Fvector fakeTarget;
        fakeTarget.set(actorPos.x + cosf(angles[i]) * 1000.0f, actorPos.y, actorPos.z + sinf(angles[i]) * 1000.0f);
        float relX;
        if (!ProjectToStrip(fakeTarget, actorPos, heading, relX, false))
        {
            st->Show(false);
            continue;
        }
        float absX = geom.left + geom.CenterX() + relX;
        float cw = st->GetWidth();
        st->SetWndPos(Fvector2().set(absX - cw * 0.5f, st->GetWndPos().y));
        st->Show(true);
    }
}

void CUICompassBar::CollectSpotCandidates(const Fvector& actorPos, const shared_str& levelName)
{
    _spotCandidates.clear();
    if (!_spotCfg.show || !_strip)
    {
        return;
    }
    CMapLocation* activeTaskLoc = _activeTargetLoc;
    xrCriticalSectionGuard guard(Level().MapManager().UpdateCS);
    const Locations& locs = Level().MapManager().Locations();
    for (const SLocationKey& key : locs)
    {
        CMapLocation* loc = key.location;
        if (!loc || !loc->ShowOnCompass() || loc == activeTaskLoc)
        {
            continue;
        }
        if (loc->GetLevelName() != levelName || !loc->SpotEnabled() || !loc->Update())
        {
            continue;
        }
        if (loc->GetCompassSpotTexture().size() == 0)
        {
            continue;
        }
        Fvector pos = loc->GetLastPosition();
        float maxDist = loc->GetCompassMaxDist();
        if (maxDist < 0.0f)
        {
            maxDist = _spotCfg.maxDistance;
        }
        if (maxDist >= 0.0f && actorPos.distance_to(pos) > maxDist)
        {
            continue;
        }
        u32 clr = loc->GetCompassSpotColor();
        if (clr == 0)
        {
            clr = color_rgba(255, 255, 255, 255);
        }
        const SCompassParams& cp = loc->GetCompassParams();
        SSpotCandidate cand;
        cand.pos = pos;
        cand.textureName = loc->GetCompassSpotTexture();
        cand.color = clr;
        cand.offsetY = cp.fOffsetY;
        cand.iconSize = cp.bOverrideSize ? cp.vSize : Fvector2().set(_spotCfg.spotWidth, _spotCfg.spotHeight);
        _spotCandidates.push_back(cand);
    }
}

void CUICompassBar::BuildRenderQueueFromCandidates(float camHeading, const Fvector& actorPos)
{
    _renderQueue.clear();
    if (!_spotCfg.show || !_strip)
    {
        return;
    }
    for (const SSpotCandidate& cand : _spotCandidates)
    {
        float relX;
        if (!ProjectToStrip(cand.pos, actorPos, camHeading, relX, false))
        {
            continue;
        }
        float dist = actorPos.distance_to(cand.pos);
        SSpotRenderItem item;
        item.relX = relX + _spotCfg.offsetX;
        if (_spotCfg.align == 2)
        {
            item.relX -= cand.iconSize.x;
        }
        else if (_spotCfg.align == 1)
        {
            item.relX -= cand.iconSize.x * 0.5f;
        }
        item.sortDist = dist;
        item.offsetY = cand.offsetY;
        item.textureName = cand.textureName;
        item.iconSize = cand.iconSize;
        item.color = cand.color;
        _renderQueue.push_back(item);
    }
}

CUIStatic* CUICompassBar::GetSpotFromPool(xr_vector<CUIStatic*>& pool, CUIWindow* parent, size_t index)
{
    if (index < pool.size())
    {
        return pool[index];
    }
    CUIStatic* item = new CUIStatic();
    item->SetAutoDelete(true);
    item->SetStretchTexture(true);
    parent->AttachChild(item);
    pool.push_back(item);
    return item;
}

void CUICompassBar::CommitLayout()
{
    std::sort(_renderQueue.begin(), _renderQueue.end());
    SCompassStripGeometry geom = GetStripGeometry();
    const float baseY = geom.top + geom.CenterY() + _cfg.markersY + _spotCfg.offsetY;
    size_t idx = 0;
    for (const SSpotRenderItem& item : _renderQueue)
    {
        CUIStatic* wnd = GetSpotFromPool(_poolSpots, _layerFg, idx);
        if (!wnd || !_layerFg)
        {
            ++idx;
            continue;
        }
        if (_poolSpotTextureNames.size() <= idx)
        {
            _poolSpotTextureNames.resize(idx + 1);
        }
        if (_poolSpotTextureNames[idx] != item.textureName)
        {
            CUITextureMaster::InitTexture(item.textureName, &wnd->GetUIStaticItem());
            _poolSpotTextureNames[idx] = item.textureName;
        }
        wnd->SetWndSize(item.iconSize);
        wnd->SetTextureColor(item.color);
        const float posX = geom.left + geom.CenterX() + item.relX;
        const float posY = baseY + item.offsetY - item.iconSize.y * 0.5f;
        wnd->SetWndPos(Fvector2().set(posX, posY));
        wnd->Show(true);
        ++idx;
    }
    for (size_t i = idx; i < _poolSpots.size(); ++i)
    {
        _poolSpots[i]->Show(false);
    }
}

void CUICompassBar::UpdateActiveTarget(const Fvector& actorPos, float camHeading, const shared_str& levelName)
{
    if (!_activeDistText || !_strip)
    {
        return;
    }
    if (!IsGameTypeSingleCompatible())
    {
        return;
    }
    _activeDistText->Show(false);
    if (_activeMarker)
    {
        _activeMarker->Show(false);
    }
    CMapLocation* activeLoc = _activeTargetLoc;
    if (!activeLoc || activeLoc->GetLevelName() != levelName)
    {
        _lastActiveLoc = nullptr;
        return;
    }
    if (!activeLoc->Update())
    {
        return;
    }
    const Fvector tgtPos = activeLoc->GetLastPosition();
    float spotX;
    if (!ProjectToStrip(tgtPos, actorPos, camHeading, spotX, true))
    {
        return;
    }
    float stripCenter = _stripWidth * 0.5f;
    spotX = stripCenter + spotX;
    spotX = clampr(spotX, _cfg.activePadding, _stripWidth - _cfg.activePadding);
    if (_lastActiveLoc != activeLoc)
    {
        _activeTargetCurX = spotX;
        _lastActiveLoc = activeLoc;
    }
    else
    {
        _activeTargetCurX += (spotX - _activeTargetCurX) * (Device.fTimeDelta * _cfg.smoothingSpeed);
    }
    spotX = _activeTargetCurX;
    const float dist = actorPos.distance_to(tgtPos);
    string64 buf;
    xr_sprintf(buf, sizeof(buf), "%.0f m", dist);
    _activeDistText->SetText(buf);
    SCompassStripGeometry geom = GetStripGeometry();
    const float baseY = geom.top + geom.CenterY() + _cfg.stripY + _cfg.markersY;
    const float anchorX = geom.left + spotX + _cfg.activeOffsetX;
    const float anchorY = baseY + _cfg.activeOffsetY;
    const float tw = _activeDistText->GetWidth();
    const float th = _activeDistText->GetHeight();
    if (_activeMarker)
    {
        shared_str texName = activeLoc->GetCompassSpotTexture().size() > 0
            ? activeLoc->GetCompassSpotTexture()
            : _activeMarkerFallbackTexture;
        if (_activeMarkerLastTexture != texName)
        {
            CUITextureMaster::InitTexture(texName, &_activeMarker->GetUIStaticItem());
            _activeMarkerLastTexture = texName;
        }
        const float mw = _activeMarker->GetWidth();
        const float mh = _activeMarker->GetHeight();
        _activeMarker->SetWndPos(Fvector2().set(anchorX - mw * 0.5f, anchorY - mh * 0.5f));
        _activeMarker->Show(true);
        _activeDistText->SetWndPos(Fvector2().set(
            anchorX - tw * 0.5f + _cfg.textOffsetX,
            anchorY - mh * 0.5f + _cfg.distY - th + _cfg.textOffsetY));
    }
    else
    {
        _activeDistText->SetWndPos(Fvector2().set(
            anchorX - tw * 0.5f + _cfg.textOffsetX,
            anchorY + _cfg.distY - th + _cfg.textOffsetY));
    }
    _activeDistText->Show(true);
}

void CUICompassBar::Draw()
{
    if (!visible)
    {
        return;
    }
    CUIWindow::Draw();
}

void CUICompassBar::Update()
{
    if (!visible)
    {
        return;
    }
    if (!g_pGameLevel)
    {
        return;
    }
    SCompassFrameContext ctx;
    if (!BuildFrameContext(ctx))
    {
        CUIWindow::Update();
        return;
    }
    UpdateStrip(ctx.heading);
    UpdateCardinals(ctx.heading);

    _collectSpotsTimer -= Device.fTimeDelta;
    if (_collectSpotsTimer <= 0.0f)
    {
        _collectSpotsTimer = _spotCfg.collectInterval;
        CollectSpotCandidates(ctx.actorPos, ctx.levelName);
    }
    BuildRenderQueueFromCandidates(ctx.heading, ctx.actorPos);
    UpdateActiveTarget(ctx.actorPos, ctx.heading, ctx.levelName);
    CommitLayout();
    CUIWindow::Update();
}

bool CUICompassBar::BuildFrameContext(SCompassFrameContext& out) const
{
    CObject* viewEntity = Level().CurrentViewEntity();
    if (!viewEntity)
    {
        out.isValid = false;
        return false;
    }
    out.actorPos = viewEntity->Position();
    out.heading = Device.vCameraDirection.getH();
    out.levelName = Level().name();
    out.isValid = true;
    return true;
}

SCompassStripGeometry CUICompassBar::GetStripGeometry() const
{
    SCompassStripGeometry geom;
    if (_strip)
    {
        Fvector2 stripPos = _strip->GetWndPos();
        geom.left = stripPos.x;
        geom.top = stripPos.y;
        geom.width = _strip->GetWidth();
        geom.height = _strip->GetHeight();
    }
    return geom;
}

CUIStatic& CUICompassBar::Background()
{
    R_ASSERT(_background);
    return *_background;
}

CUIWindow* CUICompassBar::GetFrame()
{
    return this;
}

void CUICompassBar::SetActiveTarget(CMapLocation* loc)
{
    _activeTargetLoc = loc;
}

