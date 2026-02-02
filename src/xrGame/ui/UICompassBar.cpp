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

static const char* kActiveMarkerFallbackTexture = "ui_inGame2_hint_wnd_main_window";

void CUICompassBar::detachAndDelete(CUIWindow* wnd)
{
    if (!wnd)
    {
        return;
    }
    CUIWindow* parent = wnd->GetParent();
    if (parent)
    {
        parent->DetachChild(wnd);
    }
    xr_delete(wnd);
}

CUICompassBar::CUICompassBar()
    : _background(nullptr),
      _strip(nullptr),
      _cardinalN(nullptr),
      _cardinalE(nullptr),
      _cardinalS(nullptr),
      _cardinalW(nullptr),
      _activeDistanceText(nullptr),
      _activeTargetMarker(nullptr),
      _fov(kDefaultFovDeg * kPi / 180.0f),
      _stripWidth(0.0f),
      _stripTexWidth(771.0f),
      _stripY(0.0f),
      _markersY(0.0f),
      _distTextY(-18.0f),
      _smoothSpeed(10.0f),
      _activeTargetPadding(8.0f),
      _activeBlockOffsetX(0.0f),
      _activeBlockOffsetY(0.0f),
      _textOffsetX(0.0f),
      _textOffsetY(0.0f),
      _curActiveX(0.0f),
      _lastActiveLoc(nullptr),
      _activeTargetLoc(nullptr)
{
}

CUICompassBar::~CUICompassBar()
{
    detachAndDelete(_activeDistanceText);
    _activeDistanceText = nullptr;
    detachAndDelete(_activeTargetMarker);
    _activeTargetMarker = nullptr;
    for (CUIStatic* s : _spotPool)
    {
        detachAndDelete(s);
    }
    _spotPool.clear();
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
    xmlInit.InitWindow(uiXml, "compass_bar", 0, this);
    _background = UIHelper::CreateStatic(uiXml, "compass_bar:background", this);
    if (!_background)
    {
        _background = new CUIStatic();
        _background->SetAutoDelete(true);
        _background->SetWndSize(GetWndSize());
        AttachChild(_background);
    }
    float fovDeg = uiXml.ReadAttribFlt("compass_bar", 0, "fov_angle", kDefaultFovDeg);
    _fov = fovDeg * kPi / 180.0f;
    parseSpots(uiXml, "compass_bar:spots");
    const char* layoutPath = "compass_bar:active_target";
    if (!uiXml.NavigateToNode(layoutPath, 0))
    {
        layoutPath = "compass_bar:layout";
    }
    if (uiXml.NavigateToNode(layoutPath, 0))
    {
        _stripY = uiXml.ReadAttribFlt(layoutPath, 0, "strip_y", 0.0f);
        _markersY = uiXml.ReadAttribFlt(layoutPath, 0, "markers_y", 0.0f);
        _distTextY = uiXml.ReadAttribFlt(layoutPath, 0, "dist_y", -18.0f);
        _smoothSpeed = uiXml.ReadAttribFlt(layoutPath, 0, "smoothing_speed", 10.0f);
        _activeTargetPadding = uiXml.ReadAttribFlt(layoutPath, 0, "active_target_padding", 8.0f);
        _activeBlockOffsetX = uiXml.ReadAttribFlt(layoutPath, 0, "offset_x", 0.0f);
        _activeBlockOffsetY = uiXml.ReadAttribFlt(layoutPath, 0, "offset_y", 0.0f);
        _textOffsetX = uiXml.ReadAttribFlt(layoutPath, 0, "text_offset_x", 0.0f);
        _textOffsetY = uiXml.ReadAttribFlt(layoutPath, 0, "text_offset_y", 0.0f);
    }
    initCompassDial(uiXml, xmlInit);
    if (uiXml.NavigateToNode("compass_bar:active_target:distance_text", 0))
    {
        _activeDistanceText = UIHelper::CreateStatic(uiXml, "compass_bar:active_target:distance_text", this);
        if (_activeDistanceText)
        {
            _activeDistanceText->SetAutoDelete(false);
        }
    }
    if (uiXml.NavigateToNode("compass_bar:active_target:marker", 0))
    {
        _activeTargetMarker = UIHelper::CreateStatic(uiXml, "compass_bar:active_target:marker", nullptr, false);
        if (_activeTargetMarker)
        {
            _activeTargetMarker->SetAutoDelete(false);
        }
    }
    _spotPool.reserve(kMaxSpotPoolSize);
    for (u32 i = 0; i < kMaxSpotPoolSize; ++i)
    {
        CUIStatic* s = new CUIStatic();
        s->SetAutoDelete(false);
        s->SetWndSize(Fvector2().set(16.0f, 16.0f));
        s->SetStretchTexture(true);
        s->Show(false);
        _spotPool.push_back(s);
    }
}

u8 CUICompassBar::parseAlign(const char* alignStr)
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

void CUICompassBar::parseSpots(CUIXml& uiXml, const char* path)
{
    _spotConfig.show = true;
    _spotConfig.offsetX = 0.0f;
    _spotConfig.offsetY = 0.0f;
    _spotConfig.align = 1;
    _spotConfig.spotWidth = 16.0f;
    _spotConfig.spotHeight = 16.0f;
    _spotConfig.maxDistance = -1.0f;
    _spotConfig.layer = 1;
    if (!uiXml.NavigateToNode(path, 0))
    {
        return;
    }
    _spotConfig.show = uiXml.ReadAttribInt(path, 0, "show", 1) != 0;
    _spotConfig.offsetX = uiXml.ReadAttribFlt(path, 0, "x", 0.0f);
    _spotConfig.offsetY = uiXml.ReadAttribFlt(path, 0, "y", 0.0f);
    _spotConfig.align = parseAlign(uiXml.ReadAttrib(path, 0, "align", "c"));
    _spotConfig.maxDistance = uiXml.ReadAttribFlt(path, 0, "max_distance", -1.0f);
    _spotConfig.layer = uiXml.ReadAttribInt(path, 0, "layer", 1);
    string_path tmplPath;
    xr_strconcat(tmplPath, path, ":spot_template");
    if (uiXml.NavigateToNode(tmplPath, 0))
    {
        _spotConfig.spotWidth = uiXml.ReadAttribFlt(tmplPath, 0, "width", 16.0f);
        _spotConfig.spotHeight = uiXml.ReadAttribFlt(tmplPath, 0, "height", 16.0f);
    }
}

void CUICompassBar::initCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit)
{
    const char* stripPath = "compass_bar:strip";
    const char* cardinalsPath = "compass_bar:cardinal_points";
    if (uiXml.NavigateToNode("compass_bar:compass_dial", 0))
    {
        stripPath = "compass_bar:compass_dial:strip";
        cardinalsPath = "compass_bar:compass_dial:cardinal_points";
    }
    _strip = UIHelper::CreateStatic(uiXml, stripPath, this);
    _stripTexWidth = uiXml.ReadAttribFlt(stripPath, 0, "tex_width", 771.0f);
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
    string_path nodePath;
    xr_sprintf(nodePath, "%s:n", cardinalsPath);
    if (uiXml.NavigateToNode(nodePath, 0))
    {
        _cardinalN = initCardinalStatic(uiXml, xmlInit, cardinalsPath, "n", defY, defW, defH);
    }
    xr_sprintf(nodePath, "%s:e", cardinalsPath);
    if (uiXml.NavigateToNode(nodePath, 0))
    {
        _cardinalE = initCardinalStatic(uiXml, xmlInit, cardinalsPath, "e", defY, defW, defH);
    }
    xr_sprintf(nodePath, "%s:s", cardinalsPath);
    if (uiXml.NavigateToNode(nodePath, 0))
    {
        _cardinalS = initCardinalStatic(uiXml, xmlInit, cardinalsPath, "s", defY, defW, defH);
    }
    xr_sprintf(nodePath, "%s:w", cardinalsPath);
    if (uiXml.NavigateToNode(nodePath, 0))
    {
        _cardinalW = initCardinalStatic(uiXml, xmlInit, cardinalsPath, "w", defY, defW, defH);
    }
    if (_cardinalN)
    {
        AttachChild(_cardinalN);
    }
    if (_cardinalE)
    {
        AttachChild(_cardinalE);
    }
    if (_cardinalS)
    {
        AttachChild(_cardinalS);
    }
    if (_cardinalW)
    {
        AttachChild(_cardinalW);
    }
}

CUIStatic* CUICompassBar::initCardinalStatic(CUIXml& uiXml, CUIXmlInit& xmlInit, const char* cardinalsPath, const char* directionNode, float defaultY, float defaultW, float defaultH)
{
    string_path childPath;
    xr_strconcat(childPath, cardinalsPath, ":", directionNode);
    string_path defaultTextPath;
    xr_strconcat(defaultTextPath, cardinalsPath, ":text");
    string_path childTextPath;
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
        if (alignStr)
        {
            CUILines* lines = st->TextItemControl();
            if (lines)
            {
                if (alignStr[0] == 'l' || alignStr[0] == 'L')
                {
                    lines->SetTextAlignment(CGameFont::alLeft);
                }
                else if (alignStr[0] == 'r' || alignStr[0] == 'R')
                {
                    lines->SetTextAlignment(CGameFont::alRight);
                }
                else
                {
                    lines->SetTextAlignment(CGameFont::alCenter);
                }
            }
        }
    }
    return st;
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

void CUICompassBar::updateStrip(float heading)
{
    if (!_strip)
    {
        return;
    }
    const float uvCenter = (heading + kPi) / (2.0f * kPi);
    const float stripTexW = _stripTexWidth > 0.0f ? _stripTexWidth : 771.0f;
    const float w = _strip->GetWidth();
    float u = uvCenter * stripTexW - w * 0.5f;
    u = clampr(u, 0.0f, stripTexW - w);
    const float kx = UI().get_current_kx();
    Frect rect;
    rect.lt.set(u, 0.0f);
    rect.rb.set(u + w / kx, _strip->GetHeight());
    _strip->SetTextureRect(rect);
}

bool CUICompassBar::getCardinalX(float worldAngleRad, float heading, float& outX) const
{
    if (!_strip)
    {
        return false;
    }
    float percent;
    if (!computeRelativePercent(worldAngleRad, heading, _fov, false, percent))
    {
        return false;
    }
    const float stripLeft = _strip->GetWndPos().x;
    const float stripW = _strip->GetWidth();
    const float halfW = stripW * 0.5f;
    outX = stripLeft + halfW + percent * halfW;
    return true;
}

void CUICompassBar::updateCardinals(float heading)
{
    if (!_strip)
    {
        return;
    }
    const float nAngle = 0.0f;
    const float eAngle = 0.5f * kPi;
    const float sAngle = kPi;
    const float wAngle = -0.5f * kPi;
    auto updateOneLambda = [this, heading](CUIStatic* st, float worldAngle)
    {
        if (!st)
        {
            return;
        }
        float x;
        if (!getCardinalX(worldAngle, heading, x))
        {
            st->Show(false);
            return;
        }
        float cw = st->GetWidth();
        st->SetWndPos(Fvector2().set(x - cw * 0.5f, st->GetWndPos().y));
        st->Show(true);
    };
    updateOneLambda(_cardinalN, nAngle);
    updateOneLambda(_cardinalE, eAngle);
    updateOneLambda(_cardinalS, sAngle);
    updateOneLambda(_cardinalW, wAngle);
}

bool CUICompassBar::getSpotX(const Fvector2& targetPos, const Fvector2& actorPos, float cameraHeading, float& outX) const
{
    Fvector2 dir;
    dir.sub(targetPos, actorPos);
    if (dir.square_magnitude() < 0.01f)
    {
        outX = _stripWidth * 0.5f;
        return true;
    }
    float targetYaw = dir.getH();
    float percent;
    if (!computeRelativePercent(targetYaw, cameraHeading, _fov, false, percent))
    {
        return false;
    }
    float centerX = _stripWidth * 0.5f;
    outX = centerX + percent * (_stripWidth * 0.5f);
    return true;
}

bool CUICompassBar::getActiveTargetSpotX(const Fvector2& targetPos, const Fvector2& actorPos, float cameraHeading, float& outX) const
{
    Fvector2 dir;
    dir.sub(targetPos, actorPos);
    if (dir.square_magnitude() < 0.01f)
    {
        outX = _stripWidth * 0.5f;
        return true;
    }
    float targetYaw = dir.getH();
    float percent;
    if (!computeRelativePercent(targetYaw, cameraHeading, _fov, true, percent))
    {
        return false;
    }
    float centerX = _stripWidth * 0.5f;
    outX = centerX + percent * (_stripWidth * 0.5f);
    return true;
}

bool CUICompassBar::computeRelativePercent(float targetYaw, float actorYaw, float fov, bool clampToEdges, float& outPercent) const
{
    float delta = targetYaw - actorYaw;
    while (delta > kPi)
    {
        delta -= 2.0f * kPi;
    }
    while (delta < -kPi)
    {
        delta += 2.0f * kPi;
    }
    float halfFov = fov * 0.5f;
    if (!clampToEdges)
    {
        float absDelta = delta < 0.0f ? -delta : delta;
        if (absDelta > halfFov)
        {
            return false;
        }
        outPercent = delta / halfFov;
        return true;
    }
    if (delta <= -halfFov)
    {
        outPercent = -1.0f;
    }
    else if (delta >= halfFov)
    {
        outPercent = 1.0f;
    }
    else
    {
        outPercent = delta / halfFov;
    }
    return true;
}

void CUICompassBar::SetActiveTarget(CMapLocation* loc)
{
    _activeTargetLoc = loc;
}

void CUICompassBar::collectSpots(const Locations& locs, const shared_str& levelName, CMapLocation* activeTaskLoc,
    const Fvector& actorPos, xr_vector<SCollectedSpot>& out) const
{
    out.clear();
    out.reserve(kMaxSpotPoolSize);
    for (const SLocationKey& key : locs)
    {
        CMapLocation* loc = key.location;
        if (!loc || !loc->ShowOnCompass())
        {
            continue;
        }
        if (loc == activeTaskLoc)
        {
            continue;
        }
        if (loc->GetLevelName() != levelName)
        {
            continue;
        }
        if (!loc->SpotEnabled())
        {
            continue;
        }
        if (!loc->Update())
        {
            continue;
        }
        if (loc->GetCompassSpotTexture().size() == 0)
        {
            continue;
        }
        if (!_spotConfig.show)
        {
            continue;
        }
        Fvector pos = loc->GetLastPosition();
        float maxDist = loc->GetCompassMaxDist();
        if (maxDist < 0.0f)
        {
            maxDist = _spotConfig.maxDistance;
        }
        if (maxDist >= 0.0f)
        {
            float d = actorPos.distance_to(pos);
            if (d > maxDist)
            {
                continue;
            }
        }
        out.push_back({ loc, pos });
    }
}

void CUICompassBar::layoutSpots(const xr_vector<SCollectedSpot>& collected, const Fvector2& actorPos, float cameraHeading,
    float stripLeft, float stripTop, float stripCenterY, xr_vector<SSpotLayoutInfo>& out)
{
    out.clear();
    out.reserve(kMaxSpotPoolSize);
    u32 poolIdx = 0;
    const SCompassSpotConfig& cfg = _spotConfig;
    for (u32 i = 0; i < collected.size() && poolIdx < _spotPool.size(); ++i)
    {
        const SCollectedSpot& cs = collected[i];
        Fvector2 targetPos(cs.worldPos.x, cs.worldPos.z);
        float spotX;
        if (!getSpotX(targetPos, actorPos, cameraHeading, spotX))
        {
            continue;
        }
        CUIStatic* spotStatic = _spotPool[poolIdx++];
        if (!spotStatic)
        {
            break;
        }
        spotStatic->SetWndSize(Fvector2().set(cfg.spotWidth, cfg.spotHeight));
        CUITextureMaster::InitTexture(cs.loc->GetCompassSpotTexture().c_str(), &spotStatic->GetUIStaticItem());
        u32 spotColor = cs.loc->GetCompassSpotColor();
        spotStatic->SetTextureColor(spotColor != 0 ? spotColor : color_rgba(255, 255, 255, 255));
        float posX = spotX + cfg.offsetX;
        if (cfg.align == 2)
        {
            posX -= cfg.spotWidth;
        }
        else if (cfg.align == 1)
        {
            posX -= cfg.spotWidth * 0.5f;
        }
        float absX = stripLeft + posX;
        float absY = stripTop + stripCenterY + cfg.offsetY - cfg.spotHeight * 0.5f;
        out.push_back({ spotStatic, absX, absY, cfg.layer });
    }
}

void CUICompassBar::applySpotsToUI(const xr_vector<SSpotLayoutInfo>& spotsToShow, u32 poolUsedCount)
{
    const int layerOverStrip = (int)ECompassLayer::OverStrip;
    CUIWindow* underStripParent = _background;
    float bgLeft = 0.0f;
    float bgTop = 0.0f;
    if (underStripParent)
    {
        Fvector2 bgPos = underStripParent->GetWndPos();
        bgLeft = bgPos.x;
        bgTop = bgPos.y;
    }
    for (const SSpotLayoutInfo& si : spotsToShow)
    {
        if (!si.st)
        {
            continue;
        }
        CUIWindow* desiredParent = (si.layer < layerOverStrip && underStripParent) ? underStripParent : this;
        CUIWindow* currentParent = si.st->GetParent();
        if (currentParent != desiredParent)
        {
            if (currentParent)
            {
                currentParent->DetachChild(si.st);
            }
            desiredParent->AttachChild(si.st);
        }
        float posX = si.x;
        float posY = si.y;
        if (desiredParent == underStripParent)
        {
            posX = si.x - bgLeft;
            posY = si.y - bgTop;
        }
        si.st->SetWndPos(Fvector2().set(posX, posY));
        si.st->Show(true);
    }
    for (u32 i = poolUsedCount; i < _spotPool.size(); ++i)
    {
        CUIStatic* s = _spotPool[i];
        if (!s)
        {
            continue;
        }
        if (s->GetParent())
        {
            s->GetParent()->DetachChild(s);
            s->Show(false);
        }
        else
        {
            s->Show(false);
        }
    }
}

void CUICompassBar::updateSpots(const Fvector& actorPos, float cameraHeading, const shared_str& levelName)
{
    if (!_strip)
    {
        return;
    }
    CMapLocation* activeTaskLoc = _activeTargetLoc;
    xrCriticalSectionGuard guard(Level().MapManager().UpdateCS);
    const Locations& locs = Level().MapManager().Locations();
    xr_vector<SCollectedSpot> collected;
    collectSpots(locs, levelName, activeTaskLoc, actorPos, collected);
    const float stripLeft = _strip->GetWndPos().x;
    const float stripTop = _strip->GetWndPos().y;
    const float stripCenterY = _strip->GetHeight() * 0.5f;
    const Fvector2 actorPos2(actorPos.x, actorPos.z);
    xr_vector<SSpotLayoutInfo> spotsToShow;
    layoutSpots(collected, actorPos2, cameraHeading, stripLeft, stripTop, stripCenterY, spotsToShow);
    std::sort(spotsToShow.begin(), spotsToShow.end(),
        [](const SSpotLayoutInfo& a, const SSpotLayoutInfo& b) { return a.layer < b.layer; });
    applySpotsToUI(spotsToShow, (u32)spotsToShow.size());
}

void CUICompassBar::updateActiveTarget(const Fvector& actorPos, float cameraHeading, const shared_str& levelName)
{
    if (!_activeDistanceText || !_strip)
    {
        return;
    }
    if (!IsGameTypeSingleCompatible())
    {
        return;
    }
    _activeDistanceText->Show(false);
    if (_activeTargetMarker)
    {
        _activeTargetMarker->Show(false);
        if (_activeTargetMarker->GetParent())
        {
            _activeTargetMarker->GetParent()->DetachChild(_activeTargetMarker);
        }
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
    const Fvector2 targetPos2(tgtPos.x, tgtPos.z);
    const Fvector2 actorPos2(actorPos.x, actorPos.z);
    float spotX;
    if (!getActiveTargetSpotX(targetPos2, actorPos2, cameraHeading, spotX))
    {
        return;
    }
    spotX = clampr(spotX, _activeTargetPadding, _stripWidth - _activeTargetPadding);
    if (_lastActiveLoc != activeLoc)
    {
        _curActiveX = spotX;
        _lastActiveLoc = activeLoc;
    }
    else
    {
        const float dt = Device.fTimeDelta;
        _curActiveX += (spotX - _curActiveX) * (dt * _smoothSpeed);
    }
    spotX = _curActiveX;
    const float dist = actorPos.distance_to(tgtPos);
    string64 buf;
    xr_sprintf(buf, sizeof(buf), "%.0f m", dist);
    _activeDistanceText->SetText(buf);
    const float stripLeft = _strip->GetWndPos().x;
    const float stripTop = _strip->GetWndPos().y;
    const float stripCenterY = _strip->GetHeight() * 0.5f;
    const float baseY = stripTop + stripCenterY + _stripY + _markersY;
    const float anchorX = stripLeft + spotX + _activeBlockOffsetX;
    const float anchorY = baseY + _activeBlockOffsetY;
    const float tw = _activeDistanceText->GetWidth();
    const float th = _activeDistanceText->GetHeight();
    if (_activeTargetMarker)
    {
        LPCSTR texName = activeLoc->GetCompassSpotTexture().size() > 0
            ? activeLoc->GetCompassSpotTexture().c_str()
            : kActiveMarkerFallbackTexture;
        CUITextureMaster::InitTexture(texName, &_activeTargetMarker->GetUIStaticItem());
        const float mw = _activeTargetMarker->GetWidth();
        const float mh = _activeTargetMarker->GetHeight();
        _activeTargetMarker->SetWndPos(Fvector2().set(anchorX - mw * 0.5f, anchorY - mh * 0.5f));
        _activeTargetMarker->Show(true);
        if (_activeTargetMarker->GetParent() != this)
        {
            AttachChild(_activeTargetMarker);
        }
        _activeDistanceText->SetWndPos(Fvector2().set(anchorX - tw * 0.5f + _textOffsetX, anchorY - mh * 0.5f + _distTextY - th + _textOffsetY));
    }
    else
    {
        _activeDistanceText->SetWndPos(Fvector2().set(anchorX - tw * 0.5f + _textOffsetX, anchorY + _distTextY - th + _textOffsetY));
    }
    _activeDistanceText->Show(true);
    if (_activeDistanceText->GetParent() != this)
    {
        AttachChild(_activeDistanceText);
    }
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
    CObject* viewEntity = Level().CurrentViewEntity();
    if (!viewEntity)
    {
        CUIWindow::Update();
        return;
    }
    const Fvector actorPos = viewEntity->Position();
    const float heading = Device.vCameraDirection.getH();
    const shared_str levelName = Level().name();
    updateStrip(heading);
    updateCardinals(heading);
    updateSpots(actorPos, heading, levelName);
    updateActiveTarget(actorPos, heading, levelName);
    CUIWindow::Update();
}
