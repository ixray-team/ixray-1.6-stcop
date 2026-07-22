#include "StdAfx.h"
#include "UICompassBar.h"
#include "../Actor.h"
#include "../Level.h"
#include "../map_location.h"
#include "../map_location_defs.h"
#include "../map_manager.h"
#include "../../xrEngine/device.h"
#include "../../xrEngine/GameFont.h"
#include "../../xrEngine/string_table.h"
#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "../../xrCore/_stl_extensions.h"
#include "../../xrCore/_color.h"
#include "../../xrCore/vector.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UILines.h"
#include "../../xrUI/Widgets/UILines.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../xrUI/ui_defs.h"
#include <algorithm>
#include <cmath>

// --- Init / XML ---

const float CUICompassBar::_kCardinalAngles[_kMaxCardinalPoints] =
{
    deg2rad(90.f),
    deg2rad(0.f),
    deg2rad(-90.f),
    deg2rad(180.f),
    deg2rad(45.f),
    deg2rad(-45.f),
    deg2rad(-135.f),
    deg2rad(135.f)
};

void CUICompassClipWindow::Draw()
{
    Frect clipRect;
    GetAbsoluteRect(clipRect);
    UI().PushScissor(clipRect);
    inherited::Draw();
    UI().PopScissor();
}

CUICompassBar::CUICompassBar()
    : _background(nullptr),
      _layerBg(nullptr),
      _strip(nullptr),
      _stripContainer(nullptr),
      _layerFg(nullptr),
      _activeTargetContainer(nullptr),
      _activeAltitudeArrow(nullptr),
      _activeMarker(nullptr),
      _activeDistText(nullptr),
      _activeTargetLoc(nullptr),
      _lastActiveLoc(nullptr),
      _activeTargetCurX(0.0f),
      _stripWidth(0.0f),
      _stripTexWidth(_kDefaultStripTexWidth),
      _stripTexLoop(true),
      _stripTextureScaleX(1.0f),
      _stripTextureScaleY(1.0f),
      _stripTextureOffsetX(0.0f),
      _stripTextureOffsetY(0.0f),
      _stripTextureStretch(true),
      _stripRelPos(Fvector2().set(0.0f, 0.0f)),
      _stripRelSize(Fvector2().set(1.0f, 1.0f)),
      _collectSpotsTimer(0.0f),
      _isInitialized(false),
      _isGameTypeSingleCompatible(false),
      _fadeStorageSpotCount(0),
      _stripGeometryCached(false)
{
    _runtimeCfg.fovRad = deg2rad(_kDefaultFovDeg);
    _runtimeCfg.activePadding = _kDefaultActivePadding;
    _runtimeCfg.smoothingSpeed = _kDefaultSmoothingSpeed;
    _runtimeCfg.altitudeDeadzone = _kDefaultAltitudeDeadzone;
    _runtimeCfg.cardinalFakeDistance = _kDefaultFakeTargetDistance;
    _runtimeCfg.distanceFormat = "%.0f m";
}

CUICompassBar::~CUICompassBar()
{
    _poolSpots.clear();
    _poolSpotTextureNames.clear();
}

void CUICompassBar::Init()
{
    _isInitialized = false;

    CUIXml uiXml;
    if (!uiXml.Load(CONFIG_PATH, UI_PATH, "compass_bar.xml"))
    {
        Msg("! Unable to load \"compass_bar.xml\"");
        return;
    }
    CUIXmlInit xmlInit;
    if (!uiXml.NavigateToNode("compass_bar", 0))
    {
        Msg("! CUICompassBar::Init: node 'compass_bar' not found in %s", uiXml.m_xml_file_name);
        return;
    }

    InitWindowAndBackground(uiXml, xmlInit);
    InitLayoutFromXml(uiXml);

    _layerBg = new CUIWindow();
    _layerBg->SetAutoDelete(true);
    _layerBg->SetWndSize(GetWndSize());
    _layerBg->SetWndPos(Fvector2().set(0.0f, 0.0f));

    InitCompassDial(uiXml, xmlInit, _layerBg);
    AttachChild(_layerBg);

    _layerFg = new CUIWindow();
    _layerFg->SetAutoDelete(true);
    _layerFg->SetWndSize(GetWndSize());
    _layerFg->SetWndPos(Fvector2().set(0.0f, 0.0f));
    AttachChild(_layerFg);

    InitActiveTargetWidgets(uiXml, xmlInit);
    if (!_activeDistText || !_activeMarker)
    {
        CreateDefaultActiveTargetWidgets(uiXml);
    }
    CacheGameTypeCompatibility();
    ApplyRelativeLayout();
    _isInitialized = (_strip != nullptr && _stripContainer != nullptr);
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
        float posX = 0.0f;
        float posY = 0.0f;
        if (_background->GetAlignment() == waCenter)
        {
            posX = GetWidth() * 0.5f;
            posY = GetHeight() * 0.5f;
        }
        _background->SetWndPos(Fvector2().set(posX, posY));
    }
}

void CUICompassBar::InitLayoutFromXml(CUIXml& uiXml)
{
    const char* barPath = "compass_bar";
    _layoutUnits.barRelPos.x = uiXml.ReadAttribFlt(barPath, 0, "x", 0.0f);
    _layoutUnits.barRelPos.y = uiXml.ReadAttribFlt(barPath, 0, "y", 0.0f);
    _layoutUnits.barRelSize.x = uiXml.ReadAttribFlt(barPath, 0, "width", 1.0f);
    _layoutUnits.barRelSize.y = uiXml.ReadAttribFlt(barPath, 0, "height", 1.0f);

    const float fovDeg = uiXml.ReadAttribFlt(barPath, 0, "fov_angle", _kDefaultFovDeg);
    _runtimeCfg.fovRad = (fovDeg > 0.0f) ? deg2rad(fovDeg) : deg2rad(_kDefaultFovDeg);

    _runtimeCfg.fadeInSpeed = std::max(uiXml.ReadAttribFlt(barPath, 0, "fade_in_speed", _runtimeCfg.fadeInSpeed), 0.1f);
    _runtimeCfg.fadeOutSpeed = std::max(uiXml.ReadAttribFlt(barPath, 0, "fade_out_speed", _runtimeCfg.fadeOutSpeed), 0.1f);
    _runtimeCfg.minVisibleAlpha = clampr(uiXml.ReadAttribFlt(barPath, 0, "min_visible_alpha", _runtimeCfg.minVisibleAlpha), 0.0f, 1.0f);
    _runtimeCfg.fovFadeInner = uiXml.ReadAttribFlt(barPath, 0, "fov_fade_inner", _kDefaultFovFadeInner);
    _runtimeCfg.fovFadeOuter = uiXml.ReadAttribFlt(barPath, 0, "fov_fade_outer", _kDefaultFovFadeOuter);
    _runtimeCfg.fovFadeEdgeLo = uiXml.ReadAttribFlt(barPath, 0, "fov_fade_edge_lo", _kDefaultFovFadeEdgeLo);
    _runtimeCfg.fovFadeEdgeHi = uiXml.ReadAttribFlt(barPath, 0, "fov_fade_edge_hi", _kDefaultFovFadeEdgeHi);

    ParseSpots(uiXml, "compass_bar:spots");

    if (uiXml.NavigateToNode("compass_bar:active_target", 0))
    {
        const char* targetPath = "compass_bar:active_target";
        _runtimeCfg.activePadding = uiXml.ReadAttribFlt(targetPath, 0, "padding",
            uiXml.ReadAttribFlt(targetPath, 0, "active_target_padding", _kDefaultActivePadding));
        _runtimeCfg.smoothingSpeed = uiXml.ReadAttribFlt(targetPath, 0, "smoothing_speed", _kDefaultSmoothingSpeed);
        // Legacy `y` is the vertical offset in px; prefer explicit aliases when present.
        const float legacyY = uiXml.ReadAttribFlt(targetPath, 0, "y", 0.0f);
        const float offsetY = uiXml.ReadAttribFlt(targetPath, 0, "offset_y", legacyY);
        _runtimeCfg.activeOffsetY = uiXml.ReadAttribFlt(targetPath, 0, "active_offset_y", offsetY);
        _runtimeCfg.altitudeDeadzone = uiXml.ReadAttribFlt(targetPath, 0, "altitude_deadzone", _kDefaultAltitudeDeadzone);
    }
}

void CUICompassBar::ParseSpots(CUIXml& uiXml, const char* path)
{
    if (!uiXml.NavigateToNode(path, 0))
    {
        return;
    }

    _spotCfg.show = uiXml.ReadAttribInt(path, 0, "show", 1) != 0;
    _spotCfg.offsetX = uiXml.ReadAttribFlt(path, 0, "x", 0.0f);
    _spotCfg.offsetY = uiXml.ReadAttribFlt(path, 0, "y", 0.0f);
    _spotCfg.align = ParseAlign(uiXml.ReadAttrib(path, 0, "align", "c"));
    _spotCfg.collectInterval = std::max(uiXml.ReadAttribFlt(path, 0, "collect_interval", _kDefaultCollectInterval), 0.01f);

    string_path tmplPath;
    xr_strconcat(tmplPath, path, ":spot_template");
    if (uiXml.NavigateToNode(tmplPath, 0))
    {
        _spotCfg.spotWidth = uiXml.ReadAttribFlt(tmplPath, 0, "width", 0.0f);
        _spotCfg.spotHeight = uiXml.ReadAttribFlt(tmplPath, 0, "height", 0.0f);
    }

    const CUIXmlInit::ColorDefs* colorDefs = CUIXmlInit::GetColorDefs();
    const char* defaultColorName = uiXml.ReadAttrib(path, 0, "color", "ui_1");
    CUIXmlInit::ColorDefs::const_iterator colorIt = colorDefs->find(defaultColorName);
    _spotCfg.defaultSpotColor = (colorIt != colorDefs->end()) ? colorIt->second : _kDefaultColorWhite;
}

void CUICompassBar::InitCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit, CUIWindow* stripParent)
{
    const char* stripPath = uiXml.NavigateToNode("compass_bar:compass_dial", 0) ?
        "compass_bar:compass_dial:strip" : "compass_bar:strip";

    string_path cardinalsPathBuf;
    xr_strconcat(cardinalsPathBuf, stripPath, ":cardinal_points");
    const char* cardinalsPath = uiXml.NavigateToNode(cardinalsPathBuf, 0) ?
        cardinalsPathBuf : "compass_bar:cardinal_points";

    if (!stripParent || !uiXml.NavigateToNode(stripPath, 0))
    {
        return;
    }

    _stripTexWidth = uiXml.ReadAttribFlt(stripPath, 0, "tex_width", _kDefaultStripTexWidth);
    _stripTexLoop = uiXml.ReadAttribInt(stripPath, 0, "tex_loop", 1) != 0;
    _runtimeCfg.cardinalFakeDistance = uiXml.ReadAttribFlt(cardinalsPath, 0, "fake_target_distance", _kDefaultFakeTargetDistance);

    _stripRelPos.x = uiXml.ReadAttribFlt(stripPath, 0, "x", 0.0f);
    _stripRelPos.y = uiXml.ReadAttribFlt(stripPath, 0, "y", 0.0f);
    _stripRelSize.x = uiXml.ReadAttribFlt(stripPath, 0, "width", 1.0f);
    _stripRelSize.y = uiXml.ReadAttribFlt(stripPath, 0, "height", 1.0f);

    _stripContainer = new CUICompassClipWindow();
    _stripContainer->SetAutoDelete(true);
    xmlInit.InitWindow(uiXml, stripPath, 0, _stripContainer);
    stripParent->AttachChild(_stripContainer);

    string_path texPath;
    xr_strconcat(texPath, stripPath, ":texture");
    shared_str texName = uiXml.Read(texPath, 0, "ui_inGame2_compass_dial");
    // Explicit draw units; legacy width/height/x/y remain aliases.
    const float legacyScaleX = uiXml.ReadAttribFlt(texPath, 0, "width", 1.0f);
    const float legacyScaleY = uiXml.ReadAttribFlt(texPath, 0, "height", 1.0f);
    const float drawScale = uiXml.ReadAttribFlt(texPath, 0, "draw_scale", -1.0f);
    _stripTextureScaleX = uiXml.ReadAttribFlt(texPath, 0, "draw_scale_x", (drawScale > 0.0f) ? drawScale : legacyScaleX);
    _stripTextureScaleY = uiXml.ReadAttribFlt(texPath, 0, "draw_scale_y", (drawScale > 0.0f) ? drawScale : legacyScaleY);
    _stripTextureOffsetX = uiXml.ReadAttribFlt(texPath, 0, "draw_offset_x",
        uiXml.ReadAttribFlt(texPath, 0, "x", 0.0f));
    _stripTextureOffsetY = uiXml.ReadAttribFlt(texPath, 0, "draw_offset_y",
        uiXml.ReadAttribFlt(texPath, 0, "y", 0.0f));
    _stripTextureStretch = uiXml.ReadAttribInt(texPath, 0, "stretch", 1) != 0;

    _strip = new CUIStatic();
    _strip->SetAutoDelete(true);
    _strip->SetWndPos(Fvector2().set(0.0f, 0.0f));
    _strip->SetWndSize(Fvector2().set(1.0f, 1.0f));
    _strip->InitTexture(texName.c_str(), false) || _strip->InitTexture("ui_inGame2_compass_dial", false);
    _strip->SetStretchTexture(_stripTextureStretch);
    _strip->SetTextureColor(CUIXmlInit::GetColor(uiXml, texPath, 0, 0xFFFFFFFF));

    _stripBaseTexRect = _strip->GetTextureRect();
    _stripNativeTexSize.set(_stripBaseTexRect.width(), _stripBaseTexRect.height());

    if (_stripNativeTexSize.x > 0.0f)
    {
        const float atlasRatio = _stripTexWidth / _stripNativeTexSize.x;
        if (atlasRatio < 0.5f || atlasRatio > 2.0f)
        {
            Msg("! CUICompassBar: tex_width (%.0f) differs strongly from atlas width (%.0f)",
                _stripTexWidth, _stripNativeTexSize.x);
        }
    }

    if (!_stripTextureStretch)
    {
        if (_stripTextureScaleX > 0.0f)
        {
            _stripBaseTexRect.x2 = _stripBaseTexRect.x1 + _stripNativeTexSize.x * _stripTextureScaleX;
        }
        if (_stripTextureScaleY > 0.0f)
        {
            _stripBaseTexRect.y2 = _stripBaseTexRect.y1 + _stripNativeTexSize.y * _stripTextureScaleY;
        }
        _strip->SetTextureRect(_stripBaseTexRect);
    }

    _stripContainer->AttachChild(_strip);

    const float defY = uiXml.ReadAttribFlt(cardinalsPath, 0, "y", 0.0f);
    const float defW = uiXml.ReadAttribFlt(cardinalsPath, 0, "width", 16.0f);
    const float defH = uiXml.ReadAttribFlt(cardinalsPath, 0, "height", 14.0f);

    _cardinalEntries.reserve(_kMaxCardinalPoints);

    SCompassCardinalMarkerConfig defaultMarkerCfg;
    string_path defaultMarkerPath;
    xr_strconcat(defaultMarkerPath, cardinalsPath, ":marker");
    ParseCardinalMarkerConfig(uiXml, defaultMarkerPath, defaultMarkerCfg);

    string_path mainPath;
    xr_strconcat(mainPath, cardinalsPath, ":main_cardinals");
    const char* mainDirs[] = { "n", "e", "s", "w" };
    for (const char* d : mainDirs)
    {
        string_path nodePath;
        xr_sprintf(nodePath, "%s:%s", mainPath, d);
        if (uiXml.NavigateToNode(nodePath, 0))
        {
            if (InitCardinalEntry(uiXml, xmlInit, cardinalsPath, mainPath, d, defY, defW, defH, defaultMarkerCfg) &&
                _stripContainer && !_cardinalEntries.empty())
            {
                _stripContainer->AttachChild(_cardinalEntries.back().host);
            }
        }
    }

    string_path interPath;
    xr_strconcat(interPath, cardinalsPath, ":inter_cardinals");
    if (uiXml.NavigateToNode(interPath, 0))
    {
        const char* interDirs[] = { "ne", "se", "sw", "nw" };
        for (const char* d : interDirs)
        {
            string_path nodePath;
            xr_sprintf(nodePath, "%s:%s", interPath, d);
            if (uiXml.NavigateToNode(nodePath, 0))
            {
                if (InitCardinalEntry(uiXml, xmlInit, cardinalsPath, interPath, d, defY, defW, defH, defaultMarkerCfg) &&
                    _stripContainer && !_cardinalEntries.empty())
                {
                    _stripContainer->AttachChild(_cardinalEntries.back().host);
                }
            }
        }
    }
}

void CUICompassBar::ParseCardinalMarkerConfig(CUIXml& uiXml, LPCSTR path, SCompassCardinalMarkerConfig& cfg) const
{
    if (!uiXml.NavigateToNode(path, 0))
    {
        return;
    }

    cfg.width = uiXml.ReadAttribFlt(path, 0, "width", cfg.width);
    cfg.height = uiXml.ReadAttribFlt(path, 0, "height", cfg.height);
    cfg.offsetY = uiXml.ReadAttribFlt(path, 0, "offset_y", cfg.offsetY);
    cfg.stretch = uiXml.ReadAttribInt(path, 0, "stretch", cfg.stretch ? 1 : 0) != 0;

    string_path texPath;
    xr_strconcat(texPath, path, ":texture");
    shared_str texName = uiXml.Read(texPath, 0, nullptr);
    if (!texName.size())
    {
        texName = uiXml.ReadAttrib(path, 0, "texture", nullptr);
    }
    if (texName.size())
    {
        cfg.texture = texName;
    }
}

CUIStatic* CUICompassBar::CreateCardinalMarker(CUIXml& uiXml, const SCompassCardinalMarkerConfig& cfg,
    LPCSTR colorPath) const
{
    if (!cfg.texture.size())
    {
        return nullptr;
    }

    CUIStatic* marker = new CUIStatic();
    marker->SetAutoDelete(true);
    if (!marker->InitTexture(cfg.texture.c_str(), false))
    {
        xr_delete(marker);
        return nullptr;
    }

    marker->SetStretchTexture(cfg.stretch);
    marker->SetTextureColor(CUIXmlInit::GetColor(uiXml, colorPath, 0, 0xFFFFFFFF));
    return marker;
}

float CUICompassBar::GetCardinalTextHeight(CUIStatic* textStatic)
{
    if (!textStatic || !textStatic->TextItemControl())
    {
        return 0.0f;
    }

    CUILines* lines = textStatic->TextItemControl();
    if (CGameFont* font = lines->GetFont())
    {
        return font->CurrentHeight_();
    }

    return textStatic->GetHeight();
}

float CUICompassBar::GetCardinalTextBottom(SCompassCardinalEntry& entry)
{
    if (!entry.host || !entry.text)
    {
        return 0.0f;
    }

    const float hostH = entry.host->GetHeight();
    const float textH = GetCardinalTextHeight(entry.text);
    CUILines* lines = entry.text->TextItemControl();
    if (!lines)
    {
        return hostH * 0.5f + textH * 0.5f;
    }

    switch (lines->GetVTextAlignment())
    {
    case valTop:
        return textH + lines->m_TextOffset.y;
    case valBotton:
        return hostH - lines->m_TextOffset.y;
    default:
        return hostH * 0.5f + textH * 0.5f + lines->m_TextOffset.y;
    }
}

float CUICompassBar::GetCardinalTextCenterX(const SCompassCardinalEntry& entry) const
{
    if (!entry.host || !entry.text)
    {
        return 0.0f;
    }

    const float hostW = entry.host->GetWidth();
    CUILines* lines = entry.text->TextItemControl();
    if (!lines || !lines->GetFont())
    {
        return hostW * 0.5f;
    }

    const char* caption = lines->GetText();
    float textW = 0.0f;
    if (caption && *caption)
    {
        textW = lines->GetFont()->SizeOf_(caption);
        UI().ClientToScreenScaledWidth(textW);
    }

    const float offsetX = lines->m_TextOffset.x;
    switch (lines->GetTextAlignment())
    {
    case CGameFont::alLeft:
        return offsetX + textW * 0.5f;
    case CGameFont::alRight:
        return offsetX + hostW - textW * 0.5f;
    case CGameFont::alCenter:
    default:
        return offsetX + hostW * 0.5f;
    }
}

void CUICompassBar::ApplyCardinalMarkerLayout(SCompassCardinalEntry& entry)
{
    if (!entry.host || !entry.marker)
    {
        return;
    }

    const float hostW = entry.host->GetWidth();
    const float hostH = entry.host->GetHeight();
    const SCompassCardinalMarkerConfig& cfg = entry.markerCfg;

    float markerW = cfg.width;
    float markerH = cfg.height;
    if (markerW > 0.0f && markerW <= 1.0f)
    {
        markerW = hostW * cfg.width;
    }
    if (markerH > 0.0f && markerH <= 1.0f)
    {
        markerH = hostH * cfg.height;
    }

    if (markerW <= 0.0f || markerH <= 0.0f)
    {
        const Frect nativeRect = entry.marker->GetTextureRect();
        if (markerW <= 0.0f)
        {
            markerW = nativeRect.width();
        }
        if (markerH <= 0.0f)
        {
            markerH = nativeRect.height();
        }
    }

    // Center tick under glyph bbox, not under host (E/W use align r/l).
    const float textCenterX = GetCardinalTextCenterX(entry);
    entry.marker->SetWndSize(Fvector2().set(markerW, markerH));
    entry.marker->SetWndPos(Fvector2().set(
        textCenterX - markerW * 0.5f,
        GetCardinalTextBottom(entry) + cfg.offsetY));
}

bool CUICompassBar::InitCardinalEntry(CUIXml& uiXml, CUIXmlInit& xmlInit, LPCSTR cardinalsPath, LPCSTR groupPath,
    LPCSTR directionNode, float defaultY, float defaultW, float defaultH,
    const SCompassCardinalMarkerConfig& defaultMarkerCfg)
{
    string_path childPath;
    string_path defaultTextPath;
    string_path groupTextPath;
    string_path childTextPath;
    string_path markerPath;
    xr_strconcat(childPath, groupPath, ":", directionNode);
    xr_strconcat(defaultTextPath, cardinalsPath, ":text");
    xr_strconcat(groupTextPath, groupPath, ":text");
    xr_strconcat(childTextPath, childPath, ":text");
    xr_strconcat(markerPath, childPath, ":marker");

    const float y = uiXml.ReadAttribFlt(childPath, 0, "y", defaultY);
    const float w = uiXml.ReadAttribFlt(childPath, 0, "width", defaultW);
    const float h = uiXml.ReadAttribFlt(childPath, 0, "height", defaultH);

    CUIWindow* host = new CUIWindow();
    host->SetAutoDelete(true);
    host->SetWndPos(Fvector2().set(0.0f, y));
    host->SetWndSize(Fvector2().set(w, h));

    CUIStatic* text = new CUIStatic();
    text->SetAutoDelete(true);
    text->SetWndPos(Fvector2().set(0.0f, 0.0f));
    text->SetWndSize(Fvector2().set(w, h));

    if (uiXml.NavigateToNode(defaultTextPath, 0))
    {
        xmlInit.InitText(uiXml, defaultTextPath, 0, text);
    }
    if (uiXml.NavigateToNode(groupTextPath, 0))
    {
        xmlInit.InitText(uiXml, groupTextPath, 0, text);
    }
    if (uiXml.NavigateToNode(childTextPath, 0))
    {
        xmlInit.InitText(uiXml, childTextPath, 0, text);
    }
    else
    {
        const char* caption = uiXml.Read(childPath, 0, nullptr);
        if (caption && *caption)
        {
            text->SetText(caption);
        }
        const char* colorAttr = uiXml.ReadAttrib(childPath, 0, "color", nullptr);
        const char* rAttr = uiXml.ReadAttrib(childPath, 0, "r", nullptr);
        if (colorAttr || rAttr)
        {
            text->SetTextColor(CUIXmlInit::GetColor(uiXml, childPath, 0, _kDefaultColorWhite));
        }
        const char* alignStr = uiXml.ReadAttrib(childPath, 0, "align", nullptr);
        if (alignStr && text->TextItemControl())
        {
            if (alignStr[0] == 'l' || alignStr[0] == 'L')
            {
                text->TextItemControl()->SetTextAlignment(CGameFont::alLeft);
            }
            else if (alignStr[0] == 'r' || alignStr[0] == 'R')
            {
                text->TextItemControl()->SetTextAlignment(CGameFont::alRight);
            }
            else
            {
                text->TextItemControl()->SetTextAlignment(CGameFont::alCenter);
            }
        }
    }

    host->AttachChild(text);

    SCompassCardinalMarkerConfig markerCfg = defaultMarkerCfg;
    ParseCardinalMarkerConfig(uiXml, markerPath, markerCfg);
    shared_str markerTextureOverride = uiXml.ReadAttrib(childPath, 0, "marker_texture", nullptr);
    if (markerTextureOverride.size())
    {
        markerCfg.texture = markerTextureOverride;
    }

    CUIStatic* marker = nullptr;
    if (markerCfg.texture.size())
    {
        string_path colorPath;
        if (uiXml.NavigateToNode(markerPath, 0))
        {
            xr_strcpy(colorPath, markerPath);
        }
        else
        {
            xr_strconcat(colorPath, cardinalsPath, ":marker");
        }
        marker = CreateCardinalMarker(uiXml, markerCfg, colorPath);
        if (marker)
        {
            host->AttachChild(marker);
        }
    }

    SCompassCardinalEntry entry;
    entry.host = host;
    entry.text = text;
    entry.marker = marker;
    entry.layout.set(y, w, h);
    entry.baseTextColor = text->GetTextColor();
    entry.baseMarkerColor = marker ? marker->GetTextureColor() : 0;
    entry.markerCfg = markerCfg;
    entry.alpha = 1.0f;
    const u32 cardinalIndex = (u32)_cardinalEntries.size();
    if (cardinalIndex < _kMaxCardinalPoints)
    {
        entry.dirXZ.set(cosf(_kCardinalAngles[cardinalIndex]), sinf(_kCardinalAngles[cardinalIndex]));
    }
    _cardinalEntries.push_back(entry);
    ApplyCardinalMarkerLayout(_cardinalEntries.back());

    return true;
}

void CUICompassBar::InitActiveTargetWidgets(CUIXml& uiXml, CUIXmlInit& xmlInit)
{
    const char* markerPath = "compass_bar:active_target:marker";
    const char* texAttr = uiXml.ReadAttrib(markerPath, 0, "texture", nullptr);
    if (texAttr && xr_strlen(texAttr) > 0)
    {
        _activeMarkerFallbackTexture = texAttr;
    }
    else if (uiXml.NavigateToNode(markerPath, 0))
    {
        _activeMarkerFallbackTexture = uiXml.Read(markerPath, 0, "ui_inGame2_hint_wnd_main_window");
    }

    _activeTargetContainer = new CUIWindow();
    _activeTargetContainer->SetAutoDelete(true);
    float defaultContainerW = uiXml.ReadAttribFlt("compass_bar:active_target", 0, "width", 100.0f);
    float defaultContainerH = uiXml.ReadAttribFlt("compass_bar:active_target", 0, "height", 24.0f);
    if (uiXml.NavigateToNode("compass_bar:active_target", 0))
    {
        xmlInit.InitWindow(uiXml, "compass_bar:active_target", 0, _activeTargetContainer);
        if (_activeTargetContainer->GetWidth() <= 0.0f || _activeTargetContainer->GetHeight() <= 0.0f)
        {
            _activeTargetContainer->SetWndSize(Fvector2().set(defaultContainerW, defaultContainerH));
        }
    }
    else
    {
        _activeTargetContainer->SetWndSize(Fvector2().set(defaultContainerW, defaultContainerH));
        _activeTargetContainer->SetWndPos(Fvector2().set(0.0f, 0.0f));
    }
    if (_layerFg)
    {
        _layerFg->AttachChild(_activeTargetContainer);
    }
    if (uiXml.NavigateToNode("compass_bar:active_target:altitude_arrow", 0))
    {
        const char* arrowPath = "compass_bar:active_target:altitude_arrow";
        _altitudeArrowTextureUp = uiXml.ReadAttrib(arrowPath, 0, "texture_up", "ui_inGame2_compass_altitude_up");
        _altitudeArrowTextureDown = uiXml.ReadAttrib(arrowPath, 0, "texture_down", "ui_inGame2_compass_altitude_down");
        const float deadzone = uiXml.ReadAttribFlt(arrowPath, 0, "altitude_deadzone", _runtimeCfg.altitudeDeadzone);
        if (deadzone > 0.0f)
        {
            _runtimeCfg.altitudeDeadzone = deadzone;
        }
        _activeAltitudeArrow = new CUIStatic();
        _activeAltitudeArrow->SetAutoDelete(false);
        if (xmlInit.InitWindow(uiXml, arrowPath, 0, _activeAltitudeArrow))
        {
            _activeAltitudeArrow->InitTexture(_altitudeArrowTextureUp.c_str(), false);
            _activeAltitudeArrow->SetStretchTexture(uiXml.ReadAttribInt(arrowPath, 0, "stretch", 1) != 0);
            _activeTargetContainer->AttachChild(_activeAltitudeArrow);
        }
        else
        {
            xr_delete(_activeAltitudeArrow);
            _activeAltitudeArrow = nullptr;
        }
    }
    if (uiXml.NavigateToNode("compass_bar:active_target:distance_text", 0))
    {
        const char* distPath = "compass_bar:active_target:distance_text";
        const char* stFormat = uiXml.ReadAttrib(distPath, 0, "st_format", nullptr);
        if (stFormat && xr_strlen(stFormat) > 0 && g_pStringTable)
        {
            _runtimeCfg.distanceFormat = g_pStringTable->translate(stFormat);
        }
        else
        {
            const char* textFormat = uiXml.ReadAttrib(distPath, 0, "text_format", nullptr);
            if (!textFormat || !*textFormat)
            {
                textFormat = uiXml.ReadAttrib(distPath, 0, "format", "%.0f m");
            }
            _runtimeCfg.distanceFormat = textFormat;
        }

        _activeDistText = UIHelper::CreateStatic(uiXml, distPath, _activeTargetContainer, false);
        if (_activeDistText)
        {
            _activeDistText->SetAutoDelete(false);
        }
    }
    if (uiXml.NavigateToNode("compass_bar:active_target:marker", 0))
    {
        _activeMarker = UIHelper::CreateStatic(uiXml, markerPath, _activeTargetContainer, false);
        if (_activeMarker)
        {
            _activeMarker->SetAutoDelete(false);
        }
    }
}

void CUICompassBar::CreateDefaultActiveTargetWidgets(CUIXml& uiXml)
{
    if (!_activeTargetContainer)
    {
        return;
    }
    if (!_activeMarkerFallbackTexture.size())
    {
        _activeMarkerFallbackTexture = "ui_inGame2_hint_wnd_main_window";
    }
    if (!_activeDistText)
    {
        _activeDistText = new CUIStatic();
        _activeDistText->SetAutoDelete(false);
        float distTextW = uiXml.ReadAttribFlt("compass_bar:active_target:distance_text", 0, "width", 80.0f);
        float distTextH = uiXml.ReadAttribFlt("compass_bar:active_target:distance_text", 0, "height", 14.0f);
        _activeDistText->SetWndSize(Fvector2().set(distTextW, distTextH));
        _activeDistText->SetWndPos(Fvector2().set(0.0f, 0.0f));
        const char* fontName = uiXml.ReadAttrib("compass_bar:active_target:distance_text", 0, "font", "ui_font_letterica18");
        CGameFont* font = UI().Font().GetFont(fontName);
        if (font)
        {
            _activeDistText->SetFont(font);
        }
        const char* colorName = uiXml.ReadAttrib("compass_bar:active_target:distance_text", 0, "color", "ui_1");
        CUIXmlInit::ColorDefs::const_iterator colorIt = CUIXmlInit::GetColorDefs()->find(colorName);
        u32 textColor = (colorIt != CUIXmlInit::GetColorDefs()->end()) ? colorIt->second : _kDefaultColorWhite;
        _activeDistText->SetTextColor(textColor);
        if (_activeDistText->TextItemControl())
        {
            _activeDistText->TextItemControl()->SetTextAlignment(CGameFont::alCenter);
            _activeDistText->TextItemControl()->SetVTextAlignment(valCenter);
        }
        _activeTargetContainer->AttachChild(_activeDistText);
    }
    if (!_activeMarker)
    {
        _activeMarker = new CUIStatic();
        _activeMarker->SetAutoDelete(false);
        float markerW = uiXml.ReadAttribFlt("compass_bar:active_target:marker", 0, "width", 15.0f);
        float markerH = uiXml.ReadAttribFlt("compass_bar:active_target:marker", 0, "height", 18.0f);
        _activeMarker->SetWndSize(Fvector2().set(markerW, markerH));
        _activeMarker->SetWndPos(Fvector2().set(0.0f, 0.0f));
        _activeMarker->SetStretchTexture(true);
        _activeMarker->InitTexture(_activeMarkerFallbackTexture.c_str(), false);
        _activeTargetContainer->AttachChild(_activeMarker);
    }
}

// --- Layout ---

void CUICompassBar::ApplyRelativeLayout()
{
    ApplyMainWindowLayout();
    ApplyLayerLayouts();
    ApplyStripLayout();
    ApplyCardinalsLayout();
    const float kx = UI().get_current_kx();
    if (kx > 0.0f && kx != 1.0f)
    {
        if (_activeMarker)
        {
            float w = _activeMarker->GetWidth();
            float h = _activeMarker->GetHeight();
            _activeMarker->SetWndSize(Fvector2().set(w * kx, h));
        }
        if (_activeAltitudeArrow)
        {
            float w = _activeAltitudeArrow->GetWidth();
            float h = _activeAltitudeArrow->GetHeight();
            _activeAltitudeArrow->SetWndSize(Fvector2().set(w * kx, h));
        }
    }
    InvalidateStripGeometry();
}

void CUICompassBar::ApplyMainWindowLayout()
{
    // compass_bar x/y/width/height are always parent-relative fractions (no ProbablyRelative heuristic).
    const float k = UI().get_current_kx();
    Fvector2 size;
    size.y = _layoutUnits.barRelSize.y * UI_BASE_HEIGHT;
    size.x = _layoutUnits.barRelSize.x * UI_BASE_WIDTH * k;
    SetWndSize(size);

    Fvector2 pos;
    pos.x = _layoutUnits.barRelPos.x * UI_BASE_WIDTH;
    pos.y = _layoutUnits.barRelPos.y * UI_BASE_HEIGHT;
    SetWndPos(pos);
}

void CUICompassBar::ApplyLayerLayouts()
{
    const Fvector2 wndSize = GetWndSize();
    const Fvector2 zeroPos = Fvector2().set(0.0f, 0.0f);

    if (_layerBg)
    {
        _layerBg->SetWndSize(wndSize);
        _layerBg->SetWndPos(zeroPos);
    }
    if (_layerFg)
    {
        _layerFg->SetWndSize(wndSize);
        _layerFg->SetWndPos(zeroPos);
    }
    if (_background)
    {
        _background->SetWndSize(wndSize);
        const bool isCentered = (_background->GetAlignment() == waCenter);
        _background->SetWndPos(Fvector2().set(
            isCentered ? GetWidth() * 0.5f : 0.0f,
            isCentered ? GetHeight() * 0.5f : 0.0f));
    }
}

void CUICompassBar::ApplyStripLayout()
{
    if (!_stripContainer)
    {
        return;
    }

    const float parentW = GetWidth();
    const float parentH = GetHeight();
    _stripContainer->SetWndSize(Fvector2().set(_stripRelSize.x * parentW, _stripRelSize.y * parentH));
    _stripContainer->SetWndPos(Fvector2().set(_stripRelPos.x * parentW, _stripRelPos.y * parentH));

    if (_strip)
    {
        const float cw = _stripContainer->GetWidth();
        const float ch = _stripContainer->GetHeight();
        float texW = 0.0f;
        float texH = 0.0f;

        if (_stripTextureStretch)
        {
            texW = cw * _stripTextureScaleX;
            texH = ch * _stripTextureScaleY;
        }
        else
        {
            texW = _stripNativeTexSize.x * _stripTextureScaleX;
            texH = _stripNativeTexSize.y * _stripTextureScaleY;
        }

        _strip->SetWndSize(Fvector2().set(texW, texH));
        _strip->SetWndPos(Fvector2().set((cw - texW) * 0.5f + _stripTextureOffsetX, (ch - texH) * 0.5f + _stripTextureOffsetY));
    }
    _stripWidth = _stripContainer->GetWidth();
}

void CUICompassBar::ApplyCardinalsLayout()
{
    if (!_stripContainer || _cardinalEntries.empty())
    {
        return;
    }

    const float cw = _stripContainer->GetWidth();
    const float ch = _stripContainer->GetHeight();

    for (SCompassCardinalEntry& entry : _cardinalEntries)
    {
        if (!entry.host || !entry.text)
        {
            continue;
        }

        const Fvector3& layout = entry.layout;
        const float hostW = layout.y * cw;
        const float hostH = layout.z * ch;
        entry.host->SetWndPos(Fvector2().set(0.0f, layout.x * ch));
        entry.host->SetWndSize(Fvector2().set(hostW, hostH));
        entry.text->SetWndPos(Fvector2().set(0.0f, 0.0f));
        entry.text->SetWndSize(Fvector2().set(hostW, hostH));
        ApplyCardinalMarkerLayout(entry);
    }
}

EUIItemAlign CUICompassBar::ParseAlign(const char* alignStr)
{
    if (!alignStr || !*alignStr)
    {
        return alCenter;
    }
    if (alignStr[0] == 'l' || alignStr[0] == 'L')
    {
        return alLeft;
    }
    if (alignStr[0] == 'r' || alignStr[0] == 'R')
    {
        return alRight;
    }
    return alCenter;
}

// --- Frame context ---

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
    if (_stripGeometryCached && _stripContainer)
    {
        return _cachedStripGeometry;
    }
    SCompassStripGeometry geom;
    if (_stripContainer)
    {
        Frect rect;
        _stripContainer->GetWndRect(rect);
        geom.left = rect.lt.x;
        geom.top = rect.lt.y;
        geom.width = rect.width();
        geom.height = rect.height();
    }
    _cachedStripGeometry = geom;
    _stripGeometryCached = true;
    return geom;
}

void CUICompassBar::InvalidateStripGeometry()
{
    _stripGeometryCached = false;
}

void CUICompassBar::MarkSpotsDirty()
{
    _dirty.spotsDirty = true;
}

u32 CUICompassBar::ComputeCandidateHash() const
{
    u32 hash = (u32)_spotCandidates.size();
    for (const SSpotCandidate& cand : _spotCandidates)
    {
        hash ^= (u32)(size_t)cand.sourceLoc;
        hash ^= (u32)(size_t)cand.textureName.c_str();
        hash ^= *(const u32*)&cand.iconSize.x;
        hash ^= *(const u32*)&cand.iconSize.y;
    }
    return hash;
}

bool CUICompassBar::IsHeadingPixelDirty(float heading) const
{
    if (_stripWidth <= 0.0f || _runtimeCfg.fovRad <= 0.0f)
    {
        return true;
    }

    const float delta = angle_normalize_signed(heading - _dirty.lastHeading);
    const float halfFov = _runtimeCfg.fovRad * 0.5f;
    const float projectedDeltaPx = std::abs(delta / halfFov) * (_stripWidth * 0.5f);
    return projectedDeltaPx >= _kHeadingPixelEpsilon;
}

bool CUICompassBar::HasFadingSpots() const
{
    for (float alpha : _poolSpotAlpha)
    {
        if (alpha > _runtimeCfg.minVisibleAlpha && alpha < (1.0f - _kAlphaSaturatedEpsilon))
        {
            return true;
        }
    }
    for (const SCompassCardinalEntry& entry : _cardinalEntries)
    {
        if (entry.alpha > _runtimeCfg.minVisibleAlpha && entry.alpha < (1.0f - _kAlphaSaturatedEpsilon))
        {
            return true;
        }
    }
    return false;
}

// --- Strip ---

bool CUICompassBar::ProjectToStrip(const Fvector& targetPos, const Fvector& actorPos, float camHeading,
    float& outX, bool clampToEdges) const
{
    if (_runtimeCfg.fovRad <= 0.0f || _stripWidth <= 0.0f)
    {
        return false;
    }
    Fvector2 dir;
    dir.set(targetPos.x - actorPos.x, targetPos.z - actorPos.z);
    if (dir.square_magnitude() < _kMinDistanceSq)
    {
        outX = 0.0f;
        return true;
    }
    float targetYaw = dir.getH();
    float delta = angle_normalize_signed(targetYaw - camHeading);
    float halfFov = _runtimeCfg.fovRad * 0.5f;
    if (clampToEdges)
    {
        delta = clampr(delta, -halfFov, halfFov);
    }
    else
    {
        if (delta < -halfFov || delta > halfFov)
        {
            return false;
        }
    }
    float halfW = _stripWidth * 0.5f;
    outX = -(delta / halfFov) * halfW;
    return true;
}

void CUICompassBar::UpdateStrip(float heading)
{
    if (!_strip || !_stripContainer)
    {
        return;
    }

    static const float kHalfCircleRad = deg2rad(_kHalfCircleRad);
    static const float kTwoHalfCirclesRad = deg2rad(_kTwoPiRad);
    const float uvCenter = (heading + kHalfCircleRad) / kTwoHalfCirclesRad;
    const float atlasCircumference = _stripNativeTexSize.x;
    const float stripTexW = _stripTexWidth > 0.0f ? _stripTexWidth : _kDefaultStripTexWidth;
    const float texToAtlas = (stripTexW > 0.0f) ? (atlasCircumference / stripTexW) : 1.0f;
    const float kx = UI().get_current_kx();
    const float widgetW = _strip->GetWidth();

    float winWAtlas = 0.0f;
    if (_stripTextureStretch)
    {
        winWAtlas = (widgetW / kx) * texToAtlas;
    }
    else
    {
        winWAtlas = _stripBaseTexRect.width();
    }

    if (winWAtlas <= 0.0f || atlasCircumference <= 0.0f)
    {
        return;
    }

    float uAtlas = uvCenter * atlasCircumference - winWAtlas * 0.5f;
    if (_stripTexLoop)
    {
        uAtlas = fmodf(uAtlas, atlasCircumference);
        if (uAtlas < 0.0f)
        {
            uAtlas += atlasCircumference;
        }
    }
    else
    {
        uAtlas = clampr(uAtlas, 0.0f, atlasCircumference - winWAtlas);
    }

    if (std::abs(uAtlas - _dirty.lastStripU) < 0.01f)
    {
        return;
    }
    _dirty.lastStripU = uAtlas;

    Frect rect;
    rect.lt.y = _stripBaseTexRect.lt.y;
    rect.rb.y = _stripBaseTexRect.rb.y;
    rect.lt.x = _stripBaseTexRect.x1 + uAtlas;
    rect.rb.x = rect.lt.x + winWAtlas;
    _strip->SetTextureRect(rect);
}

// --- Cardinals ---

float CUICompassBar::UpdateFadeAlpha(float alpha, bool isVisible, float fadeInSpeed, float fadeOutSpeed) const
{
    const float speed = std::max(isVisible ? fadeInSpeed : fadeOutSpeed, 1.0f);
    const float target = isVisible ? 1.0f : 0.0f;
    const float delta = target - alpha;
    const float t = clampr(Device.fTimeDelta * speed, 0.0f, 1.0f);
    const float smoothT = 1.0f - (1.0f - t) * (1.0f - t);
    return clampr(alpha + delta * smoothT, 0.0f, 1.0f);
}

float CUICompassBar::CalculateFovEdgeFade(float relX, float stripWidth) const
{
    if (stripWidth <= 0.0f)
    {
        return 1.0f;
    }
    const float normalizedX = (relX + stripWidth * 0.5f) / stripWidth;
    if (normalizedX <= _runtimeCfg.fovFadeEdgeLo)
    {
        return 0.0f;
    }
    if (normalizedX >= _runtimeCfg.fovFadeEdgeHi)
    {
        return 0.0f;
    }
    if (normalizedX < _runtimeCfg.fovFadeInner)
    {
        const float range = _runtimeCfg.fovFadeInner - _runtimeCfg.fovFadeEdgeLo;
        const float t = (range > 0.0f) ? (normalizedX - _runtimeCfg.fovFadeEdgeLo) / range : 1.0f;
        return t * t;
    }
    if (normalizedX > _runtimeCfg.fovFadeOuter)
    {
        const float range = _runtimeCfg.fovFadeEdgeHi - _runtimeCfg.fovFadeOuter;
        const float t = (range > 0.0f) ? (_runtimeCfg.fovFadeEdgeHi - normalizedX) / range : 1.0f;
        return t * t;
    }
    return 1.0f;
}

void CUICompassBar::EnsureFadeStorage()
{
    const size_t spotCount = _poolSpots.size();

    if (spotCount > _fadeStorageSpotCount)
    {
        _poolSpotAlpha.resize(spotCount, 0.0f);
        _poolSpotBaseColor.resize(spotCount, _kDefaultColorWhite);
        _fadeStorageSpotCount = spotCount;
    }
}

void CUICompassBar::UpdateCardinals(const SCompassFrameContext& ctx)
{
    if (!_stripContainer || _cardinalEntries.empty())
    {
        return;
    }

    SCompassStripGeometry geom = GetStripGeometry();
    EnsureFadeStorage();
    const bool headingDirty = IsHeadingPixelDirty(ctx.heading);

    for (u32 i = 0; i < _cardinalEntries.size() && i < _kMaxCardinalPoints; ++i)
    {
        SCompassCardinalEntry& entry = _cardinalEntries[i];
        CUIWindow* host = entry.host;
        CUIStatic* text = entry.text;
        if (!host || !text)
        {
            continue;
        }

        Fvector fakeTarget;
        fakeTarget.set(
            ctx.actorPos.x + entry.dirXZ.x * _runtimeCfg.cardinalFakeDistance,
            ctx.actorPos.y,
            ctx.actorPos.z + entry.dirXZ.y * _runtimeCfg.cardinalFakeDistance);

        float relX;
        const bool isVisible = ProjectToStrip(fakeTarget, ctx.actorPos, ctx.heading, relX, false);
        entry.lastRelX = relX;

        const float prevAlpha = entry.alpha;
        entry.alpha = UpdateFadeAlpha(entry.alpha, isVisible, _runtimeCfg.fadeInSpeed, _runtimeCfg.fadeOutSpeed);
        const float edgeFade = isVisible ? CalculateFovEdgeFade(relX, geom.width) : 0.0f;
        const float finalAlpha = entry.alpha * edgeFade;
        const bool alphaStable = (std::abs(entry.alpha - prevAlpha) <= _kAlphaSaturatedEpsilon) &&
                                 (entry.alpha <= _runtimeCfg.minVisibleAlpha ||
                                  entry.alpha >= (1.0f - _kAlphaSaturatedEpsilon));
        const bool skipUiMutation = !headingDirty && alphaStable && (host->IsShown() == (finalAlpha > _runtimeCfg.minVisibleAlpha));

        if (skipUiMutation)
        {
            continue;
        }

        if (finalAlpha > _runtimeCfg.minVisibleAlpha)
        {
            if (isVisible)
            {
                const float hostW = host->GetWidth();
                const float posX = geom.CenterX() + relX - hostW * 0.5f;
                host->SetWndPos(Fvector2().set(posX, host->GetWndPos().y));
            }
            const u32 alpha = (u32)clampr(iFloor(float(color_get_A(entry.baseTextColor)) * finalAlpha), 0, 255);
            text->SetTextColor(subst_alpha(entry.baseTextColor, alpha));
            text->Show(true);
            if (entry.marker)
            {
                entry.marker->SetTextureColor(subst_alpha(entry.baseMarkerColor, alpha));
                entry.marker->Show(true);
            }
            host->Show(true);
        }
        else
        {
            host->Show(false);
        }
    }
}

// --- Spots (collect / queue / pool) ---

bool CUICompassBar::ShouldShowSpot(CMapLocation* loc, const Fvector& actorPos, const shared_str& levelName,
    CMapLocation* activeTaskLoc) const
{
    return loc && loc != activeTaskLoc && (loc->ShowOnCompass() || loc->HasCompassConfig()) &&
           loc->SpotEnabled() && loc->Update() && loc->GetLevelName() == levelName &&
           loc->GetCompassTexture().size() > 0;
}

SSpotCandidate CUICompassBar::CreateSpotCandidate(CMapLocation* loc) const
{
    SSpotCandidate cand;
    cand.sourceLoc = loc;
    cand.pos = loc->GetLastPosition();
    cand.textureName = loc->GetCompassTexture();

    const u32 locColor = loc->GetCompassColor();
    cand.color = (locColor != 0) ? locColor : (_spotCfg.defaultSpotColor != 0) ? _spotCfg.defaultSpotColor : _kDefaultColorWhite;

    cand.offsetY = loc->GetCompassOffsetY();
    cand.offsetX = loc->GetCompassOffsetX();
    cand.valign = loc->GetCompassVertAlign();

    const Fvector2 locSize = loc->GetCompassSize();
    cand.iconSize = (locSize.x > 0.0f && locSize.y > 0.0f) ? locSize : Fvector2().set(_spotCfg.spotWidth, _spotCfg.spotHeight);

    return cand;
}

void CUICompassBar::CollectSpotCandidates(const Fvector& actorPos, const shared_str& levelName)
{
    _spotCandidates.clear();
    if (!_spotCfg.show || !_strip)
    {
        return;
    }
    CMapManager* mapManager = &Level().MapManager();
    if (!mapManager)
    {
        return;
    }

    _spotCollectScratch.clear();
    {
        xrCriticalSectionGuard guard(mapManager->UpdateCS);
        const Locations& locations = mapManager->Locations();
        if (_spotCollectScratch.capacity() < locations.size())
        {
            _spotCollectScratch.reserve(locations.size());
        }
        for (const SLocationKey& key : locations)
        {
            if (key.location)
            {
                _spotCollectScratch.push_back(key.location);
            }
        }
    }

    if (_spotCandidates.capacity() < _spotCollectScratch.size())
    {
        _spotCandidates.reserve(_spotCollectScratch.size());
    }

    CMapLocation* activeTaskLoc = _activeTargetLoc;
    for (CMapLocation* loc : _spotCollectScratch)
    {
        if (!ShouldShowSpot(loc, actorPos, levelName, activeTaskLoc))
        {
            continue;
        }
        Fvector pos = loc->GetLastPosition();
        const float maxDist = loc->GetCompassMaxDist();
        if (maxDist > 0.0f && actorPos.distance_to(pos) > maxDist)
        {
            continue;
        }
        SSpotCandidate cand = CreateSpotCandidate(loc);
        cand.distance = actorPos.distance_to(pos);
        _spotCandidates.push_back(cand);
    }

    const u32 candidateHash = ComputeCandidateHash();
    if (candidateHash != _dirty.lastCandidateHash)
    {
        _dirty.membershipChanged = true;
        _dirty.lastCandidateHash = candidateHash;
        MarkSpotsDirty();
    }
    // Same membership can still move in world space; refresh projected X/alpha.
    _dirty.layoutRefresh = true;
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
        SSpotRenderItem item;
        item.relX = relX + _spotCfg.offsetX + cand.offsetX;
        item.sourceLoc = cand.sourceLoc;
        if (_spotCfg.align == alRight)
        {
            item.relX -= cand.iconSize.x;
        }
        else if (_spotCfg.align == alCenter)
        {
            item.relX -= cand.iconSize.x * 0.5f;
        }
        item.sortDist = cand.distance;
        item.offsetY = cand.offsetY;
        item.valign = cand.valign;
        item.textureName = &cand.textureName;
        item.iconSize = cand.iconSize;
        item.color = cand.color;
        _renderQueue.push_back(item);
    }
}

CUIStatic* CUICompassBar::GetSpotFromPool(xr_vector<CUIStatic*>& pool, CUIWindow* parent, u32 index)
{
    if (!parent)
    {
        return nullptr;
    }

    if (index < pool.size())
    {
        return pool[index];
    }

    CUIStatic* item = new CUIStatic();
    item->SetAutoDelete(true);
    item->SetStretchTexture(true);
    parent->AttachChild(item);

    pool.push_back(item);
    _poolSpotOwners.push_back(nullptr);
    _poolSpotAlpha.push_back(0.0f);
    _poolSpotBaseColor.push_back(_kDefaultColorWhite);
    _fadeStorageSpotCount = pool.size();

    return item;
}

u32 CUICompassBar::AllocateSpotPoolSlot(CMapLocation* sourceLoc)
{
    xr_hash_map<CMapLocation*, u32>::iterator mapped = _spotSlotByLoc.find(sourceLoc);
    if (mapped != _spotSlotByLoc.end())
    {
        const u32 mappedIdx = mapped->second;
        if (mappedIdx < _poolSpotOwners.size() && _poolSpotOwners[mappedIdx] == sourceLoc)
        {
            return mappedIdx;
        }
        _spotSlotByLoc.erase(mapped);
    }

    for (u32 i = 0; i < _poolSpots.size(); ++i)
    {
        const bool slotFree = (_poolSpotOwners[i] == nullptr) ||
                              (_poolSpotAlpha[i] <= _runtimeCfg.minVisibleAlpha &&
                               (i >= _poolSlotUsed.size() || !_poolSlotUsed[i]));
        if (slotFree)
        {
            return i;
        }
    }

    return (u32)_poolSpots.size();
}

void CUICompassBar::CommitLayout(bool positionsOnly)
{
    if (!_stripContainer || !_layerFg)
    {
        return;
    }

    if (!positionsOnly)
    {
        std::sort(_renderQueue.begin(), _renderQueue.end());
    }

    if (_poolSpotTextureNames.capacity() < _renderQueue.size())
    {
        _poolSpotTextureNames.reserve(_renderQueue.size());
    }

    SCompassStripGeometry geom = GetStripGeometry();
    const float compassBarHeight = GetHeight();
    const float kx = UI().get_current_kx();
    EnsureFadeStorage();
    _poolSlotUsed.assign(_poolSpots.size(), 0);

    for (const SSpotRenderItem& item : _renderQueue)
    {
        const u32 poolIdx = AllocateSpotPoolSlot(item.sourceLoc);
        CUIStatic* wnd = GetSpotFromPool(_poolSpots, _layerFg, poolIdx);
        if (!wnd)
        {
            continue;
        }
        if (_poolSlotUsed.size() <= poolIdx)
        {
            _poolSlotUsed.resize(poolIdx + 1, 0);
        }
        if (_poolSpotTextureNames.size() <= poolIdx)
        {
            _poolSpotTextureNames.resize(poolIdx + 1);
        }

        _spotSlotByLoc[item.sourceLoc] = poolIdx;
        _poolSpotOwners[poolIdx] = item.sourceLoc;
        _poolSlotUsed[poolIdx] = 1;
        _poolSpotBaseColor[poolIdx] = item.color;

        if (!positionsOnly || _poolSpotTextureNames[poolIdx] != *item.textureName)
        {
            if (_poolSpotTextureNames[poolIdx] != *item.textureName)
            {
                CUITextureMaster::InitTexture(*item.textureName, &wnd->GetUIStaticItem());
                _poolSpotTextureNames[poolIdx] = *item.textureName;
            }
            Fvector2 spotSize(item.iconSize.x * kx, item.iconSize.y);
            wnd->SetWndSize(spotSize);
        }

        float posOffsetX = 0.0f;
        if (kx > 0.0f && kx != 1.0f)
        {
            if (_spotCfg.align == alCenter)
            {
                posOffsetX = item.iconSize.x * 0.5f * (1.0f - kx);
            }
            else if (_spotCfg.align == alRight)
            {
                posOffsetX = item.iconSize.x * (1.0f - kx);
            }
        }
        const float stripCenterX = geom.left + geom.CenterX();
        const float posX = stripCenterX + item.relX + posOffsetX;
        float posY;
        switch (item.valign)
        {
            case valTop:
            {
                posY = item.offsetY;
                break;
            }

            case valBotton:
            {
                posY = compassBarHeight + item.offsetY - item.iconSize.y;
                break;
            }

            case valCenter:
            default:
            {
                posY = compassBarHeight * 0.5f + item.offsetY - item.iconSize.y * 0.5f;
                break;
            }
        }
        wnd->SetWndPos(Fvector2().set(posX, posY));
        _poolSpotAlpha[poolIdx] = UpdateFadeAlpha(_poolSpotAlpha[poolIdx], true, _runtimeCfg.fadeInSpeed, _runtimeCfg.fadeOutSpeed);
        const float edgeFade = CalculateFovEdgeFade(item.relX, geom.width);
        const float finalAlpha = _poolSpotAlpha[poolIdx] * edgeFade;
        if (finalAlpha > _runtimeCfg.minVisibleAlpha)
        {
            u32 baseColor = _poolSpotBaseColor[poolIdx];
            u32 alpha = (u32)clampr(iFloor(float(color_get_A(baseColor)) * finalAlpha), 0, 255);
            wnd->SetTextureColor(subst_alpha(baseColor, alpha));
            wnd->Show(true);
        }
        else
        {
            wnd->Show(false);
        }
    }

    for (u32 i = 0; i < _poolSpots.size(); ++i)
    {
        if (i < _poolSlotUsed.size() && _poolSlotUsed[i])
        {
            continue;
        }

        CUIStatic* wnd = _poolSpots[i];
        if (!wnd)
        {
            continue;
        }
        _poolSpotAlpha[i] = UpdateFadeAlpha(_poolSpotAlpha[i], false, _runtimeCfg.fadeInSpeed, _runtimeCfg.fadeOutSpeed);
        const float lastPosX = wnd->GetWndPos().x;
        const bool isWithinStrip = (lastPosX >= geom.left) && (lastPosX <= geom.left + geom.width);
        if (_poolSpotAlpha[i] > _runtimeCfg.minVisibleAlpha && isWithinStrip)
        {
            u32 baseColor = (i < _poolSpotBaseColor.size()) ? _poolSpotBaseColor[i] : _kDefaultColorWhite;
            u32 alpha = (u32)clampr(iFloor(float(color_get_A(baseColor)) * _poolSpotAlpha[i]), 0, 255);
            wnd->SetTextureColor(subst_alpha(baseColor, alpha));
            wnd->Show(true);
        }
        else
        {
            wnd->Show(false);
            if (_poolSpotOwners[i])
            {
                _spotSlotByLoc.erase(_poolSpotOwners[i]);
            }
            _poolSpotOwners[i] = nullptr;
        }
    }
}

void CUICompassBar::UpdateSpotsLayout(float heading, const SCompassFrameContext& ctx)
{
    const bool headingChanged = IsHeadingPixelDirty(heading);
    const bool membershipChanged = _dirty.membershipChanged;
    const bool fading = HasFadingSpots();

    if (!_dirty.spotsDirty && !headingChanged && !membershipChanged && !fading && !_dirty.layoutRefresh)
    {
        return;
    }

    BuildRenderQueueFromCandidates(heading, ctx.actorPos);
    const bool positionsOnly = !membershipChanged && !_dirty.spotsDirty;
    CommitLayout(positionsOnly);

    _dirty.lastHeading = heading;
    _dirty.spotsDirty = false;
    _dirty.membershipChanged = false;
    _dirty.layoutRefresh = false;
}

// --- Active target ---

void CUICompassBar::CalculateActiveTargetPosition(const Fvector& actorPos, float camHeading, const Fvector& tgtPos,
    float& outX) const
{
    float spotX;
    if (!ProjectToStrip(tgtPos, actorPos, camHeading, spotX, true))
    {
        outX = 0.0f;
        return;
    }
    float stripCenter = _stripWidth * 0.5f;
    spotX = stripCenter + spotX;
    spotX = clampr(spotX, _runtimeCfg.activePadding, _stripWidth - _runtimeCfg.activePadding);
    outX = spotX;
}

void CUICompassBar::UpdateActiveTargetMarker(CMapLocation* activeLoc)
{
    if (!_activeMarker)
    {
        return;
    }

    const shared_str& locTex = activeLoc->GetCompassTexture();
    const shared_str texName = (locTex.size() > 0) ? locTex : _activeMarkerFallbackTexture;

    if (_activeMarkerLastTexture != texName)
    {
        CUITextureMaster::InitTexture(texName, &_activeMarker->GetUIStaticItem());
        _activeMarkerLastTexture = texName;
    }

    const u32 locColor = activeLoc->GetCompassColor();
    _activeMarker->SetTextureColor(locColor != 0 ? locColor : _kDefaultColorWhite);
    _activeMarker->Show(true);
}

void CUICompassBar::UpdateActiveTargetText(const Fvector& actorPos, const Fvector& tgtPos)
{
    if (!_activeDistText)
    {
        return;
    }
    const float dist = actorPos.distance_to(tgtPos);
    const float distRounded = float(iFloor(dist + 0.5f));
    if (std::abs(distRounded - _dirty.lastDistanceMeters) < 0.1f)
    {
        _activeDistText->Show(true);
        return;
    }
    _dirty.lastDistanceMeters = distRounded;
    string64 buf;
    xr_sprintf(buf, sizeof(buf), _runtimeCfg.distanceFormat.c_str(), dist);
    _activeDistText->SetText(buf);
    _activeDistText->Show(true);
}

void CUICompassBar::UpdateActiveTarget(const Fvector& actorPos, float camHeading, const shared_str& levelName)
{
    if (!_strip)
    {
        return;
    }
    if (_activeTargetContainer)
    {
        _activeTargetContainer->Show(false);
    }
    if (!_isGameTypeSingleCompatible || (!_activeMarker && !_activeDistText))
    {
        return;
    }
    if (_activeDistText)
    {
        _activeDistText->Show(false);
    }
    if (_activeMarker)
    {
        _activeMarker->Show(false);
    }
    if (_activeAltitudeArrow)
    {
        _activeAltitudeArrow->Show(false);
    }
    CMapLocation* activeLoc = _activeTargetLoc;
    if (!activeLoc)
    {
        _lastActiveLoc = nullptr;
        return;
    }
    if (!activeLoc->Update())
    {
        return;
    }
    if (activeLoc->GetLevelName() != levelName)
    {
        _lastActiveLoc = nullptr;
        return;
    }
    const Fvector tgtPos = activeLoc->GetLastPosition();
    float spotX;
    CalculateActiveTargetPosition(actorPos, camHeading, tgtPos, spotX);
    if (spotX <= 0.0f && tgtPos.distance_to(actorPos) > _kMinDistanceSq)
    {
        return;
    }
    if (_lastActiveLoc != activeLoc)
    {
        _activeTargetCurX = spotX;
        _lastActiveLoc = activeLoc;
    }
    else
    {
        _activeTargetCurX += (spotX - _activeTargetCurX) * (Device.fTimeDelta * _runtimeCfg.smoothingSpeed);
    }
    spotX = _activeTargetCurX;
    SCompassStripGeometry geom = GetStripGeometry();
    if (_activeTargetContainer)
    {
        const float cw = _activeTargetContainer->GetWidth();
        const float ch = _activeTargetContainer->GetHeight();
        const float containerLeft = geom.left + spotX - cw * 0.5f;
        const float containerTop = geom.top + geom.CenterY() + _runtimeCfg.activeOffsetY - ch * 0.5f;
        _activeTargetContainer->SetWndPos(Fvector2().set(containerLeft, containerTop));
        _activeTargetContainer->Show(true);
    }
    if (_activeMarker)
    {
        UpdateActiveTargetMarker(activeLoc);
    }
    if (_activeDistText)
    {
        UpdateActiveTargetText(actorPos, tgtPos);
    }
    if (_activeAltitudeArrow)
    {
        UpdateActiveAltitudeArrow(actorPos, tgtPos);
    }
}

void CUICompassBar::UpdateActiveAltitudeArrow(const Fvector& actorPos, const Fvector& tgtPos)
{
    if (!_activeAltitudeArrow || !_altitudeArrowTextureUp.size() || !_altitudeArrowTextureDown.size())
    {
        return;
    }
    const float deltaY = tgtPos.y - actorPos.y;
    const float dz = _runtimeCfg.altitudeDeadzone;
    if (deltaY < -dz)
    {
        if (_altitudeArrowLastTexture != _altitudeArrowTextureUp)
        {
            CUITextureMaster::InitTexture(_altitudeArrowTextureUp, &_activeAltitudeArrow->GetUIStaticItem());
            _altitudeArrowLastTexture = _altitudeArrowTextureUp;
        }
        _activeAltitudeArrow->Show(true);
    }
    else if (deltaY > dz)
    {
        if (_altitudeArrowLastTexture != _altitudeArrowTextureDown)
        {
            CUITextureMaster::InitTexture(_altitudeArrowTextureDown, &_activeAltitudeArrow->GetUIStaticItem());
            _altitudeArrowLastTexture = _altitudeArrowTextureDown;
        }
        _activeAltitudeArrow->Show(true);
    }
    else
    {
        _activeAltitudeArrow->Show(false);
    }
}

// --- Draw / Update ---

void CUICompassBar::SetHudVisible(bool status)
{
    visible = status;
    inherited::Show(status);
}

void CUICompassBar::Show(bool status)
{
    SetHudVisible(status);
}

void CUICompassBar::Draw()
{
    if (visible)
    {
        CUIWindow::Draw();
    }
}

void CUICompassBar::Update()
{
    if (_dirty.lastLogicFrame == Device.dwFrame)
    {
        return;
    }

    if (!visible || !g_pGameLevel)
    {
        return;
    }

    _dirty.lastLogicFrame = Device.dwFrame;

    SCompassFrameContext ctx;
    if (!BuildFrameContext(ctx))
    {
        CUIWindow::Update();
        return;
    }

    UpdateStrip(ctx.heading);
    UpdateCardinals(ctx);

    _collectSpotsTimer -= Device.fTimeDelta;
    if (_collectSpotsTimer <= 0.0f)
    {
        _collectSpotsTimer = _spotCfg.collectInterval;
        CollectSpotCandidates(ctx.actorPos, ctx.levelName);
    }

    UpdateSpotsLayout(ctx.heading, ctx);
    UpdateActiveTarget(ctx.actorPos, ctx.heading, ctx.levelName);
    CUIWindow::Update();
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
    if (_activeTargetLoc != loc)
    {
        _dirty.membershipChanged = true;
        MarkSpotsDirty();
    }
    _activeTargetLoc = loc;
}

void CUICompassBar::Reset()
{
    _activeTargetLoc = nullptr;
    _lastActiveLoc = nullptr;
    _activeTargetCurX = 0.0f;
    _dirty.lastDistanceMeters = -1.0f;
    _dirty.lastStripU = -1.0e9f;
    _dirty.lastLogicFrame = u32(-1);
    _spotSlotByLoc.clear();
    for (u32 i = 0; i < _poolSpotOwners.size(); ++i)
    {
        _poolSpotOwners[i] = nullptr;
    }
    _dirty.membershipChanged = true;
    _dirty.layoutRefresh = true;
    MarkSpotsDirty();
}

void CUICompassBar::CacheGameTypeCompatibility()
{
    _isGameTypeSingleCompatible = IsGameTypeSingleCompatible();
}
