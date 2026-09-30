#include "StdAfx.h"
#include "UICompassBar.h"
#include "UICompassProjection.h"
#include "UICompassLabelGenerator.h"
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

#include "UICompassBar_Xml.inl"

void CUICompassClipWindow::Draw()
{
    Frect clipRect;
    GetAbsoluteRect(clipRect);
    UI().PushScissor(clipRect);
    inherited::Draw();
    UI().PopScissor();
}

CUICompassBar::CUICompassBar()
    :       _background(nullptr),
      _layerBg(nullptr),
      _strip(nullptr),
      _stripLoop(nullptr),
      _stripContainer(nullptr),
      _layerFg(nullptr),
      _activeTargetContainer(nullptr),
      _activeAltitudeArrow(nullptr),
      _activeMarker(nullptr),
      _activeDistText(nullptr),
      _activeTargetLoc(nullptr),
      _lastActiveLoc(nullptr),
      _activeTargetCurX(0.0f),
      _activeDistTextFollowMarkerColor(true),
      _stripWidth(0.0f),
      _stripTexWidth(_kDefaultStripTexWidth),
      _stripTexLoop(true),
      _stripHeadingBiasRad(0.0f),
      _stripTextureScaleX(1.0f),
      _stripTextureScaleY(1.0f),
      _stripTextureStretch(true),
      _stripRelPos(Fvector2().set(0.0f, 0.0f)),
      _stripRelSize(Fvector2().set(1.0f, 1.0f)),
      _collectSpotsTimer(0.0f),
      _isInitialized(false),
      _isGameTypeSingleCompatible(false),
      _fadeStorageSpotCount(0),
      _stripGeometryCached(false),
      _hasStyleDefaultShadow(false),
      _useLabelGenerator(false)
{
    _runtimeCfg.fovRad = deg2rad(_kDefaultFovDeg);
    _runtimeCfg.activePadding = MakeCompassLayoutScalar(_kDefaultActivePadding, true);
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

    const char* bgPath = "compass_bar:background";
    _layoutUnits.bgRelPos.x = uiXml.ReadAttribFlt(bgPath, 0, "x", 0.0f);
    _layoutUnits.bgRelPos.y = uiXml.ReadAttribFlt(bgPath, 0, "y", 0.0f);
    _layoutUnits.bgRelSize.x = uiXml.ReadAttribFlt(bgPath, 0, "width", 1.0f);
    _layoutUnits.bgRelSize.y = uiXml.ReadAttribFlt(bgPath, 0, "height", 1.0f);

    const char* fit = uiXml.ReadAttrib(bgPath, 0, "fit", nullptr);
    if (fit && (!_stricmp(fit, "parent") || !_stricmp(fit, "bar")))
    {
        _layoutUnits.bgRelPos.set(0.0f, 0.0f);
        _layoutUnits.bgRelSize.set(1.0f, 1.0f);
    }

    _background = UIHelper::CreateStatic(uiXml, bgPath, this);
    if (!_background)
    {
        _background = new CUIStatic();
        _background->SetAutoDelete(true);
        AttachChild(_background);
    }

    string_path bgTexPath;
    xr_strconcat(bgTexPath, bgPath, ":draw:texture");
    if (!uiXml.NavigateToNode(bgTexPath, 0))
    {
        xr_strconcat(bgTexPath, bgPath, ":texture");
    }
    shared_str bgTex = ReadTextureName(uiXml, bgTexPath, 0, nullptr);
    if (!bgTex.size())
    {
        bgTex = ReadTextureName(uiXml, bgPath, 0, nullptr);
    }
    if (bgTex.size())
    {
        _background->InitTexture(bgTex.c_str(), false);
    }
    if (uiXml.NavigateToNode(bgTexPath, 0))
    {
        _background->SetTextureColor(CUIXmlInit::GetColor(uiXml, bgTexPath, 0, 0xFFFFFFFF));
        _background->SetStretchTexture(uiXml.ReadAttribInt(bgTexPath, 0, "stretch", 1) != 0);
    }

    const char* alignStr = uiXml.ReadAttrib(bgPath, 0, "alignment", uiXml.ReadAttrib(bgPath, 0, "align", "l"));
    _layoutUnits.bgCentered = (alignStr && (alignStr[0] == 'c' || alignStr[0] == 'C'));
    if (_layoutUnits.bgCentered)
    {
        _background->SetAlignment(waCenter);
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

    SStyleSheet styleSheet;
    _hasStyleDefaultShadow = ReadStyleSheet(uiXml, styleSheet);
    if (_hasStyleDefaultShadow)
    {
        _styleDefaultShadow = styleSheet.shadow;
    }

    ParseSpots(uiXml, "compass_bar:spots");

    if (uiXml.NavigateToNode("compass_bar:active_target", 0))
    {
        const char* targetPath = "compass_bar:active_target";
        _runtimeCfg.activePadding = ReadActivePadding(uiXml, targetPath, _kDefaultActivePadding);
        _runtimeCfg.smoothingSpeed = ReadActiveSmoothing(uiXml, targetPath, _kDefaultSmoothingSpeed);
        _runtimeCfg.activeOffsetY = ReadActiveOffsetY(uiXml, targetPath);
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
    _spotCfg.offsetX = ReadLayoutAttrib(uiXml, path, 0, "x", nullptr, 0.0f);
    _spotCfg.offsetY = ReadLayoutAttrib(uiXml, path, 0, "y", nullptr, 0.0f);
    _spotCfg.align = ParseAlign(uiXml.ReadAttrib(path, 0, "align", "c"));
    _spotCfg.collectInterval = std::max(uiXml.ReadAttribFlt(path, 0, "collect_interval", _kDefaultCollectInterval), 0.01f);

    string_path defaultsPath;
    xr_strconcat(defaultsPath, path, ":defaults");
    string_path tmplPath;
    xr_strconcat(tmplPath, path, ":spot_template");

    const char* sizePath = nullptr;
    if (uiXml.NavigateToNode(defaultsPath, 0))
    {
        sizePath = defaultsPath;
    }
    else if (uiXml.NavigateToNode(tmplPath, 0))
    {
        sizePath = tmplPath;
    }

    if (sizePath)
    {
        SCompassLayoutScalar spotW;
        SCompassLayoutScalar spotH;
        if (!ReadSizePair(uiXml, sizePath, 0, spotW, spotH))
        {
            spotW = ReadLayoutAttrib(uiXml, sizePath, 0, "width", nullptr, 0.0f);
            spotH = ReadLayoutAttrib(uiXml, sizePath, 0, "height", nullptr, 0.0f);
        }
        _spotCfg.spotWidth = spotW;
        _spotCfg.spotHeight = spotH;
        CUIXmlInit::ReadShadowsNode(uiXml, sizePath, 0, _spotCfg.defaultShadow);
        if (!_spotCfg.defaultShadow.enabled && _hasStyleDefaultShadow)
        {
            _spotCfg.defaultShadow = _styleDefaultShadow;
        }
    }
    else if (_hasStyleDefaultShadow)
    {
        _spotCfg.defaultShadow = _styleDefaultShadow;
    }

    const CUIXmlInit::ColorDefs* colorDefs = CUIXmlInit::GetColorDefs();
    const char* defaultColorName = uiXml.ReadAttrib(path, 0, "color", "ui_1");
    CUIXmlInit::ColorDefs::const_iterator colorIt = colorDefs->find(defaultColorName);
    _spotCfg.defaultSpotColor = (colorIt != colorDefs->end()) ? colorIt->second : _kDefaultColorWhite;
}

void CUICompassBar::InitCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit, CUIWindow* stripParent)
{
    const char* stripPath = ResolveStripPath(uiXml);

    string_path cardinalsPathBuf;
    const char* cardinalsPath = ResolveCardinalsPath(uiXml, stripPath, cardinalsPathBuf);

    if (!stripParent || !uiXml.NavigateToNode(stripPath, 0))
    {
        return;
    }

    _stripTexWidth = ReadCircumferencePx(uiXml, stripPath, _kDefaultStripTexWidth);
    _stripTexLoop = ReadStripLoop(uiXml, stripPath, true);
    _stripHeadingBiasRad = ReadHeadingBiasRad(uiXml, stripPath);
    _runtimeCfg.cardinalFakeDistance = uiXml.ReadAttribFlt(cardinalsPath, 0, "fake_target_distance",
        uiXml.ReadAttribFlt(cardinalsPath, 0, "fake_distance", _kDefaultFakeTargetDistance));

    _stripRelPos.x = uiXml.ReadAttribFlt(stripPath, 0, "x", 0.0f);
    _stripRelPos.y = uiXml.ReadAttribFlt(stripPath, 0, "y", 0.0f);
    _stripRelSize.x = uiXml.ReadAttribFlt(stripPath, 0, "width", 1.0f);
    _stripRelSize.y = uiXml.ReadAttribFlt(stripPath, 0, "height", 1.0f);

    _stripContainer = new CUICompassClipWindow();
    _stripContainer->SetAutoDelete(true);
    xmlInit.InitWindow(uiXml, stripPath, 0, _stripContainer);
    stripParent->AttachChild(_stripContainer);

    string_path drawPathBuf;
    const char* drawPath = ResolveDialDrawPath(uiXml, stripPath, drawPathBuf);
    shared_str texName = ReadTextureName(uiXml, drawPath, 0, nullptr);
    if (!texName.size())
    {
        texName = ReadTextureName(uiXml, stripPath, 0, nullptr);
    }

    ReadStripTextureDrawUnits(
        uiXml,
        drawPath,
        _stripTextureScaleX,
        _stripTextureScaleY,
        _stripTextureOffsetX,
        _stripTextureOffsetY,
        _stripTextureStretch);

    _strip = new CUIStatic();
    _strip->SetAutoDelete(true);
    _strip->SetWndPos(Fvector2().set(0.0f, 0.0f));
    _strip->SetWndSize(Fvector2().set(1.0f, 1.0f));
    bool texOk = false;
    shared_str dialTexName;
    if (texName.size())
    {
        texOk = _strip->InitTexture(texName.c_str(), false);
        if (texOk)
        {
            dialTexName = texName;
        }
    }
    if (!texOk)
    {
        texOk = _strip->InitTexture("ui_inGame2_compass_dial", false);
        if (texOk)
        {
            dialTexName = "ui_inGame2_compass_dial";
            if (texName.size())
            {
                WarnLegacyOnce(
                    "dial_texture_fallback",
                    "CUICompassBar: dial texture fallback to ui_inGame2_compass_dial");
            }
        }
    }
    _strip->SetStretchTexture(_stripTextureStretch);
    if (uiXml.NavigateToNode(drawPath, 0))
    {
        _strip->SetTextureColor(CUIXmlInit::GetColor(uiXml, drawPath, 0, 0xFFFFFFFF));
    }
    else
    {
        _strip->SetTextureColor(CUIXmlInit::GetColor(uiXml, stripPath, 0, 0xFFFFFFFF));
    }

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

    if (_stripTexLoop && texOk && dialTexName.size())
    {
        _stripLoop = new CUIStatic();
        _stripLoop->SetAutoDelete(true);
        _stripLoop->SetWndPos(Fvector2().set(0.0f, 0.0f));
        _stripLoop->SetWndSize(Fvector2().set(1.0f, 1.0f));
        _stripLoop->InitTexture(dialTexName.c_str(), false);
        _stripLoop->SetStretchTexture(_stripTextureStretch);
        _stripLoop->SetTextureColor(_strip->GetTextureColor());
        _stripLoop->SetTextureRect(_stripBaseTexRect);
        _stripLoop->Show(false);
        _stripContainer->AttachChild(_stripLoop);
    }

    const float defY = uiXml.ReadAttribFlt(cardinalsPath, 0, "y", 0.0f);
    const float defW = uiXml.ReadAttribFlt(cardinalsPath, 0, "width", 16.0f);
    const float defH = uiXml.ReadAttribFlt(cardinalsPath, 0, "height", 14.0f);

    _cardinalEntries.reserve(_kMaxLabelPoints);

    SCompassCardinalMarkerConfig defaultMarkerCfg;
    string_path defaultMarkerPath;
    xr_strconcat(defaultMarkerPath, cardinalsPath, ":marker");
    if (!uiXml.NavigateToNode(defaultMarkerPath, 0))
    {
        xr_strconcat(defaultMarkerPath, cardinalsPath, ":tick");
    }
    ParseCardinalMarkerConfig(uiXml, defaultMarkerPath, defaultMarkerCfg);

    _useLabelGenerator = ReadLabelSettings(uiXml, cardinalsPath, _labelSettings);
    if (_useLabelGenerator || _labelSettings.showDegrees)
    {
        _useLabelGenerator = true;
        InitGeneratedLabels(uiXml, xmlInit, cardinalsPath, defY, defW, defH, defaultMarkerCfg);
        return;
    }

    XML_NODE* cardinalsNode = uiXml.NavigateToNode(cardinalsPath, 0);
    const int pointCount = cardinalsNode ? uiXml.GetNodesNum(cardinalsNode, "point") : 0;
    if (pointCount > 0)
    {
        XML_NODE* prevLocalRoot = uiXml.GetLocalRoot();
        for (int i = 0; i < pointCount; ++i)
        {
            XML_NODE* pointNode = uiXml.NavigateToNode(cardinalsNode, "point", i);
            if (!pointNode)
            {
                continue;
            }
            uiXml.SetLocalRoot(pointNode);
            if (InitCardinalPointEntry(uiXml, xmlInit, cardinalsPath, i, defY, defW, defH, defaultMarkerCfg) &&
                _stripContainer && !_cardinalEntries.empty())
            {
                _stripContainer->AttachChild(_cardinalEntries.back().host);
            }
        }
        uiXml.SetLocalRoot(prevLocalRoot);
        return;
    }

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

void CUICompassBar::ParseCardinalMarkerConfig(CUIXml& uiXml, LPCSTR path, SCompassCardinalMarkerConfig& cfg, int index) const
{
    if (!uiXml.NavigateToNode(path, index))
    {
        return;
    }

    ReadMarkerSize(uiXml, path, index, cfg.width, cfg.height);
    cfg.offsetY = ReadMarkerOffsetY(uiXml, path, index, cfg.offsetY);
    cfg.stretch = uiXml.ReadAttribInt(path, index, "stretch", cfg.stretch ? 1 : 0) != 0;

    shared_str texName = ReadTextureName(uiXml, path, index, nullptr);
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
    case CGameFont::alJustify:
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

    float markerW = ResolveLayoutValue(cfg.width, hostW);
    float markerH = ResolveLayoutValue(cfg.height, hostH);

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

    const float textCenterX = GetCardinalTextCenterX(entry);
    const float offsetY = ResolveLayoutValue(cfg.offsetY, hostH);
    entry.marker->SetWndSize(Fvector2().set(markerW, markerH));
    entry.marker->SetWndPos(Fvector2().set(
        textCenterX - markerW * 0.5f,
        GetCardinalTextBottom(entry) + offsetY));
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
            else if (alignStr[0] == 'j' || alignStr[0] == 'J')
            {
                text->TextItemControl()->SetTextAlignment(CGameFont::alJustify);
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
    float cardinalAngle = 0.0f;
    if (TryGetCardinalAngleRad(directionNode, cardinalAngle))
    {
        entry.dirXZ.set(cosf(cardinalAngle), sinf(cardinalAngle));
        entry.angleRad = cardinalAngle;
        entry.hasAngle = true;
    }
    entry.kind = ECompassLabelKind::Cardinal;
    if (directionNode && (xr_strlen(directionNode) > 1))
    {
        entry.kind = ECompassLabelKind::Intermediate;
    }
    _cardinalEntries.push_back(entry);
    ApplyCardinalMarkerLayout(_cardinalEntries.back());

    return true;
}

bool CUICompassBar::InitCardinalPointEntry(CUIXml& uiXml, CUIXmlInit& xmlInit, LPCSTR cardinalsPath, int pointIndex,
    float defaultY, float defaultW, float defaultH, const SCompassCardinalMarkerConfig& defaultMarkerCfg)
{
    (void)pointIndex;

    XML_NODE* pointNode = uiXml.GetLocalRoot();
    if (!pointNode)
    {
        return false;
    }

    float cardinalAngle = 0.0f;
    if (uiXml.ReadAttrib(pointNode, "angle_deg", nullptr))
    {
        cardinalAngle = deg2rad(uiXml.ReadAttribFlt(pointNode, "angle_deg", 0.0f));
    }
    else if (uiXml.ReadAttrib(pointNode, "angle", nullptr))
    {
        cardinalAngle = deg2rad(uiXml.ReadAttribFlt(pointNode, "angle", 0.0f));
    }
    else
    {
        const char* id = uiXml.ReadAttrib(pointNode, "id", nullptr);
        if (!TryGetCardinalAngleRad(id, cardinalAngle))
        {
            return false;
        }
    }

    string_path defaultTextPath;
    xr_strconcat(defaultTextPath, cardinalsPath, ":text");

    const float y = uiXml.ReadAttribFlt(pointNode, "y", defaultY);
    const float w = uiXml.ReadAttribFlt(pointNode, "width", defaultW);
    const float h = uiXml.ReadAttribFlt(pointNode, "height", defaultH);

    CUIWindow* host = new CUIWindow();
    host->SetAutoDelete(true);
    host->SetWndPos(Fvector2().set(0.0f, y));
    host->SetWndSize(Fvector2().set(w, h));

    CUIStatic* text = new CUIStatic();
    text->SetAutoDelete(true);
    text->SetWndPos(Fvector2().set(0.0f, 0.0f));
    text->SetWndSize(Fvector2().set(w, h));

    uiXml.SetLocalRoot(nullptr);
    if (uiXml.NavigateToNode(defaultTextPath, 0))
    {
        xmlInit.InitText(uiXml, defaultTextPath, 0, text);
    }
    uiXml.SetLocalRoot(pointNode);
    if (uiXml.NavigateToNode("text", 0))
    {
        xmlInit.InitText(uiXml, "text", 0, text);
    }

    const char* caption = uiXml.ReadAttrib(pointNode, "text", nullptr);
    if (!caption || !*caption)
    {
        caption = uiXml.Read(pointNode, nullptr);
    }
    if (caption && *caption)
    {
        text->SetText(caption);
    }

    const char* colorAttr = uiXml.ReadAttrib(pointNode, "color", nullptr);
    const char* rAttr = uiXml.ReadAttrib(pointNode, "r", nullptr);
    if (colorAttr || rAttr)
    {
        if (colorAttr)
        {
            CUIXmlInit::ColorDefs::const_iterator colorIt = CUIXmlInit::GetColorDefs()->find(colorAttr);
            text->SetTextColor(
                (colorIt != CUIXmlInit::GetColorDefs()->end()) ? colorIt->second : _kDefaultColorWhite);
        }
        else
        {
            const int r = uiXml.ReadAttribInt(pointNode, "r", color_get_R(_kDefaultColorWhite));
            const int g = uiXml.ReadAttribInt(pointNode, "g", color_get_G(_kDefaultColorWhite));
            const int b = uiXml.ReadAttribInt(pointNode, "b", color_get_B(_kDefaultColorWhite));
            const int a = uiXml.ReadAttribInt(pointNode, "a", 0xff);
            text->SetTextColor(color_argb(a, r, g, b));
        }
    }
    const char* alignStr = uiXml.ReadAttrib(pointNode, "align", nullptr);
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
        else if (alignStr[0] == 'j' || alignStr[0] == 'J')
        {
            text->TextItemControl()->SetTextAlignment(CGameFont::alJustify);
        }
        else
        {
            text->TextItemControl()->SetTextAlignment(CGameFont::alCenter);
        }
    }

    host->AttachChild(text);

    SCompassCardinalMarkerConfig markerCfg = defaultMarkerCfg;
    if (uiXml.NavigateToNode("marker", 0))
    {
        ParseCardinalMarkerConfig(uiXml, "marker", markerCfg, 0);
    }
    shared_str markerTextureOverride = uiXml.ReadAttrib(pointNode, "marker_texture", nullptr);
    if (markerTextureOverride.size())
    {
        markerCfg.texture = SanitizeTextureName(markerTextureOverride.c_str());
    }

    CUIStatic* marker = nullptr;
    if (markerCfg.texture.size())
    {
        const bool hasPointMarker = uiXml.NavigateToNode("marker", 0) != nullptr;
        string_path colorPath;
        if (hasPointMarker)
        {
            xr_strcpy(colorPath, "marker");
            marker = CreateCardinalMarker(uiXml, markerCfg, colorPath);
        }
        else
        {
            uiXml.SetLocalRoot(nullptr);
            xr_strconcat(colorPath, cardinalsPath, ":tick");
            if (!uiXml.NavigateToNode(colorPath, 0))
            {
                xr_strconcat(colorPath, cardinalsPath, ":marker");
            }
            marker = CreateCardinalMarker(uiXml, markerCfg, colorPath);
            uiXml.SetLocalRoot(pointNode);
        }
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
    entry.dirXZ.set(cosf(cardinalAngle), sinf(cardinalAngle));
    entry.angleRad = cardinalAngle;
    entry.hasAngle = true;
    entry.kind = ECompassLabelKind::Cardinal;
    {
        const char* pointId = uiXml.ReadAttrib(pointNode, "id", nullptr);
        if (pointId && xr_strlen(pointId) > 1)
        {
            entry.kind = ECompassLabelKind::Intermediate;
        }
    }
    _cardinalEntries.push_back(entry);
    ApplyCardinalMarkerLayout(_cardinalEntries.back());

    return true;
}

void CUICompassBar::InitGeneratedLabels(CUIXml& uiXml, CUIXmlInit& xmlInit, LPCSTR cardinalsPath,
    float defaultY, float defaultW, float defaultH, const SCompassCardinalMarkerConfig& defaultMarkerCfg)
{
    float mainY = defaultY;
    float intermediateY = defaultY;
    float degreeY = defaultY + _kDefaultDegreeLayerOffsetY;

    string_path groupPath;
    xr_strconcat(groupPath, cardinalsPath, ":main");
    if (uiXml.NavigateToNode(groupPath, 0))
    {
        mainY = uiXml.ReadAttribFlt(groupPath, 0, "y", mainY);
    }
    xr_strconcat(groupPath, cardinalsPath, ":intermediate");
    if (uiXml.NavigateToNode(groupPath, 0))
    {
        intermediateY = uiXml.ReadAttribFlt(groupPath, 0, "y", intermediateY);
    }
    xr_strconcat(groupPath, cardinalsPath, ":degrees");
    if (uiXml.NavigateToNode(groupPath, 0))
    {
        degreeY = uiXml.ReadAttribFlt(groupPath, 0, "y", degreeY);
    }

    xr_vector<SCompassLabelDesc> marks;
    CompassLabelGenerator::Generate(_labelSettings, marks);
    _cardinalEntries.reserve(marks.size());

    for (const SCompassLabelDesc& desc : marks)
    {
        if (_cardinalEntries.size() >= _kMaxLabelPoints)
        {
            break;
        }

        float layerY = mainY;
        if (desc.kind == ECompassLabelKind::Intermediate)
        {
            layerY = intermediateY;
        }
        else if (desc.kind == ECompassLabelKind::Degree)
        {
            layerY = degreeY;
        }

        if (InitLabelFromDesc(uiXml, xmlInit, cardinalsPath, desc, layerY, defaultW, defaultH, degreeY, defaultMarkerCfg) &&
            _stripContainer && !_cardinalEntries.empty())
        {
            _stripContainer->AttachChild(_cardinalEntries.back().host);
        }
    }
}

bool CUICompassBar::InitLabelFromDesc(CUIXml& uiXml, CUIXmlInit& xmlInit, LPCSTR cardinalsPath, const SCompassLabelDesc& desc,
    float defaultY, float defaultW, float defaultH, float degreeY,
    const SCompassCardinalMarkerConfig& defaultMarkerCfg)
{
    (void)degreeY;

    const bool isDegree = (desc.kind == ECompassLabelKind::Degree);
    const bool isIntermediate = (desc.kind == ECompassLabelKind::Intermediate);
    const float y = defaultY;
    const float w = defaultW;
    const float h = defaultH;

    CUIWindow* host = new CUIWindow();
    host->SetAutoDelete(true);
    host->SetWndPos(Fvector2().set(0.0f, y));
    host->SetWndSize(Fvector2().set(w, h));

    CUIStatic* text = new CUIStatic();
    text->SetAutoDelete(true);
    text->SetWndPos(Fvector2().set(0.0f, 0.0f));
    text->SetWndSize(Fvector2().set(w, h));

    string_path fallbackTextPath;
    string_path groupTextPath;
    xr_strconcat(fallbackTextPath, cardinalsPath, ":text");
    if (isDegree)
    {
        xr_strconcat(groupTextPath, cardinalsPath, ":degrees:text");
    }
    else if (isIntermediate)
    {
        xr_strconcat(groupTextPath, cardinalsPath, ":intermediate:text");
    }
    else
    {
        xr_strconcat(groupTextPath, cardinalsPath, ":main:text");
    }

    XML_NODE* prevLocalRoot = uiXml.GetLocalRoot();
    uiXml.SetLocalRoot(nullptr);

    const char* textPath = fallbackTextPath;
    if (uiXml.NavigateToNode(groupTextPath, 0))
    {
        textPath = groupTextPath;
    }
    if (uiXml.NavigateToNode(textPath, 0))
    {
        xmlInit.InitText(uiXml, textPath, 0, text);
    }

    XML_NODE* pointNode = nullptr;
    if (!isDegree && desc.id.size())
    {
        const char* groupNames[] = { "main", "intermediate", nullptr };
        for (const char* groupName : groupNames)
        {
            if (!groupName)
            {
                break;
            }
            string_path pointsParentPath;
            xr_strconcat(pointsParentPath, cardinalsPath, ":", groupName);
            XML_NODE* groupNode = uiXml.NavigateToNode(pointsParentPath, 0);
            if (!groupNode)
            {
                continue;
            }
            const int pointCount = uiXml.GetNodesNum(groupNode, "point");
            for (int i = 0; i < pointCount; ++i)
            {
                XML_NODE* candidate = uiXml.NavigateToNode(groupNode, "point", i);
                if (!candidate)
                {
                    continue;
                }
                const char* pointId = uiXml.ReadAttrib(candidate, "id", nullptr);
                if (pointId && !xr_strcmp(pointId, desc.id.c_str()))
                {
                    pointNode = candidate;
                    break;
                }
            }
            if (pointNode)
            {
                break;
            }
        }

        if (!pointNode)
        {
            XML_NODE* cardinalsNode = uiXml.NavigateToNode(cardinalsPath, 0);
            if (cardinalsNode)
            {
                const int pointCount = uiXml.GetNodesNum(cardinalsNode, "point");
                for (int i = 0; i < pointCount; ++i)
                {
                    XML_NODE* candidate = uiXml.NavigateToNode(cardinalsNode, "point", i);
                    if (!candidate)
                    {
                        continue;
                    }
                    const char* pointId = uiXml.ReadAttrib(candidate, "id", nullptr);
                    if (pointId && !xr_strcmp(pointId, desc.id.c_str()))
                    {
                        pointNode = candidate;
                        break;
                    }
                }
            }
        }
    }

    if (pointNode)
    {
        uiXml.SetLocalRoot(pointNode);
        if (uiXml.NavigateToNode("text", 0))
        {
            xmlInit.InitText(uiXml, "text", 0, text);
        }

        const char* caption = uiXml.ReadAttrib(pointNode, "text", nullptr);
        if (caption && *caption)
        {
            text->SetText(caption);
        }
        else if (desc.label.size())
        {
            text->SetText(desc.label.c_str());
        }

        const char* colorAttr = uiXml.ReadAttrib(pointNode, "color", nullptr);
        const char* rAttr = uiXml.ReadAttrib(pointNode, "r", nullptr);
        if (colorAttr || rAttr)
        {
            if (colorAttr)
            {
                CUIXmlInit::ColorDefs::const_iterator colorIt = CUIXmlInit::GetColorDefs()->find(colorAttr);
                text->SetTextColor(
                    (colorIt != CUIXmlInit::GetColorDefs()->end()) ? colorIt->second : _kDefaultColorWhite);
            }
            else
            {
                const int r = uiXml.ReadAttribInt(pointNode, "r", color_get_R(_kDefaultColorWhite));
                const int g = uiXml.ReadAttribInt(pointNode, "g", color_get_G(_kDefaultColorWhite));
                const int b = uiXml.ReadAttribInt(pointNode, "b", color_get_B(_kDefaultColorWhite));
                const int a = uiXml.ReadAttribInt(pointNode, "a", 0xff);
                text->SetTextColor(color_argb(a, r, g, b));
            }
        }

        const char* alignStr = uiXml.ReadAttrib(pointNode, "align", nullptr);
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
            else if (alignStr[0] == 'j' || alignStr[0] == 'J')
            {
                text->TextItemControl()->SetTextAlignment(CGameFont::alJustify);
            }
            else
            {
                text->TextItemControl()->SetTextAlignment(CGameFont::alCenter);
            }
        }
        else if (text->TextItemControl())
        {
            text->TextItemControl()->SetTextAlignment(CGameFont::alCenter);
        }
    }
    else
    {
        if (desc.label.size())
        {
            text->SetText(desc.label.c_str());
        }
        if (text->TextItemControl())
        {
            text->TextItemControl()->SetTextAlignment(CGameFont::alCenter);
        }
    }

    uiXml.SetLocalRoot(prevLocalRoot);
    host->AttachChild(text);

    SCompassCardinalMarkerConfig markerCfg = defaultMarkerCfg;
    CUIStatic* marker = nullptr;
    if (!isDegree && markerCfg.texture.size())
    {
        string_path colorPath;
        uiXml.SetLocalRoot(nullptr);
        xr_strconcat(colorPath, cardinalsPath, ":tick");
        if (!uiXml.NavigateToNode(colorPath, 0))
        {
            xr_strconcat(colorPath, cardinalsPath, ":marker");
        }
        marker = CreateCardinalMarker(uiXml, markerCfg, colorPath);
        uiXml.SetLocalRoot(prevLocalRoot);
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
    entry.angleRad = desc.angleRad;
    entry.hasAngle = true;
    entry.kind = desc.kind;
    entry.dirXZ.set(cosf(desc.angleRad), sinf(desc.angleRad));
    entry.baseTextColor = text->GetTextColor();
    entry.baseMarkerColor = marker ? marker->GetTextureColor() : 0;
    entry.markerCfg = markerCfg;
    entry.alpha = 1.0f;
    _cardinalEntries.push_back(entry);
    ApplyCardinalMarkerLayout(_cardinalEntries.back());
    return true;
}

void CUICompassBar::InitActiveTargetWidgets(CUIXml& uiXml, CUIXmlInit& xmlInit)
{
    const char* targetPath = "compass_bar:active_target";
    if (!ReadNodeShow(uiXml, targetPath, true))
    {
        return;
    }

    const char* markerPath = "compass_bar:active_target:marker";
    string_path markerTexPathBuf;
    const char* markerTexPath = ResolveWidgetTexturePath(uiXml, markerPath, markerTexPathBuf);
    _activeMarkerFallbackTexture = ReadTextureName(
        uiXml, markerTexPath, 0, "ui_inGame2_hint_wnd_main_window");

    ReadWidgetLayout(uiXml, targetPath, _activeTargetLayout.container, 100.0f, 24.0f);
    ReadWidgetLayout(uiXml, markerPath, _activeTargetLayout.marker, 15.0f, 18.0f);
    ReadWidgetLayout(uiXml, "compass_bar:active_target:distance_text", _activeTargetLayout.distanceText, 80.0f, 14.0f);
    ReadWidgetLayout(uiXml, "compass_bar:active_target:altitude_arrow", _activeTargetLayout.altitudeArrow, 12.0f, 12.0f);

    _activeTargetContainer = new CUIWindow();
    _activeTargetContainer->SetAutoDelete(true);
    if (uiXml.NavigateToNode(targetPath, 0))
    {
        xmlInit.InitWindow(uiXml, targetPath, 0, _activeTargetContainer);
    }
    else
    {
        _activeTargetContainer->SetWndPos(Fvector2().set(0.0f, 0.0f));
    }
    if (_layerFg)
    {
        _layerFg->AttachChild(_activeTargetContainer);
    }

    const char* arrowPath = "compass_bar:active_target:altitude_arrow";
    if (ReadNodeShow(uiXml, arrowPath, true))
    {
        string_path arrowDrawBuf;
        const char* arrowDrawPath = ResolveWidgetDrawPath(uiXml, arrowPath, arrowDrawBuf);

        _altitudeArrowTextureUp = uiXml.ReadAttrib(arrowDrawPath, 0, "texture_up",
            uiXml.ReadAttrib(arrowPath, 0, "texture_up", "ui_inGame2_compass_altitude_up"));
        _altitudeArrowTextureDown = uiXml.ReadAttrib(arrowDrawPath, 0, "texture_down",
            uiXml.ReadAttrib(arrowPath, 0, "texture_down", "ui_inGame2_compass_altitude_down"));

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
            const int stretch = uiXml.ReadAttribInt(arrowDrawPath, 0, "stretch",
                uiXml.ReadAttribInt(arrowPath, 0, "stretch", 1));
            _activeAltitudeArrow->SetStretchTexture(stretch != 0);
            CUIXmlInit::ApplyShadowsToStatic(uiXml, arrowDrawPath, 0, _activeAltitudeArrow);
            _activeTargetContainer->AttachChild(_activeAltitudeArrow);
        }
        else
        {
            xr_delete(_activeAltitudeArrow);
            _activeAltitudeArrow = nullptr;
        }
    }

    const char* distPath = "compass_bar:active_target:distance_text";
    if (ReadNodeShow(uiXml, distPath, true))
    {
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

            string_path distTextPath;
            xr_strconcat(distTextPath, distPath, ":text");
            string_path distDrawTextPath;
            xr_strconcat(distDrawTextPath, distPath, ":draw:text");
            const char* colorTextPath = uiXml.NavigateToNode(distDrawTextPath, 0) ? distDrawTextPath : distTextPath;

            const bool hasColorOnRoot =
                uiXml.ReadAttrib(distPath, 0, "color", nullptr) != nullptr ||
                uiXml.ReadAttrib(distPath, 0, "r", nullptr) != nullptr ||
                uiXml.ReadAttrib(distPath, 0, "g", nullptr) != nullptr ||
                uiXml.ReadAttrib(distPath, 0, "b", nullptr) != nullptr;
            const bool hasColorOnText =
                uiXml.NavigateToNode(colorTextPath, 0) &&
                (uiXml.ReadAttrib(colorTextPath, 0, "color", nullptr) != nullptr ||
                 uiXml.ReadAttrib(colorTextPath, 0, "r", nullptr) != nullptr ||
                 uiXml.ReadAttrib(colorTextPath, 0, "g", nullptr) != nullptr ||
                 uiXml.ReadAttrib(colorTextPath, 0, "b", nullptr) != nullptr);
            _activeDistTextFollowMarkerColor = !hasColorOnRoot && !hasColorOnText;
        }
    }

    if (ReadNodeShow(uiXml, markerPath, true))
    {
        _activeMarker = UIHelper::CreateStatic(uiXml, markerPath, _activeTargetContainer, false);
        if (_activeMarker)
        {
            _activeMarker->SetAutoDelete(false);
            if (_activeMarkerFallbackTexture.size())
            {
                CUITextureMaster::InitTexture(_activeMarkerFallbackTexture, &_activeMarker->GetUIStaticItem());
            }

            string_path markerDrawBuf;
            const char* markerDrawPath = ResolveWidgetDrawPath(uiXml, markerPath, markerDrawBuf);
            const int stretch = uiXml.ReadAttribInt(markerTexPath, 0, "stretch",
                uiXml.ReadAttribInt(markerPath, 0, "stretch", 1));
            _activeMarker->SetStretchTexture(stretch != 0);
            CUIXmlInit::ApplyShadowsToStatic(uiXml, markerDrawPath, 0, _activeMarker);
            if (uiXml.NavigateToNode(markerTexPath, 0))
            {
                _activeMarker->SetTextureColor(CUIXmlInit::GetColor(uiXml, markerTexPath, 0, 0xFFFFFFFF));
            }
        }
    }

    ApplyActiveTargetLayout();
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
        if (!_activeTargetLayout.distanceText.hasNode)
        {
            _activeTargetLayout.distanceText.hasNode = true;
            _activeTargetLayout.distanceText.width = MakeLayoutScalar(80.0f, true);
            _activeTargetLayout.distanceText.height = MakeLayoutScalar(14.0f, true);
        }
        const char* fontName = uiXml.ReadAttrib("compass_bar:active_target:distance_text", 0, "font", "ui_font_letterica18");
        CGameFont* font = UI().Font().GetFont(fontName);
        if (font)
        {
            _activeDistText->SetFont(font);
        }
        const char* colorName = uiXml.ReadAttrib("compass_bar:active_target:distance_text", 0, "color", nullptr);
        const char* rAttr = uiXml.ReadAttrib("compass_bar:active_target:distance_text", 0, "r", nullptr);
        if (colorName || rAttr)
        {
            _activeDistTextFollowMarkerColor = false;
            u32 textColor = _kDefaultColorWhite;
            if (colorName)
            {
                CUIXmlInit::ColorDefs::const_iterator colorIt = CUIXmlInit::GetColorDefs()->find(colorName);
                textColor = (colorIt != CUIXmlInit::GetColorDefs()->end()) ? colorIt->second : _kDefaultColorWhite;
            }
            else
            {
                textColor = CUIXmlInit::GetColor(uiXml, "compass_bar:active_target:distance_text", 0, _kDefaultColorWhite);
            }
            _activeDistText->SetTextColor(textColor);
        }
        else
        {
            _activeDistTextFollowMarkerColor = true;
        }
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
        if (!_activeTargetLayout.marker.hasNode)
        {
            _activeTargetLayout.marker.hasNode = true;
            _activeTargetLayout.marker.width = MakeLayoutScalar(15.0f, true);
            _activeTargetLayout.marker.height = MakeLayoutScalar(18.0f, true);
        }
        _activeMarker->SetStretchTexture(true);
        _activeMarker->InitTexture(_activeMarkerFallbackTexture.c_str(), false);
        _activeTargetContainer->AttachChild(_activeMarker);
    }
    ApplyActiveTargetLayout();
}

void CUICompassBar::ApplyActiveTargetLayout()
{
    if (!_activeTargetContainer)
    {
        return;
    }

    const float barW = GetWidth();
    const float barH = GetHeight();
    const float baseW = UI_BASE_WIDTH;
    const float baseH = UI_BASE_HEIGHT;

    if (_activeTargetLayout.container.hasNode)
    {
        const float w = ResolveLayoutValue(_activeTargetLayout.container.width, barW);
        const float h = ResolveLayoutValue(_activeTargetLayout.container.height, barH);
        _activeTargetContainer->SetWndSize(Fvector2().set((w > 1.0f) ? w : 1.0f, (h > 1.0f) ? h : 1.0f));
    }
    else if (_activeTargetContainer->GetWidth() <= 0.0f || _activeTargetContainer->GetHeight() <= 0.0f)
    {
        _activeTargetContainer->SetWndSize(Fvector2().set(100.0f, 24.0f));
    }

    if (_activeMarker && _activeTargetLayout.marker.hasNode)
    {
        _activeMarker->SetWndPos(Fvector2().set(
            ResolveLayoutValue(_activeTargetLayout.marker.x, baseW),
            ResolveLayoutValue(_activeTargetLayout.marker.y, baseH)));
        const float markerW = ResolveLayoutValue(_activeTargetLayout.marker.width, baseW);
        const float markerH = ResolveLayoutValue(_activeTargetLayout.marker.height, baseH);
        if (markerW > 0.0f && markerH > 0.0f)
        {
            _activeMarker->SetWndSize(Fvector2().set(markerW, markerH));
        }
    }
    if (_activeDistText && _activeTargetLayout.distanceText.hasNode)
    {
        _activeDistText->SetWndPos(Fvector2().set(
            ResolveLayoutValue(_activeTargetLayout.distanceText.x, baseW),
            ResolveLayoutValue(_activeTargetLayout.distanceText.y, baseH)));
        const float textW = ResolveLayoutValue(_activeTargetLayout.distanceText.width, baseW);
        const float textH = ResolveLayoutValue(_activeTargetLayout.distanceText.height, baseH);
        if (textW > 0.0f && textH > 0.0f)
        {
            _activeDistText->SetWndSize(Fvector2().set(textW, textH));
        }
    }
    if (_activeAltitudeArrow && _activeTargetLayout.altitudeArrow.hasNode)
    {
        _activeAltitudeArrow->SetWndPos(Fvector2().set(
            ResolveLayoutValue(_activeTargetLayout.altitudeArrow.x, baseW),
            ResolveLayoutValue(_activeTargetLayout.altitudeArrow.y, baseH)));
        const float arrowW = ResolveLayoutValue(_activeTargetLayout.altitudeArrow.width, baseW);
        const float arrowH = ResolveLayoutValue(_activeTargetLayout.altitudeArrow.height, baseH);
        if (arrowW > 0.0f && arrowH > 0.0f)
        {
            _activeAltitudeArrow->SetWndSize(Fvector2().set(arrowW, arrowH));
        }
    }
}


void CUICompassBar::ApplyRelativeLayout()
{
    ApplyMainWindowLayout();
    ApplyLayerLayouts();
    ApplyStripLayout();
    ApplyCardinalsLayout();
    ApplyActiveTargetLayout();
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
    ApplyBackgroundLayout();
}

void CUICompassBar::ApplyBackgroundLayout()
{
    if (!_background)
    {
        return;
    }

    const float parentW = GetWidth();
    const float parentH = GetHeight();
    const float bgW = _layoutUnits.bgRelSize.x * parentW;
    const float bgH = _layoutUnits.bgRelSize.y * parentH;
    _background->SetWndSize(Fvector2().set(bgW, bgH));

    float posX = _layoutUnits.bgRelPos.x * parentW;
    float posY = _layoutUnits.bgRelPos.y * parentH;
    if (_layoutUnits.bgCentered)
    {
        _background->SetAlignment(waCenter);
        posX = _layoutUnits.bgRelPos.x * parentW;
        posY = _layoutUnits.bgRelPos.y * parentH;
    }
    _background->SetWndPos(Fvector2().set(posX, posY));
}

void CUICompassBar::GetDialDrawGeom(float& outX, float& outY, float& outW, float& outH) const
{
    outX = 0.0f;
    outY = 0.0f;
    outW = 0.0f;
    outH = 0.0f;
    if (!_stripContainer)
    {
        return;
    }

    const float cw = _stripContainer->GetWidth();
    const float ch = _stripContainer->GetHeight();
    if (_stripTextureStretch)
    {
        outW = cw * _stripTextureScaleX;
        outH = ch * _stripTextureScaleY;
    }
    else
    {
        outW = _stripNativeTexSize.x * _stripTextureScaleX;
        outH = _stripNativeTexSize.y * _stripTextureScaleY;
    }

    outX = (cw - outW) * 0.5f + ResolveLayoutValue(_stripTextureOffsetX, cw);
    outY = (ch - outH) * 0.5f + ResolveLayoutValue(_stripTextureOffsetY, ch);
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

    float drawX = 0.0f;
    float drawY = 0.0f;
    float drawW = 0.0f;
    float drawH = 0.0f;
    GetDialDrawGeom(drawX, drawY, drawW, drawH);

    if (_strip)
    {
        _strip->SetWndSize(Fvector2().set(drawW, drawH));
        _strip->SetWndPos(Fvector2().set(drawX, drawY));
    }
    if (_stripLoop)
    {
        _stripLoop->SetWndSize(Fvector2().set(drawW, drawH));
        _stripLoop->SetWndPos(Fvector2().set(drawX, drawY));
        _stripLoop->Show(false);
    }
    _stripWidth = _stripContainer->GetWidth();
    _dirty.lastStripU = -1.0e9f;
}

void CUICompassBar::UpdateStrip(float heading)
{
    if (!_strip || !_stripContainer)
    {
        return;
    }

    const float atlasCircumference = _stripNativeTexSize.x;
    const float kx = UI().get_current_kx();
    const float viewW = _stripWidth > 0.0f ? _stripWidth : _stripContainer->GetWidth();
    const float winWAtlas = CompassProjection::StripWindowWidthAtlas(
        atlasCircumference,
        _stripTexWidth,
        _kDefaultStripTexWidth,
        viewW,
        kx,
        true,
        _stripBaseTexRect.width());

    if (winWAtlas <= 0.0f || atlasCircumference <= 0.0f)
    {
        return;
    }

    const float uAtlas = CompassProjection::ComputeStripUAtlas(
        heading + _stripHeadingBiasRad,
        atlasCircumference,
        _stripTexWidth,
        _kDefaultStripTexWidth,
        viewW,
        kx,
        true,
        _stripBaseTexRect.width(),
        _stripTexLoop,
        deg2rad(_kHalfCircleRad),
        deg2rad(_kTwoPiRad));

    if (uAtlas <= -1.0e8f)
    {
        return;
    }

    if (std::abs(uAtlas - _dirty.lastStripU) < 0.01f)
    {
        return;
    }
    _dirty.lastStripU = uAtlas;

    float drawX = 0.0f;
    float drawY = 0.0f;
    float drawW = 0.0f;
    float drawH = 0.0f;
    GetDialDrawGeom(drawX, drawY, drawW, drawH);
    if (drawW <= 0.0f || drawH <= 0.0f)
    {
        return;
    }

    const float uEnd = uAtlas + winWAtlas;
    const bool wraps = _stripTexLoop && _stripLoop && (uEnd > atlasCircumference + 0.001f);

    if (!wraps)
    {
        if (_stripLoop)
        {
            _stripLoop->Show(false);
        }

        Frect rect;
        rect.lt.y = _stripBaseTexRect.lt.y;
        rect.rb.y = _stripBaseTexRect.rb.y;
        rect.lt.x = _stripBaseTexRect.x1 + uAtlas;
        rect.rb.x = rect.lt.x + winWAtlas;
        _strip->SetTextureRect(rect);
        _strip->SetWndPos(Fvector2().set(drawX, drawY));
        _strip->SetWndSize(Fvector2().set(drawW, drawH));
        _strip->Show(true);
        return;
    }

    const float w1 = atlasCircumference - uAtlas;
    const float w2 = winWAtlas - w1;
    const float screenW1 = drawW * (w1 / winWAtlas);
    const float screenW2 = drawW - screenW1;

    Frect rect1;
    rect1.lt.y = _stripBaseTexRect.lt.y;
    rect1.rb.y = _stripBaseTexRect.rb.y;
    rect1.lt.x = _stripBaseTexRect.x1 + uAtlas;
    rect1.rb.x = _stripBaseTexRect.x1 + atlasCircumference;
    _strip->SetTextureRect(rect1);
    _strip->SetWndPos(Fvector2().set(drawX, drawY));
    _strip->SetWndSize(Fvector2().set(screenW1, drawH));
    _strip->Show(true);

    Frect rect2;
    rect2.lt.y = _stripBaseTexRect.lt.y;
    rect2.rb.y = _stripBaseTexRect.rb.y;
    rect2.lt.x = _stripBaseTexRect.x1;
    rect2.rb.x = _stripBaseTexRect.x1 + w2;
    _stripLoop->SetTextureRect(rect2);
    _stripLoop->SetWndPos(Fvector2().set(drawX + screenW1, drawY));
    _stripLoop->SetWndSize(Fvector2().set(screenW2, drawH));
    _stripLoop->Show(true);
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


bool CUICompassBar::ProjectToStrip(const Fvector& targetPos, const Fvector& actorPos, float camHeading,
    float& outX, bool clampToEdges) const
{
    return CompassProjection::ProjectToStrip(
        _runtimeCfg.fovRad,
        _stripWidth,
        _kMinDistanceSq,
        targetPos,
        actorPos,
        camHeading,
        outX,
        clampToEdges);
}

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
    return CompassProjection::CalculateFovEdgeFade(
        relX,
        stripWidth,
        _runtimeCfg.fovFadeEdgeLo,
        _runtimeCfg.fovFadeEdgeHi,
        _runtimeCfg.fovFadeInner,
        _runtimeCfg.fovFadeOuter);
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

    for (u32 i = 0; i < _cardinalEntries.size() && i < _kMaxLabelPoints; ++i)
    {
        SCompassCardinalEntry& entry = _cardinalEntries[i];
        CUIWindow* host = entry.host;
        CUIStatic* text = entry.text;
        if (!host || !text)
        {
            continue;
        }

        float relX = 0.0f;
        bool isVisible = false;
        if (entry.hasAngle)
        {
            isVisible = CompassProjection::AngleToStripX(
                entry.angleRad,
                ctx.heading,
                _runtimeCfg.fovRad,
                _stripWidth,
                relX,
                false);
        }
        else
        {
            Fvector fakeTarget;
            fakeTarget.set(
                ctx.actorPos.x + entry.dirXZ.x * _runtimeCfg.cardinalFakeDistance,
                ctx.actorPos.y,
                ctx.actorPos.z + entry.dirXZ.y * _runtimeCfg.cardinalFakeDistance);
            isVisible = ProjectToStrip(fakeTarget, ctx.actorPos, ctx.heading, relX, false);
        }
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

bool CUICompassBar::ShouldShowSpot(CMapLocation* loc, const Fvector& actorPos, const shared_str& levelName,
    CMapLocation* activeTaskLoc) const
{
    (void)actorPos;
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
    if (locSize.x > 0.0f && locSize.y > 0.0f)
    {
        cand.iconSize = locSize;
    }
    else
    {
        const float parentW = _stripContainer ? _stripContainer->GetWidth() : UI_BASE_WIDTH;
        const float parentH = _stripContainer ? _stripContainer->GetHeight() : UI_BASE_HEIGHT;
        cand.iconSize = Fvector2().set(
            ResolveCompassLayoutValue(_spotCfg.spotWidth, parentW),
            ResolveCompassLayoutValue(_spotCfg.spotHeight, parentH));
    }

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
        const float parentW = _stripContainer ? _stripContainer->GetWidth() : _stripWidth;
        const float parentH = _stripContainer ? _stripContainer->GetHeight() : GetHeight();
        item.relX = relX + ResolveCompassLayoutValue(_spotCfg.offsetX, parentW) + cand.offsetX;
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
        item.sortKind = static_cast<u8>(cand.sourceLoc ? cand.sourceLoc->GetCompassSpotKind() : ECompassSpotKind::Generic);
        item.offsetY = cand.offsetY + ResolveCompassLayoutValue(_spotCfg.offsetY, parentH);
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

    if (pool.size() >= _kMaxSpotPoolSlots)
    {
        return nullptr;
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

        {
            SUITextureShadowParams shadow;
            if (item.sourceLoc)
            {
                shadow = item.sourceLoc->GetCompassTextureShadow();
                if (!item.sourceLoc->HasCompassShadowOverride() && !shadow.enabled)
                {
                    shadow = _spotCfg.defaultShadow;
                }
            }
            else
            {
                shadow = _spotCfg.defaultShadow;
            }

            if (shadow.enabled)
            {
                wnd->SetTextureShadow(true, shadow.thickness, shadow.color);
            }
            else
            {
                wnd->SetTextureShadow(false, 0.0f, 0);
            }
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
    spotX = clampr(spotX, ResolveCompassLayoutValue(_runtimeCfg.activePadding, _stripWidth),
        _stripWidth - ResolveCompassLayoutValue(_runtimeCfg.activePadding, _stripWidth));
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

    SUITextureShadowParams shadow = activeLoc->GetCompassTextureShadow();
    if (!activeLoc->HasCompassShadowOverride() && !shadow.enabled)
    {
        shadow = _spotCfg.defaultShadow;
    }
    if (shadow.enabled)
    {
        _activeMarker->SetTextureShadow(true, shadow.thickness, shadow.color);
    }
    else
    {
        _activeMarker->SetTextureShadow(false, 0.0f, 0);
    }

    _activeMarker->Show(true);
}

void CUICompassBar::UpdateActiveTargetText(const Fvector& actorPos, const Fvector& tgtPos, CMapLocation* activeLoc)
{
    if (!_activeDistText)
    {
        return;
    }

    if (_activeDistTextFollowMarkerColor && activeLoc)
    {
        const u32 locColor = activeLoc->GetCompassColor();
        _activeDistText->SetTextColor(locColor != 0 ? locColor : _kDefaultColorWhite);
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
        const float offsetY = ResolveCompassLayoutValue(_runtimeCfg.activeOffsetY, GetHeight());
        const float containerTop = geom.top + geom.CenterY() + offsetY - ch * 0.5f;
        _activeTargetContainer->SetWndPos(Fvector2().set(containerLeft, containerTop));
        _activeTargetContainer->Show(true);
    }
    if (_activeMarker)
    {
        UpdateActiveTargetMarker(activeLoc);
    }
    if (_activeDistText)
    {
        UpdateActiveTargetText(actorPos, tgtPos, activeLoc);
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
