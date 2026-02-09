#include "StdAfx.h"
#include "UICompassBar.h"
#include "../Actor.h"
#include "../Level.h"
#include "../map_location.h"
#include "../map_location_defs.h"
#include "../map_manager.h"
#include "../../xrEngine/Device.h"
#include "../../xrEngine/GameFont.h"
#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "../../xrCore/_stl_extensions.h"
#include "../../xrCore/vector.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UILines.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../xrUI/ui_defs.h"
#include <algorithm>

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

namespace
{
    EUILayoutUnits ParseUnits(CUIXml& uiXml, const char* path, int index)
    {
        LPCSTR u = uiXml.ReadAttrib(path, index, "units", nullptr);
        if (!u || !*u)
        {
            return EUILayoutUnits::Auto;
        }
        if (_stricmp(u, "relative") == 0)
        {
            return EUILayoutUnits::Relative;
        }
        if (_stricmp(u, "px") == 0)
        {
            return EUILayoutUnits::Px;
        }
        return EUILayoutUnits::Auto;
    }
}

CUICompassBar::CUICompassBar()
    : _background(nullptr),
      _layerBg(nullptr),
      _strip(nullptr),
      _stripContainer(nullptr),
      _stripUnits(EUILayoutUnits::Auto),
      _cardinalsUnits(EUILayoutUnits::Auto),
      _layerFg(nullptr),
      _activeTargetContainer(nullptr),
      _activeMarker(nullptr),
      _activeDistText(nullptr),
      _activeTargetLoc(nullptr),
      _lastActiveLoc(nullptr),
      _activeTargetCurX(0.0f),
      _fov(deg2rad(_kDefaultFovDeg)),
      _stripWidth(0.0f),
      _stripTexWidth(_kDefaultStripTexWidth),
      _stripTexLoop(true),
      _stripTextureScaleX(1.0f),
      _stripTextureScaleY(1.0f),
      _stripTextureOffsetX(0.0f),
      _stripTextureOffsetY(0.0f),
      _stripTextureHeightPx(-1.0f),
      _stripTextureWidthPx(-1.0f),
      _collectSpotsTimer(0.0f),
      _isGameTypeSingleCompatible(false),
      _stripGeometryCached(false)
{
    _cfg.markersY = 0.0f;
    _cfg.activePadding = _kDefaultActivePadding;
    _cfg.smoothingSpeed = _kDefaultSmoothingSpeed;
    _cfg.activeOffsetY = 0.0f;
}

CUICompassBar::~CUICompassBar()
{
    _poolSpots.clear();
    _poolSpotTextureNames.clear();
}

void CUICompassBar::Init()
{
    CUIXml uiXml;
    shared_str compassXmlName = UI().get_xml_name("compass_bar.xml");
    uiXml.Load(CONFIG_PATH, UI_PATH, compassXmlName.c_str());
    CUIXmlInit xmlInit;
    if (!uiXml.NavigateToNode("compass_bar", 0))
    {
        Msg("! CUICompassBar::Init: node 'compass_bar' not found in %s", compassXmlName.c_str());
        return;
    }
    InitWindowAndBackground(uiXml, xmlInit);
    InitLayoutFromXml(uiXml);

    _layerBg = new CUIWindow();
    _layerBg->SetAutoDelete(true);
    _layerBg->SetWndSize(GetWndSize());
    _layerBg->SetWndPos(Fvector2().set(0.0f, 0.0f));

    InitCompassDial(uiXml, xmlInit);

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
    float fovDeg = uiXml.ReadAttribFlt("compass_bar", 0, "fov_angle", _kDefaultFovDeg);
    _fov = deg2rad(fovDeg);
    if (_fov <= 0.0f)
    {
        _fov = deg2rad(_kDefaultFovDeg);
    }
    ParseSpots(uiXml, "compass_bar:spots");

    if (uiXml.NavigateToNode("compass_bar:active_target", 0))
    {
        _cfg.activePadding = uiXml.ReadAttribFlt("compass_bar:active_target", 0, "active_target_padding", _kDefaultActivePadding);
        _cfg.activePadding = uiXml.ReadAttribFlt("compass_bar:active_target", 0, "padding", _cfg.activePadding);
        _cfg.smoothingSpeed = uiXml.ReadAttribFlt("compass_bar:active_target", 0, "smoothing_speed", _kDefaultSmoothingSpeed);
        _cfg.activeOffsetY = uiXml.ReadAttribFlt("compass_bar:active_target", 0, "offset_y", 0.0f);
    }
}

void CUICompassBar::ParseSpots(CUIXml& uiXml, const char* path)
{
    _spotCfg.show = true;
    _spotCfg.offsetX = 0.0f;
    _spotCfg.offsetY = 0.0f;
    _spotCfg.align = 1;
    _spotCfg.maxDistance = -1.0f;
    _spotCfg.defaultSpotColor = _kDefaultColorWhite;
    if (!uiXml.NavigateToNode(path, 0))
    {
        return;
    }
    _spotCfg.show = uiXml.ReadAttribInt(path, 0, "show", 1) != 0;
    _spotCfg.offsetX = uiXml.ReadAttribFlt(path, 0, "x", 0.0f);
    _spotCfg.offsetY = uiXml.ReadAttribFlt(path, 0, "y", 0.0f);
    _cfg.markersY = uiXml.ReadAttribFlt(path, 0, "markers_y", 0.0f);
    _spotCfg.align = ParseAlign(uiXml.ReadAttrib(path, 0, "align", "c"));
    _spotCfg.maxDistance = uiXml.ReadAttribFlt(path, 0, "max_distance", -1.0f);
    _spotCfg.collectInterval = uiXml.ReadAttribFlt(path, 0, "collect_interval", _kDefaultCollectInterval);
    if (_spotCfg.collectInterval <= 0.0f)
    {
        _spotCfg.collectInterval = _kDefaultCollectInterval;
    }
    string_path tmplPath;
    xr_strconcat(tmplPath, path, ":spot_template");
    if (uiXml.NavigateToNode(tmplPath, 0))
    {
        _spotCfg.spotWidth = uiXml.ReadAttribFlt(tmplPath, 0, "width", 0.0f);
        _spotCfg.spotHeight = uiXml.ReadAttribFlt(tmplPath, 0, "height", 0.0f);
    }
    LPCSTR defaultColorName = uiXml.ReadAttrib(path, 0, "color", "ui_1");
    CUIXmlInit::ColorDefs::const_iterator colorIt = CUIXmlInit::GetColorDefs()->find(defaultColorName);
    _spotCfg.defaultSpotColor = (colorIt != CUIXmlInit::GetColorDefs()->end()) ? colorIt->second : _kDefaultColorWhite;

    string_path typesPath;
    xr_strconcat(typesPath, path, ":spot_types");
    XML_NODE* typesNode = uiXml.NavigateToNode(typesPath, 0);
    if (typesNode)
    {
        _defaultSpotConfig.size.set(_spotCfg.spotWidth, _spotCfg.spotHeight);
        _defaultSpotConfig.offsetX = 0.0f;
        for (tinyxml2::XMLElement* child = typesNode->FirstChildElement(); child; child = child->NextSiblingElement())
        {
            ParseSpotType(uiXml, child, _defaultSpotConfig);
        }
    }
}

void CUICompassBar::ParseSpotType(CUIXml& uiXml, tinyxml2::XMLElement* child, const SCompassSpotParams& defaultParams)
{
    SCompassSpotParams p = defaultParams;
    p.offsetY = uiXml.ReadAttribFlt(child, "offset_y", p.offsetY);
    p.offsetX = uiXml.ReadAttribFlt(child, "offset_x", p.offsetX);
    p.maxDist = uiXml.ReadAttribFlt(child, "max_dist", -1.0f);
    if (p.maxDist < 0.0f)
    {
        p.maxDist = uiXml.ReadAttribFlt(child, "compass_dist", -1.0f);
    }
    LPCSTR va = uiXml.ReadAttrib(child, "valign", "center");
    if (_stricmp(va, "top") == 0)
    {
        p.valign = EVAlign::Top;
    }
    else if (_stricmp(va, "bottom") == 0)
    {
        p.valign = EVAlign::Bottom;
    }
    else
    {
        p.valign = EVAlign::Center;
    }
    LPCSTR name = child->Value();
    if (name && _stricmp(name, "default") == 0)
    {
        _defaultSpotConfig = p;
    }
    else if (name && xr_strlen(name) > 0)
    {
        _spotConfigs[shared_str(name)] = p;
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
    string_path cardinalsPathBuf;
    xr_strconcat(cardinalsPathBuf, stripPath, ":cardinal_points");
    if (uiXml.NavigateToNode(cardinalsPathBuf, 0))
    {
        cardinalsPath = cardinalsPathBuf;
    }
    if (!uiXml.NavigateToNode(stripPath, 0))
    {
        return;
    }
    _stripUnits = ParseUnits(uiXml, stripPath, 0);
    _stripTexWidth = uiXml.ReadAttribFlt(stripPath, 0, "tex_width", _kDefaultStripTexWidth);
    _stripTexLoop = uiXml.ReadAttribInt(stripPath, 0, "tex_loop", 1) != 0;

    _stripContainer = new CUICompassClipWindow();
    _stripContainer->SetAutoDelete(true);
    xmlInit.InitWindow(uiXml, stripPath, 0, _stripContainer);
    AttachChild(_stripContainer);

    string_path texPath;
    xr_strconcat(texPath, stripPath, ":texture");
    shared_str texName = uiXml.Read(texPath, 0, "ui_inGame2_compass_dial");
    _stripTextureScaleX = uiXml.ReadAttribFlt(texPath, 0, "width", 1.0f);
    _stripTextureScaleY = uiXml.ReadAttribFlt(texPath, 0, "height", 1.0f);
    _stripTextureOffsetX = uiXml.ReadAttribFlt(texPath, 0, "offset_x", 0.0f);
    _stripTextureOffsetY = uiXml.ReadAttribFlt(texPath, 0, "offset_y", 0.0f);
    _stripTextureHeightPx = uiXml.ReadAttribFlt(texPath, 0, "height_px", -1.0f);
    _stripTextureWidthPx = uiXml.ReadAttribFlt(texPath, 0, "width_px", -1.0f);

    _strip = new CUIStatic();
    _strip->SetAutoDelete(true);
    _strip->SetWndPos(Fvector2().set(0.0f, 0.0f));
    _strip->SetWndSize(Fvector2().set(1.0f, 1.0f));
    _strip->SetStretchTexture(true);
    if (!_strip->InitTexture(texName.c_str(), false))
    {
        _strip->InitTexture("ui_inGame2_compass_dial", false);
    }
    _stripContainer->AttachChild(_strip);
    float defY = uiXml.ReadAttribFlt(cardinalsPath, 0, "y", 0.0f);
    float defW = uiXml.ReadAttribFlt(cardinalsPath, 0, "width", 16.0f);
    float defH = uiXml.ReadAttribFlt(cardinalsPath, 0, "height", 14.0f);
    if (uiXml.NavigateToNode(cardinalsPath, 0))
    {
        _cardinalsUnits = ParseUnits(uiXml, cardinalsPath, 0);
    }
    _cardinals.clear();
    _cardinals.reserve(_kMaxCardinalPoints);
    _cardinalLayout.clear();
    _cardinalLayout.reserve(_kMaxCardinalPoints);

    string_path mainPath;
    xr_strconcat(mainPath, cardinalsPath, ":main_cardinals");
    const char* mainDirs[] = { "n", "e", "s", "w" };
    for (const char* d : mainDirs)
    {
        string_path nodePath;
        xr_sprintf(nodePath, "%s:%s", mainPath, d);
        if (uiXml.NavigateToNode(nodePath, 0))
        {
            CUIStatic* st = InitCardinalStatic(uiXml, xmlInit, cardinalsPath, mainPath, d, defY, defW, defH,
                _cardinalsUnits, &_cardinalLayout);
            if (st && _stripContainer)
            {
                _stripContainer->AttachChild(st);
                _cardinals.push_back(st);
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
                CUIStatic* st = InitCardinalStatic(uiXml, xmlInit, cardinalsPath, interPath, d, defY, defW, defH,
                    _cardinalsUnits, &_cardinalLayout);
                if (st && _stripContainer)
                {
                    _stripContainer->AttachChild(st);
                    _cardinals.push_back(st);
                }
            }
        }
    }
}

CUIStatic* CUICompassBar::InitCardinalStatic(CUIXml& uiXml, CUIXmlInit& xmlInit, const char* cardinalsPath,
    const char* groupPath, const char* directionNode, float defaultY, float defaultW, float defaultH,
    EUILayoutUnits units, xr_vector<Fvector3>* outLayout)
{
    string_path childPath;
    string_path defaultTextPath;
    string_path groupTextPath;
    string_path childTextPath;
    xr_strconcat(childPath, groupPath, ":", directionNode);
    xr_strconcat(defaultTextPath, cardinalsPath, ":text");
    xr_strconcat(groupTextPath, groupPath, ":text");
    xr_strconcat(childTextPath, childPath, ":text");

    float y = uiXml.ReadAttribFlt(childPath, 0, "y", defaultY);
    float w = uiXml.ReadAttribFlt(childPath, 0, "width", defaultW);
    float h = uiXml.ReadAttribFlt(childPath, 0, "height", defaultH);

    if (outLayout)
    {
        outLayout->push_back(Fvector3().set(y, w, h));
    }

    CUIStatic* st = new CUIStatic();
    st->SetAutoDelete(true);
    if (units == EUILayoutUnits::Relative)
    {
        st->SetWndPos(Fvector2().set(0.0f, 0.0f));
        st->SetWndSize(Fvector2().set(1.0f, 1.0f));
    }
    else
    {
        st->SetWndPos(Fvector2().set(0.0f, y));
        st->SetWndSize(Fvector2().set(w, h));
    }

    if (uiXml.NavigateToNode(defaultTextPath, 0))
    {
        xmlInit.InitText(uiXml, defaultTextPath, 0, st);
    }
    if (uiXml.NavigateToNode(groupTextPath, 0))
    {
        xmlInit.InitText(uiXml, groupTextPath, 0, st);
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
            st->SetTextColor(CUIXmlInit::GetColor(uiXml, childPath, 0, _kDefaultColorWhite));
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

void CUICompassBar::InitActiveTargetWidgets(CUIXml& uiXml, CUIXmlInit& xmlInit)
{
    const char* markerPath = "compass_bar:active_target:marker";
    LPCSTR texAttr = uiXml.ReadAttrib(markerPath, 0, "texture", nullptr);
    if (texAttr && xr_strlen(texAttr) > 0)
    {
        _activeMarkerFallbackTexture = texAttr;
    }
    else
    {
        _activeMarkerFallbackTexture = uiXml.Read("compass_bar:active_target:marker:texture", 0,
            "ui_inGame2_hint_wnd_main_window");
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
    if (uiXml.NavigateToNode("compass_bar:active_target:distance_text", 0))
    {
        _activeDistText = UIHelper::CreateStatic(uiXml, "compass_bar:active_target:distance_text",
            _activeTargetContainer, false);
        if (_activeDistText)
        {
            _activeDistText->SetAutoDelete(false);
        }
    }
    if (uiXml.NavigateToNode("compass_bar:active_target:marker", 0))
    {
        _activeMarker = UIHelper::CreateStatic(uiXml, "compass_bar:active_target:marker",
            _activeTargetContainer, false);
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
        LPCSTR fontName = uiXml.ReadAttrib("compass_bar:active_target:distance_text", 0, "font", "ui_font_letterica18");
        CGameFont* font = UI().Font().GetFont(fontName);
        if (font)
        {
            _activeDistText->SetFont(font);
        }
        LPCSTR colorName = uiXml.ReadAttrib("compass_bar:active_target:distance_text", 0, "color", "ui_1");
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
    }
    InvalidateStripGeometry();
}

void CUICompassBar::ApplyMainWindowLayout()
{
    Fvector2 temp;
    const float k = UI().get_current_kx();
    if (WndRectIsProbablyRelative())
    {
        temp = GetWndSize();
        temp.y *= UI_BASE_HEIGHT;
        temp.x *= UI_BASE_WIDTH * k;
        SetWndSize(temp);
        temp = GetWndPos();
        temp.x *= UI_BASE_WIDTH;
        temp.y *= UI_BASE_HEIGHT;
        SetWndPos(temp);
    }
}

void CUICompassBar::ApplyLayerLayouts()
{
    if (_layerBg)
    {
        _layerBg->SetWndSize(GetWndSize());
        _layerBg->SetWndPos(Fvector2().set(0.0f, 0.0f));
    }
    if (_layerFg)
    {
        _layerFg->SetWndSize(GetWndSize());
        _layerFg->SetWndPos(Fvector2().set(0.0f, 0.0f));
    }
    if (_background)
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

void CUICompassBar::ApplyStripLayout()
{
    if (!_stripContainer)
    {
        return;
    }
    Fvector2 temp;
    bool shouldScaleStrip = (_stripUnits == EUILayoutUnits::Relative) ||
        (_stripUnits == EUILayoutUnits::Auto && _stripContainer->WndRectIsProbablyRelative());
    if (shouldScaleStrip)
    {
        temp = _stripContainer->GetWndSize();
        temp.x *= GetWidth();
        temp.y *= GetHeight();
        _stripContainer->SetWndSize(temp);
        temp = _stripContainer->GetWndPos();
        temp.x *= GetWidth();
        temp.y *= GetHeight();
        _stripContainer->SetWndPos(temp);
    }
    else if (_stripUnits == EUILayoutUnits::Px)
    {
        // Keep size from XML (preserve intended width/height in px); scale only position
        temp = _stripContainer->GetWndPos();
        temp.x *= GetWidth();
        temp.y *= GetHeight();
        _stripContainer->SetWndPos(temp);
    }
    SCompassStripGeometry geom = GetStripGeometry();
    _stripContainer->SetWndSize(Fvector2().set(geom.width, geom.height));
    if (_stripContainer->GetAlignment() == waCenter)
    {
        float centerX = geom.left + geom.width * 0.5f;
        float centerY = geom.top + geom.height * 0.5f;
        _stripContainer->SetWndPos(Fvector2().set(centerX, centerY));
    }
    else
    {
        _stripContainer->SetWndPos(Fvector2().set(geom.left, geom.top));
    }
    if (_strip)
    {
        const float cw = _stripContainer->GetWidth();
        const float ch = _stripContainer->GetHeight();
        const float texW = _stripTextureWidthPx >= 0.0f ? _stripTextureWidthPx : (cw * _stripTextureScaleX);
        const float texH = _stripTextureHeightPx >= 0.0f ? _stripTextureHeightPx : (ch * _stripTextureScaleY);
        _strip->SetWndSize(Fvector2().set(texW, texH));
        const float texX = (cw - texW) * 0.5f + _stripTextureOffsetX;
        const float texY = (ch - texH) * 0.5f + _stripTextureOffsetY;
        _strip->SetWndPos(Fvector2().set(texX, texY));
    }
    _stripWidth = _strip ? _strip->GetWidth() : 0.0f;
}

void CUICompassBar::ApplyCardinalsLayout()
{
    if (!_stripContainer)
    {
        return;
    }
    bool shouldScaleCardinals = (_cardinalsUnits == EUILayoutUnits::Relative) ||
        (_cardinalsUnits == EUILayoutUnits::Auto && _cardinalLayout.size() > 0 &&
            _cardinalLayout[0].x <= 1.0f && _cardinalLayout[0].y <= 1.0f && _cardinalLayout[0].z <= 1.0f);
    if (shouldScaleCardinals && _cardinalLayout.size() == _cardinals.size())
    {
        const float cw = _stripContainer->GetWidth();
        const float ch = _stripContainer->GetHeight();
        for (size_t i = 0; i < _cardinals.size(); ++i)
        {
            CUIStatic* st = _cardinals[i];
            if (!st)
            {
                continue;
            }
            const Fvector3& l = _cardinalLayout[i];
            st->SetWndPos(Fvector2().set(0.0f, l.x * ch));
            st->SetWndSize(Fvector2().set(l.y * cw, l.z * ch));
        }
    }
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
    if (_fov <= 0.0f || _stripWidth <= 0.0f)
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
    float halfFov = _fov * 0.5f;
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
    if (!_strip)
    {
        return;
    }
    static const float kHalfCircleRad = deg2rad(_kHalfCircleRad);
    static const float kTwoHalfCirclesRad = deg2rad(_kTwoPiRad);
    const float uvCenter = (heading + kHalfCircleRad) / kTwoHalfCirclesRad;
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
    Frect rect = _strip->GetTextureRect();
    rect.lt.x = u;
    rect.rb.x = u + winW;
    _strip->SetTextureRect(rect);
}

void CUICompassBar::UpdateCardinals(float heading)
{
    if (!_stripContainer || _cardinals.empty())
    {
        return;
    }
    SCompassStripGeometry geom = GetStripGeometry();
    CObject* viewEntity = Level().CurrentViewEntity();
    if (!viewEntity)
    {
        return;
    }
    Fvector actorPos = viewEntity->Position();
    for (u32 i = 0; i < _cardinals.size() && i < _kMaxCardinalPoints; ++i)
    {
        CUIStatic* st = _cardinals[i];
        if (!st)
        {
            continue;
        }
        Fvector fakeTarget;
        fakeTarget.set(actorPos.x + cosf(_kCardinalAngles[i]) * _kFakeTargetDistance,
            actorPos.y, actorPos.z + sinf(_kCardinalAngles[i]) * _kFakeTargetDistance);
        float relX;
        if (!ProjectToStrip(fakeTarget, actorPos, heading, relX, false))
        {
            st->Show(false);
            continue;
        }
        float cw = st->GetWidth();
        float posX = geom.CenterX() + relX - cw * 0.5f;
        st->SetWndPos(Fvector2().set(posX, st->GetWndPos().y));
        st->Show(true);
    }
}

bool CUICompassBar::ShouldShowSpot(CMapLocation* loc, const Fvector& actorPos, const shared_str& levelName,
    CMapLocation* activeTaskLoc) const
{
    if (!loc || !loc->ShowOnCompass() || loc == activeTaskLoc)
    {
        return false;
    }
    if (loc->GetLevelName() != levelName || !loc->SpotEnabled() || !loc->Update())
    {
        return false;
    }
    if (loc->GetCompassSpotTexture().size() == 0)
    {
        return false;
    }
    return true;
}

float CUICompassBar::GetSpotMaxDistance(const SCompassSpotParams& params, CMapLocation* loc) const
{
    float maxDist = params.maxDist;
    if (maxDist < 0.0f)
    {
        maxDist = loc->GetCompassMaxDist();
    }
    if (maxDist < 0.0f)
    {
        maxDist = _spotCfg.maxDistance;
    }
    return maxDist;
}

SSpotCandidate CUICompassBar::CreateSpotCandidate(CMapLocation* loc, const SCompassSpotParams& params) const
{
    SSpotCandidate cand;
    cand.pos = loc->GetLastPosition();
    cand.textureName = loc->GetCompassSpotTexture();
    cand.color = loc->GetCompassSpotColor();
    if (cand.color == 0)
    {
        cand.color = (_spotCfg.defaultSpotColor != 0) ? _spotCfg.defaultSpotColor : _kDefaultColorWhite;
    }
    cand.offsetY = params.offsetY;
    cand.offsetX = params.offsetX;
    cand.iconSize = loc->GetCompassSpotSize();
    if (cand.iconSize.x <= 0.0f || cand.iconSize.y <= 0.0f)
    {
        cand.iconSize = params.size;
    }
    cand.valign = params.valign;
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
    Locations locationsSnapshot;
    {
        xrCriticalSectionGuard guard(mapManager->UpdateCS);
        locationsSnapshot = mapManager->Locations();
    }
    CMapLocation* activeTaskLoc = _activeTargetLoc;
    for (const SLocationKey& key : locationsSnapshot)
    {
        CMapLocation* loc = key.location;
        if (!ShouldShowSpot(loc, actorPos, levelName, activeTaskLoc))
        {
            continue;
        }
        shared_str spotName = loc->GetSpotName();
        xr_map<shared_str, SCompassSpotParams>::const_iterator it = _spotConfigs.find(spotName);
        const SCompassSpotParams& params = (it != _spotConfigs.end()) ? it->second : _defaultSpotConfig;
        Fvector pos = loc->GetLastPosition();
        float maxDist = GetSpotMaxDistance(params, loc);
        if (maxDist >= 0.0f && actorPos.distance_to(pos) > maxDist)
        {
            continue;
        }
        SSpotCandidate cand = CreateSpotCandidate(loc, params);
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
        item.relX = relX + _spotCfg.offsetX + cand.offsetX;
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
    return item;
}

void CUICompassBar::CommitLayout()
{
    if (!_stripContainer)
    {
        return;
    }
    std::sort(_renderQueue.begin(), _renderQueue.end());
    if (_poolSpotTextureNames.capacity() < _renderQueue.size())
    {
        _poolSpotTextureNames.reserve(_renderQueue.size());
    }
    SCompassStripGeometry geom = GetStripGeometry();
    const float globalY = _cfg.markersY + _spotCfg.offsetY;
    const float kx = UI().get_current_kx();
    u32 idx = 0;
    for (const SSpotRenderItem& item : _renderQueue)
    {
        CUIStatic* wnd = GetSpotFromPool(_poolSpots, _stripContainer, idx);
        if (!wnd)
        {
            ++idx;
            continue;
        }
        if (_poolSpotTextureNames.size() <= idx)
        {
            _poolSpotTextureNames.resize(idx + 1);
        }
        if (_poolSpotTextureNames[idx] != *item.textureName)
        {
            CUITextureMaster::InitTexture(*item.textureName, &wnd->GetUIStaticItem());
            _poolSpotTextureNames[idx] = *item.textureName;
        }
        Fvector2 spotSize(item.iconSize.x * kx, item.iconSize.y);
        wnd->SetWndSize(spotSize);
        wnd->SetTextureColor(item.color);
        float posOffsetX = 0.0f;
        if (kx > 0.0f && kx != 1.0f)
        {
            if (_spotCfg.align == 1)
            {
                posOffsetX = item.iconSize.x * 0.5f * (1.0f - kx);
            }
            else if (_spotCfg.align == 2)
            {
                posOffsetX = item.iconSize.x * (1.0f - kx);
            }
        }
        const float posX = geom.CenterX() + item.relX + posOffsetX;
        float posY;
        switch (item.valign)
        {
            case EVAlign::Top:
            {
                posY = globalY + item.offsetY;
                break;
            }

            case EVAlign::Bottom:
            {
                posY = geom.height + globalY + item.offsetY - item.iconSize.y;
                break;
            }

            case EVAlign::Center:
            default:
            {
                posY = geom.CenterY() + globalY + item.offsetY - item.iconSize.y * 0.5f;
                break;
            }
        }
        wnd->SetWndPos(Fvector2().set(posX, posY));
        wnd->Show(true);
        ++idx;
    }
    for (u32 i = idx; i < _poolSpots.size(); ++i)
    {
        if (_poolSpots[i])
        {
            _poolSpots[i]->Show(false);
        }
    }
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
    spotX = clampr(spotX, _cfg.activePadding, _stripWidth - _cfg.activePadding);
    outX = spotX;
}

void CUICompassBar::UpdateActiveTargetMarker(CMapLocation* activeLoc)
{
    if (!_activeMarker)
    {
        return;
    }
    shared_str texName = activeLoc->GetCompassSpotTexture().size() > 0
        ? activeLoc->GetCompassSpotTexture()
        : _activeMarkerFallbackTexture;
    if (_activeMarkerLastTexture != texName)
    {
        CUITextureMaster::InitTexture(texName, &_activeMarker->GetUIStaticItem());
        _activeMarkerLastTexture = texName;
    }
    _activeMarker->Show(true);
}

void CUICompassBar::UpdateActiveTargetText(const Fvector& actorPos, const Fvector& tgtPos)
{
    if (!_activeDistText)
    {
        return;
    }
    const float dist = actorPos.distance_to(tgtPos);
    string64 buf;
    xr_sprintf(buf, sizeof(buf), "%.0f m", dist);
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
        _activeTargetCurX += (spotX - _activeTargetCurX) * (Device.fTimeDelta * _cfg.smoothingSpeed);
    }
    spotX = _activeTargetCurX;
    SCompassStripGeometry geom = GetStripGeometry();
    if (_activeTargetContainer)
    {
        const float cw = _activeTargetContainer->GetWidth();
        const float ch = _activeTargetContainer->GetHeight();
        const float containerLeft = geom.left + spotX - cw * 0.5f;
        const float containerTop = geom.top + geom.CenterY() + _cfg.activeOffsetY - ch * 0.5f;
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

void CUICompassBar::CacheGameTypeCompatibility()
{
    _isGameTypeSingleCompatible = IsGameTypeSingleCompatible();
}
