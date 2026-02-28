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
#include "../../xrCore/_color.h"
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

CUICompassBar::CUICompassBar()
    : _background(nullptr),
      _layerBg(nullptr),
      _strip(nullptr),
      _stripContainer(nullptr),
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
      _collectSpotsTimer(0.0f),
      _isGameTypeSingleCompatible(false),
      _stripGeometryCached(false)
{
    _cfg.activePadding = _kDefaultActivePadding;
    _cfg.smoothingSpeed = _kDefaultSmoothingSpeed;
    _cfg.activeOffsetY = 0.0f;
    _cfg.fadeInSpeed = 12.0f;
    _cfg.fadeOutSpeed = 8.0f;
    _cfg.minVisibleAlpha = 0.01f;
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
    const char* barPath = "compass_bar";
    const float fovDeg = uiXml.ReadAttribFlt(barPath, 0, "fov_angle", _kDefaultFovDeg);
    _fov = (fovDeg > 0.0f) ? deg2rad(fovDeg) : deg2rad(_kDefaultFovDeg);
    
    _cfg.fadeInSpeed = std::max(uiXml.ReadAttribFlt(barPath, 0, "fade_in_speed", _cfg.fadeInSpeed), 0.1f);
    _cfg.fadeOutSpeed = std::max(uiXml.ReadAttribFlt(barPath, 0, "fade_out_speed", _cfg.fadeOutSpeed), 0.1f);
    _cfg.minVisibleAlpha = clampr(uiXml.ReadAttribFlt(barPath, 0, "min_visible_alpha", _cfg.minVisibleAlpha), 0.0f, 1.0f);
    
    ParseSpots(uiXml, "compass_bar:spots");

    if (uiXml.NavigateToNode("compass_bar:active_target", 0))
    {
        const char* targetPath = "compass_bar:active_target";
        _cfg.activePadding = uiXml.ReadAttribFlt(targetPath, 0, "padding", 
            uiXml.ReadAttribFlt(targetPath, 0, "active_target_padding", _kDefaultActivePadding));
        _cfg.smoothingSpeed = uiXml.ReadAttribFlt(targetPath, 0, "smoothing_speed", _kDefaultSmoothingSpeed);
        _cfg.activeOffsetY = uiXml.ReadAttribFlt(targetPath, 0, "y", 0.0f);
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
    _spotCfg.collectInterval = 0.1f;
    
    string_path tmplPath;
    xr_strconcat(tmplPath, path, ":spot_template");
    if (uiXml.NavigateToNode(tmplPath, 0))
    {
        _spotCfg.spotWidth = uiXml.ReadAttribFlt(tmplPath, 0, "width", 0.0f);
        _spotCfg.spotHeight = uiXml.ReadAttribFlt(tmplPath, 0, "height", 0.0f);
    }
    
    const CUIXmlInit::ColorDefs* colorDefs = CUIXmlInit::GetColorDefs();
    LPCSTR defaultColorName = uiXml.ReadAttrib(path, 0, "color", "ui_1");
    CUIXmlInit::ColorDefs::const_iterator colorIt = colorDefs->find(defaultColorName);
    _spotCfg.defaultSpotColor = (colorIt != colorDefs->end()) ? colorIt->second : _kDefaultColorWhite;
}

void CUICompassBar::InitCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit)
{
    const char* stripPath = uiXml.NavigateToNode("compass_bar:compass_dial", 0) ? 
        "compass_bar:compass_dial:strip" : "compass_bar:strip";
    
    string_path cardinalsPathBuf;
    xr_strconcat(cardinalsPathBuf, stripPath, ":cardinal_points");
    const char* cardinalsPath = uiXml.NavigateToNode(cardinalsPathBuf, 0) ? 
        cardinalsPathBuf : "compass_bar:cardinal_points";
    
    if (!uiXml.NavigateToNode(stripPath, 0))
    {
        return;
    }
    
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
    _stripTextureOffsetX = uiXml.ReadAttribFlt(texPath, 0, "x", 0.0f);
    _stripTextureOffsetY = uiXml.ReadAttribFlt(texPath, 0, "y", 0.0f);

    _strip = new CUIStatic();
    _strip->SetAutoDelete(true);
    _strip->SetWndPos(Fvector2().set(0.0f, 0.0f));
    _strip->SetWndSize(Fvector2().set(1.0f, 1.0f));
    _strip->SetStretchTexture(true);
    _strip->InitTexture(texName.c_str(), false) || _strip->InitTexture("ui_inGame2_compass_dial", false);
    _stripContainer->AttachChild(_strip);
    
    const float defY = uiXml.ReadAttribFlt(cardinalsPath, 0, "y", 0.0f);
    const float defW = uiXml.ReadAttribFlt(cardinalsPath, 0, "width", 16.0f);
    const float defH = uiXml.ReadAttribFlt(cardinalsPath, 0, "height", 14.0f);
    
    _cardinals.reserve(_kMaxCardinalPoints);
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
            CUIStatic* st = InitCardinalStatic(uiXml, xmlInit, cardinalsPath, mainPath, d, defY, defW, defH, &_cardinalLayout);
            if (st && _stripContainer)
            {
                _stripContainer->AttachChild(st);
                _cardinals.push_back(st);
                _cardinalAlpha.push_back(1.0f);
                _cardinalBaseTextColor.push_back(st->GetTextColor());
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
                CUIStatic* st = InitCardinalStatic(uiXml, xmlInit, cardinalsPath, interPath, d, defY, defW, defH, &_cardinalLayout);
                if (st && _stripContainer)
                {
                    _stripContainer->AttachChild(st);
                    _cardinals.push_back(st);
                    _cardinalAlpha.push_back(1.0f);
                    _cardinalBaseTextColor.push_back(st->GetTextColor());
                }
            }
        }
    }
}

CUIStatic* CUICompassBar::InitCardinalStatic(CUIXml& uiXml, CUIXmlInit& xmlInit, const char* cardinalsPath,
    const char* groupPath, const char* directionNode, float defaultY, float defaultW, float defaultH,
    xr_vector<Fvector3>* outLayout)
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
    st->SetWndPos(Fvector2().set(0.0f, y));
    st->SetWndSize(Fvector2().set(w, h));

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
    
    if (_stripContainer->WndRectIsProbablyRelative())
    {
        const Fvector2 containerSize = _stripContainer->GetWndSize();
        _stripContainer->SetWndSize(Fvector2().set(containerSize.x * GetWidth(), containerSize.y * GetHeight()));
        
        const Fvector2 containerPos = _stripContainer->GetWndPos();
        _stripContainer->SetWndPos(Fvector2().set(containerPos.x * GetWidth(), containerPos.y * GetHeight()));
    }
    
    if (_strip)
    {
        const float cw = _stripContainer->GetWidth();
        const float ch = _stripContainer->GetHeight();
        const float texW = cw * _stripTextureScaleX;
        const float texH = ch * _stripTextureScaleY;
        _strip->SetWndSize(Fvector2().set(texW, texH));
        _strip->SetWndPos(Fvector2().set((cw - texW) * 0.5f + _stripTextureOffsetX, (ch - texH) * 0.5f + _stripTextureOffsetY));
    }
    _stripWidth = _stripContainer->GetWidth();
}

void CUICompassBar::ApplyCardinalsLayout()
{
    if (!_stripContainer || _cardinalLayout.size() != _cardinals.size())
    {
        return;
    }
    
    const float cw = _stripContainer->GetWidth();
    const float ch = _stripContainer->GetHeight();
    
    for (size_t i = 0; i < _cardinals.size(); ++i)
    {
        CUIStatic* st = _cardinals[i];
        if (st)
        {
            const Fvector3& l = _cardinalLayout[i];
            st->SetWndPos(Fvector2().set(0.0f, l.x * ch));
            st->SetWndSize(Fvector2().set(l.y * cw, l.z * ch));
        }
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
    if (normalizedX <= 0.05f)
    {
        return 0.0f;
    }
    if (normalizedX >= 0.95f)
    {
        return 0.0f;
    }
    if (normalizedX < 0.30f)
    {
        const float t = (normalizedX - 0.05f) / 0.25f;
        return t * t;
    }
    if (normalizedX > 0.70f)
    {
        const float t = (0.95f - normalizedX) / 0.25f;
        return t * t;
    }
    return 1.0f;
}

void CUICompassBar::EnsureFadeStorage()
{
    const size_t cardinalCount = _cardinals.size();
    const size_t spotCount = _poolSpots.size();
    
    _cardinalAlpha.resize(cardinalCount, 1.0f);
    _cardinalBaseTextColor.resize(cardinalCount, _kDefaultColorWhite);
    _poolSpotAlpha.resize(spotCount, 0.0f);
    _poolSpotBaseColor.resize(spotCount, _kDefaultColorWhite);
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
    if (!_strip || !_stripContainer)
    {
        return;
    }
    static const float kHalfCircleRad = deg2rad(_kHalfCircleRad);
    static const float kTwoHalfCirclesRad = deg2rad(_kTwoPiRad);
    const float uvCenter = (heading + kHalfCircleRad) / kTwoHalfCirclesRad;
    const float stripTexW = _stripTexWidth > 0.0f ? _stripTexWidth : _kDefaultStripTexWidth;
    const float w = _stripContainer->GetWidth();
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
    EnsureFadeStorage();
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
        bool isVisible = ProjectToStrip(fakeTarget, actorPos, heading, relX, false);
        _cardinalAlpha[i] = UpdateFadeAlpha(_cardinalAlpha[i], isVisible, _cfg.fadeInSpeed, _cfg.fadeOutSpeed);
        const float edgeFade = isVisible ? CalculateFovEdgeFade(relX, geom.width) : 0.0f;
        const float finalAlpha = _cardinalAlpha[i] * edgeFade;
        if (finalAlpha > _cfg.minVisibleAlpha)
        {
            if (isVisible)
            {
                float cw = st->GetWidth();
                float posX = geom.CenterX() + relX - cw * 0.5f;
                st->SetWndPos(Fvector2().set(posX, st->GetWndPos().y));
            }
            u32 baseColor = _cardinalBaseTextColor[i];
            u32 alpha = (u32)clampr(iFloor(float(color_get_A(baseColor)) * finalAlpha), 0, 255);
            st->SetTextColor(subst_alpha(baseColor, alpha));
            st->Show(true);
        }
        else
        {
            st->Show(false);
        }
    }
}

bool CUICompassBar::ShouldShowSpot(CMapLocation* loc, const Fvector& actorPos, const shared_str& levelName,
    CMapLocation* activeTaskLoc) const
{
    return loc && loc != activeTaskLoc && loc->HasCompassConfig() && loc->SpotEnabled() &&
           loc->Update() && loc->GetLevelName() == levelName && loc->GetCompassTexture().size() > 0;
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
        Fvector pos = loc->GetLastPosition();
        const float maxDist = loc->GetCompassMaxDist();
        // max_dist: not specified or 0.0f = infinite distance (always visible)
        // max_dist > 0.0f = distance limit in game meters
        if (maxDist > 0.0f && actorPos.distance_to(pos) > maxDist)
        {
            continue;
        }
        SSpotCandidate cand = CreateSpotCandidate(loc);
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
        item.sourceLoc = cand.sourceLoc;
        if (_spotCfg.align == alRight)
        {
            item.relX -= cand.iconSize.x;
        }
        else if (_spotCfg.align == alCenter)
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
    _poolSpotOwners.push_back(nullptr);
    _poolSpotAlpha.push_back(0.0f);
    _poolSpotBaseColor.push_back(_kDefaultColorWhite);
    
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
    const float compassBarHeight = GetHeight();
    const float kx = UI().get_current_kx();
    EnsureFadeStorage();
    xr_vector<u8> poolSlotUsed;
    poolSlotUsed.assign(_poolSpots.size(), 0);

    auto allocatePoolSlotLambda = [this, &poolSlotUsed](CMapLocation* sourceLoc) -> u32
    {
        for (u32 i = 0; i < _poolSpotOwners.size(); ++i)
        {
            if (_poolSpotOwners[i] == sourceLoc)
            {
                return i;
            }
        }

        for (u32 i = 0; i < _poolSpots.size(); ++i)
        {
            const bool slotFree = (_poolSpotOwners[i] == nullptr) ||
                                  (_poolSpotAlpha[i] <= _cfg.minVisibleAlpha && !poolSlotUsed[i]);
            if (slotFree)
            {
                return i;
            }
        }

        return _poolSpots.size();
    };

    for (const SSpotRenderItem& item : _renderQueue)
    {
        const u32 poolIdx = allocatePoolSlotLambda(item.sourceLoc);
        CUIStatic* wnd = GetSpotFromPool(_poolSpots, this, poolIdx);
        if (!wnd)
        {
            continue;
        }
        if (poolSlotUsed.size() <= poolIdx)
        {
            poolSlotUsed.resize(poolIdx + 1, 0);
        }
        if (_poolSpotTextureNames.size() <= poolIdx)
        {
            _poolSpotTextureNames.resize(poolIdx + 1);
        }
        if (_poolSpotTextureNames[poolIdx] != *item.textureName)
        {
            CUITextureMaster::InitTexture(*item.textureName, &wnd->GetUIStaticItem());
            _poolSpotTextureNames[poolIdx] = *item.textureName;
        }
        _poolSpotOwners[poolIdx] = item.sourceLoc;
        poolSlotUsed[poolIdx] = 1;
        _poolSpotBaseColor[poolIdx] = item.color;
        Fvector2 spotSize(item.iconSize.x * kx, item.iconSize.y);
        wnd->SetWndSize(spotSize);
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
        const float posX = geom.CenterX() + item.relX + posOffsetX;
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
        _poolSpotAlpha[poolIdx] = UpdateFadeAlpha(_poolSpotAlpha[poolIdx], true, _cfg.fadeInSpeed, _cfg.fadeOutSpeed);
        const float edgeFade = CalculateFovEdgeFade(item.relX, geom.width);
        const float finalAlpha = _poolSpotAlpha[poolIdx] * edgeFade;
        if (finalAlpha > _cfg.minVisibleAlpha)
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
        if (poolSlotUsed[i])
        {
            continue;
        }

        CUIStatic* wnd = _poolSpots[i];
        if (!wnd)
        {
            continue;
        }
        _poolSpotAlpha[i] = UpdateFadeAlpha(_poolSpotAlpha[i], false, _cfg.fadeInSpeed, _cfg.fadeOutSpeed);
        if (_poolSpotAlpha[i] > _cfg.minVisibleAlpha)
        {
            u32 baseColor = (i < _poolSpotBaseColor.size()) ? _poolSpotBaseColor[i] : _kDefaultColorWhite;
            u32 alpha = (u32)clampr(iFloor(float(color_get_A(baseColor)) * _poolSpotAlpha[i]), 0, 255);
            wnd->SetTextureColor(subst_alpha(baseColor, alpha));
            wnd->Show(true);
        }
        else
        {
            wnd->Show(false);
            _poolSpotOwners[i] = nullptr;
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
    
    const shared_str& locTex = activeLoc->GetCompassTexture();
    const shared_str texName = (locTex.size() > 0) ? locTex : _activeMarkerFallbackTexture;
    
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
    if (visible)
    {
        CUIWindow::Draw();
    }
}

void CUICompassBar::Update()
{
    if (!visible || !g_pGameLevel)
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
