#include "stdafx.h"
#include "UIVectorBinding.h"

#include "../Include/xrRender/UIShader.h"
#include "UIXmlInit.h"
#include "Widgets/UIStatic.h"
#include "ui_base.h"

namespace
{
    bool hasNonEmptyString(const char* str)
    {
        return str != nullptr && str[0] != 0;
    }
}

CUIVectorBinding::CUIVectorBinding() : _hasVector(false)
{
}

CUIVectorBinding::~CUIVectorBinding()
{
}

void CUIVectorBinding::Reset()
{
    _svgFile = nullptr;
    _tint = {};
    _hasVector = false;
}

bool CUIVectorBinding::HasSvgChildElement(CUIXml& xmlDoc, const char* elementPath, int index)
{
    string256 svgPath;
    xr_strconcat(svgPath, elementPath, ":svg");
    return xmlDoc.NavigateToNode(svgPath, index) != nullptr;
}

const char* CUIVectorBinding::QueryFileNameFromXml(CUIXml& xmlDoc, const char* elementPath, int index)
{
    const char* svg = xmlDoc.ReadAttrib(elementPath, index, "svg");
    if (!hasNonEmptyString(svg))
    {
        string256 svgNode;
        xr_strconcat(svgNode, elementPath, ":svg");
        if (xmlDoc.NavigateToNode(svgNode, index))
            svg = xmlDoc.Read(svgNode, index, nullptr);
    }
    if (!hasNonEmptyString(svg))
        return nullptr;
    return svg;
}

void CUIVectorBinding::LoadTintFromXml(SVGTintRGBA& outTint, CUIXml& xmlDoc, const char* elementPath, int index)
{
    outTint.SetFromColourDword(CUIXmlInit::GetColor(xmlDoc, elementPath, index, 255));
}

void CUIVectorBinding::LoadFromXml(CUIXml& xmlDoc, const char* elementPath, int index)
{
    Reset();
    const char* name = QueryFileNameFromXml(xmlDoc, elementPath, index);
    if (hasNonEmptyString(name))
    {
        _svgFile = name;
        _hasVector = true;
        LoadTintFromXml(_tint, xmlDoc, elementPath, index);
    }
}

void CUIVectorBinding::Assign(const char* svgFileName, SVGTintRGBA tint)
{
    Reset();
    if (hasNonEmptyString(svgFileName))
    {
        _svgFile = svgFileName;
        _tint = tint;
        _hasVector = true;
    }
}

const char* CUIVectorBinding::GetFileName() const
{
    return (_hasVector && _svgFile.size() != 0) ? _svgFile.c_str() : nullptr;
}

bool CUIVectorBinding::ApplyToStaticItem(CUIStaticItem& item, float widgetWidth, float widgetHeight) const
{
    if (!_hasVector)
        return false;
    return ApplyVectorFileToStaticItem(_svgFile, _tint, item, widgetWidth, widgetHeight);
}

bool CUIVectorBinding::ApplyVectorFileToStaticItem(const shared_str& svgTextureName, SVGTintRGBA tint, CUIStaticItem& item, float widgetWidth, float widgetHeight)
{
    const xr_string_view subpath = svgTextureName.size() ? xr_string_view(svgTextureName.c_str()) : xr_string_view{};
    return ApplyVectorRasterToStaticItem(subpath, tint, item, widgetWidth, widgetHeight, true);
}

bool CUIVectorBinding::ApplyVectorRasterToStaticItem(xr_string_view svgSubpath, SVGTintRGBA tint, CUIStaticItem& item, float widgetWidth, float widgetHeight, bool setScaledSizeOnItem)
{
    float reqW = widgetWidth;
    float reqH = widgetHeight;
    Fvector2 scaled;
    UI().ClientToScreenScaled(scaled, reqW, reqH);
    reqW = scaled.x;
    reqH = scaled.y;

    const xr_string_view pathForShader =
        svgSubpath.empty() ? xr_string_view(_kDefaultSVGShader) : svgSubpath;
    const ui_shader& sh = UI().GetVectorShader(pathForShader, reqW, reqH, tint);
    const Frect uv = UI().GetVectorUV(pathForShader, reqW, reqH, tint);
    item.SetShader(sh);
    item.SetTextureRect(uv);
    if (setScaledSizeOnItem)
        item.SetSize(Fvector2().set(reqW, reqH));
    return true;
}

bool CUIVectorBinding::ApplyXmlStaticVectorToWindow(CUIStatic& uiStatic, CUIXml& xmlDoc)
{
    if (!uiStatic.isSVGPresented())
        return false;
    R_ASSERT(uiStatic.WindowNodeName().size() > 0);
    const char* fileName = uiStatic.getSVGFilename(xmlDoc, uiStatic.WindowNodeName().c_str(), 0);
    if (fileName == nullptr)
        return false;
    Fvector2 scaledWH;
    UI().ClientToScreenScaled(scaledWH, uiStatic.GetWidth(), uiStatic.GetHeight());
    const ui_shader& svgShader = UI().GetVectorShader(fileName, scaledWH.x, scaledWH.y, uiStatic.GetVectorTint());
    const Frect svgUv = UI().GetVectorUV(fileName, scaledWH.x, scaledWH.y, uiStatic.GetVectorTint());
    uiStatic.SetShader(svgShader);
    uiStatic.SetTextureRect(svgUv);
    return true;
}

bool CUIVectorBinding::ApplyVectorPathToStatic(CUIStatic& uiStatic, xr_string_view vectorSubpath, float requestedWidth, float requestedHeight, SVGTintRGBA tint)
{
    CUIStaticItem* item = uiStatic.GetStaticItem();
    R_ASSERT(item);
    return ApplyVectorRasterToStaticItem(vectorSubpath, tint, *item, requestedWidth, requestedHeight, false);
}

bool CUIVectorBinding::CaptureMiniMapSpotNormalVectorIcon(CUIStatic& spot, CUIXml& xmlDoc, const char* elementPath, int nodeIndex, ui_shader& outIconNormal, Frect& outTexRectNormal, const Frect& restoreTextureRect)
{
    auto restoreAndFailLambda = [&]() -> bool
    {
        spot.SetTextureRect(restoreTextureRect);
        return false;
    };

    if (!spot.isSVGPresented())
        return restoreAndFailLambda();

    string256 buf;
    xr_strconcat(buf, elementPath, ":texture");
    if (!xmlDoc.NavigateToNode(buf, nodeIndex))
        return restoreAndFailLambda();

    const char* svgFileName = spot.getSVGFilename(xmlDoc, elementPath, nodeIndex);
    if (!hasNonEmptyString(svgFileName))
        return restoreAndFailLambda();

    spot.InitTexture("", svgFileName);
    outTexRectNormal = spot.GetTextureRect();
    outIconNormal = spot.GetShader();
    spot.SetTextureRect(restoreTextureRect);
    return true;
}

