#pragma once

#include "../Include/xrRender/SVGTypes.h"
#include "../xrCore/_rect.h"
#include "../xrCore/_stl_extensions_nonalloc.h"
#include "ui_defs.h"
#include "xrUIXmlParser.h"

class CUIStatic;
class CUIStaticItem;
class CUIXml;

class UI_API CUIVectorBinding final
{
public:
    CUIVectorBinding();
    ~CUIVectorBinding();

    void Reset();
    void LoadFromXml(CUIXml& xmlDoc, pcstr elementPath, int index);
    void Assign(pcstr svgFileName, SVGTintRGBA tint = {});

    bool IsActive() const { return _hasVector; }
    const SVGTintRGBA& GetTint() const { return _tint; }
    pcstr GetFileName() const;

    bool ApplyToStaticItem(CUIStaticItem& item, float widgetWidth, float widgetHeight) const;

    static bool ApplyVectorFileToStaticItem(const shared_str& svgTextureName, SVGTintRGBA tint, CUIStaticItem& item, float widgetWidth, float widgetHeight);

    static bool HasSvgChildElement(CUIXml& xmlDoc, pcstr elementPath, int index);
    static pcstr QueryFileNameFromXml(CUIXml& xmlDoc, pcstr elementPath, int index);
    static void LoadTintFromXml(SVGTintRGBA& outTint, CUIXml& xmlDoc, pcstr elementPath, int index);
    static bool ApplyXmlStaticVectorToWindow(CUIStatic& uiStatic, CUIXml& xmlDoc);
    static bool ApplyVectorPathToStatic(CUIStatic& uiStatic, xr_string_view vectorSubpath, float requestedWidth, float requestedHeight, SVGTintRGBA tint = {});
    static bool CaptureMiniMapSpotNormalVectorIcon(CUIStatic& spot, CUIXml& xmlDoc, pcstr elementPath, int nodeIndex, ui_shader& outIconNormal, Frect& outTexRectNormal, const Frect& restoreTextureRect);

private:
    static bool ApplyVectorRasterToStaticItem(xr_string_view svgSubpath, SVGTintRGBA tint, CUIStaticItem& item, float widgetWidth, float widgetHeight, bool setScaledSizeOnItem);

    shared_str _svgFile;
    SVGTintRGBA _tint;
    bool _hasVector;
};

