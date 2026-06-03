#include "stdafx.h"
#include "UIMapZoomScale.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UILines.h"

#include "../../xrEngine/device.h"
#include "../../xrEngine/string_table.h"

namespace
{
float Log2Positive(float value)
{
    if (value <= 0.f)
    {
        return 0.f;
    }

    return logf(value) / logf(2.f);
}

float ClampRatio(float ratio, float maxRatio)
{
    if (maxRatio <= 1.f)
    {
        return 1.f;
    }

    return clampr(ratio, 1.f, maxRatio);
}

float MinFloat(float a, float b)
{
    return a < b ? a : b;
}

float Smoothstep01(float t)
{
    t = clampr(t, 0.f, 1.f);
    return t * t * (3.f - 2.f * t);
}

struct ZoomScaleAxis final
{
    bool isHorizontal = false;

    float along(const Fvector2& v) const
    {
        return isHorizontal ? v.x : v.y;
    }

    float cross(const Fvector2& v) const
    {
        return isHorizontal ? v.y : v.x;
    }

    void setAlong(Fvector2& v, float value) const
    {
        if (isHorizontal)
        {
            v.x = value;
        }
        else
        {
            v.y = value;
        }
    }

    void setCross(Fvector2& v, float value) const
    {
        if (isHorizontal)
        {
            v.y = value;
        }
        else
        {
            v.x = value;
        }
    }

    float alongSize(const Fvector2& size) const
    {
        return isHorizontal ? size.x : size.y;
    }

    float crossSize(const Fvector2& size) const
    {
        return isHorizontal ? size.y : size.x;
    }

    float thumbCenterAlongLocal(const Fvector2& thumbPos, const Fvector2& thumbSize) const
    {
        return along(thumbPos) + alongSize(thumbSize) * 0.5f;
    }
};

bool InitTickLabelTemplate(
    CUIXml& xml,
    const char* node,
    CUIStatic*& outTemplate,
    Fvector2& outOffset,
    Fvector2& outSize)
{
    if (!xml.NavigateToNode(node, 0))
    {
        return false;
    }

    outTemplate = new CUIStatic();
    CUIXmlInit::InitStatic(xml, node, 0, outTemplate);
    outOffset.x = xml.ReadAttribFlt(node, 0, "x", 0.f);
    outOffset.y = xml.ReadAttribFlt(node, 0, "y", 0.f);
    outSize.x = outTemplate->GetWidth();
    outSize.y = outTemplate->GetHeight();
    return true;
}

u8 ParseLcrAlignStr(const char* alignStr)
{
    if (alignStr == nullptr || alignStr[0] == 0)
    {
        return 0;
    }

    if (_stricmp(alignStr, "c") == 0 || _stricmp(alignStr, "center") == 0)
    {
        return 1;
    }
    if (_stricmp(alignStr, "r") == 0 || _stricmp(alignStr, "right") == 0)
    {
        return 2;
    }
    return 0;
}

const char* ResolveThumbXmlPath(CUIXml& xml)
{
    if (xml.NavigateToNode("rail:thumb", 0))
    {
        return "rail:thumb";
    }
    if (xml.NavigateToNode("thumb", 0))
    {
        return "thumb";
    }
    return nullptr;
}

CUIStatic* CreateZoomScaleThumb(CUIXml& xml, CUIWindow* railParent)
{
    if (const char* thumbPath = ResolveThumbXmlPath(xml))
    {
        return UIHelper::CreateStatic(xml, thumbPath, railParent, false);
    }
    return nullptr;
}

u8 ParseThumbCrossAlign(CUIXml& xml, const char* zoomScalePath)
{
    shared_str alignStr;

    if (const char* thumbPath = ResolveThumbXmlPath(xml))
    {
        alignStr = xml.ReadAttrib(thumbPath, 0, "thumb_align", nullptr);
        if (!alignStr.size())
        {
            alignStr = xml.ReadAttrib(thumbPath, 0, "align", nullptr);
        }
    }

    if (!alignStr.size())
    {
        alignStr = xml.ReadAttrib(zoomScalePath, 0, "thumb_align", nullptr);
    }
    if (!alignStr.size())
    {
        alignStr = xml.ReadAttrib(zoomScalePath, 0, "align", "l");
    }

    return ParseLcrAlignStr(alignStr.c_str());
}

u8 ParseRailCrossAlign(CUIXml& xml)
{
    shared_str alignStr = xml.ReadAttrib("rail", 0, "rail_align", nullptr);
    if (!alignStr.size())
    {
        alignStr = xml.ReadAttrib("rail", 0, "align", "l");
    }
    return ParseLcrAlignStr(alignStr.c_str());
}

struct ZoomScaleThumbClampState final
{
    bool clampAlong = true;
    bool clampCross = true;
};

void ApplyClampAttribsFromNode(CUIXml& xml, const char* nodePath, ZoomScaleThumbClampState& clamp)
{
    if (!nodePath || !xml.NavigateToNode(nodePath, 0))
    {
        return;
    }

    if (xml.ReadAttribInt(nodePath, 0, "clamp_to_rail", -1) == 0)
    {
        clamp.clampAlong = false;
        clamp.clampCross = false;
    }

    if (xml.ReadAttribInt(nodePath, 0, "clamp_along", -1) >= 0)
    {
        clamp.clampAlong = (xml.ReadAttribInt(nodePath, 0, "clamp_along", 1) != 0);
    }

    if (xml.ReadAttribInt(nodePath, 0, "clamp_cross", -1) >= 0)
    {
        clamp.clampCross = (xml.ReadAttribInt(nodePath, 0, "clamp_cross", 1) != 0);
    }

    if (xml.ReadAttribInt(nodePath, 0, "thumb_clamp_to_rail", -1) == 0)
    {
        clamp.clampAlong = false;
        clamp.clampCross = false;
    }

    if (xml.ReadAttribInt(nodePath, 0, "thumb_clamp_along", -1) >= 0)
    {
        clamp.clampAlong = (xml.ReadAttribInt(nodePath, 0, "thumb_clamp_along", 1) != 0);
    }

    if (xml.ReadAttribInt(nodePath, 0, "thumb_clamp_cross", -1) >= 0)
    {
        clamp.clampCross = (xml.ReadAttribInt(nodePath, 0, "thumb_clamp_cross", 1) != 0);
    }
}
} // namespace

UIMapZoomScale::~UIMapZoomScale()
{
    xr_delete(_legacyLabelTemplate);
    xr_delete(_boundStyle.xmlTemplate);
    xr_delete(_valueStyle.xmlTemplate);
    xr_delete(_minLabel);
    xr_delete(_maxLabel);
    xr_delete(_valueLabel);
}

void UIMapZoomScale::ReadConfigAttribs(CUIXml& xml, const char* path)
{
    _motionConfig.inertion = xml.ReadAttribFlt(path, 0, "inertion", 0.85f);
    _motionConfig.smoothingScale = xml.ReadAttribFlt(path, 0, "smoothing_scale", 20.f);
    xr_strcpy(_labelConfig.labelFormat, xml.ReadAttrib(path, 0, "label_format", "x%.1f"));
    _labelConfig.boundLabelsMinMax = (xml.ReadAttribInt(path, 0, "bound_labels", 0) != 0);
    _labelConfig.boundLabelMinId = xml.ReadAttrib(path, 0, "bound_label_min", "ui_map_zoom_min");
    _labelConfig.boundLabelMaxId = xml.ReadAttrib(path, 0, "bound_label_max", "ui_map_zoom_max");
    // 0: vertical rail (min zoom at bottom). 1: horizontal (min zoom at left).
    _isHorizontal = (xml.ReadAttribInt(path, 0, "horizontal", 0) != 0);
    _thumbScaleWithZoom = (xml.ReadAttribInt(path, 0, "thumb_scaling", 0) != 0);
    _valueLabelConfig.alignToThumb = (xml.ReadAttribInt(path, 0, "value_label_align_thumb", 0) != 0);
    _valueLabelConfig.alignToBound = (xml.ReadAttribInt(path, 0, "value_label_align_bound", 0) != 0);
    _valueLabelConfig.fadeAtRailEdges = (xml.ReadAttribInt(path, 0, "value_label_hide_at_bounds", 1) != 0);
    _valueLabelConfig.edgeFadeSize = xml.ReadAttribFlt(path, 0, "value_label_edge_fade", 0.f);
}

void UIMapZoomScale::InitRailAndThumb(CUIXml& xml, const char* path)
{
    _thumbCrossAlign = (EZoomCrossAlign)ParseThumbCrossAlign(xml, path);

    _rail = UIHelper::CreateStatic(xml, "rail", this, false);
    if (_rail)
    {
        _rail->Enable(false);
        _railCrossAlign = (EZoomCrossAlign)ParseRailCrossAlign(xml);
        ApplyRailAlignInParent();
    }

    if (!_rail)
    {
        return;
    }

    _thumb = CreateZoomScaleThumb(xml, _rail);
    if (!_thumb)
    {
        return;
    }

    _thumb->Enable(false);
    _thumb->Show(true);
    _thumbBaseSize.set(_thumb->GetWidth(), _thumb->GetHeight());
    _thumbOffset.set(0.f, 0.f);

    if (const char* thumbPath = ResolveThumbXmlPath(xml))
    {
        _thumbOffset.x = xml.ReadAttribFlt(thumbPath, 0, "x", 0.f);
        _thumbOffset.y = xml.ReadAttribFlt(thumbPath, 0, "y", 0.f);
    }

    InitThumbClampAttribs(xml, path);
}

void UIMapZoomScale::InitThumbClampAttribs(CUIXml& xml, const char* zoomScalePath)
{
    ZoomScaleThumbClampState clampState;
    clampState.clampAlong = true;
    clampState.clampCross = true;

    ApplyClampAttribsFromNode(xml, ResolveThumbXmlPath(xml), clampState);
    ApplyClampAttribsFromNode(xml, zoomScalePath, clampState);

    _thumbClamp.clampAlong = clampState.clampAlong;
    _thumbClamp.clampCross = clampState.clampCross;
}

void UIMapZoomScale::InitLabelTemplates(CUIXml& xml)
{
    const bool hasBoundTemplate = InitTickLabelTemplate(
        xml, "tick_label_bound", _boundStyle.xmlTemplate, _boundStyle.offset, _boundStyle.size);
    const bool hasValueTemplate = InitTickLabelTemplate(
        xml, "tick_label_value", _valueStyle.xmlTemplate, _valueStyle.offset, _valueStyle.size);

    if (!xml.NavigateToNode("tick_label", 0))
    {
        return;
    }

    _legacyLabelTemplate = new CUIStatic();
    CUIXmlInit::InitStatic(xml, "tick_label", 0, _legacyLabelTemplate);
    const Fvector2 legacyOffset = {
        xml.ReadAttribFlt("tick_label", 0, "x", 0.f),
        xml.ReadAttribFlt("tick_label", 0, "y", 0.f)};
    const Fvector2 legacySize = {_legacyLabelTemplate->GetWidth(), _legacyLabelTemplate->GetHeight()};

    if (!hasBoundTemplate)
    {
        _boundStyle.offset = legacyOffset;
        _boundStyle.size = legacySize;
    }

    if (!hasValueTemplate)
    {
        _valueStyle.offset = legacyOffset;
        _valueStyle.size = legacySize;
    }

    if (xml.NavigateToNode("tick_label_value", 0))
    {
        const int alignThumb = xml.ReadAttribInt("tick_label_value", 0, "align_thumb", -1);
        if (alignThumb >= 0)
        {
            _valueLabelConfig.alignToThumb = (alignThumb != 0);
        }

        const int alignBound = xml.ReadAttribInt("tick_label_value", 0, "align_bound", -1);
        if (alignBound >= 0)
        {
            _valueLabelConfig.alignToBound = (alignBound != 0);
        }
    }
}

void UIMapZoomScale::InitFromXml(CUIXml& xml, const char* path)
{
    CUIXmlInit::InitWindow(xml, path, 0, this);
    ReadConfigAttribs(xml, path);

    XML_NODE* storedRoot = xml.GetLocalRoot();
    XML_NODE* nodeRoot = xml.NavigateToNode(path, 0);
    xml.SetLocalRoot(nodeRoot);

    InitRailAndThumb(xml, path);
    InitLabelTemplates(xml);

    xml.SetLocalRoot(storedRoot);

    _isInitialized = (_rail != nullptr && _thumb != nullptr);
    _displayRatio = 1.f;
    _targetRatio = 1.f;

    if (_isInitialized)
    {
        EnsureLabels();
        if (_thumbScaleWithZoom)
        {
            UpdateThumb();
        }
    }
}

void UIMapZoomScale::SyncFromMap(float minZoom, float maxZoom, float currentZoom)
{
    if (!_isInitialized)
    {
        return;
    }

    const bool boundsChanged = !fsimilar(_minZoom, minZoom, EPS_L) || !fsimilar(_maxZoom, maxZoom, EPS_L);
    _minZoom = minZoom;
    _maxZoom = maxZoom;

    const float maxRatio = GetMaxRatio();
    const float targetRatio = fis_zero(_minZoom, EPS_L) ? 1.f : ClampRatio(currentZoom / _minZoom, maxRatio);

    if (boundsChanged)
    {
        EnsureLabels();
        UpdateBoundLabels();
        _displayRatio = targetRatio;
    }

    _targetRatio = targetRatio;

    if (_thumbScaleWithZoom)
    {
        UpdateThumb();
    }
}

void UIMapZoomScale::UpdateDisplayRatioSmoothing()
{
    if (fsimilar(_displayRatio, _targetRatio, EPS_L))
    {
        return;
    }

    const float diff = _targetRatio - _displayRatio;
    const float step = diff * (1.f - _motionConfig.inertion) * Device.fTimeDelta * _motionConfig.smoothingScale;

    if (fabsf(step) >= fabsf(diff))
    {
        _displayRatio = _targetRatio;
    }
    else
    {
        _displayRatio += step;
    }
}

void UIMapZoomScale::Update()
{
    if (!_isInitialized)
    {
        inherited::Update();
        return;
    }

    UpdateDisplayRatioSmoothing();
    UpdateThumb();
    inherited::Update();
    UpdateLabels();
}

float UIMapZoomScale::GetMaxRatio() const
{
    if (fis_zero(_minZoom, EPS_L))
    {
        return 1.f;
    }

    const float maxRatio = _maxZoom / _minZoom;
    if (maxRatio <= 1.f)
    {
        return 1.f;
    }

    return maxRatio;
}

float UIMapZoomScale::GetTrackNormalized(float ratio) const
{
    const float maxRatio = GetMaxRatio();
    ratio = ClampRatio(ratio, maxRatio);

    if (maxRatio <= 1.f)
    {
        return 0.f;
    }

    float trackT = Log2Positive(ratio) / Log2Positive(maxRatio);
    clamp(trackT, 0.f, 1.f);
    return trackT;
}

float UIMapZoomScale::GetRailAlongSize() const
{
    R_ASSERT(_rail);
    const ZoomScaleAxis axis = {_isHorizontal};
    return axis.alongSize(_rail->GetWndSize());
}

float UIMapZoomScale::GetRailCrossSize() const
{
    R_ASSERT(_rail);
    const ZoomScaleAxis axis = {_isHorizontal};
    return axis.crossSize(_rail->GetWndSize());
}

float UIMapZoomScale::RatioToAlongLocal(float ratio) const
{
    const float normalizedTrack = GetTrackNormalized(ratio);
    const float alongSize = GetRailAlongSize();
    const ZoomScaleAxis axis = {_isHorizontal};

    if (axis.isHorizontal)
    {
        return normalizedTrack * alongSize;
    }

    return (1.f - normalizedTrack) * alongSize;
}

float UIMapZoomScale::RatioToAlongParent(float ratio) const
{
    R_ASSERT(_rail);

    const ZoomScaleAxis axis = {_isHorizontal};
    return axis.along(_rail->GetWndPos()) + RatioToAlongLocal(ratio);
}

void UIMapZoomScale::GetRailLocalRect(Frect& out) const
{
    R_ASSERT(_rail);
    out.set(0.f, 0.f, _rail->GetWidth(), _rail->GetHeight());
}

void UIMapZoomScale::ApplyCrossAlignToCoord(
    float& crossPos,
    float crossSize,
    float parentCrossSize,
    EZoomCrossAlign align)
{
    switch (align)
    {
    case EZoomCrossAlign::Center:
        crossPos = (parentCrossSize - crossSize) * 0.5f;
        break;

    case EZoomCrossAlign::Right:
        crossPos = parentCrossSize - crossSize;
        break;

    case EZoomCrossAlign::Left:
    default:
        break;
    }
}

void UIMapZoomScale::ClampThumbToRail(Fvector2& thumbPos, Fvector2& size) const
{
    Frect railRect;
    GetRailLocalRect(railRect);

    const ZoomScaleAxis axis = {_isHorizontal};
    const float railAlong = axis.alongSize(Fvector2().set(railRect.width(), railRect.height()));
    const float railCross = axis.crossSize(Fvector2().set(railRect.width(), railRect.height()));

    if (_thumbClamp.clampAlong)
    {
        if (railAlong > 0.f)
        {
            axis.setAlong(size, clampr(axis.along(size), 0.f, railAlong));
        }
        const float alongPos = axis.along(thumbPos);
        const float alongMax = axis.along(Fvector2().set(railRect.x2, railRect.y2)) - axis.along(size);
        axis.setAlong(thumbPos, clampr(alongPos, axis.along(Fvector2().set(railRect.x1, railRect.y1)), alongMax));
    }

    if (_thumbClamp.clampCross)
    {
        if (railCross > 0.f)
        {
            axis.setCross(size, clampr(axis.cross(size), 0.f, railCross));
        }
        const float crossPos = axis.cross(thumbPos);
        const float crossMax = axis.cross(Fvector2().set(railRect.x2, railRect.y2)) - axis.cross(size);
        axis.setCross(thumbPos, clampr(crossPos, axis.cross(Fvector2().set(railRect.x1, railRect.y1)), crossMax));
    }
}

void UIMapZoomScale::ApplyRailAlignInParent()
{
    if (!_rail)
    {
        return;
    }

    Fvector2 pos = _rail->GetWndPos();
    const Fvector2 size = _rail->GetWndSize();
    const ZoomScaleAxis axis = {_isHorizontal};

    if (axis.isHorizontal)
    {
        ApplyCrossAlignToCoord(pos.y, size.y, GetHeight(), _railCrossAlign);
    }
    else
    {
        ApplyCrossAlignToCoord(pos.x, size.x, GetWidth(), _railCrossAlign);
    }

    _rail->SetWndPos(pos);
}

void UIMapZoomScale::ApplyCrossAlignThumb(
    Fvector2& thumbPos,
    const Fvector2& size,
    const Frect& railLocalRect) const
{
    const ZoomScaleAxis axis = {_isHorizontal};
    const float crossSize = axis.cross(size);
    const float parentCrossSize = axis.cross(Fvector2().set(railLocalRect.width(), railLocalRect.height()));
    float crossPos = axis.cross(thumbPos);

    ApplyCrossAlignToCoord(crossPos, crossSize, parentCrossSize, _thumbCrossAlign);
    axis.setCross(thumbPos, crossPos);
}

void UIMapZoomScale::ApplyThumbOffset(Fvector2& thumbPos) const
{
    thumbPos.x += _thumbOffset.x;
    thumbPos.y += _thumbOffset.y;
}

void UIMapZoomScale::ApplyThumbMarkerAlongCenter(Fvector2& thumbPos, const Fvector2& size) const
{
    const ZoomScaleAxis axis = {_isHorizontal};
    const float alongSize = GetRailAlongSize();
    const float trackAlong = RatioToAlongLocal(_displayRatio);
    const float centerAlong = clampr(trackAlong, 0.f, alongSize);
    const float halfAlong = axis.alongSize(size) * 0.5f;

    axis.setAlong(thumbPos, centerAlong - halfAlong);
}

void UIMapZoomScale::ClampThumbCrossToRail(Fvector2& thumbPos, const Fvector2& size) const
{
    if (!_thumbClamp.clampCross)
    {
        return;
    }

    Frect railRect;
    GetRailLocalRect(railRect);

    const ZoomScaleAxis axis = {_isHorizontal};
    const float railCross = axis.cross(Fvector2().set(railRect.width(), railRect.height()));

    if (railCross <= 0.f)
    {
        return;
    }

    const float crossSize = axis.cross(size);
    const float crossMin = axis.cross(Fvector2().set(railRect.x1, railRect.y1));
    const float crossMax = axis.cross(Fvector2().set(railRect.x2, railRect.y2)) - crossSize;
    axis.setCross(thumbPos, clampr(axis.cross(thumbPos), crossMin, crossMax));
}

UIMapZoomScale::ThumbLayout UIMapZoomScale::ComputeThumbFillLayout(float displayRatio) const
{
    ThumbLayout layout;
    layout.size = _thumbBaseSize;
    layout.pos.set(0.f, 0.f);

    const float trackPos = RatioToAlongLocal(displayRatio);
    const float alongSize = GetRailAlongSize();
    const float crossSize = GetRailCrossSize();
    const ZoomScaleAxis axis = {_isHorizontal};

    if (axis.isHorizontal)
    {
        const float fillLen = clampr(trackPos, 0.f, alongSize);
        layout.pos.x = 0.f;
        layout.size.x = (fillLen >= _thumbBaseSize.x) ? fillLen : _thumbBaseSize.x;
        layout.size.y = _thumbClamp.clampCross
            ? MinFloat(_thumbBaseSize.y, crossSize > 0.f ? crossSize : _thumbBaseSize.y)
            : _thumbBaseSize.y;
    }
    else
    {
        const float fillLen = clampr(alongSize - trackPos, 0.f, alongSize);
        layout.size.x = _thumbClamp.clampCross
            ? MinFloat(_thumbBaseSize.x, crossSize > 0.f ? crossSize : _thumbBaseSize.x)
            : _thumbBaseSize.x;
        if (fillLen >= _thumbBaseSize.y)
        {
            layout.size.y = fillLen;
            layout.pos.y = trackPos;
        }
        else
        {
            layout.size.y = _thumbBaseSize.y;
            layout.pos.y = alongSize - layout.size.y;
        }
    }

    return layout;
}

void UIMapZoomScale::ApplyThumbLayout(const ThumbLayout& layout)
{
    Fvector2 thumbPos = layout.pos;
    Fvector2 size = layout.size;

    Frect railLocalRect;
    GetRailLocalRect(railLocalRect);

    if (_thumbClamp.clampCross)
    {
        ApplyCrossAlignThumb(thumbPos, size, railLocalRect);
    }
    ApplyThumbOffset(thumbPos);

    if (!_thumbScaleWithZoom)
    {
        ApplyThumbMarkerAlongCenter(thumbPos, size);
        ClampThumbCrossToRail(thumbPos, size);
    }
    else
    {
        ClampThumbToRail(thumbPos, size);
    }

    _thumb->SetWndSize(size);
    _thumb->SetWndPos(thumbPos);
}

void UIMapZoomScale::UpdateThumb()
{
    if (!_thumb || !_rail)
    {
        return;
    }

    if (_thumbScaleWithZoom)
    {
        _thumb->SetStretchTexture(true);
        ApplyThumbLayout(ComputeThumbFillLayout(_displayRatio));
    }
    else
    {
        ThumbLayout layout;
        layout.pos.set(0.f, 0.f);
        layout.size = _thumb->GetWndSize();
        ApplyThumbLayout(layout);
    }
}

const CUIStatic* UIMapZoomScale::GetBoundLabelStyleTemplate() const
{
    if (_boundStyle.xmlTemplate)
    {
        return _boundStyle.xmlTemplate;
    }
    return _legacyLabelTemplate;
}

const CUIStatic* UIMapZoomScale::GetValueLabelStyleTemplate() const
{
    if (_valueStyle.xmlTemplate)
    {
        return _valueStyle.xmlTemplate;
    }
    return _legacyLabelTemplate;
}

void UIMapZoomScale::EnsureLabels()
{
    if (!_rail)
    {
        return;
    }

    const CUIStatic* boundStyleTemplate = GetBoundLabelStyleTemplate();
    const CUIStatic* valueStyleTemplate = GetValueLabelStyleTemplate();

    const auto attachLabelIfNullLambda = [this](
        CUIStatic*& slot, const CUIStatic* styleTemplate, const Fvector2& labelSize)
    {
        if (slot || !styleTemplate)
        {
            return;
        }
        slot = new CUIStatic();
        ApplyLabelStyle(slot, const_cast<CUIStatic*>(styleTemplate), labelSize);
        AttachChild(slot);
    };

    attachLabelIfNullLambda(_minLabel, boundStyleTemplate, _boundStyle.size);
    attachLabelIfNullLambda(_maxLabel, boundStyleTemplate, _boundStyle.size);
    attachLabelIfNullLambda(_valueLabel, valueStyleTemplate, _valueStyle.size);

    if (_valueLabel)
    {
        _valueLabelConfig.baseTextColor = _valueLabel->GetTextColor();
    }
}

void UIMapZoomScale::ApplyLabelStyle(
    CUIStatic* label,
    CUIStatic* styleTemplate,
    const Fvector2& labelSize) const
{
    if (!label || !styleTemplate)
    {
        return;
    }

    label->SetWndSize(labelSize);
    if (CGameFont* tickFont = styleTemplate->GetFont())
    {
        label->SetFont(tickFont);
    }
    CUILines* labelLines = label->TextItemControl();
    CUILines* styleLines = styleTemplate->TextItemControl();
    labelLines->SetTextAlignment(styleLines->GetTextAlignment());
    labelLines->SetVTextAlignment(styleLines->GetVTextAlignment());
    label->SetTextColor(styleLines->GetTextColor());
    label->Enable(false);
}

void UIMapZoomScale::FormatRatioLabel(string32& buffer, float ratio) const
{
    xr_sprintf(buffer, _labelConfig.labelFormat, ratio);
}

float UIMapZoomScale::GetThumbAlongCenterInParent() const
{
    const ZoomScaleAxis axis = {_isHorizontal};
    return axis.along(_rail->GetWndPos()) + axis.thumbCenterAlongLocal(_thumb->GetWndPos(), _thumb->GetWndSize());
}

float UIMapZoomScale::GetValueLabelAlongCenterLocal() const
{
    if (_valueLabelConfig.alignToThumb && _thumb)
    {
        const ZoomScaleAxis axis = {_isHorizontal};
        return axis.thumbCenterAlongLocal(_thumb->GetWndPos(), _thumb->GetWndSize());
    }

    return RatioToAlongLocal(_displayRatio);
}

float UIMapZoomScale::GetValueLabelEdgeFadeSize() const
{
    if (_valueLabelConfig.edgeFadeSize > 0.f)
    {
        return _valueLabelConfig.edgeFadeSize;
    }

    const ZoomScaleAxis axis = {_isHorizontal};
    float fadeSize = axis.alongSize(_valueStyle.size);
    const float boundSize = axis.alongSize(_boundStyle.size);
    if (boundSize > fadeSize)
    {
        fadeSize = boundSize;
    }

    return fadeSize;
}

float UIMapZoomScale::GetValueLabelRailEdgeFade() const
{
    if (!_valueLabelConfig.fadeAtRailEdges || !_valueLabel || !_rail)
    {
        return 1.f;
    }

    const float alongSize = GetRailAlongSize();
    if (alongSize <= EPS_L)
    {
        return 1.f;
    }

    const float fadeZone = GetValueLabelEdgeFadeSize();
    if (fadeZone <= EPS_L)
    {
        return 1.f;
    }

    const float alongLocal = GetValueLabelAlongCenterLocal();
    float fade = 1.f;

    if (alongLocal < fadeZone)
    {
        fade = Smoothstep01(alongLocal / fadeZone);
    }

    const float distFromMaxEdge = alongSize - alongLocal;
    if (distFromMaxEdge < fadeZone)
    {
        fade = MinFloat(fade, Smoothstep01(distFromMaxEdge / fadeZone));
    }

    return fade;
}

void UIMapZoomScale::ApplyValueLabelTextAlpha(float fade) const
{
    const u32 baseAlpha = color_get_A(_valueLabelConfig.baseTextColor);
    const u32 alpha = (u32)clampr(iFloor(float(baseAlpha) * fade), 0, 255);
    _valueLabel->SetTextColor(subst_alpha(_valueLabelConfig.baseTextColor, alpha));
}

Fvector2 UIMapZoomScale::GetValueLabelCrossOffset() const
{
    return _valueLabelConfig.alignToBound ? _boundStyle.offset : _valueStyle.offset;
}

void UIMapZoomScale::PlaceLabel(
    CUIStatic* label,
    float ratio,
    const Fvector2& crossOffset,
    bool alignToThumb) const
{
    const ZoomScaleAxis axis = {_isHorizontal};
    const Fvector2& railPos = _rail->GetWndPos();
    const float labelHalfAlong = axis.alongSize(label->GetWndSize()) * 0.5f;

    const float alongPos = (alignToThumb && _thumb)
        ? GetThumbAlongCenterInParent() - labelHalfAlong
        : RatioToAlongParent(ratio) - labelHalfAlong;

    if (axis.isHorizontal)
    {
        label->SetWndPos(Fvector2().set(alongPos, railPos.y + crossOffset.y));
    }
    else
    {
        label->SetWndPos(Fvector2().set(railPos.x + crossOffset.x, alongPos));
    }
}

void UIMapZoomScale::UpdateBoundLabels()
{
    if (!_rail || !_minLabel || !_maxLabel)
    {
        return;
    }

    const float maxRatio = GetMaxRatio();

    if (_labelConfig.boundLabelsMinMax)
    {
        _minLabel->SetText(g_pStringTable->translate(_labelConfig.boundLabelMinId.c_str()).c_str());
        _maxLabel->SetText(g_pStringTable->translate(_labelConfig.boundLabelMaxId.c_str()).c_str());
    }
    else
    {
        string32 labelText = {};

        FormatRatioLabel(labelText, 1.f);
        _minLabel->SetText(labelText);
        FormatRatioLabel(labelText, maxRatio);
        _maxLabel->SetText(labelText);
    }

    PlaceLabel(_minLabel, 1.f, _boundStyle.offset, false);
    PlaceLabel(_maxLabel, maxRatio, _boundStyle.offset, false);
}

void UIMapZoomScale::UpdateValueLabel()
{
    if (!_rail || !_valueLabel)
    {
        return;
    }

    string32 labelText = {};
    FormatRatioLabel(labelText, _displayRatio);
    _valueLabel->SetText(labelText);

    const Fvector2 crossOffset = GetValueLabelCrossOffset();
    PlaceLabel(_valueLabel, _displayRatio, crossOffset, _valueLabelConfig.alignToThumb);

    _valueLabel->Show(true);
    ApplyValueLabelTextAlpha(GetValueLabelRailEdgeFade());
}

void UIMapZoomScale::UpdateLabels()
{
    UpdateBoundLabels();
    UpdateValueLabel();
}
