#pragma once

#include "../../xrUI/Widgets/UIWindow.h"

class CUIXml;
class CUIStatic;

// Map zoom slider widget; owned by CUIMapWnd when zoom_scale is present in map XML.
class UIMapZoomScale final : public CUIWindow
{
private:
    using inherited = CUIWindow;

    enum class EZoomCrossAlign : u8
    {
        Left = 0,
        Center = 1,
        Right = 2,
    };

    struct ZoomScaleMotionConfig final
    {
        float inertion = 0.85f;
        float smoothingScale = 20.f;
    };

    struct ZoomScaleLabelConfig final
    {
        string64 labelFormat = {};
        shared_str boundLabelMinId;
        shared_str boundLabelMaxId;
        bool boundLabelsMinMax = false;
    };

    struct ZoomScaleLabelStyle final
    {
        CUIStatic* xmlTemplate = nullptr;
        Fvector2 offset = { 0.f, 0.f };
        Fvector2 size = { 0.f, 0.f };
    };

    struct ZoomScaleValueLabelConfig final
    {
        bool alignToThumb = false;
        bool alignToBound = false;
        bool fadeAtRailEdges = true;
        float edgeFadeSize = 0.f;
        u32 baseTextColor = 0xffffffff;
    };

    struct ZoomScaleThumbClamp final
    {
        bool clampAlong = true;
        bool clampCross = true;
    };

    struct ThumbLayout final
    {
        Fvector2 pos = { 0.f, 0.f };
        Fvector2 size = { 0.f, 0.f };
    };

public:
    UIMapZoomScale() = default;
    ~UIMapZoomScale() override;

    void InitFromXml(CUIXml& xml, const char* path);
    void SyncFromMap(float minZoom, float maxZoom, float currentZoom);
    void Update() override;

private:
    void ReadConfigAttribs(CUIXml& xml, const char* path);
    void InitRailAndThumb(CUIXml& xml, const char* path);
    void InitLabelTemplates(CUIXml& xml);
    void InitThumbClampAttribs(CUIXml& xml, const char* zoomScalePath);

    void EnsureLabels();
    void ApplyLabelStyle(CUIStatic* label, CUIStatic* styleTemplate, const Fvector2& labelSize) const;

    void UpdateDisplayRatioSmoothing();
    void UpdateThumb();
    void UpdateLabels();
    void UpdateBoundLabels();
    void UpdateValueLabel();

    void PlaceLabel(CUIStatic* label, float ratio, const Fvector2& crossOffset, bool alignToThumb) const;
    void FormatRatioLabel(string32& buffer, float ratio) const;

    float GetMaxRatio() const;
    float GetTrackNormalized(float ratio) const;
    float RatioToAlongLocal(float ratio) const;
    float RatioToAlongParent(float ratio) const;
    float GetRailAlongSize() const;
    float GetRailCrossSize() const;
    void GetRailLocalRect(Frect& out) const;

    float GetThumbAlongCenterInParent() const;
    float GetValueLabelAlongCenterLocal() const;
    float GetValueLabelRailEdgeFade() const;
    float GetValueLabelEdgeFadeSize() const;
    void ApplyValueLabelTextAlpha(float fade) const;

    static void ApplyCrossAlignToCoord(
        float& crossPos,
        float crossSize,
        float parentCrossSize,
        EZoomCrossAlign align);

    void ApplyRailAlignInParent();
    void ApplyCrossAlignThumb(Fvector2& thumbPos, const Fvector2& size, const Frect& railLocalRect) const;
    void ClampThumbToRail(Fvector2& thumbPos, Fvector2& size) const;
    void ClampThumbCrossToRail(Fvector2& thumbPos, const Fvector2& size) const;
    void ApplyThumbOffset(Fvector2& thumbPos) const;
    void ApplyThumbMarkerAlongCenter(Fvector2& thumbPos, const Fvector2& size) const;

    ThumbLayout ComputeThumbFillLayout(float displayRatio) const;
    void ApplyThumbLayout(const ThumbLayout& layout);

    const CUIStatic* GetBoundLabelStyleTemplate() const;
    const CUIStatic* GetValueLabelStyleTemplate() const;
    Fvector2 GetValueLabelCrossOffset() const;

    bool _isHorizontal = false;
    bool _thumbScaleWithZoom = false;
    bool _isInitialized = false;

    ZoomScaleMotionConfig _motionConfig;
    ZoomScaleLabelConfig _labelConfig;
    ZoomScaleValueLabelConfig _valueLabelConfig;
    ZoomScaleThumbClamp _thumbClamp;
    ZoomScaleLabelStyle _boundStyle;
    ZoomScaleLabelStyle _valueStyle;

    CUIStatic* _rail = nullptr;
    CUIStatic* _thumb = nullptr;
    CUIStatic* _minLabel = nullptr;
    CUIStatic* _maxLabel = nullptr;
    CUIStatic* _valueLabel = nullptr;
    CUIStatic* _legacyLabelTemplate = nullptr;

    float _minZoom = 1.f;
    float _maxZoom = 1.f;
    float _displayRatio = 1.f;
    float _targetRatio = 1.f;
    Fvector2 _thumbBaseSize = { 0.f, 0.f };
    Fvector2 _thumbOffset = { 0.f, 0.f };
    EZoomCrossAlign _railCrossAlign = EZoomCrossAlign::Left;
    EZoomCrossAlign _thumbCrossAlign = EZoomCrossAlign::Left;
};
