#pragma once
#include "../../xrCore/vector.h"
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/uiabstract.h"
#include "../../xrUI/ui_defs.h"
#include "../map_location_defs.h"

class CUIStatic;
class CUIXml;
class CMapLocation;

struct SSpotRenderItem
{
    CMapLocation* sourceLoc;
    float relX;
    float sortDist;
    float offsetY;
    EVTextAlignment valign;
    const shared_str* textureName;
    Fvector2 iconSize;
    u32 color;

    bool operator<(const SSpotRenderItem& other) const
    {
        return sortDist > other.sortDist;
    }
};

struct SCompassStripGeometry
{
    float left = 0.0f;
    float top = 0.0f;
    float width = 0.0f;
    float height = 0.0f;

    float CenterX() const
    {
        return width * 0.5f;
    }

    float CenterY() const
    {
        return height * 0.5f;
    }
};

struct SCompassFrameContext
{
    Fvector actorPos;
    float heading = 0.0f;
    shared_str levelName;
    bool isValid = false;
};

struct SCompassSpotConfig
{
    bool show = true;
    float offsetX = 0.0f;
    float offsetY = 0.0f;
    EUIItemAlign align = alCenter;
    float spotWidth = 0.0f;
    float spotHeight = 0.0f;
    float collectInterval = 0.1f;
    u32 defaultSpotColor = 0;
};

struct SSpotCandidate
{
    CMapLocation* sourceLoc;
    Fvector pos;
    shared_str textureName;
    u32 color;
    float offsetY;
    float offsetX;
    Fvector2 iconSize;
    EVTextAlignment valign;
};

class CUICompassClipWindow final :
    public CUIWindow
{
    using inherited = CUIWindow;

public:
    void Draw() override;
};

class CUICompassBar final :
    public CUIWindow
{
    using inherited = CUIWindow;

public:
    CUICompassBar();
    ~CUICompassBar() override;

    void Init();
    void Draw() override;
    void Update() override;

    CUIStatic& Background();
    CUIWindow* GetFrame();

    void SetActiveTarget(CMapLocation* loc);
    void Reset();

    bool visible = true;

private:
    static constexpr float _kDefaultFovDeg = 120.0f;
    static constexpr float _kDefaultStripTexWidth = 1024.0f;
    static constexpr float _kMinDistanceSq = 0.01f;
    static constexpr float _kFakeTargetDistance = 1000.0f;
    static constexpr float _kHalfCircleRad = 180.0f;
    static constexpr float _kTwoPiRad = 360.0f;
    static constexpr float _kDefaultCollectInterval = 0.1f;
    static constexpr float _kDefaultActivePadding = 8.0f;
    static constexpr float _kDefaultSmoothingSpeed = 10.0f;
    static constexpr float _kDefaultAltitudeDeadzone = 1.8f;
    static constexpr u32 _kMaxCardinalPoints = 8;
    static constexpr u32 _kDefaultColorWhite = 0xFFFFFFFF;

    static const float _kCardinalAngles[_kMaxCardinalPoints];

    struct
    {
        float activePadding;
        float smoothingSpeed;
        float activeOffsetY;
        float altitudeDeadzone;
        float fadeInSpeed;
        float fadeOutSpeed;
        float minVisibleAlpha;
    } _cfg;

    SCompassSpotConfig _spotCfg;

    CUIStatic* _background;
    CUIWindow* _layerBg;
    CUIStatic* _strip;
    CUICompassClipWindow* _stripContainer;
    xr_vector<Fvector3> _cardinalLayout;
    xr_vector<CUIStatic*> _cardinals;
    xr_vector<float> _cardinalAlpha;
    xr_vector<u32> _cardinalBaseTextColor;
    CUIWindow* _layerFg;
    CUIWindow* _activeTargetContainer;
    CUIStatic* _activeAltitudeArrow;
    CUIStatic* _activeMarker;
    CUIStatic* _activeDistText;

    shared_str _altitudeArrowTextureUp;
    shared_str _altitudeArrowTextureDown;
    shared_str _altitudeArrowLastTexture;

    CMapLocation* _activeTargetLoc;
    CMapLocation* _lastActiveLoc;
    float _activeTargetCurX;

    xr_vector<SSpotCandidate> _spotCandidates;
    xr_vector<SSpotRenderItem> _renderQueue;
    float _collectSpotsTimer;
    xr_vector<CUIStatic*> _poolSpots;
    xr_vector<CMapLocation*> _poolSpotOwners;
    xr_vector<shared_str> _poolSpotTextureNames;
    xr_vector<float> _poolSpotAlpha;
    xr_vector<u32> _poolSpotBaseColor;

    shared_str _activeMarkerFallbackTexture;
    shared_str _activeMarkerLastTexture;

    float _fov;
    float _stripWidth;
    float _stripTexWidth;
    bool _stripTexLoop;
    float _stripTextureScaleX;
    float _stripTextureScaleY;
    float _stripTextureOffsetX;
    float _stripTextureOffsetY;

    bool _isGameTypeSingleCompatible;
    mutable SCompassStripGeometry _cachedStripGeometry;
    mutable bool _stripGeometryCached;

    bool ProjectToStrip(const Fvector& targetPos, const Fvector& actorPos, float camHeading, float& outX,
        bool clampToEdges) const;

    void UpdateStrip(float heading);
    void UpdateCardinals(float heading);
    void CollectSpotCandidates(const Fvector& actorPos, const shared_str& levelName);
    void BuildRenderQueueFromCandidates(float camHeading, const Fvector& actorPos);
    void CommitLayout();
    void UpdateActiveTarget(const Fvector& actorPos, float camHeading, const shared_str& levelName);

    CUIStatic* GetSpotFromPool(xr_vector<CUIStatic*>& pool, CUIWindow* parent, u32 index);
    bool ShouldShowSpot(CMapLocation* loc, const Fvector& actorPos, const shared_str& levelName,
        CMapLocation* activeTaskLoc) const;
    SSpotCandidate CreateSpotCandidate(CMapLocation* loc) const;

    void CalculateActiveTargetPosition(const Fvector& actorPos, float camHeading, const Fvector& tgtPos,
        float& outX) const;
    void UpdateActiveTargetMarker(CMapLocation* activeLoc);
    void UpdateActiveTargetText(const Fvector& actorPos, const Fvector& tgtPos);
    void UpdateActiveAltitudeArrow(const Fvector& actorPos, const Fvector& tgtPos);

    bool BuildFrameContext(SCompassFrameContext& out) const;
    SCompassStripGeometry GetStripGeometry() const;
    void InvalidateStripGeometry();

    void InitWindowAndBackground(CUIXml& uiXml, CUIXmlInit& xmlInit);
    void InitLayoutFromXml(CUIXml& uiXml);
    void ParseSpots(CUIXml& uiXml, const char* path);
    void InitCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit);
    CUIStatic* InitCardinalStatic(CUIXml& uiXml, CUIXmlInit& xmlInit, const char* cardinalsPath, const char* groupPath,
        const char* directionNode, float defaultY, float defaultW, float defaultH, xr_vector<Fvector3>* outLayout);
    void InitActiveTargetWidgets(CUIXml& uiXml, CUIXmlInit& xmlInit);
    void CreateDefaultActiveTargetWidgets(CUIXml& uiXml);

    void ApplyRelativeLayout();
    void ApplyMainWindowLayout();
    void ApplyLayerLayouts();
    void ApplyStripLayout();
    void ApplyCardinalsLayout();

    static EUIItemAlign ParseAlign(const char* alignStr);
    float UpdateFadeAlpha(float alpha, bool isVisible, float fadeInSpeed, float fadeOutSpeed) const;
    float CalculateFovEdgeFade(float relX, float stripWidth) const;
    void EnsureFadeStorage();
    void CacheGameTypeCompatibility();
};
