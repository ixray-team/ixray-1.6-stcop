#pragma once
#include "../../xrCore/vector.h"
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/UIXmlInit.h"
#include "../map_location_defs.h"

class CUIStatic;
class CUIXml;
class CMapLocation;

enum class EUILayoutUnits : u8
{
    Auto,
    Relative,
    Px
};

enum class EVAlign : u8
{
    Top = 0,
    Center,
    Bottom
};

struct SCompassSpotParams
{
    Fvector2 size;
    EVAlign valign;
    float offsetY;
    float offsetX;
    float maxDist;

    SCompassSpotParams()
        : size(8.f, 8.f), valign(EVAlign::Center), offsetY(0.f), offsetX(0.f), maxDist(-1.f)
    {
    }
};

struct SSpotRenderItem
{
    float relX;
    float sortDist;
    float offsetY;
    EVAlign valign;
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
    u8 align = 1;
    float spotWidth = 0.0f;
    float spotHeight = 0.0f;
    float maxDistance = -1.0f;
    float collectInterval = 0.1f;
    u32 defaultSpotColor = 0;
};

struct SSpotCandidate
{
    Fvector pos;
    shared_str textureName;
    u32 color;
    float offsetY;
    float offsetX;
    Fvector2 iconSize;
    EVAlign valign;
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
    static constexpr u32 _kMaxCardinalPoints = 8;
    static constexpr u32 _kDefaultColorWhite = 0xFFFFFFFF;

    static const float _kCardinalAngles[_kMaxCardinalPoints];

    struct
    {
        float markersY;
        float activePadding;
        float smoothingSpeed;
        float activeOffsetY;
    } _cfg;

    SCompassSpotConfig _spotCfg;
    xr_map<shared_str, SCompassSpotParams> _spotConfigs;
    SCompassSpotParams _defaultSpotConfig;

    CUIStatic* _background;
    CUIWindow* _layerBg;
    CUIStatic* _strip;
    CUICompassClipWindow* _stripContainer;
    EUILayoutUnits _stripUnits;
    EUILayoutUnits _cardinalsUnits;
    xr_vector<Fvector3> _cardinalLayout;
    xr_vector<CUIStatic*> _cardinals;
    CUIWindow* _layerFg;
    CUIWindow* _activeTargetContainer;
    CUIStatic* _activeMarker;
    CUIStatic* _activeDistText;

    CMapLocation* _activeTargetLoc;
    CMapLocation* _lastActiveLoc;
    float _activeTargetCurX;

    xr_vector<SSpotCandidate> _spotCandidates;
    xr_vector<SSpotRenderItem> _renderQueue;
    float _collectSpotsTimer;
    xr_vector<CUIStatic*> _poolSpots;
    xr_vector<shared_str> _poolSpotTextureNames;

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
    /** If >= 0, texture height in UI units (pixels); otherwise use scale. */
    float _stripTextureHeightPx;
    /** If >= 0, texture width in UI units (pixels); otherwise use scale. */
    float _stripTextureWidthPx;

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
    float GetSpotMaxDistance(const SCompassSpotParams& params, CMapLocation* loc) const;
    SSpotCandidate CreateSpotCandidate(CMapLocation* loc, const SCompassSpotParams& params) const;

    void CalculateActiveTargetPosition(const Fvector& actorPos, float camHeading, const Fvector& tgtPos,
        float& outX) const;
    void UpdateActiveTargetMarker(CMapLocation* activeLoc);
    void UpdateActiveTargetText(const Fvector& actorPos, const Fvector& tgtPos);

    bool BuildFrameContext(SCompassFrameContext& out) const;
    SCompassStripGeometry GetStripGeometry() const;
    void InvalidateStripGeometry();

    void InitWindowAndBackground(CUIXml& uiXml, CUIXmlInit& xmlInit);
    void InitLayoutFromXml(CUIXml& uiXml);
    void ParseSpots(CUIXml& uiXml, const char* path);
    void ParseSpotType(CUIXml& uiXml, tinyxml2::XMLElement* child, const SCompassSpotParams& defaultParams);
    void InitCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit);
    CUIStatic* InitCardinalStatic(CUIXml& uiXml, CUIXmlInit& xmlInit, const char* cardinalsPath, const char* groupPath,
        const char* directionNode, float defaultY, float defaultW, float defaultH, EUILayoutUnits units,
        xr_vector<Fvector3>* outLayout);
    void InitActiveTargetWidgets(CUIXml& uiXml, CUIXmlInit& xmlInit);
    void CreateDefaultActiveTargetWidgets(CUIXml& uiXml);

    void ApplyRelativeLayout();
    void ApplyMainWindowLayout();
    void ApplyLayerLayouts();
    void ApplyStripLayout();
    void ApplyCardinalsLayout();

    static u8 ParseAlign(const char* alignStr);
    void CacheGameTypeCompatibility();
};
