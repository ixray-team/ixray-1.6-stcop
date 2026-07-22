#pragma once
#include "../../xrCore/vector.h"
#include "../../xrCore/_stl_extensions.h"
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

struct SCompassBarRuntimeConfig
{
    float fovRad = 0.0f;
    float fadeInSpeed = 6.0f;
    float fadeOutSpeed = 5.0f;
    float minVisibleAlpha = 0.01f;
    float fovFadeInner = 0.30f;
    float fovFadeOuter = 0.70f;
    float fovFadeEdgeLo = 0.05f;
    float fovFadeEdgeHi = 0.95f;
    float activePadding = 8.0f;
    float smoothingSpeed = 10.0f;
    float activeOffsetY = 0.0f;
    float altitudeDeadzone = 1.8f;
    float cardinalFakeDistance = 1000.0f;
    shared_str distanceFormat;
};

struct SCompassSpotLayerConfig
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
    CMapLocation* sourceLoc = nullptr;
    Fvector pos;
    shared_str textureName;
    u32 color = 0;
    float offsetY = 0.0f;
    float offsetX = 0.0f;
    Fvector2 iconSize;
    EVTextAlignment valign = valCenter;
    float distance = 0.0f;
};

struct SCompassDirtyState
{
    float lastHeading = 0.0f;
    float lastStripU = -1.0e9f;
    float lastDistanceMeters = -1.0f;
    u32 lastCandidateHash = 0;
    u32 lastLogicFrame = u32(-1);
    bool spotsDirty = true;
    bool membershipChanged = true;
    bool layoutRefresh = true;
};

struct SCompassLayoutUnits
{
    Fvector2 barRelPos = Fvector2().set(0.0f, 0.0f);
    Fvector2 barRelSize = Fvector2().set(1.0f, 1.0f);
};

struct SCompassCardinalMarkerConfig
{
    shared_str texture;
    float width = 0.0f;
    float height = 0.0f;
    float offsetY = 1.0f;
    bool stretch = true;
};

struct SCompassCardinalEntry
{
    CUIWindow* host = nullptr;
    CUIStatic* text = nullptr;
    CUIStatic* marker = nullptr;
    Fvector3 layout = Fvector3().set(0.0f, 0.0f, 0.0f);
    Fvector2 dirXZ = Fvector2().set(0.0f, 0.0f);
    float alpha = 1.0f;
    float lastRelX = 0.0f;
    u32 baseTextColor = 0;
    u32 baseMarkerColor = 0;
    SCompassCardinalMarkerConfig markerCfg;
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
    void Show(bool status) override;

    bool IsInitialized() const { return _isInitialized; }

    CUIStatic& Background();
    CUIWindow* GetFrame();

    void SetActiveTarget(CMapLocation* loc);
    void Reset();

    bool GetHudVisible() const { return visible; }
    void SetHudVisible(bool status);

    bool visible = true;

private:
    static constexpr float _kDefaultFovDeg = 45.0f;
    static constexpr float _kDefaultStripTexWidth = 1024.0f;
    static constexpr float _kMinDistanceSq = 0.01f;
    static constexpr float _kDefaultFakeTargetDistance = 1000.0f;
    static constexpr float _kHalfCircleRad = 180.0f;
    static constexpr float _kTwoPiRad = 360.0f;
    static constexpr float _kDefaultCollectInterval = 0.1f;
    static constexpr float _kDefaultActivePadding = 8.0f;
    static constexpr float _kDefaultSmoothingSpeed = 10.0f;
    static constexpr float _kDefaultAltitudeDeadzone = 1.8f;
    static constexpr float _kDefaultFovFadeInner = 0.30f;
    static constexpr float _kDefaultFovFadeOuter = 0.70f;
    static constexpr float _kDefaultFovFadeEdgeLo = 0.05f;
    static constexpr float _kDefaultFovFadeEdgeHi = 0.95f;
    static constexpr float _kHeadingPixelEpsilon = 0.5f;
    static constexpr float _kAlphaSaturatedEpsilon = 0.001f;
    static constexpr u32 _kMaxCardinalPoints = 8;
    static constexpr u32 _kDefaultColorWhite = 0xFFFFFFFF;

    static const float _kCardinalAngles[_kMaxCardinalPoints];

    SCompassBarRuntimeConfig _runtimeCfg;
    SCompassSpotLayerConfig _spotCfg;
    SCompassDirtyState _dirty;
    SCompassLayoutUnits _layoutUnits;

    CUIStatic* _background;
    CUIWindow* _layerBg;
    CUIStatic* _strip;
    CUICompassClipWindow* _stripContainer;
    xr_vector<SCompassCardinalEntry> _cardinalEntries;
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
    xr_vector<CMapLocation*> _spotCollectScratch;
    xr_vector<SSpotRenderItem> _renderQueue;
    float _collectSpotsTimer;
    xr_vector<CUIStatic*> _poolSpots;
    xr_vector<CMapLocation*> _poolSpotOwners;
    xr_vector<shared_str> _poolSpotTextureNames;
    xr_vector<float> _poolSpotAlpha;
    xr_vector<u32> _poolSpotBaseColor;
    xr_vector<u8> _poolSlotUsed;
    xr_hash_map<CMapLocation*, u32> _spotSlotByLoc;

    shared_str _activeMarkerFallbackTexture;
    shared_str _activeMarkerLastTexture;

    float _stripWidth;
    float _stripTexWidth;
    bool _stripTexLoop;
    float _stripTextureScaleX;
    float _stripTextureScaleY;
    float _stripTextureOffsetX;
    float _stripTextureOffsetY;
    bool _stripTextureStretch;
    Frect _stripBaseTexRect;
    Fvector2 _stripNativeTexSize;
    Fvector2 _stripRelPos;
    Fvector2 _stripRelSize;

    bool _isInitialized;
    bool _isGameTypeSingleCompatible;
    size_t _fadeStorageSpotCount;
    mutable SCompassStripGeometry _cachedStripGeometry;
    mutable bool _stripGeometryCached;

    bool ProjectToStrip(const Fvector& targetPos, const Fvector& actorPos, float camHeading, float& outX,
        bool clampToEdges) const;

    void UpdateStrip(float heading);
    void UpdateCardinals(const SCompassFrameContext& ctx);
    void CollectSpotCandidates(const Fvector& actorPos, const shared_str& levelName);
    void BuildRenderQueueFromCandidates(float camHeading, const Fvector& actorPos);
    void CommitLayout(bool positionsOnly);
    void UpdateActiveTarget(const Fvector& actorPos, float camHeading, const shared_str& levelName);
    void UpdateSpotsLayout(float heading, const SCompassFrameContext& ctx);

    CUIStatic* GetSpotFromPool(xr_vector<CUIStatic*>& pool, CUIWindow* parent, u32 index);
    u32 AllocateSpotPoolSlot(CMapLocation* sourceLoc);
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
    u32 ComputeCandidateHash() const;
    void MarkSpotsDirty();
    bool IsHeadingPixelDirty(float heading) const;
    bool HasFadingSpots() const;

    void InitWindowAndBackground(CUIXml& uiXml, CUIXmlInit& xmlInit);
    void InitLayoutFromXml(CUIXml& uiXml);
    void ParseSpots(CUIXml& uiXml, const char* path);
    void InitCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit, CUIWindow* stripParent);
    void ParseCardinalMarkerConfig(CUIXml& uiXml, LPCSTR path, SCompassCardinalMarkerConfig& cfg) const;
    bool InitCardinalEntry(CUIXml& uiXml, CUIXmlInit& xmlInit, LPCSTR cardinalsPath, LPCSTR groupPath,
        LPCSTR directionNode, float defaultY, float defaultW, float defaultH,
        const SCompassCardinalMarkerConfig& defaultMarkerCfg);
    CUIStatic* CreateCardinalMarker(CUIXml& uiXml, const SCompassCardinalMarkerConfig& cfg, LPCSTR colorPath) const;
    float GetCardinalTextHeight(CUIStatic* textStatic);
    float GetCardinalTextBottom(SCompassCardinalEntry& entry);
    float GetCardinalTextCenterX(const SCompassCardinalEntry& entry) const;
    void ApplyCardinalMarkerLayout(SCompassCardinalEntry& entry);
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
