#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrCore/_stl_extensions.h"
#include "../../xrCore/shared_string.h"
#include "../map_location_defs.h"

class CUIStatic;
class CUIXml;
class CMapLocation;

struct SSpotRenderItem
{
    float relX;
    float sortDist;
    float offsetY;
    shared_str textureName;
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
    float spotWidth = 14.0f;
    float spotHeight = 17.0f;
    float maxDistance = -1.0f;
    float collectInterval = 0.1f;
};

struct SSpotCandidate
{
    Fvector pos;
    shared_str textureName;
    u32 color;
    float offsetY;
    Fvector2 iconSize;
};

class CUICompassBar final : public CUIWindow
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
    struct
    {
        float stripY;
        float markersY;
        float activePadding;
        float smoothingSpeed;
        float distY;
        float activeOffsetX;
        float activeOffsetY;
        float textOffsetX;
        float textOffsetY;
    } _cfg;

    SCompassSpotConfig _spotCfg;
    CUIStatic* _background;
    CUIWindow* _layerBg;
    CUIStatic* _strip;
    xr_vector<CUIStatic*> _cardinals;
    CUIWindow* _layerFg;
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
    shared_str _stripXmlPath;
    shared_str _stripSvgPath;

    bool ProjectToStrip(const Fvector& targetPos, const Fvector& actorPos, float camHeading,
        float& outX, bool clampToEdges) const;

    void UpdateStrip(float heading);
    void UpdateCardinals(float heading);
    void CollectSpotCandidates(const Fvector& actorPos, const shared_str& levelName);
    void BuildRenderQueueFromCandidates(float camHeading, const Fvector& actorPos);
    void CommitLayout();
    void UpdateActiveTarget(const Fvector& actorPos, float camHeading, const shared_str& levelName);

    CUIStatic* GetSpotFromPool(xr_vector<CUIStatic*>& pool, CUIWindow* parent, size_t index);

    bool BuildFrameContext(SCompassFrameContext& out) const;
    SCompassStripGeometry GetStripGeometry() const;

    void InitWindowAndBackground(CUIXml& uiXml, CUIXmlInit& xmlInit);
    void InitLayoutFromXml(CUIXml& uiXml);
    void ParseSpots(CUIXml& uiXml, const char* path);
    void InitCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit);
    CUIStatic* InitCardinalStatic(CUIXml& uiXml, CUIXmlInit& xmlInit, const char* cardinalsPath,
        const char* directionNode, float defaultY, float defaultW, float defaultH);
    void InitActiveTargetWidgets(CUIXml& uiXml);
    void InitStripVectorIcon(CUIXml& uiXml);
    static u8 ParseAlign(const char* alignStr);

    static constexpr float _kPi = 3.14159265358979323846f;
    static constexpr float _kDefaultFovDeg = 120.0f;
    static constexpr float _kDefaultStripTexWidth = 1024.0f;
};

