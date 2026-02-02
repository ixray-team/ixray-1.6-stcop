#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrCore/_stl_extensions.h"
#include "../../xrCore/shared_string.h"
#include "../map_location_defs.h"

class CUIStatic;
class CUIXml;
class CMapLocation;

struct SCompassSpotConfig
{
    bool show = true;
    float offsetX = 0.0f;
    float offsetY = 0.0f;
    u8 align = 1;
    float spotWidth = 16.0f;
    float spotHeight = 16.0f;
    float maxDistance = -1.0f;
    int layer = 1;
};

enum class ECompassLayer : int
{
    UnderBackground = 0,
    OnStrip = 1,
    OverStrip = 2
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
    void updateStrip(float heading);
    void updateCardinals(float heading);
    void updateSpots(const Fvector& actorPos, float cameraHeading, const shared_str& levelName);
    void updateActiveTarget(const Fvector& actorPos, float cameraHeading, const shared_str& levelName);
    bool getSpotX(const Fvector2& targetPos, const Fvector2& actorPos, float cameraHeading, float& outX) const;
    bool getActiveTargetSpotX(const Fvector2& targetPos, const Fvector2& actorPos, float cameraHeading, float& outX) const;
    bool getCardinalX(float worldAngleRad, float heading, float& outX) const;
    bool computeRelativePercent(float targetYaw, float actorYaw, float fov, bool clampToEdges, float& outPercent) const;
    void parseSpots(CUIXml& uiXml, const char* path);
    void initCompassDial(CUIXml& uiXml, CUIXmlInit& xmlInit);
    CUIStatic* initCardinalStatic(CUIXml& uiXml, CUIXmlInit& xmlInit, const char* cardinalsPath, const char* directionNode, float defaultY, float defaultW, float defaultH);
    static u8 parseAlign(const char* alignStr);

    struct SCollectedSpot
    {
        CMapLocation* loc;
        Fvector worldPos;
    };

    struct SSpotLayoutInfo
    {
        CUIStatic* st;
        float x;
        float y;
        int layer;
    };

    void collectSpots(const Locations& locs, const shared_str& levelName, CMapLocation* activeTaskLoc,
        const Fvector& actorPos, xr_vector<SCollectedSpot>& out) const;
    void layoutSpots(const xr_vector<SCollectedSpot>& collected, const Fvector2& actorPos, float cameraHeading,
        float stripLeft, float stripTop, float stripCenterY, xr_vector<SSpotLayoutInfo>& out);
    void applySpotsToUI(const xr_vector<SSpotLayoutInfo>& spotsToShow, u32 poolUsedCount);

    static void detachAndDelete(CUIWindow* wnd);

    static constexpr u32 kMaxSpotPoolSize = 32u;
    static constexpr float kDefaultFovDeg = 120.0f;
    static constexpr float kPi = 3.14159265358979323846f;

    CUIStatic* _background;
    CUIStatic* _strip;
    CUIStatic* _cardinalN;
    CUIStatic* _cardinalE;
    CUIStatic* _cardinalS;
    CUIStatic* _cardinalW;
    CUIStatic* _activeDistanceText;
    CUIStatic* _activeTargetMarker;
    xr_vector<CUIStatic*> _spotPool;
    float _fov;
    float _stripWidth;
    float _stripTexWidth;
    float _stripY;
    float _markersY;
    float _distTextY;
    float _smoothSpeed;
    float _activeTargetPadding;
    float _activeBlockOffsetX;
    float _activeBlockOffsetY;
    float _textOffsetX;
    float _textOffsetY;
    float _curActiveX;
    CMapLocation* _lastActiveLoc;
    CMapLocation* _activeTargetLoc;
    SCompassSpotConfig _spotConfig;
};
