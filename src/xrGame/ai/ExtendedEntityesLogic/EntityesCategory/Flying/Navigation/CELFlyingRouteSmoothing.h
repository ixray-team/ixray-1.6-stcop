#pragma once
#include "../../../Interfaces/IELNavigation.h"

class CELFlyingRouteSmoothing final
{
public:
	static void Compact(SELNavigationRequest& Request, xr_vector<Fvector>& Points);

private:
	CELFlyingRouteSmoothing() = delete;
};
