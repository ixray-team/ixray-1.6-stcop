#pragma once

#include "TiramisuRenderTypes.h"

class TiramisuPrimitiveSceneProxy;
// Render-thread сцена и владелец зарегистрированных primitive proxies.
class TiramisuRenderScene
{
public:
	TiramisuRenderScene();
	~TiramisuRenderScene();

	void AddRenderSceneProxy(TiramisuPrimitiveSceneProxy* NewProxy);
	void RemoveRenderSceneProxy(TiramisuPrimitiveSceneProxy* InProxy);
	void Update();

	xr_vector<TiramisuPrimitiveSceneProxy*> RenderSceneProxies;
};

template <typename T>
void RemoveRenderSceneProxy(T*& InProxy)
{
	if (InProxy)
	{
		GRenderResourcesManager->RenderScene->RemoveRenderSceneProxy(InProxy);
	}
	InProxy = nullptr;
}
