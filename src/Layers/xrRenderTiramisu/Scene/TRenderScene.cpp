#include "TRenderScene.h"

#include "SceneProxy/TPrimitiveSceneProxy.h"

TRenderScene::TRenderScene()
{
}

TRenderScene::~TRenderScene()
{
}

void TRenderScene::AddRenderSceneProxy(TPrimitiveSceneProxy* NewProxy)
{
    RenderSceneProxies.push_back(NewProxy);
}

void TRenderScene::RemoveRenderSceneProxy(TPrimitiveSceneProxy* InOutProxy)
{
    InOutProxy->bNeedRemove = true;
}


void TRenderScene::Update()
{
    u32 RemoveCounter = 0;
    u32 Counter = RenderSceneProxies.size();
    for (u32 i = 0 ;i < Counter;)
    {
        if (RenderSceneProxies[i]->bNeedRemove)
        {
            xr_delete(RenderSceneProxies[i]);
            
            RenderSceneProxies[i] = RenderSceneProxies[Counter - 1];
            
            Counter--;
            RemoveCounter++;
        }
        else
        {
            i++;
        }
    }
    
    if (RemoveCounter)
    {    
        RenderSceneProxies.erase(RenderSceneProxies.begin() + RemoveCounter + Counter, RenderSceneProxies.end());
    }
}
