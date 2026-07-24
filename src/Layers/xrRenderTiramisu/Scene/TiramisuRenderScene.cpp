#include "TiramisuRenderScene.h"

#include "SceneProxy/TiramisuPrimitiveSceneProxy.h"

TiramisuRenderScene::TiramisuRenderScene()
{
    CheckIsGameThread();
}

TiramisuRenderScene::~TiramisuRenderScene()
{
    CheckIsGameThread();
    R_ASSERT(!IsRenderThreadRunning());
}

void TiramisuRenderScene::AddRenderSceneProxy(TiramisuPrimitiveSceneProxy* NewProxy)
{
    CheckIsGameThread();
    VERIFY(NewProxy);
    ENQUEUE_RENDER_COMMAND(TiramisuRenderScene::AddRenderSceneProxy)([this,NewProxy]
    {
        CheckIsRenderThread();
        RenderSceneProxies.push_back(NewProxy);
    });
    
}

void TiramisuRenderScene::RemoveRenderSceneProxy(TiramisuPrimitiveSceneProxy* InOutProxy)
{
    CheckIsGameThread();
    if (!InOutProxy)
        return;
    ENQUEUE_RENDER_COMMAND(TiramisuRenderScene::RemoveRenderSceneProxy)([InOutProxy]
    {
        CheckIsRenderThread();
        InOutProxy->bNeedRemove = true;
    });
}


void TiramisuRenderScene::Update()
{
    CheckIsRenderThread();
    
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
