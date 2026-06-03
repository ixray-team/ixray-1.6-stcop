#pragma once

class TPrimitiveSceneProxy;
class TRenderScene
{
public:
                                        TRenderScene                    ();
                                        ~TRenderScene                   ();
                            
        void                            AddRenderSceneProxy            (TPrimitiveSceneProxy* NewProxy);
        void                            RemoveRenderSceneProxy         (TPrimitiveSceneProxy* InProxy);
        void                            Update                         ();
    
    xr_vector<TPrimitiveSceneProxy*>    RenderSceneProxies;
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
