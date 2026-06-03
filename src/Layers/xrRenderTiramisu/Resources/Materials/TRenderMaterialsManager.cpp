#include "TRenderMaterialsManager.h"

TRenderMaterialsManager::TRenderMaterialsManager()
{
}

TRenderMaterialsManager::~TRenderMaterialsManager()
{
    VERIFY(Materials.empty());
}

TRenderMaterialInterface* TRenderMaterialsManager::Get(const shared_str& InName)
{
    auto Iterator = Materials.find(InName);
    if (Iterator != Materials.end())
    {
        Iterator->second->Counter++;
    }
    else
    {
        Iterator = Materials.emplace(InName, new TRenderMaterial).first;
        Iterator->second->Name = InName;
    }
    return Iterator->second;
}

TRenderMaterialInstanceDynamic* TRenderMaterialsManager::CreateInstanceDynamic(const shared_str& InName, TRenderMaterialInterface* Parent)
{
    VERIFY (!Materials.contains(InName));
    VERIFY(Parent);
    
    TRenderMaterialInstanceDynamic* NewInstanceDynamic= new TRenderMaterialInstanceDynamic(Copy(Parent));
    Materials.emplace(InName, NewInstanceDynamic);
    return NewInstanceDynamic;
}

void TRenderMaterialsManager::Free(TRenderMaterialInterface* Material)
{
    if (!Material)
    {
        return;
    }
    
    if (--Material->Counter == 0)
    {
        Materials.erase(Material->Name);
        delete Material;
    }
}

TRenderMaterialInterface* TRenderMaterialsManager::Copy(TRenderMaterialInterface* Material)
{
    Material->Counter++;
    return Material;
}