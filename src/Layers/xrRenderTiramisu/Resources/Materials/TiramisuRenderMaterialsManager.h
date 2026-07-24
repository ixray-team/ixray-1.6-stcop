#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuRenderMaterial.h"
#include "TiramisuRenderMaterialInstanceDynamic.h"
#include <LegacyMaterialResolver.h>
#include <MaterialAsset.h>

// Загружает material assets и создаёт render-thread proxies.
class TiramisuRenderMaterialsManager
{
public:
	TiramisuRenderMaterialsManager();
	~TiramisuRenderMaterialsManager();

	// Загружает material либо создаёт dynamic/legacy adapter с единым render proxy contract.
	TiramisuRenderMaterialInterface* Get(const shared_str& InName);
	TiramisuRenderMaterialInstanceDynamic* CreateInstanceDynamic(const shared_str& InName, TiramisuRenderMaterialInterface* Parent);
	TiramisuRenderMaterialInstanceDynamic* CreateLegacyInstanceDynamic(const shared_str& InName, const shared_str& ShaderName, const xr_vector<shared_str>& TextureNames);
	// Освобождает runtime-ссылку; GPU proxy уничтожается отложенно в render thread.
	void Free(TiramisuRenderMaterialInterface* Material);
	[[nodiscard]] const FResolvedMaterialInstance*
	ResolveSourceMaterial_RenderThread(const FMaterialAssetId& MaterialId) const;
	[[nodiscard]] const FMaterialAsset*
	ResolveSourceMaster_RenderThread(const FMaterialAssetId& MaterialId) const;
	TiramisuRenderMaterialInterface* Copy(TiramisuRenderMaterialInterface* Material);

private:
	void LoadSourceMaterialAssets();
	void LoadLegacyMaterialMap();

	xr_map<shared_str, TiramisuRenderMaterialInterface*> Materials;
	FLegacyMaterialMap LegacyMaterialMap;
	TiramisuMaterialLibrary SourceMaterialLibrary;
	xr_map<FMaterialAssetId, FResolvedMaterialInstance> SourceResolvedMaterials;
};
