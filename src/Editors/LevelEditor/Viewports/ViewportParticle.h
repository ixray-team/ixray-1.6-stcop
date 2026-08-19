#pragma once
#include "IViewport.h"
#include "../../../Include/xrRender/EditorRenderer.h"

class CViewportParticle :
    public IViewport
{
public:
    CViewportParticle();
    virtual ~CViewportParticle();

    virtual void Draw() override;
    virtual void Render() override;
	void RenderTiramisu() override;
	void OpenModel(
		xr_string_view AssetName,
		EEditorParticleAssetType AssetType
	);

private:
    UIRenderForm View;

	xr_string ParticleAssetName;
	EEditorParticleAssetType ParticleAssetType =
		EEditorParticleAssetType::Effect;
	u64 SceneRevision = 0;
};
