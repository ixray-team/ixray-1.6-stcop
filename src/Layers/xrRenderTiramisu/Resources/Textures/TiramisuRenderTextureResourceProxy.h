#pragma once

#include "TiramisuRenderTypes.h"
#include "Resources/TiramisuRenderResourceProxy.h"

// Render-thread GPU-представление texture и bindless descriptor.
class TiramisuRenderTextureResourceProxy : public TiramisuRenderResourceProxy
{
public:
	~TiramisuRenderTextureResourceProxy() override;
	nri::Texture* Texture = nullptr;
	nri::TextureDesc TextureDescription = {};
};
