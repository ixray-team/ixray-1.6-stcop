#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuRenderTextureResourceProxy.h"

class TiramisuRenderTexturesManager;

// Базовый runtime texture asset, владеющий render proxy.
class TiramisuRenderTexture
{
public:
	TiramisuRenderTexture(const shared_str& InName = "None");
	virtual ~TiramisuRenderTexture();
	virtual bool IsDynamic() { return false; };
	virtual void Update();

	TiramisuRenderTextureResourceProxy* ResourceProxy = nullptr;
	nri::TextureDesc TextureDescription = {};
	shared_str Name = "";
	u32 Counter = 1;
	TiramisuRenderTexturesManager* Owner = nullptr;
};
