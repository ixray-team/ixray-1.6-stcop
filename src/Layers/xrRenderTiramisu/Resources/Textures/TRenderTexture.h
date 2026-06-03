#pragma once
#include "TRenderTextureResourceProxy.h"

class TRenderTexturesManager;

class TRenderTexture
{
public:
									TRenderTexture          (const shared_str& InName = "None");
	virtual							~TRenderTexture         ();
	virtual bool					IsDynamic				() {return false;};
	virtual void					Update					();
	
	TRenderTextureResourceProxy*	ResourceProxy = nullptr;
	nri::TextureDesc        		TextureDescription = {};
	shared_str              		Name = "";
	uint32_t                		Counter = 1;
	TRenderTexturesManager* 		Owner = nullptr;
};
