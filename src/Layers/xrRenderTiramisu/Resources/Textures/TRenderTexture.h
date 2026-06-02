#pragma once

class TRenderTexturesManager;

class TRenderTexture
{
public:
							TRenderTexture          (const shared_str& InName = "None");
	virtual					~TRenderTexture         ();
	virtual u32				GetOrCreateHeapIndex    ();
	virtual bool			IsDynamic				() {return false;};
	virtual void			Update					();
	
	nri::Descriptor*        Descriptor = nullptr;
	nri::Texture*           Texture = nullptr;
	nri::TextureDesc        TextureDescription = {};
	shared_str              Name = "";
	uint32_t                Counter = 1;
	uint32_t                HeapIndex = INDEX_NONE;
	TRenderTexturesManager*    Owner = nullptr;
};
