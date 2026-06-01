#pragma once

class XRayTexturesManager;

class XRayTexture
{
public:
							XRayTexture             (const shared_str& InName = "None");
	virtual					~XRayTexture            ();
	virtual uint32_t		GetOrCreateHeapIndex    ();
	
	nri::Descriptor*        Descriptor = nullptr;
	nri::Texture*           Texture = nullptr;
	nri::TextureDesc        TextureDescription = {};
	shared_str              Name = "";
	uint32_t                Counter = 1;
	uint32_t                HeapIndex = INDEX_NONE;
	XRayTexturesManager*    Owner = nullptr;
};
