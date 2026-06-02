#pragma once
#include "XRayTexture2D.h"


class XRayTextureSeq final : public XRayTexture
{
public:
								XRayTextureSeq			(const shared_str& InName = "None") : XRayTexture(InName) {};
	virtual 					~XRayTextureSeq			() override;
	virtual u32					GetOrCreateHeapIndex	() override;
			bool				LoadFromSeqFile			(const char* FilePath);
	virtual void				Update					() override;
	virtual bool				IsDynamic				() override {return true;};

	// Sequence settings
			void				SetCycles				(bool InCycles) { bCycles = InCycles; }
			u32					GetFPS					() const { return FPS; }
			u32					GetFrameCount			() const { return static_cast<u32>(FrameTextures.size()); }

private:
			bool				LoadFrameTexture		(const char* TextureName);

private:

	xr_vector<XRayTexture* >	 FrameTextures;
	u32							MSPF = 0;       
	u32							FPS = 0;        
	bool						bCycles = false;

	// Current frame
	u32							CurrentFrame = 0;
	u32							LastUpdateTime = 0;
};