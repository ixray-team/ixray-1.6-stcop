#pragma once
#include "TRenderTexture2D.h"


class TRenderTextureSeq final : public TRenderTexture
{
public:
								TRenderTextureSeq		(const shared_str& InName = "None") : TRenderTexture(InName) {};
	virtual 					~TRenderTextureSeq		() override;
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

	xr_vector<TRenderTexture* >	 FrameTextures;
	u32							MSPF = 0;       
	u32							FPS = 0;        
	bool						bCycles = false;

	// Current frame
	u32							CurrentFrame = 0;
	u32							LastUpdateTime = 0;
};