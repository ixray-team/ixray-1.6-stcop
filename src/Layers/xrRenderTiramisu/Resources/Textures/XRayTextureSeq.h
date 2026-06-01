#pragma once
#include "XRayTexture2D.h"

class XRayTextureSeq final : 
	public XRayTexture2D
{
public:
	XRayTextureSeq(const shared_str& InName = "None") : XRayTexture2D(InName) {};
	~XRayTextureSeq();

	virtual u32 GetOrCreateHeapIndex() override;
	bool LoadFromSeqFile(const char* FilePath);
	void Update();

	// Sequence settings
	void SetCycles(bool bCycles) { bCycles = bCycles; }
	u32  GetFPS() const { return FPS; }
	u32  GetFrameCount() const { return (u32)FrameTextures.size(); }

private:
	bool LoadFrameTexture(const char* TextureName);

private:
	struct FSeqFrame
	{
		XRayTexture2D* Texture = nullptr;
		shared_str Name;
		u32 HeapIndex = INDEX_NONE;
	};

	xr_vector<FSeqFrame> FrameTextures;
	u32 MSPF = 0;       
	u32 FPS = 0;        
	bool bCycles = false;

	// Current frame
	u32 CurrentFrame = 0;
	u32 LastUpdateTime = 0;
};