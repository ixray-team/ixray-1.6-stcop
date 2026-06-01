#include "stdafx.h"
#include "XRayTextureSeq.h"
#include "XRayTexturesManager.h"

XRayTextureSeq::~XRayTextureSeq()
{
	for (auto& Frame : FrameTextures)
	{
		if (Frame.Texture)
		{
			Owner->Free(Frame.Texture);
		}
	}

	FrameTextures.clear();
}

uint32_t XRayTextureSeq::GetOrCreateHeapIndex()
{
	auto& CurrentSeqTexture = FrameTextures[CurrentFrame];

	if (CurrentSeqTexture.HeapIndex == INDEX_NONE && Descriptor)
	{
		CurrentSeqTexture.HeapIndex = GRenderResourcesManager->DescriptorHeapAllocator->Alloc(Descriptor);
	}

	HeapIndex = CurrentSeqTexture.HeapIndex;
	return HeapIndex;
}

bool XRayTextureSeq::LoadFromSeqFile(const char* FilePath)
{
	IReader* Reader = FS.r_open(FilePath);
	if (!Reader)
	{
		Msg("! Failed to open seq file: %s", FilePath);
		return false;
	}

	string256 Buffer;
	Reader->r_string(Buffer, sizeof(Buffer));
	if (0 == _stricmp(Buffer, "cycled"))
	{
		bCycles = true;
		Reader->r_string(Buffer, sizeof(Buffer));
	}

	// Read FPS
	FPS = atoi(Buffer);
	if (FPS == 0) FPS = 30;
	MSPF = 1000 / FPS;

	while (!Reader->eof())
	{
		Reader->r_string(Buffer, sizeof(Buffer));
		_Trim(Buffer);
		if (Buffer[0] == 0)
		{
			continue;
		}

		if (!LoadFrameTexture(Buffer))
		{
			Msg("! Failed to load frame texture: %s", Buffer);
		}
	}

	FS.r_close(Reader);

	if (FrameTextures.empty())
	{
		Msg("! No frames loaded for sequence: %s", FilePath);
		return false;
	}

	CurrentFrame = 0;
	LastUpdateTime = DevicePtr->dwTimeContinual;

	Texture = FrameTextures[0].Texture->Texture;
	Descriptor = FrameTextures[0].Texture->Descriptor;
	TextureDescription = FrameTextures[0].Texture->TextureDescription;

	return true;
}

bool XRayTextureSeq::LoadFrameTexture(const char* TextureName)
{
	if (XRayTexture* ExistingTexture = Owner->GetTexture(TextureName))
	{
		if (ExistingTexture && ExistingTexture->Texture)
		{
			FSeqFrame Frame;
			Frame.Name = TextureName;
			Frame.Texture = (XRayTexture2D*)ExistingTexture;
			ExistingTexture->Counter++;
			FrameTextures.push_back(Frame);
			return true;
		}
	}

	XRayTexture2D* NewTexture = new XRayTexture2D(TextureName);
	if (NewTexture->LoadFromFile(TextureName, false))
	{
		FSeqFrame Frame;
		Frame.Name = TextureName;
		Frame.Texture = NewTexture;
		FrameTextures.push_back(Frame);
		return true;
	}

	xr_delete(NewTexture);
	return false;
}

void XRayTextureSeq::Update()
{
	if (FrameTextures.empty())
	{
		return;
	}

	u32	Frame = DevicePtr->dwTimeContinual / MSPF;
	u32	FrameData = (u32)FrameTextures.size();
	u32	FrameID = 0;

	if (bCycles)
	{
		FrameID = Frame % (FrameData * 2);
		if (FrameID >= FrameData)
		{
			FrameID = (FrameData - 1) - (FrameID % FrameData);
		}
	}
	else
	{
		FrameID = Frame % FrameData;
	}

	if (DevicePtr->dwTimeDelta == LastUpdateTime)
	{
		return;
	}

	LastUpdateTime = DevicePtr->dwTimeDelta;

	if (FrameID != CurrentFrame)
	{
		CurrentFrame = FrameID;
		auto& CurrentSeqTexture = FrameTextures[CurrentFrame];

		Texture = CurrentSeqTexture.Texture->Texture;
		Descriptor = CurrentSeqTexture.Texture->Descriptor;
		TextureDescription = CurrentSeqTexture.Texture->TextureDescription;
	}
}