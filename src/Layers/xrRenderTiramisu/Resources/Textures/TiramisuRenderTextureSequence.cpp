#include "stdafx.h"
#include "TiramisuRenderTextureSequence.h"
#include "TiramisuRenderTexturesManager.h"

TiramisuRenderTextureSequence::~TiramisuRenderTextureSequence()
{
	CheckIsGameThread();
	for (TiramisuRenderTexture*& Textxure : FrameTextures)
	{
		Owner->Free(Textxure);
	}

	FrameTextures.clear();
}


bool TiramisuRenderTextureSequence::LoadFromSeqFile(const char* FilePath)
{
	CheckIsGameThread();
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
	if (FPS == 0)
	{
		FPS = 30;
	}
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
	TextureDescription = FrameTextures[0]->TextureDescription;
	return true;
}

bool TiramisuRenderTextureSequence::LoadFrameTexture(const char* TextureName)
{
	CheckIsGameThread();
	if (TiramisuRenderTexture* ExistingTexture = Owner->GetTexture(TextureName))
	{
		FrameTextures.push_back(ExistingTexture);
		return true;
	}

	return false;
}

void TiramisuRenderTextureSequence::Update()
{
	CheckIsGameThread();
	if (FrameTextures.empty())
	{
		return;
	}

	u32 Frame = DevicePtr->dwTimeContinual / MSPF;
	u32 FrameData = FrameTextures.size();
	u32 FrameID;

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
		TextureDescription = FrameTextures[FrameID]->TextureDescription;
	}
}
