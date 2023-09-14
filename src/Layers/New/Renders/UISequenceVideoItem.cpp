#include "pch.h"

using namespace Rendering;

UISequenceVideoItem::~UISequenceVideoItem()
{
}

void 
UISequenceVideoItem::Copy(IUISequenceVideoItem& _in)
{
}

bool 
UISequenceVideoItem::HasTexture()
{
	return false;
}

void 
UISequenceVideoItem::CaptureTexture()
{
}

void 
UISequenceVideoItem::ResetTexture()
{
}

BOOL 
UISequenceVideoItem::video_IsPlaying()
{
	return 0;
}

void
UISequenceVideoItem::video_Sync(u32 _time)
{
}

void 
UISequenceVideoItem::video_Play(BOOL looped, u32 _time)
{
}

void 
UISequenceVideoItem::video_Stop()
{
}
