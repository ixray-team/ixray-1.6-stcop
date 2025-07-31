#include "stdafx.h"

CDS0_UISequenceVideoItem::CDS0_UISequenceVideoItem()
{
}

void CDS0_UISequenceVideoItem::Copy(IUISequenceVideoItem & _in)
{
}

bool CDS0_UISequenceVideoItem::HasTexture()
{
	return false;
}
void CDS0_UISequenceVideoItem::CaptureTexture()
{
}

void CDS0_UISequenceVideoItem::ResetTexture()
{
}

BOOL CDS0_UISequenceVideoItem::video_IsPlaying()
{
	return FALSE;
}

void CDS0_UISequenceVideoItem::video_Sync(u32 _time)
{
}

void CDS0_UISequenceVideoItem::video_Play(BOOL looped, u32 _time)
{
}

void CDS0_UISequenceVideoItem::video_Stop()
{
}
