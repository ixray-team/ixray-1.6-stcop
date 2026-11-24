#include "stdafx.h"
#include "dxUISequenceVideoItem.h"

dxUISequenceVideoItem::dxUISequenceVideoItem()
{
	m_texture = nullptr;
}

void dxUISequenceVideoItem::Copy(IUISequenceVideoItem&_in)
{
	*this = *((dxUISequenceVideoItem*)&_in);
}

void dxUISequenceVideoItem::CaptureTexture()
{
	m_texture = DrawRecorder::IsRecording() ? Recorder.PassTexture() : RImplementation.Imm().GetTexture(0);
}