#pragma once

struct ECORE_API _SoundProcessor :
	public pureFrame
{
	virtual void OnFrame() override;
};

extern ECORE_API _SoundProcessor SoundProcessor;