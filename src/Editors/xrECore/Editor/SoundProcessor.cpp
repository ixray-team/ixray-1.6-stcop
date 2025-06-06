#include "stdafx.h"
#include "SoundProcessor.h"

ECORE_API _SoundProcessor SoundProcessor;

void _SoundProcessor::OnFrame()
{
	Device.Statistic->Sound.Begin();
	::Sound->update(Device.mView_saved, Device.vCameraPosition_saved, Device.vCameraDirection_saved, Device.vCameraTop_saved);
	Device.Statistic->Sound.End();
}