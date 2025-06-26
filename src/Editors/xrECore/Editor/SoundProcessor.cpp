#include "stdafx.h"
#include "SoundProcessor.h"

ECORE_API _SoundProcessor SoundProcessor;

void _SoundProcessor::OnFrame()
{
	Device.Statistic->Sound.Begin();
	::Sound->update(Device.vCameraPosition, Device.vCameraDirection, Device.vCameraTop);
	Device.Statistic->Sound.End();
}