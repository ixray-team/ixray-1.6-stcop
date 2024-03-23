#include "stdafx.h"

CEditorDevice::CEditorDevice()
{
	DevicePtr.reset(this);
}

CEditorDevice::~CEditorDevice()
{
	DevicePtr.reset(nullptr);
}
