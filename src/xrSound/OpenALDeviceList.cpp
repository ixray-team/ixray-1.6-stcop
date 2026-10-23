/*
 * Copyright (c) 2005, Creative Labs Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice, this list of conditions and
 * 	     the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions
 * 	     and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *     * Neither the name of Creative Labs Inc. nor the names of its contributors may be used to endorse or
 * 	     promote products derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include "stdafx.h"

#include "OpenALDeviceList.h"


ALDeviceList::ALDeviceList()
{
	snd_device_id			= u32(-1);
	Enumerate();
}

/* 
 * Exit call
 */
ALDeviceList::~ALDeviceList()
{
	for( int i=0; snd_devices_token[i].name; i++ )
	{
		xr_free					(snd_devices_token[i].name);
	}
	xr_free						(snd_devices_token);
	snd_devices_token			= nullptr;
}

void ALDeviceList::Enumerate()
{
	Msg("SOUND: OpenAL: enumerate devices...");
	m_devices.clear				();

	int ALmajor = 1, ALminor = 1, EFXmajor = 1, EFXminor = 0;
	alcGetIntegerv(nullptr, ALC_MAJOR_VERSION, 1, &ALmajor);
	alcGetIntegerv(nullptr, ALC_MINOR_VERSION, 1, &ALminor);

	m_devices.push_back(ALDeviceDesc("Default Device", "", ALminor, ALmajor, EFXminor, EFXmajor));

	const ALCchar* DefaultDevice = nullptr;
	const ALCchar* Devices = nullptr;
	if (alcIsExtensionPresent(nullptr, "ALC_ENUMERATE_ALL_EXT"))
	{
		DefaultDevice = alcGetString(nullptr, ALC_DEFAULT_ALL_DEVICES_SPECIFIER);
		Devices = alcGetString(nullptr, ALC_ALL_DEVICES_SPECIFIER);
	}
	else
	{
		Msg("SOUND: OpenAL: EnumerationExtension NOT Present");
		DefaultDevice = alcGetString(nullptr, ALC_DEFAULT_DEVICE_SPECIFIER);
		Devices = alcGetString(nullptr, ALC_DEVICE_SPECIFIER);
	}

	xr_strcpy(m_defaultDeviceName, DefaultDevice ? Platform::UTF8_to_CP1251(DefaultDevice).c_str() : "");
	Msg("SOUND: OpenAL: system default SndDevice name is %s", m_defaultDeviceName);

	for (const ALCchar* Device = Devices; Device && *Device != '\0'; Device += xr_strlen(Device) + 1)
	{
		if (xr_strlen(Device) >= sizeof(string256))
		{
			Msg("! SOUND: OpenAL: device name is too long, skipped: %s", Device);
			continue;
		}

		xr_string DisplayName = Platform::UTF8_to_CP1251(Device);
		const char* Prefix = "OpenAL Soft on ";
		if (DisplayName.starts_with(Prefix))
		{
			DisplayName = DisplayName.substr(xr_strlen(Prefix));
		}
		if (DisplayName.size() > 200)
		{
			DisplayName.resize(200);
		}

		xr_string UniqueName = DisplayName;
		for (u32 Suffix = 2; std::any_of(m_devices.begin(), m_devices.end(), [&](const ALDeviceDesc& Desc) { return !_stricmp(Desc.name, UniqueName.c_str()); }); ++Suffix)
		{
			UniqueName = DisplayName + " #" + std::to_string(Suffix).c_str();
		}

		m_devices.push_back(ALDeviceDesc(UniqueName.c_str(), Device, ALminor, ALmajor, EFXminor, EFXmajor));
	}

//make token
	u32 _cnt								= GetNumDevices();
	snd_devices_token						= xr_alloc<xr_token>(_cnt+1);
	snd_devices_token[_cnt].id				= -1;
	snd_devices_token[_cnt].name = nullptr;
	for(u32 i=0; i<_cnt;++i)
	{
		snd_devices_token[i].id				= i;

		xr_string NormalName = m_devices[i].name;
		if (NormalName.find("OpenAL Soft on") != xr_string::npos) {
			NormalName = NormalName.substr(15);
		}

		snd_devices_token[i].name = xr_strdup(NormalName.data());
	}
//--

	if(0!=GetNumDevices())
		Msg("SOUND: OpenAL: All available devices:");

	for (u32 j = 0; j < GetNumDevices(); j++)
	{
		GetDeviceVersion(j, &ALmajor, &ALminor, &EFXmajor, &EFXminor);
		// Assume EFX by default, we only care about the spec version.
		Msg("%d. %s, Spec Version %d.%d, EFX Spec Version %d.%d",
			j+1, 
			GetDeviceName(j), 
			ALmajor,
			ALminor,
			EFXmajor,
			EFXminor
			);
	}
}

const char* ALDeviceList::GetDeviceName(u32 index)
{
	return snd_devices_token[index].name;
}

void ALDeviceList::SelectBestDevice()
{
	if (GetNumDevices() == 0)
	{
		Msg("SOUND: Can't select device. List empty");
		return;
	}

	if (snd_device_id == u32(-1) || snd_device_id >= GetNumDevices())
	{
		snd_device_id = 0;
	}

	Msg("SOUND: Selected device is %s", GetDeviceName(snd_device_id));
}

void ALDeviceList::GetDeviceVersion(u32 index, int* ALmajor, int* ALminor, int* EFXmajor, int* EFXminor)
{
	*ALmajor = m_devices[index].ALmajor_ver;
	*ALminor = m_devices[index].ALminor_ver;
	*EFXmajor = m_devices[index].EFXmajor_ver;
	*EFXminor = m_devices[index].EFXminor_ver;
	return;
}
