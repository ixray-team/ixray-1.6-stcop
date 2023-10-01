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

#pragma warning(push)
#pragma warning(disable:4995)
#include <objbase.h>
#pragma warning(pop)

#ifdef _EDITOR
	log_fn_ptr_type*	pLog = NULL;

void __cdecl al_log(char* msg)
{
	Log(msg);
}
#endif

ALDeviceList::ALDeviceList()
{
#ifdef _EDITOR
	pLog					= al_log;
#endif

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
	snd_devices_token			= NULL;
}

void ALDeviceList::Enumerate()
{
	int	ALmajor, ALminor, EFXmajor, EFXminor, index;
	const char* actualDeviceName;
	
	Msg("SOUND: OpenAL: enumerate devices...");
	// have a set of vectors storing the device list, selection status, spec version #, and XRAM support status
	// -- empty all the lists and reserve space for 10 devices
	m_devices.clear				();
	
	CoUninitialize();

	xr_vector<const char*> DeviceNameList;
	xr_vector<const char*> DeviceOALNameList;
	auto list_audio_devices = [&DeviceNameList, &DeviceOALNameList](const ALCchar* devices) 	{
		const ALCchar* device = devices, * next = devices + 1;
		size_t len = 0;

		while (device && *device != '\0' && next && *next != '\0') {
			len = strlen(device);
			wchar_t* wDevice = new wchar_t[len];
			ZeroMemory(wDevice, sizeof(wchar_t) * len);
			char* cDevice = new char[len];

			MultiByteToWideChar(CP_UTF8, 0, device, len, wDevice, len);
			WideCharToMultiByte(CP_ACP, 0, wDevice, len, cDevice, len, NULL, NULL);

			DeviceNameList.push_back(cDevice);
			DeviceOALNameList.push_back(device);

			device += (len + 1);
			next += (len + 2);

			delete[] wDevice;
		}
	};

	// grab function pointers for 1.0-API functions, and if successful proceed to enumerate all devices
	if (alcIsExtensionPresent(nullptr, "ALC_ENUMERATE_ALL_EXT"))
	{
		list_audio_devices(alcGetString(NULL, ALC_ALL_DEVICES_SPECIFIER));
		
		xr_strcpy(m_defaultDeviceName, DeviceNameList[0]);
		Msg("SOUND: OpenAL: system  default SndDevice name is %s", m_defaultDeviceName);

		index				= 0;
		// go through device list (each device terminated with a single NULL, list terminated with double NULL)
		for (size_t Iter = 0; Iter < DeviceOALNameList.size(); Iter++)
		{
			const char* Device = DeviceOALNameList[Iter];
			ALCdevice *device = alcOpenDevice(Device);
			if (device) 
			{
				ALCcontext* context = alcCreateContext(device, nullptr);
				if (context) 
				{
					alcMakeContextCurrent(context);

					// if new actual device name isn't already in the list, then add it...
					if ((Device != nullptr) && xr_strlen(Device) > 0) {
						alcGetIntegerv(device, ALC_MAJOR_VERSION, sizeof(int), &ALmajor);
						alcGetIntegerv(device, ALC_MINOR_VERSION, sizeof(int), &ALminor);

						alcGetIntegerv(device, ALC_EFX_MAJOR_VERSION, sizeof(int), &EFXmajor);
						alcGetIntegerv(device, ALC_EFX_MINOR_VERSION, sizeof(int), &EFXminor);

						m_devices.push_back(ALDeviceDesc(DeviceNameList[Iter], Device, ALminor, ALmajor, EFXminor, EFXmajor));

						++index;
					}
					alcDestroyContext(context);
				}else
				{
					Msg("SOUND: OpenAL: cant create context for %s",device);
				}
				alcCloseDevice(device);
			}else
			{
				Msg("SOUND: OpenAL: cant open device %s", Device);
			}
		}
	}else
		Msg("SOUND: OpenAL: EnumerationExtension NOT Present");

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

LPCSTR ALDeviceList::GetDeviceName(u32 index)
{
	return snd_devices_token[index].name;
}

void ALDeviceList::SelectBestDevice()
{
	int best_majorVersion	= -1;
	int best_minorVersion	= -1;
	int ALmajorVersion;
	int ALminorVersion;
	int EFXmajorVersion;
	int EFXminorVersion;
	
	if(snd_device_id==u32(-1))
	{
		//select best
		u32 new_device_id		= snd_device_id;
		for (u32 i = 0; i < GetNumDevices(); ++i)
		{
			if(_stricmp(m_defaultDeviceName,GetDeviceName(i))!=0)
				continue;

			GetDeviceVersion		(i, &ALmajorVersion, &ALminorVersion, &EFXmajorVersion, &EFXminorVersion);
			if( (ALmajorVersion>best_majorVersion) ||
				(ALmajorVersion==best_majorVersion && ALminorVersion>best_minorVersion) )
			{
				best_majorVersion		= ALmajorVersion;
				best_minorVersion		= ALminorVersion;
				new_device_id			= i;
			}
		}
		if(new_device_id==u32(-1) )
		{
			R_ASSERT(GetNumDevices()!=0);
			new_device_id = 0; //first
		};
		snd_device_id = new_device_id;
	}
	if(GetNumDevices()==0)
		Msg("SOUND: Can't select device. List empty");
	else
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
