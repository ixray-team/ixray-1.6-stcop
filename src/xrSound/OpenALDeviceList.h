#ifndef ALDEVICELIST_H
#define ALDEVICELIST_H

#include "AL/al.h"
#include "AL/alc.h"
#include "AL/efx.h"

#define AL_GENERIC_HARDWARE "Generic Hardware"
#define AL_GENERIC_SOFTWARE "Generic Software"

struct ALDeviceDesc{
	string256			name;
	string256			name_al;
	int	ALminor_ver;
	int	ALmajor_ver;
	int	EFXminor_ver;
	int	EFXmajor_ver;

	union ESndProps
	{
		struct{
			u16				selected	:1;
			u16				efx			:1;
			u16 xra : 1;

			u16				unused		:9;
		};
		u16 storage;
	};
	ESndProps				props;
	ALDeviceDesc(LPCSTR nm,LPCSTR nml, int almn, int almj, int efxmn, int efxmj)
	{
		xr_strcpy(name, nm);
		xr_strcpy(name_al, nml);
		ALminor_ver = almn;
		ALmajor_ver = almj;
		EFXminor_ver = efxmn;
		EFXmajor_ver = efxmj;
		props.storage = 0;
		//props.eax_unwanted=true;
	}
};

class ALDeviceList
{
private:
	xr_vector<ALDeviceDesc>	m_devices;
	string256			m_defaultDeviceName;
	void				Enumerate				();
public:
						ALDeviceList			();
						~ALDeviceList			();

	u32					GetNumDevices			()				{return (u32)m_devices.size();}
	const ALDeviceDesc&	GetDeviceDesc			(u32 index)		{return m_devices[index];}
	LPCSTR				GetDeviceName			(u32 index);
	void GetDeviceVersion(u32 index, int* ALmajor, int* ALminor, int* EFXmajor, int* EFXminor);
	void				SelectBestDevice		();
};

#endif // ALDEVICELIST_H
