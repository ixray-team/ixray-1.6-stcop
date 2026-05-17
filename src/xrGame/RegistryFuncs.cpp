#include "StdAfx.h"
#include "RegistryFuncs.h"
#include "../xrGameSpy/xrGameSpy_MainDefs.h"

#define REGISTRY_BASE HKEY_CURRENT_USER

bool ReadRegistryValue(const char* rKeyName, DWORD rKeyType, void* value )
{	
#ifdef IXR_WINDOWS
	HKEY hKey = 0;	
	long res = RegOpenKeyExA(REGISTRY_BASE, 
		REGISTRY_PATH, 0, KEY_READ, &hKey);

	if (res != ERROR_SUCCESS)
	{
		//Msg ("! Unable to find %s in registry", REGISTRY_PATH);
		return false;
	}

	if (!hKey) 
	{
		//Msg ("! Unable to find %s entry in registry", REGISTRY_PATH); 
		return false;
	}

	string64	rBuf;
	DWORD KeyValueSize = 0;
	switch (rKeyType)
	{
	case REG_DWORD:
		{
			KeyValueSize = 4;
		}break;
	case REG_SZ:
		{
			KeyValueSize = 64;
		}break;
	default:
		{
			Msg ("! Unknown registry data type.");
			return false;
		}break;
	};	
		
	res = RegQueryValueExA(hKey, rKeyName, nullptr, &rKeyType, (LPBYTE)rBuf, &KeyValueSize);
	if (hKey != 0) RegCloseKey(hKey);

	if (res != ERROR_SUCCESS)
	{
		//Msg ("! Unable to find %s entry in registry", rKeyName); 
		return false;
	}
	
	memcpy(value, rBuf, KeyValueSize);
#endif
	return true;
};

bool WriteRegistryValue(const char* rKeyName, DWORD rKeyType, const void* value)
{
#ifdef IXR_WINDOWS
	HKEY hKey;

	long res = RegCreateKeyExA
	(
		REGISTRY_BASE,
		REGISTRY_PATH,
		0, nullptr , 0,
		KEY_WRITE,
		nullptr,
		&hKey,
		nullptr
	);

	if (res != ERROR_SUCCESS)
	{
		//Msg ("! Unable to find %s in registry", REGISTRY_PATH);
		return false;
	}

	if (!hKey) 
	{
		//Msg ("! Unable to find %s entry in registry", REGISTRY_PATH); 
		return false;
	}

	DWORD KeyValueSize = 0;
	switch (rKeyType)
	{
	case REG_DWORD:
		{
			KeyValueSize = 4;
		}break;
	case REG_SZ:
		{
			KeyValueSize = 64;
		}break;
	default:
		{
			Msg ("! Unknown registry data type.");
			return false;
		}break;
	};	
	
	res = RegSetValueExA(hKey, rKeyName, 0, rKeyType, (LPBYTE)value, KeyValueSize);

	if (hKey) RegCloseKey(hKey);
#endif
	return true;
};

bool	ReadRegistry_StrValue	(const char* rKeyName, char* value )
{
#ifdef IXR_WINDOWS
	return ReadRegistryValue(rKeyName, REG_SZ, value);
#else
	return true;
#endif
}

void	WriteRegistry_StrValue	(const char* rKeyName, const char* value )
{
#ifdef IXR_WINDOWS
	WriteRegistryValue(rKeyName, REG_SZ, value);
#endif
}

void	ReadRegistry_DWValue	(const char* rKeyName, DWORD& value )
{
#ifdef IXR_WINDOWS
	ReadRegistryValue(rKeyName, REG_DWORD, &value);
#endif
}
void	WriteRegistry_DWValue	(const char* rKeyName, const DWORD& value )
{
#ifdef IXR_WINDOWS
	WriteRegistryValue(rKeyName, REG_DWORD, &value);
#endif
}

u32 const	ReadRegistry_BinaryValue	(const char* rKeyName, u8 * buffer_dest, u32 const buffer_size)
{
#ifdef IXR_WINDOWS
	HKEY hKey = 0;	
	long res = RegOpenKeyExA(REGISTRY_BASE, REGISTRY_PATH, 0, KEY_READ, &hKey);

	if (res != ERROR_SUCCESS)
	{
		//Msg ("! Unable to find %s in registry", REGISTRY_PATH);
		return 0;
	}
	if (!hKey) 
	{
		//Msg ("! Unable to find %s entry in registry", REGISTRY_PATH); 
		return 0;
	}

	DWORD	value_type = REG_BINARY;
	DWORD	tmp_buffer_size = buffer_size;

	res		= RegQueryValueExA(hKey, rKeyName, nullptr, &value_type, buffer_dest, &tmp_buffer_size);
	
	if (res != ERROR_SUCCESS)
	{
		//Msg ("! Unable to find %s entry in registry", rKeyName); 
		return 0;
	}
	
	return static_cast<u32>(tmp_buffer_size);
#else
	return 0;
#endif
}

void	WriteRegistry_BinaryValue	(const char* rKeyName, u8 const * buffer_src, u32 const buffer_size)
{
#ifdef IXR_WINDOWS
	HKEY hKey;

	long res = RegOpenKeyExA(REGISTRY_BASE, 
		REGISTRY_PATH, 0, KEY_WRITE, &hKey);

	if (res != ERROR_SUCCESS)
	{
		//Msg ("! Unable to find %s in registry", REGISTRY_PATH);
		return;
	}

	if (!hKey) 
	{
		//Msg ("! Unable to find %s entry in registry", REGISTRY_PATH); 
		return;
	}

	res = RegSetValueExA(hKey, rKeyName, 0, REG_BINARY, buffer_src, buffer_size);

	RegCloseKey(hKey);
#endif
}
