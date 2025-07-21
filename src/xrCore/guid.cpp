#include "stdafx.h"
#include "guid.h"

#ifdef IXR_WINDOWS
#	include <rpc.h>
#	include <rpcdce.h>
#	pragma comment(lib, "Rpcrt4.lib")
#else
#	include <uuid/uuid.h>
#endif


XRCORE_API xrGUID generate_guid()
{
	xrGUID result;

#ifdef IXR_WINDOWS
	GUID _result;
	RPC_STATUS gen_result = UuidCreate(&_result);
	Memory.mem_copy(&result, &_result, sizeof(_result));

	switch (gen_result)
	{
		case RPC_S_OK: return(result);
		case RPC_S_UUID_LOCAL_ONLY: return(result);
		case RPC_S_UUID_NO_ADDRESS:
		default: break;
	}
#else
//	uuid_t uuid;
//	uuid_generate(uuid);
//	Memory.mem_copy(&result, &uuid, sizeof(uuid_t));
#endif

	memset(&result, 0,  sizeof(result));
	u64 temp = CPU::GetCLK();
	Memory.mem_copy(&result, &temp, sizeof(temp));
	return (result);
}

XRCORE_API LPCSTR generate_guid(const xrGUID& guid, LPSTR buffer, const u32& buffer_size)
{
#ifdef IXR_WINDOWS
	static_assert(sizeof(xrGUID) == sizeof(GUID), "Different GUID types");
	GUID temp;
	Memory.mem_copy(&temp, &guid, sizeof(guid));
	RPC_CSTR temp2;
	RPC_STATUS status = UuidToStringA(&temp, &temp2);
	switch (status)
	{
		case RPC_S_OK: break;
		case RPC_S_OUT_OF_MEMORY: NODEFAULT;
		default: NODEFAULT;
	}
	VERIFY(buffer_size > xr_strlen((LPCSTR)temp2));
	xr_strcpy(buffer, buffer_size, (LPCSTR)temp2);
	RpcStringFreeA(&temp2);
#else
//	uuid_t uuid;
//	std::memcpy(&uuid, &guid, sizeof(uuid));
//
//	char uuid_str[37];
//	uuid_unparse(uuid, uuid_str);
//
//	if (buffer_size <= 36)
//	{
//		NODEFAULT;
//		return nullptr;
//	}
//
//	std::strcpy(buffer, uuid_str);
#endif
	return buffer;
}