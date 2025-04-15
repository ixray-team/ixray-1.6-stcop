////////////////////////////////////////////////////////////////////////////
//	Module 		: guid_generator.cpp
//	Created 	: 21.03.2005
//  Modified 	: 21.03.2005
//	Author		: Dmitriy Iassenev
//	Description : GUID generator
////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#pragma hdrstop

#include "guid_generator.h"

#ifdef IXR_WINDOWS
#	include <rpcdce.h>
#	include <rpc.h>
#	pragma comment(lib, "Rpcrt4.lib")
#else
#include <uuid/uuid.h>
#endif


ECORE_API xrGUID generate_guid()
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
	uuid_t uuid;
	uuid_generate(uuid);
	Memory.mem_copy(&result, &uuid, sizeof(uuid_t));

#endif

	ZeroMemory(&result, sizeof(result));
	u64 temp = CPU::GetCLK();
	Memory.mem_copy(&result, &temp, sizeof(temp));
	return (result);
}

u32 GetGpuNum()
{
	return 2;
}