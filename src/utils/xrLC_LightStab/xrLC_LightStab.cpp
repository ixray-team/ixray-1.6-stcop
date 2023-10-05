#include "xrlc_lightstab.h"

#include <hxgrid/Interface/IAgent.h>

extern "C" XRLC_LIGHT_STUB_API  bool __cdecl RunTask(IAgent* agent,
                 DWORD sessionId,
                 IGenericStream* inStream,
                 IGenericStream* outStream)
{

	if(lc_net::g_net_task_interface)
			return lc_net::g_net_task_interface->run_task( agent, sessionId, inStream, outStream );

	 return false;
}

