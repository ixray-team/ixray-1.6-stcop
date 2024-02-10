////////////////////////////////////////////////////////////////////////////
//	Module 		: script_net packet.h
//	Created 	: 06.02.2004
//  Modified 	: 24.06.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script net packet class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrScripts/script_export_space.h"
class NET_Packet;

typedef class_exporter_lib<NET_Packet> SCRIPTS_API CScriptNetPacket;
add_to_type_list(CScriptNetPacket)
#undef script_type_list
#define script_type_list save_type_list(CScriptNetPacket)
