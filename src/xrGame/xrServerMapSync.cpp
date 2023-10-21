#include "stdafx.h"
#include "Level.h"
#include "xrServer.h"
#include "xrServerMapSync.h"

void xrServer::OnProcessClientMapData(NET_Packet& P, ClientID const & clientID)
{
	NET_Packet	responseP;
	string128	client_map_name;
	string128	client_map_version;
	u32			client_geom_crc32;
	
	P.r_stringZ_s(client_map_name);
	P.r_stringZ_s(client_map_version);
	P.r_u32(client_geom_crc32);

	LPCSTR	server_map_name = Level().get_net_DescriptionData().map_name;
	LPCSTR	server_map_version = Level().get_net_DescriptionData().map_version;

	responseP.w_begin	(M_SV_MAP_NAME);

	if ((xr_strcmp(server_map_name, client_map_name)) ||
		(xr_strcmp(server_map_version, client_map_version)))
	{
		responseP.w_u8(static_cast<u8>(YouHaveOtherMap));
		//here we can make hard disconnect of this client...
	} else if (!Level().IsChecksumsEqual(client_geom_crc32))
	{
		responseP.w_u8(static_cast<u8>(InvalidChecksum));
	} else
	{
		responseP.w_u8(static_cast<u8>(SuccessSync));
	}

	SendTo		(clientID, responseP, net_flags(TRUE, TRUE));
}