#pragma once
#include "NET_Shared.h"

#pragma warning(push)
#pragma warning(disable:4995)
#include <DPlay/dplay8.h>
#pragma warning(pop)

struct SteamNetConnectionRealTimeStatus_t;

class XRNETSERVER_API IClientStatistic
{
	// steam
	float				qualityLocal = 0;
	float				qualityRemote = 0;

	float				packetsInPerSec = 0;
	float				packetsOutPerSec = 0;

	s64					queueTime = 0;
	s32					sendRateBytesPerSecond = 0;
	s32					pendingReliable = 0;
	s32					pendingUnreliable = 0;
	s32					sentUnackedReliable = 0;
	// steam

	u32					mps_recive, mps_receive_base;
	u32					mps_send, mps_send_base;
	u32					dwBaseTime;
	CTimer*				device_timer;
public:
	IClientStatistic(CTimer* timer) { ZeroMemory(this, sizeof(*this)); device_timer = timer; dwBaseTime = TimeGlobal(device_timer); }

	void	Update(DPN_CONNECTION_INFO& CI);
	void	Update(SteamNetConnectionRealTimeStatus_t& status);

	IC u32	getPing() { return dwRoundTripLatencyMS; }
	IC u32	getBPS() { return dwThroughputBPS; }
	IC u32	getPeakBPS() { return dwPeakThroughputBPS; }
	IC u32	getDroppedCount() { return dwPacketsDropped; }
	IC u32	getRetriedCount() { return dwPacketsRetried; }
	IC u32	getMPS_Receive() { return mps_recive; }
	IC u32	getMPS_Send() { return mps_send; }
	IC u32	getReceivedPerSec() { return dwBytesReceivedPerSec; }
	IC u32	getSendedPerSec() { return dwBytesSendedPerSec; }	


	IC float	getQualityLocal() { return qualityLocal; }
	IC float	getQualityRemote() { return qualityRemote; }

	IC float	getPacketsInPerSec() { return packetsInPerSec; }
	IC float	getPacketsOutPerSec() { return packetsOutPerSec; }

	IC s64	getQueueTime() { return queueTime; }
	IC s32	getSendRateBytesPerSecond() { return sendRateBytesPerSecond; }
	IC s32	getPendingReliable() { return pendingReliable; }
	IC s32	getPendingUnreliable() { return pendingUnreliable; }
	IC s32	getSentUnackedReliable() { return sentUnackedReliable; }

	IC void	Clear() { CTimer* timer = device_timer; ZeroMemory(this, sizeof(*this)); device_timer = timer; dwBaseTime = TimeGlobal(device_timer); }

	//-----------------------------------------------------------------------
	u32		dwRoundTripLatencyMS = 0;
	u32		dwThroughputBPS = 0;
	u32		dwPeakThroughputBPS = 0;
	u32		dwPacketsDropped = 0;
	u32		dwPacketsRetried = 0;

	u32		dwTimesBlocked = 0;
	u32		dwBytesSended = 0;
	u32		dwBytesSendedPerSec = 0;
	u32		dwBytesReceived = 0;
	u32		dwBytesReceivedPerSec = 0;
};
