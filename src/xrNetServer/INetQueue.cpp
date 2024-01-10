#include "stdafx.h"
#include "INetQueue.h"

static size_t LastTimeCreate = 0;

INetQueue::INetQueue()
#ifdef PROFILE_CRITICAL_SECTIONS
	:cs(MUTEX_PROFILE_ID(INetQueue))
#endif // PROFILE_CRITICAL_SECTIONS
{
	unused.reserve(128);
	for (int i = 0; i < 16; i++)
		unused.push_back(xr_new<NET_Packet>());
}

INetQueue::~INetQueue()
{
	cs.Enter();
	u32 it;
	for (it = 0; it < unused.size(); it++)	xr_delete(unused[it]);
	for (it = 0; it < ready.size(); it++)	xr_delete(ready[it]);
	cs.Leave();
}

NET_Packet* INetQueue::Create()
{
	NET_Packet* P = 0;
	if (unused.empty())
	{
		ready.push_back(xr_new<NET_Packet>());
		P = ready.back();
		LastTimeCreate = GetTickCount();
	}
	else 
	{
		ready.push_back(unused.back());
		unused.pop_back();
		P = ready.back();
	}
	return	P;
}

NET_Packet* INetQueue::Create(const NET_Packet& _other)
{
	NET_Packet* P = 0;
	cs.Enter();
	if (unused.empty())
	{
		ready.push_back(xr_new<NET_Packet>());
		P = ready.back();
		//---------------------------------------------
		LastTimeCreate = GetTickCount();
		//---------------------------------------------
	}
	else 
	{
		ready.push_back(unused.back());
		unused.pop_back();
		P = ready.back();
	}
	CopyMemory(P, &_other, sizeof(NET_Packet));
	cs.Leave();
	return			P;
}

NET_Packet* INetQueue::Retreive()
{
	NET_Packet* P = 0;

	if (!ready.empty())		P = ready.front();
	//---------------------------------------------	
	else
	{
		u32 tmp_time = GetTickCount() - 60000;
		size_t size = unused.size();
		if ((LastTimeCreate < tmp_time) && (size > 32))
		{
			xr_delete(unused.back());
			unused.pop_back();
		}
	}

	return	P;
}

void INetQueue::Release()
{
	VERIFY(!ready.empty());
	//---------------------------------------------
	u32 tmp_time = GetTickCount() - 60000;
	size_t size = unused.size();
	ready.front()->B.count = 0;
	if ((LastTimeCreate < tmp_time) && (size > 32))
	{
		xr_delete(ready.front());
	}
	else
		unused.push_back(ready.front());
	//---------------------------------------------	
	ready.pop_front();
}
