#pragma once

class XRNETSERVER_API INetQueue
{
	xrCriticalSection		cs;
	xr_deque<NET_Packet*>	ready;
	xr_vector<NET_Packet*>	unused;
public:
	INetQueue();
	~INetQueue();

	NET_Packet*			Create();
	NET_Packet*			Create(const NET_Packet& _other);
	NET_Packet*			Retreive();
	void				Release();
	inline void			Lock() { cs.Enter(); };
	inline void			Unlock() { cs.Leave(); };
};
