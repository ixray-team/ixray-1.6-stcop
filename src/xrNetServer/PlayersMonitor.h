#pragma once
#include "net_shared.h"
#include "NET_Common.h"

class IClient;

class PlayersMonitor
{
private:
	typedef xr_vector<IClient*>	players_collection_t;
	xrCriticalSection			csPlayers;
	players_collection_t		net_Players;
	players_collection_t		net_Players_disconnected;
	bool						now_iterating_in_net_players;
	bool						now_iterating_in_net_players_disconn;
#ifdef DEBUG
	DWORD						iterator_thread_id;
#endif
public:
	PlayersMonitor()
	{
		now_iterating_in_net_players = false;
		now_iterating_in_net_players_disconn = false;
#ifdef DEBUG
		iterator_thread_id = 0;
#endif
	}
#ifdef DEBUG
	bool IsCurrentThreadIteratingOnClients() const
	{
		if (now_iterating_in_net_players || now_iterating_in_net_players_disconn)
		{
			if (iterator_thread_id == GetCurrentThreadId())
			{
				return true;
			}
		}
		return false;
	}
#endif

	template<typename ActionFunctor>
	void ForEachClientDo(ActionFunctor & functor)
	{
		csPlayers.Enter();
		now_iterating_in_net_players = true;
#ifdef DEBUG
		iterator_thread_id = GetCurrentThreadId();
#endif
		for (players_collection_t::iterator i = net_Players.begin(),
			ie = net_Players.end(); i != ie; ++i)
		{
			VERIFY2(*i != NULL, "IClient ptr is NULL");
			functor(*i);
		}
		now_iterating_in_net_players = false;
		csPlayers.Leave();
	}

	void ForEachClientDo(xr_delegate<void(IClient*)> & fast_delegate)
	{
		csPlayers.Enter();
		now_iterating_in_net_players = true;
#ifdef DEBUG
		iterator_thread_id = GetCurrentThreadId();
#endif
		for (players_collection_t::iterator i = net_Players.begin(),
			ie = net_Players.end(); i != ie; ++i)
		{
			VERIFY2(*i != NULL, "IClient ptr is NULL");
			fast_delegate(*i);
		}
		now_iterating_in_net_players = false;
		csPlayers.Leave();
	}

	template<typename SearchPredicate, typename ActionFunctor>
	u32	ForFoundClientsDo(SearchPredicate const & predicate, ActionFunctor & functor)
	{
		u32 ret_count = 0;
		csPlayers.Enter();

		now_iterating_in_net_players = true;
#ifdef DEBUG
		iterator_thread_id = GetCurrentThreadId();
#endif
		players_collection_t::iterator players_endi = net_Players.end();
		players_collection_t::iterator temp_iter = std::find_if(
			net_Players.begin(),
			players_endi,
			predicate);

		while (temp_iter != players_endi)
		{
			VERIFY2(*temp_iter != NULL, "IClient ptr is NULL");
			functor(*temp_iter);
			temp_iter = std::find_if(++temp_iter, players_endi, predicate);
		}
		now_iterating_in_net_players = false;

		csPlayers.Leave();
		return ret_count;
	}

	template<typename SearchPredicate>
	IClient* FindAndEraseClient(SearchPredicate const & predicate)
	{
		csPlayers.Enter();
		VERIFY(!now_iterating_in_net_players);
		now_iterating_in_net_players = true;
#ifdef DEBUG
		iterator_thread_id = GetCurrentThreadId();
#endif
		players_collection_t::iterator client_iter = std::find_if(
			net_Players.begin(),
			net_Players.end(),
			predicate);
		IClient* ret_client = NULL;
		if (client_iter != net_Players.end())
		{
			ret_client = *client_iter;
			net_Players.erase(client_iter);
		}
		now_iterating_in_net_players = false;

		csPlayers.Leave();
		return ret_client;
	}

	template<typename SearchPredicate>
	IClient* GetFoundClient(SearchPredicate const & predicate)
	{
		csPlayers.Enter();
		players_collection_t::iterator client_iter = std::find_if(
			net_Players.begin(),
			net_Players.end(),
			predicate);

		IClient* ret_client = NULL;
		if (client_iter != net_Players.end())
		{
			ret_client = *client_iter;
		}

		csPlayers.Leave();
		return ret_client;
	}

	void AddNewClient(IClient* new_client)
	{
		csPlayers.Enter();
		VERIFY(!now_iterating_in_net_players);
		net_Players.push_back(new_client);
		csPlayers.Leave();
	}

	u32 ClientsCount()
	{
		csPlayers.Enter();
		u32 ret_count = (u32)net_Players.size();
		csPlayers.Leave();
		return ret_count;
	}
}; 
