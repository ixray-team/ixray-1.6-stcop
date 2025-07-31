#pragma once
#include <mysql/jdbc.h>

class XRNETSERVER_API DBService
{
private:
	sql::mysql::MySQL_Driver*	driver;
	sql::Connection*			con;

public:

	struct UserDBProperty
	{
		int id;
		float health;
		float stamina;
		float radiation;
		float psy;
		float sleepiness;
		float hunger;
		float thirst;
		float wounds;
		int money;
		int community;
		Fvector position;
	};

	struct UserDBProfile
	{
		int id;
		LPCSTR username;
	};

	struct ItemDBState
	{
		union
		{
			struct
			{
				u64 Condition : 8; // 0-100 
				u64 AmmoCount : 8; // 0-100 
				u64 AmmoType : 8; // 0-100 
				u64 AddonScopeID : 8; // 255 доступных прицелов
				u64 AddonSilenceID : 8; // 255 доступных глушителей
				u64 AddonLauncherID : 8; // 255 доступных подстволок
				u64 reserved : 16;
			};

			u64 dummy;
		};
		u64 Updagrades; // 64 апгрейда
	};

	~DBService();
	void							Connect();
	void							Test();
	void							ErrorMsg(LPCSTR function_name, int code, LPCSTR what);

	int								GetUserIdByName(LPCSTR name);
	UserDBProfile					Logon(LPCSTR username, LPCSTR password);

	void							UpdateInsertProperty(UserDBProperty data);
	UserDBProperty					SelectProperty(int id);

	void							DeleteInventory(int user_id);
	xr_vector<int>					LoadInventory(int user_id);
	void							SaveInventory(int user_id, int item_id, u64 state);

	xr_hash_map<xr_string, int>		LoadGame(shared_str need_field);
	void							PushTask(const std::function<void()>& Functor);
private:
	void							UpdateInsertPropertyInternal(UserDBProperty data);
	void							SaveInventoryInternal(int user_id, int item_id, u64 state);

private:
	// Tasks
	static void SQLUpdateThread();
	xr_task_group SQLTask;
	xr_vector<std::function<void()>> TasksActive;
	xr_vector<std::function<void()>> TasksDelay;
	volatile bool Exit = false;
	xrCriticalSection DelayCS;
};

extern XRNETSERVER_API DBService GSQLConnector;