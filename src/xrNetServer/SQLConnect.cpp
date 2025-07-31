#include "stdafx.h"
#include "SQLConnect.h"
#include <mysql/jdbc.h>

#include <json/json.hpp>
#include <fstream>

XRNETSERVER_API DBService GSQLConnector;


void DBService::SQLUpdateThread()
{
	PROF_THREAD("SQL Server Updater");
	while (!GSQLConnector.Exit)
	{
		{
			xrCriticalSectionGuard guard(GSQLConnector.DelayCS);
			GSQLConnector.TasksActive = GSQLConnector.TasksDelay;
			GSQLConnector.TasksDelay.clear();
		}

		for (const auto& Functor : GSQLConnector.TasksActive)
		{
			Functor();
		}
	}
}

DBService::~DBService()
{
	Exit = true;
	SQLTask.wait();

	delete driver;
	delete con;
}

void DBService::Connect()
{
	string_path jfn;
	nlohmann::json JSONData = {};
	FS.update_path(jfn, "$app_data_root$", "sql_login.json");

	if (std::filesystem::exists(jfn))
	{
		std::ifstream f(jfn);
		f >> JSONData;
	}

	auto RestoreFromJSONLambda = [&JSONData](const char* Value, auto& Out)
	{
		if (JSONData.contains(Value))
		{
			xr_strcpy(Out, JSONData[Value].get<std::string>().c_str());
		}
	};

	Msg("IX-Ray SQL Connector init...\r\nParse data from json...");

	string64 Login;
	string64 Password;
	string128 Host;
	RestoreFromJSONLambda("Host", Host);
	RestoreFromJSONLambda("Login", Login);
	RestoreFromJSONLambda("Password", Password);

	driver = sql::mysql::get_mysql_driver_instance();
	con = driver->connect(Host, Login, Password);

	SQLTask.run(&DBService::SQLUpdateThread);
}

void DBService::Test()
{
	#pragma todo("hkuprin to hkuprin: rename method 'Test' to 'Connect' in SQLConnect.cpp/h")
	try
	{

		con->setSchema("ixray-test");
		sql::Statement* stmt = con->createStatement();

		stmt->execute("SELECT * FROM users");
		sql::ResultSet* set = stmt->getResultSet();

		while (set->next())
		{
			int id = set->getInt("uuid");
			Msg("User#%d", id);
		}
		delete set;
		delete stmt;
	}
	catch (sql::SQLException& e)
	{
		ErrorMsg("Connect", e.getErrorCode(), e.what());
	}
}

void DBService::ErrorMsg(LPCSTR function_name, int code, LPCSTR what)
{
	Msg("! [DBService::%s::SQLException] Code: %d / Desc: %s", function_name , code, what);
}

void DBService::DeleteInventory(int user_id)
{
	auto DeleteInvLambda = [user_id, this]()
	{
		try
		{
			if (!con->isClosed())
			{
				sql::PreparedStatement* pstmt = con->prepareStatement("DELETE FROM users_items WHERE user_id = ?;");
				pstmt->setInt(1, user_id);
				pstmt->execute();

				delete pstmt;
			}
		}
		catch (sql::SQLException& e)
		{
			ErrorMsg("DeleteInventory", e.getErrorCode(), e.what());
		}
	};

	TasksDelay.push_back(DeleteInvLambda);
}

void DBService::SaveInventoryInternal(int user_id, int item_id, u64 state)
{
#pragma todo("hkuprin to hkuprin: rename method 'SaveInventory' to 'InsertInventory' in SQLConnect.cpp/h")
	try
	{
		if (!con->isClosed())
		{
			Msg("id: %d, state: %d", item_id, state);

			sql::PreparedStatement* pstmt;
			if (state > 0)
			{
				pstmt = con->prepareStatement("INSERT INTO users_items (user_id, item_id, item_state) VALUES (?, ?, ?)");
			}
			else 
			{
				pstmt = con->prepareStatement("INSERT INTO users_items (user_id, item_id) VALUES (?, ?)");
			}

			pstmt->setInt(1, user_id);
			pstmt->setInt(2, item_id);

			if (state > 0) 
			{
				pstmt->setInt(3, state);
			}

			pstmt->execute();
			delete pstmt;
		}
	}
	catch (sql::SQLException& e)
	{
		ErrorMsg("SaveInventory", e.getErrorCode(), e.what());
	}
}

xr_vector<int> DBService::LoadInventory(int user_id)
{
#pragma todo("hkuprin to hkuprin: rename method 'LoadInventory' to 'SelectInventory' in SQLConnect.cpp/h")
	xr_vector<int> items;
	try
	{
		if (!con->isClosed())
		{
			sql::PreparedStatement* pstmt = con->prepareStatement("SELECT item_id FROM users_items WHERE user_id = ?");
			pstmt->setInt(1, user_id);
			pstmt->execute();
			sql::ResultSet* set = pstmt->getResultSet();

			while (set->next())
			{
				items.push_back(set->getInt("item_id"));
			}

			delete set;
			delete pstmt;
		}
	}
	catch (sql::SQLException& e)
	{
		ErrorMsg("LoadInventory", e.getErrorCode(), e.what());
	}

	return std::move(items);
}

void DBService::SaveInventory(int user_id, int item_id, u64 state)
{
	auto SendSQLambda = [user_id, item_id, state, this]()
	{
		SaveInventoryInternal(user_id, item_id, state);
	};

	xrCriticalSectionGuard guard(DelayCS);
	TasksDelay.push_back(SendSQLambda);
}

void DBService::UpdateInsertProperty(UserDBProperty data)
{
	auto SendSQLambda = [data, this]()
	{
		UpdateInsertPropertyInternal(data);
	};

	xrCriticalSectionGuard guard(DelayCS);
	TasksDelay.push_back(SendSQLambda);
}

void DBService::UpdateInsertPropertyInternal(UserDBProperty data)
{
	try
	{
		if (!con->isClosed())
		{
			sql::PreparedStatement* pstmt = con->prepareStatement("SELECT COUNT(*) FROM users_property WHERE user_id = ?");
			pstmt->setInt(1, data.id);
			pstmt->execute();
			sql::ResultSet* set = pstmt->getResultSet();

			set->next();
			bool exist = set->getInt(1) > 0;

			if (exist)
			{
				pstmt = con->prepareStatement
				(
					"UPDATE users_property SET health = ?, stamina = ?, radiation = ?, psy = ?, sleepiness = ?, hunger = ?, thirst = ?, wounds = ?, money = ?, community = ? WHERE user_id = ?"
				);

				pstmt->setDouble	(1, data.health);
				pstmt->setDouble	(2, data.stamina);
				pstmt->setDouble	(3, data.radiation);
				pstmt->setDouble	(4, data.psy);
				pstmt->setDouble	(5, data.sleepiness);
				pstmt->setDouble	(6, data.hunger);
				pstmt->setDouble	(7, data.thirst);
				pstmt->setDouble	(8, data.wounds);
				pstmt->setInt		(9, data.money);
				pstmt->setInt		(10, data.community);
				pstmt->setInt		(11, data.id);
				pstmt->execute		();
			}
			else
			{
				pstmt = con->prepareStatement
				(
					"INSERT INTO users_property (user_id, health, stamina, radiation, psy, sleepiness, hunger, thirst, wounds, money, community ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
				);

				pstmt->setInt		(1, data.id);
				pstmt->setDouble	(2, data.health);
				pstmt->setDouble	(3, data.stamina);
				pstmt->setDouble	(4, data.radiation);
				pstmt->setDouble	(5, data.psy);
				pstmt->setDouble	(6, data.sleepiness);
				pstmt->setDouble	(7, data.hunger);
				pstmt->setDouble	(8, data.thirst);
				pstmt->setDouble	(9, data.wounds);
				pstmt->setInt		(10, data.money);
				pstmt->setInt		(11, data.community);
				pstmt->execute		();
			}

			delete set;
			delete pstmt;
		}
	}
	catch (sql::SQLException& e)
	{
		ErrorMsg("UpdateInsertProperty", e.getErrorCode(), e.what());
	}
}

int DBService::GetUserIdByName(LPCSTR name)
{
	try
	{
		if (!con->isClosed())
		{
			sql::PreparedStatement* pstmt = con->prepareStatement("SELECT uuid FROM users WHERE username = ?");
			pstmt->setString(1, name);
			pstmt->execute();
			sql::ResultSet* set = pstmt->getResultSet();

			if (set->next()) {
				int uuid = set->getInt("uuid");
				if (!set->wasNull() && uuid > 0) {
					delete set;
					delete pstmt;
					return uuid;
				}
			}
			else {
				return -1;
			}
			return -1;
		}
	}
	catch (sql::SQLException& e)
	{
		ErrorMsg("GetUserIdByName", e.getErrorCode(), e.what());
	}
	return -1;
}

DBService::UserDBProfile Logon(LPCSTR username, LPCSTR password)
{
	DBService::UserDBProfile res_data;
	
	return std::move(res_data);
}

xr_hash_map<xr_string, int> DBService::LoadGame(shared_str need_field)
{
	xr_hash_map<xr_string, int> something;
	something.clear();

	try
	{
		if (!con->isClosed())
		{
			string32 text;
			xr_sprintf(text, "SELECT * FROM %s", need_field.c_str());
			sql::PreparedStatement* pstmt = con->prepareStatement(text);
			pstmt->execute();
			sql::ResultSet* set = pstmt->getResultSet();

			while (set->next())
			{
				Msg("* [DBService::LoadGame] item_%s_%d", set->getString("name").c_str(), set->getInt("id"));
				something.insert({set->getString("name").c_str(), set->getInt("id")});
			}

			delete set;
			delete pstmt;
		}
	}
	catch (sql::SQLException& e)
	{
		ErrorMsg("LoadGame", e.getErrorCode(), e.what());
	}

	return std::move(something);
}

DBService::UserDBProperty DBService::SelectProperty(int id)
{
	DBService::UserDBProperty res = {};
	try
	{
		if (!con->isClosed())
		{
			sql::PreparedStatement* pstmt = con->prepareStatement("SELECT * FROM users_property WHERE user_id = ?");
			pstmt->setInt(1, id);
			pstmt->execute();
			sql::ResultSet* set = pstmt->getResultSet();

			if (set->next())
			{
				Msg("health: %f, stamina: %f", set->getDouble("health"), set->getDouble("stamina"));
				res.id			= id;
				res.health		= set->getDouble("health");
				res.stamina		= set->getDouble("stamina");
				res.radiation	= set->getDouble("radiation");
				res.psy			= set->getDouble("psy");
				res.sleepiness	= set->getDouble("sleepiness");
				res.hunger		= set->getDouble("hunger");
				res.thirst		= set->getDouble("thirst");
				res.wounds		= set->getDouble("wounds");
				res.money		= set->getInt("money");
				res.community	= set->getInt("community");
			}

			delete set;
			delete pstmt;
			
		}
	}
	catch (sql::SQLException& e)
	{
		ErrorMsg("SelectProperty", e.getErrorCode(), e.what());
	}
	return std::move(res);
}