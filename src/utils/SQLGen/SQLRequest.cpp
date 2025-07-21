#include "api.h"
#include <mysql/jdbc.h>

void RunSQLRequest(bool Quest, bool Items, const char* SubDB)
{
	string_path fname;
	FS.update_path(fname, "$game_config$", "system.ltx");
	CInifile* File = new CInifile(fname);

	try
	{
		sql::mysql::MySQL_Driver* driver;
		sql::Connection* con;

		driver = sql::mysql::get_mysql_driver_instance();
		con = driver->connect(GLoginInfo.Host, GLoginInfo.Login, GLoginInfo.Pass);

		con->setSchema(SubDB);
		sql::Statement* stmt = con->createStatement();
		sql::PreparedStatement* pstmt = nullptr;

		GLoginInfo.ProgressStatus = 0.04f;
		// 1. Создаем таблицу `game_items` (если её нет)
		if (Items)
		{
			stmt->execute
			(
				"CREATE TABLE IF NOT EXISTS game_items ("
				"id INT AUTO_INCREMENT PRIMARY KEY, "
				"name VARCHAR(100) NOT NULL UNIQUE"
				")"
			);

			xr_vector<shared_str> dbItems;
			auto result = stmt->executeQuery("SELECT name FROM game_items");
			GLoginInfo.SubProgressStatus = 0;
			while (result->next())
			{
				dbItems.push_back(result->getString("name").c_str());
				GLoginInfo.SubProgressStatus += 0.02;
			}

			auto ItemsList = ParseGameItems(File);
			GLoginInfo.ProgressStatus = 0.15f;

			pstmt = con->prepareStatement("DELETE FROM game_items WHERE name = ?");
			size_t Iter = 0;
			for (const shared_str& dbItem : dbItems)
			{
				if (std::find(ItemsList.begin(), ItemsList.end(), dbItem) == ItemsList.end())
				{
					pstmt->setString(1, *dbItem);
					pstmt->execute();
				}

				GLoginInfo.SubProgressStatus = float(dbItem.size()) / 100.f * float(Iter);
				GLoginInfo.SubProgressStatus /= 100;
			}

			pstmt = con->prepareStatement("INSERT IGNORE INTO game_items (name) VALUES (?)");
			GLoginInfo.ProgressStatus = 0.35f;
			Iter = 0;
			for (const shared_str& Name : ItemsList)
			{
				if (std::find(dbItems.begin(), dbItems.end(), Name) != dbItems.end())
				{
					continue;
				}
				pstmt->setString(1, *Name);
				pstmt->execute();

				GLoginInfo.SubProgressStatus = float(ItemsList.size()) / 100.f * float(Iter);
				GLoginInfo.SubProgressStatus /= 100;
				Iter++;
			}
		}

		// 2. Создаем таблицу `game_quests` (если её нет)
		if (Quest)
		{
			stmt->execute
			(
				"CREATE TABLE IF NOT EXISTS game_quests ("
				"id INT AUTO_INCREMENT PRIMARY KEY, "
				"quest_name VARCHAR(100) NOT NULL"
				")"
			);

			Msg("Clear 'game_quests' table");
			stmt->execute("TRUNCATE TABLE game_quests");

			pstmt = con->prepareStatement("INSERT INTO game_quests (quest_name) VALUES (?)");

			auto QuestsList = ParseGameQuests();
			GLoginInfo.ProgressStatus = 0.7f;

			float LastProgress = 100 - GLoginInfo.ProgressStatus;
			size_t Iter = 0;
			for (shared_str Name : QuestsList)
			{
				pstmt->setString(1, *Name);
				pstmt->execute();
				GLoginInfo.SubProgressStatus = float(QuestsList.size()) / 100.f * float(Iter);
				GLoginInfo.SubProgressStatus /= 100;
				Iter++;
			}
		}

		GLoginInfo.ProgressStatus = 1.f;
		xr_delete(stmt);
	}
	catch (sql::SQLException& e)
	{
		// Обработка ошибок
		Msg("! MySQL Error: %s", e.what());
		Msg("! SQL State: %s", e.getSQLState().c_str());
	}

	xr_delete(File);
}
