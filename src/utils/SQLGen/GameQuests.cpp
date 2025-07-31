#include "api.h"

xr_vector<shared_str> ParseGameQuests()
{
	string_path fname;
	FS.update_path(fname, "$game_config$", "misc\\task_manager.ltx");
	CInifile TMIni(fname);

	xr_vector<shared_str> Trash;

	for (auto Sect : TMIni.sections())
	{
		Trash.emplace_back(Sect->Name);
	}

	return std::move(Trash);
}