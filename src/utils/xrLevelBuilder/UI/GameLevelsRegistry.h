#pragma once

namespace GameLevelsRegistry
{
	xr_vector<xr_string> FindUnregistered(const xr_vector<xr_string>& Levels);
	bool Register(const xr_vector<xr_string>& Levels);
}
