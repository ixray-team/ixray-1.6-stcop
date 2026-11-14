#pragma once

#include <nlohmann/json.hpp>

class CCommandVar;

CCommandVar ExecAIStageMacroCommand(u32 Command, const xr_string& First, const xr_string& Second, nlohmann::json& Report);
