// Copyright (c) 2026 IX-Ray and contributors. All rights reserved.
//
// This file is part of the IX-Ray engine and is licensed for use only within
// the IX-Ray project (https://github.com/ixray-team). The project-wide license
// in LICENSE.md does not apply to this file.
//
// You may use, modify and distribute this file only as part of IX-Ray or of
// a game or modification built on IX-Ray. Using, copying or adapting this file,
// or any part of it, in any other project is prohibited without prior written
// permission from the IX-Ray team.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY ARISING FROM THE USE OF THIS FILE.

#include "ReflectModel.h"

#include <fstream>
#include <sstream>

namespace XReflectTool
{
	std::string Trim(std::string Text)
	{
		Text.erase(0, Text.find_first_not_of(" \t\r\n"));
		Text.erase(Text.find_last_not_of(" \t\r\n") + 1);
		return Text;
	}

	std::vector<std::string> SplitList(const std::string& Text, char Separator)
	{
		std::vector<std::string> Result;
		std::stringstream Stream(Text);
		std::string Item;
		while (std::getline(Stream, Item, Separator))
		{
			Item = Trim(Item);
			if (!Item.empty())
			{
				Result.push_back(Item);
			}
		}
		return Result;
	}

	std::string JoinList(const std::vector<std::string>& Items)
	{
		std::string Result;
		for (size_t Index = 0; Index < Items.size(); ++Index)
		{
			Result += (Index ? ", " : "") + Items[Index];
		}
		return Result;
	}


	std::string Guard(const std::string& Condition, const std::string& Enclosing, const std::string& Text)
	{
		if (Condition.empty() || Condition == Enclosing)
		{
			return Text;
		}
		return "#if " + Condition + "\n" + Text + "#endif\n";
	}

	std::string ReadFile(const std::filesystem::path& Path)
	{
		std::ifstream Stream(Path, std::ios::binary);
		std::ostringstream Content;
		Content << Stream.rdbuf();
		return Content.str();
	}

	std::string ToCrlf(const std::string& Text)
	{
		std::string Result;
		Result.reserve(Text.size() + Text.size() / 16);
		for (char C : Text)
		{
			if (C == '\n')
			{
				Result += '\r';
			}
			Result += C;
		}
		return Result;
	}

	bool WriteIfChanged(const std::string& Path, const std::string& Content, std::vector<ParseError>& Errors)
	{
		if (ReadFile(Path) == Content)
		{
			return true;
		}
		std::ofstream Stream(Path, std::ios::binary | std::ios::trunc);
		Stream << Content;
		if (!Stream)
		{
			Errors.push_back({ Path, 0, "can not write the output" });
			return false;
		}
		return true;
	}
}
