#pragma once

#include "PackagePresentation.h"

namespace SdkPackages::Markdown
{
struct Span
{
	xr_string Text;
	xr_string Url;
	bool IsStrong = false;
	bool IsCode = false;
};

inline xr_vector<Span> ParseInline(const xr_string& Input)
{
	xr_string Text = Input;
	for (size_t Position = 0; (Position = Text.find("\xe2\x86\x92", Position)) != xr_string::npos; ++Position)
	{
		Text.replace(Position, 3, ">");
	}
	xr_vector<Span> Result;
	bool IsStrong = false, IsCode = false;
	const auto AppendLambda = [&](const xr_string& Value, const xr_string& Url = {})
	{
		if (!Result.empty() && Result.back().Url == Url && Result.back().IsStrong == IsStrong && Result.back().IsCode == IsCode)
		{
			Result.back().Text += Value;
		}
		else
		{
			Result.push_back({Value, Url, IsStrong, IsCode});
		}
	};
	for (size_t Position = 0; Position < Text.size();)
	{
		if (Text[Position] == '`' && (IsCode || Text.find('`', Position + 1) != xr_string::npos))
		{
			IsCode = !IsCode;
			++Position;
			continue;
		}
		if (!IsCode && Text.compare(Position, 2, "**") == 0 && (IsStrong || Text.find("**", Position + 2) != xr_string::npos))
		{
			IsStrong = !IsStrong;
			Position += 2;
			continue;
		}
		if (!IsCode && Text[Position] == '[')
		{
			const auto Middle = Text.find("](", Position + 1);
			const auto End = Middle == xr_string::npos ? xr_string::npos : Text.find(')', Middle + 2);
			if (End != xr_string::npos && Middle > Position + 1 && End > Middle + 2)
			{
				AppendLambda(Text.substr(Position + 1, Middle - Position - 1).c_str(), Text.substr(Middle + 2, End - Middle - 2).c_str());
				Position = End + 1;
				continue;
			}
		}
		AppendLambda(Text.substr(Position++, 1).c_str());
	}
	return Result;
}

inline xr_string Overview(const xr_string& Text)
{
	xr_string Result;
	size_t Position = 0, HiddenLevel = 0;
	bool IsInCode = false;
	while (Position < Text.size())
	{
		const auto Newline = Text.find('\n', Position);
		const auto End = Newline == xr_string::npos ? Text.size() : Newline + 1;
		const xr_string Original = Text.substr(Position, End - Position).c_str();
		const auto Line = Original.Trim();
		Position = End;
		if (Line.starts_with("```"))
		{
			IsInCode = !IsInCode;
		}
		const auto Level = Line.find_first_not_of('#');
		if (!IsInCode && Level > 0 && Level <= 6 && Level < Line.size() && Line[Level] == ' ')
		{
			if (HiddenLevel && Level <= HiddenLevel)
			{
				HiddenLevel = 0;
			}
			const xr_string Heading = Line.substr(Level + 1).c_str();
			xr_string Name = Heading.Trim();
			for (auto& Letter : Name)
			{
				if (Letter >= 'A' && Letter <= 'Z')
				{
					Letter += 'a' - 'A';
				}
			}
			if (!HiddenLevel && (Name == "license" || Name == "package files"))
			{
				HiddenLevel = Level;
			}
		}
		if (!HiddenLevel)
		{
			Result += Original;
		}
	}
	return Result;
}
} // namespace SdkPackages::Markdown
