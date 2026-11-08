#pragma once

#include "PackageStore.h"
#include <charconv>

namespace SdkPackages::DependencyFormats::Details
{
static bool Fail(xr_string& Error, const char* Message)
{
	return SdkPackages::Details::Fail(Error, Message);
}
static xr_string Trim(const xr_string& Text)
{
	const auto Begin = Text.find_first_not_of(" \t\r\n");
	return Begin == xr_string::npos ? xr_string{} : Text.substr(Begin, Text.find_last_not_of(" \t\r\n") - Begin + 1);
}
static bool Number(const xr_string& Text, u32& Value)
{
	const auto Result = std::from_chars(Text.data(), Text.data() + Text.size(), Value);
	return Result.ec == std::errc{} && Result.ptr == Text.data() + Text.size();
}
using Fields = xr_map<xr_string, xr_string>;
using SectionCallback = std::function<bool(const xr_string&, const Fields&)>;

static bool ReadLtx(const xr_path& Path, const SectionCallback& SectionLambda, xr_string& Error, std::stop_token Stop, bool AllowEmpty = false)
{
	if (Stop.stop_requested())
	{
		return Fail(Error, "Dependency check cancelled.");
	}
	std::error_code Code;
	if (std::filesystem::file_size(Path, Code) > 64 * 1024 * 1024 || Code)
	{
		return Fail(Error, "Cannot read scene text or it exceeds 64 MiB.");
	}
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return Fail(Error, "Cannot read scene text.");
	}
	xr_array<char, 32768> Buffer{};
	xr_string Section;
	Fields Values;
	xr_string PendingKey, PendingValue;
	bool HasPendingValue = false;
	size_t Lines = 0;
	while (Input.getline(Buffer.data(), Buffer.size()))
	{
		if (++Lines > 1000000 || Stop.stop_requested())
		{
			return Fail(Error, Stop.stop_requested() ? "Dependency check cancelled." : "Scene text exceeds the line limit.");
		}
		xr_string Line = Trim(Buffer.data());
		if (Lines == 1 && Line.starts_with("\xef\xbb\xbf"))
		{
			Line.erase(0, 3);
		}
		bool IsQuoted = HasPendingValue;
		for (size_t Index = 0; Index < Line.size(); ++Index)
		{
			if (Line[Index] == '"')
			{
				IsQuoted = !IsQuoted;
			}
			if (!IsQuoted && (Line[Index] == ';' || (Line[Index] == '/' && Index + 1 < Line.size() && Line[Index + 1] == '/')))
			{
				Line = Trim(Line.substr(0, Index));
				break;
			}
		}
		if (Line.empty())
		{
			continue;
		}
		if (HasPendingValue)
		{
			PendingValue += "\n" + Line;
			if (PendingValue.size() > 65536)
			{
				return Fail(Error, "A multiline scene value exceeds 64 KiB.");
			}
			if (IsQuoted)
			{
				continue;
			}
			if (PendingValue.back() != '"')
			{
				return Fail(Error, "Malformed multiline scene value.");
			}
			Values[PendingKey] = PendingValue.substr(1, PendingValue.size() - 2);
			HasPendingValue = false;
			continue;
		}
		if (Line.front() == '#')
		{
			return Fail(Error, "Scene includes or directives are not followed during dependency checking.");
		}
		if (Line.front() == '[')
		{
			if (!Section.empty() && !SectionLambda(Section, Values))
			{
				return false;
			}
			const auto End = Line.find(']');
			if (End == xr_string::npos || !Trim(Line.substr(End + 1)).empty())
			{
				return Fail(Error, "Malformed or inherited scene section.");
			}
			Section = SdkPackages::Details::Lower(Trim(Line.substr(1, End - 1)));
			Values.clear();
		}
		else
		{
			const auto Equal = Line.find('=');
			if (Section.empty() || Equal == xr_string::npos || Values.size() >= 65536)
			{
				return Fail(Error, "Malformed or oversized scene section.");
			}
			auto Value = Trim(Line.substr(Equal + 1));
			if (!Value.empty() && Value.front() == '"' && IsQuoted)
			{
				PendingKey = SdkPackages::Details::Lower(Trim(Line.substr(0, Equal)));
				PendingValue = Value;
				HasPendingValue = true;
				continue;
			}
			if (Value.size() >= 2 && Value.front() == '"' && Value.back() == '"')
			{
				Value = Value.substr(1, Value.size() - 2);
			}
			Values[SdkPackages::Details::Lower(Trim(Line.substr(0, Equal)))] = std::move(Value);
		}
	}
	if (HasPendingValue)
	{
		return Fail(Error, "Unterminated multiline scene value.");
	}
	if (!Input.eof())
	{
		return Fail(Error, "Cannot read scene text or a line exceeds 32767 bytes.");
	}
	return !Section.empty() ? SectionLambda(Section, Values) : AllowEmpty || Fail(Error, "Scene text has no sections.");
}

struct Range
{
	u64 Begin = 0;
	u64 End = 0;
};

// File ranges skip mesh/slot payloads instead of loading whole models or scenes.
class Reader final
{
public:
	Reader(const xr_path& Path, xr_string& Error, std::stop_token Stop)
		: Input(Path, std::ios::binary), Error(Error), Stop(Stop)
	{
		std::error_code Code;
		Root.End = std::filesystem::file_size(Path, Code);
		if (!Input || Code || Root.End > 2ull * 1024 * 1024 * 1024)
		{
			Fail(Error, "Cannot read resource or it exceeds 2 GiB.");
		}
	}
	~Reader() = default;
	bool IsValid() const { return Error.empty(); }
	bool Read(Range& Cursor, void* Bytes, size_t Size)
	{
		if (!IsValid())
		{
			return false;
		}
		if (Stop.stop_requested())
		{
			return Fail(Error, "Dependency check cancelled.");
		}
		if (Cursor.Begin > Cursor.End || Size > Cursor.End - Cursor.Begin)
		{
			return Fail(Error, "Truncated resource dependency data.");
		}
		if (Position != Cursor.Begin)
		{
			Input.seekg(static_cast<std::streamoff>(Cursor.Begin));
		}
		Input.read(static_cast<char*>(Bytes), Size);
		if (!Input)
		{
			return Fail(Error, "Cannot read resource dependency data.");
		}
		Cursor.Begin += Size;
		Position = Cursor.Begin;
		return true;
	}
	bool Integer(Range& Cursor, u32& Value, size_t Size = 4)
	{
		xr_array<u8, 4> Bytes{};
		if (!Read(Cursor, Bytes.data(), Size))
		{
			return false;
		}
		Value = Bytes[0] | (static_cast<u32>(Bytes[1]) << 8) | (static_cast<u32>(Bytes[2]) << 16) | (static_cast<u32>(Bytes[3]) << 24);
		return true;
	}
	bool String(Range& Cursor, xr_string& Text)
	{
		Text.clear();
		for (size_t Index = 0; Index <= 1024; ++Index)
		{
			char Letter;
			if (!Read(Cursor, &Letter, 1))
			{
				return false;
			}
			if (!Letter)
			{
				return true;
			}
			Text.push_back(Letter);
		}
		return Fail(Error, "Resource reference exceeds 1024 bytes.");
	}
	bool Chunks(Range Area, const std::function<bool(u32, Range)>& ChunkLambda)
	{
		while (Area.Begin < Area.End)
		{
			u32 Id, Size;
			if (++ChunksRead > 1000000 || !Integer(Area, Id) || !Integer(Area, Size))
			{
				return Fail(Error, "Malformed resource or chunk limit exceeded.");
			}
			if (Size > Area.End - Area.Begin)
			{
				return Fail(Error, "Resource chunk extends beyond its file.");
			}
			if (!ChunkLambda(Id, {Area.Begin, Area.Begin + Size}))
			{
				return false;
			}
			Area.Begin += Size;
		}
		return true;
	}
	bool Find(Range Area, u32 Id, Range& Result)
	{
		bool HasFound = false;
		const auto ChunkLambda = [&](u32 Candidate, Range Data)
		{
			if ((Candidate & 0x7fffffff) == Id)
			{
				if (Candidate & 0x80000000)
				{
					return Fail(Error, "Compressed dependency chunks are not supported; the check is incomplete.");
				}
				if (HasFound)
				{
					return Fail(Error, "Duplicate resource dependency chunk.");
				}
				HasFound = true;
				Result = Data;
			}
			return true;
		};
		return Chunks(Area, ChunkLambda) && HasFound;
	}
	Range Root;

private:
	std::ifstream Input;
	u64 Position = std::numeric_limits<u64>::max();
	xr_string& Error;
	std::stop_token Stop;
	size_t ChunksRead = 0;
};

} // namespace SdkPackages::DependencyFormats::Details
