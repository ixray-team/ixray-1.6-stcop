#pragma once

#include "PackageComparison.h"
#include "DependencyReader.h"

namespace SdkPackages::ComparisonReader
{
using Range = DependencyFormats::Details::Range;
using Reader = DependencyFormats::Details::Reader;
using Chunks = xr_map<u32, Range>;

struct Value
{
	ComparisonProperty Property;
	Range Payload;
};
struct Snapshot
{
	xr_map<xr_string, Value> Values;
	bool HasOpaqueData = false;
};

inline bool Fail(xr_string& Error, const char* Message)
{
	return SdkPackages::Details::Fail(Error, Message);
}
inline xr_string Number(u64 Value)
{
	string32 Text;
	xr_sprintf(Text, "%llu", static_cast<unsigned long long>(Value));
	return Text;
}
inline xr_string Hex(u32 Value)
{
	string32 Text;
	xr_sprintf(Text, "0x%04X", static_cast<unsigned>(Value));
	return Text;
}
inline bool Text(Reader& Input, Range& Data, xr_string& Result)
{
	if (!Input.String(Data, Result))
	{
		return false;
	}
	if (!Presentation::IsUtf8(Result))
	{
		Result = Platform::ANSI_TO_UTF8(Result.c_str()).c_str();
	}
	return true;
}
inline bool Add(Snapshot& State, const char* Section, const xr_string& Owner, const xr_string& Name, const xr_string& Text, xr_string& Error, Range Data = {}, bool IsRaw = false)
{
	if (State.Values.size() >= 100000)
	{
		return Fail(Error, "File comparison exceeds 100000 properties.");
	}
	const xr_string Key = xr_string(xr_strcmp(Section, "Object") == 0 ? "0:" : xr_strcmp(Section, "Material") == 0 ? "1:"
																												   : "2:") +
						  Section + ":" + Number(Owner.size()) + ":" + Owner + ":" + Name;
	Value Entry;
	Entry.Property.Section = Section;
	Entry.Property.Owner = Owner;
	Entry.Property.Name = Name;
	Entry.Property.Current = Text;
	Entry.Property.IsRawData = IsRaw;
	Entry.Payload = Data;
	return State.Values.emplace(Key, std::move(Entry)).second || Fail(Error, "Duplicate comparison property or resource name.");
}
inline bool Raw(Snapshot& State, const char* Section, const xr_string& Owner, const xr_string& Name, Range Data, xr_string& Error)
{
	return Add(State, Section, Owner, Name, "Present", Error, Data, true);
}
inline bool Tail(Snapshot& State, const char* Section, const xr_string& Owner, u32 Id, Range Data, xr_string& Error)
{
	if (Data.Begin == Data.End)
	{
		return true;
	}
	State.HasOpaqueData = true;
	return Raw(State, Section, Owner, Hex(Id), Data, Error);
}
inline bool Equal(Reader& Current, Range A, Reader& Imported, Range B, bool& IsEqual)
{
	IsEqual = A.End - A.Begin == B.End - B.Begin;
	xr_array<char, 65536> Left{}, Right{};
	while (IsEqual && A.Begin < A.End)
	{
		const auto Size = static_cast<size_t>(std::min<u64>(Left.size(), A.End - A.Begin));
		if (!Current.Read(A, Left.data(), Size) || !Imported.Read(B, Right.data(), Size))
		{
			return false;
		}
		IsEqual = std::equal(Left.begin(), Left.begin() + Size, Right.begin());
	}
	return true;
}
template <typename TSnapshot, typename TDecodeCallback>
inline bool Read(const xr_path& CurrentPath, const xr_path& ImportedPath, AssetComparison& Result, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress, const TDecodeCallback& Decode)
{
	Result = {};
	Error.clear();
	std::error_code Code;
	const auto CurrentTime = std::filesystem::last_write_time(CurrentPath, Code);
	if (Code)
	{
		return Fail(Error, "Cannot inspect the current SDK file.");
	}
	const auto ImportedTime = std::filesystem::last_write_time(ImportedPath, Code);
	if (Code)
	{
		return Fail(Error, "Cannot inspect the package file.");
	}
	Reader Current(CurrentPath, Error, Stop), Imported(ImportedPath, Error, Stop);
	TSnapshot A, B;
	if (!Decode(Current, A, Error) || !Decode(Imported, B, Error))
	{
		return false;
	}
	AssetComparison Candidate;
	Candidate.IsSupported = true;
	xr_map<xr_string, const Value*> Keys;
	for (const auto& [Key, Entry] : A.Values)
	{
		Keys.emplace(Key, &Entry);
	}
	for (const auto& [Key, Entry] : B.Values)
	{
		Keys.try_emplace(Key, nullptr);
	}
	if (Keys.size() > 100000)
	{
		return Fail(Error, "File comparison exceeds 100000 properties.");
	}
	PackageProgress State{"Comparing file contents"};
	State.FilesTotal = Keys.size();
	for (const auto& [Key, Left] : Keys)
	{
		if (Stop.stop_requested())
		{
			return Fail(Error, "File comparison cancelled.");
		}
		const auto Found = B.Values.find(Key);
		const auto* Right = Found == B.Values.end() ? nullptr : &Found->second;
		auto Property = (Left ? Left : Right)->Property;
		Property.HasCurrent = Left != nullptr;
		Property.HasImported = Right != nullptr;
		Property.Current = Left ? Left->Property.Current : xr_string{};
		Property.Imported = Right ? Right->Property.Current : xr_string{};
		Property.IsDifferent = !Left || !Right || Property.Current != Property.Imported;
		if (Left && Right && Property.IsRawData)
		{
			bool IsEqual;
			if (!Equal(Current, Left->Payload, Imported, Right->Payload, IsEqual))
			{
				return false;
			}
			Property.IsDifferent |= !IsEqual;
		}
		Candidate.Properties.push_back(std::move(Property));
		++State.FilesCompleted;
		if (Progress)
		{
			State.Detail = Candidate.Properties.back().Owner + " / " + Candidate.Properties.back().Name;
			Progress(State);
		}
	}
	if (Stop.stop_requested())
	{
		return Fail(Error, "File comparison cancelled.");
	}
	if (std::filesystem::last_write_time(CurrentPath, Code) != CurrentTime || Code || std::filesystem::file_size(CurrentPath, Code) != Current.Root.End || Code || std::filesystem::last_write_time(ImportedPath, Code) != ImportedTime || Code || std::filesystem::file_size(ImportedPath, Code) != Imported.Root.End || Code)
	{
		return Fail(Error, "A file changed during comparison. Reopen Import and try again.");
	}
	Result = std::move(Candidate);
	return true;
}
} // namespace SdkPackages::ComparisonReader
