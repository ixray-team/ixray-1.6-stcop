#pragma once

#include "../../../xrCore/xrCore.h"
#include <nlohmann/json.hpp>
#include <fstream>
#include <initializer_list>

namespace SdkPackages
{
struct PackageCardMetadata
{
	xr_string License;
	xr_string SourceUrl;
	xr_string WebsiteUrl;
	xr_string DocumentationUrl;
	xr_string ChangelogUrl;
};

struct PackageDocuments
{
	xr_string Readme;
	xr_string Changelog;
	xr_string License;
	xr_string Notice;
};

namespace Presentation
{
inline constexpr size_t DocumentLimit = 32 * 1024;
inline constexpr size_t CacheLimit = 1024 * 1024;

inline bool Fail(xr_string& Error, const char* Message)
{
	Error = Message;
	return false;
}

inline bool IsUtf8(const xr_string& Text)
{
	for (size_t Index = 0; Index < Text.size();)
	{
		const u8 First = static_cast<u8>(Text[Index++]);
		if (First < 0x80)
		{
			if ((First < 0x20 && First != '\n' && First != '\r' && First != '\t') || First == 0x7f)
			{
				return false;
			}
			continue;
		}
		const size_t Count = First >= 0xc2 && First <= 0xdf ? 1 : First >= 0xe0 && First <= 0xef ? 2
															  : First >= 0xf0 && First <= 0xf4	 ? 3
																								 : 0;
		if (!Count || Index + Count > Text.size())
		{
			return false;
		}
		u32 Value = First & (0x7f >> Count);
		for (size_t Part = 0; Part < Count; ++Part)
		{
			const u8 Next = static_cast<u8>(Text[Index++]);
			if ((Next & 0xc0) != 0x80)
			{
				return false;
			}
			Value = (Value << 6) | (Next & 0x3f);
		}
		if ((Count == 1 && Value < 0x80) || (Count == 2 && Value < 0x800) || (Count == 3 && Value < 0x10000) || Value > 0x10ffff || (Value >= 0xd800 && Value <= 0xdfff))
		{
			return false;
		}
	}
	return true;
}

inline bool IsUtf8Path(const std::filesystem::path& Path)
{
	const auto& Native = Path.native();
	if constexpr (sizeof(std::filesystem::path::value_type) == 1)
	{
		xr_string Bytes;
		Bytes.assign(reinterpret_cast<const char*>(Native.data()), Native.size());
		return IsUtf8(Bytes);
	}
	else
	{
		for (size_t Index = 0; Index < Native.size(); ++Index)
		{
			const auto Value = static_cast<u32>(Native[Index]);
			if constexpr (sizeof(std::filesystem::path::value_type) == 2)
			{
				if (Value >= 0xd800 && Value <= 0xdbff)
				{
					if (++Index == Native.size())
					{
						return false;
					}
					const auto Low = static_cast<u32>(Native[Index]);
					if (Low < 0xdc00 || Low > 0xdfff)
					{
						return false;
					}
					continue;
				}
			}
			if ((Value >= 0xd800 && Value <= 0xdfff) || Value > 0x10ffff)
			{
				return false;
			}
		}
		return true;
	}
}

inline bool Text(const nlohmann::json& Json, const char* Key, xr_string& Result, size_t Limit)
{
	const auto Item = Json.find(Key);
	if (Item == Json.end())
	{
		Result.clear();
		return true;
	}
	if (!Item->is_string())
	{
		return false;
	}
	const auto& Value = Item->get_ref<const nlohmann::json::string_t&>();
	if (Value.size() > Limit)
	{
		return false;
	}
	Result.assign(Value.data(), Value.size());
	return IsUtf8(Result);
}

inline bool IsWebUrl(const xr_string& Url)
{
	if (!Url.starts_with("https://") || Url.size() > 2048)
	{
		return false;
	}
	for (unsigned char Letter : Url)
	{
		if (Letter <= 0x20 || Letter >= 0x7f || Letter == '\\' || Letter == '"' || Letter == '<' || Letter == '>' || Letter == '`')
		{
			return false;
		}
	}
	const auto End = Url.find_first_of("/?#", 8);
	const auto Host = Url.substr(8, End == xr_string::npos ? xr_string::npos : End - 8);
	return !Host.empty() && Host.front() != '.' && Host.back() != '.' && std::all_of(Host.begin(), Host.end(), [](char Letter)
																					 { return (Letter >= 'A' && Letter <= 'Z') || (Letter >= 'a' && Letter <= 'z') || (Letter >= '0' && Letter <= '9') || Letter == '.' || Letter == '-'; });
}

inline bool ParseCard(const nlohmann::json& Json, PackageCardMetadata& Card, xr_string& Error)
{
	Error.clear();
	Card = {};
	if (!Json.is_object() || !Text(Json, "license", Card.License, 256) || !Text(Json, "sourceUrl", Card.SourceUrl, 2048) || !Text(Json, "websiteUrl", Card.WebsiteUrl, 2048) || !Text(Json, "documentationUrl", Card.DocumentationUrl, 2048) || !Text(Json, "changelogUrl", Card.ChangelogUrl, 2048))
	{
		return Fail(Error, "Package card metadata must contain bounded UTF-8 strings.");
	}
	for (const auto* Url : {&Card.SourceUrl, &Card.WebsiteUrl, &Card.DocumentationUrl, &Card.ChangelogUrl})
	{
		if (!Url->empty() && !IsWebUrl(*Url))
		{
			return Fail(Error, "Package source, website, documentation and changelog links must use valid HTTPS URLs.");
		}
	}
	return true;
}

inline bool ParseDocuments(const nlohmann::json& Json, PackageDocuments& Documents, xr_string& Error)
{
	Error.clear();
	Documents = {};
	return (Json.is_object() && Text(Json, "readme", Documents.Readme, DocumentLimit) && Text(Json, "changelog", Documents.Changelog, DocumentLimit) && Text(Json, "licenseText", Documents.License, DocumentLimit) && Text(Json, "notice", Documents.Notice, 4096)) || Fail(Error, "Invalid package documentation cache.");
}

inline nlohmann::json ToJson(const PackageCardMetadata& Card)
{
	return {{"license", Card.License.c_str()}, {"sourceUrl", Card.SourceUrl.c_str()}, {"websiteUrl", Card.WebsiteUrl.c_str()}, {"documentationUrl", Card.DocumentationUrl.c_str()}, {"changelogUrl", Card.ChangelogUrl.c_str()}};
}

inline nlohmann::json ToJson(const PackageDocuments& Documents)
{
	return {{"readme", Documents.Readme.c_str()}, {"changelog", Documents.Changelog.c_str()}, {"licenseText", Documents.License.c_str()}, {"notice", Documents.Notice.c_str()}};
}

inline void FindDocument(const xr_path& Root, const std::initializer_list<const char*>& Names, xr_string& Result, xr_string& Notice)
{
	std::error_code Code;
	if (!std::filesystem::is_directory(Root, Code))
	{
		return;
	}
	size_t Count = 0;
	for (std::filesystem::directory_iterator Iter(Root, Code), End; !Code && Iter != End && Count++ < 4096; Iter.increment(Code))
	{
		if (!IsUtf8Path(Iter->path()))
		{
			Notice += "A documentation filename is not valid Unicode and was skipped.\n";
			continue;
		}
		const auto NameBytes = Iter->path().filename().u8string();
		xr_string Name;
		Name.assign(reinterpret_cast<const char*>(NameBytes.data()), NameBytes.size());
		for (char& Letter : Name)
		{
			if (Letter >= 'A' && Letter <= 'Z')
			{
				Letter += 'a' - 'A';
			}
		}
		if (std::none_of(Names.begin(), Names.end(), [&Name](const char* Candidate)
						 { return Name == Candidate; }))
		{
			continue;
		}
		const auto Status = Iter->symlink_status(Code);
		if (Code || !std::filesystem::is_regular_file(Status))
		{
			Notice += "A documentation link or special file was skipped.\n";
			continue;
		}
		std::ifstream Stream(Iter->path(), std::ios::binary);
		xr_string Bytes;
		Bytes.resize(DocumentLimit + 4);
		Stream.read(Bytes.data(), Bytes.size());
		Bytes.resize(static_cast<size_t>(Stream.gcount()));
		if (Stream.bad() || !Stream.is_open())
		{
			Notice += "A documentation file could not be read.\n";
			continue;
		}
		if (Bytes.starts_with("\xef\xbb\xbf"))
		{
			Bytes.erase(0, 3);
		}
		if (Bytes.size() > DocumentLimit)
		{
			size_t End = DocumentLimit;
			while (End && (static_cast<u8>(Bytes[End]) & 0xc0) == 0x80)
			{
				--End;
			}
			Bytes.resize(End);
			Notice += "A document preview was truncated to 32 KiB.\n";
		}
		if (!IsUtf8(Bytes))
		{
			Notice += "A documentation file is not UTF-8 text and was skipped.\n";
			continue;
		}
		Result = std::move(Bytes);
		return;
	}
}

inline void Discover(const xr_path& Root, const xr_path& Content, PackageDocuments& Documents)
{
	Documents = {};
	for (const auto& Directory : {Root, Content})
	{
		if (Documents.Readme.empty())
		{
			FindDocument(Directory, {"readme.md", "readme.txt", "readme.markdown", "readme"}, Documents.Readme, Documents.Notice);
		}
		if (Documents.Changelog.empty())
		{
			FindDocument(Directory, {"changelog.md", "changelog.txt", "changelog", "changes.md", "changes.txt", "history.md"}, Documents.Changelog, Documents.Notice);
		}
		if (Documents.License.empty())
		{
			FindDocument(Directory, {"license", "license.md", "license.txt", "copying", "copying.txt"}, Documents.License, Documents.Notice);
		}
		if (Root == Content)
		{
			break;
		}
	}
	Documents.Notice.resize(std::min(Documents.Notice.size(), size_t(4096)));
}

inline bool SaveDocuments(const xr_path& Path, const PackageDocuments& Documents, xr_string& Error)
{
	std::error_code Code;
	std::filesystem::create_directories(Path.parent_path(), Code);
	if (Code)
	{
		return Fail(Error, "Cannot create the package documentation cache.");
	}
	const auto Json = ToJson(Documents);
	const auto Bytes = Json.dump(2);
	if (Bytes.size() > CacheLimit)
	{
		return Fail(Error, "Package documentation cache exceeds 1 MiB.");
	}
	std::ofstream Stream(Path, std::ios::binary | std::ios::trunc);
	Stream.write(Bytes.data(), Bytes.size());
	Stream.close();
	return Stream.good() || Fail(Error, "Cannot write the package documentation cache.");
}

inline bool LoadDocuments(const xr_path& Path, PackageDocuments& Documents, xr_string& Error)
{
	Documents = {};
	Error.clear();
	std::error_code Code;
	const auto Status = std::filesystem::symlink_status(Path, Code);
	if (Code == std::errc::no_such_file_or_directory || (!Code && !std::filesystem::exists(Status)))
	{
		return true;
	}
	if (Code || !std::filesystem::is_regular_file(Status) || std::filesystem::file_size(Path, Code) > CacheLimit || Code)
	{
		return Fail(Error, "Package documentation cache is inaccessible or unsafe.");
	}
	std::ifstream Stream(Path, std::ios::binary);
	const auto Json = nlohmann::json::parse(Stream, nullptr, false);
	return ((Stream.good() || Stream.eof()) && ParseDocuments(Json, Documents, Error)) || Fail(Error, "Invalid package documentation cache.");
}
} // namespace Presentation
} // namespace SdkPackages
