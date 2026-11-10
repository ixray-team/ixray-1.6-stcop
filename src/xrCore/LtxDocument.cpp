#include "stdafx.h"
#include "LtxDocument.h"
#include <fstream>
#include <filesystem>
#include <iterator>

namespace LtxText
{
static xr_vector<size_t> Lines(const xr_string& Text)
{
	xr_vector<size_t> Result{0};
	for (size_t Index = 0; Index < Text.size(); ++Index)
	{
		if (Text[Index] == '\n' || (Text[Index] == '\r' && (Index + 1 == Text.size() || Text[Index + 1] != '\n')))
		{
			Result.push_back(Index + 1);
		}
	}
	Result.push_back(Text.size());
	return Result;
}

static size_t Comment(const xr_string& Text)
{
	bool IsQuoted = false;
	for (size_t Index = 0; Index < Text.size(); ++Index)
	{
		if (Text[Index] == '"')
		{
			IsQuoted = !IsQuoted;
		}
		if (!IsQuoted && (Text[Index] == ';' || (Text[Index] == '/' && Index + 1 < Text.size() && Text[Index + 1] == '/')))
		{
			return Index;
		}
	}
	return xr_string::npos;
}

static bool Read(const xr_string& File, xr_string& Text)
{
	std::ifstream Stream(xr_path(File.c_str()), std::ios::binary);
	if (!Stream)
	{
		return false;
	}
	Text.assign(std::istreambuf_iterator<char>(Stream), std::istreambuf_iterator<char>());
	return !Stream.bad();
}

static bool Write(const xr_string& File, const xr_string& Text)
{
	std::ofstream Stream(xr_path(File.c_str()), std::ios::binary | std::ios::trunc);
	if (!Stream)
	{
		return false;
	}
	Stream.write(Text.data(), static_cast<std::streamsize>(Text.size()));
	Stream.flush();
	const bool IsWritten = Stream.good();
	Stream.close();
	xr_string Check;
	return IsWritten && !Stream.fail() && Read(File, Check) && Check == Text;
}

static bool PhysicalPath(const xr_string& File, xr_string& Result, xr_string& Error)
{
	const auto* Entry = FS.exist(File.c_str());
	if (!Entry || Entry->vfs != u32(-1))
	{
		Error = "Source is packed or unavailable. Use Copy LTX: " + File;
		return false;
	}
	Result = Entry->wrap ? Entry->wrap : Entry->name;
	if (!FS.can_modify_file(Result.c_str()))
	{
		Error = "Source is read-only. Use Copy LTX: " + Result;
		return false;
	}
	return true;
}

static bool ExistingParents(const CInifile& Config, const char* Parents, xr_string& Error)
{
	if (!Parents || !Parents[0])
	{
		return true;
	}
	const xr_string Names = Parents;
	for (const auto& Part : Names.Split())
	{
		const xr_string Name = Part.Trim();
		if (!Config.section_exist(Name.c_str()))
		{
			Error = "Missing parent section: " + Name;
			return false;
		}
	}
	return true;
}

static bool ValidParents(const char* Parents)
{
	if (!Parents || !Parents[0])
	{
		return true;
	}
	xr_string Names = Parents;
	xr_vector<xr_string> Parts;
	Names.Split(Parts);
	for (auto& Part : Parts)
	{
		const size_t First = Part.find_first_not_of(" \t");
		const size_t Last = Part.find_last_not_of(" \t");
		if (First == xr_string::npos || !LtxDocument::IsIdentifier(xr_string_view(Part.data() + First, Last - First + 1)))
		{
			return false;
		}
	}
	return !Parts.empty();
}
} // namespace LtxText

bool LtxDocument::IsIdentifier(xr_string_view Text)
{
	if (Text.empty())
	{
		return false;
	}
	for (char Value : Text)
	{
		if (!((Value >= 'a' && Value <= 'z') || (Value >= '0' && Value <= '9') || Value == '_' || Value == '-' || Value == '.'))
		{
			return false;
		}
	}
	return true;
}

bool LtxDocument::Load(const char* FileName, xr_string& Error)
{
	const CInifile::SourceTextMap Empty;
	auto Loaded = xr_make_unique<CInifile>(FileName, Empty);
	if (!Loaded || !Loaded->GetSourceError().empty() || !Loaded->section_count())
	{
		Error = Loaded ? Loaded->GetSourceError() : "Cannot open configuration";
		return false;
	}
	RootFile = FileName;
	SavedTexts = Texts = Loaded->GetSourceTexts();
	Lists = Loaded->GetSourceLists();
	Config = std::move(Loaded);
	Error.clear();
	return true;
}

bool LtxDocument::Restore(const CInifile::SourceTextMap& NewTexts, xr_string& Error)
{
	auto Loaded = xr_make_unique<CInifile>(RootFile.c_str(), NewTexts, &Lists);
	if (!Loaded || !Loaded->GetSourceError().empty())
	{
		Error = Loaded ? Loaded->GetSourceError() : "Cannot evaluate configuration";
		return false;
	}
	Texts = Loaded->GetSourceTexts();
	Config = std::move(Loaded);
	Error.clear();
	return true;
}

xr_unique_ptr<CInifile> LtxDocument::CreateConfig() const
{
	return xr_make_unique<CInifile>(RootFile.c_str(), Texts, &Lists);
}

bool LtxDocument::Set(const char* Section, const char* Key, const char* Value, xr_string& Error)
{
	if (!Config || !IsIdentifier(Section) || !IsIdentifier(Key) || (Value && (strpbrk(Value, "\r\n;\"") || strstr(Value, "//"))))
	{
		Error = "Invalid section, key or single-line value";
		return false;
	}
	const auto* Source = Config->GetEditSource(Section, Key);
	if (const auto* Effective = Config->GetSource(Section, Key); Effective && Effective->Section == Section && Effective->Key != Key)
	{
		Error = "A DLTX list modifier supplies this field. Edit its source or use Copy LTX.";
		return false;
	}
	if (!Source)
	{
		Error = "Cannot find the section's source";
		return false;
	}
	return Replace(*Source, Key, Value, false, Error);
}

bool LtxDocument::SetParents(const char* Section, const char* Parents, xr_string& Error)
{
	if (!Config || !LtxText::ValidParents(Parents))
	{
		Error = "Invalid comma-separated parent names";
		return false;
	}
	if (!LtxText::ExistingParents(*Config, Parents, Error))
	{
		return false;
	}
	// Edit the base header; existing DLTX parent modifiers retain their normal precedence.
	for (const auto& Source : Config->GetSourceLocations())
	{
		if (Source.Section == Section && Source.Key.empty() && !Source.IsOverride)
		{
			return Replace(Source, nullptr, Parents, true, Error);
		}
	}
	Error = "Cannot find the base section header";
	return false;
}

bool LtxDocument::Replace(const CInifile::SourceLocation& Source, const char* Key, const char* Value, bool IsParents, xr_string& Error)
{
	auto Changed = Texts;
	xr_string& Text = Changed[Source.File];
	const auto Offsets = LtxText::Lines(Text);
	if (!Source.Line || Source.Line >= Offsets.size())
	{
		Error = "Source location is no longer valid";
		return false;
	}
	const size_t Begin = Offsets[Source.Line - 1];
	const size_t End = Offsets[std::min<size_t>(Source.EndLine, Offsets.size() - 1)];
	xr_string Line = Text.substr(Begin, End - Begin);
	const xr_string Eol = Line.ends_with("\r\n") ? "\r\n" : Line.ends_with("\n") ? "\n"
														: Line.ends_with("\r")	 ? "\r"
																				 : "";
	const size_t CommentAt = LtxText::Comment(Line);
	xr_string Suffix = Eol;
	if (CommentAt != xr_string::npos)
	{
		Suffix = " ";
		Suffix.append(Line, CommentAt, xr_string::npos);
	}
	if (IsParents)
	{
		const size_t Close = Line.find(']');
		if (Close == xr_string::npos)
		{
			Error = "Invalid section header";
			return false;
		}
		xr_string Header = Line.substr(0, Close + 1);
		if (Value && Value[0])
		{
			Header += ":";
			Header += Value;
		}
		Text.replace(Begin, End - Begin, Header + Suffix);
	}
	else if (!Source.Key.empty())
	{
		if (Config->DuplicateLineCount(Source.Section.c_str(), Key) > 1)
		{
			Error = "Duplicate local keys must be resolved in the source file first";
			return false;
		}
		if (!Value)
		{
			Text.replace(Begin, End - Begin, CommentAt != xr_string::npos ? Line.substr(0, Line.find_first_not_of(" \t")) + Line.substr(CommentAt) : "");
		}
		else
		{
			const size_t Equals = Line.find('=');
			size_t ValueBegin = Equals == xr_string::npos ? 0 : Line.find_first_not_of(" \t", Equals + 1);
			if (ValueBegin == xr_string::npos || ValueBegin > (CommentAt == xr_string::npos ? Line.size() - Eol.size() : CommentAt))
			{
				ValueBegin = Equals + 1;
			}
			xr_string Prefix = Equals == xr_string::npos ? xr_string(Key) + " = " : Line.substr(0, ValueBegin);
			// A DLTX deletion line becomes a regular assignment when enabled again.
			const size_t First = Prefix.find_first_not_of(" \t");
			if (First != xr_string::npos && Prefix[First] == '!')
			{
				Prefix.erase(First, 1);
			}
			Text.replace(Begin, End - Begin, Prefix + Value + Suffix);
		}
	}
	else if (Value)
	{
		const xr_string Newline = Text.find("\r\n") != xr_string::npos ? "\r\n" : "\n";
		Text.insert(End, (Eol.empty() ? Newline : "") + xr_string(Key) + " = " + Value + Newline);
	}
	return Restore(Changed, Error);
}

bool LtxDocument::AddSection(const char* File, const char* Section, const char* Parents, xr_string& Error)
{
	const bool IsDefined = Config && std::any_of(Config->GetSourceLocations().begin(), Config->GetSourceLocations().end(), [&](const auto& Source)
												 { return Source.Section == Section; });
	if (!Config || !IsIdentifier(Section) || !LtxText::ValidParents(Parents) || IsDefined || Config->section_exist(Section) || !Texts.contains(File))
	{
		Error = "Section name is invalid, already exists, or the target file is not loaded";
		return false;
	}
	if (!LtxText::ExistingParents(*Config, Parents, Error))
	{
		return false;
	}
	auto Changed = Texts;
	Changed[File] += xr_string("\r\n[") + Section + "]" + (Parents && Parents[0] ? xr_string(":") + Parents : "") + "\r\n";
	return Restore(Changed, Error);
}

xr_vector<xr_string> LtxDocument::GetChangedFiles() const
{
	xr_vector<xr_string> Result;
	for (const auto& [File, Text] : Texts)
	{
		const auto It = SavedTexts.find(File);
		if (It == SavedTexts.end() || It->second != Text)
		{
			Result.push_back(File);
		}
	}
	return Result;
}

bool LtxDocument::IsDirty() const
{
	return Texts != SavedTexts;
}

bool LtxDocument::CanSave(xr_string& Error) const
{
	// Parent configs and newly added DLTX files can change the meaning of an edit too.
	for (const auto& [File, Expected] : SavedTexts)
	{
		xr_string Current;
		const auto* Entry = FS.exist(File.c_str());
		bool IsSame = false;
		if (Entry && Entry->vfs == u32(-1))
		{
			const xr_string Physical = Entry->wrap ? Entry->wrap : Entry->name;
			IsSame = LtxText::Read(Physical, Current) && Current == Expected;
		}
		else if (Entry)
		{
			auto Reader = FS.rg_open(File.c_str());
			IsSame = Reader && xr_string_view(static_cast<const char*>(Reader->pointer()), Reader->length()) == xr_string_view(Expected.data(), Expected.size());
		}
		if (!IsSame)
		{
			Error = "Configuration changed on disk. Reload before saving: " + File;
			return false;
		}
	}
	for (const auto& [Key, List] : Lists)
	{
		FS_FileSet Files;
		FS.file_list(Files, List.Directory.c_str(), FS_ListFiles, List.Mask.c_str());
		xr_vector<xr_string> Names;
		for (const auto& File : Files)
		{
			Names.push_back(File.name);
		}
		if (Names != List.Names)
		{
			Error = "Included/DLTX file list changed. Reload before saving: " + List.Directory;
			return false;
		}
	}
	for (const auto& File : GetChangedFiles())
	{
		xr_string Physical, Current;
		if (!LtxText::PhysicalPath(File, Physical, Error))
		{
			return false;
		}
		const auto Saved = SavedTexts.find(File);
		if (Saved == SavedTexts.end() || !LtxText::Read(Physical, Current) || Current != Saved->second)
		{
			Error = "Source changed on disk. Reload before saving: " + File;
			return false;
		}
	}
	Error.clear();
	return true;
}

bool LtxDocument::Save(xr_string& Error)
{
	if (!CanSave(Error))
	{
		return false;
	}
	struct PendingFile
	{
		xr_string File;
		xr_string Physical;
		bool IsMoved = false;
		bool IsInstalled = false;
	};
	xr_vector<PendingFile> Pending;
	for (const auto& File : GetChangedFiles())
	{
		PendingFile Entry;
		Entry.File = File;
		if (!LtxText::PhysicalPath(File, Entry.Physical, Error))
		{
			return false;
		}
		std::error_code Code;
		if (std::filesystem::exists(xr_path((Entry.Physical + ".dof.rollback").c_str()), Code) || Code)
		{
			Error = "A previous recovery file needs attention: " + Entry.Physical + ".dof.rollback";
			return false;
		}
		if (!LtxText::Write(Entry.Physical + ".dof.tmp", Texts.at(File)) || !LtxText::Write(Entry.Physical + ".bak", SavedTexts.at(File)))
		{
			Error = "Cannot prepare and verify temporary file / backup: " + Entry.Physical;
			return false;
		}
		Pending.push_back(std::move(Entry));
	}
	if (!CanSave(Error))
	{
		return false;
	}
	for (auto& Entry : Pending)
	{
		std::error_code Code;
		std::filesystem::rename(xr_path(Entry.Physical.c_str()), xr_path((Entry.Physical + ".dof.rollback").c_str()), Code);
		Entry.IsMoved = !Code;
		if (!Code)
		{
			std::filesystem::rename(xr_path((Entry.Physical + ".dof.tmp").c_str()), xr_path(Entry.Physical.c_str()), Code);
			Entry.IsInstalled = !Code;
		}
		xr_string Check;
		if (Code || !LtxText::Read(Entry.Physical, Check) || Check != Texts.at(Entry.File))
		{
			Error = "Save failed: " + Entry.Physical;
			for (auto It = Pending.rbegin(); It != Pending.rend(); ++It)
			{
				if (!It->IsMoved)
				{
					continue;
				}
				std::error_code Recovery;
				if (It->IsInstalled)
				{
					std::filesystem::remove(xr_path(It->Physical.c_str()), Recovery);
				}
				if (!Recovery)
				{
					std::filesystem::rename(xr_path((It->Physical + ".dof.rollback").c_str()), xr_path(It->Physical.c_str()), Recovery);
				}
				xr_string Restored;
				if (Recovery || !LtxText::Read(It->Physical, Restored) || Restored != SavedTexts.at(It->File))
				{
					Error += "\nRecovery failed; keep .bak/.dof.rollback: " + It->Physical;
				}
				FS.file_update(It->Physical.c_str());
			}
			return false;
		}
	}
	xr_string CleanupWarning;
	for (const auto& Entry : Pending)
	{
		std::error_code Code;
		std::filesystem::remove(xr_path((Entry.Physical + ".dof.rollback").c_str()), Code);
		if (Code)
		{
			CleanupWarning += "Saved, but could not remove recovery file: " + Entry.Physical + ".dof.rollback\n";
		}
		FS.file_update(Entry.Physical.c_str());
		if (Entry.Physical != Entry.File)
		{
			FS.file_update(Entry.File.c_str());
		}
	}
	SavedTexts = Texts;
	Error = std::move(CleanupWarning);
	return true;
}

xr_string LtxDocument::ExportChanges() const
{
	xr_string Result;
	if (!Config)
	{
		return Result;
	}
	const auto Before = xr_make_unique<CInifile>(RootFile.c_str(), SavedTexts, &Lists);
	if (!Before || !Before->GetSourceError().empty())
	{
		return Result;
	}
	for (const auto& Section : Config->sections())
	{
		const char* Name = Section.Name.c_str();
		xr_hash_set<shared_str> Keys;
		auto LocalKeysLambda = [&](const CInifile& Ini)
		{
			if (const auto* Found = Ini.r_section_nullable(Name))
			{
				for (const auto& Entry : Found->Data)
				{
					const auto* Source = Entry.first.size() ? Ini.GetSource(Name, Entry.first.c_str()) : nullptr;
					if (Source && Source->Section == Name)
					{
						Keys.insert(Entry.first);
					}
				}
			}
		};
		LocalKeysLambda(*Config);
		LocalKeysLambda(*Before);
		xr_string Body;
		for (const auto& Key : Keys)
		{
			const auto* Old = Before->GetSource(Name, Key.c_str());
			const auto* New = Config->GetSource(Name, Key.c_str());
			const char* OldValue = Before->r_string_nullable(Name, Key.c_str());
			const char* NewValue = Config->r_string_nullable(Name, Key.c_str());
			const bool WasLocal = Old && Old->Section == Name;
			const bool IsLocal = New && New->Section == Name;
			if (WasLocal && (!IsLocal || (New && Old->File != New->File)))
			{
				Body += "; Remove local key '" + xr_string(Key.c_str()) + "' from " + Old->File + "\r\n";
			}
			if (IsLocal && (!WasLocal || xr_string_view(OldValue ? OldValue : "") != xr_string_view(NewValue ? NewValue : "")))
			{
				Body += "; Target: " + New->File + (New->IsOverride ? " (DLTX override)\r\n" : "\r\n");
				Body += xr_string(Key.c_str()) + " = " + (NewValue ? NewValue : "") + "\r\n";
			}
		}
		const auto* Old = Before->GetSource(Name);
		const auto* New = Config->GetSource(Name);
		const bool ChangedParents = New && (!Old || Old->Parents != New->Parents);
		if (!Body.empty() || ChangedParents)
		{
			Result += "; Merge changes into the named source section; removal comments require deleting the old key.\r\n";
			if (New)
			{
				Result += "; Section: " + New->File + "\r\n";
			}
			Result += xr_string("[") + Name + "]" + (New && !New->Parents.empty() ? ":" + New->Parents : "") + "\r\n" + Body + "\r\n";
		}
	}
	return Result;
}
