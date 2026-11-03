#include "stdafx.h"
#include "PackageStore.h"

namespace SdkPackages
{
namespace Details
{
bool Equal(const xr_path& First, const xr_path& Second, xr_string& Error, std::stop_token Stop)
{
	xr_vector<xr_string> FirstFiles, SecondFiles;
	if (!Files(First, FirstFiles, Error, false, Stop) || !Files(Second, SecondFiles, Error, false, Stop))
	{
		return false;
	}
	if (FirstFiles != SecondFiles)
	{
		return Fail(Error, "Package has local file changes. Back them up and restore the installed files before updating or removing.");
	}
	for (const auto& Relative : FirstFiles)
	{
		std::ifstream Left(Join(First, Relative), std::ios::binary), Right(Join(Second, Relative), std::ios::binary);
		if (!Left || !Right)
		{
			return Fail(Error, "Cannot read installed package files.");
		}
		xr_array<char, 65536> LeftBytes{}, RightBytes{};
		do
		{
			if (Stop.stop_requested())
			{
				return Fail(Error, "Package operation cancelled.");
			}
			Left.read(LeftBytes.data(), LeftBytes.size());
			Right.read(RightBytes.data(), RightBytes.size());
			if (Left.gcount() != Right.gcount() || !std::equal(LeftBytes.begin(), LeftBytes.begin() + Left.gcount(), RightBytes.begin()))
			{
				return Fail(Error, "Package has locally modified files. Restore them before updating or removing.");
			}
		} while (Left.gcount() != 0);
		if ((!Left.eof() && Left.fail()) || (!Right.eof() && Right.fail()))
		{
			return Fail(Error, "Cannot compare installed package files.");
		}
	}
	return true;
}
} // namespace Details

namespace Details
{
bool Copy(const xr_path& Source, const xr_path& Target, xr_string& Error, bool IgnoreRepository, std::stop_token Stop, const ProgressCallback& Progress)
{
	xr_vector<xr_string> Paths;
	if (!Files(Source, Paths, Error, IgnoreRepository, Stop))
	{
		return false;
	}
	std::error_code Code;
	PackageProgress Current{"Copying package files"};
	if (Progress)
	{
		for (const auto& Relative : Paths)
		{
			if (Stop.stop_requested())
			{
				return Fail(Error, "Package operation cancelled.");
			}
			const auto Size = std::filesystem::file_size(Join(Source, Relative), Code);
			if (Code || Size > std::numeric_limits<u64>::max() - Current.BytesTotal)
			{
				return Fail(Error, "Cannot determine package copy size.");
			}
			Current.BytesTotal += Size;
		}
	}
	std::filesystem::create_directories(Target, Code);
	for (const auto& Relative : Paths)
	{
		if (Stop.stop_requested())
		{
			return Fail(Error, "Package operation cancelled.");
		}
		if (Code)
		{
			break;
		}
		const xr_path Destination = Join(Target, Relative);
		std::filesystem::create_directories(Destination.parent_path(), Code);
		if (!Code)
		{
			if (std::filesystem::exists(Destination, Code) || Code)
			{
				return Fail(Error, "Package copy destination already exists or is inaccessible.");
			}
			std::ifstream Input(Join(Source, Relative), std::ios::binary);
			std::ofstream Output(Destination, std::ios::binary | std::ios::trunc);
			if (!Input || !Output)
			{
				return Fail(Error, "Cannot open package files for copying.");
			}
			Current.Detail = Relative;
			const auto PreviousBytes = Current.BytesCompleted;
			ProgressCallback FileProgress;
			if (Progress)
			{
				Progress(Current);
				FileProgress = [&](const PackageProgress& Value)
				{
					Current.BytesCompleted = PreviousBytes + Value.BytesCompleted;
					Progress(Current);
				};
			}
			if (!CopyBytes(Input, Output, Error, Stop, FileProgress))
			{
				return false;
			}
			Output.close();
			if (!Input.eof() || !Output)
			{
				return Fail(Error, "Cannot finish copying package files.");
			}
			const auto Permissions = std::filesystem::status(Join(Source, Relative), Code).permissions();
			if (!Code)
			{
				std::filesystem::permissions(Destination, Permissions, Code);
			}
		}
	}
	return !Code || Fail(Error, "Cannot copy package files; check permissions and free disk space.");
}
} // namespace Details

namespace Details
{
bool CopyBytes(std::istream& Input, std::ostream& Output, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress)
{
	xr_array<char, 65536> Bytes;
	PackageProgress Current;
	do
	{
		if (Stop.stop_requested())
		{
			return Fail(Error, "Package operation cancelled.");
		}
		Input.read(Bytes.data(), Bytes.size());
		Output.write(Bytes.data(), Input.gcount());
		if (!Output)
		{
			return Fail(Error, "Cannot write package files; check free disk space.");
		}
		if (Progress && Input.gcount())
		{
			Current.BytesCompleted += static_cast<u64>(Input.gcount());
			Progress(Current);
		}
	} while (Input.gcount());
	return Input.eof() || Fail(Error, "Cannot finish reading package files.");
}
} // namespace Details

namespace Details
{
bool Files(const xr_path& Root, xr_vector<xr_string>& Result, xr_string& Error, bool IgnoreRepository, std::stop_token Stop)
{
	if (Stop.stop_requested())
	{
		return Fail(Error, "Package operation cancelled.");
	}
	std::error_code Code;
	if (std::filesystem::is_symlink(std::filesystem::symlink_status(Root, Code)) || Code || !std::filesystem::is_directory(Root, Code) || Code)
	{
		return Fail(Error, "Package content must be a directory without symbolic links.");
	}
	xr_dir_recursive_iter Iter(Root, Code), End;
	while (!Code && Iter != End)
	{
		if (Stop.stop_requested())
		{
			return Fail(Error, "Package operation cancelled.");
		}
		const auto Status = Iter->symlink_status(Code);
		if (Code || std::filesystem::is_symlink(Status))
		{
			return Fail(Error, "Symbolic links are not supported in packages.");
		}
		if (!Presentation::IsUtf8Path(Iter->path()))
		{
			return Fail(Error, "Package filenames must be valid Unicode.");
		}
		const auto Relative = Iter->path().lexically_relative(Root);
		const auto Name = Iter->path().filename();
		if (IgnoreRepository && (Name == ".git" || Name == ".github"))
		{
			if (Iter->is_directory(Code))
			{
				Iter.disable_recursion_pending();
			}
		}
		else if (std::filesystem::is_regular_file(Status))
		{
			Result.emplace_back(Utf8(Relative));
		}
		else if (!std::filesystem::is_directory(Status))
		{
			return Fail(Error, "Unsupported file type in package.");
		}
		Iter.increment(Code);
	}
	if (Code)
	{
		return Fail(Error, "Cannot enumerate package files.");
	}
	std::sort(Result.begin(), Result.end());
	return true;
}
} // namespace Details

namespace Details
{
bool NormalizeLocalSource(PackageInfo& Info, xr_string& Error)
{
	if (Info.LocalPath.empty() || !Presentation::IsUtf8(Info.LocalPath) || Info.Source == PackageSource::GitHub)
	{
		return Fail(Error, "Select a local package folder or ZIP file.");
	}
	xr_path Path = FromUtf8(Info.LocalPath);
	std::error_code Code;
	if (Info.Source == PackageSource::Folder && Path.xfilename() == "package.json")
	{
		Path = Path.parent_path();
	}
	Path = std::filesystem::canonical(Path, Code);
	if (Code || !Presentation::IsUtf8Path(Path) || (Info.Source == PackageSource::Folder ? !std::filesystem::is_directory(Path, Code) : !std::filesystem::is_regular_file(Path, Code)) || Code)
	{
		return Fail(Error, "The original local package source is missing or inaccessible.");
	}
	if (Info.Source == PackageSource::Zip && Lower(Utf8(Path.extension())) != ".zip")
	{
		return Fail(Error, "Select a .zip archive.");
	}
	Info.LocalPath = Utf8(Path);
	if (Info.LocalPath.size() >= 4096)
	{
		return Fail(Error, "Local package path exceeds the 4095 byte limit.");
	}
	Info.Url.clear();
	Info.Ref.clear();
	Info.Commit.clear();
	return true;
}
} // namespace Details

namespace Details
{
bool SamePath(const xr_path& First, const xr_path& Second)
{
	if constexpr (std::filesystem::path::preferred_separator == '\\')
	{
		return Lower(Utf8(First)) == Lower(Utf8(Second));
	}
	return First == Second;
}
} // namespace Details

namespace Details
{
bool IsWithin(const xr_path& Path, const xr_path& Parent)
{
	std::error_code Code;
	const auto ResolvedParent = std::filesystem::weakly_canonical(Parent, Code);
	if (Code)
	{
		return false;
	}
	const auto ResolvedPath = std::filesystem::weakly_canonical(Path, Code);
	if (Code || ResolvedParent == ResolvedPath)
	{
		return false;
	}
	auto Component = ResolvedPath.begin();
	for (const auto& Part : ResolvedParent)
	{
		if (Component == ResolvedPath.end() || *Component != Part)
		{
			return false;
		}
		++Component;
	}
	return true;
}
} // namespace Details

namespace Details
{
bool WriteJson(const xr_path& Path, const nlohmann::json& Json, xr_string& Error)
{
	std::ofstream Stream(Path, std::ios::binary | std::ios::trunc);
	Stream << std::setw(2) << Json << '\n';
	Stream.close();
	return Stream.good() || Fail(Error, "Cannot write package metadata.");
}
} // namespace Details

namespace Details
{
bool ReadJson(const xr_path& Path, nlohmann::json& Json, xr_string& Error, size_t Limit)
{
	std::error_code Code;
	const auto Size = std::filesystem::file_size(Path, Code);
	if (Code || Size > Limit)
	{
		return Fail(Error, "Package metadata is missing or exceeds its size limit.");
	}
	std::ifstream Stream(Path, std::ios::binary);
	xr_string Bytes;
	Bytes.resize(static_cast<size_t>(Size) + 1);
	Stream.read(Bytes.data(), Bytes.size());
	Bytes.resize(static_cast<size_t>(Stream.gcount()));
	if (!Stream.is_open() || Stream.bad() || Bytes.size() > Size)
	{
		return Fail(Error, "Cannot read package JSON metadata or its size exceeds the limit.");
	}
	Json = nlohmann::json::parse(Bytes.begin(), Bytes.end(), nullptr, false);
	return Json.is_object() || Fail(Error, "Invalid package JSON metadata.");
}
} // namespace Details

namespace Details
{
bool ReadString(const nlohmann::json& Json, const char* Key, xr_string& Value, bool IsRequired, size_t MaxSize)
{
	const auto Entry = Json.find(Key);
	if (Entry == Json.end())
	{
		return !IsRequired;
	}
	if (!Entry->is_string())
	{
		return false;
	}
	Value = Entry->get_ref<const nlohmann::json::string_t&>().c_str();
	return Value.size() <= MaxSize && (!IsRequired || !Value.empty());
}
} // namespace Details

namespace Details
{
bool IsRef(const xr_string& Ref)
{
	if (Ref.size() > 200 || Ref.starts_with('-') || Ref.find("..") != xr_string::npos)
	{
		return false;
	}
	return std::all_of(Ref.begin(), Ref.end(), [](char Letter)
					   { return IsAlphaNumeric(Letter) || Letter == '/' || Letter == '-' || Letter == '_' || Letter == '.'; });
}
} // namespace Details

namespace Details
{
bool IsId(const xr_string& Id)
{
	if (Id.empty() || Id.size() > 96 || !IsAlphaNumeric(Id.front()) || !IsAlphaNumeric(Id.back()) || Id != Lower(Id))
	{
		return false;
	}
	for (char Letter : Id)
	{
		if (!IsAlphaNumeric(Letter) && Letter != '.' && Letter != '-' && Letter != '_')
		{
			return false;
		}
	}
	const xr_string Stem = Id.substr(0, Id.find('.'));
	if (Stem == "con" || Stem == "prn" || Stem == "aux" || Stem == "nul")
	{
		return false;
	}
	return !(Stem.size() == 4 && (Stem.starts_with("com") || Stem.starts_with("lpt")) && Stem[3] >= '1' && Stem[3] <= '9');
}
} // namespace Details

namespace Details
{
xr_string Lower(const xr_string& Input)
{
	xr_string Value = Input;
	for (char& Letter : Value)
	{
		if (Letter >= 'A' && Letter <= 'Z')
		{
			Letter += 'a' - 'A';
		}
	}
	return Value;
}
} // namespace Details

namespace Details
{
bool IsAlphaNumeric(char Value)
{
	return (Value >= 'a' && Value <= 'z') || (Value >= 'A' && Value <= 'Z') || (Value >= '0' && Value <= '9');
}
} // namespace Details

namespace Details
{
bool Fail(xr_string& Error, const char* Message)
{
	Error = Message;
	return false;
}
} // namespace Details

namespace Details
{
bool ParseKind(const xr_string& Name, PackageKind& Kind)
{
	for (PackageKind Candidate : {PackageKind::Plugin, PackageKind::Assets})
	{
		if (Name == KindName(Candidate))
		{
			Kind = Candidate;
			return true;
		}
	}
	return false;
}
} // namespace Details

namespace Details
{
const char* KindName(PackageKind Kind)
{
	switch (Kind)
	{
		case PackageKind::Plugin: return "sdk-plugin";
		case PackageKind::Assets: return "assets";
	}
	return "";
}
} // namespace Details

namespace Details
{
bool ParseSource(const xr_string& Name, PackageSource& Source)
{
	for (PackageSource Candidate : {PackageSource::GitHub, PackageSource::Folder, PackageSource::Zip})
	{
		if (Name == SourceName(Candidate))
		{
			Source = Candidate;
			return true;
		}
	}
	return false;
}
} // namespace Details

namespace Details
{
const char* SourceName(PackageSource Source)
{
	switch (Source)
	{
		case PackageSource::GitHub: return "github";
		case PackageSource::Folder: return "folder";
		case PackageSource::Zip: return "zip";
	}
	return "";
}
} // namespace Details

PackageStore::PackageStore(const xr_path& SdkRoot)
{
	std::error_code Code;
	Root = std::filesystem::weakly_canonical(SdkRoot, Code);
	if (Code)
	{
		Root = std::filesystem::absolute(SdkRoot, Code).lexically_normal();
	}
}

bool PackageStore::ParseGitHubUrl(const xr_string& Input, xr_string& Url, xr_string& Id, xr_string& Error)
{
	Error.clear();
	const xr_string Prefix = "https://github.com/";
	if (!Input.starts_with(Prefix) || Input.size() > 512)
	{
		return Details::Fail(Error, "Use an HTTPS repository URL: https://github.com/owner/repository");
	}
	xr_string Repository = Input.substr(Prefix.size());
	if (Repository.ends_with('/'))
	{
		Repository.pop_back();
	}
	if (Repository.ends_with(".git"))
	{
		Repository.resize(Repository.size() - 4);
	}
	const auto Slash = Repository.find('/');
	if (Slash == xr_string::npos || Slash == 0 || Slash + 1 == Repository.size() || Repository.find('/', Slash + 1) != xr_string::npos)
	{
		return Details::Fail(Error, "Use the repository root URL. Enter the branch or tag separately.");
	}
	for (char Letter : Repository)
	{
		if (!Details::IsAlphaNumeric(Letter) && Letter != '/' && Letter != '-' && Letter != '_' && Letter != '.')
		{
			return Details::Fail(Error, "Invalid character in GitHub repository URL.");
		}
	}
	Id = Details::Lower(Repository);
	Id[Slash] = '.';
	if (!Details::IsId(Id) || Repository.find("..") != xr_string::npos || Repository[Slash + 1] == '-')
	{
		return Details::Fail(Error, "Invalid GitHub repository name.");
	}
	Url = Prefix + Repository + ".git";
	return true;
}

bool PackageStore::ParseManifest(const nlohmann::json& Json, PackageInfo& Info, xr_string& ContentName, xr_string& Error)
{
	Error.clear();
	xr_string Kind;
	if (!Json.is_object() || !Presentation::Text(Json, "id", Info.Id, 96) || !Details::IsId(Info.Id) ||
		!Presentation::Text(Json, "name", Info.Name, 256) || !Presentation::Text(Json, "version", Info.Version, 128) ||
		!Presentation::Text(Json, "description", Info.Description, 2048) || !Presentation::Text(Json, "author", Info.Author, 256) ||
		!Presentation::Text(Json, "type", Kind, 32) || !Details::ParseKind(Kind, Info.Kind) ||
		!Presentation::Text(Json, "content", ContentName, 2048) || !Presentation::ParseCard(Json, Info.Card, Error))
	{
		return Details::Fail(Error, "Invalid package.json metadata. Provide a valid id and type; display fields must be bounded UTF-8 strings.");
	}
	if (Info.Name.empty())
	{
		Info.Name = Info.Id;
	}
	if (Info.Version.empty())
	{
		Info.Version = "unversioned";
	}
	if (!Json.contains("content"))
	{
		ContentName = ".";
	}
	const auto Relative = FromUtf8(ContentName);
	if (Relative.empty() || Relative.is_absolute() || Relative.has_root_name() || ContentName.find('\\') != xr_string::npos || ContentName.find(':') != xr_string::npos)
	{
		return Details::Fail(Error, "Package content must be a relative directory inside its repository.");
	}
	for (const auto& Part : Relative)
	{
		if (Part == "..")
		{
			return Details::Fail(Error, "Package content cannot escape the repository.");
		}
	}
	return true;
}

bool PackageStore::InstallCheckout(const xr_path& Checkout, const PackageInfo& Source, bool IsUpdate, xr_string& Error, PackageInfo* InstalledInfo, bool RequireKindMatch, std::stop_token Stop, const ProgressCallback& Progress) const
{
	Error.clear();
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Package operation cancelled.");
	}
	const auto LockPath = Join(GetStateRoot(), "transaction.lock");
	if (!Managed(LockPath, Error))
	{
		return false;
	}
	Platform::FileLock StorageLock(LockPath);
	if (!StorageLock.IsLocked())
	{
		return Details::Fail(Error, "SDK package storage is busy or inaccessible. Close other package operations and retry.");
	}

	if (Progress)
	{
		Progress({"Validating package", Source.Name});
	}
	PackageInfo Info = Source;
	xr_path Content;
	if (!Manifest(Checkout, Info, Content, Error, Stop))
	{
		return false;
	}
	if (RequireKindMatch && Info.Kind != Source.Kind)
	{
		return Details::Fail(Error, "Package type does not match its catalog entry.");
	}
	const xr_path Record = Join(Join(GetStateRoot(), "installed"), Info.Id);
	const xr_path Destination = GetDestination(Info);
	if (Info.Source != PackageSource::GitHub)
	{
		const xr_path Original = FromUtf8(Info.LocalPath);
		if (Details::SamePath(Original, Root) || Details::IsWithin(Root, Original))
		{
			return Details::Fail(Error, "Local source must be separate from SDK package storage and cannot contain the SDK directory.");
		}
		for (const char* Directory : {".ixray-packages", "packages", "plugins/packages"})
		{
			const xr_path Storage = Join(Root, Directory);
			if (Details::SamePath(Original, Storage) || Details::IsWithin(Original, Storage) || Details::IsWithin(Storage, Original))
			{
				return Details::Fail(Error, "Local sources cannot overlap SDK package storage. Select the original folder or ZIP outside managed packages.");
			}
		}
	}
	if (!Managed(Record, Error) || !Managed(Destination, Error))
	{
		return false;
	}
	std::error_code Code;
	const bool HasRecord = std::filesystem::exists(Record, Code);
	if (Code || HasRecord != IsUpdate)
	{
		return Details::Fail(Error, IsUpdate ? "The installed package record is missing." : "Package is already installed. Use Update.");
	}
	if (IsUpdate)
	{
		PackageInfo Previous;
		if (!ReadRecord(Info.Id, Previous, Error) || Previous.Kind != Info.Kind || Previous.Source != Info.Source || Previous.Url != Info.Url ||
			(Info.Source != PackageSource::GitHub && !Details::SamePath(FromUtf8(Previous.LocalPath), FromUtf8(Info.LocalPath))) ||
			!Details::Equal(Destination, Join(Record, "payload"), Error, Stop))
		{
			if (Error.empty())
			{
				Error = "An update cannot change the package ID, type or original source.";
			}
			return false;
		}
	}
	else if (std::filesystem::exists(Destination, Code) || Code)
	{
		return Details::Fail(Error, "Destination already exists and is not owned by this package manager.");
	}

	xr_path Stage;
	if (!CreateStage(Stage, Error))
	{
		return false;
	}
	Details::ScopedDirectory Cleanup(Stage);
	const xr_path NewRecord = Join(Stage, "record");
	auto Metadata = ToJson(Info);
	Metadata["transaction"] = Utf8(static_cast<const std::filesystem::path&>(Stage).filename()).c_str();
	const auto CopyProgressLambda = [&](const char* StageName)
	{
		return ProgressCallback([&, StageName](const PackageProgress& Input)
								{
			if (Progress)
			{
				auto Value = Input;
				Value.Stage = StageName;
				Progress(Value);
			} });
	};
	if (!Details::Copy(Content, Join(NewRecord, "payload"), Error, true, Stop, Progress ? CopyProgressLambda("Staging package files") : ProgressCallback{}) ||
		!Details::Copy(Join(NewRecord, "payload"), Join(Stage, "deploy"), Error, false, Stop, Progress ? CopyProgressLambda("Preparing installation") : ProgressCallback{}) ||
		!Details::WriteJson(Join(NewRecord, "package.json"), Metadata, Error))
	{
		return false;
	}
	if (Progress)
	{
		Progress({"Saving package documentation", Info.Name});
	}
	PackageDocuments Documents;
	Presentation::Discover(Checkout, Content, Documents);
	if (!Presentation::SaveDocuments(Join(NewRecord, "documents.json"), Documents, Error))
	{
		return false;
	}
	std::filesystem::create_directories(Destination.parent_path(), Code);
	if (!Code)
	{
		std::filesystem::create_directories(Record.parent_path(), Code);
	}
	bool HasOldRecord = false, HasOldDeploy = false, HasNewDeploy = false, HasNewRecord = false;
	if (IsUpdate && !Details::Equal(Destination, Join(Record, "payload"), Error, Stop))
	{
		return false;
	}
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Package operation cancelled.");
	}
	if (!Details::WriteJson(Join(Stage, "transaction.json"), {{"operation", IsUpdate ? "update" : "install"}, {"id", Info.Id.c_str()}, {"type", Details::KindName(Info.Kind)}}, Error))
	{
		return false;
	}
	if (Progress)
	{
		Progress({"Applying package installation", Info.Name});
	}
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Package operation cancelled.");
	}
	// Preserve staged backups if an unexpected exception interrupts the commit.
	Cleanup.Keep = true;
	if (!Code && IsUpdate)
	{
		std::filesystem::rename(Record, Join(Stage, "old-record"), Code);
		HasOldRecord = !Code;
		if (!Code)
		{
			std::filesystem::rename(Destination, Join(Stage, "old-deploy"), Code);
			HasOldDeploy = !Code;
		}
	}
	if (!Code)
	{
		if (IsUpdate && !Details::Equal(Join(Stage, "old-deploy"), Join(Join(Stage, "old-record"), "payload"), Error, Stop))
		{
			Code = std::make_error_code(std::errc::state_not_recoverable);
		}
	}
	if (!Code)
	{
		std::filesystem::rename(Join(Stage, "deploy"), Destination, Code);
		HasNewDeploy = !Code;
	}
	if (!Code)
	{
		std::filesystem::rename(NewRecord, Record, Code);
		HasNewRecord = !Code;
	}
	if (!Code && !Details::WriteJson(Join(Stage, "committed.json"), {{"committed", true}}, Error))
	{
		Code = std::make_error_code(std::errc::io_error);
	}
	if (Code)
	{
		std::error_code RestoreCode;
		if (HasNewRecord)
		{
			std::filesystem::rename(Record, NewRecord, RestoreCode);
		}
		if (!RestoreCode && HasNewDeploy)
		{
			std::filesystem::rename(Destination, Join(Stage, "deploy"), RestoreCode);
		}
		if (!RestoreCode && HasOldDeploy)
		{
			std::filesystem::rename(Join(Stage, "old-deploy"), Destination, RestoreCode);
		}
		if (!RestoreCode && HasOldRecord)
		{
			std::filesystem::rename(Join(Stage, "old-record"), Record, RestoreCode);
		}
		Cleanup.Keep = bool(RestoreCode) || !RetireTransaction(Stage);
		Error = RestoreCode ? "Cannot restore package backup. Recovery files: " + Utf8(Stage) : "Cannot replace package files. The previous installation was restored.";
		if (!RestoreCode && Stop.stop_requested())
		{
			Error = "Package operation cancelled. Previous installation restored.";
		}
		return false;
	}
	if (InstalledInfo)
	{
		*InstalledInfo = Info;
	}
	Cleanup.Keep = !RetireTransaction(Stage);
	return true;
}

bool PackageStore::Remove(const xr_string& Id, xr_string& Error, std::stop_token Stop) const
{
	Error.clear();
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Package operation cancelled.");
	}
	const auto LockPath = Join(GetStateRoot(), "transaction.lock");
	if (!Managed(LockPath, Error))
	{
		return false;
	}
	Platform::FileLock StorageLock(LockPath);
	if (!StorageLock.IsLocked())
	{
		return Details::Fail(Error, "SDK package storage is busy or inaccessible. Close other package operations and retry.");
	}

	PackageInfo Info;
	if (!ReadRecord(Id, Info, Error))
	{
		return false;
	}
	const xr_path Record = Join(Join(GetStateRoot(), "installed"), Id);
	const xr_path Destination = GetDestination(Info);
	if (!Managed(Destination, Error) || !Details::Equal(Destination, Join(Record, "payload"), Error, Stop))
	{
		return false;
	}
	xr_path Stage;
	if (!CreateStage(Stage, Error))
	{
		return false;
	}
	Details::ScopedDirectory Cleanup(Stage);
	std::error_code Code;
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Package operation cancelled.");
	}
	if (!Details::WriteJson(Join(Stage, "transaction.json"), {{"operation", "remove"}, {"id", Info.Id.c_str()}, {"type", Details::KindName(Info.Kind)}}, Error))
	{
		return false;
	}
	Cleanup.Keep = true;
	std::filesystem::rename(Destination, Join(Stage, "removed"), Code);
	if (Code)
	{
		Cleanup.Keep = false;
		return Details::Fail(Error, "Cannot remove package files. Close programs using them and retry.");
	}
	std::filesystem::rename(Record, Join(Stage, "record"), Code);
	const bool HasMovedRecord = !Code;
	if (!Code && !Details::Equal(Join(Stage, "removed"), Join(Join(Stage, "record"), "payload"), Error, Stop))
	{
		Code = std::make_error_code(std::errc::state_not_recoverable);
	}
	if (!Code && !Details::WriteJson(Join(Stage, "committed.json"), {{"committed", true}}, Error))
	{
		Code = std::make_error_code(std::errc::io_error);
	}
	if (Code)
	{
		std::error_code RestoreCode;
		std::filesystem::rename(Join(Stage, "removed"), Destination, RestoreCode);
		if (!RestoreCode && HasMovedRecord)
		{
			std::filesystem::rename(Join(Stage, "record"), Record, RestoreCode);
		}
		Cleanup.Keep = bool(RestoreCode) || !RetireTransaction(Stage);
		Error = RestoreCode ? "Cannot restore package backup. Recovery files: " + Utf8(Stage) : "Cannot remove package metadata. The installation was restored.";
		return false;
	}
	Cleanup.Keep = !RetireTransaction(Stage);
	return true;
}

bool PackageStore::Recover(xr_string& Error) const
{
	Error.clear();
	const auto Parent = Join(GetStateRoot(), "staging");
	if (!Managed(Parent, Error))
	{
		return false;
	}
	std::error_code Code;
	if (!std::filesystem::exists(Parent, Code))
	{
		return !Code || Details::Fail(Error, "Cannot inspect package recovery storage.");
	}
	bool HasTransactions = false;
	for (xr_dir_iter Iter(Parent, Code), End; !Code && Iter != End; Iter.increment(Code))
	{
		if (std::filesystem::exists(Join(Iter->path(), "transaction.json"), Code) || std::filesystem::exists(Join(Iter->path(), "finished.json"), Code))
		{
			HasTransactions = true;
			break;
		}
	}
	if (Code)
	{
		return Details::Fail(Error, "Cannot inspect package recovery storage.");
	}
	if (!HasTransactions)
	{
		return true;
	}
	const auto LockPath = Join(GetStateRoot(), "transaction.lock");
	if (!Managed(LockPath, Error))
	{
		return false;
	}
	Platform::FileLock StorageLock(LockPath);
	if (!StorageLock.IsLocked())
	{
		return Details::Fail(Error, "SDK package storage is busy or inaccessible. Close other package operations and retry.");
	}
	for (xr_dir_iter Iter(Parent, Code), End; !Code && Iter != End; Iter.increment(Code))
	{
		const auto Stage = Iter->path();
		const auto Journal = Join(Stage, "transaction.json");
		if (!std::filesystem::exists(Journal, Code))
		{
			if (Code)
			{
				break;
			}
			if (std::filesystem::exists(Join(Stage, "finished.json"), Code))
			{
				xr_vector<xr_string> Files;
				if (!Managed(Stage, Error) || !Details::Files(Stage, Files, Error))
				{
					return false;
				}
				std::filesystem::remove_all(Stage, Code);
				if (Code)
				{
					break;
				}
			}
			continue;
		}
		if (!Managed(Stage, Error) || !Managed(Journal, Error) || !RecoverTransaction(Stage, Error))
		{
			return false;
		}
	}
	return !Code || Details::Fail(Error, "Cannot enumerate package recovery storage.");
}

xr_vector<PackageInfo> PackageStore::List(xr_string& Error) const
{
	Error.clear();
	xr_vector<PackageInfo> Result;
	const xr_path Records = Join(GetStateRoot(), "installed");
	if (!Managed(Records, Error))
	{
		return Result;
	}
	std::error_code Code;
	if (!std::filesystem::exists(Records, Code))
	{
		return Result;
	}
	xr_dir_iter Iter(Records, Code), End;
	while (!Code && Iter != End)
	{
		PackageInfo Info;
		xr_string RecordError;
		if (!Presentation::IsUtf8Path(Iter->path()))
		{
			RecordError = "Installed package records must have valid Unicode filenames.";
		}
		else if (ReadRecord(Utf8(Iter->path().filename()), Info, RecordError))
		{
			Result.push_back(Info);
		}
		if (Error.empty() && !RecordError.empty())
		{
			Error = RecordError;
		}
		Iter.increment(Code);
	}
	if (Code)
	{
		Error = "Cannot read installed package list.";
	}
	std::sort(Result.begin(), Result.end(), [](const PackageInfo& Left, const PackageInfo& Right)
			  { return Left.Id < Right.Id; });
	return Result;
}

bool PackageStore::ReadDocuments(const xr_string& Id, PackageDocuments& Documents, xr_string& Error) const
{
	Error.clear();
	Documents = {};
	if (!Details::IsId(Id))
	{
		return Details::Fail(Error, "Invalid package ID.");
	}
	const auto Record = Join(Join(GetStateRoot(), "installed"), Id);
	return Managed(Record, Error) && Presentation::LoadDocuments(Join(Record, "documents.json"), Documents, Error);
}

bool PackageStore::CreateStage(xr_path& Stage, xr_string& Error) const
{
	const xr_path Parent = Join(GetStateRoot(), "staging");
	if (!Managed(Parent, Error))
	{
		return false;
	}
	std::error_code Code;
	std::filesystem::create_directories(Parent, Code);
	if (Code)
	{
		return Details::Fail(Error, "Cannot create package storage in the SDK directory.");
	}
	for (u32 Attempt = 0; Attempt < 10; ++Attempt)
	{
		Stage = Join(Parent, xr_string("job-") + std::to_string(SDL_GetPerformanceCounter()).c_str());
		if (std::filesystem::create_directory(Stage, Code))
		{
			return true;
		}
		if (Code)
		{
			break;
		}
	}
	return Details::Fail(Error, "Cannot create package staging directory.");
}

xr_path PackageStore::GetDestination(const PackageInfo& Info) const
{
	const char* Directory = "packages";
	if (Info.Kind == PackageKind::Plugin)
	{
		Directory = "plugins/packages";
	}
	return Join(Join(Root, Directory), Info.Id);
}

bool PackageStore::RetireTransaction(const xr_path& Stage)
{
	std::error_code Code;
	std::filesystem::rename(Join(Stage, "transaction.json"), Join(Stage, "finished.json"), Code);
	return !Code;
}

bool PackageStore::RecoverTransaction(const xr_path& Stage, xr_string& Error) const
{
	nlohmann::json Journal;
	xr_string Operation, Kind;
	PackageInfo Info;
	if (!Details::ReadJson(Join(Stage, "transaction.json"), Journal, Error) ||
		!Details::ReadString(Journal, "id", Info.Id) || !Details::IsId(Info.Id) ||
		!Details::ReadString(Journal, "type", Kind) || !Details::ParseKind(Kind, Info.Kind) ||
		!Details::ReadString(Journal, "operation", Operation) || (Operation != "install" && Operation != "update" && Operation != "remove"))
	{
		return Details::Fail(Error, "Invalid package recovery journal. Recovery files were preserved.");
	}
	const auto Record = Join(Join(GetStateRoot(), "installed"), Info.Id);
	const auto Destination = GetDestination(Info);
	xr_vector<xr_string> Files;
	if (!Managed(Record, Error) || !Managed(Destination, Error) || !Details::Files(Stage, Files, Error))
	{
		return false;
	}
	std::error_code Code;
	const auto ExistsLambda = [&](const xr_path& Path)
	{ return std::filesystem::exists(Path, Code); };
	const auto MoveLambda = [&](const xr_path& From, const xr_path& To)
	{
		if (Code || ExistsLambda(To))
		{
			return false;
		}
		std::filesystem::create_directories(To.parent_path(), Code);
		if (!Code)
		{
			std::filesystem::rename(From, To, Code);
		}
		return !Code;
	};
	nlohmann::json Marker;
	xr_string MarkerError;
	const bool IsCommitted = Details::ReadJson(Join(Stage, "committed.json"), Marker, MarkerError) && Marker.contains("committed") && Marker["committed"].is_boolean() && Marker["committed"].get<bool>();
	const auto NewRecord = Join(Stage, "record");
	const auto Deploy = Join(Stage, "deploy");
	const auto OldRecord = Join(Stage, "old-record");
	const auto OldDeploy = Join(Stage, "old-deploy");
	const auto Removed = Join(Stage, "removed");
	if (!IsCommitted && Operation != "remove")
	{
		const auto Published = ExistsLambda(NewRecord) ? NewRecord : Record;
		nlohmann::json Metadata;
		xr_string Transaction;
		if (!Details::ReadJson(Join(Published, "package.json"), Metadata, Error) || !Presentation::Text(Metadata, "transaction", Transaction, 256) || Transaction != Utf8(static_cast<const std::filesystem::path&>(Stage).filename()))
		{
			return Details::Fail(Error, "Recovery record does not belong to this transaction. Existing files were preserved.");
		}
	}
	bool HasRestored = true;
	if (!IsCommitted && Operation == "remove")
	{
		if (ExistsLambda(NewRecord))
		{
			HasRestored = MoveLambda(NewRecord, Record);
		}
		if (HasRestored && ExistsLambda(Removed))
		{
			HasRestored = Details::Equal(Removed, Join(Record, "payload"), Error) && MoveLambda(Removed, Destination);
		}
	}
	else if (!IsCommitted)
	{
		const bool HasOldRecord = ExistsLambda(OldRecord);
		if ((Operation == "install" || HasOldRecord) && ExistsLambda(Record))
		{
			HasRestored = !ExistsLambda(NewRecord) && MoveLambda(Record, NewRecord);
		}
		if (HasRestored && ExistsLambda(Destination) && (Operation == "install" || ExistsLambda(OldDeploy)))
		{
			HasRestored = !ExistsLambda(Deploy) && Details::Equal(Destination, Join(NewRecord, "payload"), Error) && MoveLambda(Destination, Deploy);
		}
		if (HasRestored && Operation == "update")
		{
			if (ExistsLambda(OldDeploy))
			{
				HasRestored = MoveLambda(OldDeploy, Destination);
			}
			if (HasRestored && HasOldRecord)
			{
				HasRestored = Details::Equal(Destination, Join(OldRecord, "payload"), Error) && MoveLambda(OldRecord, Record);
			}
			if (HasRestored)
			{
				HasRestored = ExistsLambda(Record) && ExistsLambda(Destination);
			}
		}
	}
	if (!HasRestored || Code)
	{
		Error = "Cannot safely recover package transaction. Recovery files: " + Utf8(Stage);
		return false;
	}
	if (!RetireTransaction(Stage))
	{
		return Details::Fail(Error, "Package recovered, but its journal could not be retired.");
	}
	std::filesystem::remove_all(Stage, Code);
	return !Code || Details::Fail(Error, "Package recovered, but its staging files could not be cleaned up.");
}

bool PackageStore::Managed(const xr_path& Path, xr_string& Error) const
{
	if (!Presentation::IsUtf8Path(Path))
	{
		return Details::Fail(Error, "SDK package storage paths must be valid Unicode.");
	}
	std::error_code Code;
	const auto Absolute = std::filesystem::absolute(Path, Code).lexically_normal();
	const auto Resolved = std::filesystem::weakly_canonical(Path, Code);
	return (!Code && Details::SamePath(Resolved, Absolute) && Details::IsWithin(Path, Root)) || Details::Fail(Error, "Package storage contains a symbolic link or escapes the SDK directory.");
}

nlohmann::json PackageStore::ToJson(const PackageInfo& Info)
{
	nlohmann::json Json = {{"id", Info.Id.c_str()}, {"name", Info.Name.c_str()}, {"version", Info.Version.c_str()}, {"description", Info.Description.c_str()}, {"author", Info.Author.c_str()}, {"type", Details::KindName(Info.Kind)}, {"source", Details::SourceName(Info.Source)}, {"localPath", Info.LocalPath.c_str()}, {"url", Info.Url.c_str()}, {"ref", Info.Ref.c_str()}, {"commit", Info.Commit.c_str()}};
	Json.update(Presentation::ToJson(Info.Card));
	return Json;
}

bool PackageStore::ReadRecord(const xr_string& Id, PackageInfo& Info, xr_string& Error) const
{
	if (!Details::IsId(Id))
	{
		return Details::Fail(Error, "Invalid package ID.");
	}
	const xr_path Record = Join(Join(GetStateRoot(), "installed"), Id);
	nlohmann::json Json;
	xr_string Kind;
	xr_string Source = "github";
	if (!Managed(Record, Error) || !Details::ReadJson(Join(Record, "package.json"), Json, Error))
	{
		return false;
	}
	if (!Details::ReadString(Json, "id", Info.Id, true) || Info.Id != Id || !Details::ReadString(Json, "name", Info.Name, true) ||
		!Details::ReadString(Json, "version", Info.Version) || !Details::ReadString(Json, "url", Info.Url) ||
		!Details::ReadString(Json, "source", Source) || !Details::ParseSource(Source, Info.Source) || !Details::ReadString(Json, "localPath", Info.LocalPath, false, 4096) ||
		!Details::ReadString(Json, "description", Info.Description) || !Details::ReadString(Json, "author", Info.Author) ||
		!Details::ReadString(Json, "ref", Info.Ref) || !Details::ReadString(Json, "commit", Info.Commit) ||
		!Details::ReadString(Json, "type", Kind, true) || !Details::ParseKind(Kind, Info.Kind))
	{
		return Details::Fail(Error, "Invalid installed package record.");
	}
	if (Info.Source == PackageSource::GitHub ? Info.Url.empty() : Info.LocalPath.empty())
	{
		return Details::Fail(Error, "Installed package source is missing.");
	}
	return Presentation::ParseCard(Json, Info.Card, Error);
}

bool PackageStore::Manifest(const xr_path& Checkout, PackageInfo& Info, xr_path& Content, xr_string& Error, std::stop_token Stop)
{
	xr_string CanonicalUrl, DerivedId;
	if (Info.Source == PackageSource::GitHub)
	{
		if (!ParseGitHubUrl(Info.Url, CanonicalUrl, DerivedId, Error) || !Details::IsRef(Info.Ref))
		{
			return Details::Fail(Error, "Invalid package repository or branch/tag.");
		}
		Info.Url = CanonicalUrl;
	}
	else if (!Details::NormalizeLocalSource(Info, Error))
	{
		return false;
	}
	const xr_string RequestedId = Info.Id;
	Info.Id = DerivedId;
	Info.Name = DerivedId;
	Info.Version = "unversioned";
	Info.Description.clear();
	Info.Author.clear();
	Info.Card = {};
	xr_string ContentName = ".";
	const xr_path ManifestPath = Join(Checkout, "package.json");
	std::error_code Code;
	if (std::filesystem::exists(ManifestPath, Code))
	{
		nlohmann::json Json;
		if (!Details::ReadJson(ManifestPath, Json, Error) || !ParseManifest(Json, Info, ContentName, Error))
		{
			return false;
		}
	}
	else if (Info.Source != PackageSource::GitHub)
	{
		return Details::Fail(Error, "Local packages require package.json at their root.");
	}
	else if (Info.Kind == PackageKind::Plugin && std::filesystem::is_directory(Join(Checkout, "plugins"), Code))
	{
		ContentName = "plugins";
	}
	if (!Details::IsId(Info.Id) || Info.Name.empty() || Info.Name.size() > 256 || Info.Version.size() > 128 || Info.Author.size() > 256)
	{
		return Details::Fail(Error, "Invalid package ID, name or version.");
	}
	// An update is bound to its installed identity even if the remote manifest changes.
	if (!RequestedId.empty() && RequestedId != Info.Id)
	{
		return Details::Fail(Error, "Package ID does not match the requested installation.");
	}
	const xr_path Relative = FromUtf8(ContentName);
	if (Relative.empty() || Relative.is_absolute() || Relative.has_root_name() || ContentName.find('\\') != xr_string::npos || ContentName.find(':') != xr_string::npos)
	{
		return Details::Fail(Error, "Package content must be a relative directory inside its repository.");
	}
	for (const auto& Part : Relative)
	{
		if (Part == "..")
		{
			return Details::Fail(Error, "Package content cannot escape the repository.");
		}
	}
	Content = Join(Checkout, Relative).lexically_normal();
	const bool IsCheckoutRoot = std::filesystem::equivalent(Content, Checkout, Code);
	if (!IsCheckoutRoot && !Details::IsWithin(Content, Checkout))
	{
		return Details::Fail(Error, "Package content escapes the repository.");
	}
	xr_vector<xr_string> Paths;
	if (!Details::Files(Content, Paths, Error, true, Stop))
	{
		return false;
	}
	bool HasPlugin = false;
	for (const auto& Path : Paths)
	{
		const auto Extension = Details::Lower(Utf8(FromUtf8(Path).extension()));
		HasPlugin |= Extension == ".lua" || Extension == ".py";
	}
	if (Info.Kind == PackageKind::Assets && (Paths.empty() || (IsCheckoutRoot && Paths.size() == 1 && Paths.front() == "package.json")))
	{
		return Details::Fail(Error, "Asset package content must contain at least one file.");
	}
	if (Info.Kind == PackageKind::Plugin && !HasPlugin)
	{
		return Details::Fail(Error, "No Lua or Python plugin found in package content.");
	}
	return true;
}
} // namespace SdkPackages
