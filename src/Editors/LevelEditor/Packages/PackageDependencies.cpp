#include "stdafx.h"
#include "PackageDependencies.h"
#include "DependencyFormats.h"
#include "ImportPaths.h"
#include "TextureDependencyFormats.h"

namespace SdkPackages
{
namespace DependencyDetails
{
static bool IsPart(const xr_string& Name)
{
	const auto Position = Name.rfind(".part");
	return Position != xr_string::npos && std::all_of(Name.begin() + Position + 5, Name.end(), [](char Letter)
													  { return Letter >= '0' && Letter <= '9'; });
}
static bool IsRelevantPart(const xr_string& Name)
{
	return IsPart(Name) && (Name.starts_with("scene_object.part") || Name.starts_with("group.part") || Name.starts_with("glow.part") || Name.starts_with("light.part") || Name.starts_with("detail_object.part") || Name.starts_with("spawn.part"));
}
static bool Normalize(xr_string& Name, const char* Extension)
{
	std::replace(Name.begin(), Name.end(), '\\', '/');
	if (Name.empty() || Name.size() > 1024 || Name.front() == '/' || Name.find(':') != xr_string::npos || !Presentation::IsUtf8(Name))
	{
		return false;
	}
	const auto Lower = Details::Lower(Name);
	for (const char* Suffix : {Extension, ".dds", ".tga", ".bmp", ".ogm"})
	{
		if (Lower.ends_with(Suffix))
		{
			Name.resize(Name.size() - xr_strlen(Suffix));
			break;
		}
	}
	const auto Path = FromUtf8(Name);
	for (const auto& Part : Path)
	{
		if (Part == ".." || Part == "." || Part.empty())
		{
			return false;
		}
	}
	if (Name.empty() || Name.back() == '/')
	{
		return false;
	}
	Name += Extension;
	return true;
}

class Scan final
{
public:
	Scan(const PackageStore& Store, const ImportPlan& Plan, DependencyReport& Report, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress)
		: Store(Store), Plan(Plan), Report(Report), Error(Error), Stop(Stop), Progress(Progress)
	{
		for (const auto& File : Plan.Files)
		{
			if (!File.Destination.empty())
			{
				PackageFiles.emplace(ImportDetails::Key(File.Destination), &File);
			}
		}
	}
	~Scan() = default;
	bool Run()
	{
		for (const auto& File : Plan.Files)
		{
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Dependency check cancelled.");
			}
			if (!File.IsSelected || File.Alias != "$maps$")
			{
				continue;
			}
			const auto Lower = Details::Lower(File.Relative);
			if (Lower.ends_with(".level"))
			{
				auto Directory = File.Destination;
				Directory.replace_extension();
				LevelDirectories.insert(ImportDetails::Key(Directory));
				if (!CheckScene(File.Destination, File.Relative, File.Relative, false))
				{
					return false;
				}
				const auto Prefix = File.Relative.substr(0, File.Relative.size() - 6);
				for (const auto& Part : Plan.Files)
				{
					if (Stop.stop_requested())
					{
						return Details::Fail(Error, "Dependency check cancelled.");
					}
					const auto Name = Utf8(static_cast<const std::filesystem::path&>(Part.Destination).filename());
					if (Part.Alias == "$maps$" && Details::SamePath(Part.Destination.parent_path(), Directory) && IsRelevantPart(Details::Lower(Name)))
					{
						if (!CheckScene(Part.Destination, Part.Relative, File.Relative, true))
						{
							return false;
						}
					}
				}
				std::error_code Code;
				const auto* Mapping = Root("$maps$");
				const bool IsSafeDirectory = Mapping && !Mapping->Destination.empty() && ImportDetails::IsSafe(Join(Directory, "dependency-check"), Mapping->Destination);
				if (!IsSafeDirectory)
				{
					const auto Status = std::filesystem::symlink_status(Directory, Code);
					if ((!Code && std::filesystem::exists(Status)) || (Code && Code != std::errc::no_such_file_or_directory))
					{
						Warn(File.Relative, "Scene parts directory is unsafe or inaccessible; its dependencies could not be checked.");
					}
				}
				else if (std::filesystem::is_directory(Directory, Code))
				{
					for (std::filesystem::directory_iterator Iter(Directory, Code), End; !Code && Iter != End; Iter.increment(Code))
					{
						if (Stop.stop_requested())
						{
							return Details::Fail(Error, "Dependency check cancelled.");
						}
						const auto Name = Utf8(Iter->path().filename());
						if (IsRelevantPart(Details::Lower(Name)) && !CheckScene(Iter->path(), Prefix + "/" + Name, File.Relative, true))
						{
							return false;
						}
					}
					if (Code)
					{
						Warn(File.Relative, "Some existing scene parts could not be enumerated.");
					}
				}
				else if ((Code && Code != std::errc::no_such_file_or_directory) || (!Code && std::filesystem::exists(Directory, Code)))
				{
					Warn(File.Relative, "Scene parts path is inaccessible or is not a directory; the dependency check is incomplete.");
				}
				const auto Key = ImportDetails::Key(Directory);
				if (!ObjectDirectories.contains(Key))
				{
					Warn(File.Relative, "No readable scene object data was found. Include the level's scene_object.part and related parts.");
				}
			}
		}
		// Importing only parts of an existing level also needs a dependency check.
		for (const auto& File : Plan.Files)
		{
			if (File.IsSelected && File.Alias == "$maps$" && IsRelevantPart(Details::Lower(Utf8(static_cast<const std::filesystem::path&>(File.Destination).filename()))))
			{
				LevelDirectories.insert(ImportDetails::Key(File.Destination.parent_path()));
				if (!CheckScene(File.Destination, File.Relative, File.Relative, true))
				{
					return false;
				}
			}
		}
		Report.Levels = LevelDirectories.size();
		constexpr size_t MaxObjectReaders = 4;
		const size_t ReaderCount = std::min(MaxObjectReaders, std::max<size_t>(1, xr_max_concurrency()));
		for (size_t Begin = 0; Begin < Objects.size();)
		{
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Dependency check cancelled.");
			}
			const size_t Count = std::min(ReaderCount, Objects.size() - Begin);
			ReportProgress(Objects[Begin].Name, Begin, Objects.size());
			xr_array<ObjectReadResult, MaxObjectReaders> Results;
			const auto ReadLambda = [&](size_t Offset)
			{
				auto& Result = Results[Offset];
				Result.IsReadable = DependencyFormats::ReadObject(Objects[Begin + Offset].Source, Result.References, Result.Error, Stop);
			};
			if (Count == 1)
			{
				ReadLambda(0);
			}
			else
			{
				xr_parallel_for(size_t{0}, Count, ReadLambda);
			}
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Dependency check cancelled.");
			}
			// Readers touch only their result slots; merge in discovery order after every task joins.
			for (size_t Offset = 0; Offset < Count; ++Offset)
			{
				const auto Object = Objects[Begin + Offset];
				if (Offset)
				{
					ReportProgress(Object.Name, Begin + Offset, Objects.size());
				}
				if (!Results[Offset].IsReadable)
				{
					Warn(Object.Name, Results[Offset].Error);
				}
				if (!Follow(Results[Offset].References, Object.Name))
				{
					return false;
				}
			}
			Begin += Count;
		}
		for (size_t Index = 0; Index < Textures.size(); ++Index)
		{
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Dependency check cancelled.");
			}
			const auto Texture = Textures[Index];
			ReportProgress(Texture.Name, Index, Textures.size());
			const auto Description = DescribeTexture(Texture.Name);
			if ((Texture.Usage & 1) && !Description.Bump.empty() && !AddBump(Description.Bump, Texture.Name + " (bump)"))
			{
				return false;
			}
			if ((Texture.Usage & 2) && Description.HasDetail && Description.DetailUsage && !Description.Detail.empty())
			{
				if (!AddBinding(Description.Detail, Texture.Name + " (detail)", true, (Description.DetailUsage & 2) ? 1 : 0))
				{
					return false;
				}
			}
			if (!Error.empty())
			{
				return false;
			}
		}
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Dependency check cancelled.");
		}
		ReportProgress("Dependency check complete", Objects.size(), Objects.size());
		std::stable_sort(Report.Files.begin(), Report.Files.end(), [](const auto& Left, const auto& Right)
						 { return Left.Status != Right.Status ? Left.Status < Right.Status : Left.Name < Right.Name; });
		return true;
	}

private:
	struct Resolved
	{
		DependencyFile File;
		xr_path Source;
	};
	struct Object
	{
		xr_string Name;
		xr_path Source;
	};
	struct ObjectReadResult
	{
		xr_vector<DependencyFormats::Reference> References;
		xr_string Error;
		bool IsReadable = false;
	};
	struct Texture
	{
		xr_string Name;
		u32 Usage;
	};
	void ReportProgress(const xr_string& Name, size_t Completed = 0, size_t Total = 0) const
	{
		if (Progress)
		{
			PackageProgress State{"Checking level dependencies", Name};
			State.FilesCompleted = Completed;
			State.FilesTotal = Total;
			Progress(State);
		}
	}
	void Warn(const xr_string& File, const xr_string& Message, const xr_string& RequiredBy = {}, const xr_path& Source = {}, DependencyWarningKind Kind = DependencyWarningKind::IncompleteCheck, const xr_string& Remedy = {})
	{
		if (Report.Warnings.size() < 10000)
		{
			Report.Warnings.push_back({File, Message.empty() ? xr_string("Resource dependencies could not be read completely.") : Message, RequiredBy, Source, Remedy, Kind});
		}
	}
	const ImportRoot* Root(const char* Alias) const
	{
		const auto Found = std::find_if(Plan.Roots.begin(), Plan.Roots.end(), [&](const auto& Entry)
										{ return Entry.Alias == Alias; });
		return Found != Plan.Roots.end() ? &*Found : nullptr;
	}
	Resolved Resolve(DependencyKind Kind, const xr_string& Name, const xr_string& Owner, const xr_path& Destination, const ImportRoot* Mapping)
	{
		xr_string Key = std::to_string(static_cast<int>(Kind)).c_str();
		Key += ":";
		Key += Mapping ? Mapping->Alias : xr_string{};
		Key += ":";
		Key += Utf8(Destination.lexically_normal());
		if (const auto Cached = ResolvedResources.find(Key); Cached != ResolvedResources.end())
		{
			auto Result = Cached->second;
			Result.File.Name = Name;
			Result.File.RequiredBy = Owner;
			return Result;
		}
		auto Result = ResolveUncached(Kind, Name, Owner, Destination, Mapping);
		if (ResolvedResources.size() < MaxCachedPaths)
		{
			ResolvedResources.emplace(std::move(Key), Result);
		}
		return Result;
	}
	Resolved ResolveUncached(DependencyKind Kind, const xr_string& Name, const xr_string& Owner, const xr_path& Destination, const ImportRoot* Mapping) const
	{
		Resolved Result;
		Result.File.Kind = Kind;
		Result.File.Name = Name;
		Result.File.RequiredBy = Owner;
		Result.File.Destination = Destination;
		if (!Mapping || Mapping->Destination.empty() || !ImportDetails::IsSafe(Destination, Mapping->Destination))
		{
			Result.File.Reason = "The fs.ltx destination is missing, inaccessible or unsafe.";
			return Result;
		}
		const auto Found = PackageFiles.find(ImportDetails::Key(Destination));
		const auto* Candidate = Found == PackageFiles.end() ? nullptr : Found->second;
		if (Candidate)
		{
			Result.File.PackageRelative = Candidate->Relative;
		}
		const auto UsePackageLambda = [&]()
		{
			const auto SourceRoot = Store.GetDestination(Plan.Package);
			const auto Source = Join(SourceRoot, Candidate->Relative);
			std::error_code Code;
			if (!ImportDetails::IsSafe(Source, SourceRoot) || !std::filesystem::is_regular_file(std::filesystem::symlink_status(Source, Code)) || Code)
			{
				Result.File.Reason = "The downloaded resource is unreadable or unsafe.";
				return;
			}
			Result.Source = Source;
			Result.File.Status = Candidate->IsSelected ? DependencyStatus::Selected : DependencyStatus::NotSelected;
			Result.File.Reason = Candidate->IsSelected ? "Selected for import." : "Present in the package, but not selected for import.";
		};
		if (Candidate && Candidate->IsSelected && Candidate->CanSelect())
		{
			UsePackageLambda();
			return Result;
		}
		std::error_code Code;
		if (std::filesystem::is_regular_file(std::filesystem::symlink_status(Destination, Code)) && !Code)
		{
			Result.File.Status = DependencyStatus::InSdk;
			Result.File.Reason = "Already available in the SDK; kept contents are checked.";
			Result.Source = Destination;
		}
		else if (Candidate && Candidate->CanSelect())
		{
			UsePackageLambda();
		}
		else
		{
			Result.File.Reason = Candidate && !Candidate->Notice.empty() ? Candidate->Notice : "Not found in the SDK or selected package files.";
		}
		return Result;
	}
	bool Register(Resolved Resource)
	{
		xr_string Key = std::to_string(static_cast<int>(Resource.File.Kind)).c_str();
		Key += ":";
		Key += Resource.File.Destination.empty() ? Resource.File.Name : ImportDetails::Key(Resource.File.Destination);
		if (!Resources.insert(Key).second)
		{
			return true;
		}
		if (Report.Files.size() >= 100000)
		{
			return Details::Fail(Error, "The dependency check exceeds 100000 resources.");
		}
		if (Resource.File.Kind == DependencyKind::Object && !Resource.Source.empty())
		{
			Objects.push_back({Resource.File.Name, Resource.Source});
		}
		Report.Files.push_back(std::move(Resource.File));
		return true;
	}
	bool HasEntry(const xr_path& Destination)
	{
		if (Destination.empty())
		{
			return false;
		}
		const auto Key = Utf8(Destination.lexically_normal());
		if (const auto Cached = ExistingPaths.find(Key); Cached != ExistingPaths.end())
		{
			return Cached->second;
		}
		std::error_code Code;
		bool DoesExist = PackageFiles.contains(ImportDetails::Key(Destination));
		if (!DoesExist)
		{
			const auto Status = std::filesystem::symlink_status(Destination, Code);
			DoesExist = (!Code && std::filesystem::exists(Status)) || (Code && Code != std::errc::no_such_file_or_directory);
		}
		if (ExistingPaths.size() < MaxCachedPaths)
		{
			ExistingPaths.emplace(Key, DoesExist);
		}
		return DoesExist;
	}
	bool ReadOptional(const ImportRoot* Mapping, const xr_string& Name, const xr_string& Owner, Resolved& Resource)
	{
		if (!Mapping || Mapping->Destination.empty())
		{
			return false;
		}
		const auto Destination = Join(Mapping->Destination, Name);
		if (!HasEntry(Destination))
		{
			return false;
		}
		Resource = Resolve(DependencyKind::ScenePart, Name, Owner, Destination, Mapping);
		if (Resource.File.Status == DependencyStatus::NotSelected)
		{
			Resource.File.Reason = "Optional texture metadata is not selected; its settings will not apply after import.";
		}
		if (Mapping->Alias == "$level$")
		{
			Resource.File.Name = "$level$/" + Name;
		}
		if (!Register(Resource))
		{
			return false;
		}
		if (Resource.File.Status == DependencyStatus::NotSelected)
		{
			return false;
		}
		if (Resource.Source.empty())
		{
			Warn(Name, "Texture metadata is inaccessible or unsafe; its references could not be checked.", Owner, Resource.File.Destination);
			return false;
		}
		return true;
	}
	Resolved ResolveTexture(const xr_string& Name, const xr_string& Owner, bool CanAnimate)
	{
		const auto* Mapping = Root("$game_textures$");
		Resolved Fallback;
		bool HasFallback = false;
		const auto TryLambda = [&](const ImportRoot* CandidateRoot, const xr_string& CandidateName, Resolved& Resource)
		{
			if (!CandidateRoot || CandidateRoot->Destination.empty())
			{
				return false;
			}
			Resource = Resolve(DependencyKind::Texture, CandidateName, Owner, Join(CandidateRoot->Destination, CandidateName), CandidateRoot);
			if (Resource.File.Status == DependencyStatus::InSdk || Resource.File.Status == DependencyStatus::Selected)
			{
				return true;
			}
			if (Resource.File.Status == DependencyStatus::Missing && HasEntry(Resource.File.Destination))
			{
				return true;
			}
			if (!HasFallback && Resource.File.Status == DependencyStatus::NotSelected)
			{
				Fallback = Resource;
				HasFallback = true;
			}
			return false;
		};
		Resolved Resource;
		if (CanAnimate)
		{
			for (const char* Extension : {".ogm", ".avi", ".seq"})
			{
				const xr_string Candidate = Name.substr(0, Name.size() - 4) + Extension;
				if (TryLambda(Mapping, Candidate, Resource))
				{
					return Resource;
				}
			}
		}
		// The DDS loader sends missing global *_bump maps to its fallback before trying $level$.
		bool CanUseLevel = Name.find("_bump") == xr_string::npos;
		if (!CanUseLevel && Mapping && !Mapping->Destination.empty())
		{
			const auto Global = Resolve(DependencyKind::Texture, Name, Owner, Join(Mapping->Destination, Name), Mapping);
			CanUseLevel = Global.File.Status == DependencyStatus::InSdk || Global.File.Status == DependencyStatus::Selected;
		}
		if ((CanUseLevel && TryLambda(Root("$level$"), Name, Resource)) || TryLambda(Mapping, Name, Resource))
		{
			return Resource;
		}
		return HasFallback ? Fallback : Resolve(DependencyKind::Texture, Name, Owner, Mapping && !Mapping->Destination.empty() ? Join(Mapping->Destination, Name) : xr_path{}, Mapping);
	}
	bool AddBinding(xr_string Name, const xr_string& Owner, bool CanAnimate, u32 Usage = 0)
	{
		if (Name.empty() || Name.starts_with('$'))
		{
			return true;
		}
		if (!Normalize(Name, ".dds"))
		{
			Warn(Owner, "A texture reference has an invalid path or encoding: " + Name);
			return true;
		}
		const auto Resource = ResolveTexture(Name, Owner, CanAnimate);
		if (!Register(Resource))
		{
			return false;
		}
		if (Usage)
		{
			const auto Key = ImportDetails::Key(FromUtf8(Name));
			auto& Scheduled = TextureUsage[Key];
			const auto Added = Usage & ~Scheduled;
			Scheduled |= Usage;
			if (Added)
			{
				Textures.push_back({Name, Added});
			}
		}
		if (CanAnimate && !Resource.Source.empty() && Details::Lower(Resource.File.Name).ends_with(".seq") && Sequences.insert(ImportDetails::Key(Resource.Source)).second)
		{
			xr_vector<xr_string> Frames;
			xr_string ReadError;
			if (!TextureDependencyFormats::ReadSequence(Resource.Source, Frames, ReadError, Stop))
			{
				Warn(Resource.File.Name, ReadError, Name, Resource.Source);
			}
			for (const auto& Frame : Frames)
			{
				if (Stop.stop_requested())
				{
					return Details::Fail(Error, "Dependency check cancelled.");
				}
				// The renderer loads sequence frames as plain DDS images, not nested animations.
				if (!AddBinding(Frame, Resource.File.Name + " (frame)", false))
				{
					return false;
				}
			}
		}
		return true;
	}
	bool AddBump(xr_string Name, const xr_string& Owner)
	{
		if (Name.empty() || Name.starts_with('$'))
		{
			return true;
		}
		if (!Normalize(Name, ".dds"))
		{
			Warn(Owner, "Invalid bump texture reference: " + Name);
			return true;
		}
		if (!AddBinding(Name, Owner, true))
		{
			return false;
		}
		Name.resize(Name.size() - 4);
		Name += "#.dds";
		return AddBinding(Name, Owner, true);
	}
	TextureDependencyFormats::Description DescribeTexture(const xr_string& Name)
	{
		const auto Key = ImportDetails::Key(FromUtf8(Name));
		if (const auto Cached = TextureDescriptions.find(Key); Cached != TextureDescriptions.end())
		{
			return Cached->second;
		}
		if (!HasReadTextureIndex)
		{
			HasReadTextureIndex = true;
			Resolved Resource;
			if (ReadOptional(Root("$game_textures$"), "textures.ltx", Name, Resource))
			{
				xr_string ReadError;
				if (!TextureDependencyFormats::ReadIndex(Resource.Source, TextureIndex, ReadError, Stop))
				{
					Warn("textures.ltx", ReadError, Name, Resource.Source);
				}
			}
		}
		const xr_string Stem = Name.substr(0, Name.size() - 4);
		const auto Index = TextureIndex.find(Details::Lower(Stem));
		auto Description = Index == TextureIndex.end() ? TextureDependencyFormats::Description{} : Index->second;
		for (const char* Alias : {"$game_textures$", "$level$"})
		{
			Resolved Resource;
			const xr_string Thumbnail = Stem + ".thm";
			if (!ReadOptional(Root(Alias), Thumbnail, Name, Resource))
			{
				continue;
			}
			TextureDependencyFormats::Description Values;
			xr_string ReadError;
			if (!TextureDependencyFormats::ReadThumbnail(Resource.Source, Values, ReadError, Stop))
			{
				Warn(Resource.File.Name, ReadError, Name, Resource.Source);
				continue;
			}
			if (Values.HasEmptyBumpReference)
			{
				string512 Notice;
				xr_sprintf(Notice, "Bump = %s; Bump texture is empty.", Values.BumpMode == 3 ? "Use parallax (3)" : "Use (2)");
				Warn(Resource.File.Name, Notice, Name, Resource.Source, DependencyWarningKind::EmptyBumpReference, "Open this texture's THM in the texture editor. Set Bump > Texture, or set Bump > Mode to None if no bump map is needed. The THM is readable, but a bump map cannot be identified from an empty reference.");
			}
			// THM always replaces the specification, but only an enabled detail association replaces LTX.
			if (Values.HasSpecification)
			{
				Description.Bump = Values.Bump;
				Description.HasSpecification = true;
			}
			if (Values.HasDetail)
			{
				Description.Detail = Values.Detail;
				Description.DetailUsage = Values.DetailUsage;
				Description.HasDetail = true;
			}
		}
		TextureDescriptions.emplace(Key, Description);
		return Description;
	}
	bool Add(DependencyKind Kind, xr_string Name, const xr_string& Owner)
	{
		if (Name.empty() || Name.starts_with('$'))
		{
			return true;
		}
		if (!Normalize(Name, Kind == DependencyKind::Object ? ".object" : ".dds"))
		{
			Warn(Owner, "A resource reference has an invalid path or encoding: " + Name);
			return true;
		}
		const auto* Mapping = Root(Kind == DependencyKind::Object ? "$objects$" : "$game_textures$");
		const auto Destination = Mapping && !Mapping->Destination.empty() ? Join(Mapping->Destination, Name) : xr_path{};
		return Register(Resolve(Kind, Name, Owner, Destination, Mapping));
	}
	bool Follow(const xr_vector<DependencyFormats::Reference>& References, const xr_string& Owner)
	{
		for (const auto& Reference : References)
		{
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Dependency check cancelled.");
			}
			if (Reference.Type == DependencyFormats::Kind::SharedMaterial)
			{
				if (!HasReadMaterials)
				{
					HasReadMaterials = true;
					const auto* Mapping = Root("$game_config$");
					const auto Destination = Mapping && !Mapping->Destination.empty() ? Join(Mapping->Destination, "SharedMaterials.ltx") : xr_path{};
					const auto Resource = Resolve(DependencyKind::ScenePart, "SharedMaterials.ltx", Owner, Destination, Mapping);
					if (!Register(Resource))
					{
						return false;
					}
					xr_string ReadError;
					if (Resource.Source.empty() || !DependencyFormats::ReadMaterials(Resource.Source, Materials, ReadError, Stop))
					{
						Warn(Owner, "SharedMaterials.ltx is missing or unreadable; shared material textures cannot be checked.");
					}
				}
				const auto Material = Materials.find(Reference.Name);
				if (Material == Materials.end())
				{
					Warn(Owner, "Shared material not found: " + Reference.Name);
				}
				else if (!AddTexture(Material->second, Owner))
				{
					return false;
				}
			}
			else if (!(Reference.Type == DependencyFormats::Kind::Object ? Add(DependencyKind::Object, Reference.Name, Owner) : AddTexture(Reference, Owner)))
			{
				return false;
			}
		}
		return true;
	}
	bool AddTexture(const DependencyFormats::Reference& Reference, const xr_string& Owner)
	{
		if (!AddBinding(Reference.Name, Owner, true, 3))
		{
			return false;
		}
		if (Reference.Shader.empty() || Reference.Name.empty() || Reference.Name.starts_with('$'))
		{
			return true;
		}
		if (!HasReadShaders)
		{
			HasReadShaders = true;
			const auto* Mapping = Root("$game_data$");
			const auto Destination = Mapping && !Mapping->Destination.empty() ? Join(Mapping->Destination, "shaders.xr") : xr_path{};
			const auto Resource = Resolve(DependencyKind::ScenePart, "shaders.xr", Owner, Destination, Mapping);
			if (!Register(Resource))
			{
				return false;
			}
			xr_string ReadError;
			if (Resource.Source.empty() || !DependencyFormats::ReadTerrainShaders(Resource.Source, TerrainShaders, ReadError, Stop))
			{
				Warn(Owner, "Shader descriptions are unreadable; implicit terrain mask dependencies could not be checked.");
			}
		}
		auto Shader = Details::Lower(Reference.Shader);
		std::replace(Shader.begin(), Shader.end(), '\\', '/');
		if (TerrainShaders.contains(Shader))
		{
			auto Name = Reference.Name;
			if (Normalize(Name, ".dds"))
			{
				Name.resize(Name.size() - 4);
				Name += "_mask.dds";
				return AddBinding(Name, Owner, true);
			}
		}
		return true;
	}
	bool CheckScene(const xr_path& Destination, const xr_string& Name, const xr_string& Owner, bool IsPart)
	{
		if (!Scenes.insert(ImportDetails::Key(Destination)).second)
		{
			return true;
		}
		ReportProgress(Name);
		const auto Resource = Resolve(DependencyKind::ScenePart, Name, Owner, Destination, Root("$maps$"));
		if (IsPart && !Register(Resource))
		{
			return false;
		}
		if (Resource.Source.empty())
		{
			Warn(Name, "Scene data is unavailable; its object and texture references could not be checked.");
			return true;
		}
		DependencyFormats::Scene Scene;
		xr_string ReadError;
		if (!DependencyFormats::ReadScene(Resource.Source, Scene, ReadError, Stop))
		{
			Warn(Name, ReadError);
		}
		if (Scene.HasObjectData)
		{
			auto Directory = Destination;
			if (IsPart)
			{
				Directory = Directory.parent_path();
			}
			else
			{
				Directory.replace_extension();
			}
			ObjectDirectories.insert(ImportDetails::Key(Directory));
		}
		return Follow(Scene.References, Name);
	}
	const PackageStore& Store;
	const ImportPlan& Plan;
	DependencyReport& Report;
	xr_string& Error;
	std::stop_token Stop;
	const ProgressCallback& Progress;
	static constexpr size_t MaxCachedPaths = 100000;
	// Scope caches to one check: later checks must observe new files and selection changes.
	xr_hash_map<xr_string, Resolved> ResolvedResources;
	xr_hash_map<xr_string, bool> ExistingPaths;
	xr_hash_map<xr_string, const ImportFile*> PackageFiles;
	xr_hash_set<xr_string> Resources;
	xr_hash_set<xr_string> Scenes;
	xr_hash_set<xr_string> LevelDirectories;
	xr_hash_set<xr_string> ObjectDirectories;
	xr_vector<Object> Objects;
	xr_vector<Texture> Textures;
	xr_hash_map<xr_string, u32> TextureUsage;
	xr_hash_set<xr_string> Sequences;
	xr_hash_map<xr_string, TextureDependencyFormats::Description> TextureIndex;
	xr_hash_map<xr_string, TextureDependencyFormats::Description> TextureDescriptions;
	xr_hash_map<xr_string, DependencyFormats::Reference> Materials;
	xr_hash_set<xr_string> TerrainShaders;
	bool HasReadMaterials = false;
	bool HasReadShaders = false;
	bool HasReadTextureIndex = false;
};
} // namespace DependencyDetails

bool PackageDependencyChecker::Check(const ImportPlan& Plan, DependencyReport& Report, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress) const
{
	Report = {};
	Error.clear();
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Dependency check cancelled.");
	}
	const auto LockPath = Join(Store.GetStateRoot(), "transaction.lock");
	if (!ImportDetails::IsSafe(LockPath, Store.GetStateRoot()))
	{
		return Details::Fail(Error, "Package storage path is unsafe or inaccessible.");
	}
	Platform::FileLock Lock(LockPath);
	if (!Lock.IsLocked())
	{
		return Details::Fail(Error, "SDK package storage is busy. Retry after the other operation finishes.");
	}
	const auto Installed = Store.List(Error);
	if (!Error.empty())
	{
		return false;
	}
	const auto Package = std::find_if(Installed.begin(), Installed.end(), [&](const auto& Entry)
									  { return Entry.Id == Plan.Package.Id; });
	if (Package == Installed.end() || Package->Kind != PackageKind::Assets || Plan.Package.Kind != PackageKind::Assets || Package->Ref != Plan.Package.Ref || Package->Commit != Plan.Package.Commit || Package->Version != Plan.Package.Version || Package->Source != Plan.Package.Source || Package->Url != Plan.Package.Url || Package->LocalPath != Plan.Package.LocalPath)
	{
		return Details::Fail(Error, "The downloaded package changed. Reopen Import before checking dependencies.");
	}
	return DependencyDetails::Scan(Store, Plan, Report, Error, Stop, Progress).Run();
}

} // namespace SdkPackages
