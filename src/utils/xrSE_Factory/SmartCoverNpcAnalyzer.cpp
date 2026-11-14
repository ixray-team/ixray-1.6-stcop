#include "stdafx.h"
#include "SmartCoverNpcAnalyzer.h"

namespace SmartCoverNpc
{
	namespace
	{
		constexpr const char* CacheName = "smart_cover_npc.cache";
		constexpr const char* CacheVersion = "smart_cover_npc 4";

		// Logic section that sends an NPC to a cover
		struct CoverRef
		{
			xr_string LogicFile;
			xr_vector<xr_string> NamePatterns;
			xr_string AnimState;
			bool Animpoint = false;
		};

		class CAnalyzer
		{
		public:
			bool Pick(const char* CoverName, Candidate& Out, u32& Count);
			bool IsAnimpointCover(const char* CoverName, const char* Description);
			xr_string IntoAnimation(const char* State);
			void Reset();

		private:
			void EnsureIndex();
			u64 ComputeStamp() const;
			bool LoadCache(u64 Stamp);
			void SaveCache(u64 Stamp) const;
			void Build();

			xrCriticalSection Lock;
			bool Ready = false;
			xr_hash_map<xr_string, xr_vector<Candidate>> Covers;
			xr_hash_map<xr_string, u32> Picked;
			bool AnimpointLoaded = false;
			xr_hash_set<xr_string> AnimpointDescriptions;
			// Covers some animpoint@ logic sends an NPC to
			xr_hash_set<xr_string> AnimpointCovers;
			// state_lib and state_mgr_animstate_list sources, loaded on first use
			bool StateTextLoaded = false;
			xr_string StateLibText;
			xr_string AnimstateText;
		};

		CAnalyzer GAnalyzer;

		xr_string Lower(xr_string Text)
		{
			xr_strlwr(Text);
			return Text;
		}

		xr_string Trim(const xr_string& Text)
		{
			const size_t Begin = Text.find_first_not_of(" \t\r\n\"");
			if (Begin == xr_string::npos)
			{
				return "";
			}
			const size_t End = Text.find_last_not_of(" \t\r\n\"");
			return Text.substr(Begin, End - Begin + 1);
		}

		xr_string NormalizeFile(xr_string Path)
		{
			std::replace(Path.begin(), Path.end(), '/', '\\');
			return Lower(Trim(Path));
		}

		bool ReadText(const char* FullPath, xr_string& Out)
		{
			IReader* Reader = FS.r_open(FullPath);
			if (Reader == nullptr)
			{
				return false;
			}

			Out.assign(static_cast<const char*>(Reader->pointer()), Reader->length());
			FS.r_close(Reader);
			return true;
		}

		// Every value between Open and Close after From, until Limit
		xr_vector<xr_string> FindAll(const xr_string& Text, const char* Open, const char* Close, size_t From = 0, size_t Limit = xr_string::npos)
		{
			xr_vector<xr_string> Result;
			const size_t OpenLen = xr_strlen(Open);
			while (true)
			{
				const size_t Begin = Text.find(Open, From);
				if (Begin == xr_string::npos || Begin >= Limit)
				{
					break;
				}

				const size_t ValueBegin = Begin + OpenLen;
				const size_t End = Text.find(Close, ValueBegin);
				if (End == xr_string::npos || End > Limit)
				{
					break;
				}

				Result.push_back(Trim(Text.substr(ValueBegin, End - ValueBegin)));
				From = End;
			}
			return Result;
		}

		// `Name = {` as a whole identifier, returns the position of `{`
		size_t FindTable(const xr_string& Text, const xr_string& Name)
		{
			size_t From = 0;
			while (true)
			{
				const size_t Pos = Text.find(Name, From);
				if (Pos == xr_string::npos)
				{
					return xr_string::npos;
				}
				From = Pos + Name.size();

				const bool StartOk = Pos == 0 || !(isalnum(u8(Text[Pos - 1])) || Text[Pos - 1] == '_');
				size_t Next = From;
				while (Next < Text.size() && (Text[Next] == ' ' || Text[Next] == '\t'))
				{
					++Next;
				}
				if (!StartOk || Next >= Text.size() || Text[Next] != '=')
				{
					continue;
				}

				++Next;
				while (Next < Text.size() && isspace(u8(Text[Next])))
				{
					++Next;
				}
				if (Next < Text.size() && Text[Next] == '{')
				{
					return Next;
				}
			}
		}

		// End of the Lua table opened at Brace
		size_t TableEnd(const xr_string& Text, size_t Brace)
		{
			int Depth = 0;
			for (size_t Pos = Brace; Pos < Text.size(); ++Pos)
			{
				if (Text[Pos] == '{')
				{
					++Depth;
				}
				else if (Text[Pos] == '}' && --Depth == 0)
				{
					return Pos;
				}
			}
			return Text.size();
		}

		// First quoted string in [From, Limit)
		xr_string FirstQuoted(const xr_string& Text, size_t From, size_t Limit)
		{
			const size_t Begin = Text.find('"', From);
			if (Begin == xr_string::npos || Begin >= Limit)
			{
				return "";
			}
			const size_t End = Text.find('"', Begin + 1);
			if (End == xr_string::npos || End >= Limit)
			{
				return "";
			}
			return Text.substr(Begin + 1, End - Begin - 1);
		}

		// avail_animations state -> idle animation of the stalker
		xr_string ResolveAnimation(const xr_string& StateLib, const xr_string& AnimationList, const xr_string& State)
		{
			if (State.empty())
			{
				return "";
			}

			const size_t StateBrace = FindTable(StateLib, State);
			if (StateBrace == xr_string::npos)
			{
				return "";
			}

			const size_t StateEnd = TableEnd(StateLib, StateBrace);
			const size_t AnimKey = StateLib.find("animation", StateBrace);
			if (AnimKey == xr_string::npos || AnimKey > StateEnd)
			{
				return "";
			}

			// `animation = nil`: the cover animation is played
			const xr_string Animation = FirstQuoted(StateLib, AnimKey, std::min(StateLib.find(',', AnimKey), StateEnd));
			if (Animation.empty())
			{
				return "";
			}

			const size_t AnimBrace = FindTable(AnimationList, Animation);
			if (AnimBrace == xr_string::npos)
			{
				return "";
			}

			const size_t AnimEnd = TableEnd(AnimationList, AnimBrace);
			const size_t Idle = AnimationList.find("idle", AnimBrace);
			if (Idle == xr_string::npos || Idle > AnimEnd)
			{
				return "";
			}

			const size_t IdleBrace = AnimationList.find('{', Idle);
			if (IdleBrace == xr_string::npos || IdleBrace > AnimEnd)
			{
				return "";
			}

			return FirstQuoted(AnimationList, IdleBrace, TableEnd(AnimationList, IdleBrace));
		}

		struct ProfileData
		{
			xr_hash_map<xr_string, xr_vector<xr_string>> ProfileClasses;
			xr_hash_map<xr_string, xr_vector<xr_string>> ClassVisuals;
		};

		void ParseProfiles(const xr_string& Text, ProfileData& Data)
		{
			size_t From = 0;
			while ((From = Text.find("<character ", From)) != xr_string::npos)
			{
				const size_t End = Text.find("</character>", From);
				const xr_vector<xr_string> Ids = FindAll(Text, "id=\"", "\"", From, Text.find('>', From));
				if (End == xr_string::npos)
				{
					break;
				}
				if (!Ids.empty())
				{
					xr_vector<xr_string>& Classes = Data.ProfileClasses[Lower(Ids.front())];
					for (const xr_string& Class : FindAll(Text, "<class>", "</class>", From, End))
					{
						Classes.push_back(Lower(Class));
					}
				}
				From = End;
			}

			From = 0;
			while ((From = Text.find("<specific_character ", From)) != xr_string::npos)
			{
				const size_t End = Text.find("</specific_character>", From);
				if (End == xr_string::npos)
				{
					break;
				}

				const xr_vector<xr_string> Ids = FindAll(Text, "id=\"", "\"", From, Text.find('>', From));
				const xr_vector<xr_string> Visuals = FindAll(Text, "<visual>", "</visual>", From, End);
				if (!Ids.empty() && !Visuals.empty())
				{
					const xr_string Visual = Visuals.front();
					Data.ClassVisuals[Lower(Ids.front())].push_back(Visual);
					for (const xr_string& Class : FindAll(Text, "<class>", "</class>", From, End))
					{
						Data.ClassVisuals[Lower(Class)].push_back(Visual);
					}
				}
				From = End;
			}
		}

		xr_vector<xr_string> ProfileVisuals(const ProfileData& Data, const xr_string& Profile)
		{
			xr_vector<xr_string> Keys = {Lower(Profile)};
			if (auto It = Data.ProfileClasses.find(Keys.front()); It != Data.ProfileClasses.end())
			{
				Keys.insert(Keys.end(), It->second.begin(), It->second.end());
			}

			xr_vector<xr_string> Result;
			for (const xr_string& Key : Keys)
			{
				auto It = Data.ClassVisuals.find(Key);
				if (It == Data.ClassVisuals.end())
				{
					continue;
				}
				for (const xr_string& Visual : It->second)
				{
					if (std::find(Result.begin(), Result.end(), Visual) == Result.end())
					{
						Result.push_back(Visual);
					}
				}
			}
			return Result;
		}

		void ListSources(FS_FileSet& Config, FS_FileSet& Scripts)
		{
			FS.file_list(Config, _game_config_, FS_ListFiles, "*.ltx,*.xml");
			FS.file_list(Scripts, "$game_scripts$", FS_ListFiles, "state_*.script");
		}
	}

	u64 CAnalyzer::ComputeStamp() const
	{
		FS_FileSet Config;
		FS_FileSet Scripts;
		ListSources(Config, Scripts);

		// FNV-1a over names, times and sizes
		u64 Hash = 1469598103934665603ull;
		auto Mix = [&Hash](const void* Data, size_t Size)
		{
			const u8* Bytes = static_cast<const u8*>(Data);
			for (size_t Index = 0; Index < Size; ++Index)
			{
				Hash = (Hash ^ Bytes[Index]) * 1099511628211ull;
			}
		};

		for (const FS_FileSet* Set : {&Config, &Scripts})
		{
			for (const FS_File& File : *Set)
			{
				const s64 Time = s64(File.time_write);
				Mix(File.name.data(), File.name.size());
				Mix(&Time, sizeof(Time));
				Mix(&File.size, sizeof(File.size));
			}
		}
		return Hash;
	}

	bool CAnalyzer::LoadCache(u64 Stamp)
	{
		string_path Path;
		FS.update_path(Path, "$temp$", CacheName);
		if (!FS.exist(Path))
		{
			return false;
		}

		IReader* Reader = FS.r_open(Path);
		if (Reader == nullptr)
		{
			return false;
		}

		xr_string Line;
		Reader->r_string(Line);
		bool Valid = Line == CacheVersion;
		if (Valid)
		{
			Reader->r_string(Line);
			Valid = Line == xr_string().append(std::to_string(Stamp).c_str());
		}

		xr_vector<Candidate>* Current = nullptr;
		while (Valid && !Reader->eof())
		{
			Reader->r_string(Line);
			if (Line.starts_with("animpoint "))
			{
				AnimpointCovers.insert(Line.substr(10));
			}
			else if (Line.starts_with("cover "))
			{
				Current = &Covers[Line.substr(6)];
			}
			else if (Line.starts_with("npc ") && Current != nullptr)
			{
				// npc <section>|<visual>|<animation>
				const xr_string Value = Line.substr(4);
				const size_t A = Value.find('|');
				const size_t B = A == xr_string::npos ? xr_string::npos : Value.find('|', A + 1);
				const size_t C = B == xr_string::npos ? xr_string::npos : Value.find('|', B + 1);
				const size_t D = C == xr_string::npos ? xr_string::npos : Value.find('|', C + 1);
				if (D == xr_string::npos)
				{
					continue;
				}
				Current->push_back({Value.substr(0, A), Value.substr(A + 1, B - A - 1), Value.substr(B + 1, C - B - 1), Value.substr(C + 1, D - C - 1), Value.substr(D + 1) == "1"});
			}
		}

		FS.r_close(Reader);
		if (!Valid)
		{
			Covers.clear();
		}
		return Valid;
	}

	void CAnalyzer::SaveCache(u64 Stamp) const
	{
		string_path Path;
		FS.update_path(Path, "$temp$", CacheName);
		IWriter* Writer = FS.w_open(Path);
		if (Writer == nullptr)
		{
			return;
		}

		Writer->w_string(CacheVersion);
		Writer->w_string(std::to_string(Stamp).c_str());
		for (const xr_string& Cover : AnimpointCovers)
		{
			Writer->w_string(("animpoint " + Cover).c_str());
		}
		for (const auto& [Cover, Candidates] : Covers)
		{
			Writer->w_string(("cover " + Cover).c_str());
			for (const Candidate& Item : Candidates)
			{
				Writer->w_string(("npc " + Item.Npc + "|" + Item.Visual + "|" + Item.Animation + "|" + Item.State + "|" + (Item.Animpoint ? "1" : "0")).c_str());
			}
		}
		FS.w_close(Writer);
	}

	void CAnalyzer::Build()
	{
		const u64 Start = CPU::QPC();

		FS_FileSet Config;
		FS_FileSet Scripts;
		ListSources(Config, Scripts);

		xr_hash_map<xr_string, xr_vector<CoverRef>> Refs;
		xr_hash_map<xr_string, xr_vector<xr_string>> JobKeys;
		ProfileData Profiles;

		string_path ConfigRoot;
		FS.update_path(ConfigRoot, _game_config_, "");

		for (const FS_File& File : Config)
		{
			const xr_string Name = Lower(File.name);
			const xr_string FullPath = xr_string(ConfigRoot) + File.name;

			if (Name.ends_with(".xml"))
			{
				if (Name.starts_with("gameplay\\"))
				{
					xr_string Text;
					if (ReadText(FullPath.c_str(), Text))
					{
						ParseProfiles(Text, Profiles);
					}
				}
				continue;
			}

			if (!Name.starts_with("scripts\\"))
			{
				continue;
			}

			const xr_string LogicFile = Name.substr(xr_strlen("scripts\\"));
			CInifile Ini(FullPath.c_str(), TRUE, TRUE, FALSE);

			xr_vector<xr_string> Patterns;
			for (const CInifile::Sect& Section : Ini.sections())
			{
				if (!xr_string(Section.Name.c_str()).starts_with("logic"))
				{
					continue;
				}

				for (const CInifile::Item& Item : Section.Data)
				{
					if (!xr_strcmp(Item.first, "suitable") && Item.second.size())
					{
						for (const xr_string& Pattern : FindAll(Item.second.c_str(), "check_npc_name(", ")"))
						{
							Patterns.push_back(Lower(Pattern));
						}
					}
				}
			}

			for (const CInifile::Sect& Section : Ini.sections())
			{
				if (!xr_strcmp(Section.Name, "exclusive"))
				{
					for (const CInifile::Item& Item : Section.Data)
					{
						if (Item.second.size())
						{
							JobKeys[NormalizeFile(Item.second.c_str())].push_back(Lower(Item.first.c_str()));
						}
					}
					continue;
				}

				const char* CoverName = nullptr;
				const char* Animations = nullptr;
				for (const CInifile::Item& Item : Section.Data)
				{
					if (!xr_strcmp(Item.first, "cover_name"))
					{
						CoverName = Item.second.c_str();
					}
					else if (!xr_strcmp(Item.first, "avail_animations"))
					{
						Animations = Item.second.c_str();
					}
				}

				if (CoverName == nullptr || *CoverName == '\0')
				{
					continue;
				}

				CoverRef Ref;
				Ref.LogicFile = LogicFile;
				// animpoint@ runs xr_animpoint: the animation is played in the cover space
				Ref.Animpoint = xr_string(Section.Name.c_str()).starts_with("animpoint");
				Ref.NamePatterns = Patterns;
				if (Animations != nullptr)
				{
					const xr_string List = Animations;
					Ref.AnimState = Lower(Trim(List.substr(0, List.find(','))));
				}
				if (Ref.Animpoint)
				{
					AnimpointCovers.insert(Lower(Trim(CoverName)));
				}
				Refs[Lower(Trim(CoverName))].push_back(Ref);
			}
		}

		xr_string StateLib;
		xr_string AnimationList;
		string_path ScriptsRoot;
		FS.update_path(ScriptsRoot, "$game_scripts$", "");
		for (const FS_File& File : Scripts)
		{
			xr_string Text;
			if (!ReadText((xr_string(ScriptsRoot) + File.name).c_str(), Text))
			{
				continue;
			}

			const xr_string Name = Lower(File.name);
			if (Name.starts_with("state_lib"))
			{
				StateLib += Text;
				StateLib += "\n";
			}
			else if (Name.starts_with("state_mgr_animation_list"))
			{
				AnimationList += Text;
				AnimationList += "\n";
			}
		}

		xr_vector<std::pair<xr_string, xr_string>> NpcSections;
		for (const CInifile::Sect& Section : pSettings->sections())
		{
			for (const CInifile::Item& Item : Section.Data)
			{
				if (!xr_strcmp(Item.first, "character_profile") && Item.second.size())
				{
					NpcSections.emplace_back(Lower(Section.Name.c_str()), Trim(Item.second.c_str()));
					break;
				}
			}
		}

		for (const auto& [Cover, CoverRefs] : Refs)
		{
			xr_vector<Candidate>& Candidates = Covers[Cover];
			for (const CoverRef& Ref : CoverRefs)
			{
				const xr_string Animation = ResolveAnimation(StateLib, AnimationList, Ref.AnimState);

				auto Matches = [&](const xr_string& Section)
				{
					if (!Ref.NamePatterns.empty())
					{
						return std::any_of(Ref.NamePatterns.begin(), Ref.NamePatterns.end(), [&](const xr_string& Pattern) { return Section.find(Pattern) != xr_string::npos; });
					}

					auto It = JobKeys.find(Ref.LogicFile);
					return It != JobKeys.end() && std::find(It->second.begin(), It->second.end(), Section) != It->second.end();
				};

				for (const auto& [Section, Profile] : NpcSections)
				{
					if (!Matches(Section))
					{
						continue;
					}

					for (const xr_string& Visual : ProfileVisuals(Profiles, Profile))
					{
						const bool Known = std::any_of(Candidates.begin(), Candidates.end(), [&](const Candidate& Item) { return Item.Visual == Visual && Item.Animation == Animation && Item.Animpoint == Ref.Animpoint; });
						if (!Known)
						{
							Candidates.push_back({Section, Visual, Animation, Ref.AnimState, Ref.Animpoint});
						}
					}
				}
			}
		}

		Msg("* Smart cover NPC index: %u covers, %u logic refs, %.0f ms", u32(Covers.size()), u32(Refs.size()), float(CPU::QPC() - Start) * 1000.0f / float(CPU::qpc_freq));
	}

	void CAnalyzer::EnsureIndex()
	{
		if (Ready)
		{
			return;
		}
		Ready = true;

		const u64 Stamp = ComputeStamp();
		if (LoadCache(Stamp))
		{
			return;
		}

		Build();
		SaveCache(Stamp);
	}

	bool CAnalyzer::Pick(const char* CoverName, Candidate& Out, u32& Count)
	{
		Count = 0;
		if (CoverName == nullptr || *CoverName == '\0')
		{
			return false;
		}

		xrCriticalSectionGuard Guard(Lock);
		EnsureIndex();

		const xr_string Key = Lower(CoverName);
		auto It = Covers.find(Key);
		if (It == Covers.end() || It->second.empty())
		{
			return false;
		}

		Count = u32(It->second.size());
		auto PickIt = Picked.find(Key);
		if (PickIt == Picked.end() || PickIt->second >= Count)
		{
			PickIt = Picked.insert_or_assign(Key, u32(::Random.randI(size_t(Count)))).first;
		}

		Out = It->second[PickIt->second];
		return true;
	}

	void CAnalyzer::Reset()
	{
		xrCriticalSectionGuard Guard(Lock);
		Ready = false;
		Covers.clear();
		Picked.clear();
		AnimpointLoaded = false;
		AnimpointDescriptions.clear();
		AnimpointCovers.clear();
		StateTextLoaded = false;
		StateLibText.clear();
		AnimstateText.clear();
	}

	bool CAnalyzer::IsAnimpointCover(const char* CoverName, const char* Description)
	{
		xrCriticalSectionGuard Guard(Lock);
		EnsureIndex();

		if (CoverName != nullptr && AnimpointCovers.contains(Lower(CoverName)))
		{
			return true;
		}

		if (Description == nullptr || *Description == '\0')
		{
			return false;
		}

		// animpoint_* descriptions exist for xr_animpoint, even the ones its predicates miss
		if (Lower(Description).starts_with("animpoint"))
		{
			return true;
		}

		if (!AnimpointLoaded)
		{
			AnimpointLoaded = true;

			string_path Path;
			FS.update_path(Path, "$game_scripts$", "xr_animpoint_predicates.script");
			xr_string Text;
			const size_t Begin = ReadText(Path, Text) ? FindTable(Text, "associations") : xr_string::npos;
			if (Begin != xr_string::npos)
			{
				// Keys of the table: `name = {` one level down
				const size_t End = TableEnd(Text, Begin);
				int Depth = 0;
				for (size_t Pos = Begin; Pos < End; ++Pos)
				{
					if (Text[Pos] == '}')
					{
						--Depth;
						continue;
					}

					if (Text[Pos] != '{' || ++Depth != 2)
					{
						continue;
					}

					const size_t Equal = Text.find_last_of('=', Pos);
					size_t KeyBegin = Equal;
					while (KeyBegin > Begin && (isalnum(u8(Text[KeyBegin - 1])) || Text[KeyBegin - 1] == '_' || isspace(u8(Text[KeyBegin - 1]))))
					{
						--KeyBegin;
					}
					AnimpointDescriptions.insert(Lower(Trim(Text.substr(KeyBegin, Equal - KeyBegin))));
				}
			}
		}

		return AnimpointDescriptions.contains(Lower(Description));
	}

	xr_string CAnalyzer::IntoAnimation(const char* State)
	{
		if (State == nullptr || *State == '\0')
		{
			return "";
		}

		xrCriticalSectionGuard Guard(Lock);
		if (!StateTextLoaded)
		{
			StateTextLoaded = true;

			FS_FileSet Files;
			FS.file_list(Files, "$game_scripts$", FS_ListFiles, "state_lib*.script,state_mgr_animstate_list*.script");

			string_path Root;
			FS.update_path(Root, "$game_scripts$", "");
			for (const FS_File& File : Files)
			{
				xr_string Text;
				if (!ReadText((xr_string(Root) + File.name).c_str(), Text))
				{
					continue;
				}

				xr_string& Target = Lower(File.name).starts_with("state_lib") ? StateLibText : AnimstateText;
				Target += Text;
				Target += "\n";
			}
		}

		// state_lib: `animstate = "name"`
		const size_t StateBrace = FindTable(StateLibText, Lower(State));
		if (StateBrace == xr_string::npos)
		{
			return "";
		}

		const size_t StateEnd = TableEnd(StateLibText, StateBrace);
		const size_t AnimstateKey = StateLibText.find("animstate", StateBrace);
		if (AnimstateKey == xr_string::npos || AnimstateKey > StateEnd)
		{
			return "";
		}

		const xr_string Animstate = FirstQuoted(StateLibText, AnimstateKey, std::min(StateLibText.find(',', AnimstateKey), StateEnd));
		if (Animstate.empty())
		{
			return "";
		}

		// state_mgr_animstate_list: `into = { [0] = { "name" } }` or `into = nil`
		const size_t ListBrace = FindTable(AnimstateText, Animstate);
		if (ListBrace == xr_string::npos)
		{
			return "";
		}

		const size_t ListEnd = TableEnd(AnimstateText, ListBrace);
		const size_t IntoKey = AnimstateText.find("into", ListBrace);
		if (IntoKey == xr_string::npos || IntoKey > ListEnd)
		{
			return "";
		}

		size_t Value = AnimstateText.find('=', IntoKey);
		if (Value == xr_string::npos)
		{
			return "";
		}
		++Value;
		while (Value < ListEnd && isspace(u8(AnimstateText[Value])))
		{
			++Value;
		}

		if (Value >= ListEnd || AnimstateText[Value] != '{')
		{
			return "";
		}

		return FirstQuoted(AnimstateText, Value, TableEnd(AnimstateText, Value));
	}

	bool Pick(const char* CoverName, Candidate& Out, u32& CandidateCount)
	{
		return GAnalyzer.Pick(CoverName, Out, CandidateCount);
	}

	bool IsAnimpointCover(const char* CoverName, const char* Description)
	{
		return GAnalyzer.IsAnimpointCover(CoverName, Description);
	}

	xr_string IntoAnimation(const char* State)
	{
		return GAnalyzer.IntoAnimation(State);
	}

	void Reset()
	{
		GAnalyzer.Reset();
	}
}
