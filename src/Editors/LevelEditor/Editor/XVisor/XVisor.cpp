#include "stdafx.h"
#include "XVisor.h"
#include "../../UI/Coder/CodeEditorUtils.h"
#include "../../../../xrScripts/linker.h"
#include "../../../../xrScripts/script_engine.h"
#include "../../../../3rd-party/lua/lauxlib.h"
#include "../../Engine/XrGameManager.h"

#include <regex>

static constexpr int MaxDepth = 32;

static bool IsWordChar(char Char)
{
	return isalnum((u8)Char) || Char == '_';
}

// xr_string::Split with the parts trimmed and the empty ones dropped
static xr_vector<xr_string> Split(const xr_string& Text, char Separator)
{
	xr_vector<xr_string> Parts;
	for (const xr_string& Part : Text.Split(Separator))
	{
		xr_string Value = Part.Trim();
		if (!Value.empty())
		{
			Parts.push_back(std::move(Value));
		}
	}
	return Parts;
}

static xr_vector<xr_string> SplitTabs(const xr_string& Line)
{
	xr_vector<xr_string> Parts;
	size_t Start = 0;
	for (size_t Tab = Line.find('\t'); Tab != xr_string::npos; Tab = Line.find('\t', Start))
	{
		Parts.push_back(Line.substr(Start, Tab - Start).c_str());
		Start = Tab + 1;
	}
	Parts.push_back(Line.substr(Start).c_str());
	return Parts;
}

// A script that failed to load is baked from its text too: the same lines may come twice
static void AddUnique(xr_vector<xr_string>& List, const xr_string& Value)
{
	if (std::find(List.begin(), List.end(), Value) == List.end())
	{
		List.push_back(Value);
	}
}

static bool IsLuaKeyword(const xr_string& Word)
{
	static const char* Keywords[] = { "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while" };
	return std::any_of(std::begin(Keywords), std::end(Keywords), [&Word](const char* Keyword) { return Word == Keyword; });
}

xr_string CXVisor::GetApiPath()
{
	string_path Path;
	FS.update_path(Path, "$app_data_root$", "lua_api.txt");
	return Path;
}

xr_string CXVisor::GetSchemaPath()
{
	string_path Path;
	FS.update_path(Path, "$app_data_root$", "ltx_schema.txt");
	return Path;
}

bool CXVisor::Bake(xr_string& Error)
{
	if (g_pScriptEngine == nullptr)
	{
		Error = "The script engine is not created";
		return false;
	}

	if (!FS.path_exist("$app_data_root$"))
	{
		Error = "No $app_data_root$ path for lua_api.txt";
		return false;
	}

	if (!g_pScriptEngine->BakeApi(GetApiPath().c_str()))
	{
		Error = "Can't write " + GetApiPath();
		return false;
	}

	// The ltx schema of the XREFLECT(IniRead) classes goes with the API
	if (g_XrGameManager == nullptr || !g_XrGameManager->BakeIniSchema(GetSchemaPath().c_str()))
	{
		Msg("! Lua API: xrGame wrote no ltx schema");
	}

	++BakeGeneration;
	InvalidateScripts();
	return Load() && IsApiLoaded();
}

bool CXVisor::Compile(const char* ChunkName, const xr_string& Source, xr_string& Error, int& Line)
{
	Line = 0;

	// The game state is shared with the game thread in PIE: the check gets a state of its own
	lua_State* L = luaL_newstate();
	if (L == nullptr)
	{
		Error = "Can't create a Lua state";
		return false;
	}

	const xr_string Chunk = xr_string("@") + ChunkName;
	const int Result = luaL_loadbuffer(L, Source.data(), Source.size(), Chunk.c_str());
	if (Result != 0)
	{
		// name.script:12: '=' expected near 'x'
		const char* Message = lua_tostring(L, -1);
		Error = Message != nullptr ? Message : "Unknown error";

		const size_t NameEnd = Error.find(ChunkName);
		const size_t LineStart = NameEnd != xr_string::npos ? NameEnd + xr_strlen(ChunkName) + 1 : xr_string::npos;
		if (LineStart != xr_string::npos && LineStart < Error.size() && isdigit((u8)Error[LineStart]))
		{
			Line = atoi(Error.c_str() + LineStart);
		}
	}

	lua_close(L);
	return Result == 0;
}

bool CXVisor::Load()
{
	if (LoadedGeneration == BakeGeneration)
	{
		return IsApiLoaded() || IsSchemaLoaded();
	}
	LoadedGeneration = BakeGeneration;

	LoadApi();
	LoadSchema();
	return IsApiLoaded() || IsSchemaLoaded();
}

void CXVisor::LoadApi()
{
	Classes.clear();
	Namespaces.clear();
	NamespaceFields.clear();

	const xr_string Path = GetApiPath();
	if (!FS.path_exist("$app_data_root$") || !FS.exist(Path.c_str()))
	{
		return;
	}

	IReader* Reader = FS.r_open(Path.c_str());
	if (Reader == nullptr)
	{
		return;
	}

	string4096 Buffer;
	while (!Reader->eof())
	{
		Reader->r_string(Buffer, sizeof(Buffer));
		const xr_vector<xr_string> Parts = SplitTabs(Buffer);
		if (Parts.size() < 2 || Parts[0].empty() || Parts[0][0] == '#')
		{
			continue;
		}

		const char Kind = Parts[0][0];
		if (Kind == 'C')
		{
			SLuaClass& Class = Classes[Parts[1]];
			if (Parts.size() > 2 && !Parts[2].empty())
			{
				for (const xr_string& Base : Split(Parts[2], ','))
				{
					AddUnique(Class.Bases, Base);
				}
			}
		}
		else if (Kind == 'M' && Parts.size() >= 4)
		{
			AddUnique(Classes[Parts[1]].Methods[Parts[2]].Overloads, Parts[3]);
		}
		else if (Kind == 'P' && Parts.size() >= 3)
		{
			Classes[Parts[1]].Properties.push_back(Parts[2]);
		}
		else if (Kind == 'K' && Parts.size() >= 3)
		{
			Classes[Parts[1]].Constants.push_back(Parts[2]);
		}
		else if (Kind == 'N')
		{
			Namespaces[Parts[1]];
		}
		else if (Kind == 'F' && Parts.size() >= 4)
		{
			AddUnique(Namespaces[Parts[1]][Parts[2]].Overloads, Parts[3]);
		}
		else if (Kind == 'V' && Parts.size() >= 4)
		{
			// The type from Lua wins over "any" from the text
			xr_string& Type = NamespaceFields[Parts[1]][Parts[2]];
			if (Type.empty() || Type == "any")
			{
				Type = Parts[3];
			}
			if (!Parts[1].empty())
			{
				Namespaces[Parts[1]];
			}
		}
	}
	FS.r_close(Reader);

	// HACK: db.actor is nil until the game starts, it is always the actor's CScriptGameObject
	if (Classes.contains("game_object"))
	{
		NamespaceFields["db"]["actor"] = "game_object";
	}
}

void CXVisor::LoadSchema()
{
	Schemas.clear();
	ClassIds.clear();

	const xr_string Path = GetSchemaPath();
	if (!FS.path_exist("$app_data_root$") || !FS.exist(Path.c_str()))
	{
		Msg("! ltx schema: no %s, bake the API", Path.c_str());
		return;
	}

	IReader* Reader = FS.r_open(Path.c_str());
	if (Reader == nullptr)
	{
		return;
	}

	string4096 Buffer;
	while (!Reader->eof())
	{
		Reader->r_string(Buffer, sizeof(Buffer));
		const xr_string Line = Buffer;
		if (Line.empty() || Line[0] == '#')
		{
			continue;
		}

		const xr_vector<xr_string> Parts = SplitTabs(Line);
		if (Parts[0] == "S" && Parts.size() >= 4)
		{
			Schemas[Parts[1]].Bases = Split(Parts[3], ',');
		}
		else if (Parts[0] == "F" && Parts.size() >= 7)
		{
			Schemas[Parts[1]].Fields.push_back({ Parts[2], Parts[3], Parts[4] == "1", Parts[5], Parts[6] == "1" });
		}
		else if (Parts[0] == "O" && Parts.size() >= 3)
		{
			ClassIds[Parts[1]].push_back(Parts[2]);
		}
	}
	FS.r_close(Reader);
	Msg("* ltx schema: %u schemas, %u class ids from %s", (u32)Schemas.size(), (u32)ClassIds.size(), Path.c_str());
}

void CXVisor::BuildLuaLanguage(TextEditor::LanguageDefinition& Out) const
{
	Out = TextEditor::LanguageDefinition::Lua();
	Out.mKeywords.insert("self");

	// A name before a bracket is a call, it goes before the plain identifiers
	using EIndex = TextEditor::PaletteIndex;
	auto Identifier = std::find_if(Out.mTokenRegexStrings.begin(), Out.mTokenRegexStrings.end(), [](const auto& Token) { return Token.second == EIndex::Identifier; });
	Out.mTokenRegexStrings.insert(Identifier, { "[a-zA-Z_][a-zA-Z0-9_]*(?=[ \\t]*\\()", EIndex::PreprocIdentifier });

	auto Declare = [](TextEditor::Identifiers& Names, const xr_string& Name, const xr_string& Declaration)
	{
		TextEditor::Identifier Id;
		Id.mDeclaration = Declaration.c_str();
		Names.insert({ Name.c_str(), Id });
	};

	for (const auto& [Name, Class] : Classes)
	{
		xr_string Bases;
		for (const xr_string& Base : Class.Bases)
		{
			Bases += (Bases.empty() ? " : " : ", ") + Base;
		}
		Declare(Out.mIdentifiers, Name, "class " + Name + Bases);
	}

	for (const auto& [Namespace, Functions] : Namespaces)
	{
		if (!Namespace.empty())
		{
			Declare(Out.mIdentifiers, Namespace, "namespace " + Namespace);
		}
	}

	auto Globals = Namespaces.find("");
	if (Globals != Namespaces.end())
	{
		for (const auto& [Name, Function] : Globals->second)
		{
			xr_string Declaration;
			for (const xr_string& Overload : Function.Overloads)
			{
				Declaration += (Declaration.empty() ? "" : "\n") + Name + "(" + Overload + ")";
			}
			Declare(Out.mPreprocIdentifiers, Name, Declaration);
		}
	}
}

xr_string CXVisor::GetInitClass(const xr_string& Function) const
{
	static const std::pair<const char*, const char*> Aliases[] =
	{
		{ "InitFrame", "CUIFrameWindow" },
		{ "InitFrameLine", "CUIFrameLineWnd" },
		{ "InitCheck", "CUICheckButton" },
		{ "InitSpinNum", "CUISpinNum" },
		{ "InitSpinFlt", "CUISpinFlt" },
		{ "InitSpinText", "CUISpinText" },
		{ "InitTab", "CUITabControl" },
		{ "InitMMShniaga", "CUIMMShniaga" },
		{ "InitTextWnd", "CUIStatic" },
	};

	for (const auto& [Init, Class] : Aliases)
	{
		if (Function == Init && Classes.contains(Class))
		{
			return Class;
		}
	}

	if (Function.starts_with("Init"))
	{
		const xr_string Class = "CUI" + Function.substr(4);
		if (Classes.contains(Class))
		{
			return Class;
		}
	}
	return "";
}

const CXVisor::SLuaFunction* CXVisor::FindMethod(const xr_string& ClassName, const xr_string& Name, int Depth) const
{
	auto It = Classes.find(ClassName);
	if (It == Classes.end() || Depth > MaxDepth)
	{
		return nullptr;
	}

	auto Method = It->second.Methods.find(Name);
	if (Method != It->second.Methods.end())
	{
		return &Method->second;
	}

	for (const xr_string& Base : It->second.Bases)
	{
		if (const SLuaFunction* Found = FindMethod(Base, Name, Depth + 1))
		{
			return Found;
		}
	}
	return nullptr;
}

xr_string CXVisor::ResolveCall(const xr_string& Callee, const xr_string& Script, const FieldResolver& Fields) const
{
	if (Classes.contains(Callee))
	{
		return Callee;
	}

	// Without return types in the API only the xml widgets are known: xml:InitStatic(...)
	const size_t Separator = Callee.find_last_of(".:");
	return GetInitClass(Separator != xr_string::npos ? Callee.substr(Separator + 1).c_str() : Callee);
}

xr_string CXVisor::ResolveType(const xr_string& Expression, const xr_string& Script, const FieldResolver& Fields) const
{
	if (Expression == "self")
	{
		static const std::regex ClassDecl(R"(class\s*\"\w+\"\s*\(\s*(\w+)\s*\))");
		std::smatch Match;
		const std::string Source(Script.c_str());
		if (std::regex_search(Source, Match, ClassDecl) && Classes.contains(Match[1].str().c_str()))
		{
			return Match[1].str().c_str();
		}
		return Classes.contains("CUIScriptWnd") ? "CUIScriptWnd" : "";
	}

	if (Expression.starts_with("self.") && !Fields.empty())
	{
		const xr_string Field = Fields(Expression.substr(5).c_str());
		if (!Field.empty())
		{
			return Field;
		}
	}

	const size_t Dot = Expression.find('.');
	if (Dot != xr_string::npos && Expression.find_first_of(".:", Dot + 1) == xr_string::npos)
	{
		auto NamespaceFieldList = NamespaceFields.find(Expression.substr(0, Dot).c_str());
		if (NamespaceFieldList != NamespaceFields.end())
		{
			auto Field = NamespaceFieldList->second.find(Expression.substr(Dot + 1).c_str());
			if (Field != NamespaceFieldList->second.end() && Classes.contains(Field->second))
			{
				return Field->second;
			}
		}
	}

	// The last assignment from a call: local xml = CScriptXmlInit(), self.btn = xml:Init3tButton(...)
	xr_string Escaped;
	for (char Char : Expression)
	{
		if (Char == '.')
		{
			Escaped += "\\";
		}
		Escaped += Char;
	}

	const std::regex Assign("(?:^|[^\\w.])" + std::string(Escaped.c_str()) + R"(\s*=\s*([\w.:]+)\s*\()");
	const std::string Source(Script.c_str());
	xr_string Result;
	for (std::sregex_iterator It(Source.begin(), Source.end(), Assign), End; It != End; ++It)
	{
		const xr_string Type = ResolveCall((*It)[1].str().c_str(), Script, Fields);
		if (!Type.empty())
		{
			Result = Type;
		}
	}
	return Result;
}

const CXVisor::SLuaFunction* CXVisor::FindCalledFunction(const xr_string& Text, const xr_string& Script, const FieldResolver& Fields, xr_string& Name) const
{
	int Depth = 0;
	int Open = -1;
	for (int Index = (int)Text.size() - 1; Index >= 0; --Index)
	{
		if (Text[Index] == ')')
		{
			++Depth;
		}
		else if (Text[Index] == '(')
		{
			if (Depth == 0)
			{
				Open = Index;
				break;
			}
			--Depth;
		}
	}
	if (Open <= 0)
	{
		return nullptr;
	}

	int Start = Open;
	while (Start > 0 && (IsWordChar(Text[Start - 1]) || Text[Start - 1] == '.' || Text[Start - 1] == ':'))
	{
		--Start;
	}
	const xr_string Callee = Text.substr(Start, Open - Start).c_str();
	if (Callee.empty())
	{
		return nullptr;
	}

	const size_t Separator = Callee.find_last_of(".:");
	if (Separator == xr_string::npos)
	{
		Name = Callee;
		auto Globals = Namespaces.find("");
		if (Globals != Namespaces.end())
		{
			auto Function = Globals->second.find(Callee);
			if (Function != Globals->second.end())
			{
				return &Function->second;
			}
		}
		return nullptr;
	}

	const xr_string Owner = Callee.substr(0, Separator).c_str();
	const xr_string Function = Callee.substr(Separator + 1).c_str();
	auto Namespace = Namespaces.find(Owner);
	if (Callee[Separator] == '.' && Namespace != Namespaces.end())
	{
		auto Found = Namespace->second.find(Function);
		Name = Callee;
		return Found != Namespace->second.end() ? &Found->second : nullptr;
	}

	const xr_string Type = ResolveType(Owner, Script, Fields);
	Name = (Type.empty() ? Owner : Type) + Callee[Separator] + Function;
	return Type.empty() ? nullptr : FindMethod(Type, Function);
}

void CXVisor::CollectScriptNames(const xr_string& Script, xr_hash_map<xr_string, xr_string>& Functions, xr_hash_set<xr_string>& Variables)
{
	size_t At = 0;
	auto SkipSpaces = [&Script, &At]()
	{
		while (At < Script.size() && isspace((u8)Script[At]))
		{
			++At;
		}
	};
	auto ReadWord = [&Script, &At]()
	{
		const size_t Start = At;
		while (At < Script.size() && IsWordChar(Script[At]))
		{
			++At;
		}
		return xr_string(Script.substr(Start, At - Start).c_str());
	};
	auto ReadNames = [&]()
	{
		for (;;)
		{
			SkipSpaces();
			const xr_string Name = ReadWord();
			if (Name.empty() || IsLuaKeyword(Name))
			{
				return;
			}
			Variables.insert(Name);
			SkipSpaces();
			if (At >= Script.size() || Script[At] != ',')
			{
				return;
			}
			++At;
		}
	};

	while (At < Script.size())
	{
		const char Char = Script[At];
		if (Char == '-' && At + 1 < Script.size() && Script[At + 1] == '-')
		{
			const size_t End = Script.find('\n', At);
			At = End == xr_string::npos ? Script.size() : End;
			continue;
		}
		if (Char == '"' || Char == '\'')
		{
			for (++At; At < Script.size() && Script[At] != Char && Script[At] != '\n'; ++At)
			{
				if (Script[At] == '\\')
				{
					++At;
				}
			}
			++At;
			continue;
		}
		if (!IsWordChar(Char) || (At > 0 && (IsWordChar(Script[At - 1]) || Script[At - 1] == '.' || Script[At - 1] == ':')))
		{
			++At;
			continue;
		}

		const xr_string Word = ReadWord();
		if (Word == "local")
		{
			SkipSpaces();
			const size_t Mark = At;
			if (ReadWord() != "function")
			{
				At = Mark;
				ReadNames();
			}
			else
			{
				At = Mark;
			}
		}
		else if (Word == "for")
		{
			ReadNames();
		}
		else if (Word == "function")
		{
			SkipSpaces();
			const size_t NameStart = At;
			while (At < Script.size() && (IsWordChar(Script[At]) || Script[At] == '.' || Script[At] == ':'))
			{
				++At;
			}
			const xr_string Name = Script.substr(NameStart, At - NameStart).c_str();
			SkipSpaces();
			if (At >= Script.size() || Script[At] != '(')
			{
				continue;
			}
			const size_t Close = Script.find(')', At);
			const xr_string Params = Script.substr(At + 1, Close == xr_string::npos ? xr_string::npos : Close - At - 1).c_str();
			if (!Name.empty() && Name.find_first_of(".:") == xr_string::npos)
			{
				Functions[Name] = Params;
			}
			++At;
			ReadNames();
		}
	}

	for (const auto& [Name, Params] : Functions)
	{
		Variables.erase(Name);
	}
	Variables.erase("self");
}

const xr_vector<xr_string>& CXVisor::GetScriptFunctions(const xr_string& Namespace)
{
	auto Cached = ScriptFunctions.find(Namespace);
	if (Cached != ScriptFunctions.end())
	{
		return Cached->second;
	}

	xr_vector<xr_string>& Names = ScriptFunctions[Namespace];
	const xr_string FileName = Namespace + ".script";
	if (FS.exist("$game_scripts$", FileName.c_str()))
	{
		if (IReader* Reader = FS.r_open("$game_scripts$", FileName.c_str()))
		{
			const xr_string Text((const char*)Reader->pointer(), Reader->length());
			FS.r_close(Reader);

			// Only the global functions: "function check_npc_name (actor, npc, p)"
			size_t Begin = 0;
			while (Begin < Text.size())
			{
				size_t End = Text.find('\n', Begin);
				if (End == xr_string::npos)
				{
					End = Text.size();
				}
				if (Text.compare(Begin, 9, "function ") == 0)
				{
					size_t Index = Begin + 9;
					const size_t NameStart = Index;
					while (Index < End && IsWordChar(Text[Index]))
					{
						++Index;
					}
					const xr_string Name = Text.substr(NameStart, Index - NameStart).c_str();
					while (Index < End && (Text[Index] == ' ' || Text[Index] == '\t'))
					{
						++Index;
					}
					if (!Name.empty() && Index < End && Text[Index] == '(')
					{
						Names.push_back(Name);
					}
				}
				Begin = End + 1;
			}
		}
	}

	auto Baked = Namespaces.find(Namespace);
	if (!Namespace.empty() && Baked != Namespaces.end())
	{
		for (const auto& [Name, Function] : Baked->second)
		{
			Names.push_back(Name);
		}
	}

	std::sort(Names.begin(), Names.end());
	Names.erase(std::unique(Names.begin(), Names.end()), Names.end());
	return Names;
}

const xr_vector<xr_string>& CXVisor::GetScriptNamespaces()
{
	if (ScriptNamespacesListed)
	{
		return ScriptNamespaces;
	}
	ScriptNamespacesListed = true;

	FS_FileSet Files;
	if (FS.path_exist("$game_scripts$"))
	{
		FS.file_list(Files, "$game_scripts$", FS_ListFiles | FS_RootOnly, "*.script");
	}
	for (const FS_File& File : Files)
	{
		xr_string Name = File.name.c_str();
		const size_t Extension = Name.rfind('.');
		if (Extension != xr_string::npos)
		{
			Name.resize(Extension);
		}
		ScriptNamespaces.push_back(Name);
	}
	std::sort(ScriptNamespaces.begin(), ScriptNamespaces.end());
	ScriptNamespaces.erase(std::unique(ScriptNamespaces.begin(), ScriptNamespaces.end()), ScriptNamespaces.end());
	return ScriptNamespaces;
}

void CXVisor::GetFunctionHints(const xr_string& Before, xr_vector<xr_string>& Out)
{
	size_t WordStart = Before.size();
	while (WordStart > 0 && (IsWordChar(Before[WordStart - 1]) || Before[WordStart - 1] == '.'))
	{
		--WordStart;
	}
	const xr_string Word = Before.substr(WordStart).c_str();

	const size_t Dot = Word.rfind('.');
	if (Dot == xr_string::npos)
	{
		for (const xr_string& Namespace : GetScriptNamespaces())
		{
			Out.push_back(Namespace + ".");
		}
		return;
	}

	const xr_string Namespace = Word.substr(0, Dot).c_str();
	for (const xr_string& Function : GetScriptFunctions(Namespace))
	{
		Out.push_back(Namespace + "." + Function);
	}
}

void CXVisor::InvalidateScripts()
{
	ScriptFunctions.clear();
	ScriptNamespaces.clear();
	ScriptNamespacesListed = false;
}

void CXVisor::BuildIniLanguage(TextEditor::LanguageDefinition& Out) const
{
	Out = CodeEditor::GetIniLanguage();

	// Hover: the type of a key and the schemas that read it
	xr_hash_map<xr_string, xr_string> Declarations;
	for (const auto& [Name, Schema] : Schemas)
	{
		xr_vector<SLtxKey> Keys;
		ExpandSchema(Name, "", Keys);
		for (const SLtxKey& Key : Keys)
		{
			xr_string& Declaration = Declarations[Key.Key];
			const xr_string Line = Key.Type + " " + Key.Key + (Key.Required ? ", required" : "") + (Key.If.empty() ? "" : ", if " + Key.If) + " (" + Key.Schema + ")";
			if (Declaration.find(Line) == xr_string::npos)
			{
				Declaration += (Declaration.empty() ? "" : "\n") + Line;
			}
		}
	}

	for (const auto& [Key, Declaration] : Declarations)
	{
		TextEditor::Identifier Id;
		Id.mDeclaration = Declaration.c_str();
		Out.mIdentifiers.insert({ Key.c_str(), Id });
	}
}

xr_string CXVisor::GetLtxStatus() const
{
	string256 Text;
	xr_sprintf(Text, "ltx schema: %u schemas, %u class ids, %u sections", (u32)Schemas.size(), (u32)ClassIds.size(), (u32)Sections.size());
	return Text;
}

void CXVisor::ParseLtx(const xr_string& Text, LtxSectionMap& Out, xr_vector<xr_string>* Lines)
{
	xr_string Current;
	int LineIndex = 0;
	size_t Start = 0;
	while (Start <= Text.size())
	{
		size_t End = Text.find('\n', Start);
		if (End == xr_string::npos)
		{
			End = Text.size();
		}
		xr_string Line = Text.substr(Start, End - Start).c_str();
		Start = End + 1;

		const size_t Comment = Line.find(';');
		if (Comment != xr_string::npos)
		{
			Line.resize(Comment);
		}
		Line = Line.Trim();

		// DLTX: ![section] and @[section] override, !key deletes
		bool Override = false;
		if (!Line.empty() && (Line[0] == '!' || Line[0] == '@') && Line.size() > 1 && Line[1] == '[')
		{
			Line.erase(0, 1);
			Override = true;
		}

		if (!Line.empty() && Line[0] == '[')
		{
			const size_t Close = Line.find(']');
			if (Close != xr_string::npos)
			{
				Current = xr_string(Line.substr(1, Close - 1)).Trim();
				SLtxSection& Section = Out[Current];
				Section.Line = LineIndex;
				Section.Override = Override;
				const size_t Colon = Line.find(':', Close);
				if (Colon != xr_string::npos)
				{
					for (const xr_string& Parent : Split(Line.substr(Colon + 1).c_str(), ','))
					{
						Section.Parents.push_back(Parent);
					}
				}
			}
		}
		else if (!Line.empty() && Line[0] != '#' && !Current.empty())
		{
			const size_t Assign = Line.find('=');
			xr_string Key = xr_string(Assign != xr_string::npos ? Line.substr(0, Assign).c_str() : Line).Trim();
			if (!Key.empty() && Key[0] == '!')
			{
				Key.erase(0, 1);
			}
			if (!Key.empty())
			{
				Out[Current].Keys[Key] = Assign != xr_string::npos ? xr_string(Line.substr(Assign + 1)).Trim() : xr_string();
			}
		}

		if (Lines != nullptr)
		{
			Lines->push_back(Current);
		}
		++LineIndex;
		if (End == Text.size())
		{
			break;
		}
	}
}

void CXVisor::IndexConfigs()
{
	Indexed = true;
	Sections.clear();

	string_path Root;
	FS.update_path(Root, _game_config_, "");

	std::error_code Code;
	for (const auto& Entry : std::filesystem::recursive_directory_iterator(Root, Code))
	{
		if (!Entry.is_regular_file() || Entry.path().extension() != ".ltx")
		{
			continue;
		}

		IReader* Reader = FS.r_open(Entry.path().string().c_str());
		if (Reader == nullptr)
		{
			continue;
		}

		xr_string Text((const char*)Reader->pointer(), Reader->length());
		FS.r_close(Reader);
		std::erase(Text, '\r');

		LtxSectionMap File;
		ParseLtx(Text, File);
		for (auto& [Name, Section] : File)
		{
			// DLTX overrides merge into the section
			SLtxSection& Merged = Sections[Name];
			if (!Section.Override && Merged.Path.empty())
			{
				Merged.Path = Entry.path().string().c_str();
				Merged.Line = Section.Line;
			}
			if (Merged.Parents.empty())
			{
				Merged.Parents = Section.Parents;
			}
			for (auto& [Key, Value] : Section.Keys)
			{
				Merged.Keys[Key] = Value;
			}
		}
	}
	Msg("* ltx index: %u sections from %s", (u32)Sections.size(), Root);
}

const CXVisor::SLtxSection* CXVisor::FindSection(const xr_string& Name, const LtxSectionMap* Local)
{
	if (Local != nullptr)
	{
		auto Own = Local->find(Name);
		if (Own != Local->end())
		{
			return &Own->second;
		}
	}

	if (!Indexed)
	{
		IndexConfigs();
	}
	auto It = Sections.find(Name);
	return It != Sections.end() ? &It->second : nullptr;
}

xr_string CXVisor::GetClassId(const xr_string& Name, const LtxSectionMap* Local, int Depth)
{
	const SLtxSection* Section = FindSection(Name, Local);
	if (Section == nullptr || Depth > MaxDepth)
	{
		return "";
	}

	auto Class = Section->Keys.find("class");
	if (Class != Section->Keys.end())
	{
		return Class->second;
	}

	// The last parent wins, like the values of the keys
	for (auto Parent = Section->Parents.rbegin(); Parent != Section->Parents.rend(); ++Parent)
	{
		const xr_string Id = GetClassId(*Parent, Local, Depth + 1);
		if (!Id.empty())
		{
			return Id;
		}
	}
	return "";
}

bool CXVisor::HasKey(const xr_string& Name, const xr_string& Key, const LtxSectionMap* Local, int Depth)
{
	const SLtxSection* Section = FindSection(Name, Local);
	if (Section == nullptr || Depth > MaxDepth)
	{
		return false;
	}
	if (Section->Keys.contains(Key))
	{
		return true;
	}
	for (const xr_string& Parent : Section->Parents)
	{
		if (HasKey(Parent, Key, Local, Depth + 1))
		{
			return true;
		}
	}
	return false;
}

void CXVisor::ExpandSchema(const xr_string& Name, const xr_string& Prefix, xr_vector<SLtxKey>& Out, int Depth) const
{
	auto It = Schemas.find(Name);
	if (It == Schemas.end() || Depth > MaxDepth)
	{
		return;
	}

	for (const xr_string& Base : It->second.Bases)
	{
		ExpandSchema(Base, Prefix, Out, Depth + 1);
	}

	for (const SLtxField& Field : It->second.Fields)
	{
		if (Field.Nested)
		{
			// The nested struct reads its keys with the prefix: hud_offset_pos
			const size_t Scope = Field.Type.rfind("::");
			ExpandSchema(Scope != xr_string::npos ? xr_string(Field.Type.substr(Scope + 2).c_str()) : Field.Type, Prefix + Field.Key, Out, Depth + 1);
			continue;
		}

		const xr_string Key = Prefix + Field.Key;
		if (std::none_of(Out.begin(), Out.end(), [&Key](const SLtxKey& Other) { return Other.Key == Key; }))
		{
			Out.push_back({ Key, Field.Type, Field.Required, Field.If.empty() ? xr_string() : Prefix + Field.If, Name });
		}
	}
}

xr_vector<CXVisor::SLtxKey> CXVisor::GetKeys(const xr_string& Section, const LtxSectionMap* Local)
{
	xr_vector<SLtxKey> Keys;
	auto Class = ClassIds.find(GetClassId(Section, Local));
	if (Class != ClassIds.end())
	{
		for (const xr_string& Schema : Class->second)
		{
			ExpandSchema(Schema, "", Keys);
		}
	}
	return Keys;
}

bool CXVisor::FindDefinition(const xr_string& Name, xr_string& Path, int& Line)
{
	if (!Indexed)
	{
		IndexConfigs();
	}

	auto It = Sections.find(Name);
	if (It == Sections.end() || It->second.Path.empty())
	{
		return false;
	}
	Path = It->second.Path;
	Line = It->second.Line;
	return true;
}
