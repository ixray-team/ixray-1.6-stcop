#include "stdafx.h"
#include "LuaCompletion.h"
#include "IconsFontAwesome6.h"
#include "imgui_internal.h"
#include "../../../../xrScripts/linker.h"
#include "../../../../xrScripts/script_engine.h"
#include "../../../../3rd-party/lua/lauxlib.h"
#include "../../Engine/XrGameManager.h"
#include "../Coder/LtxCompletion.h"

#include <regex>

static constexpr u32 MaxItems = 64;
static constexpr int MaxBaseDepth = 32;

static bool IsWordChar(char Char)
{
	return isalnum((u8)Char) || Char == '_';
}

static bool StartsWithNoCase(const xr_string& Text, const xr_string& Prefix)
{
	if (Prefix.size() > Text.size())
	{
		return false;
	}

	for (size_t Index = 0; Index < Prefix.size(); ++Index)
	{
		if (tolower((u8)Text[Index]) != tolower((u8)Prefix[Index]))
		{
			return false;
		}
	}
	return true;
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

bool CLuaCompletion::Compile(const char* ChunkName, const xr_string& Source, xr_string& Error, int& Line)
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

static void CollectScriptNames(const xr_string& Script, xr_hash_map<xr_string, xr_string>& Functions, xr_hash_set<xr_string>& Variables)
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

u32 CLuaCompletion::BakeGeneration = 1;

xr_string CLuaCompletion::GetApiPath()
{
	string_path Path;
	FS.update_path(Path, "$app_data_root$", "lua_api.txt");
	return Path;
}

bool CLuaCompletion::Bake(xr_string& Error)
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
	if (g_XrGameManager == nullptr || !g_XrGameManager->BakeIniSchema(CLtxCompletion::GetSchemaPath().c_str()))
	{
		Msg("! Lua API: xrGame wrote no ltx schema");
	}

	++BakeGeneration;
	return Load();
}

bool CLuaCompletion::Load()
{
	if (LoadedGeneration == BakeGeneration)
	{
		return Loaded;
	}
	LoadedGeneration = BakeGeneration;
	Loaded = false;
	Classes.clear();
	Namespaces.clear();
	NamespaceFields.clear();

	if (!FS.path_exist("$app_data_root$"))
	{
		return false;
	}

	LoadFile(GetApiPath());

	// HACK: db.actor is nil until the game starts, it is always the actor's CScriptGameObject
	if (Classes.contains("game_object"))
	{
		NamespaceFields["db"]["actor"] = "game_object";
	}

	Loaded = !Classes.empty();
	return Loaded;
}

void CLuaCompletion::LoadFile(const xr_string& Path)
{
	if (!FS.exist(Path.c_str()))
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
			SClass& Class = Classes[Parts[1]];
			if (Parts.size() > 2 && !Parts[2].empty())
			{
				size_t Start = 0;
				for (size_t Comma = Parts[2].find(','); ; Comma = Parts[2].find(',', Start))
				{
					AddUnique(Class.Bases, Parts[2].substr(Start, Comma == xr_string::npos ? xr_string::npos : Comma - Start).c_str());
					if (Comma == xr_string::npos)
					{
						break;
					}
					Start = Comma + 1;
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
}

void CLuaCompletion::BuildLanguage(TextEditor::LanguageDefinition& Out) const
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

xr_string CLuaCompletion::GetInitClass(const xr_string& Function) const
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

void CLuaCompletion::CollectMembers(const xr_string& ClassName, xr_vector<SItem>& Out, xr_hash_set<xr_string>& Seen, int Depth) const
{
	auto It = Classes.find(ClassName);
	if (It == Classes.end() || Depth > MaxBaseDepth)
	{
		return;
	}

	for (const auto& [Name, Function] : It->second.Methods)
	{
		if (Seen.insert(Name).second)
		{
			Out.push_back({ Name, ClassName + ":" + Name + "(" + Function.Overloads.front() + ")", EItemKind::Method });
		}
	}
	for (const xr_string& Name : It->second.Properties)
	{
		if (Seen.insert(Name).second)
		{
			Out.push_back({ Name, ClassName + "." + Name, EItemKind::Property });
		}
	}
	for (const xr_string& Name : It->second.Constants)
	{
		if (Seen.insert(Name).second)
		{
			Out.push_back({ Name, ClassName + "." + Name + " (constant)", EItemKind::Constant });
		}
	}

	for (const xr_string& Base : It->second.Bases)
	{
		CollectMembers(Base, Out, Seen, Depth + 1);
	}
}

const CLuaCompletion::SFunction* CLuaCompletion::FindMethod(const xr_string& ClassName, const xr_string& Name, int Depth) const
{
	auto It = Classes.find(ClassName);
	if (It == Classes.end() || Depth > MaxBaseDepth)
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
		if (const SFunction* Found = FindMethod(Base, Name, Depth + 1))
		{
			return Found;
		}
	}
	return nullptr;
}

xr_string CLuaCompletion::ResolveCall(const xr_string& Callee, const xr_string& Script, const FieldResolver& Fields) const
{
	if (Classes.contains(Callee))
	{
		return Callee;
	}

	// Without return types in the API only the xml widgets are known: xml:InitStatic(...)
	const size_t Separator = Callee.find_last_of(".:");
	return GetInitClass(Separator != xr_string::npos ? Callee.substr(Separator + 1).c_str() : Callee);
}

xr_string CLuaCompletion::ResolveType(const xr_string& Expression, const xr_string& Script, const FieldResolver& Fields) const
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

const CLuaCompletion::SFunction* CLuaCompletion::FindCalledFunction(const xr_string& Text, const xr_string& Script, const FieldResolver& Fields, xr_string& Name) const
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

void CLuaCompletion::BeforeRender(TextEditor& View)
{
	Move = 0;
	AcceptRequest = false;
	CloseRequest = false;
	KeysTaken = false;

	// A click focuses the list before it is drawn
	const ImGuiWindow* Focused = GImGui->NavWindow;
	const bool ListFocused = Open && Focused != nullptr && Focused->RootWindow != nullptr && strstr(Focused->RootWindow->Name, "##lua_completion") != nullptr;
	if (!ListFocused && !ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows))
	{
		Open = false;
		return;
	}

	if (ListFocused)
	{
		return;
	}

	EditorWindow = GImGui->NavWindow;

	if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_Space))
	{
		ForceOpen = true;
		KeysTaken = true;
	}

	if (!Open)
	{
		if (KeysTaken)
		{
			View.SetHandleKeyboardInputs(false);
		}
		return;
	}

	if (ImGui::IsKeyPressed(ImGuiKey_DownArrow))
	{
		Move = 1;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_UpArrow))
	{
		Move = -1;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_PageDown))
	{
		Move = 10;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_PageUp))
	{
		Move = -10;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_Enter) || ImGui::IsKeyPressed(ImGuiKey_KeypadEnter) || ImGui::IsKeyPressed(ImGuiKey_Tab))
	{
		AcceptRequest = true;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_Escape))
	{
		CloseRequest = true;
	}
	else
	{
		static constexpr ImGuiKey FilterKeys[] = { ImGuiKey_M, ImGuiKey_F, ImGuiKey_T, ImGuiKey_N };
		for (int Filter = 0; Filter < (int)EFilter::Count; ++Filter)
		{
			if (ImGui::IsKeyChordPressed(ImGuiMod_Alt | FilterKeys[Filter]))
			{
				Filters[Filter] = !Filters[Filter];
				KeysTaken = true;
			}
		}
	}

	KeysTaken |= Move != 0 || AcceptRequest || CloseRequest;
	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(false);
	}
}

void CLuaCompletion::Accept(TextEditor& View)
{
	if (Current < 0 || Current >= (int)Items.size())
	{
		return;
	}

	const SItem& Item = Items[Current];
	const TextEditor::Coordinates Cursor = View.GetCursorPosition();
	if (!Word.empty())
	{
		// The typed part is a word: no tabs, one column per byte
		View.SetSelection({ Cursor.mLine, std::max(0, Cursor.mColumn - (int)Word.size()) }, Cursor);
		View.Delete();
	}

	if (!IsCallable(Item.Kind))
	{
		View.InsertText(Item.Name.c_str());
		return;
	}

	// The cursor goes between the brackets, the signature hint shows up there
	View.InsertText((Item.Name + "()").c_str());
	const TextEditor::Coordinates End = View.GetCursorPosition();
	View.SetCursorPosition({ End.mLine, std::max(0, End.mColumn - 1) });
}

CLuaCompletion::EFilter CLuaCompletion::GetFilter(EItemKind Kind)
{
	switch (Kind)
	{
	case EItemKind::Function:	return EFilter::Functions;
	case EItemKind::Class:		return EFilter::Types;
	case EItemKind::Namespace:	return EFilter::Namespaces;
	default:					return EFilter::Members;
	}
}

bool CLuaCompletion::IsCallable(EItemKind Kind)
{
	return Kind == EItemKind::Method || Kind == EItemKind::Function || Kind == EItemKind::Class;
}

struct SKindStyle
{
	const char* Icon;
	ImU32 Color;
};

static SKindStyle GetKindStyle(int Kind)
{
	static const SKindStyle Styles[] =
	{
		{ ICON_FA_CUBE, IM_COL32(180, 130, 255, 255) },		// Method
		{ ICON_FA_WRENCH, IM_COL32(200, 200, 200, 255) },	// Property
		{ ICON_FA_LOCK, IM_COL32(120, 170, 255, 255) },		// Constant
		{ ICON_FA_CUBE, IM_COL32(180, 130, 255, 255) },		// Function
		{ ICON_FA_TAG, IM_COL32(100, 180, 255, 255) },		// Field
		{ ICON_FA_SHAPES, IM_COL32(255, 175, 80, 255) },	// Class
		{ ICON_FA_LAYER_GROUP, IM_COL32(200, 200, 200, 255) },	// Namespace
	};
	return Kind >= 0 && Kind < (int)std::size(Styles) ? Styles[Kind] : Styles[0];
}

int CLuaCompletion::DrawItem(const SItem& Item, bool Selected) const
{
	const SKindStyle Style = GetKindStyle((int)Item.Kind);
	const ImVec2 Start = ImGui::GetCursorScreenPos();
	const float IconWidth = ImGui::GetFrameHeight();

	const bool Clicked = ImGui::Selectable(("##" + Item.Name).c_str(), Selected, ImGuiSelectableFlags_AllowDoubleClick);
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	DrawList->AddText({ Start.x + 2.0f, Start.y }, Style.Color, Style.Icon);
	DrawList->AddText({ Start.x + IconWidth, Start.y }, ImGui::GetColorU32(ImGuiCol_Text), Item.Name.c_str());

	if (!Clicked)
	{
		return 0;
	}
	return ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) ? 2 : 1;
}

static const char* FilterKeys[] = { "M", "F", "T", "N" };
static const char* FilterNames[] = { "Members (Alt+M)", "Functions (Alt+F)", "Types (Alt+T)", "Namespaces (Alt+N)" };
static const int FilterKinds[] = { 0, 3, 5, 6 };

float CLuaCompletion::GetFiltersWidth(const u32 (&Counts)[(int)EFilter::Count]) const
{
	const ImGuiStyle& Style = ImGui::GetStyle();
	float Width = 0.0f;
	for (int Filter = 0; Filter < (int)EFilter::Count; ++Filter)
	{
		if (Counts[Filter] != 0)
		{
			const xr_string Label = xr_string(GetKindStyle(FilterKinds[Filter]).Icon) + " " + FilterKeys[Filter];
			Width += ImGui::CalcTextSize(Label.c_str()).x + Style.FramePadding.x * 2.0f + Style.ItemSpacing.x;
		}
	}
	return Width;
}

bool CLuaCompletion::DrawFilters(const u32 (&Counts)[(int)EFilter::Count])
{
	bool Clicked = false;
	bool First = true;
	for (int Filter = 0; Filter < (int)EFilter::Count; ++Filter)
	{
		if (Counts[Filter] == 0)
		{
			continue;
		}

		if (!First)
		{
			ImGui::SameLine();
		}
		First = false;

		const SKindStyle Style = GetKindStyle(FilterKinds[Filter]);
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(Filters[Filter] ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
		ImGui::PushStyleColor(ImGuiCol_Text, Filters[Filter] ? ImGui::ColorConvertU32ToFloat4(Style.Color) : ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled));
		const xr_string Label = xr_string(Style.Icon) + " " + FilterKeys[Filter] + "##filter" + FilterKeys[Filter];
		if (ImGui::Button(Label.c_str()))
		{
			Filters[Filter] = !Filters[Filter];
			Clicked = true;
		}
		ImGui::PopStyleColor(2);
		ImGui::SetItemTooltip("%s: %u", FilterNames[Filter], Counts[Filter]);
	}
	return Clicked;
}

void CLuaCompletion::AfterRender(TextEditor& View, const xr_string& Script, const FieldResolver& Fields)
{
	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(true);
	}

	if (!Load())
	{
		Open = false;
		return;
	}

	if (CloseRequest)
	{
		Open = false;
		return;
	}

	if (AcceptRequest)
	{
		Accept(View);
		Open = false;
		return;
	}

	char Buffer[512];
	View.CopyTextBeforeCursor(Buffer, sizeof(Buffer));
	const xr_string Text = Buffer;

	size_t WordStart = Text.size();
	while (WordStart > 0 && IsWordChar(Text[WordStart - 1]))
	{
		--WordStart;
	}
	Word = Text.substr(WordStart).c_str();

	const char Separator = WordStart > 0 ? Text[WordStart - 1] : 0;
	const bool IsMember = Separator == '.' || Separator == ':';
	xr_string Expression;
	if (IsMember)
	{
		size_t ExpressionStart = WordStart - 1;
		while (ExpressionStart > 0 && (IsWordChar(Text[ExpressionStart - 1]) || Text[ExpressionStart - 1] == '.' || Text[ExpressionStart - 1] == ':'))
		{
			--ExpressionStart;
		}
		Expression = Text.substr(ExpressionStart, WordStart - 1 - ExpressionStart).c_str();
	}

	const bool InComment = Text.find("--") != xr_string::npos;
	const bool InString = std::count(Text.begin(), Text.end(), '"') % 2 == 1 || std::count(Text.begin(), Text.end(), '\'') % 2 == 1;
	const int CursorLine = View.GetCursorPosition().mLine;

	if (View.IsTextChanged() || ForceOpen)
	{
		const bool Typed = !Text.empty() && (IsWordChar(Text.back()) || Text.back() == '.' || Text.back() == ':');
		Open = !InComment && !InString && (ForceOpen || (Typed && (IsMember || Word.size() >= 2)));
		Current = 0;
		Line = CursorLine;
	}
	ForceOpen = false;

	if (Open && (CursorLine != Line || (!IsMember && Word.empty()) || InComment || InString))
	{
		Open = false;
	}

	if (Open)
	{
		xr_vector<SItem> All;
		xr_hash_set<xr_string> Seen;
		if (IsMember)
		{
			auto Namespace = Namespaces.find(Expression);
			if (Separator == '.' && Namespace != Namespaces.end())
			{
				for (const auto& [Name, Function] : Namespace->second)
				{
					All.push_back({ Name, Expression + "." + Name + "(" + Function.Overloads.front() + ")", EItemKind::Function });
				}

				auto NamespaceFieldList = NamespaceFields.find(Expression);
				if (NamespaceFieldList != NamespaceFields.end())
				{
					for (const auto& [Name, Type] : NamespaceFieldList->second)
					{
						if (!Namespace->second.contains(Name))
						{
							All.push_back({ Name, Expression + "." + Name + ": " + Type, EItemKind::Field });
						}
					}
				}
			}
			else
			{
				const xr_string Type = ResolveType(Expression, Script, Fields);
				if (!Type.empty())
				{
					CollectMembers(Type, All, Seen);
				}
				else
				{
					for (const auto& [ClassName, Class] : Classes)
					{
						CollectMembers(ClassName, All, Seen, MaxBaseDepth);
					}
				}
			}
		}
		else
		{
			xr_hash_map<xr_string, xr_string> ScriptFunctions;
			xr_hash_set<xr_string> ScriptVariables;
			CollectScriptNames(Script, ScriptFunctions, ScriptVariables);
			for (const auto& [Name, Params] : ScriptFunctions)
			{
				Seen.insert(Name);
				All.push_back({ Name, "function " + Name + "(" + Params + ")", EItemKind::Function });
			}
			for (const xr_string& Name : ScriptVariables)
			{
				Seen.insert(Name);
				All.push_back({ Name, "local " + Name, EItemKind::Field });
			}

			auto Globals = Namespaces.find("");
			if (Globals != Namespaces.end())
			{
				for (const auto& [Name, Function] : Globals->second)
				{
					if (!Seen.contains(Name))
					{
						All.push_back({ Name, Name + "(" + Function.Overloads.front() + ")", EItemKind::Function });
					}
				}
			}
			for (const auto& [Name, Functions] : Namespaces)
			{
				if (!Name.empty() && !Seen.contains(Name) && !(Globals != Namespaces.end() && Globals->second.contains(Name)))
				{
					All.push_back({ Name, "namespace " + Name + ", " + xr_string::ToString((u32)Functions.size()) + " functions", EItemKind::Namespace });
				}
			}
			for (const auto& [Name, Class] : Classes)
			{
				All.push_back({ Name, "class " + Name, EItemKind::Class });
			}

			auto GlobalFields = NamespaceFields.find("");
			if (GlobalFields != NamespaceFields.end())
			{
				for (const auto& [Name, Type] : GlobalFields->second)
				{
					if (!Seen.contains(Name) && !Namespaces.contains(Name) && !Classes.contains(Name) && !(Globals != Namespaces.end() && Globals->second.contains(Name)))
					{
						All.push_back({ Name, Name + ": " + Type, EItemKind::Field });
					}
				}
			}
		}

		Items.clear();
		std::fill(std::begin(FilterCounts), std::end(FilterCounts), 0u);
		for (SItem& Item : All)
		{
			if (!StartsWithNoCase(Item.Name, Word) || Item.Name == Word)
			{
				continue;
			}

			const int Filter = (int)GetFilter(Item.Kind);
			++FilterCounts[Filter];
			if (Filters[Filter])
			{
				Items.push_back(std::move(Item));
			}
		}
		std::sort(Items.begin(), Items.end(), [](const SItem& Left, const SItem& Right) { return Left.Name < Right.Name; });
		if (Items.size() > MaxItems)
		{
			Items.resize(MaxItems);
		}
		Open = std::any_of(std::begin(FilterCounts), std::end(FilterCounts), [](u32 Count) { return Count != 0; });
		Current = std::clamp(Current + Move, 0, std::max(0, (int)Items.size() - 1));
	}

	const ImVec2 CursorPos = View.GetCursorScreenPos();
	const ImGuiWindowFlags Flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings
		| ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_Tooltip;
	const ImGuiWindowFlags ListFlags = (Flags & ~(ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_AlwaysAutoResize)) | ImGuiWindowFlags_NoScrollbar;

	if (Open)
	{
		static constexpr int MaxVisible = 14;
		const float IconWidth = ImGui::GetFrameHeight();
		float Width = 280.0f;
		for (const SItem& Item : Items)
		{
			Width = std::max(Width, IconWidth + ImGui::CalcTextSize(Item.Name.c_str()).x);
		}
		const ImGuiStyle& Style = ImGui::GetStyle();
		const int Rows = std::max(1, std::min((int)Items.size(), MaxVisible));
		const float ListHeight = Rows * ImGui::GetTextLineHeightWithSpacing();
		const float FooterHeight = Style.ItemSpacing.y * 2.0f + 1.0f + ImGui::GetFrameHeight();
		const float Height = ListHeight + FooterHeight + Style.ItemSpacing.y + Style.WindowPadding.y * 2.0f;
		Width = std::max(Width + Style.ScrollbarSize, GetFiltersWidth(FilterCounts));
		Width = std::min(Width + Style.WindowPadding.x * 2.0f, 700.0f);

		ImGui::SetNextWindowPos(CursorPos);
		ImGui::SetNextWindowSize({ Width, Height });
		bool Clicked = false;
		bool Insert = false;
		if (ImGui::Begin("##lua_completion", nullptr, ListFlags))
		{
			if (ImGui::BeginChild("##lua_completion_items", { 0.0f, ListHeight }, ImGuiChildFlags_None, ImGuiWindowFlags_NoNav))
			{
				for (int Index = 0; Index < (int)Items.size(); ++Index)
				{
					const bool Selected = Index == Current;
					const int Click = DrawItem(Items[Index], Selected);
					if (Click != 0)
					{
						Current = Index;
						Clicked = true;
						Insert = Click == 2;
					}
					if (Selected && Move != 0)
					{
						ImGui::SetScrollHereY();
					}
				}
				if (Items.empty())
				{
					ImGui::TextDisabled("Everything is filtered out");
				}
			}
			ImGui::EndChild();

			ImGui::Separator();
			Clicked |= DrawFilters(FilterCounts);
		}
		ImGui::End();

		if (Clicked && EditorWindow != nullptr)
		{
			ImGui::FocusWindow(EditorWindow);
		}
		if (Insert)
		{
			Accept(View);
			Open = false;
			return;
		}

		if (!Items.empty())
		{
			ImGui::SetNextWindowPos({ CursorPos.x, CursorPos.y - ImGui::GetTextLineHeightWithSpacing() * 2.0f });
			if (ImGui::Begin("##lua_completion_detail", nullptr, Flags))
			{
				ImGui::TextUnformatted(Items[Current].Detail.c_str());
			}
			ImGui::End();
		}
		return;
	}

	if (!InComment)
	{
		xr_string Name;
		if (const SFunction* Function = FindCalledFunction(Text, Script, Fields, Name))
		{
			ImGui::SetNextWindowPos({ CursorPos.x, CursorPos.y - ImGui::GetTextLineHeightWithSpacing() * (Function->Overloads.size() + 1.5f) });
			if (ImGui::Begin("##lua_signature", nullptr, Flags))
			{
				for (const xr_string& Overload : Function->Overloads)
				{
					ImGui::Text("%s(%s)", Name.c_str(), Overload.c_str());
				}
			}
			ImGui::End();
		}
	}
}
