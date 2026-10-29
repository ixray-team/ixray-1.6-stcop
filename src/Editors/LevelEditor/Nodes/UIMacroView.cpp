#include "stdafx.h"
#include "UIMacroView.h"
#include "../../xrEUI/IconsFontAwesome6.h"
#include "NodesCommon.h"
#include "UINodesEditorForm.h"

namespace ed = ax::NodeEditor;
using namespace NodesUI;

static const char* GraphWindow = "Macro Graph###LogicGraph";
static const char* OutlinerWindow = "Outliner###LogicOutliner";
static const char* DetailsWindow = "Details###LogicDetails";
static const char* MessagesWindow = "Message Log###LogicMessages";
static const char* MacroFile = "editor_macros.json";
static constexpr size_t UndoLimit = 128;

static const ImColor ColorStart = ImColor(200, 60, 60);
static const ImColor ColorVoid = ImColor(90, 130, 210);
static const ImColor ColorOneArg = ImColor(110, 200, 120);
static const ImColor ColorTwoArg = ImColor(200, 160, 90);

static std::array<size_t, 13> CommandsOneArgsInt =
{
	COMMAND_CHANGE_ACTION,    // u32
	COMMAND_CHANGE_AXIS,      // u32
	COMMAND_UPDATE_PROPERTIES,// u32
	COMMAND_ZOOM_EXTENTS,     // u32
	COMMAND_GRID_NUMBER_OF_SLOTS, // u32
	COMMAND_GRID_SLOT_SIZE,   // u32
	COMMAND_MUTE_SOUND,       // u32
	COMMAND_LOG_COMMANDS,     // u32
	COMMAND_HIDE_ALL,         // u32
	COMMAND_HIDE_SEL,		  // u32
	COMMAND_LOCK_ALL,		  // u32
	COMMAND_LOCK_SEL,		  // u32
	COMMAND_SHOWCONTEXTMENU   // u32
};

static std::array<size_t, 8> CommandsOneArgsString =
{
	COMMAND_ICON_REMOVE,      // string
	COMMAND_LOAD,             // string
	COMMAND_UNLOAD_LEVEL_PART,// string
	COMMAND_LOAD_LEVEL_PART,  // string
	COMMAND_EXECUTE_COMMAND_LIST, // string
	COMMAND_ICON_PICKER,      // string
	COMMAND_SOUND_EDITOR,     // string
	COMMAND_SHOW_PROPERTIES   // string ?
};

static std::array<size_t, 9> CommandsTwoArgs =
{
	COMMAND_SET_SETTINGS,
	COMMAND_IMAGE_EDITOR_SELECT,
	COMMAND_RUN_MACRO,
	COMMAND_ASSIGN_MACRO,
	COMMAND_CHANGE_TARGET,
	COMMAND_ENABLE_TARGET,
	COMMAND_SHOW_TARGET,
	COMMAND_READONLY_TARGET,
	COMMAND_SAVE
};

template<typename T>
static bool Contains(const T& Array, size_t Value)
{
	return std::find(Array.begin(), Array.end(), Value) != Array.end();
}

static ImColor TypeColor(MacroType Type)
{
	switch (Type)
	{
	case MacroType::eOneArg:
		return ColorOneArg;
	case MacroType::eTwoArg:
		return ColorTwoArg;
	default:
		return ColorVoid;
	}
}

static xr_string MacroFilePath()
{
	string_path Path = {};
	FS.update_path(Path, "$app_data_root$", MacroFile);
	return Path;
}

CUIMacroView::CUIMacroView()
{
	// "COMMAND_LOAD_LEVEL_PART" is shown as "Load level part"
	auto AddCommand = [this](u32 Command, std::string_view EnumName)
	{
		if (!EnumName.starts_with("COMMAND_"))
		{
			return;
		}

		SCommand Item;
		Item.Command = Command;
		Item.Name = xr_string(EnumName.substr(xr_strlen("COMMAND_")).data(), EnumName.size() - xr_strlen("COMMAND_"));
		for (size_t Index = 0; Index < Item.Name.size(); ++Index)
		{
			char& Char = Item.Name[Index];
			Char = Char == '_' ? ' ' : Index == 0 ? Char : (char)tolower((u8)Char);
		}

		const bool OneArg = Contains(CommandsOneArgsInt, Command) || Contains(CommandsOneArgsString, Command);
		Item.Type = OneArg ? MacroType::eOneArg : Contains(CommandsTwoArgs, Command) ? MacroType::eTwoArg : MacroType::eVoid;
		Item.IntArg = Contains(CommandsOneArgsInt, Command);
		Commands.push_back(std::move(Item));
	};

	for (u32 Command = COMMAND_INITIALIZE + 1; Command < COMMAND_MAIN_LAST; ++Command)
	{
		AddCommand(Command, magic_enum::enum_name((ECoreCommands)Command));
	}
	for (u32 Command = COMMAND_EXTFIRST_EXT + 1; Command < COMMAND_LE_END; ++Command)
	{
		AddCommand(Command, magic_enum::enum_name((ELECommand)Command));
	}

	std::sort(Commands.begin(), Commands.end(), [](const SCommand& Left, const SCommand& Right)
	{
		return Left.Name < Right.Name;
	});

	ed::Config Config;
	Config.SettingsFile = nullptr;
	Editor = ed::CreateEditor(&Config);

	Properties = new UIPropertiesForm();

	if (!LoadFile())
	{
		Read(nlohmann::json::object());
	}
}

CUIMacroView::~CUIMacroView()
{
	xr_delete(Properties);
	if (Editor != nullptr)
	{
		ed::DestroyEditor(Editor);
	}
}

const char* CUIMacroView::GetName() const
{
	return "Macros";
}

const char* CUIMacroView::GetIcon() const
{
	return ICON_FA_SQUARE_SHARE_NODES;
}

void CUIMacroView::GetWindows(xr_vector<SNodesWindow>& Windows) const
{
	Windows.push_back({ GraphWindow, ENodesDock::Graph });
	Windows.push_back({ OutlinerWindow, ENodesDock::Outliner });
	Windows.push_back({ DetailsWindow, ENodesDock::Details });
	Windows.push_back({ MessagesWindow, ENodesDock::Bottom });
}

bool CUIMacroView::IsModeModified() const
{
	return Modified;
}

xr_string CUIMacroView::GetModePath() const
{
	return xr_string();
}

void CUIMacroView::SerializeMode(nlohmann::json&, bool)
{
	// The graph is kept in its own file
}

// The ids of the nodes, the pins and the links must not be equal: they are ImGui ids of one canvas
static constexpr u64 PinFlag = 1ull << 48;
static constexpr u64 LinkFlag = 1ull << 62;

ed::PinId CUIMacroView::InputPin(int Node)
{
	return ed::PinId(PinFlag | ((u64)Node * 2));
}

ed::PinId CUIMacroView::OutputPin(int Node)
{
	return ed::PinId(PinFlag | ((u64)Node * 2 + 1));
}

ed::LinkId CUIMacroView::LinkOf(int From, int To)
{
	return ed::LinkId(LinkFlag | ((u64)(u32)From << 24) | (u32)To);
}

int CUIMacroView::PinNode(ed::PinId Pin, bool& IsOutput)
{
	const u64 Value = Pin.Get() & ~PinFlag;
	IsOutput = (Value & 1) != 0;
	return (int)(Value / 2);
}

CUIMacroView::SNode* CUIMacroView::Find(int Id)
{
	auto Iter = std::find_if(Nodes.begin(), Nodes.end(), [Id](const SNode& Node) { return Node.Id == Id; });
	return Iter != Nodes.end() ? &*Iter : nullptr;
}

const CUIMacroView::SCommand* CUIMacroView::CommandOf(const SNode& Node) const
{
	return Node.Command >= 0 && Node.Command < (int)Commands.size() ? &Commands[Node.Command] : nullptr;
}

int CUIMacroView::FindCommand(const xr_string& Name) const
{
	for (size_t Index = 0; Index < Commands.size(); ++Index)
	{
		if (Commands[Index].Name == Name)
		{
			return (int)Index;
		}
	}
	return -1;
}

CUIMacroView::SNode& CUIMacroView::CreateNode(int Command, ImVec2 Position)
{
	SNode& Node = Nodes.emplace_back();
	Node.Id = NextId++;
	Node.Command = Command;
	Node.Position = Position;
	Node.HasPosition = true;
	return Node;
}

void CUIMacroView::RemoveNode(int Id)
{
	if (Id == StartId)
	{
		return;
	}
	std::erase_if(Nodes, [Id](const SNode& Node) { return Node.Id == Id; });
	for (SNode& Node : Nodes)
	{
		std::erase(Node.Next, Id);
	}
	if (Selected == Id)
	{
		Selected = 0;
	}
}

nlohmann::json CUIMacroView::Write() const
{
	nlohmann::json Data = nlohmann::json::object();
	nlohmann::json Items = nlohmann::json::array();

	ed::EditorContext* Previous = ed::GetCurrentEditor();
	ed::SetCurrentEditor(Editor);
	for (const SNode& Node : Nodes)
	{
		const ImVec2 Position = Node.HasPosition ? Node.Position : ed::GetNodePosition(Node.Id);
		nlohmann::json Item = nlohmann::json::object();
		Item["id"] = Node.Id;
		if (const SCommand* Command = CommandOf(Node))
		{
			Item["command"] = Command->Name.c_str();
		}
		Item["first"] = Node.First.c_str();
		Item["second"] = Node.Second.c_str();
		Item["x"] = std::isfinite(Position.x) ? Position.x : 0.0f;
		Item["y"] = std::isfinite(Position.y) ? Position.y : 0.0f;
		Item["next"] = Node.Next;
		Items.push_back(Item);
	}
	ed::SetCurrentEditor(Previous);

	Data["nodes"] = Items;
	return Data;
}

void CUIMacroView::Read(const nlohmann::json& Data)
{
	Nodes.clear();
	NextId = StartId + 1;
	Selected = 0;
	PropertiesNode = -1;

	auto Iter = Data.find("nodes");
	if (Iter != Data.end() && Iter->is_array())
	{
		for (const nlohmann::json& Item : *Iter)
		{
			if (!Item.is_object() || !Item.contains("id") || !Item["id"].is_number_integer())
			{
				continue;
			}

			SNode Node;
			Node.Id = Item["id"].get<int>();
			if (Item.contains("command") && Item["command"].is_string())
			{
				Node.Command = FindCommand(Item["command"].get_ref<const nlohmann::json::string_t&>().c_str());
				if (Node.Command < 0)
				{
					Msg("! Macros: unknown command [%s]", Item["command"].get_ref<const nlohmann::json::string_t&>().c_str());
					continue;
				}
			}
			else if (Node.Id != StartId)
			{
				continue;
			}

			if (Item.contains("first") && Item["first"].is_string())
			{
				Node.First = Item["first"].get_ref<const nlohmann::json::string_t&>().c_str();
			}
			if (Item.contains("second") && Item["second"].is_string())
			{
				Node.Second = Item["second"].get_ref<const nlohmann::json::string_t&>().c_str();
			}
			if (Item.contains("x") && Item["x"].is_number() && Item.contains("y") && Item["y"].is_number())
			{
				Node.Position = { Item["x"].get<float>(), Item["y"].get<float>() };
				Node.HasPosition = true;
			}
			if (Item.contains("next") && Item["next"].is_array())
			{
				for (const nlohmann::json& Next : Item["next"])
				{
					if (Next.is_number_integer())
					{
						Node.Next.push_back(Next.get<int>());
					}
				}
			}

			NextId = std::max(NextId, Node.Id + 1);
			Nodes.push_back(std::move(Node));
		}
	}

	if (Find(StartId) == nullptr)
	{
		SNode Start;
		Start.Id = StartId;
		Start.HasPosition = true;
		Nodes.insert(Nodes.begin(), std::move(Start));
	}

	// Links to the nodes that were not read
	for (SNode& Node : Nodes)
	{
		std::erase_if(Node.Next, [this](int Id) { return Id == StartId || Find(Id) == nullptr; });
	}
}

bool CUIMacroView::LoadFile()
{
	const xr_string Path = MacroFilePath();
	if (!FS.exist(Path.c_str()))
	{
		return false;
	}

	IReader* File = FS.r_open(Path.c_str());
	if (File == nullptr)
	{
		return false;
	}
	const xr_string Text((const char*)File->pointer(), File->length());
	FS.r_close(File);

	nlohmann::json Data = nlohmann::json::parse(Text.c_str(), nullptr, false);
	if (Data.is_discarded())
	{
		Msg("! Macros: can't parse %s", Path.c_str());
		return false;
	}

	Read(Data);
	UndoStack.clear();
	RedoStack.clear();
	SavedState = Write().dump().c_str();
	Modified = false;
	NeedFit = true;
	return true;
}

bool CUIMacroView::SaveMode()
{
	const xr_string Path = MacroFilePath();
	IWriter* File = FS.w_open(Path.c_str());
	if (File == nullptr)
	{
		ELog.DlgMsg(mtError, "Macros: can't write %s", Path.c_str());
		return false;
	}
	const std::string Text = Write().dump(1, '\t');
	File->w(Text.data(), (u32)Text.size());
	FS.w_close(File);

	SavedState = Write().dump().c_str();
	Modified = false;
	Msg("* Macros: %s saved", Path.c_str());
	return true;
}

bool CUIMacroView::ReloadMode()
{
	if (Modified && ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "Discard the changes of the macros?") != mrYes)
	{
		return false;
	}
	if (!LoadFile())
	{
		Read(nlohmann::json::object());
		SavedState = Write().dump().c_str();
		Modified = false;
	}
	return true;
}

void CUIMacroView::PushUndo()
{
	UndoStack.push_back(Write().dump().c_str());
	if (UndoStack.size() > UndoLimit)
	{
		UndoStack.erase(UndoStack.begin());
	}
	RedoStack.clear();
}

void CUIMacroView::MarkModified()
{
	Modified = true;
	// The links and the arguments shown in the Details are made again
	PropertiesNode = -1;
}

bool CUIMacroView::UndoMode(bool Redo)
{
	xr_vector<xr_string>& From = Redo ? RedoStack : UndoStack;
	xr_vector<xr_string>& To = Redo ? UndoStack : RedoStack;
	if (From.empty())
	{
		return false;
	}

	To.push_back(Write().dump().c_str());
	const nlohmann::json Data = nlohmann::json::parse(From.back().c_str(), nullptr, false);
	From.pop_back();
	Read(Data);
	MarkModified();
	Modified = xr_string(Write().dump().c_str()) != SavedState;
	return true;
}

void CUIMacroView::Exec()
{
	Messages.clear();
	const SNode* Start = Find(StartId);
	if (Start == nullptr || Start->Next.empty())
	{
		Messages.push_back({ true, StartId, "Nothing is linked to the Start node" });
		return;
	}

	xr_set<int> Visited;
	ExecNode(*Start, Visited);
	Messages.push_back({ false, StartId, make_string<xr_string>("Done: %u commands", (u32)Visited.size() - 1) });
}

void CUIMacroView::ExecNode(const SNode& Node, xr_set<int>& Visited)
{
	if (!Visited.insert(Node.Id).second)
	{
		Messages.push_back({ true, Node.Id, "The node is run again: the loop is stopped" });
		return;
	}

	if (const SCommand* Command = CommandOf(Node))
	{
		CCommandVar Result;
		if (Command->Type == MacroType::eVoid)
		{
			Result = ExecCommand(Command->Command);
		}
		else if (Command->Type == MacroType::eOneArg && Command->IntArg)
		{
			Result = ExecCommand(Command->Command, u32(Node.First.empty() ? 0 : atoi(Node.First.c_str())));
		}
		else if (Command->Type == MacroType::eOneArg)
		{
			Result = ExecCommand(Command->Command, Node.First);
		}
		else
		{
			Result = ExecCommand(Command->Command, Node.First, Node.Second);
		}

		// The commands return 0 when they fail
		const bool Failed = Result.IsInteger() && (u32)Result == 0;
		Messages.push_back({ Failed, Node.Id, Command->Name + (Failed ? ": failed" : ": done") });
	}

	// The copy: a command may change the graph
	const xr_vector<int> Next = Node.Next;
	for (int Id : Next)
	{
		if (const SNode* Target = Find(Id))
		{
			ExecNode(*Target, Visited);
		}
	}
}

void CUIMacroView::Layout()
{
	// Columns by the distance from the Start node
	xr_hash_map<int, int> Depth;
	xr_vector<int> Queue = { StartId };
	Depth[StartId] = 0;
	for (size_t Index = 0; Index < Queue.size(); ++Index)
	{
		const SNode* Node = Find(Queue[Index]);
		if (Node == nullptr)
		{
			continue;
		}
		for (int Next : Node->Next)
		{
			if (!Depth.contains(Next))
			{
				Depth[Next] = Depth[Queue[Index]] + 1;
				Queue.push_back(Next);
			}
		}
	}

	ed::EditorContext* Previous = ed::GetCurrentEditor();
	ed::SetCurrentEditor(Editor);
	xr_hash_map<int, float> ColumnY;
	int Unlinked = 0;
	for (int Id : Queue)
	{
		SNode* Node = Find(Id);
		const int Column = Depth[Id];
		Node->Position = { Column * 320.0f, ColumnY[Column] };
		Node->HasPosition = true;
		ColumnY[Column] += ed::GetNodeSize(Id).y + 40.0f;
	}
	float LastY = 0.0f;
	for (const auto& [Column, Y] : ColumnY)
	{
		LastY = std::max(LastY, Y);
	}
	for (SNode& Node : Nodes)
	{
		if (!Depth.contains(Node.Id))
		{
			Node.Position = { (Unlinked++ % 4) * 320.0f, LastY + 120.0f + (Unlinked / 4) * 160.0f };
			Node.HasPosition = true;
		}
	}
	ed::SetCurrentEditor(Previous);
	NeedFit = true;
}

void CUIMacroView::DrawModeTopBar(float ButtonSize)
{
	auto Hint = [](const char* Text)
	{
		if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("%s", Text);
		}
	};

	if (XRay::ImGui::ToolbarButton("##macro_run", ICON_FA_PLAY " Run", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersAll))
	{
		Exec();
	}
	Hint("Run the commands from the Start node (F5)");

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##macro_layout", ICON_FA_SITEMAP " Auto Layout", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersLeft))
	{
		PushUndo();
		LayoutDelay = 1;
		MarkModified();
	}
	Hint("Arrange the commands by their order");

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##macro_fit", ICON_FA_EXPAND " Fit", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersRight))
	{
		NeedFit = true;
	}
	Hint("Show the whole graph (F)");
}

void CUIMacroView::DrawMode()
{
	Dock.BeginFrame();
	DrawGraph();
	DrawOutliner();
	DrawDetails();
	DrawMessages();
}

void CUIMacroView::DrawNode(SNode& Node)
{
	const SCommand* Command = CommandOf(Node);
	const bool IsStart = Node.Id == StartId;
	const ImColor HeaderColor = IsStart ? ColorStart : TypeColor(Command->Type);
	Minimap.AddNode(Node.Id, HeaderColor);
	const char* Title = IsStart ? "Start" : Command->Name.c_str();
	const float Spacing = ImGui::GetStyle().ItemSpacing.x;
	const float Width = std::clamp(ImGui::CalcTextSize(Title).x + PinSize * 2.0f + 70.0f, 200.0f, 420.0f);

	if (Node.HasPosition)
	{
		ed::SetNodePosition(Node.Id, Node.Position);
		Node.HasPosition = false;
	}

	ed::BeginNode(Node.Id);
	ImGui::PushID(Node.Id);
	const ImVec2 Origin = ImGui::GetCursorScreenPos();

	// Header: the input, the title and the output
	if (IsStart)
	{
		ImGui::Dummy({ PinSize, PinSize });
	}
	else
	{
		ed::BeginPin(InputPin(Node.Id), ed::PinKind::Input);
		ed::PinPivotAlignment({ 0.0f, 0.5f });
		ed::PinPivotSize({ 0, 0 });
		ImGui::Dummy({ PinSize, PinSize });
		DrawExecPin(ImGui::GetWindowDrawList(), ImGui::GetItemRectMin(), PinSize, ColorPin, ed::PinHadAnyLinks(InputPin(Node.Id)));
		ed::EndPin();
	}
	ImGui::SameLine();
	ImGui::Text("%s %s", IsStart ? ICON_FA_PLAY : ICON_FA_TERMINAL, Title);

	ImGui::SameLine();
	ImGui::SetCursorScreenPos({ Origin.x + Width - PinSize, Origin.y });
	ed::BeginPin(OutputPin(Node.Id), ed::PinKind::Output);
	ed::PinPivotAlignment({ 1.0f, 0.5f });
	ed::PinPivotSize({ 0, 0 });
	ImGui::Dummy({ PinSize, PinSize });
	DrawExecPin(ImGui::GetWindowDrawList(), ImGui::GetItemRectMin(), PinSize, ColorPin, !Node.Next.empty());
	ed::EndPin();

	ImGui::SetCursorScreenPos({ Origin.x + PinSize + Spacing, ImGui::GetItemRectMax().y + 2.0f });
	{
		const char* Caption = IsStart ? "Event" : Command->Type == MacroType::eVoid ? "Command" : Command->IntArg ? "Command (number)" : "Command (text)";
		ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1, 1, 1, 0.55f));
		ImGui::TextUnformatted(Caption);
		ImGui::PopStyleColor();
	}
	const float HeaderBottom = ImGui::GetItemRectMax().y + 4.0f;
	ImGui::SetCursorScreenPos({ Origin.x, HeaderBottom + 6.0f });

	// The arguments
	if (Command != nullptr && Command->Type != MacroType::eVoid)
	{
		auto Argument = [&](const char* Label, const char* Id, xr_string& Value)
		{
			const float RowY = ImGui::GetCursorScreenPos().y;
			ImGui::SetCursorScreenPos({ Origin.x, RowY });
			ImGui::AlignTextToFramePadding();
			ImGui::TextUnformatted(Label);
			ImGui::SetCursorScreenPos({ Origin.x + 60.0f, RowY });
			ImGui::SetNextItemWidth(Width - 60.0f);
			if (InputString(Id, Value, Command->IntArg ? ImGuiInputTextFlags_CharsDecimal : 0))
			{
				MarkModified();
			}
			if (ImGui::IsItemActivated())
			{
				PushUndo();
			}
		};
		Argument("Value", "##first", Node.First);
		if (Command->Type == MacroType::eTwoArg)
		{
			Argument("Second", "##second", Node.Second);
		}
	}
	else
	{
		ImGui::Dummy({ Width, 1.0f });
	}

	ImGui::PopID();
	ed::EndNode();

	const ImVec2 Size = ed::GetNodeSize(Node.Id);
	if (Size.x > 1.0f)
	{
		const float HeaderHeight = HeaderBottom - Origin.y + ed::GetStyle().NodePadding.y;
		DrawNodeHeader(ed::GetNodeBackgroundDrawList(Node.Id), ed::GetNodePosition(Node.Id), Size.x, HeaderHeight, HeaderColor);
	}
}

void CUIMacroView::HandleCreate()
{
	if (ed::BeginCreate(ImColor(255, 255, 255), 2.5f))
	{
		ed::PinId Start = 0;
		ed::PinId End = 0;
		if (ed::QueryNewLink(&Start, &End))
		{
			bool StartOutput = false;
			bool EndOutput = false;
			int From = PinNode(Start, StartOutput);
			int To = PinNode(End, EndOutput);
			if (!StartOutput)
			{
				std::swap(From, To);
				std::swap(StartOutput, EndOutput);
			}

			SNode* Source = Find(From);
			if (!StartOutput || EndOutput || From == To || Source == nullptr || std::find(Source->Next.begin(), Source->Next.end(), To) != Source->Next.end())
			{
				ed::RejectNewItem(ColorMissing, 2.0f);
			}
			else if (ed::AcceptNewItem(ImColor(130, 255, 130), 3.0f))
			{
				PushUndo();
				Source->Next.push_back(To);
				MarkModified();
			}
		}

		ed::PinId Pin = 0;
		if (ed::QueryNewNode(&Pin))
		{
			bool IsOutput = false;
			const int From = PinNode(Pin, IsOutput);
			if (IsOutput && ed::AcceptNewItem())
			{
				PendingLink = From;
				CreatePosition = ed::ScreenToCanvas(ImGui::GetMousePos());
				CreateFilter[0] = 0;
				ed::Suspend();
				ImGui::OpenPopup("##macro_create");
				ed::Resume();
			}
		}
	}
	ed::EndCreate();
}

void CUIMacroView::HandleDelete()
{
	if (ed::BeginDelete())
	{
		bool Pushed = false;
		auto Push = [&]()
		{
			if (!Pushed)
			{
				PushUndo();
				Pushed = true;
			}
		};

		ed::LinkId LinkId = 0;
		while (ed::QueryDeletedLink(&LinkId))
		{
			const u64 Value = LinkId.Get() & ~LinkFlag;
			SNode* Source = Find((int)(Value >> 24));
			if (Source != nullptr && ed::AcceptDeletedItem())
			{
				Push();
				std::erase(Source->Next, (int)(Value & 0xFFFFFF));
				MarkModified();
			}
		}

		ed::NodeId NodeId = 0;
		while (ed::QueryDeletedNode(&NodeId))
		{
			const int Id = (int)NodeId.Get();
			if (Id == StartId)
			{
				ed::RejectDeletedItem();
			}
			else if (ed::AcceptDeletedItem())
			{
				Push();
				RemoveNode(Id);
				MarkModified();
			}
		}
	}
	ed::EndDelete();
}

bool CUIMacroView::DrawCommandList(const char* Id, char* Filter, size_t FilterSize, int& Picked)
{
	if (ImGui::IsWindowAppearing())
	{
		ImGui::SetKeyboardFocusHere();
	}
	ImGui::SetNextItemWidth(-1);
	ImGui::InputTextWithHint(Id, ICON_FA_MAGNIFYING_GLASS " Search commands", Filter, FilterSize);

	bool Result = false;
	for (size_t Index = 0; Index < Commands.size(); ++Index)
	{
		const SCommand& Command = Commands[Index];
		if (!PassFilter(Filter, Command.Name))
		{
			continue;
		}

		ImGui::PushStyleColor(ImGuiCol_Text, TypeColor(Command.Type).Value);
		ImGui::TextUnformatted(ICON_FA_TERMINAL);
		ImGui::PopStyleColor();
		ImGui::SameLine();
		if (ImGui::Selectable(Command.Name.c_str()))
		{
			Picked = (int)Index;
			Result = true;
		}
	}
	return Result;
}

void CUIMacroView::DrawContextMenus()
{
	ed::Suspend();

	ed::NodeId NodeId = 0;
	if (ed::ShowNodeContextMenu(&NodeId))
	{
		ContextNode = (int)NodeId.Get();
		ImGui::OpenPopup("##macro_node");
	}
	else if (ed::ShowBackgroundContextMenu())
	{
		CreatePosition = ed::ScreenToCanvas(ImGui::GetMousePos());
		PendingLink = 0;
		CreateFilter[0] = 0;
		ImGui::OpenPopup("##macro_create");
	}

	ImGui::SetNextWindowSizeConstraints({ 260.0f, 0.0f }, { 400.0f, 420.0f });
	if (ImGui::BeginPopup("##macro_create"))
	{
		int Picked = -1;
		if (DrawCommandList("##macro_create_filter", CreateFilter, sizeof(CreateFilter), Picked))
		{
			PushUndo();
			SNode& Node = CreateNode(Picked, CreatePosition);
			const int Id = Node.Id;
			if (SNode* Source = Find(PendingLink))
			{
				Source->Next.push_back(Id);
			}
			Selected = Id;
			MarkModified();
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
	else
	{
		PendingLink = 0;
	}

	if (ImGui::BeginPopup("##macro_node"))
	{
		SNode* Node = Find(ContextNode);
		if (Node != nullptr)
		{
			const SCommand* Command = CommandOf(*Node);
			ImGui::TextDisabled("%s", Command != nullptr ? Command->Name.c_str() : "Start");
			ImGui::Separator();
			if (ImGui::MenuItem(ICON_FA_PLAY " Run from here"))
			{
				Messages.clear();
				xr_set<int> Visited;
				ExecNode(*Node, Visited);
			}
			if (ImGui::MenuItem(ICON_FA_LINK_SLASH " Break links", nullptr, false, !Node->Next.empty()))
			{
				PushUndo();
				Node->Next.clear();
				MarkModified();
			}
			if (Command != nullptr && ImGui::MenuItem(ICON_FA_COPY " Duplicate"))
			{
				PushUndo();
				const SNode Copy = *Node;
				const ImVec2 Position = ed::GetNodePosition(Copy.Id);
				SNode& Created = CreateNode(Copy.Command, { Position.x + 40.0f, Position.y + 40.0f });
				Created.First = Copy.First;
				Created.Second = Copy.Second;
				Selected = Created.Id;
				MarkModified();
			}
			if (Command != nullptr && ImGui::MenuItem(ICON_FA_TRASH " Delete", "Del"))
			{
				PushUndo();
				RemoveNode(ContextNode);
				MarkModified();
			}
		}
		ImGui::EndPopup();
	}

	ed::Resume();
}

void CUIMacroView::DrawGraph()
{
	if (Dock.Begin(GraphWindow))
	{
		if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && !ImGui::GetIO().WantTextInput)
		{
			if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_S))
			{
				SaveMode();
			}
			if (ImGui::IsKeyPressed(ImGuiKey_F5, false))
			{
				Exec();
			}
			if (ImGui::IsKeyPressed(ImGuiKey_F, false))
			{
				NeedFit = true;
			}
		}

		ed::SetCurrentEditor(Editor);
		ApplyGraphStyle();
		Minimap.TakeClick();
		ed::Begin("##macro_graph");

		if (LayoutDelay > 0 && --LayoutDelay == 0)
		{
			Layout();
		}

		for (SNode& Node : Nodes)
		{
			DrawNode(Node);
		}

		for (const SNode& Node : Nodes)
		{
			for (int Next : Node.Next)
			{
				ed::Link(LinkOf(Node.Id, Next), OutputPin(Node.Id), InputPin(Next), ImColor(235, 235, 235), 2.5f);
				Minimap.AddLink(Node.Id, Next);
			}
		}

		HandleCreate();
		HandleDelete();
		DrawContextMenus();

		if (FocusRequest != 0)
		{
			ed::SelectNode(FocusRequest);
			ed::NavigateToSelection(false);
			FocusRequest = 0;
		}

		ed::NodeId SelectedNodes[1] = {};
		const int Count = ed::GetSelectedNodes(SelectedNodes, 1);
		if (Count > 0)
		{
			Selected = (int)SelectedNodes[0].Get();
		}
		else if (ed::IsBackgroundClicked())
		{
			Selected = 0;
		}

		if (NeedFit)
		{
			ed::NavigateToContent(0.0f);
			NeedFit = false;
		}

		ed::End();
		// After End: the minimap is an overlay in screen space
		Minimap.Draw(ImGui::GetItemRectMin(), ImGui::GetItemRectMax());
		ed::SetCurrentEditor(nullptr);
	}
	ImGui::End();
}

void CUIMacroView::DrawOutliner()
{
	if (Dock.Begin(OutlinerWindow))
	{
		if (ImGui::CollapsingHeader(ICON_FA_LIST " Macro", ImGuiTreeNodeFlags_DefaultOpen))
		{
			for (const SNode& Node : Nodes)
			{
				const SCommand* Command = CommandOf(Node);
				ImGui::PushID(Node.Id);
				const xr_string Label = Command != nullptr ? xr_string(ICON_FA_TERMINAL " ") + Command->Name : xr_string(ICON_FA_PLAY " Start");
				if (ImGui::Selectable(Label.c_str(), Selected == Node.Id))
				{
					Selected = Node.Id;
					FocusRequest = Node.Id;
				}
				ImGui::PopID();
			}
		}

		if (ImGui::CollapsingHeader(ICON_FA_TERMINAL " Commands", ImGuiTreeNodeFlags_DefaultOpen))
		{
			int Picked = -1;
			if (DrawCommandList("##macro_outliner_filter", OutlinerFilter, sizeof(OutlinerFilter), Picked))
			{
				// The new command runs after the selected one
				PushUndo();
				SNode* After = Find(Selected != 0 ? Selected : StartId);
				ImVec2 Position = { 0, 0 };
				if (After != nullptr)
				{
					ed::SetCurrentEditor(Editor);
					const ImVec2 AfterPosition = ed::GetNodePosition(After->Id);
					Position = { AfterPosition.x + 320.0f, AfterPosition.y };
					ed::SetCurrentEditor(nullptr);
				}
				const int AfterId = After != nullptr ? After->Id : 0;
				SNode& Node = CreateNode(Picked, Position);
				const int Id = Node.Id;
				if (SNode* Source = Find(AfterId))
				{
					Source->Next.push_back(Id);
				}
				Selected = Id;
				FocusRequest = Id;
				MarkModified();
			}
			ImGui::SetItemTooltip("Click a command to run it after the selected node");
		}
	}
	ImGui::End();
}

void CUIMacroView::FillProperties()
{
	PropertiesNode = Selected;
	PropItemVec Items;
	Properties->ClearProperties();

	SNode* Node = Find(Selected);
	if (Node == nullptr)
	{
		Properties->AssignItems(Items);
		return;
	}

	const SCommand* Command = CommandOf(*Node);
	PHelper().CreateCaption(Items, "Node\\Command", Command != nullptr ? Command->Name.c_str() : "Start");
	if (Command != nullptr)
	{
		PHelper().CreateCaption(Items, "Node\\Id", xr_string::ToString(Command->Command).c_str());

		const char* FirstName = Command->Type == MacroType::eTwoArg ? "Arguments\\First" : "Arguments\\Value";
		if (Command->Type != MacroType::eVoid && Command->IntArg)
		{
			PropFirstInt = Node->First.empty() ? 0 : (u32)atoi(Node->First.c_str());
			PHelper().CreateU32(Items, FirstName, &PropFirstInt, 0, u32(-1))->OnChangeEvent = xr_make_delegate(this, &CUIMacroView::OnPropertyChanged);
		}
		else if (Command->Type != MacroType::eVoid)
		{
			PropFirst = Node->First.c_str();
			PHelper().CreateRText(Items, FirstName, &PropFirst)->OnChangeEvent = xr_make_delegate(this, &CUIMacroView::OnPropertyChanged);
		}
		if (Command->Type == MacroType::eTwoArg)
		{
			PropSecond = Node->Second.c_str();
			PHelper().CreateRText(Items, "Arguments\\Second", &PropSecond)->OnChangeEvent = xr_make_delegate(this, &CUIMacroView::OnPropertyChanged);
		}
	}

	if (Node->Next.empty())
	{
		PHelper().CreateCaption(Items, "Runs next\\-", "Nothing");
	}
	for (size_t Index = 0; Index < Node->Next.size(); ++Index)
	{
		const SNode* Target = Find(Node->Next[Index]);
		const SCommand* TargetCommand = Target != nullptr ? CommandOf(*Target) : nullptr;
		PHelper().CreateCaption(Items, make_string<xr_string>("Runs next\\%u", (u32)Index + 1).c_str(), TargetCommand != nullptr ? TargetCommand->Name.c_str() : "?");
	}

	Properties->AssignItems(Items);
	PropertiesSnapshot = Write().dump().c_str();
}

void CUIMacroView::OnPropertyChanged(PropValue*)
{
	SNode* Node = Find(PropertiesNode);
	const SCommand* Command = Node != nullptr ? CommandOf(*Node) : nullptr;
	if (Command == nullptr)
	{
		return;
	}

	UndoStack.push_back(PropertiesSnapshot);
	if (UndoStack.size() > UndoLimit)
	{
		UndoStack.erase(UndoStack.begin());
	}
	RedoStack.clear();

	Node->First = Command->IntArg ? xr_string::ToString(PropFirstInt) : xr_string(PropFirst.size() > 0 ? PropFirst.c_str() : "");
	Node->Second = PropSecond.size() > 0 ? PropSecond.c_str() : "";
	MarkModified();
	PropertiesSnapshot = Write().dump().c_str();
}

void CUIMacroView::DrawDetails()
{
	if (Dock.Begin(DetailsWindow))
	{
		if (PropertiesNode != Selected)
		{
			FillProperties();
		}
		Properties->Draw();
	}
	ImGui::End();
}

void CUIMacroView::DrawMessages()
{
	if (Dock.Begin(MessagesWindow))
	{
		if (Messages.empty())
		{
			ImGui::TextDisabled("Run the macro to see the result of each command");
		}
		for (size_t Index = 0; Index < Messages.size(); ++Index)
		{
			const SMessage& Message = Messages[Index];
			ImGui::PushID((int)Index);
			ImGui::PushStyleColor(ImGuiCol_Text, Message.IsError ? ColorMissing.Value : ImGui::GetStyleColorVec4(ImGuiCol_Text));
			const xr_string Label = xr_string(Message.IsError ? ICON_FA_CIRCLE_XMARK " " : ICON_FA_CHECK " ") + Message.Text;
			if (ImGui::Selectable(Label.c_str()))
			{
				Selected = Message.Node;
				FocusRequest = Message.Node;
			}
			ImGui::PopStyleColor();
			ImGui::PopID();
		}
	}
	ImGui::End();
}
