#include "stdafx.h"
#include "UIMacroView.h"

#include "../../xrEUI/ImNodeEditor/imnodes.h"

std::array<size_t, 13> CommandsOneArgsInt =
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

std::array<size_t, 8> CommandsOneArgsString =
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

std::array<size_t, 9> CommandsTwoArgs =
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


CUIMacroView::CUIMacroView()
{
	auto ConvertCommandToString = [this](auto& EnumValue)
	{
		xr_string CommandStr = magic_enum::enum_name(EnumValue).data();
		CommandStr = CommandStr.substr(xr_strlen("COMMAND_"));
		
		bool FirstChar = true;
		for (char& Word : CommandStr)
		{
			if (Word == '_')
			{
				Word = ' ';
			}

			if (!FirstChar)
			{
				Word = tolower(Word);
			}

			FirstChar = false;
		}

		MacroNodeTypes NodeInst;
		NodeInst.CommandID = EnumValue;
		NodeInst.RegName = CommandStr;

		bool HasOneArg = std::find(CommandsOneArgsInt.begin(), CommandsOneArgsInt.end(), EnumValue) != CommandsOneArgsInt.end();
		HasOneArg = HasOneArg || std::find(CommandsOneArgsString.begin(), CommandsOneArgsString.end(), EnumValue) != CommandsOneArgsString.end();
		bool HasTwoArg = std::find(CommandsTwoArgs.begin(), CommandsTwoArgs.end(), EnumValue) != CommandsTwoArgs.end();
		NodeInst.Type = HasOneArg ? MacroType::eOneArg : (HasTwoArg ? MacroType::eTwoArg : MacroType::eVoid);

		CommandsList.push_back(std::move(NodeInst));
		(*(u32*)&EnumValue)++;
	};

	for (ECoreCommands StartCommand = (ECoreCommands)(COMMAND_INITIALIZE + 1); StartCommand < COMMAND_MAIN_LAST;)
	{
		ConvertCommandToString(StartCommand);
	}

	for (ELECommand StartCommand = (ELECommand)(COMMAND_EXTFIRST_EXT + 1); StartCommand < COMMAND_LE_END;)
	{
		ConvertCommandToString(StartCommand);
	}

	std::sort(CommandsList.begin(), CommandsList.end(), [](const MacroNodeTypes& Left, const MacroNodeTypes& Right)
	{
		return Left.RegName[0] < Right.RegName[0];
	});
}

CUIMacroView::~CUIMacroView()
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
}

void CUIMacroView::Draw()
{
	if (!IsOpen)
		return;

	if (ImGui::Begin("Macro Node Viewer", &IsOpen))
	{
		auto& io = ImGui::GetIO();
		
		if (ImGui::Button("Run"))
		{
			Exec();
		}
		ImGui::SameLine();

		ImGui::Text("FPS: %.2f (%.2gms)", io.Framerate, io.Framerate ? 1000.0f / io.Framerate : 0.0f);

		ImGui::Separator();
		
		int HoveredNodeID = GetHoveredMode();

		if (ImGui::IsMouseReleased(1))
		{
			ImGui::OpenPopup("##nodesviewportcontextmenumacro");
		}

		if (ImGui::BeginPopup("##nodesviewportcontextmenumacro"))
		{
			if (ImGui::BeginMenu("Create Node"))
			{
				for (const MacroNodeTypes& Node : CommandsList)
				{
					if (ImGui::MenuItem(Node.RegName.data()))
					{
						const ImVec2 click_pos = ImGui::GetMousePosOnOpeningCurrentPopup();

						CNodeMacro* MacroNode = (CNodeMacro*)Nodes.emplace_back(new CNodeMacro(Node.CommandID, Node.RegName));
						MacroNode->SetType(Node.Type);
						MacroNode->SetStartPos(click_pos.x, click_pos.y);
					}
				}

				ImGui::EndMenu();
			}

			if (HoveredNodeID != -1 && ImGui::MenuItem("Remove"))
			{
				Nodes.erase
				(
					std::find_if(Nodes.begin(), Nodes.end(), [HoveredNodeID](INodeUnknown* Val)
					{
						return Val->NodeID == HoveredNodeID;
					})
				);
			}

			ImGui::EndPopup();
		}
		CNodeViewport::Draw();
	}

	ImGui::End();

	CNodeViewport::DrawEnd();
}

void CUIMacroView::Show(bool State)
{
	IsOpen = State;
}

void CUIMacroView::Exec()
{
	CNodeMacro* Node = (CNodeMacro*)Nodes.front();
	ExecMacro(Node);
}

void CUIMacroView::ExecMacro(CNodeMacro* Node)
{
	MacroType Type = Node->GetType();

	if (Type == MacroType::eVoid)
	{
		ExecCommand(Node->MacroCommandID);
	}
	else if (Type == MacroType::eOneArg)
	{
		bool IsInt = std::find(CommandsOneArgsInt.begin(), CommandsOneArgsInt.end(), Node->MacroCommandID) != CommandsOneArgsInt.end();
		if (IsInt)
		{
			u32 Value = Node->FirstValue.empty() ? 0 : atoi(Node->FirstValue.data());
			ExecCommand(Node->MacroCommandID, Value);
		}
		else
		{
			ExecCommand(Node->MacroCommandID, Node->FirstValue);
		}
	}

	for (INodeUnknown* Macro : Node->OutNodes)
	{
		ExecMacro((CNodeMacro*)Macro);
	}
}
