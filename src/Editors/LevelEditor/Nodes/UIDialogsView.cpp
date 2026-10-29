#include "stdafx.h"
#include "UIDialogsView.h"
#include "../Editor/UI_LevelTools.h"
#include "../../../xrEngine/string_table.h"
#include "../../xrEUI/IconsFontAwesome6.h"
#include "NodesCommon.h"
#include "../Editor/XVisor/XVisor.h"
#include "UINodesEditorForm.h"

static const char* GraphWindow = "Dialog Graph###LogicGraph";
static const char* OutlinerWindow = "Outliner###LogicOutliner";
static const char* DetailsWindow = "Details###LogicDetails";
static const char* MessagesWindow = "Message Log###LogicMessages";

static constexpr size_t UndoLimit = 128;

DialogEditor* DialogEditor::Instance = nullptr;

namespace
{
ImColor NodeHeaderColor(CDialogNode* Node)
{
	if (Node->IsFinal)
	{
		return ImColor(110, 200, 120);
	}
	if (Node->IsBranch)
	{
		return ImColor(150, 110, 210);
	}
	if (Node->HasInfo.size() || Node->DontHasInfo.size() || Node->Precondition.size())
	{
		return ImColor(200, 160, 90);
	}
	return ImColor(90, 130, 210);
}

ImColor PinColorOut = ImColor(150, 230, 120);
ImColor LinkColor = PinColorOut;

void DrawPinArrow(bool Filled)
{
	ImGui::Dummy({ NodesUI::PinSize, NodesUI::PinSize });
	NodesUI::DrawExecPin(ImGui::GetWindowDrawList(), ImGui::GetItemRectMin(), NodesUI::PinSize, NodesUI::ColorPin, Filled);
}
} // namespace

static bool FindDialogTranslation(const shared_str& Id, xr_string& OutText)
{
	if (g_pStringTable == nullptr || !g_pStringTable->HasString(Id))
	{
		return false;
	}

	OutText = Platform::ANSI_TO_UTF8(g_pStringTable->translate(Id).c_str());
	return true;
}

static void RequestDialogLocalization(CDialogNode* Node)
{
	if (DialogEditor::Instance != nullptr)
	{
		DialogEditor::Instance->OpenLocalization(Node);
	}
}

static bool ContainsNoCase(const char* Str, const char* Sub)
{
	if (Sub == nullptr || Sub[0] == '\0')
	{
		return true;
	}

	for (; *Str != '\0'; ++Str)
	{
		const char* A = Str;
		const char* B = Sub;
		while (*A != '\0' && *B != '\0' && tolower((u8)*A) == tolower((u8)*B))
		{
			++A;
			++B;
		}

		if (*B == '\0')
		{
			return true;
		}
	}

	return false;
}

DialogEditor::DialogEditor()
{
	Instance = this;
	ed::Config Config;
	Config.SettingsFile = nullptr;
	Editor = ed::CreateEditor(&Config);
	SelectedNode = nullptr;

	CDialogNode::FindTranslation = &FindDialogTranslation;
	CDialogNode::RequestLocalization = &RequestDialogLocalization;

	Properties = new UIPropertiesForm();
	Properties->SetModifiedEvent(TOnCloseEvent(this, &DialogEditor::OnPropertiesModified));

	// The same list the Content Browser uses to open dialogs
	if (pSettings->line_exist("dialogs", "files"))
	{
		xr_string Files = pSettings->r_string("dialogs", "files");
		Files.RemoveWhitespaces();
		GameDialogs = Files.Split(',');
	}
}

const char* DialogEditor::GetName() const
{
	return "Dialogs";
}

const char* DialogEditor::GetIcon() const
{
	return ICON_FA_COMMENT_DOTS;
}

void DialogEditor::GetWindows(xr_vector<SNodesWindow>& Windows) const
{
	Windows.push_back({ GraphWindow, ENodesDock::Graph });
	Windows.push_back({ OutlinerWindow, ENodesDock::Outliner });
	Windows.push_back({ DetailsWindow, ENodesDock::Details });
	Windows.push_back({ MessagesWindow, ENodesDock::Bottom });
}

bool DialogEditor::IsModeModified() const
{
	return Modified;
}

bool DialogEditor::SaveMode()
{
	if (DialogFileName.empty())
	{
		return false;
	}
	SaveDialog();
	return true;
}

bool DialogEditor::ReloadMode()
{
	if (DialogFileName.empty())
	{
		return false;
	}
	if (Modified && ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "Discard the changes of %s?", DialogFileName.c_str()) != mrYes)
	{
		return false;
	}

	const shared_str Dialog = LastOpenDialog;
	OpenFileInstance(xr_path(DialogFileName));
	for (const DialogPair& Pair : Dialogs)
	{
		if (Pair.first == Dialog)
		{
			OpenDialog(Pair.first, Pair.second);
			LastOpenDialog = Pair.first;
			break;
		}
	}
	return true;
}

bool DialogEditor::UndoMode(bool Redo)
{
	if (ChangePending)
	{
		CommitChange();
	}

	xr_vector<xr_string>& From = Redo ? RedoStack : UndoStack;
	xr_vector<xr_string>& To = Redo ? UndoStack : RedoStack;
	if (From.empty() || File.GetRoot() == nullptr)
	{
		return false;
	}

	To.push_back(LastState);
	LastState = From.back();
	From.pop_back();
	RestoreState(LastState);
	Modified = LastState != SavedState;
	return true;
}

void DialogEditor::MarkChanged()
{
	Modified = true;
	ChangePending = true;
}

xr_string DialogEditor::CaptureState()
{
	XML_NODE* Root = File.GetRoot();
	if (Root == nullptr)
	{
		return xr_string();
	}
	tinyxml2::XMLPrinter Printer;
	Root->GetDocument()->Print(&Printer);
	return Printer.CStr();
}

void DialogEditor::CommitChange()
{
	ChangePending = false;
	const xr_string State = CaptureState();
	if (State == LastState)
	{
		return;
	}
	UndoStack.push_back(LastState);
	if (UndoStack.size() > UndoLimit)
	{
		UndoStack.erase(UndoStack.begin());
	}
	RedoStack.clear();
	LastState = State;
	Modified = State != SavedState;
}

void DialogEditor::RestoreState(const xr_string& State)
{
	XML_NODE* Root = File.GetRoot();
	tinyxml2::XMLDocument Source;
	if (Root == nullptr || Source.Parse(State.c_str()) != tinyxml2::XML_SUCCESS || Source.FirstChildElement() == nullptr)
	{
		return;
	}

	// The graph keeps its places and selection by the phrase names
	ed::SetCurrentEditor(Editor);
	xr_map<xr_string, ImVec2> Positions;
	for (CDialogNode* Node : Nodes)
	{
		auto Iter = NodeIds.find(Node);
		if (Iter != NodeIds.end())
		{
			Positions[Node->NodeName.c_str()] = ed::GetNodePosition(Iter->second);
		}
	}
	const xr_string Selected = SelectedNode != nullptr ? SelectedNode->NodeName.c_str() : "";
	const shared_str Dialog = LastOpenDialog;

	// The root element stays: the file keeps a pointer to it
	Root->DeleteChildren();
	for (const XML_NODE* Child = Source.FirstChildElement()->FirstChild(); Child != nullptr; Child = Child->NextSibling())
	{
		Root->InsertEndChild(Child->DeepClone(Root->GetDocument()));
	}

	RebuildDialogList();
	for (const DialogPair& Pair : Dialogs)
	{
		if (Pair.first == Dialog)
		{
			OpenDialog(Pair.first, Pair.second);
			break;
		}
	}
	if (LastOpenDialog != Dialog)
	{
		// The dialog is gone
		for (auto Node : Nodes)
		{
			xr_delete(Node);
		}
		Nodes.clear();
		Links.clear();
		LastOpenDialog = nullptr;
		SelectedNode = nullptr;
		Properties->ClearProperties();
		return;
	}

	bool AllPlaced = true;
	for (CDialogNode* Node : Nodes)
	{
		auto Iter = Positions.find(Node->NodeName.c_str());
		if (Iter != Positions.end())
		{
			ed::SetNodePosition(NodeIds[Node], Iter->second);
		}
		else
		{
			AllPlaced = false;
		}
		if (Node->NodeName == Selected.c_str())
		{
			PendingSelect = NodeIds[Node];
			SelectNodeEvent(Node);
		}
	}
	NeedLayout = !AllPlaced && Positions.empty();
	FocusFirstNode = false;
}

xr_string DialogEditor::GetModePath() const
{
	if (DialogFileName.empty())
	{
		return xr_string();
	}
	string_path Path = {};
	FS.update_path(Path, CONFIG_PATH, ("gameplay\\" + DialogFileName).c_str());
	return Path;
}

void DialogEditor::SerializeMode(nlohmann::json& Data, bool IsSaving)
{
	if (IsSaving)
	{
		Data["file"] = DialogFileName.c_str();
		Data["dialog"] = LastOpenDialog.size() > 0 ? LastOpenDialog.c_str() : "";
		return;
	}

	auto FileIter = Data.find("file");
	if (FileIter == Data.end() || !FileIter->is_string() || FileIter->get_ref<const nlohmann::json::string_t&>().empty())
	{
		return;
	}

	const xr_string FileName = FileIter->get_ref<const nlohmann::json::string_t&>().c_str();
	string_path Path = {};
	FS.update_path(Path, CONFIG_PATH, ("gameplay\\" + FileName).c_str());
	if (!FS.exist(Path))
	{
		return;
	}

	OpenFileInstance(xr_path(FileName));
	auto DialogIter = Data.find("dialog");
	if (DialogIter != Data.end() && DialogIter->is_string())
	{
		const shared_str Dialog = DialogIter->get_ref<const nlohmann::json::string_t&>().c_str();
		for (const DialogPair& Pair : Dialogs)
		{
			if (Pair.first == Dialog)
			{
				OpenDialog(Pair.first, Pair.second);
				LastOpenDialog = Pair.first;
				break;
			}
		}
	}
}

void DialogEditor::DrawModeTopBar(float ButtonSize)
{
	auto Hint = [](const char* Text)
	{
		if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("%s", Text);
		}
	};

	ImGui::BeginDisabled(LastOpenDialog.size() == 0);
	if (XRay::ImGui::ToolbarButton("##dialog_layout", ICON_FA_SITEMAP " Auto Layout", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersLeft))
	{
		NeedLayout = true;
	}
	Hint("Arrange the phrases by their links");

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##dialog_fit", ICON_FA_EXPAND " Fit", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersRight))
	{
		ed::SetCurrentEditor(Editor);
		ed::NavigateToContent();
	}
	Hint("Show all phrases");
	ImGui::EndDisabled();

	ImGui::SameLine();
	ImGui::BeginDisabled(Dialogs.empty() && DialogFileName.empty());
	if (XRay::ImGui::ToolbarButton("##dialog_new", ICON_FA_PLUS " New Dialog", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersAll))
	{
		InputBox.Mode = InputBoxMode::DialogName;
		InputBox.Buffer[0] = '\0';
		InputBox.IsOpen = true;
	}
	Hint("Add a dialog to this file");
	ImGui::EndDisabled();
}

DialogEditor::~DialogEditor()
{
	CDialogNode::FindTranslation = nullptr;
	CDialogNode::RequestLocalization = nullptr;

	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	Nodes.clear();

	if (Editor != nullptr)
	{
		ed::DestroyEditor(Editor);
		Editor = nullptr;
	}

	if (Instance == this)
	{
		Instance = nullptr;
	}
}

void DialogEditor_DrawInputBox(DialogEditor::InputBoxState& Box)
{
	if (!Box.IsOpen)
	{
		return;
	}

	ImVec2 Center = ImGui::GetMainViewport()->GetCenter();
	ImGui::SetNextWindowPos(Center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

	if (ImGui::BeginPopupModal("DialogInputBox", NULL, ImGuiWindowFlags_AlwaysAutoResize))
	{
		const char* Title = "Phrase ID";
		if (Box.Mode == DialogEditor::InputBoxMode::BranchName)
		{
			Title = "Branch phrase ID";
		}
		else if (Box.Mode == DialogEditor::InputBoxMode::DialogName)
		{
			Title = "Dialog ID";
		}

		ImGui::TextUnformatted(Title);
		if (ImGui::IsWindowAppearing())
		{
			ImGui::SetKeyboardFocusHere();
		}

		ImGui::SetNextItemWidth(260.f);
		const bool IsEnter = ImGui::InputText("##input", Box.Buffer, IM_ARRAYSIZE(Box.Buffer), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_CharsNoBlank);

		ImGui::BeginDisabled(Box.Buffer[0] == '\0');
		if (ImGui::Button("OK", ImVec2(127, 0)) || (IsEnter && Box.Buffer[0] != '\0'))
		{
			Box.IsOpen = false;
			Box.HasResult = true;
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndDisabled();

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(127, 0)) || ImGui::IsKeyPressed(ImGuiKey_Escape))
		{
			Box.Buffer[0] = '\0';
			Box.IsOpen = false;
			ImGui::CloseCurrentPopup();
		}

		ImGui::EndPopup();
	}
}

XML_NODE* DialogEditor::CurrentDialogNode()
{
	auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
							 { return LastOpenDialog == Pair.first; });

	return Iter != Dialogs.end() ? Iter->second : nullptr;
}

void DialogEditor::DrawOutliner()
{
	if (Dock.Begin(OutlinerWindow))
	{
		ImGui::TextDisabled("%s", DialogFileName.empty() ? "No file" : DialogFileName.c_str());

		const bool ShowFiles = DialogFileName.empty();
		ImGui::SetNextItemWidth(-1);
		ImGui::InputTextWithHint("##DialogFilter", ShowFiles ? ICON_FA_MAGNIFYING_GLASS " Filter files" : ICON_FA_MAGNIFYING_GLASS " Filter dialogs", DialogFilter, sizeof(DialogFilter));

		if (ShowFiles)
		{
			// No file is opened: offer the game dialogs files
			if (ImGui::BeginChild("##DialogFiles"))
			{
				int Shown = 0;
				for (const xr_string& Name : GameDialogs)
				{
					if (Name.empty() || !ContainsNoCase(Name.c_str(), DialogFilter))
					{
						continue;
					}

					++Shown;
					const xr_string Label = xr_string(ICON_FA_FILE " ") + Name;
					if (ImGui::Selectable(Label.c_str()))
					{
						OpenFileInstance(xr_string(Name + ".xml"));
						DialogFilter[0] = '\0';
					}
				}

				if (Shown == 0)
				{
					ImGui::TextDisabled(GameDialogs.empty() ? "No dialogs files in [dialogs] files" : "No files match the filter");
				}
			}
			ImGui::EndChild();
		}
		else if (ImGui::BeginChild("##ItemsInFile"))
		{
			int Shown = 0;
			for (const auto& [ID, Node] : Dialogs)
			{
				if (!ContainsNoCase(ID.c_str(), DialogFilter))
				{
					continue;
				}

				++Shown;
				const bool IsCurrent = LastOpenDialog == ID;
				const xr_string Label = xr_string(ICON_FA_COMMENT_DOTS " ") + ID.c_str();
				if (ImGui::Selectable(Label.c_str(), IsCurrent))
				{
					OpenDialog(ID, Node);
					LastOpenDialog = ID;
				}

				if (IsCurrent && ImGui::IsWindowAppearing())
				{
					ImGui::SetScrollHereY();
				}
			}

			if (Shown == 0)
			{
				ImGui::TextDisabled(Dialogs.empty() ? "The file has no dialogs" : "No dialogs match the filter");
			}
		}
		if (!ShowFiles)
		{
			ImGui::EndChild();
		}
	}
	ImGui::End();
}

void DialogEditor::DrawDetails()
{
	if (Dock.Begin(DetailsWindow))
	{
		if (RefreshProperties)
		{
			RefreshProperties = false;
			SelectNodeEvent(SelectedNode);
		}
		Properties->Draw();
	}
	ImGui::End();
}

void DialogEditor::SelectPhrase(CDialogNode* Node)
{
	auto Iter = NodeIds.find(Node);
	if (Iter != NodeIds.end())
	{
		PendingSelect = Iter->second;
		PendingFocus = true;
		SelectNodeEvent(Node);
	}
}

void DialogEditor::DrawMessages()
{
	if (Dock.Begin(MessagesWindow))
	{
		int Count = 0;
		for (CDialogNode* Node : Nodes)
		{
			const bool HasNext = std::any_of(Links.begin(), Links.end(), [Node](const NodeLink& Link) { return Link.Source == Node; });
			const bool HasPrevious = Node == Nodes.front() || std::any_of(Links.begin(), Links.end(), [Node](const NodeLink& Link) { return Link.Target == Node; });

			xr_string Text;
			bool IsError = false;
			if (!Node->IsBranch && (Node->Text.size() == 0 || g_pStringTable == nullptr || !g_pStringTable->HasString(Node->Text)))
			{
				Text = "the text is not localized";
			}
			else if (!HasPrevious)
			{
				Text = "nothing links to the phrase";
				IsError = true;
			}
			else if (Node->IsBranch && !HasNext)
			{
				Text = "the branch has no next phrase";
				IsError = true;
			}
			else
			{
				continue;
			}

			++Count;
			ImGui::PushID(Node);
			const xr_string Label = make_string<xr_string>("%s Phrase %s: %s", IsError ? ICON_FA_CIRCLE_XMARK : ICON_FA_TRIANGLE_EXCLAMATION, Node->NodeName.c_str(), Text.c_str());
			ImGui::PushStyleColor(ImGuiCol_Text, (IsError ? NodesUI::ColorMissing : NodesUI::ColorWarning).Value);
			if (ImGui::Selectable(Label.c_str(), Node == SelectedNode))
			{
				SelectPhrase(Node);
			}
			ImGui::PopStyleColor();
			ImGui::PopID();
		}

		if (Count == 0)
		{
			ImGui::TextDisabled(LastOpenDialog.size() == 0 ? "Select a dialog" : ICON_FA_CHECK " No problems");
		}
	}
	ImGui::End();
}

void DialogEditor::DrawCanvasToolbar()
{
	if (LastOpenDialog.size() == 0)
	{
		ImGui::AlignTextToFramePadding();
		ImGui::TextDisabled("Select a dialog");
		return;
	}

	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted(LastOpenDialog.c_str());
	ImGui::SameLine();
	ImGui::TextDisabled("%u phrases", (u32)Nodes.size());


	// Phrases whose text is missing in the string table
	xr_vector<CDialogNode*> Missing;
	for (CDialogNode* Node : Nodes)
	{
		if (!Node->IsBranch && (Node->Text.size() == 0 || g_pStringTable == nullptr || !g_pStringTable->HasString(Node->Text)))
		{
			Missing.push_back(Node);
		}
	}

	if (!Missing.empty())
	{
		ImGui::SameLine();
		ImGui::PushStyleColor(ImGuiCol_Text, { 0.95f, 0.72f, 0.3f, 1.f });
		const bool IsPressed = ImGui::Button(make_string<xr_string>(ICON_FA_TRIANGLE_EXCLAMATION " %u not localized", (u32)Missing.size()).c_str());
		ImGui::PopStyleColor();
		ImGui::SetItemTooltip("Go to the next phrase without a text");

		if (IsPressed)
		{
			auto Current = std::find(Missing.begin(), Missing.end(), SelectedNode);
			CDialogNode* Next = (Current == Missing.end() || std::next(Current) == Missing.end()) ? Missing.front() : *std::next(Current);
			PendingSelect = NodeIds[Next];
			PendingFocus = true;
		}
	}
}

void DialogEditor::DrawContextMenu()
{
	if (!ImGui::BeginPopup("##dialogscontextmenu"))
	{
		return;
	}

	if (SelectedNode != nullptr)
	{
		ImGui::TextDisabled("Phrase %s", SelectedNode->NodeName.c_str());

		const bool IsLocalized = SelectedNode->Text.size() > 0 && g_pStringTable != nullptr && g_pStringTable->HasString(SelectedNode->Text);
		if (!SelectedNode->IsBranch && ImGui::MenuItem(ICON_FA_LANGUAGE " Localize Text...", nullptr, false, !IsLocalized))
		{
			OpenLocalization(SelectedNode);
		}

		if (ImGui::MenuItem(ICON_FA_TRASH " Delete Phrase", "Del"))
		{
			DeleteNode(SelectedNode);
		}

		ImGui::Separator();
	}

	if (ImGui::MenuItem(ICON_FA_PLUS " Create Phrase"))
	{
		InputBox.Mode = InputBoxMode::NodeName;
		InputBox.Buffer[0] = '\0';

		if (!Phrases.empty())
		{
			int NodeID = atoi(*Phrases.back().first) + 1;
			xr_string Suggestion = xr_string::ToString(NodeID);
			strcpy_s(InputBox.Buffer, Suggestion.c_str());
		}
		InputBox.IsOpen = true;
	}

	if (ImGui::MenuItem(ICON_FA_CODE_BRANCH " Create Branch Phrase"))
	{
		InputBox.Mode = InputBoxMode::BranchName;
		InputBox.Buffer[0] = '\0';
		InputBox.IsOpen = true;
	}

	ImGui::Separator();

	if (ImGui::MenuItem(ICON_FA_SITEMAP " Auto Layout"))
	{
		NeedLayout = true;
	}

	if (ImGui::MenuItem(ICON_FA_LOCATION_CROSSHAIRS " Fit to Content"))
	{
		ed::NavigateToContent();
	}

	ImGui::EndPopup();
}

void DialogEditor::HandleShortcuts()
{
	if (!ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) || ImGui::GetIO().WantTextInput)
	{
		return;
	}

	if (ImGui::GetIO().KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_S, false))
	{
		SaveDialog();
	}
}

void DialogEditor::OpenLocalization(CDialogNode* Node)
{
	if (Node == nullptr)
	{
		return;
	}

	// The game convention: <dialog>_<phrase>, stored in st_<dialogs file>
	xr_string StringId = Node->Text.size() > 0 ? Node->Text.c_str() : make_string<xr_string>("%s_%s", LastOpenDialog.c_str(), Node->NodeName.c_str());
	xr_string FileName = DialogFileName.empty() ? xr_string("st_dialogs_editor.xml") : "st_" + DialogFileName;

	const xr_string NodeName = Node->NodeName.c_str();
	Localization.Open(StringId.c_str(), ("Phrase " + NodeName).c_str(), FileName.c_str(), [this, NodeName](const char* Id)
	{
		OnPhraseLocalized(NodeName.c_str(), Id);
	});
}

void DialogEditor::OnPhraseLocalized(const char* NodeName, const char* StringId)
{
	for (CDialogNode* Node : Nodes)
	{
		if (Node->NodeName == NodeName)
		{
			Node->SetText(StringId);
			MarkChanged();
			if (Node == SelectedNode)
			{
				SelectNodeEvent(Node);
			}
			break;
		}
	}
}

void DialogEditor::DrawMode()
{
	Dock.BeginFrame();
	DrawGraph();
	DrawOutliner();
	DrawDetails();
	DrawMessages();
	Localization.Draw();

	// The changes of a frame are one step of the history
	if (ChangePending)
	{
		CommitChange();
	}
}

void DialogEditor::DrawGraph()
{
	if (Dock.Begin(GraphWindow))
	{
		HandleShortcuts();

		ImGui::BeginGroup();
		DrawCanvasToolbar();

		ed::SetCurrentEditor(Editor);
		NodesUI::ApplyGraphStyle();

		if (NeedLayout)
		{
			// Once more in the next frame when the sizes of the nodes are not known yet
			NeedLayout = !LayoutNodes();
			FocusFirstNode = true;
		}

		Minimap.TakeClick();

		ed::Begin("Canvas");
		{
			if (PendingSelect)
			{
				ed::ClearSelection();
				ed::SelectNode(PendingSelect);
				PendingSelect = ed::NodeId(0);
			}

			if (FocusFirstNode && !Nodes.empty())
			{
				ed::NodeId FirstNode = NodeIds[Nodes[0]];
				ed::SelectNode(FirstNode);

				if (ed::GetNodeSize(FirstNode).x > 1.0f)
				{
					ed::NavigateToSelection();
					FocusFirstNode = false;
				}
			}

			UpdateSelection();

			ConnectedPins.clear();
			for (const auto& Link : Links)
			{
				ConnectedPins.insert(Link.Start);
				ConnectedPins.insert(Link.End);
			}

			for (auto* Node : Nodes)
			{
				DrawNode(Node);
				Minimap.AddNode(NodeIds[Node], NodeHeaderColor(Node));
			}

			for (const auto& Link : Links)
			{
				ed::Link(Link.Id, Link.Start, Link.End, LinkColor, 2.0f);
				if (NodeIds.contains(Link.Source) && NodeIds.contains(Link.Target))
				{
					Minimap.AddLink(NodeIds[Link.Source], NodeIds[Link.Target]);
				}
			}

			HandleLinkCreation();
			HandleDeletion();

			if (PendingFocus && ed::GetSelectedObjectCount() > 0)
			{
				ed::NavigateToSelection(true);
				PendingFocus = false;
			}

			// Inside Begin/End the mouse is in canvas space
			const ImVec2 CanvasMouse = ImGui::GetMousePos();

			ed::Suspend();
			ed::NodeId ContextNode = 0;
			if (ed::ShowNodeContextMenu(&ContextNode))
			{
				ed::ClearSelection();
				ed::SelectNode(ContextNode);
				SelectNodeEvent(NodeFromId(ContextNode));
				ContextMouseCanvas = CanvasMouse;
				ImGui::OpenPopup("##dialogscontextmenu");
			}
			else if (ed::ShowBackgroundContextMenu())
			{
				ContextMouseCanvas = CanvasMouse;
				ImGui::OpenPopup("##dialogscontextmenu");
			}

			if (LastOpenDialog.size() > 0)
			{
				DrawContextMenu();
			}
			ed::Resume();
		}
		ed::End();

		// After End: the minimap is an overlay in screen space
		Minimap.Draw(ImGui::GetItemRectMin(), ImGui::GetItemRectMax());

		ImGui::EndGroup();

		if (LastOpenDialog.size() == 0)
		{
			const char* Hint = Dialogs.empty() ? ICON_FA_COMMENT_DOTS "  Select a dialogs file in the Outliner" : ICON_FA_COMMENT_DOTS "  Select a dialog in the Outliner";
			const ImVec2 Min = ImGui::GetItemRectMin();
			const ImVec2 Max = ImGui::GetItemRectMax();
			const ImVec2 Size = ImGui::CalcTextSize(Hint);
			ImGui::GetWindowDrawList()->AddText({ (Min.x + Max.x - Size.x) * 0.5f, Min.y + (Max.y - Min.y) * 0.45f }, ImGui::GetColorU32(ImGuiCol_TextDisabled), Hint);
		}

		if (InputBox.IsOpen)
		{
			ImGui::OpenPopup("DialogInputBox");
			DialogEditor_DrawInputBox(InputBox);
		}
		else if (InputBox.HasResult)
		{
			InputBox.HasResult = false;

			XML_NODE* DialogNode = CurrentDialogNode();

			if (InputBox.Mode == InputBoxMode::NodeName || InputBox.Mode == InputBoxMode::BranchName)
			{
				if (DialogNode != nullptr)
				{
					if (XML_NODE* RootNode = File.NavigateToNode(DialogNode, "phrase_list"))
					{
						XML_NODE* NewNode = RootNode->ToElement()->InsertNewChildElement("phrase");
						NewNode->ToElement()->SetAttribute("id", InputBox.Buffer);

						CDialogNode* MacroNode = Nodes.emplace_back(new CDialogNode(InputBox.Buffer));
						MacroNode->ParentNode = NewNode;

						if (InputBox.Mode == InputBoxMode::BranchName)
						{
							MacroNode->IsBranch = true;
							NewNode->ToElement()->SetAttribute("branch", "1");
						}

						BuildIdMaps();
						ed::SetCurrentEditor(Editor);
						ed::SetNodePosition(NodeIds[MacroNode], ContextMouseCanvas);
						PendingSelect = NodeIds[MacroNode];

						SelectNodeEvent(MacroNode);
						MarkChanged();
					}
				}
			}
			else
			{
				XML_NODE* RootNode = nullptr;

				if (DialogNode != nullptr)
				{
					RootNode = DialogNode->Parent();
				}
				else if (!Dialogs.empty())
				{
					RootNode = Dialogs.front().second->Parent();
				}
				else
				{
					RootNode = File.GetRoot();
				}

				if (RootNode != nullptr)
				{
					NewDialog(RootNode);
				}
			}
		}
	}
	ImGui::End();
}

void DialogEditor::NewDialog(XML_NODE* RootDialogNode)
{
	XML_NODE* NewDialog = RootDialogNode->ToElement()->InsertNewChildElement("dialog");
	NewDialog->ToElement()->SetAttribute("id", InputBox.Buffer);
	NewDialog->ToElement()->InsertNewChildElement("phrase_list");

	Dialogs.emplace_back(InputBox.Buffer, NewDialog);
	OpenDialog(InputBox.Buffer, NewDialog);
	LastOpenDialog = InputBox.Buffer;
	MarkChanged();
}

void DialogEditor::SaveDialog()
{
	if (DialogFileName.empty())
	{
		return;
	}

	File.Save();
	SavedState = CaptureState();
	Modified = false;
	Msg("* Dialog Editor: %s saved", DialogFileName.c_str());
}

void DialogEditor::OpenDialog(const shared_str& Str, XML_NODE* Node)
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	Nodes.clear();
	Links.clear();
	Phrases.clear();
	SelectedNode = nullptr;

	// The dialog conditions belong to the previous dialog
	HasInfo = nullptr;
	DontHasInfo = nullptr;
	Precondition = nullptr;
	NodeHasInfo = nullptr;
	NodeDontHasInfo = nullptr;
	NodePrecondition = nullptr;

	XML_NODE* RootNode = File.NavigateToNode(Node, "phrase_list");
	if (RootNode == nullptr)
	{
		return;
	}

	LastOpenDialog = Str;

	XML_NODE* PhraseNode = RootNode->FirstChildElement();

	xr_map<CDialogNode*, xr_vector<shared_str>> NodeGraph;

	auto MakeListStringFromNode = [](shared_str& Value, const shared_str& Text)
	{
		if (Value.size() > 0)
		{
			Value = make_string<shared_str>("%s, %s", *Value, *Text);
		}
		else
		{
			Value = Text;
		}
	};

	while (PhraseNode != nullptr)
	{
		shared_str NodeID = PhraseNode->ToElement()->Attribute("id");
		if (NodeID.size() == 0)
		{
			PhraseNode = PhraseNode->NextSibling();
			continue;
		}

		CDialogNode* MacroNode = Nodes.emplace_back(new CDialogNode(*NodeID));
		MacroNode->ParentNode = PhraseNode;

		if (const char* BranchAttr = PhraseNode->ToElement()->Attribute("branch"))
		{
			MacroNode->IsBranch = (xr_string(BranchAttr) == "1");
		}

		XML_NODE* ChildNode = PhraseNode->FirstChildElement();

		while (ChildNode != nullptr)
		{
			xr_string NodeName = ChildNode->Value();
			shared_str NodeText = ChildNode->ToElement() ? ChildNode->ToElement()->GetText() : nullptr;

			if (NodeText.size() == 0)
			{
				ChildNode = ChildNode->NextSibling();
				continue;
			}

			if (NodeName == "text")
			{
				MacroNode->Text = NodeText;
				MacroNode->TextNode = ChildNode;
			}
			else if (NodeName == "dont_has_info")
			{
				MakeListStringFromNode(MacroNode->DontHasInfo, NodeText);
				MacroNode->DontHasInfoNode.push_back(ChildNode);
			}
			else if (NodeName == "has_info")
			{
				MakeListStringFromNode(MacroNode->HasInfo, NodeText);
				MacroNode->HasInfoNode.push_back(ChildNode);
			}
			else if (NodeName == "is_final")
			{
				MacroNode->IsFinal = NodeText == "1";
				MacroNode->IsFinalNode = ChildNode;
			}
			else if (NodeName == "give_info")
			{
				MakeListStringFromNode(MacroNode->GiveInfo, NodeText);
				MacroNode->GiveInfoNode.push_back(ChildNode);
			}
			else if (NodeName == "precondition")
			{
				MacroNode->Precondition = NodeText;
				MacroNode->PreconditionNode = ChildNode;
			}
			else if (NodeName == "action")
			{
				MacroNode->Action = NodeText;
				MacroNode->ActionNode = ChildNode;
			}
			else if (NodeName == "next")
			{
				NodeGraph[MacroNode].push_back(NodeText);
				MacroNode->NextNodes.push_back(ChildNode);
			}

			Phrases.emplace_back(NodeID, MacroNode->Text);
			ChildNode = ChildNode->NextSibling();
		}
		PhraseNode = PhraseNode->NextSibling();
	}

	RootNode = RootNode->Parent()->FirstChildElement();
	while (RootNode != nullptr)
	{
		if (RootNode->ToElement() != nullptr)
		{
			xr_string UpperNodeName = RootNode->Value();

			if (UpperNodeName == "has_info")
			{
				shared_str NodeText = RootNode->ToElement()->GetText();
				MakeListStringFromNode(HasInfo, NodeText);
				NodeHasInfo = RootNode;
			}
			else if (UpperNodeName == "dont_has_info")
			{
				shared_str NodeText = RootNode->ToElement()->GetText();
				MakeListStringFromNode(DontHasInfo, NodeText);
				NodeDontHasInfo = RootNode;
			}
			else if (UpperNodeName == "precondition")
			{
				shared_str NodeText = RootNode->ToElement()->GetText();
				Precondition = NodeText;
				NodePrecondition = RootNode;
			}
		}

		RootNode = RootNode->NextSiblingElement();
	}

	BuildIdMaps();

	for (auto& [Node, ContactsList] : NodeGraph)
	{
		int Slot = 0;
		for (const shared_str& NodeName : ContactsList)
		{
			for (CDialogNode* TryNode : Nodes)
			{
				if (TryNode->NodeName == *NodeName)
				{
					NodeLink Link;
					Link.Id = ed::LinkId(NextLinkId++);
					Link.Start = Node->IsBranch ? OutputPins[Node][Slot] : OutputPins[Node][0];
					Link.End = InputPins[TryNode];
					Link.Source = Node;
					Link.Target = TryNode;
					Link.NextNode = nullptr;

					if (Node->IsBranch && Slot < (int)Node->NextNodes.size())
					{
						Link.NextNode = Node->NextNodes[Slot];
					}
					else
					{
						for (XML_NODE* Child = Node->ParentNode->FirstChildElement(); Child != nullptr; Child = Child->NextSiblingElement())
						{
							const char* ChildText = Child->ToElement()->GetText();
							if (ChildText && 0 == xr_strcmp(Child->Value(), "next") && 0 == xr_strcmp(ChildText, TryNode->NodeName.c_str()))
							{
								Link.NextNode = Child;
								break;
							}
						}
					}

					Links.push_back(Link);
					break;
				}
			}
			++Slot;
		}
	}

	std::sort(Phrases.begin(), Phrases.end(), [](auto L, auto R)
			  {
		xr_string NameA = *L.first;
		xr_string NameB = *R.first;
		return NameA < NameB; });

	NeedLayout = true;
	FocusFirstNode = true;
	SelectNodeEvent(nullptr);
}

void DialogEditor::OnPropertiesModified()
{
	MarkChanged();
}

void DialogEditor::SelectNodeEvent(CDialogNode* Node)
{
	PropItemVec items;
	Properties->ClearProperties();

	if (Node == nullptr)
	{
		PHelper().CreateRText(items, "Preconditions\\Has Info", &HasInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeHasInfo);
		PHelper().CreateRText(items, "Preconditions\\Don't Has Info", &DontHasInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeDontHasInfo);
		{
			RTextValue* Value = PHelper().CreateRText(items, "Preconditions\\Lua Precondition", &Precondition);
			Value->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangePrecondition);
			Value->OnHintEvent = xr_make_delegate(this, &DialogEditor::HintScriptFunction);
		}

		for (const auto& [ID, String] : Phrases)
		{
			xr_string Name = "Phrases\\";
			Name += *ID;
			PHelper().CreateCaption(items, Name.c_str(), *String);
		}

		Properties->AssignItems(items);
		SelectedNode = nullptr;
		return;
	}

	SelectedNode = Node;

	PHelper().CreateRText(items, "Preconditions\\Has Info", &SelectedNode->HasInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeHasInfo);
	PHelper().CreateRText(items, "Preconditions\\Don't Has Info", &SelectedNode->DontHasInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeDontHasInfo);
	{
		RTextValue* Value = PHelper().CreateRText(items, "Preconditions\\Lua Precondition", &SelectedNode->Precondition);
		Value->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodePrecondition);
		Value->OnHintEvent = xr_make_delegate(this, &DialogEditor::HintScriptFunction);
	}

	PHelper().CreateRText(items, "Actions\\Give Info", &SelectedNode->GiveInfo)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeGiveInfo);
	{
		RTextValue* Value = PHelper().CreateRText(items, "Actions\\Lua Action", &SelectedNode->Action);
		Value->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeAction);
		Value->OnHintEvent = xr_make_delegate(this, &DialogEditor::HintScriptFunction);
	}

	PHelper().CreateRText(items, "Text\\String ID", &SelectedNode->Text)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeText);

	if (!SelectedNode->IsBranch)
	{
		SelectedFinal = SelectedNode->IsFinal;
		PHelper().CreateBool(items, "Phrase\\Final", &SelectedFinal)->OnChangeEvent = xr_make_delegate(this, &DialogEditor::ChangeNodeFinal);
	}

	static shared_str TranslateStr;

	xr_string Translation;
	if (SelectedNode->Text.size() > 0 && FindDialogTranslation(SelectedNode->Text, Translation))
	{
		TranslateStr = Translation.c_str();
	}
	else
	{
		TranslateStr = "<not localized>";
	}
	PHelper().CreateCaption(items, "Text\\Translated", TranslateStr);

	Properties->AssignItems(items);
}

void DialogEditor::OpenFile(const xr_path& Path)
{
	if (Instance == nullptr)
	{
		return;
	}

	Instance->OpenFileInstance(Path);
}

void DialogEditor::OpenFileInstance(const xr_path& Path)
{
	Dialogs.clear();
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	Nodes.clear();
	Links.clear();
	LastOpenDialog = nullptr;
	SelectedNode = nullptr;
	Properties->ClearProperties();
	Modified = false;
	DialogFileName = Path.xfilename();

	File.Load(CONFIG_PATH, "gameplay", Path.xstring().c_str());

	XML_NODE* Node = File.GetRoot();
	if (Node == nullptr)
	{
		return;
	}

	RebuildDialogList();

	UndoStack.clear();
	RedoStack.clear();
	ChangePending = false;
	LastState = SavedState = CaptureState();

	UINodesEditorForm::Activate(ENodesMode::Dialogs);
}

void DialogEditor::RebuildDialogList()
{
	Dialogs.clear();
	XML_NODE* Node = File.GetRoot();
	if (Node == nullptr)
	{
		return;
	}

	XML_NODE* ChildNode = Node->FirstChildElement();

	while (ChildNode != nullptr)
	{
		if (ChildNode->ToElement() == nullptr)
		{
			ChildNode = ChildNode->NextSibling();
			continue;
		}

		shared_str NodeID = ChildNode->ToElement()->Attribute("id");
		if (NodeID.size() == 0)
		{
			ChildNode = ChildNode->NextSibling();
			continue;
		}

		Dialogs.emplace_back(NodeID, ChildNode);
		ChildNode = ChildNode->NextSibling();
	}

	std::sort(Dialogs.begin(), Dialogs.end(), [](std::pair<shared_str, XML_NODE*>& L, std::pair<shared_str, XML_NODE*>& R)
			  {
		xr_string NameA = *L.first;
		xr_string NameB = *R.first;
		return NameA < NameB; });
}

void DialogEditor::ChangeHasInfo(PropValue*)
{
	if (NodeHasInfo == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
								 { return LastOpenDialog == Pair.first; });

		if (Iter != Dialogs.end())
		{
			NodeHasInfo = Iter->second->ToElement()->InsertNewChildElement("has_info");
		}
	}

	NodeHasInfo->ToElement()->SetText(*HasInfo);
}

void DialogEditor::ChangeDontHasInfo(PropValue*)
{
	if (NodeDontHasInfo == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
								 { return LastOpenDialog == Pair.first; });

		if (Iter != Dialogs.end())
		{
			NodeDontHasInfo = Iter->second->ToElement()->InsertNewChildElement("dont_has_info");
		}
	}

	NodeDontHasInfo->ToElement()->SetText(*DontHasInfo);
}

void DialogEditor::ChangePrecondition(PropValue*)
{
	if (NodePrecondition == nullptr)
	{
		auto Iter = std::find_if(Dialogs.begin(), Dialogs.end(), [this](auto& Pair)
								 { return LastOpenDialog == Pair.first; });

		if (Iter != Dialogs.end())
		{
			NodePrecondition = Iter->second->ToElement()->InsertNewChildElement("precondition");
		}
	}

	NodePrecondition->ToElement()->SetText(*Precondition);
}

void DialogEditor::ChangeNodeHasInfo(PropValue*)
{
	if (SelectedNode == nullptr)
	{
		return;
	}

	SelectedNode->ValidateNodes(SelectedNode->HasInfo, "has_info");
}

void DialogEditor::ChangeNodeDontHasInfo(PropValue*)
{
	if (SelectedNode == nullptr)
	{
		return;
	}

	SelectedNode->ValidateNodes(SelectedNode->DontHasInfo, "dont_has_info");
}

void DialogEditor::ChangeNodeGiveInfo(PropValue*)
{
	if (SelectedNode == nullptr)
	{
		return;
	}

	SelectedNode->ValidateNodes(SelectedNode->GiveInfo, "give_info");
}

void DialogEditor::ChangeNodePrecondition(PropValue*)
{
	if (SelectedNode != nullptr)
	{
		SelectedNode->SetElement("precondition", SelectedNode->Precondition, SelectedNode->PreconditionNode);
	}
}

void DialogEditor::HintScriptFunction(const xr_string& Before, xr_vector<xr_string>& Out)
{
	EContext.Visor->GetFunctionHints(Before, Out);
}

void DialogEditor::ChangeNodeAction(PropValue*)
{
	if (SelectedNode != nullptr)
	{
		SelectedNode->SetElement("action", SelectedNode->Action, SelectedNode->ActionNode);
	}
}

void DialogEditor::ChangeNodeText(PropValue*)
{
	if (SelectedNode != nullptr)
	{
		SelectedNode->SetText(SelectedNode->Text);
		// The translation caption follows the new id: not while the properties are drawn
		RefreshProperties = true;
	}
}

void DialogEditor::ChangeNodeFinal(PropValue*)
{
	if (SelectedNode != nullptr)
	{
		SelectedNode->SetFinal(SelectedFinal);
	}
}

void DialogEditor::BuildIdMaps()
{
	auto Contains = [](const xr_vector<CDialogNode*>& V, CDialogNode* N)
	{
		for (CDialogNode* X : V)
		{
			if (X == N)
			{
				return true;
			}
		}
		return false;
	};

	for (auto It = NodeIds.begin(); It != NodeIds.end();)
	{
		if (!Contains(Nodes, It->first))
		{
			It = NodeIds.erase(It);
		}
		else
		{
			++It;
		}
	}
	for (auto It = InputPins.begin(); It != InputPins.end();)
	{
		if (!Contains(Nodes, It->first))
		{
			It = InputPins.erase(It);
		}
		else
		{
			++It;
		}
	}
	for (auto It = OutputPins.begin(); It != OutputPins.end();)
	{
		if (!Contains(Nodes, It->first))
		{
			It = OutputPins.erase(It);
		}
		else
		{
			++It;
		}
	}

	{
		xr_set<ed::PinId> ValidPins;
		for (auto& [Node, Pins] : OutputPins)
		{
			for (ed::PinId P : Pins)
			{
				ValidPins.insert(P);
			}
		}
		for (auto It = OutputPinXml.begin(); It != OutputPinXml.end();)
		{
			if (ValidPins.find(It->first) == ValidPins.end())
			{
				It = OutputPinXml.erase(It);
			}
			else
			{
				++It;
			}
		}
	}

	PinNode.clear();
	PinIsOutput.clear();
	OutputPinSlot.clear();

	for (auto& [Node, Pin] : InputPins)
	{
		PinNode[Pin] = Node;
		PinIsOutput[Pin] = false;
	}
	for (auto& [Node, Pins] : OutputPins)
	{
		int k = 0;
		for (ed::PinId Pin : Pins)
		{
			PinNode[Pin] = Node;
			PinIsOutput[Pin] = true;
			OutputPinSlot[Pin] = k++;
		}
	}
	for (auto& [Node, Pins] : OutputPins)
	{
		if (!Node->IsBranch)
		{
			continue;
		}
		for (int k = 0; k < (int)Pins.size(); ++k)
		{
			if (OutputPinXml.find(Pins[k]) == OutputPinXml.end() && k < (int)Node->NextNodes.size())
			{
				OutputPinXml[Pins[k]] = Node->NextNodes[k];
			}
		}
	}

	for (CDialogNode* Node : Nodes)
	{
		if (NodeIds.find(Node) != NodeIds.end())
		{
			continue;
		}

		NodeIds[Node] = ed::NodeId(NextNodeId++);

		ed::PinId InputPin = ed::PinId(NextPinId++);
		InputPins[Node] = InputPin;
		PinNode[InputPin] = Node;
		PinIsOutput[InputPin] = false;

		int NumOutputs = Node->IsBranch ? std::max(1, (int)Node->NextNodes.size()) : 1;
		xr_vector<ed::PinId> OutPins;
		for (int k = 0; k < NumOutputs; ++k)
		{
			ed::PinId Pin = ed::PinId(NextPinId++);
			OutPins.push_back(Pin);
			PinNode[Pin] = Node;
			PinIsOutput[Pin] = true;
			OutputPinSlot[Pin] = k;
			if (Node->IsBranch && k < (int)Node->NextNodes.size())
			{
				OutputPinXml[Pin] = Node->NextNodes[k];
			}
		}
		OutputPins[Node] = OutPins;
	}
}

CDialogNode* DialogEditor::NodeFromId(ed::NodeId Id)
{
	for (auto& [Node, NodeIdValue] : NodeIds)
	{
		if (NodeIdValue == Id)
		{
			return Node;
		}
	}
	return nullptr;
}

CDialogNode* DialogEditor::NodeOwningPin(ed::PinId Id)
{
	auto It = PinNode.find(Id);
	return It != PinNode.end() ? It->second : nullptr;
}

bool DialogEditor::LayoutNodes()
{
	const float GapX = 80.0f;
	const float GapY = 40.0f;
	const float OriginX = 40.0f;
	const float OriginY = 40.0f;

	if (Nodes.empty())
	{
		return true;
	}

	xr_map<CDialogNode*, xr_vector<CDialogNode*>> Outgoing;
	xr_map<CDialogNode*, bool> HasIncoming;
	for (const auto& Link : Links)
	{
		if (Link.Source == nullptr || Link.Target == nullptr)
		{
			continue;
		}
		Outgoing[Link.Source].push_back(Link.Target);
		HasIncoming[Link.Target] = true;
	}

	xr_map<CDialogNode*, int> Depth;
	xr_vector<CDialogNode*> Queue;
	for (CDialogNode* Node : Nodes)
	{
		if (!HasIncoming[Node])
		{
			Queue.push_back(Node);
		}
	}

	if (Queue.empty())
	{
		Queue = Nodes;
	}

	for (CDialogNode* Node : Queue)
	{
		if (Depth.find(Node) == Depth.end())
		{
			Depth[Node] = 0;
		}
	}

	size_t Head = 0;
	while (Head < Queue.size())
	{
		CDialogNode* Current = Queue[Head++];
		int CurrentDepth = Depth[Current];
		auto It = Outgoing.find(Current);
		if (It != Outgoing.end())
		{
			for (CDialogNode* Next : It->second)
			{
				if (Depth.find(Next) == Depth.end())
				{
					Depth[Next] = CurrentDepth + 1;
					Queue.push_back(Next);
				}
			}
		}
	}

	int MaxDepth = 0;
	for (const auto& [Node, D] : Depth)
	{
		MaxDepth = std::max(MaxDepth, D);
	}
	for (CDialogNode* Node : Nodes)
	{
		if (Depth.find(Node) == Depth.end())
		{
			Depth[Node] = ++MaxDepth;
		}
	}

	// The nodes are as big as their text: the columns and the rows take the real sizes.
	// Before the first draw the size is not known yet: a guess, and the layout is made again
	bool SizesKnown = true;
	xr_map<CDialogNode*, ImVec2> Sizes;
	xr_map<int, float> ColumnWidths;
	for (CDialogNode* Node : Nodes)
	{
		ImVec2 Size = ed::GetNodeSize(NodeIds[Node]);
		if (Size.x < 1.0f)
		{
			Size = { ComputeNodeBodyWidth(Node, 480.0f) + ed::GetStyle().NodePadding.x + ed::GetStyle().NodePadding.z, 200.0f };
			SizesKnown = false;
		}
		Sizes[Node] = Size;
		float& Width = ColumnWidths[Depth[Node]];
		Width = std::max(Width, Size.x);
	}

	xr_map<int, float> ColumnX;
	float X = OriginX;
	for (const auto& [Column, Width] : ColumnWidths)
	{
		ColumnX[Column] = X;
		X += Width + GapX;
	}

	xr_map<int, float> ColumnY;
	for (CDialogNode* Node : Nodes)
	{
		const int Column = Depth[Node];
		auto Iter = ColumnY.find(Column);
		const float Y = Iter != ColumnY.end() ? Iter->second : OriginY;
		ed::SetNodePosition(NodeIds[Node], ImVec2(ColumnX[Column], Y));
		ColumnY[Column] = Y + Sizes[Node].y + GapY;
	}
	return SizesKnown;
}

void DialogEditor::AddBranchOutput(CDialogNode* Node)
{
	if (Node == nullptr || !Node->IsBranch)
	{
		return;
	}

	ed::PinId Pin = ed::PinId(NextPinId++);
	OutputPins[Node].push_back(Pin);
	PinNode[Pin] = Node;
	PinIsOutput[Pin] = true;
	OutputPinSlot[Pin] = (int)OutputPins[Node].size() - 1;
}

void DialogEditor::DeleteNode(CDialogNode* Node)
{
	for (CDialogNode* Other : Nodes)
	{
		if (Other == Node)
		{
			continue;
		}

		XML_NODE* Child = Other->ParentNode->FirstChildElement();
		while (Child != nullptr)
		{
			const char* ChildText = Child->ToElement()->GetText();
			if (0 == xr_strcmp(Child->Value(), "next") && ChildText && ChildText[0] && 0 == xr_strcmp(ChildText, Node->NodeName.c_str()))
			{
				XML_NODE* Next = Child->NextSiblingElement();
				Other->ParentNode->ToElement()->DeleteChild(Child);
				Child = Next;
			}
			else
			{
				Child = Child->NextSiblingElement();
			}
		}
	}

	if (Node->ParentNode != nullptr && Node->ParentNode->Parent() != nullptr)
	{
		Node->ParentNode->Parent()->DeleteChild(Node->ParentNode);
	}

	Nodes.erase(std::remove(Nodes.begin(), Nodes.end(), Node), Nodes.end());

	Links.erase(
		std::remove_if(Links.begin(), Links.end(), [Node](const NodeLink& L)
					   { return L.Source == Node || L.Target == Node; }),
		Links.end()
	);

	// The properties point to the fields of the node
	if (SelectedNode == Node)
	{
		SelectNodeEvent(nullptr);
	}

	xr_delete(Node);
	BuildIdMaps();
}

void DialogEditor::UpdateSelection()
{
	int Count = ed::GetSelectedObjectCount();
	if (Count > 0)
	{
		xr_vector<ed::NodeId> Selected;
		Selected.resize(Count);
		int NodeCount = ed::GetSelectedNodes(Selected.data(), Count);

		CDialogNode* Node = nullptr;
		if (NodeCount > 0)
		{
			Node = NodeFromId(Selected[0]);
		}

		if (Node != SelectedNode)
		{
			SelectNodeEvent(Node);
		}
	}
	else if (SelectedNode != nullptr)
	{
		SelectNodeEvent(nullptr);
	}
}

void DialogEditor::DrawNode(CDialogNode* Node)
{
	const ed::NodeId NodeIdValue = NodeIds[Node];
	const auto& OutPins = OutputPins[Node];
	const float Width = ComputeNodeBodyWidth(Node, 480.0f);
	const float Spacing = ImGui::GetStyle().ItemSpacing.x;

	ed::BeginNode(NodeIdValue);
	ImGui::PushID(NodeIdValue.AsPointer());
	const ImVec2 Origin = ImGui::GetCursorScreenPos();

	auto Pin = [&](ed::PinId Id, ed::PinKind Kind)
	{
		ed::BeginPin(Id, Kind);
		ed::PinPivotAlignment({ Kind == ed::PinKind::Input ? 0.0f : 1.0f, 0.5f });
		ed::PinPivotSize({ 0, 0 });
		DrawPinArrow(ConnectedPins.contains(Id));
		ed::EndPin();
	};

	// Header: the input, the title and the first output, like the nodes of Logic
	Pin(InputPins[Node], ed::PinKind::Input);
	ImGui::SameLine();
	const char* Icon = Node->IsBranch ? ICON_FA_CODE_BRANCH : Node->IsFinal ? ICON_FA_FLAG_CHECKERED : ICON_FA_COMMENT;
	ImGui::Text("%s %s", Icon, Node->NodeName.c_str());
	if (!OutPins.empty())
	{
		ImGui::SameLine();
		ImGui::SetCursorScreenPos({ Origin.x + Width - NodesUI::PinSize, Origin.y });
		Pin(OutPins[0], ed::PinKind::Output);
	}

	ImGui::SetCursorScreenPos({ Origin.x + NodesUI::PinSize + Spacing, ImGui::GetItemRectMax().y + 2.0f });
	{
		xr_string Caption = Node->IsBranch ? "Branch" : Node->IsFinal ? "Final phrase: the dialog ends" : "Phrase";
		if (Node == Nodes.front())
		{
			Caption = "Start  " + Caption;
		}
		ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1, 1, 1, 0.55f));
		ImGui::TextUnformatted(Caption.c_str());
		ImGui::PopStyleColor();
	}
	const float HeaderBottom = ImGui::GetItemRectMax().y + 4.0f;
	ImGui::SetCursorScreenPos({ Origin.x, HeaderBottom + 8.0f });

	if (!Node->IsBranch)
	{
		DrawNodeBody(Node, Width);
	}

	// The other outputs of a branch
	for (size_t Index = 1; Index < OutPins.size(); ++Index)
	{
		const xr_string Label = make_string<xr_string>("Out %u", (u32)Index + 1);
		const float RowY = ImGui::GetCursorScreenPos().y;
		ImGui::SetCursorScreenPos({ Origin.x + Width - NodesUI::PinSize - Spacing - ImGui::CalcTextSize(Label.c_str()).x, RowY });
		ImGui::TextDisabled("%s", Label.c_str());
		ImGui::SameLine();
		Pin(OutPins[Index], ed::PinKind::Output);
	}
	if (Node->IsBranch)
	{
		ImGui::SetCursorScreenPos({ Origin.x, ImGui::GetCursorScreenPos().y + 2.0f });
		if (ImGui::SmallButton(ICON_FA_PLUS " Output"))
		{
			AddBranchOutput(Node);
			MarkChanged();
		}
	}
	ImGui::Dummy({ Width, 1.0f });

	ImGui::PopID();
	ed::EndNode();

	DrawNodeHeaderBackground(NodeIdValue, HeaderBottom - Origin.y, NodeHeaderColor(Node));
}

float DialogEditor::ComputeNodeBodyWidth(CDialogNode* Node, float MaxWidth)
{
	// The node is as wide as its widest line, so the header output pin stays on the right edge
	float Result = ImGui::CalcTextSize(Node->NodeName.c_str()).x + NodesUI::PinSize * 2.0f + 80.0f;
	if (!Node->IsBranch)
	{
		// The string id is not wrapped: the bar offset is 8
		Result = std::max(Result, ImGui::CalcTextSize(Node->Text.c_str()).x + 8.0f);

		// A chip is never wrapped: the row icon and the chip padding are 24 and 10 (see DrawNodeBody and DrawChips)
		auto Chips = [&Result](const shared_str& Value)
		{
			if (Value.size() == 0)
			{
				return;
			}
			for (xr_string Item : xr_string(Value.c_str()).Split(','))
			{
				Item = Item.RemoveWhitespaces();
				Result = std::max(Result, ImGui::CalcTextSize(("+" + Item).c_str()).x + 10.0f + 24.0f);
			}
		};
		Chips(Node->HasInfo);
		Chips(Node->DontHasInfo);
		Chips(Node->Precondition);
		Chips(Node->GiveInfo);
		Chips(Node->Action);
	}
	return std::clamp(Result, 260.0f, MaxWidth);
}

// "a, b" of the XML lists
static void SplitList(const shared_str& Value, const char* Prefix, ImColor Color, const char* Tooltip, xr_vector<NodesUI::SChip>& Chips)
{
	if (Value.size() == 0)
	{
		return;
	}
	for (xr_string Item : xr_string(Value.c_str()).Split(','))
	{
		Item = Item.RemoveWhitespaces();
		if (!Item.empty())
		{
			Chips.push_back({ xr_string(Prefix) + Item, Color, Tooltip });
		}
	}
}

void DialogEditor::DrawNodeBody(CDialogNode* Node, float BodyWidth)
{
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	const ImVec2 Start = ImGui::GetCursorScreenPos();

	// The phrase as the player reads it, with a bar on the left
	xr_string Translation;
	const bool HasText = Node->Text.size() > 0;
	const bool Localized = HasText && CDialogNode::FindTranslation != nullptr && CDialogNode::FindTranslation(Node->Text, Translation);
	ImGui::SetCursorScreenPos({ Start.x + 8.0f, Start.y });
	ImGui::BeginGroup();
	if (Localized)
	{
		size_t MaxLength = 260;
		const bool IsCut = Translation.size() > MaxLength;
		while (IsCut && MaxLength > 0 && (static_cast<u8>(Translation[MaxLength]) & 0xC0) == 0x80)
		{
			--MaxLength;
		}
		const xr_string Shown = IsCut ? Translation.substr(0, MaxLength) + "..." : Translation;
		ImGui::PushTextWrapPos(ImGui::GetCursorPosX() + BodyWidth - 8.0f);
		ImGui::TextUnformatted(Shown.c_str());
		ImGui::PopTextWrapPos();
		if (IsCut)
		{
			ImGui::SetItemTooltip("%s", Translation.c_str());
		}
	}
	else
	{
		ImGui::TextColored(NodesUI::ColorWarning, ICON_FA_TRIANGLE_EXCLAMATION " %s", HasText ? "Not localized" : "No text");
		ImGui::SameLine();
		if (ImGui::SmallButton(ICON_FA_LANGUAGE " Localize..."))
		{
			OpenLocalization(Node);
		}
	}
	if (HasText)
	{
		ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1, 1, 1, 0.4f));
		ImGui::TextUnformatted(Node->Text.c_str());
		ImGui::PopStyleColor();
		ImGui::SetItemTooltip("The string id of the text");
	}
	ImGui::EndGroup();
	DrawList->AddRectFilled({ Start.x, Start.y }, { Start.x + 3.0f, ImGui::GetItemRectMax().y }, Localized ? (ImU32)NodeHeaderColor(Node) : (ImU32)NodesUI::ColorWarning, 1.5f);
	ImGui::SetCursorScreenPos({ Start.x, ImGui::GetItemRectMax().y + 6.0f });

	// Only what is set is shown: the fields are edited in the Details
	xr_vector<NodesUI::SChip> Conditions;
	SplitList(Node->HasInfo, "+", ImColor(110, 200, 90), "The player has the info", Conditions);
	SplitList(Node->DontHasInfo, "-", ImColor(230, 90, 80), "The player has no info", Conditions);
	SplitList(Node->Precondition, "?", ImColor(90, 170, 240), "Lua precondition", Conditions);

	xr_vector<NodesUI::SChip> Effects;
	SplitList(Node->GiveInfo, "+", ImColor(190, 120, 240), "Gives the info", Effects);
	SplitList(Node->Action, "%", ImColor(240, 150, 60), "Lua action", Effects);

	auto Row = [&](const char* Icon, const char* Tooltip, const xr_vector<NodesUI::SChip>& Chips)
	{
		if (Chips.empty())
		{
			return;
		}
		ImGui::TextDisabled("%s", Icon);
		ImGui::SetItemTooltip("%s", Tooltip);
		ImGui::SameLine();
		NodesUI::DrawChips(Chips, BodyWidth - 24.0f);
		ImGui::SetCursorScreenPos({ Start.x, ImGui::GetCursorScreenPos().y + 2.0f });
	};
	Row(ICON_FA_FILTER, "Shown when", Conditions);
	Row(ICON_FA_BOLT, "Does when said", Effects);
}

void DialogEditor::DrawNodeHeaderBackground(ed::NodeId Id, float HeaderHeight, ImColor Color)
{
	ImVec2 Position = ed::GetNodePosition(Id);
	ImVec2 Size = ed::GetNodeSize(Id);
	if (Size.x < 1.0f)
	{
		return;
	}

	NodesUI::DrawNodeHeader(ed::GetNodeBackgroundDrawList(Id), Position, Size.x, ed::GetStyle().NodePadding.y + HeaderHeight, Color);
}

void DialogEditor::HandleLinkCreation()
{
	if (ed::BeginCreate(ImColor(255, 255, 255), 2.0f))
	{
		ed::PinId StartPin = 0;
		ed::PinId EndPin = 0;

		if (ed::QueryNewLink(&StartPin, &EndPin))
		{
			CDialogNode* First = NodeOwningPin(StartPin);
			CDialogNode* Second = NodeOwningPin(EndPin);

			if (First != nullptr && Second != nullptr && First != Second)
			{
				bool StartIsOutput = (PinIsOutput.find(StartPin) != PinIsOutput.end() && PinIsOutput[StartPin]);
				bool EndIsOutput = (PinIsOutput.find(EndPin) != PinIsOutput.end() && PinIsOutput[EndPin]);

				if (StartIsOutput != EndIsOutput)
				{
					CDialogNode* Source = StartIsOutput ? First : Second;
					CDialogNode* Target = StartIsOutput ? Second : First;
					ed::PinId OutPin = StartIsOutput ? StartPin : EndPin;

					if (ed::AcceptNewItem(ImColor(128, 255, 128), 4.0f))
					{
						XML_NODE* NextNode = nullptr;
						if (Source->IsBranch)
						{
							NextNode = OutputPinXml[OutPin];
							if (NextNode == nullptr)
							{
								NextNode = Source->ParentNode->ToElement()->InsertNewChildElement("next");
								OutputPinXml[OutPin] = NextNode;
							}
							NextNode->ToElement()->SetText(Target->NodeName.c_str());
						}
						else
						{
							NextNode = Source->ParentNode->ToElement()->InsertNewChildElement("next");
							NextNode->ToElement()->SetText(Target->NodeName.c_str());
						}

						Source->MakeOutNode(Target, true);

						NodeLink Link;
						Link.Id = ed::LinkId(NextLinkId++);
						Link.Start = OutPin;
						Link.End = InputPins[Target];
						Link.Source = Source;
						Link.Target = Target;
						Link.NextNode = NextNode;
						Links.push_back(Link);
						MarkChanged();
					}
				}
				else
				{
					ed::RejectNewItem(ImColor(255, 0, 0), 2.0f);
				}
			}
			else
			{
				ed::RejectNewItem(ImColor(255, 0, 0), 2.0f);
			}
		}

		ed::PinId NewNodePin = 0;
		if (ed::QueryNewNode(&NewNodePin))
		{
			ed::AcceptNewItem();
		}
	}
	ed::EndCreate();
}

void DialogEditor::HandleDeletion()
{
	if (ed::BeginDelete())
	{
		ed::NodeId NodeIdValue = 0;
		while (ed::QueryDeletedNode(&NodeIdValue))
		{
			CDialogNode* Node = NodeFromId(NodeIdValue);
			if (Node != nullptr && ed::AcceptDeletedItem())
			{
				DeleteNode(Node);
				MarkChanged();
			}
		}

		ed::LinkId LinkIdValue = 0;
		while (ed::QueryDeletedLink(&LinkIdValue))
		{
			auto Iter = std::find_if(Links.begin(), Links.end(), [LinkIdValue](const NodeLink& L)
									 { return L.Id == LinkIdValue; });

			if (Iter != Links.end() && ed::AcceptDeletedItem())
			{
				if (Iter->NextNode != nullptr && Iter->Source != nullptr && Iter->Source->ParentNode != nullptr)
				{
					Iter->Source->ParentNode->ToElement()->DeleteChild(Iter->NextNode);
				}

				for (auto& [Pin, Xml] : OutputPinXml)
				{
					if (Xml == Iter->NextNode)
					{
						Xml = nullptr;
					}
				}

				Links.erase(Iter);
				MarkChanged();
			}
		}
	}
	ed::EndDelete();
}
