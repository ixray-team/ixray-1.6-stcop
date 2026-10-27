#include "stdafx.h"
#include "EFDEditor.h"
#include "IconsFontAwesome6.h"
#include "imgui_internal.h"

// Limits against broken files
static constexpr u32 MaxVariableCount = 64;
static constexpr u32 MaxWeightCount = 1u << 22;

xr_vector<CEFDEditor*> CEFDEditor::Opened;

const char* SEvaluationFunction::GetFunctionName(u32 Type)
{
	switch (Type)
	{
	case 0:		return "Distance";
	case 1:		return "GraphPointType0";
	case 2:		return "EquipmentType";
	case 3:		return "ItemDeterioration";
	case 4:		return "EquipmentPreference";
	case 5:		return "MainWeaponType";
	case 6:		return "MainWeaponPreference";
	case 7:		return "ItemValue";
	case 8:		return "WeaponAmmo";
	case 9:		return "DetectorType";
	case 21:	return "PersonalHealth";
	case 22:	return "PersonalMorale";
	case 23:	return "PersonalCreatureType";
	case 24:	return "PersonalWeaponType";
	case 25:	return "PersonalAccuracy";
	case 26:	return "PersonalIntelligence";
	case 27:	return "PersonalRelation";
	case 28:	return "PersonalGreed";
	case 29:	return "PersonalAggressiveness";
	case 30:	return "PersonalEyeRange";
	case 31:	return "PersonalMaxHealth";
	case 41:	return "EnemyHealth";
	case 42:	return "EnemyCreatureType";
	case 43:	return "EnemyWeaponType";
	case 44:	return "EnemyEquipmentCost";
	case 45:	return "EnemyRukzakWeight";
	case 46:	return "EnemyAnomality";
	case 47:	return "EnemyEyeRange";
	case 48:	return "EnemyMaxHealth";
	case 49:	return "EnemyAnomalyType";
	case 50:	return "EnemyDistanceToGraphPoint";
	case 61:	return "WeaponEffectiveness";
	case 62:	return "CreatureEffectiveness";
	case 63:	return "IntCreatureEffectiveness";
	case 64:	return "AccWeaponEffectiveness";
	case 65:	return "FinCreatureEffectiveness";
	case 66:	return "VictoryProbability";
	case 67:	return "EntityCost";
	case 68:	return "Expediency";
	case 69:	return "SurgeDeathProbability";
	case 70:	return "EquipmentValue";
	case 71:	return "MainWeaponValue";
	case 72:	return "SmallWeaponValue";
	case 73:	return "TerrainType";
	case 74:	return "WeaponAttackTimes";
	case 75:	return "WeaponSuccessProbability";
	case 76:	return "EnemyDetectability";
	case 77:	return "EnemyDetectProbability";
	case 78:	return "EnemyRetreatProbability";
	case 79:	return "AnomalyDetectProbability";
	case 80:	return "AnomalyInteractProbability";
	case 81:	return "AnomalyRetreatProbability";
	case 82:	return "BirthPercentage";
	case 83:	return "BirthProbability";
	case 84:	return "BirthSpeed";
	default:	return nullptr;
	}
}

bool SEvaluationFunction::Load(IReader& Reader, xr_string& Error)
{
	Variables.clear();
	Patterns.clear();

	if (Reader.elapsed() < intptr_t(3 * sizeof(u32)))
	{
		Error = "The file is too short";
		return false;
	}

	const u32 Version = Reader.r_u32();
	DataFormat = Reader.r_u32();
	if (Version != BuilderVersion)
	{
		Error = "Unsupported builder version " + xr_string::ToString(Version);
		return false;
	}

	const u32 VariableCount = Reader.r_u32();
	if (VariableCount == 0 || VariableCount > MaxVariableCount || Reader.elapsed() < intptr_t((2 * VariableCount + 4) * sizeof(u32)))
	{
		Error = "Invalid variable count";
		return false;
	}

	Variables.resize(VariableCount);
	for (SVariable& Variable : Variables)
	{
		Variable.Range = Reader.r_u32();
		if (Variable.Range == 0 || Variable.Range > MaxWeightCount)
		{
			Error = "Invalid variable range";
			return false;
		}
	}

	for (SVariable& Variable : Variables)
	{
		Variable.Type = Reader.r_u32();
	}

	FunctionType = Reader.r_u32();
	MinValue = Reader.r_float();
	MaxValue = Reader.r_float();

	const u32 PatternCount = Reader.r_u32();
	if (PatternCount > MaxVariableCount)
	{
		Error = "Invalid pattern count";
		return false;
	}

	Patterns.resize(PatternCount);
	u64 WeightCount = 0;
	for (SPattern& Pattern : Patterns)
	{
		const u32 Cardinality = Reader.elapsed() >= intptr_t(sizeof(u32)) ? Reader.r_u32() : 0;
		if (Cardinality == 0 || Cardinality > VariableCount || Reader.elapsed() < intptr_t(Cardinality * sizeof(u32)))
		{
			Error = "Invalid pattern cardinality";
			return false;
		}

		u64 Combinations = 1;
		Pattern.Variables.resize(Cardinality);
		for (u32& Index : Pattern.Variables)
		{
			Index = Reader.r_u32();
			if (Index >= VariableCount)
			{
				Error = "Invalid pattern variable";
				return false;
			}
			Combinations *= Variables[Index].Range;
			if (Combinations > MaxWeightCount)
			{
				Error = "The pattern is too large";
				return false;
			}
		}

		Pattern.Weights.resize(size_t(Combinations));
		WeightCount += Combinations;
	}

	if (Reader.elapsed() != intptr_t(WeightCount * sizeof(float)))
	{
		Error = "The weight table does not match the patterns";
		return false;
	}

	for (SPattern& Pattern : Patterns)
	{
		Reader.r(Pattern.Weights.data(), u32(Pattern.Weights.size() * sizeof(float)));
	}
	return true;
}

void SEvaluationFunction::Save(IWriter& Writer) const
{
	Writer.w_u32(BuilderVersion);
	Writer.w_u32(DataFormat);
	Writer.w_u32(u32(Variables.size()));

	for (const SVariable& Variable : Variables)
	{
		Writer.w_u32(Variable.Range);
	}

	for (const SVariable& Variable : Variables)
	{
		Writer.w_u32(Variable.Type);
	}

	Writer.w_u32(FunctionType);
	Writer.w_float(MinValue);
	Writer.w_float(MaxValue);
	Writer.w_u32(u32(Patterns.size()));

	for (const SPattern& Pattern : Patterns)
	{
		Writer.w_u32(u32(Pattern.Variables.size()));
		for (u32 Index : Pattern.Variables)
		{
			Writer.w_u32(Index);
		}
	}

	for (const SPattern& Pattern : Patterns)
	{
		Writer.w(Pattern.Weights.data(), u32(Pattern.Weights.size() * sizeof(float)));
	}
}

u32 SEvaluationFunction::GetCombinationCount(const SPattern& Pattern) const
{
	return u32(Pattern.Weights.size());
}

void SEvaluationFunction::GetCombination(const SPattern& Pattern, u32 Index, xr_vector<u32>& Values) const
{
	Values.resize(Pattern.Variables.size());
	for (size_t Item = Pattern.Variables.size(); Item > 0; --Item)
	{
		const u32 Range = Variables[Pattern.Variables[Item - 1]].Range;
		Values[Item - 1] = Index % Range;
		Index /= Range;
	}
}

static xr_string GetVariableName(const SEvaluationFunction::SVariable& Variable)
{
	const char* Name = SEvaluationFunction::GetFunctionName(Variable.Type);
	return Name != nullptr ? xr_string(Name) : "Function " + xr_string::ToString(Variable.Type);
}

void CEFDEditor::Open(const xr_path& Path)
{
	for (CEFDEditor* Editor : Opened)
	{
		if (Editor->Path == Path)
		{
			Editor->NeedFocus = true;
			return;
		}
	}

	CEFDEditor* Editor = new CEFDEditor(Path);
	Opened.push_back(Editor);
	EContext.UI->Push(Editor);
}

CEFDEditor::CEFDEditor(const xr_path& FilePath) :
	Path(FilePath)
{
	Title = Path.xfilename() + "###EFD_" + Path.xstring();
	Load();
}

CEFDEditor::~CEFDEditor()
{
	Opened.erase(std::remove(Opened.begin(), Opened.end(), this), Opened.end());
}

bool CEFDEditor::Load()
{
	Error.clear();
	Modified = false;
	SelectCell(-1);
	Journal.Clear();
	SavedData.clear();

	IReader* Reader = FS.r_open(Path.xstring().c_str());
	if (Reader == nullptr)
	{
		Error = "Can't open the file";
		return false;
	}

	const bool Result = Function.Load(*Reader, Error);
	FS.r_close(Reader);

	if (!Result)
	{
		Msg("! Evaluation function [%s]: %s", Path.xstring().c_str(), Error.c_str());
		return false;
	}

	CUndoJournal::State Parts;
	CaptureState(Parts);
	SavedData = *Parts.front().Data;
	Journal.Reset(std::move(Parts));
	return true;
}

void CEFDEditor::CaptureState(CUndoJournal::State& Parts) const
{
	static const shared_str PartName = "function";

	CMemoryWriter Writer;
	Function.Save(Writer);
	CUndoJournal::Add(Parts, 0, PartName, Writer);
}

void CEFDEditor::CommitUndo()
{
	CUndoJournal::State Parts;
	CaptureState(Parts);
	Journal.Commit(std::move(Parts));
	UpdateModified();
}

void CEFDEditor::UpdateModified()
{
	CMemoryWriter Writer;
	Function.Save(Writer);
	Modified = Writer.size() != SavedData.size() || memcmp(Writer.pointer(), SavedData.data(), SavedData.size()) != 0;
}

void CEFDEditor::Restore(const CUndoJournal::State& Parts)
{
	for (const CUndoJournal::SPart& Part : Parts)
	{
		IReader Reader = Part.Open();
		xr_string RestoreError;
		if (!Function.Load(Reader, RestoreError))
		{
			Msg("! Evaluation function [%s]: can't restore the undo step: %s", Path.xstring().c_str(), RestoreError.c_str());
		}
	}

	EditingCell = -1;
	UpdateModified();
}

bool CEFDEditor::Undo()
{
	CUndoJournal::State Parts;
	if (!Journal.Undo(Parts))
	{
		return false;
	}

	Restore(Parts);
	return true;
}

bool CEFDEditor::Redo()
{
	CUndoJournal::State Parts;
	if (!Journal.Redo(Parts))
	{
		return false;
	}

	Restore(Parts);
	return true;
}

CEFDEditor* CEFDEditor::GetFocused()
{
	for (CEFDEditor* Editor : Opened)
	{
		if (Editor->Focused && !Editor->IsClosed())
		{
			return Editor;
		}
	}
	return nullptr;
}

bool CEFDEditor::Save()
{
	IWriter* Writer = FS.w_open(Path.xstring().c_str());
	if (Writer == nullptr)
	{
		Msg("! Can't save the evaluation function [%s]", Path.xstring().c_str());
		return false;
	}

	Function.Save(*Writer);
	FS.w_close(Writer);

	CMemoryWriter Saved;
	Function.Save(Saved);
	SavedData.assign(Saved.pointer(), Saved.pointer() + Saved.size());
	Modified = false;
	Msg("* Evaluation function saved [%s]", Path.xstring().c_str());
	return true;
}

static const ImVec4 OutOfRangeColor = {1.f, 0.55f, 0.3f, 1.f};

void CEFDEditor::Draw()
{
	if (NeedFocus)
	{
		ImGui::SetNextWindowFocus();
		ImGui::SetNextWindowSize(ImVec2(760.f, 560.f), ImGuiCond_FirstUseEver);
		NeedFocus = false;
	}

	const ImGuiWindowFlags WindowFlags = Modified ? ImGuiWindowFlags_UnsavedDocument : ImGuiWindowFlags_None;
	const bool Visible = ImGui::Begin(Title.c_str(), &bOpen, WindowFlags);
	Focused = Visible && ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	if (!Visible)
	{
		ImGui::End();
		return;
	}

	if (!Error.empty())
	{
		ImGui::TextColored(ImVec4(1.f, 0.4f, 0.4f, 1.f), ICON_FA_TRIANGLE_EXCLAMATION " Can't read the evaluation function: %s", Error.c_str());
		if (ImGui::Button(ICON_FA_ROTATE " Reload"))
		{
			Load();
		}
		ImGui::End();
		return;
	}

	if (Function.Patterns.size() == 1)
	{
		DrawPattern(Function.Patterns.front());
	}
	else if (ImGui::BeginTabBar("##Patterns"))
	{
		for (size_t Index = 0; Index < Function.Patterns.size(); ++Index)
		{
			xr_string Label = "Pattern " + xr_string::ToString(u32(Index + 1));
			if (ImGui::BeginTabItem(Label.c_str()))
			{
				if (ActivePattern != Index)
				{
					ActivePattern = Index;
					SelectCell(-1);
				}
				DrawPattern(Function.Patterns[Index]);
				ImGui::EndTabItem();
			}
		}
		ImGui::EndTabBar();
	}

	if (Focused && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_S))
	{
		Save();
	}

	ImGui::End();

	if (!bOpen && Modified)
	{
		const int Result = ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo | mbCancel, "%s has unsaved changes. Save them?", Path.xfilename().c_str());
		if (Result == mrCancel)
		{
			bOpen = true;
		}
		else if (Result == mrYes && !Save())
		{
			bOpen = true;
		}
	}
}

bool CEFDEditor::IsGridView(const SEvaluationFunction::SPattern& Pattern) const
{
	return ViewMode == EViewMode::Grid && Pattern.Variables.size() == 2;
}

void CEFDEditor::SelectCell(int Index)
{
	SelectedCell = Index;
	EditingCell = -1;
	ScrollToSelected = Index >= 0;
}

void CEFDEditor::BeginEdit(const SEvaluationFunction::SPattern& Pattern)
{
	if (SelectedCell < 0 || SelectedCell >= int(Pattern.Weights.size()))
	{
		return;
	}

	EditingCell = SelectedCell;
	EditValue = Pattern.Weights[SelectedCell];
	EditFocus = true;
}

// Toolbar groups wrap to the next line when the window is too narrow
void CEFDEditor::BeginToolbarGroup(int Index)
{
	if (Index > 0)
	{
		const float Spacing = ImGui::GetFontSize();
		const float Right = ImGui::GetItemRectMax().x + Spacing + ToolbarGroupWidths[Index];
		if (Right <= ImGui::GetWindowPos().x + ImGui::GetWindowContentRegionMax().x)
		{
			ImGui::SameLine(0.f, Spacing);
		}
	}
	ImGui::BeginGroup();
}

void CEFDEditor::EndToolbarGroup(int Index)
{
	ImGui::EndGroup();
	ToolbarGroupWidths[Index] = ImGui::GetItemRectSize().x;
}

void CEFDEditor::DrawToolbar(SEvaluationFunction::SPattern& Pattern)
{
	BeginToolbarGroup(0);
	if (ImGui::Button(ICON_FA_FLOPPY_DISK " Save"))
	{
		Save();
	}
	ImGui::SetItemTooltip("Save the file (Ctrl+S)");

	ImGui::SameLine();
	ImGui::BeginDisabled(!Modified);
	if (ImGui::Button(ICON_FA_CLOCK_ROTATE_LEFT " Revert"))
	{
		Load();
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Reload the file and drop the changes and the undo history");

	EndToolbarGroup(0);
	BeginToolbarGroup(1);
	ImGui::BeginDisabled(!Journal.CanUndo());
	if (ImGui::Button(ICON_FA_ARROW_ROTATE_LEFT))
	{
		Undo();
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Undo (Ctrl+Z), %zu step(s)", Journal.UndoCount());

	ImGui::SameLine();
	ImGui::BeginDisabled(!Journal.CanRedo());
	if (ImGui::Button(ICON_FA_ARROW_ROTATE_RIGHT))
	{
		Redo();
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Redo (Ctrl+Y), %zu step(s)", Journal.RedoCount());

	EndToolbarGroup(1);
	BeginToolbarGroup(2);
	const bool CanGrid = Pattern.Variables.size() == 2;
	const bool Grid = IsGridView(Pattern);

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0.f, ImGui::GetStyle().ItemSpacing.y));
	bool ListActive = !Grid;
	if (XRay::ImGui::ToolbarButton("##ViewList", ICON_FA_LIST " List", &ListActive, {0.f, ImGui::GetFrameHeight()}, ImDrawFlags_RoundCornersLeft))
	{
		ViewMode = EViewMode::List;
	}
	ImGui::SetItemTooltip("Every combination of the inputs in its own row");
	ImGui::SameLine();

	ImGui::BeginDisabled(!CanGrid);
	bool GridActive = Grid;
	if (XRay::ImGui::ToolbarButton("##ViewGrid", ICON_FA_TABLE_CELLS " Grid", &GridActive, {0.f, ImGui::GetFrameHeight()}, ImDrawFlags_RoundCornersRight))
	{
		ViewMode = EViewMode::Grid;
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip(CanGrid ? "First input in rows, second in columns" : "Only for functions of two inputs");
	ImGui::PopStyleVar();

	if (Grid != IsGridView(Pattern))
	{
		ScrollToSelected = SelectedCell >= 0;
	}

	EndToolbarGroup(2);
	BeginToolbarGroup(3);
	if (ImGui::Button(ICON_FA_COPY))
	{
		CopyToClipboard(Pattern);
	}
	ImGui::SetItemTooltip("Copy the table as tab separated text, ready for a spreadsheet");

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_PASTE))
	{
		PasteFromClipboard(Pattern);
	}
	ImGui::SetItemTooltip("Paste values from a spreadsheet: the last column of each row, in the List order");

	// The engine does not clamp the result
	EndToolbarGroup(3);
	BeginToolbarGroup(4);
	const float FieldWidth = ImGui::GetFontSize() * 5.f;

	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("Min");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(FieldWidth);
	if (ImGui::DragFloat("##Min", &Function.MinValue, 0.1f, 0.f, Function.MaxValue, "%.2f"))
	{
		Modified = true;
	}
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		CommitUndo();
	}

	ImGui::SameLine();
	ImGui::TextUnformatted("Max");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(FieldWidth);
	if (ImGui::DragFloat("##Max", &Function.MaxValue, 0.1f, Function.MinValue, FLT_MAX, "%.2f"))
	{
		Modified = true;
	}
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		CommitUndo();
	}

	EndToolbarGroup(4);
	BeginToolbarGroup(5);
	ImGui::Checkbox("Heat map", &HeatMap);
	ImGui::SetItemTooltip("Tint the values from Min (blue) to Max (red)");
	EndToolbarGroup(5);
}

void CEFDEditor::DrawPattern(SEvaluationFunction::SPattern& Pattern)
{
	const char* Name = SEvaluationFunction::GetFunctionName(Function.FunctionType);
	ImGui::TextUnformatted(Name != nullptr ? Name : "Unknown function");
	ImGui::SameLine();

	xr_string Inputs = "(";
	for (u32 Index : Pattern.Variables)
	{
		const SEvaluationFunction::SVariable& Variable = Function.Variables[Index];
		if (Inputs.size() > 1)
		{
			Inputs += ", ";
		}
		Inputs += GetVariableName(Variable) + " 1.." + xr_string::ToString(Variable.Range);
	}
	Inputs += ")";
	ImGui::TextDisabled("%s", Inputs.c_str());
	ImGui::SetItemTooltip("Function type %u, %zu values", Function.FunctionType, Pattern.Weights.size());

	DrawToolbar(Pattern);
	ImGui::Spacing();

	if (SelectedCell >= int(Pattern.Weights.size()))
	{
		SelectCell(-1);
	}

	const float StatusHeight = ImGui::GetFrameHeightWithSpacing();
	const float TableHeight = std::max(ImGui::GetContentRegionAvail().y - StatusHeight, ImGui::GetFrameHeight() * 4.f);

	if (IsGridView(Pattern))
	{
		DrawGrid(Pattern, TableHeight);
	}
	else
	{
		DrawList(Pattern, TableHeight);
	}

	HandleKeyboard(Pattern);
	DrawStatusBar(Pattern);
}

void CEFDEditor::DrawCell(SEvaluationFunction::SPattern& Pattern, u32 Index)
{
	float& Weight = Pattern.Weights[Index];
	const bool OutOfRange = Weight < Function.MinValue || Weight > Function.MaxValue;

	if (HeatMap && Function.MaxValue > Function.MinValue)
	{
		const float Factor = clampr((Weight - Function.MinValue) / (Function.MaxValue - Function.MinValue), 0.f, 1.f);
		const ImVec4 Color = {_lerp(0.2f, 0.85f, Factor), _lerp(0.35f, 0.3f, Factor), _lerp(0.75f, 0.2f, Factor), 0.35f};
		ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImGui::GetColorU32(Color));
	}

	ImGui::PushID(int(Index));

	if (EditingCell == int(Index))
	{
		if (EditFocus)
		{
			ImGui::SetKeyboardFocusHere();
			EditFocus = false;
		}

		ImGui::SetNextItemWidth(-1.f);
		ImGui::InputFloat("##Edit", &EditValue, 0.f, 0.f, "%.3f", ImGuiInputTextFlags_AutoSelectAll);
		if (ImGui::IsItemDeactivatedAfterEdit() && EditValue != Weight)
		{
			Weight = EditValue;
			CommitUndo();
		}

		if (ImGui::IsItemDeactivated())
		{
			EditingCell = -1;

			if (ImGui::IsKeyPressed(ImGuiKey_Enter) || ImGui::IsKeyPressed(ImGuiKey_KeypadEnter))
			{
				const int Step = IsGridView(Pattern) ? int(Function.Variables[Pattern.Variables[1]].Range) : 1;
				if (SelectedCell + Step < int(Pattern.Weights.size()))
				{
					SelectCell(SelectedCell + Step);
				}
			}
		}
	}
	else
	{
		const bool Selected = SelectedCell == int(Index);
		const ImVec2 Pos = ImGui::GetCursorScreenPos();
		const float Width = ImGui::GetContentRegionAvail().x;

		if (ImGui::Selectable("##Cell", Selected, ImGuiSelectableFlags_AllowDoubleClick))
		{
			SelectedCell = int(Index);
			EditingCell = -1;
			if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
			{
				BeginEdit(Pattern);
			}
		}

		string32 Text;
		xr_sprintf(Text, "%.3f", Weight);
		const float TextWidth = ImGui::CalcTextSize(Text).x;
		const ImU32 TextColor = ImGui::GetColorU32(OutOfRange ? OutOfRangeColor : ImGui::GetStyleColorVec4(ImGuiCol_Text));
		ImGui::GetWindowDrawList()->AddText({Pos.x + std::max(0.f, Width - TextWidth), Pos.y}, TextColor, Text);

		if (OutOfRange)
		{
			ImGui::SetItemTooltip("Outside the Min..Max range");
		}

		if (Selected && ScrollToSelected)
		{
			if (!ImGui::IsRectVisible(ImGui::GetItemRectMin(), ImGui::GetItemRectMax()))
			{
				ImGui::SetScrollHereY(0.5f);
			}
			ScrollToSelected = false;
		}
	}

	ImGui::PopID();
}

void CEFDEditor::DrawList(SEvaluationFunction::SPattern& Pattern, float Height)
{
	const int ColumnCount = int(Pattern.Variables.size()) + 2;
	const ImGuiTableFlags Flags = ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_BordersOuter | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY | ImGuiTableFlags_Resizable;

	if (!ImGui::BeginTable("##EFDList", ColumnCount, Flags, ImVec2(0.f, Height)))
	{
		return;
	}

	const float Padding = ImGui::GetStyle().CellPadding.x * 2.f + ImGui::GetFontSize();
	ImGui::TableSetupScrollFreeze(0, 1);
	ImGui::TableSetupColumn("#", ImGuiTableColumnFlags_WidthFixed, ImGui::CalcTextSize("0000").x);
	for (u32 Index : Pattern.Variables)
	{
		const xr_string Name = GetVariableName(Function.Variables[Index]);
		ImGui::TableSetupColumn(Name.c_str(), ImGuiTableColumnFlags_WidthFixed, ImGui::CalcTextSize(Name.c_str()).x + Padding);
	}
	ImGui::TableSetupColumn("Value", ImGuiTableColumnFlags_WidthStretch);
	ImGui::TableHeadersRow();

	xr_vector<u32> Values;
	ImGuiListClipper Clipper;
	Clipper.Begin(int(Pattern.Weights.size()));
	if (ScrollToSelected && SelectedCell >= 0)
	{
		Clipper.IncludeItemByIndex(SelectedCell);
	}

	while (Clipper.Step())
	{
		for (int Row = Clipper.DisplayStart; Row < Clipper.DisplayEnd; ++Row)
		{
			Function.GetCombination(Pattern, u32(Row), Values);

			ImGui::TableNextRow();
			ImGui::TableNextColumn();
			ImGui::TextDisabled("%d", Row + 1);

			// The values are 1-based, as in the efc examples and the engine debug output
			for (u32 Value : Values)
			{
				ImGui::TableNextColumn();
				ImGui::Text("%u", Value + 1);
			}

			ImGui::TableNextColumn();
			DrawCell(Pattern, u32(Row));
		}
	}

	ImGui::EndTable();
}

void CEFDEditor::DrawGrid(SEvaluationFunction::SPattern& Pattern, float Height)
{
	const SEvaluationFunction::SVariable& RowVariable = Function.Variables[Pattern.Variables[0]];
	const SEvaluationFunction::SVariable& ColumnVariable = Function.Variables[Pattern.Variables[1]];
	const xr_string RowName = GetVariableName(RowVariable);
	const xr_string ColumnName = GetVariableName(ColumnVariable);

	const float MinCellWidth = ImGui::CalcTextSize("000.000").x + ImGui::GetStyle().CellPadding.x * 2.f;
	const float HeaderWidth = ImGui::CalcTextSize("0000").x;
	const bool Fits = HeaderWidth + MinCellWidth * float(ColumnVariable.Range) <= ImGui::GetContentRegionAvail().x;

	ImGuiTableFlags Flags = ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_BordersOuter | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY;
	Flags |= Fits ? ImGuiTableFlags_SizingStretchSame : (ImGuiTableFlags_ScrollX | ImGuiTableFlags_SizingFixedFit);

	if (!ImGui::BeginTable("##EFDGrid", int(ColumnVariable.Range) + 1, Flags, ImVec2(0.f, Height)))
	{
		return;
	}

	ImGui::TableSetupScrollFreeze(1, 1);
	ImGui::TableSetupColumn("##Rows", ImGuiTableColumnFlags_WidthFixed, HeaderWidth);
	for (u32 Column = 0; Column < ColumnVariable.Range; ++Column)
	{
		ImGui::TableSetupColumn(xr_string::ToString(Column + 1).c_str(), Fits ? ImGuiTableColumnFlags_WidthStretch : ImGuiTableColumnFlags_WidthFixed, Fits ? 0.f : MinCellWidth);
	}

	ImGui::TableNextRow(ImGuiTableRowFlags_Headers);
	ImGui::TableNextColumn();
	ImGui::TextDisabled(ICON_FA_ARROW_DOWN ICON_FA_ARROW_RIGHT);
	ImGui::SetItemTooltip("Rows: %s\nColumns: %s", RowName.c_str(), ColumnName.c_str());
	for (u32 Column = 0; Column < ColumnVariable.Range; ++Column)
	{
		ImGui::TableNextColumn();
		const xr_string Label = xr_string::ToString(Column + 1);
		const float Offset = ImGui::GetContentRegionAvail().x - ImGui::CalcTextSize(Label.c_str()).x;
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + std::max(0.f, Offset));
		ImGui::TableHeader(Label.c_str());
		ImGui::SetItemTooltip("%s = %u", ColumnName.c_str(), Column + 1);
	}

	ImGuiListClipper Clipper;
	Clipper.Begin(int(RowVariable.Range));
	if (ScrollToSelected && SelectedCell >= 0)
	{
		Clipper.IncludeItemByIndex(SelectedCell / int(ColumnVariable.Range));
	}

	while (Clipper.Step())
	{
		for (int Row = Clipper.DisplayStart; Row < Clipper.DisplayEnd; ++Row)
		{
			ImGui::TableNextRow();
			ImGui::TableNextColumn();
			ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImGui::GetColorU32(ImGuiCol_TableHeaderBg));
			ImGui::Text("%d", Row + 1);
			ImGui::SetItemTooltip("%s = %d", RowName.c_str(), Row + 1);

			for (u32 Column = 0; Column < ColumnVariable.Range; ++Column)
			{
				ImGui::TableNextColumn();
				DrawCell(Pattern, u32(Row) * ColumnVariable.Range + Column);
			}
		}
	}

	ImGui::EndTable();
}

void CEFDEditor::HandleKeyboard(SEvaluationFunction::SPattern& Pattern)
{
	if (EditingCell >= 0 || SelectedCell < 0 || !ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) || ImGui::GetIO().WantTextInput)
	{
		return;
	}

	const int Count = int(Pattern.Weights.size());
	const int Columns = IsGridView(Pattern) ? int(Function.Variables[Pattern.Variables[1]].Range) : 1;

	int Target = SelectedCell;
	if (ImGui::IsKeyPressed(ImGuiKey_DownArrow))
	{
		Target += Columns;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_UpArrow))
	{
		Target -= Columns;
	}
	else if (Columns > 1 && ImGui::IsKeyPressed(ImGuiKey_RightArrow) && SelectedCell % Columns + 1 < Columns)
	{
		Target += 1;
	}
	else if (Columns > 1 && ImGui::IsKeyPressed(ImGuiKey_LeftArrow) && SelectedCell % Columns > 0)
	{
		Target -= 1;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_Home))
	{
		Target = 0;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_End))
	{
		Target = Count - 1;
	}

	if (Target != SelectedCell && Target >= 0 && Target < Count)
	{
		SelectCell(Target);
		return;
	}

	if (ImGui::IsKeyPressed(ImGuiKey_Enter) || ImGui::IsKeyPressed(ImGuiKey_KeypadEnter) || ImGui::IsKeyPressed(ImGuiKey_F2))
	{
		BeginEdit(Pattern);
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_Delete) && Pattern.Weights[SelectedCell] != 0.f)
	{
		Pattern.Weights[SelectedCell] = 0.f;
		CommitUndo();
	}
	else if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_C))
	{
		string32 Text;
		xr_sprintf(Text, "%g", Pattern.Weights[SelectedCell]);
		ImGui::SetClipboardText(Text);
	}
	else if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_V))
	{
		xr_string Text = ImGui::GetClipboardText() != nullptr ? ImGui::GetClipboardText() : "";
		std::replace(Text.begin(), Text.end(), ',', '.');

		char* End = nullptr;
		const float Value = std::strtof(Text.c_str(), &End);
		if (End != Text.c_str())
		{
			Pattern.Weights[SelectedCell] = Value;
			CommitUndo();
		}
	}
}

void CEFDEditor::DrawStatusBar(const SEvaluationFunction::SPattern& Pattern)
{
	float MinWeight = FLT_MAX;
	float MaxWeight = -FLT_MAX;
	double Sum = 0.0;
	u32 OutOfRange = 0;
	for (float Weight : Pattern.Weights)
	{
		MinWeight = std::min(MinWeight, Weight);
		MaxWeight = std::max(MaxWeight, Weight);
		Sum += Weight;
		OutOfRange += (Weight < Function.MinValue || Weight > Function.MaxValue) ? 1 : 0;
	}

	string128 Stats;
	xr_sprintf(Stats, "min %.2f  avg %.2f  max %.2f", MinWeight, Pattern.Weights.empty() ? 0.0 : Sum / double(Pattern.Weights.size()), MaxWeight);

	string64 Warning = "";
	if (OutOfRange != 0)
	{
		xr_sprintf(Warning, ICON_FA_TRIANGLE_EXCLAMATION " %u out of range   ", OutOfRange);
	}

	xr_string Left;
	const bool HasSelection = SelectedCell >= 0;
	if (HasSelection)
	{
		xr_vector<u32> Values;
		Function.GetCombination(Pattern, u32(SelectedCell), Values);

		for (size_t Item = 0; Item < Values.size(); ++Item)
		{
			if (!Left.empty())
			{
				Left += ", ";
			}
			Left += GetVariableName(Function.Variables[Pattern.Variables[Item]]) + " " + xr_string::ToString(Values[Item] + 1);
		}

		string32 Value;
		xr_sprintf(Value, "  " ICON_FA_ARROW_RIGHT "  %.3f", Pattern.Weights[SelectedCell]);
		Left += Value;
	}
	else
	{
		Left = "Click a value to select it";
	}

	// The statistics keep their place, the left text is cut with an ellipsis before them
	const float RightWidth = ImGui::CalcTextSize(Warning).x + ImGui::CalcTextSize(Stats).x;
	const float LineStart = ImGui::GetCursorPosX();
	const float RightStart = std::max(LineStart, ImGui::GetWindowContentRegionMax().x - RightWidth);

	ImGui::AlignTextToFramePadding();
	const ImVec2 LeftPos = ImGui::GetCursorScreenPos();
	const float LeftMaxX = LeftPos.x + std::max(0.f, RightStart - LineStart - ImGui::GetFontSize());
	const ImVec2 LeftSize = {LeftMaxX - LeftPos.x, ImGui::GetTextLineHeight()};

	ImGui::PushStyleColor(ImGuiCol_Text, ImGui::GetStyleColorVec4(HasSelection ? ImGuiCol_Text : ImGuiCol_TextDisabled));
	ImGui::RenderTextEllipsis(ImGui::GetWindowDrawList(), LeftPos, {LeftMaxX, LeftPos.y + LeftSize.y}, LeftMaxX, Left.c_str(), nullptr, nullptr);
	ImGui::PopStyleColor();

	ImGui::Dummy(LeftSize);
	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("%s\nEnter, F2 or double-click: edit\nArrows, Home, End: move\nDelete: set to zero\nCtrl+C, Ctrl+V: copy and paste one value", Left.c_str());
	}

	ImGui::SameLine(RightStart, 0.f);
	if (OutOfRange != 0)
	{
		ImGui::TextColored(OutOfRangeColor, "%s", Warning);
		ImGui::SetItemTooltip("Values outside Min..Max: the engine uses them as they are");
		ImGui::SameLine(0.f, 0.f);
	}
	ImGui::TextDisabled("%s", Stats);
}

void CEFDEditor::CopyToClipboard(const SEvaluationFunction::SPattern& Pattern) const
{
	xr_string Text;
	for (u32 Index : Pattern.Variables)
	{
		Text += GetVariableName(Function.Variables[Index]) + "\t";
	}
	Text += "Value\n";

	xr_vector<u32> Values;
	string64 Number;
	for (u32 Row = 0; Row < u32(Pattern.Weights.size()); ++Row)
	{
		Function.GetCombination(Pattern, Row, Values);
		for (u32 Value : Values)
		{
			Text += xr_string::ToString(Value + 1) + "\t";
		}
		xr_sprintf(Number, "%g\n", Pattern.Weights[Row]);
		Text += Number;
	}

	ImGui::SetClipboardText(Text.c_str());
}

void CEFDEditor::PasteFromClipboard(SEvaluationFunction::SPattern& Pattern)
{
	const char* Text = ImGui::GetClipboardText();
	if (Text == nullptr)
	{
		return;
	}

	// The last cell of each line is the value, other lines are skipped
	xr_vector<float> Pasted;
	xr_string Cell;
	const char* Line = Text;
	while (*Line != 0)
	{
		const char* LineEnd = Line;
		while (*LineEnd != 0 && *LineEnd != '\n')
		{
			++LineEnd;
		}

		const char* CellBegin = LineEnd;
		while (CellBegin > Line && CellBegin[-1] != '\t')
		{
			--CellBegin;
		}

		Cell.assign(CellBegin, LineEnd);
		std::replace(Cell.begin(), Cell.end(), ',', '.');
		while (!Cell.empty() && std::isspace((unsigned char)Cell.back()))
		{
			Cell.pop_back();
		}

		char* NumberEnd = nullptr;
		const float Number = std::strtof(Cell.c_str(), &NumberEnd);
		if (!Cell.empty() && NumberEnd == Cell.c_str() + Cell.size())
		{
			Pasted.push_back(Number);
		}

		Line = *LineEnd != 0 ? LineEnd + 1 : LineEnd;
	}

	if (Pasted.empty())
	{
		return;
	}

	const size_t Count = std::min(Pasted.size(), Pattern.Weights.size());
	std::copy_n(Pasted.begin(), Count, Pattern.Weights.begin());
	CommitUndo();

	if (Pasted.size() != Pattern.Weights.size())
	{
		Msg("~ Evaluation function: pasted %zu of %zu values", Count, Pattern.Weights.size());
	}
}
