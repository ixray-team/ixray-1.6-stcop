#pragma once
#include "../../../xrECore/Editor/UndoJournal.h"

// gamedata\ai\*.efd, read by CPatternFunction::vfLoadEF in xrGame
struct SEvaluationFunction
{
	static constexpr u32 BuilderVersion = 1;

	struct SPattern
	{
		// Indexes into Variables, the first one is the most significant
		xr_vector<u32> Variables;
		xr_vector<float> Weights;
	};

	struct SVariable
	{
		u32 Type = 0;
		u32 Range = 0;
	};

	u32 DataFormat = 0;
	u32 FunctionType = 0;
	float MinValue = 0.f;
	float MaxValue = 0.f;
	xr_vector<SVariable> Variables;
	xr_vector<SPattern> Patterns;

	bool Load(IReader& Reader, xr_string& Error);
	void Save(IWriter& Writer) const;

	u32 GetCombinationCount(const SPattern& Pattern) const;
	void GetCombination(const SPattern& Pattern, u32 Index, xr_vector<u32>& Values) const;

	// Names from efc.ini
	static const char* GetFunctionName(u32 Type);
};

class CEFDEditor : public IEditorWnd
{
public:
	static void Open(const xr_path& Path);
	static CEFDEditor* GetFocused();

	bool Undo();
	bool Redo();

	~CEFDEditor() override;
	void Draw() override;

private:
	enum class EViewMode : u8
	{
		List,
		Grid
	};

	explicit CEFDEditor(const xr_path& Path);

	bool Load();
	bool Save();

	void CaptureState(CUndoJournal::State& Parts) const;
	void CommitUndo();
	void Restore(const CUndoJournal::State& Parts);
	void UpdateModified();

	void DrawToolbar(SEvaluationFunction::SPattern& Pattern);
	void BeginToolbarGroup(int Index);
	void EndToolbarGroup(int Index);
	void DrawPattern(SEvaluationFunction::SPattern& Pattern);
	void DrawList(SEvaluationFunction::SPattern& Pattern, float Height);
	void DrawGrid(SEvaluationFunction::SPattern& Pattern, float Height);
	void DrawCell(SEvaluationFunction::SPattern& Pattern, u32 Index);
	void DrawStatusBar(const SEvaluationFunction::SPattern& Pattern);
	void HandleKeyboard(SEvaluationFunction::SPattern& Pattern);
	bool IsGridView(const SEvaluationFunction::SPattern& Pattern) const;

	void SelectCell(int Index);
	void BeginEdit(const SEvaluationFunction::SPattern& Pattern);

	void CopyToClipboard(const SEvaluationFunction::SPattern& Pattern) const;
	void PasteFromClipboard(SEvaluationFunction::SPattern& Pattern);

private:
	static xr_vector<CEFDEditor*> Opened;

	xr_path Path;
	xr_string Title;
	xr_string Error;
	SEvaluationFunction Function;
	CUndoJournal Journal;
	xr_vector<u8> SavedData;

	int SelectedCell = -1;
	int EditingCell = -1;
	float EditValue = 0.f;
	bool EditFocus = false;
	bool ScrollToSelected = false;
	size_t ActivePattern = 0;
	float ToolbarGroupWidths[6] = {};

	EViewMode ViewMode = EViewMode::Grid;
	bool HeatMap = true;
	bool Modified = false;
	bool NeedFocus = true;
	bool Focused = false;
};
