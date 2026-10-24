#pragma once

// Top-down map of the world partition grid: select cells and disable or enable them
class UIWorldPartition:
	public IEditorWnd
{
public:
	UIWorldPartition();
	virtual ~UIWorldPartition();
	virtual void Draw();
	static void Update();
	static void Show();
	static void Close();
	static IC bool IsOpen() { return Form; }

private:
	void DrawToolbar();
	void DrawCanvas();
	void DrawContextMenu();

	void FitView();
	void SetSelectedDisabled(bool Disabled);
	void IsolateSelected();
	void FocusCell(u64 Key);

	ImVec2 WorldToScreen(float X, float Z) const;
	void ScreenToWorld(const ImVec2& Pos, float& X, float& Z) const;
	u64 CellAtScreen(const ImVec2& Pos) const;

	xr_hash_set<u64> SelectedCells;
	float ViewX;
	float ViewZ;
	float Zoom;
	ImVec2 CanvasMin;
	ImVec2 CanvasSize;
	bool NeedFit;

	bool RectSelecting;
	ImVec2 RectStart;
	float EditCellSize;
	bool EditingCellSize = false;

	static UIWorldPartition* Form;
};
