#pragma once
class CCustomObject;
class CGroupObject;

// Scene outliner: objects grouped into per-class folders, groups expand to their members
class UIObjectList:
	public IEditorWnd
{
public:
	UIObjectList();
	virtual ~UIObjectList();
	virtual void Draw();
	static void Update();
	static void Show();
	static void Close();
	static IC bool IsOpen()  { return Form; }
	static void Refresh();
	// Keys the outliner handles itself while focused, so editor shortcuts skip them
	static bool ConsumesShortcut(u32 Key);

private:
	struct Node
	{
		CCustomObject* Object = nullptr;
		xr_vector<Node> Children;
	};

	struct Folder
	{
		ObjClassID ClassID = OBJCLASS_DUMMY;
		xr_string Name;
		xr_vector<Node> Nodes;
	};

	struct Row
	{
		Folder* OwnerFolder = nullptr;
		Node* ObjectNode = nullptr;
		int Depth = 0;
	};

	enum class EVisibilityFilter
	{
		All,
		Visible,
		Hidden
	};

	void Rebuild();
	void DrawToolbar();
	void DrawTree();
	void DrawFolderRow(Folder& F, bool Filtering);
	void DrawObjectRow(const Row& R, int RowIndex, bool Filtering);
	void DrawContextMenu();
	void BuildRows(bool Filtering);
	void AppendNodeRows(Folder& F, Node& N, int Depth, bool Filtering);

	bool PassesFilter(CCustomObject* Object) const;
	bool NodePasses(const Node& N) const;
	bool FolderPasses(const Folder& F) const;

	void ClickObject(CCustomObject* Object, int RowIndex);
	void FocusSelected();
	void ShowSelected(bool Flag);
	void LockSelected(bool Flag);
	static void ShowNode(Node& N, bool Flag);
	static void LockNode(Node& N, bool Flag);
	void TrackExternalSelection();
	void ScrollToCurrentRow();

	bool IsRowOpen(const Row& R, bool Filtering) const;
	bool HasRowChildren(const Row& R) const;
	void SetRowOpen(const Row& R, bool Open);
	void MoveCursor(int NewRow, bool Extend);
	void HandleKeyboard(bool Filtering);

	void BeginRename(CCustomObject* Object);
	void DrawRenameField(float OffsetX);
	void CommitRename();

	static void CollectSelected(const Node& N, ObjectList& List);
	void DrawEmptySpaceDropTarget();
	void DropSelection(CGroupObject* Target);

	xr_vector<Folder> Folders;
	xr_vector<Row> Rows;
	xr_hash_set<ObjClassID> CollapsedFolders;
	xr_hash_set<CCustomObject*> ExpandedNodes;

	EVisibilityFilter VisibilityFilter;
	bool OnlyCurrentTool;
	ObjClassID CurrentClass;
	string_path Filter;

	int AnchorRow;
	int CursorRow;
	CCustomObject* ScrollTarget;
	bool ScrollCentered;
	CCustomObject* RenameObject;
	bool RenameFocus;
	string256 RenameBuffer;
	CCustomObject* LastSingleSelection;
	bool NeedRebuild;
	xrCriticalSection LoaderCS;

	static UIObjectList* Form;
};
