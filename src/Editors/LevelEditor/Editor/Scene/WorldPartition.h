#pragma once

// Editor-only world partition: the level is split into a square XZ grid and
// cells can be disabled. Objects of a disabled cell are not rendered, picked
// or selected. Nothing is saved to the level.
class CWorldPartition
{
public:
	struct CellInfo
	{
		u32 Objects = 0;
		// Static objects whose library object is streamed out
		u32 Unloaded = 0;
		// Every object whose center lies in the cell; rebuilt by Apply
		xr_vector<CCustomObject*> List;
	};

	CWorldPartition();

	float GetCellSize() const { return CellSize; }
	void SetCellSize(float Size);

	static u64 MakeKey(s32 X, s32 Z) { return (u64(u32(X)) << 32) | u64(u32(Z)); }
	static s32 KeyX(u64 Key) { return s32(u32(Key >> 32)); }
	static s32 KeyZ(u64 Key) { return s32(u32(Key)); }

	void CellOf(const Fvector& Position, s32& X, s32& Z) const;
	void CellBox(s32 X, s32 Z, Fbox& Box) const;

	bool IsCellDisabled(u64 Key) const { return DisabledCells.contains(Key); }
	bool IsPointDisabled(const Fvector& Position) const;
	// Disabled cell or outside the streaming radius: chunks of large meshes there hide
	bool IsPointHidden(const Fvector& Position) const;
	void SetCellDisabled(u64 Key, bool Disabled);
	void EnableAll();
	bool HasDisabledCells() const { return !DisabledCells.empty(); }

	// Objects of the cells that can be seen: not disabled and, when streaming around the camera,
	// within the streaming radius, plus objects spanning several cells. False when the lists are
	// stale or the partition is off: the caller walks every object.
	bool CollectActive(xr_vector<CCustomObject*>& Out) const;
	// Objects of the cells a ray segment crosses, plus spanning objects. Same fallback rule.
	bool CollectAlongRay(const Fvector& Start, const Fvector& Direction, float Distance, xr_vector<CCustomObject*>& Out) const;

	// Recomputes the cell of every object and applies the disabled state
	void Apply();
	// Applies again when the scene changed since the last pass
	void Update();
	void MarkDirty() { Dirty = true; }

	// Off: no disabled cells and no streaming, every object loads and large meshes are culled
	// per chunk by the camera view. On: chunks outside the cells and the streaming radius hide.
	bool IsEnabled() const { return Enabled; }
	void SetEnabled(bool Value);

	// Streaming of static object references (library .object files)
	bool UnloadDisabled = true;
	bool StreamAroundCamera = false;
	float StreamRadius = 300.0f;
	// Milliseconds per frame spent loading references
	float StreamBudget = 8.0f;

	// True when a static object should not load its reference yet. The bounds come from the
	// mesh headers of the .object file, so terrain placed at the origin still loads.
	bool ShouldDeferReference(const char* Reference, const Fmatrix& Transform);
	// Local bounds of a library object without loading it; false when the file cannot be read
	bool PeekReferenceBounds(const char* Reference, Fbox& Box);
	// Spans several cells: hides per chunk and is never streamed out
	bool IsLargeBox(const Fbox& Box) const;
	// True when the cell lies outside the camera streaming radius
	bool IsCellStreamedOut(u64 Key) const;
	// Loads every streamed out reference, for building and exporting the full level
	void StreamInAll();
	u32 GetPendingCount() const { return PendingCount; }

	// Per-level state (cell size, disabled cells) next to the level parts.
	// Loaded before the objects so streaming applies at load time.
	void LoadLevelState(const char* PartPath);
	void SaveLevelState(const char* PartPath) const;
	u32 GetStaticCount() const { return StaticCount; }

	const xr_hash_map<u64, CellInfo>& GetCells() const { return Cells; }
	const Fbox& GetBounds() const { return Bounds; }
	bool HasBounds() const { return Bounds.min.x <= Bounds.max.x; }

private:
	bool IsPositionActive(const Fvector& Position, float RadiusScale) const;
	void UpdateStreaming();

	u32 PendingCount = 0;
	u32 StaticCount = 0;
	u32 FramesSinceApply = 0;
	// The pass over every static object runs only when its result can differ
	Fvector LastScanCamera = { flt_max, flt_max, flt_max };
	u32 FramesSinceScan = 0;
	bool ScanNeeded = true;
	bool StreamChanged = false;

	bool Enabled = true;
	float CellSize;
	xr_hash_set<u64> DisabledCells;
	xr_hash_map<u64, CellInfo> Cells;
	xr_vector<CCustomObject*> Spanning;
	// Bounds read from .object headers this session; invalid box when the file has none
	xr_hash_map<xr_string, Fbox> ReferenceBounds;
	Fbox Bounds;
	bool Dirty;
	int LastObjectCount;
};

extern CWorldPartition GWorldPartition;
