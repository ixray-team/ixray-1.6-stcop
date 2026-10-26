#include "stdafx.h"

CWorldPartition GWorldPartition;

static Fvector StreamCameraPosition()
{
	// In PIE the game camera drives the device camera, the editor view stays where it was
	if (Scene->IsPlayInEditor())
	{
		return EDevice->vCameraPosition;
	}
	return EContext.UI->CurrentView().m_Camera.GetPosition();
}

static bool ObjectCenter(CCustomObject* Object, Fvector& Center)
{
	Fbox Box;
	if (Object->GetBox(Box))
	{
		Box.getcenter(Center);
		return true;
	}

	Center = Object->GetPosition();
	return true;
}

static bool IsWorldPointHidden(const Fvector& WorldPoint)
{
	return GWorldPartition.IsPointHidden(WorldPoint);
}

bool CWorldPartition::IsPointHidden(const Fvector& Position) const
{
	if (IsPointDisabled(Position))
	{
		return true;
	}

	if (!StreamAroundCamera)
	{
		return false;
	}

	const Fvector Camera = StreamCameraPosition();
	return _sqr(Position.x - Camera.x) + _sqr(Position.z - Camera.z) > _sqr(StreamRadius);
}

void CWorldPartition::SetEnabled(bool Value)
{
	if (Enabled == Value)
	{
		return;
	}

	Enabled = Value;
	ScanNeeded = true;
	Dirty = true;
}

bool CWorldPartition::IsPointDisabled(const Fvector& Position) const
{
	s32 X, Z;
	CellOf(Position, X, Z);
	return DisabledCells.contains(MakeKey(X, Z));
}

CWorldPartition::CWorldPartition()
{
	CellSize = 100.0f;
	Bounds.invalidate();
	Dirty = true;
	LastObjectCount = -1;
}

void CWorldPartition::SetCellSize(float Size)
{
	Size = std::max(Size, 8.0f);
	if (Size == CellSize)
	{
		return;
	}

	// Cell keys depend on the size, so the old disabled set is meaningless
	CellSize = Size;
	DisabledCells.clear();
	Dirty = true;
}

void CWorldPartition::CellOf(const Fvector& Position, s32& X, s32& Z) const
{
	X = iFloor(Position.x / CellSize);
	Z = iFloor(Position.z / CellSize);
}

void CWorldPartition::CellBox(s32 X, s32 Z, Fbox& Box) const
{
	Box.min.set(X * CellSize, HasBounds() ? Bounds.min.y : -10.0f, Z * CellSize);
	Box.max.set((X + 1) * CellSize, HasBounds() ? Bounds.max.y : 10.0f, (Z + 1) * CellSize);
}

void CWorldPartition::SetCellDisabled(u64 Key, bool Disabled)
{
	if (Disabled)
	{
		DisabledCells.insert(Key);
	}
	else
	{
		DisabledCells.erase(Key);
	}
	Dirty = true;
}

void CWorldPartition::EnableAll()
{
	DisabledCells.clear();
	Dirty = true;
}

void CWorldPartition::Update()
{
	PROF_EVENT("CWorldPartition::Update")
	if (!Scene)
	{
		return;
	}

	if (Dirty)
	{
		ScanNeeded = true;
	}
	UpdateStreaming();

	// Streaming changes boxes every frame; the full pass waits until loading settles
	FramesSinceApply++;
	if (StreamChanged && (PendingCount == 0 || FramesSinceApply > 30))
	{
		StreamChanged = false;
		FramesSinceApply = 0;
		Dirty = true;
	}

	const int Count = Scene->ObjCount();
	if (Dirty || Count != LastObjectCount)
	{
		Apply();
	}
}

bool CWorldPartition::IsPositionActive(const Fvector& Position, float RadiusScale) const
{
	if (!Enabled)
	{
		return true;
	}

	if (UnloadDisabled && IsPointDisabled(Position))
	{
		return false;
	}

	if (StreamAroundCamera)
	{
		const Fvector Camera = StreamCameraPosition();
		const float DX = Position.x - Camera.x;
		const float DZ = Position.z - Camera.z;
		const float Radius = StreamRadius * RadiusScale;
		if (DX * DX + DZ * DZ > Radius * Radius)
		{
			return false;
		}
	}
	return true;
}

bool CWorldPartition::IsCellStreamedOut(u64 Key) const
{
	if (!StreamAroundCamera)
	{
		return false;
	}

	Fbox Box;
	CellBox(KeyX(Key), KeyZ(Key), Box);
	Fvector Center;
	Box.getcenter(Center);
	return !IsPositionActive(Center, 1.0f) && !IsCellDisabled(Key);
}

bool CWorldPartition::IsLargeBox(const Fbox& Box) const
{
	return (Box.max.x - Box.min.x) > CellSize * 2.0f || (Box.max.z - Box.min.z) > CellSize * 2.0f;
}

// Same ids as EditObject.h / EditMeshIO.cpp
static constexpr u32 ObjectBodyChunk = 0x7777;
static constexpr u32 ObjectMeshesChunk = 0x0910;
static constexpr u32 MeshBoxChunk = 0x1004;

static bool ReadObjectFileBounds(const char* Reference, Fbox& Box)
{
	string_path FileName;
	FS.update_path(FileName, _objects_, EFS.ChangeFileExt(Reference, ".object").c_str());
	if (!FS.exist(FileName))
	{
		return false;
	}

	IReader* File = FS.r_open(FileName);
	if (!File)
	{
		return false;
	}

	Box.invalidate();
	if (IReader* Body = File->open_chunk(ObjectBodyChunk))
	{
		if (IReader* Meshes = Body->open_chunk(ObjectMeshesChunk))
		{
			for (u32 Index = 0;; Index++)
			{
				IReader* Mesh = Meshes->open_chunk(Index);
				if (!Mesh)
				{
					break;
				}

				Fbox MeshBox;
				if (Mesh->find_chunk(MeshBoxChunk) && Mesh->elapsed() >= intptr_t(sizeof(Fbox)))
				{
					Mesh->r(&MeshBox, sizeof(Fbox));
					Box.merge(MeshBox);
				}
				Mesh->close();
			}
			Meshes->close();
		}
		Body->close();
	}
	FS.r_close(File);

	return Box.min.x <= Box.max.x;
}

bool CWorldPartition::PeekReferenceBounds(const char* Reference, Fbox& Box)
{
	auto It = ReferenceBounds.find(Reference);
	if (It == ReferenceBounds.end())
	{
		Fbox Read;
		if (!ReadObjectFileBounds(Reference, Read))
		{
			Read.invalidate();
		}
		It = ReferenceBounds.emplace(Reference, Read).first;
	}

	Box = It->second;
	return Box.min.x <= Box.max.x;
}

bool CWorldPartition::ShouldDeferReference(const char* Reference, const Fmatrix& Transform)
{
	if (!Enabled || (!UnloadDisabled && !StreamAroundCamera))
	{
		return false;
	}

	// Unknown size: loading is the safe choice
	Fbox Box;
	if (!PeekReferenceBounds(Reference, Box))
	{
		return false;
	}

	Box.xform(Transform);
	if (IsLargeBox(Box))
	{
		return false;
	}

	Fvector Center;
	Box.getcenter(Center);
	return !IsPositionActive(Center, 1.0f);
}

void CWorldPartition::UpdateStreaming()
{
	PROF_EVENT("CWorldPartition::UpdateStreaming")
	// A build keeps every object loaded, even while its progress bar draws frames
	if (EContext.UI->ContainEState(esBuildLevel))
	{
		return;
	}

	ESceneCustomOTool* Tool = Scene->GetOTool(OBJCLASS_SCENEOBJECT);
	if (!Tool)
	{
		return;
	}

	// Settings changes are picked up by the periodic pass
	const Fvector CameraNow = StreamCameraPosition();
	const bool CameraMoved = _sqr(CameraNow.x - LastScanCamera.x) + _sqr(CameraNow.z - LastScanCamera.z) > _sqr(StreamRadius * 0.02f);
	if (!ScanNeeded && !(StreamAroundCamera && CameraMoved) && ++FramesSinceScan < 60)
	{
		return;
	}

	ScanNeeded = false;
	FramesSinceScan = 0;
	LastScanCamera = CameraNow;

	struct SCandidate
	{
		CSceneObject* Object;
		float Distance;
	};
	static xr_vector<SCandidate> ToLoad;
	ToLoad.clear();

	const Fvector Camera = StreamCameraPosition();
	PendingCount = 0;
	StaticCount = 0;

	for (CCustomObject* Custom : Tool->GetObjects())
	{
		CSceneObject* Object = static_cast<CSceneObject*>(Custom);
		StaticCount++;

		Fvector Center = Object->GetPosition();
		Fbox Box;
		if (Object->GetStreamBox(Box))
		{
			// Objects spanning several cells (terrain) hide per chunk and stay loaded
			if (IsLargeBox(Box))
			{
				if (Object->IsReferencePending())
				{
					Object->StreamIn();
					StreamChanged = true;
				}
				continue;
			}
			Box.getcenter(Center);
		}
		if (CCustomObject* Owner = Object->GetOwner())
		{
			Center = Owner->GetPosition();
		}

		if (Object->IsReferencePending())
		{
			PendingCount++;
			if (IsPositionActive(Center, 1.0f))
			{
				ToLoad.push_back({Object, (Center.x - Camera.x) * (Center.x - Camera.x) + (Center.z - Camera.z) * (Center.z - Camera.z)});
			}
		}
		else if (!Object->Selected() && !IsPositionActive(Center, 1.2f))
		{
			// A wider radius for unloading keeps objects on the edge from reloading every frame
			Object->StreamOut();
			PendingCount++;
			StreamChanged = true;
		}
	}

	if (ToLoad.empty())
	{
		return;
	}

	std::sort(ToLoad.begin(), ToLoad.end(), [](const SCandidate& A, const SCandidate& B)
			  { return A.Distance < B.Distance; });

	CTimer Timer;
	Timer.Start();
	for (const SCandidate& Candidate : ToLoad)
	{
		Candidate.Object->StreamIn();
		PendingCount--;
		StreamChanged = true;

		if (Timer.GetElapsed_sec() * 1000.0f > StreamBudget)
		{
			// The rest loads in the next frames
			ScanNeeded = true;
			break;
		}
	}

	EContext.UI->RedrawScene();
}

static xr_string LevelStateFile(const char* PartPath)
{
	return xr_string(PartPath) + "world_partition.ltx";
}

void CWorldPartition::LoadLevelState(const char* PartPath)
{
	DisabledCells.clear();
	Dirty = true;

	const xr_string FileName = LevelStateFile(PartPath);
	if (!FS.exist(FileName.c_str()))
	{
		return;
	}

	CInifile Ini(FileName.c_str());
	if (Ini.line_exist("world_partition", "cell_size"))
	{
		CellSize = std::max(Ini.r_float("world_partition", "cell_size"), 8.0f);
	}

	if (!Ini.section_exist("disabled_cells"))
	{
		return;
	}

	const u32 Count = Ini.r_u32("disabled_cells", "count");
	for (u32 i = 0; i < Count; i++)
	{
		string32 Key;
		xr_sprintf(Key, "cell_%u", i);
		if (!Ini.line_exist("disabled_cells", Key))
		{
			continue;
		}

		s32 X = 0, Z = 0;
		if (sscanf(Ini.r_string("disabled_cells", Key), "%d,%d", &X, &Z) == 2)
		{
			DisabledCells.insert(MakeKey(X, Z));
		}
	}
}

void CWorldPartition::SaveLevelState(const char* PartPath) const
{
	const xr_string FileName = LevelStateFile(PartPath);
	CInifile Ini(FileName.c_str(), false, false, false);

	Ini.w_float("world_partition", "cell_size", CellSize);
	Ini.w_u32("disabled_cells", "count", u32(DisabledCells.size()));

	u32 Index = 0;
	for (u64 CellKey : DisabledCells)
	{
		string32 Key;
		string64 Value;
		xr_sprintf(Key, "cell_%u", Index++);
		xr_sprintf(Value, "%d,%d", KeyX(CellKey), KeyZ(CellKey));
		Ini.w_string("disabled_cells", Key, Value);
	}

	Ini.save_as();
}

void CWorldPartition::StreamInAll()
{
	PROF_EVENT("CWorldPartition::StreamInAll")
	// Loading references changes the static objects a backup may be writing
	g_LevelAutoBackup.Cancel();
	ESceneCustomOTool* Tool = Scene ? Scene->GetOTool(OBJCLASS_SCENEOBJECT) : nullptr;
	if (!Tool)
	{
		return;
	}

	u32 Count = 0;
	for (CCustomObject* Custom : Tool->GetObjects())
	{
		Count += static_cast<CSceneObject*>(Custom)->IsReferencePending() ? 1 : 0;
	}

	if (Count == 0)
	{
		return;
	}

	SPBItem* Progress = EContext.UI->ProgressStart(float(Count), "Loading streamed out objects...");
	for (CCustomObject* Custom : Tool->GetObjects())
	{
		CSceneObject* Object = static_cast<CSceneObject*>(Custom);
		if (Object->IsReferencePending())
		{
			Object->StreamIn();
			Progress->Inc(Object->m_ReferenceName.c_str());
		}
	}
	EContext.UI->ProgressEnd(Progress);

	PendingCount = 0;
	Dirty = true;
}

bool CWorldPartition::CollectActive(xr_vector<CCustomObject*>& Out) const
{
	PROF_EVENT("CWorldPartition::CollectActive")
	if (!Enabled || Dirty || !Scene || Scene->ObjCount() != LastObjectCount)
	{
		return false;
	}

	Out.insert(Out.end(), Spanning.begin(), Spanning.end());

	const Fvector Camera = StreamCameraPosition();
	// Objects are binned by their center: a cell farther than the radius by half a cell
	// diagonal can still hold objects reaching into the circle
	const float Reach = StreamRadius + CellSize * 0.75f;

	for (const auto& [Key, Cell] : Cells)
	{
		if (Cell.List.empty() || DisabledCells.contains(Key))
		{
			continue;
		}

		if (StreamAroundCamera)
		{
			const float CX = (float(KeyX(Key)) + 0.5f) * CellSize;
			const float CZ = (float(KeyZ(Key)) + 0.5f) * CellSize;
			if (_sqr(CX - Camera.x) + _sqr(CZ - Camera.z) > _sqr(Reach))
			{
				continue;
			}
		}

		Out.insert(Out.end(), Cell.List.begin(), Cell.List.end());
	}
	return true;
}

bool CWorldPartition::CollectAlongRay(const Fvector& Start, const Fvector& Direction, float Distance, xr_vector<CCustomObject*>& Out) const
{
	PROF_EVENT("CWorldPartition::CollectAlongRay")
	if (!Enabled || Dirty || !Scene || Scene->ObjCount() != LastObjectCount)
	{
		return false;
	}

	Out.insert(Out.end(), Spanning.begin(), Spanning.end());

	// XZ rectangle of the segment in cells; one cell of margin, since an object binned by its
	// center reaches up to a cell further (larger ones are spanning)
	Fvector End;
	End.mad(Start, Direction, Distance);
	s32 X0, Z0, X1, Z1;
	CellOf(Start, X0, Z0);
	CellOf(End, X1, Z1);
	if (X0 > X1)
	{
		std::swap(X0, X1);
	}
	if (Z0 > Z1)
	{
		std::swap(Z0, Z1);
	}
	X0--;
	Z0--;
	X1++;
	Z1++;

	const s64 Area = s64(X1 - X0 + 1) * s64(Z1 - Z0 + 1);
	if (Area <= s64(Cells.size()))
	{
		for (s32 Z = Z0; Z <= Z1; Z++)
		{
			for (s32 X = X0; X <= X1; X++)
			{
				auto It = Cells.find(MakeKey(X, Z));
				if (It != Cells.end())
				{
					Out.insert(Out.end(), It->second.List.begin(), It->second.List.end());
				}
			}
		}
		return true;
	}

	for (const auto& [Key, Cell] : Cells)
	{
		const s32 X = KeyX(Key);
		const s32 Z = KeyZ(Key);
		if (X >= X0 && X <= X1 && Z >= Z0 && Z <= Z1)
		{
			Out.insert(Out.end(), Cell.List.begin(), Cell.List.end());
		}
	}
	return true;
}

void CWorldPartition::Apply()
{
	PROF_EVENT("CWorldPartition::Apply")
	Dirty = false;
	Cells.clear();
	Spanning.clear();
	Bounds.invalidate();

	if (!Scene)
	{
		return;
	}

	LastObjectCount = Scene->ObjCount();

	for (SceneToolsMapPairIt it = Scene->FirstTool(); it != Scene->LastTool(); ++it)
	{
		ESceneCustomOTool* Tool = smart_cast<ESceneCustomOTool*>(it->second);
		if (!Tool || it->first == OBJCLASS_DUMMY)
		{
			continue;
		}

		for (CCustomObject* Object : Tool->GetObjects())
		{
			Fvector Center;
			ObjectCenter(Object, Center);

			// Grouped objects follow their group so a group is never split
			if (CCustomObject* Owner = Object->GetOwner())
			{
				ObjectCenter(Owner, Center);
			}

			s32 X, Z;
			CellOf(Center, X, Z);
			const u64 Key = MakeKey(X, Z);

			CellInfo& Cell = Cells[Key];
			Cell.Objects++;
			if (CSceneObject* Static = smart_cast<CSceneObject*>(Object); Static && Static->IsReferencePending())
			{
				Cell.Unloaded++;
			}

			Fbox Box;
			const bool HasBox = Object->GetBox(Box);
			if (HasBox)
			{
				Bounds.merge(Box);
			}
			else
			{
				Bounds.modify(Center);
			}

			// Objects spanning several cells (terrain) stay active: their meshes hide per chunk
			const bool IsSpanning = HasBox && IsLargeBox(Box);
			Object->SetRegionDisabled(Enabled && !IsSpanning && DisabledCells.contains(Key));

			if (IsSpanning)
			{
				Spanning.push_back(Object);
			}
			else
			{
				Cell.List.push_back(Object);
			}
		}
	}

	CEditableMesh::RegionHidden = Enabled ? &IsWorldPointHidden : nullptr;
	CEditableMesh::CullChunksByView = !Enabled;

	UIObjectList::Refresh();
	EContext.UI->RedrawScene();
}
