#include "stdafx.h"

// Object tools: an unnamed part for the tool data and a part per object, so an undo recreates only
// the changed objects. The level options and the snap list are keyed by OBJCLASS_DUMMY.
static constexpr u32 UndoLevelKey = u32(OBJCLASS_DUMMY);

enum ESceneUndoChunks : u32
{
	UndoChunkLevelOptions,
	UndoChunkSnapObjects,
};

static bool IsUndoTool(ObjClassID ID, ESceneToolBase* Tool)
{
	return ID != OBJCLASS_DUMMY && Tool != nullptr && Tool->IsEnabled() && Tool->IsEditable() && Tool->IsNeedSave();
}

static bool IsUndoObject(CCustomObject* Object)
{
	return !Object->IsDeleted() && !Object->m_CO_Flags.test(CCustomObject::flObjectInGroup);
}

// Pointers to recreated objects kept by other tools: replaced by names while the objects are
// recreated, then found again
class CSceneUndoLinks
{
public:
	void Unlink(EScene& Scene, SceneToolsMap& Tools, const xr_hash_set<CCustomObject*>& Objects)
	{
		for (auto& [ToolID, Tool] : Tools)
		{
			ObjectList* SnapList = Tool != nullptr ? Tool->GetSnapList() : nullptr;
			if (SnapList == nullptr || std::none_of(SnapList->begin(), SnapList->end(), [&Objects](CCustomObject* O) { return Objects.contains(O); }))
			{
				continue;
			}

			SSnapList& Saved = SnapLists.emplace_back();
			Saved.List = SnapList;
			for (CCustomObject* O : *SnapList)
			{
				Saved.Names.push_back(O->GetName());
			}
			SnapList->clear();
		}

		if (ESceneCustomOTool* SectorTool = Scene.GetOTool(OBJCLASS_SECTOR))
		{
			for (CCustomObject* O : SectorTool->GetObjects())
			{
				CSector* Sector = static_cast<CSector*>(O);
				if (!Objects.contains(Sector) && Sector->UnlinkObjects(Objects))
				{
					Sectors.push_back(Sector);
				}
			}
		}

		if (ESceneCustomOTool* PortalTool = Scene.GetOTool(OBJCLASS_PORTAL))
		{
			for (CCustomObject* O : PortalTool->GetObjects())
			{
				CPortal* Portal = static_cast<CPortal*>(O);
				CSector* Front = Portal->SectorFront();
				CSector* Back = Portal->SectorBack();
				const bool UnlinkFront = Front != nullptr && Objects.contains(Front);
				const bool UnlinkBack = Back != nullptr && Objects.contains(Back);
				if (Objects.contains(Portal) || (!UnlinkFront && !UnlinkBack))
				{
					continue;
				}

				SPortal& Saved = Portals.emplace_back();
				Saved.Portal = Portal;
				if (UnlinkFront)
				{
					Saved.Front = Front->GetName();
					Front = nullptr;
				}
				if (UnlinkBack)
				{
					Saved.Back = Back->GetName();
					Back = nullptr;
				}
				Portal->SetSectors(Front, Back);
			}
		}
	}

	void Relink(EScene& Scene)
	{
		for (SSnapList& Saved : SnapLists)
		{
			for (const shared_str& Name : Saved.Names)
			{
				if (CCustomObject* O = Scene.FindObjectByName(Name.c_str(), OBJCLASS_SCENEOBJECT))
				{
					Saved.List->push_back(O);
				}
			}
		}

		for (CSector* Sector : Sectors)
		{
			Sector->RelinkObjects();
		}

		for (SPortal& Saved : Portals)
		{
			CSector* Front = Saved.Portal->SectorFront();
			CSector* Back = Saved.Portal->SectorBack();
			if (Saved.Front.size() != 0)
			{
				Front = smart_cast<CSector*>(Scene.FindObjectByName(Saved.Front.c_str(), OBJCLASS_SECTOR));
			}
			if (Saved.Back.size() != 0)
			{
				Back = smart_cast<CSector*>(Scene.FindObjectByName(Saved.Back.c_str(), OBJCLASS_SECTOR));
			}
			Saved.Portal->SetSectors(Front, Back);
			if (Front == nullptr || Back == nullptr)
			{
				Msg("! Portal '%s' lost its sector after undo", Saved.Portal->GetName());
			}
		}
	}

private:
	struct SSnapList
	{
		ObjectList* List = nullptr;
		xr_vector<shared_str> Names;
	};

	struct SPortal
	{
		CPortal* Portal = nullptr;
		shared_str Front;
		shared_str Back;
	};

	xr_vector<SSnapList> SnapLists;
	xr_vector<CSector*> Sectors;
	xr_vector<SPortal> Portals;
};

void EScene::UndoClear()
{
	UndoJournal.Clear();
}

void EScene::UndoSave()
{
	if (EContext.UI->GetEState() != esEditScene)
	{
		return;
	}

	Modified();
	EContext.UI->RedrawScene();

	if (0 == EPrefs->scene_undo_level)
	{
		return;
	}

	CUndoJournal::State Parts;
	CaptureUndoState(Parts);

	if (UndoJournal.IsEmpty())
	{
		UndoJournal.Reset(std::move(Parts));
	}
	else
	{
		UndoJournal.Commit(std::move(Parts), true);
	}
}

bool EScene::Undo()
{
	CUndoJournal::State Parts;
	if (!UndoJournal.Undo(Parts))
	{
		return false;
	}

	RestoreUndoState(Parts);
	Modified();
	return true;
}

bool EScene::Redo()
{
	CUndoJournal::State Parts;
	if (!UndoJournal.Redo(Parts))
	{
		return false;
	}

	RestoreUndoState(Parts);
	Modified();
	return true;
}

void EScene::CaptureUndoState(CUndoJournal::State& Parts)
{
	CMemoryWriter Writer;

	Writer.open_chunk(UndoChunkLevelOptions);
	m_LevelOp.Save(Writer);
	Writer.close_chunk();

	Writer.open_chunk(UndoChunkSnapObjects);
	Writer.w_u32((u32)m_ESO_SnapObjects.size());
	for (CCustomObject* Object : m_ESO_SnapObjects)
	{
		Writer.w_stringZ(Object->GetName());
	}
	Writer.close_chunk();

	CUndoJournal::Add(Parts, UndoLevelKey, shared_str(), Writer);

	for (auto& [ToolID, ToolPtr] : m_SceneTools)
	{
		if (!IsUndoTool(ToolID, ToolPtr))
		{
			continue;
		}

		ESceneCustomOTool* OTool = smart_cast<ESceneCustomOTool*>(ToolPtr);
		Writer.clear();
		if (OTool == nullptr)
		{
			ToolPtr->SaveStream(Writer);
			CUndoJournal::Add(Parts, u32(ToolID), shared_str(), Writer);
			continue;
		}

		OTool->SaveToolStream(Writer);
		CUndoJournal::Add(Parts, u32(ToolID), shared_str(), Writer);

		for (CCustomObject* Object : OTool->GetObjects())
		{
			if (!IsUndoObject(Object))
			{
				continue;
			}

			Writer.clear();
			SaveObjectStream(Object, Writer);
			CUndoJournal::Add(Parts, u32(ToolID), Object->GetName(), Writer);
		}
	}
}

void EScene::RestoreUndoState(const CUndoJournal::State& Parts)
{
	g_LevelAutoBackup.Cancel();
	GWorldPartition.MarkDirty();
	Device.DetailsTask.wait();

	// Tools loaded again as a whole: other tools, and object tools whose own data changed
	xr_set<ObjClassID> Rebuild;
	xr_map<ObjClassID, xr_vector<const CUndoJournal::SPart*>> Recreate;
	for (const CUndoJournal::SPart& Part : Parts)
	{
		if (Part.Key == UndoLevelKey)
		{
			continue;
		}

		const ObjClassID ToolID = ObjClassID(Part.Key);
		if (Part.Name.size() == 0)
		{
			Rebuild.insert(ToolID);
		}
		else
		{
			Recreate[ToolID].push_back(&Part);
		}
	}

	// Objects that go away: every object of a rebuilt object tool, and the changed ones
	xr_hash_set<CCustomObject*> Removed;
	xr_map<ObjClassID, xr_vector<CCustomObject*>> RemovedByTool;
	for (ObjClassID ToolID : Rebuild)
	{
		if (ESceneCustomOTool* OTool = GetOTool(ToolID))
		{
			for (CCustomObject* Object : OTool->GetObjects())
			{
				Removed.insert(Object);
			}
		}
	}

	for (auto& [ToolID, Changes] : Recreate)
	{
		ESceneCustomOTool* OTool = GetOTool(ToolID);
		if (OTool == nullptr || Rebuild.contains(ToolID))
		{
			continue;
		}

		xr_hash_map<shared_str, CCustomObject*> ByName;
		for (CCustomObject* Object : OTool->GetObjects())
		{
			if (IsUndoObject(Object))
			{
				ByName.emplace(Object->GetName(), Object);
			}
		}

		for (const CUndoJournal::SPart* Part : Changes)
		{
			auto It = ByName.find(Part->Name);
			if (It != ByName.end())
			{
				Removed.insert(It->second);
				RemovedByTool[ToolID].push_back(It->second);
			}
		}
	}

	CSceneUndoLinks Links;
	Links.Unlink(*this, m_SceneTools, Removed);
	for (CCustomObject* Object : Removed)
	{
		m_ESO_SnapObjects.remove(Object);
	}

	for (auto& [ToolID, Objects] : RemovedByTool)
	{
		ESceneCustomOTool* OTool = GetOTool(ToolID);
		for (CCustomObject* Object : Objects)
		{
			OTool->_RemoveObject(Object);
			xr_delete(Object);
		}
	}

	// Create: in class order, so sectors find their scene objects and portals their sectors
	auto CreateObject = [this](const CUndoJournal::SPart& Part)
	{
		IReader Reader = Part.Open();
		CCustomObject* Object = nullptr;
		if (ReadObjectStream(Reader, Object))
		{
			AppendObject(Object, false);
		}
		else
		{
			Msg("! Scene undo: can't restore object '%s'", Part.Name.c_str());
		}
	};

	xr_set<ObjClassID> ObjectTools = Rebuild;
	for (auto& [ToolID, Changes] : Recreate)
	{
		ObjectTools.insert(ToolID);
	}

	for (ObjClassID ToolID : ObjectTools)
	{
		ESceneCustomOTool* OTool = GetOTool(ToolID);
		if (OTool == nullptr)
		{
			continue;
		}

		if (Rebuild.contains(ToolID))
		{
			OTool->Clear();
			if (CUndoJournal::Blob Data = UndoJournal.Find(u32(ToolID), shared_str()))
			{
				IReader Reader = CUndoJournal::SPart{u32(ToolID), shared_str(), Data}.Open();
				OTool->LoadStream(Reader);
			}

			CUndoJournal::State Objects;
			UndoJournal.Collect(u32(ToolID), Objects);
			for (const CUndoJournal::SPart& Part : Objects)
			{
				if (Part.Name.size() != 0)
				{
					CreateObject(Part);
				}
			}
			continue;
		}

		for (const CUndoJournal::SPart* Part : Recreate[ToolID])
		{
			if (Part->Data)
			{
				CreateObject(*Part);
			}
		}
	}

	Links.Relink(*this);

	// Tools without objects load as a whole and find the objects by name
	for (ObjClassID ToolID : Rebuild)
	{
		ESceneToolBase* Tool = GetTool(ToolID);
		if (Tool == nullptr || GetOTool(ToolID) != nullptr)
		{
			continue;
		}

		Tool->Clear();
		if (CUndoJournal::Blob Data = UndoJournal.Find(u32(ToolID), shared_str()))
		{
			IReader Reader = CUndoJournal::SPart{u32(ToolID), shared_str(), Data}.Open();
			Tool->LoadStream(Reader);
		}
	}

	if (CUndoJournal::Blob Level = UndoJournal.Find(UndoLevelKey, shared_str()))
	{
		IReader Reader = CUndoJournal::SPart{UndoLevelKey, shared_str(), Level}.Open();
		if (IReader* Options = Reader.open_chunk(UndoChunkLevelOptions))
		{
			m_LevelOp.Read(*Options);
			Options->close();
		}

		m_ESO_SnapObjects.clear();
		if (Reader.find_chunk(UndoChunkSnapObjects))
		{
			const u32 Count = Reader.r_u32();
			shared_str Name;
			for (u32 Index = 0; Index < Count; ++Index)
			{
				Reader.r_stringZ(Name);
				if (CCustomObject* Object = FindObjectByName(Name.c_str(), OBJCLASS_SCENEOBJECT))
				{
					m_ESO_SnapObjects.push_back(Object);
				}
			}
		}
	}

	UpdateSnapList();
	Tools->ClearDebugDraw();
	EContext.UI->UpdateScene(true);
	SynchronizeObjects();
}
