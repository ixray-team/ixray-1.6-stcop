#include "stdafx.h"
#include "ActorEditor.h"
#include "UI_ActorTools.h"
#include "UIActorEditorForm.h"

static constexpr u32 UndoObjectKey = 0;

void CActorTools::UndoClear()
{
	UndoJournal.Clear();
}

void CActorTools::UndoSave()
{
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
		UndoJournal.Commit(std::move(Parts));
	}
}

bool CActorTools::Undo()
{
	CUndoJournal::State Parts;
	if (!UndoJournal.Undo(Parts))
	{
		return false;
	}

	if (RestoreUndoState())
	{
		Modified();
	}
	return true;
}

bool CActorTools::Redo()
{
	CUndoJournal::State Parts;
	if (!UndoJournal.Redo(Parts))
	{
		return false;
	}

	if (RestoreUndoState())
	{
		Modified();
	}
	return true;
}

void CActorTools::CaptureUndoState(CUndoJournal::State& Parts)
{
	if (m_pEditObject == nullptr)
	{
		return;
	}

	CMemoryWriter Writer;
	m_pEditObject->Save(Writer);
	CUndoJournal::Add(Parts, UndoObjectKey, shared_str(), Writer);
}

bool CActorTools::RestoreUndoState()
{
	CUndoJournal::Blob Data = UndoJournal.Find(UndoObjectKey, shared_str());
	if (!Data || m_pEditObject == nullptr)
	{
		return false;
	}

	CEditableObject* Object = new CEditableObject(m_pEditObject->m_LibName.c_str());
	IReader Reader = CUndoJournal::SPart{UndoObjectKey, shared_str(), Data}.Open();
	if (!Object->Load(Reader))
	{
		xr_delete(Object);
		return false;
	}

	Object->SetLoadInfo(m_pEditObject->m_LoadName.c_str(), m_pEditObject->Version());

	PhysicsStopSimulate();
	m_CurrentMotion = "";
	m_CurrentSlot = 0;
	UVView->SetSurface(nullptr, nullptr);
	m_ObjectItems->ClearList();
	m_Props->ClearProperties();

	m_pEditObject->DeletePhysicsShell();
	xr_delete(m_pEditObject);
	m_pEditObject = Object;
	m_pEditObject->Optimize();
	m_RenderObject.Clear();

	UIActorEditorForm::SetRenderMode(false);
	UpdateProperties();
	return true;
}
