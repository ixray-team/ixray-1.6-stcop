//---------------------------------------------------------------------------
#include "stdafx.h"


#include "SHToolsInterface.h"
#include "UI_shadertools.h"
#include "../../xrEProps/FolderLib.h"
#include "../../xrECore/Editor/ui_main.h"

ISHTools::ISHTools(const ISHInit& init)
{
	m_bModified			= false;
	m_bLockUpdate		= false;
	Ext					= init;
}
//---------------------------------------------------------------------------

void ISHTools::ViewSetCurrentItem(const char* full_name)
{
	if (m_bLockUpdate) 	return;

	m_bLockUpdate		= true;
	Ext.m_Items->SelectItem(full_name);
	m_bLockUpdate		= false;
}
//---------------------------------------------------------------------------

void ISHTools::Modified()
{
	m_bModified = true;

	ApplyChanges();
	UndoSave();
}

void ISHTools::UndoTrack()
{
	shared_str Name;
	CMemoryWriter Writer;
	if (!SaveUndoItem(Name, Writer))
	{
		return;
	}

	CUndoJournal::State Parts;
	CUndoJournal::Add(Parts, 0, Name, Writer);
	UndoJournal.Track(std::move(Parts));
}

void ISHTools::UndoClear()
{
	UndoJournal.Clear();
}

void ISHTools::UndoSave()
{
	if (0 == EPrefs->scene_undo_level)
	{
		return;
	}

	shared_str Name;
	CMemoryWriter Writer;
	if (!SaveUndoItem(Name, Writer))
	{
		return;
	}

	CUndoJournal::State Parts;
	CUndoJournal::Add(Parts, 0, Name, Writer);
	UndoJournal.Commit(std::move(Parts));
}

bool ISHTools::Undo()
{
	CUndoJournal::State Parts;
	if (!UndoJournal.Undo(Parts))
	{
		return false;
	}

	RestoreUndoItems(Parts);
	return true;
}

bool ISHTools::Redo()
{
	CUndoJournal::State Parts;
	if (!UndoJournal.Redo(Parts))
	{
		return false;
	}

	RestoreUndoItems(Parts);
	return true;
}

void ISHTools::RestoreUndoItems(const CUndoJournal::State& Parts)
{
	for (const CUndoJournal::SPart& Part : Parts)
	{
		IReader Reader = Part.Open();
		if (!LoadUndoItem(Part.Name, Reader))
		{
			Msg("! %s undo: can't restore '%s'", ToolsName(), Part.Name.c_str());
		}
	}

	m_bModified = true;
	STools->UpdateProperties();
}
//---------------------------------------------------------------------------

bool ISHTools::IfModified()
{
	if (m_bModified){
		int mr = ELog.DlgMsg(mtConfirmation, mbYes | mbNo | mbCancel, "The '%s' has been modified.\nDo you want to save your changes?",ToolsName());
		switch(mr){
		case mrYes: Save(); m_bModified = false; break;
		case mrNo: m_bModified = false; break;
		case mrCancel: return false;
		}
	}
	return true;
}
//---------------------------------------------------------------------------

void ISHTools::ZoomObject(bool bOnlySel)
{
	Fbox BB;
	BB.set(-5,-5,-5,5,5,5);
	EContext.UI->CurrentView().m_Camera.ZoomExtents(BB);
}
//---------------------------------------------------------------------------

xr_string ISHTools::ViewGetCurrentItem(bool bFolderOnly)
{
	xr_string name;
	RStringVec lst;
	Ext.m_Items->GetSelected(lst);
	if (lst.size() == 1)
	{
		name = lst[0].c_str();
	}
	return name;
}

void ISHTools::RemoveCurrent()
{
	Ext.m_Items->RemoveSelectItem();
}

void ISHTools::RenameCurrent()
{
	R_ASSERT(0);
//	Ext.m_Items->RenameSelItem();
}

void ISHTools::OnFrame()
{
	if (m_LastSelection.size()){
		SetCurrentItem				(m_LastSelection.c_str(),true);
		m_LastSelection				= "";
	}
}                
void ISHTools::OnActivate()
{
	SetCurrentItem					(m_LastSelection.c_str(),true);
	EContext.UI->RedrawScene();
}
void ISHTools::OnDeactivate()
{
	Ext.m_PreviewProps->ClearProperties();
	m_LastSelection					= ViewGetCurrentItem(false);
	ResetCurrentItem				();
	Ext.m_Items->ClearList			();
}
//---------------------------------------------------------------------------
void ISHTools::OnCloneItem(const char* parent_path, const char* new_full_name)
{
	AppendItem(new_full_name, parent_path);
}

void ISHTools::OnCreateItem(const char* path)
{
	AppendItem(path);
}