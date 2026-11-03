#include "stdafx.h"
#include "UI_ParticleTools.h"
#include "../../../Layers/xrRenderPC_R4/ParticleAnimCurve.h"

extern xr_string _item_to_select_after_edit;

void CParticleTool::UndoClear()
{
	UndoJournal.Clear();
	m_Flags.set(flUndoSave, false);
}

bool CParticleTool::Undo()
{
	if (m_Flags.is(flUndoSave))
	{
		RealUndoSave();
	}

	CUndoJournal::State Parts;
	if (!UndoJournal.Undo(Parts))
	{
		return false;
	}

	RestoreUndoState(Parts);
	m_bModified = true;
	return true;
}

bool CParticleTool::Redo()
{
	if (m_Flags.is(flUndoSave))
	{
		RealUndoSave();
	}

	CUndoJournal::State Parts;
	if (!UndoJournal.Redo(Parts))
	{
		return false;
	}

	RestoreUndoState(Parts);
	m_bModified = true;
	return true;
}

void CParticleTool::RealUndoSave()
{
	m_Flags.set(flUndoSave, false);
	if (0 == EPrefs->scene_undo_level)
	{
		return;
	}

	// The stream holds the compiled actions too
	if (m_Flags.is(flCompileEffect))
	{
		RealCompileEffect();
	}

	CUndoJournal::State Parts;
	CaptureUndoState(Parts);
	UndoJournal.Commit(std::move(Parts));
}

void CParticleTool::CaptureUndoState(CUndoJournal::State& Parts)
{
	CMemoryWriter Writer;
	switch (m_EditMode)
	{
	case emEffect:
	case emAction:
		if (m_LibPED != nullptr)
		{
			m_LibPED->Save(Writer);
			CUndoJournal::Add(Parts, emEffect, m_LibPED->m_Name, Writer);
		}
		break;
	case emGroup:
	case emEffectSlot:
		if (m_LibPGD != nullptr)
		{
			m_LibPGD->Save(Writer);
			CUndoJournal::Add(Parts, emGroup, m_LibPGD->m_Name, Writer);
		}
		break;
	case emAnimCurve:
		if (m_LibAC != nullptr)
		{
			m_LibAC->Save(Writer);
			CUndoJournal::Add(Parts, emAnimCurve, m_LibAC->getName(), Writer);
		}
		break;
	default:
		break;
	}
}

void CParticleTool::RestoreUndoState(const CUndoJournal::State& Parts)
{
	// The actions and effect slots of the edited item are recreated: drop every pointer to them
	ResetCurrent();
	m_EditPE->Compile(nullptr);
	m_EditPG->Compile(nullptr);
	m_ItemProps->ClearProperties();

	shared_str Restored;
	for (const CUndoJournal::SPart& Part : Parts)
	{
		IReader Reader = Part.Open();
		bool Result = false;
		switch (EEditMode(Part.Key))
		{
		case emEffect:
			if (PS::CPEDef* Def = FindPE(Part.Name.c_str()))
			{
				Result = Def->Restore(Reader);
			}
			break;
		case emGroup:
			if (PS::CPGDef* Def = FindPG(Part.Name.c_str()))
			{
				Result = Def->Restore(Reader);
			}
			break;
		case emAnimCurve:
			if (PS::CPACDef* Def = FindPAC(Part.Name.c_str()))
			{
				Result = Def->Restore(Reader);
			}
			break;
		default:
			break;
		}

		if (Result)
		{
			Restored = Part.Name;
		}
		else
		{
			Msg("! Particles undo: can't restore '%s'", Part.Name.c_str());
		}
	}

	// The rebuilt list focuses the restored item again
	if (Restored.size() != 0)
	{
		_item_to_select_after_edit = Restored.c_str();
	}
	UpdateProperties(true);
}
