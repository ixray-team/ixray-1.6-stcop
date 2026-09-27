#include "stdafx.h"


TUI_ControlSpawnAdd::TUI_ControlSpawnAdd(int st, int act, ESceneToolBase* parent)
	: TUI_CustomControl(st, act, parent)
{
}

bool TUI_ControlSpawnAdd::AppendCallback(SBeforeAppendCallbackParams* p)
{
	const char* RefName = ((UISpawnTool*)parent_tool->pForm)->Current();
	if (!RefName)
	{
		ELog.DlgMsg(mtInformation, "Nothing selected.");
		return false;
	}
	if (Scene->LevelPrefix().c_str())
	{
		p->name_prefix = Scene->LevelPrefix().c_str();
		p->name_prefix += "_";
	}
	p->name_prefix += RefName;
	p->data = (void*)RefName;
	return (0 != p->name_prefix.length());
}

bool TUI_ControlSpawnAdd::Start(TShiftState Shift)
{
	UISpawnTool* F = (UISpawnTool*)parent_tool->pForm;
	if (F->IsAttachObject())
	{
		CCustomObject* From = Scene->RayPickObject(EContext.UI->ZFar(), EContext.UI->m_CurrentRStart, EContext.UI->m_CurrentRDir, OBJCLASS_DUMMY, 0, 0);
		if (From && From->FClassID != OBJCLASS_SPAWNPOINT)
		{
			ObjectList Lst;
			int Count = Scene->GetQueryObjects(Lst, OBJCLASS_SPAWNPOINT, 1, 1, 0);
			if (1 != Count)
			{
				ELog.DlgMsg(mtError, "Select one shape.");
			}
			else
			{
				CSpawnPoint* Base = smart_cast<CSpawnPoint*>(Lst.back());
				R_ASSERT(Base);
				if (Base->AttachObject(From))
				{
					if (!(Shift & ssAlt))
					{
						F->SetAttachObject(false);
						ResetActionToSelect();
					}
				}
				else
				{
					ELog.DlgMsg(mtError, "Attach impossible.");
				}
			}
		}
		else
		{
			ELog.DlgMsg(mtError, "Attach impossible.");
		}
	}
	else
	{
		DefaultAddObject(Shift, TBeforeAppendCallback(this, &TUI_ControlSpawnAdd::AppendCallback));
	}
	return false;
}