#include "stdafx.h"
#include "EScenePuddlesTools.h"
#include "../../../UI/Tools/UIPuddlesTool.h"

EScenePuddlesTool::EScenePuddlesTool() :
	ESceneCustomOTool(OBJCLASS_PUDDLES)
{
	Clear();
}

EScenePuddlesTool::~EScenePuddlesTool()
{
}

void EScenePuddlesTool::Clear(bool bSpecific)
{
	inherited::Clear(bSpecific);

	lcontrol_last_idx	= 0;
	lcontrols.clear		();
}

void EScenePuddlesTool::BeforeRender()
{
}

void EScenePuddlesTool::AfterRender()
{
}

void EScenePuddlesTool::OnRender(int priority, bool strictB2F)
{
	for (ObjectIt it=m_Objects.begin(); it!=m_Objects.end(); it++)
    	(*it)->Render(priority,strictB2F);
}

void EScenePuddlesTool::OnControlAppendClick(ButtonValue* sender, bool& bDataModified, bool& bSafe)
{
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	bDataModified = true;
}

void EScenePuddlesTool::OnControlRenameRemoveClick(ButtonValue* V, bool& bDataModified, bool& bSafe)
{
}

void EScenePuddlesTool::FillProp(LPCSTR pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);
}

bool EScenePuddlesTool::Validate(bool full_test)
{
	return true;
}

void EScenePuddlesTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	pForm = new UIPuddleTool();
}
 
void EScenePuddlesTool::RemoveControls()
{
	inherited::RemoveControls();
}


CCustomObject* EScenePuddlesTool::CreateObject(LPVOID data, LPCSTR name)
{
	CCustomObject* O = new CPuddle(data, name);
	O->FParentTools = this;
	return O;
}

void EScenePuddlesTool::OnDrawUI()
{
	xr_string result;
	bool ok = false;
	if (UITextForm::GetResult(ok,result))
	{
		if (ok)
		{
		}
	}

	UITextForm::Update();
}