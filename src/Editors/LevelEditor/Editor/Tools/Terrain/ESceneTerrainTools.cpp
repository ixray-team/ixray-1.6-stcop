#include "stdafx.h"
#include "ESceneTerrainTools.h"
#include "../../../UI/Tools/UIPuddlesTool.h"

ESceneTerrainTool::ESceneTerrainTool() :
	ESceneCustomOTool(OBJCLASS_TERRAIN)
{
	Clear();
}

ESceneTerrainTool::~ESceneTerrainTool()
{
}

void ESceneTerrainTool::Clear(bool bSpecific)
{
	inherited::Clear(bSpecific);

	lcontrol_last_idx	= 0;
	lcontrols.clear		();
}

void ESceneTerrainTool::BeforeRender()
{
	
}

void ESceneTerrainTool::AfterRender()
{
}

void ESceneTerrainTool::OnRender(int priority, bool strictB2F)
{
	for (ObjectIt it=m_Objects.begin(); it!=m_Objects.end(); it++)
    	(*it)->Render(priority,strictB2F);
}

void ESceneTerrainTool::OnControlAppendClick(ButtonValue* sender, bool& bDataModified, bool& bSafe)
{
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	bDataModified = true;
}

void ESceneTerrainTool::OnControlRenameRemoveClick(ButtonValue* V, bool& bDataModified, bool& bSafe)
{
}

void ESceneTerrainTool::FillProp(LPCSTR pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);
}

BOOL ESceneTerrainTool::_AppendObject(CCustomObject* object)
{
	string_path Path = {};
	FS.update_path(Path, "$server_data_root$", object->GetName());
	xr_strcat(Path, ".r16");

	FS.TryLoad(Path);
	IReader* Stream = FS.r_open(Path);
	object->LoadStream(*Stream);
	FS.r_close(Stream);

	return inherited::_AppendObject(object);
}

bool ESceneTerrainTool::Validate(bool full_test)
{
	return true;
}

void ESceneTerrainTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	pForm = new UIPuddleTool();
}
 
void ESceneTerrainTool::RemoveControls()
{
	inherited::RemoveControls();
}


CCustomObject* ESceneTerrainTool::CreateObject(LPVOID data, LPCSTR name)
{
	CCustomObject* O = new CTerrain(data, name);
	O->FParentTools = this;
	return O;
}

void ESceneTerrainTool::OnDrawUI()
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

void ESceneTerrainTool::GetStaticDesc(int& v_cnt, int& f_cnt, bool b_selected_only, bool b_cform)
{
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); it++)
	{
		CTerrain* obj = (CTerrain*)(*it);

		if (b_selected_only && !obj->Selected())
			continue;

		f_cnt += obj->TerrainObject.GetFaceCount();
		v_cnt += obj->TerrainObject.GetVertexCount();
	}
}