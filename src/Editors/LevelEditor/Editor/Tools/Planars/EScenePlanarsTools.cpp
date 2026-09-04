#include "stdafx.h"
#include "EScenePlanarsTools.h"

EScenePlanarsTool::EScenePlanarsTool() :
	ESceneCustomOTool(OBJCLASS_PLANARS)
{
	Clear();
}

EScenePlanarsTool::~EScenePlanarsTool()
{
}

void EScenePlanarsTool::Clear(bool bSpecific)
{
	inherited::Clear(bSpecific);
}

void EScenePlanarsTool::BeforeRender()
{
}

void EScenePlanarsTool::AfterRender()
{
}

void EScenePlanarsTool::OnRender(int priority, bool strictB2F)
{
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
		(*it)->Render(priority, strictB2F);
}

void EScenePlanarsTool::FillProp(const char* pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);
}

bool EScenePlanarsTool::Validate(bool /*full_test*/)
{
	return true;
}

void EScenePlanarsTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
}

void EScenePlanarsTool::RemoveControls()
{
	inherited::RemoveControls();
}

CCustomObject* EScenePlanarsTool::CreateObject(LPVOID data, const char* name)
{
	CCustomObject* O = new CPlanar(data, name);
	O->FParentTools = this;
	return O;
}

bool EScenePlanarsTool::Export(const char* path)
{
	xr_string ltx_filename = xr_string(path) + "level.planars";

	if (m_Objects.empty())
		return true;

	if (FS.exist(ltx_filename.c_str()))
		EFS.MarkFile(ltx_filename.c_str(), true);

	IWriter* F = FS.w_open(ltx_filename.c_str());
	if (!F)
		return false;

	for (CCustomObject* Object : m_Objects)
	{
		string128 buff;
		sprintf(buff, "[%s]", Object->FName.c_str());
		F->w_string(buff);

		sprintf(buff, "position = %0.3f, %0.3f, %0.3f", Object->FPosition.x, Object->FPosition.y, Object->FPosition.z);
		F->w_string(buff);

		sprintf(buff, "rotation = %0.3f, %0.3f, %0.3f", Object->FRotation.x, Object->FRotation.y, Object->FRotation.z);
		F->w_string(buff);

		sprintf(buff, "influence = %0.3f", Object->FScale.y * 0.5f);
		F->w_string(buff);

		sprintf(buff, "size_xz = %0.3f, %0.3f", Object->FScale.x * 0.5f, Object->FScale.z * 0.5f);
		F->w_string(buff);

		CPlanar* planar = static_cast<CPlanar*>(Object);
		sprintf(buff, "stiffness = %0.3f", planar->m_Stiffness);
		F->w_string(buff);
	}

	FS.w_close(F);
	return true;
}
