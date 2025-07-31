#include "stdafx.h"
#include "ViewportMesh.h"

static void ViewportFocusCallback()
{
	LUI->EndEState(esEditLibrary);
	LUI->EndEState(esEditScene);
	LUI->BeginEState(esEditMesh);
}

CViewportMesh::CViewportMesh()
{
	View.OnFocusCallback = ViewportFocusCallback;
	ViewName = "Empty mesh";
}

CViewportMesh::~CViewportMesh()
{
}

void CViewportMesh::Draw()
{
	if (ViewName.size() <= 0)
	{
		return;
	}

	if (!ImGui::Begin(*ViewName, &bOpen))
	{
		ImGui::End();
		return;
	}

	if (ImGui::IsWindowFocused())
	{
		UI->ViewID = View.ViewportID;
	}

	View.DrawVP();

	ImGui::End();
}

void CViewportMesh::Render()
{
	if (UI->ViewID != View.ViewportID)
	{
		return;
	}

	if (ViewMesh != nullptr)
	{
		ViewMesh->OnFrame();
		ViewMesh->RenderSingle();
	}
}

void CViewportMesh::OpenModel(const xr_path& File)
{
	if (ViewMesh != nullptr)
	{
		xr_delete(ViewMesh);
	}

	xr_string Str = File.xstring();
	ViewMesh = new CSceneObject(nullptr, Str.c_str());

	xr_strlwr(Str);
	ViewName = Str.c_str();
	ViewMesh->SetReference(Str.c_str());
}