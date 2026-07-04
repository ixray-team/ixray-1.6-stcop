#include "stdafx.h"
#include "XRayEditor.h"

XRayEditor::XRayEditor()
{
	Device.seqFrame.Add(this, REG_PRIORITY_HIGH + 1000);
}

XRayEditor::~XRayEditor()
{
}

void XRayEditor::Level_Scan()
{
}

int XRayEditor::Level_ID(const char* name, const char* ver, bool bSet)
{
	return 0;
}

void XRayEditor::Level_Set(u32 ID)
{
}

void XRayEditor::LoadAllArchives()
{
}

CInifile* XRayEditor::GetArchiveHeader(const char* name, const char* ver)
{
	return nullptr;
}

void XRayEditor::LoadBegin()
{
}

void XRayEditor::LoadEnd()
{
}

void XRayEditor::LoadTitleInt(const char* str1, const char* str2, const char* str3)
{
}

void XRayEditor::LoadStage()
{
}

void XRayEditor::LoadSwitch()
{
}

void XRayEditor::LoadDraw()
{
}

void XRayEditor::OnEvent(EVENT E, u64 P1, u64 P2)
{
	CEngineApp::OnEvent(E, P1, P2);
}

void XRayEditor::OnFrame()
{
	CEngineApp::OnFrame();
}

void XRayEditor::load_draw_internal()
{
}

void XRayEditor::DestroyLoadingScreen()
{
}
