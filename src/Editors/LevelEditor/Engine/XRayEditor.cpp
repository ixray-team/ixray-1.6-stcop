#include "stdafx.h"
#include "XRayEditor.h"
struct _SoundProcessor : public pureFrame
{
	virtual void	_BCL	OnFrame()
	{
		//Msg							("------------- sound: %d [%3.2f,%3.2f,%3.2f]",u32(EngineDevice->dwFrame),VPUSH(EngineDevice->vCameraPosition));
		Device->Statistic->Sound.Begin();
		::Sound->update(Device->vCameraPosition, Device->vCameraDirection, Device->vCameraTop);
		Device->Statistic->Sound.End();
	}
}	SoundProcessor;
XRayEditor::XRayEditor()
{
	Device->seqFrame.Add(this, REG_PRIORITY_HIGH + 1000);

	if (psDeviceFlags.test(mtSound))	Device->seqFrameMT.Add(&SoundProcessor);
	else								Device->seqFrame.Add(&SoundProcessor);
}

XRayEditor::~XRayEditor()
{
}

void XRayEditor::Level_Scan()
{
}

int XRayEditor::Level_ID(LPCSTR name, LPCSTR ver, bool bSet)
{
	return 0;
}

void XRayEditor::Level_Set(u32 ID)
{
}

void XRayEditor::LoadAllArchives()
{
}

CInifile* XRayEditor::GetArchiveHeader(LPCSTR name, LPCSTR ver)
{
	return nullptr;
}

void XRayEditor::LoadBegin()
{
}

void XRayEditor::LoadEnd()
{
}

void XRayEditor::LoadTitleInt(LPCSTR str1, LPCSTR str2, LPCSTR str3)
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
	CApplication::OnEvent(E,P1,P2);
}

void _BCL XRayEditor::OnFrame()
{
	CApplication::OnFrame();
}

void XRayEditor::load_draw_internal()
{
}

void XRayEditor::destroy_loading_shaders()
{
}
