#include "stdafx.h"
#include "Dosimeter.h"
#include "DosimeterUI.h"

void CDosimeter::Load(const char* section)
{
	CHudItemObject::Load(section);

	m_sounds.LoadSound(section, "snd_draw", "sndShow");
	m_sounds.LoadSound(section, "snd_holster", "sndHide");
}

void CDosimeter::shedule_Update(u32 dt)
{
	CHudItemObject::shedule_Update(dt);

	if (!IsWorking())
		return;

	Position().set(H_Parent()->Position());
}

void CDosimeter::UpdateAf()
{
}

void CDosimeter::CreateUI()
{
	VERIFY(m_ui == nullptr);
	m_ui = new CUIDosimeter;
	ui().construct(this);
}

CUIDosimeter& CDosimeter::ui()
{
	return *((CUIDosimeter*)m_ui);
}

bool CDosimeter::render_item_3d_ui_query()
{
	return IsWorking();
}

void CDosimeter::render_item_3d_ui()
{
	R_ASSERT(HudItemData());
	CCustomDetector::render_item_3d_ui();
	ui().Draw();
	//	Restore cull mode
	UIRender->CacheSetCullMode(IUIRender::cmCCW);
}