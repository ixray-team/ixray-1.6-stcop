#include "stdafx.h"
#include "Dosimeter.h"
#include "DosimeterUI.h"

void CDosimeter::Load(const char* section)
{
	CHudItemObject::Load(section);
}

void CDosimeter::shedule_Update(u32 dt)
{
	CHudItemObject::shedule_Update(dt);

	if (!IsWorking())
	{
		return;
	}

	Position().set(H_Parent()->Position());
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
	UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
}