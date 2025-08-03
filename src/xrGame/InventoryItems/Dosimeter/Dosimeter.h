#pragma once

#include "../../CustomDetector.h"

class CUIDosimeter;

class CDosimeter :
	public CCustomDetector
{
public:
	void Load(const char* section) override;
	void shedule_Update(u32 dt) override;

	void render_item_3d_ui() override;
	bool render_item_3d_ui_query() override;

protected:
	void UpdateAf() override;
	void CreateUI() override;
	CUIDosimeter& ui();
};