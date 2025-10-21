#pragma once

#include "../../CustomDetector.h"

class CUIDosimeter;

class CDosimeter final :
	public CCustomDetector
{
public:
	void Load(const char* section) override;
	void shedule_Update(u32 dt) override;

	void render_item_3d_ui() override;
	bool render_item_3d_ui_query() override;

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }

protected:
	void CreateUI() override;
	CUIDosimeter& ui();
};