#pragma once

#include "../../CustomDetector.h"

class CUIDosimeter;

class CDosimeter final :
	public CCustomDetector
{
public:
	virtual void Load(const char* section) override;
	virtual void shedule_Update(u32 dt) override;

	virtual void render_item_3d_ui() override;
	virtual bool render_item_3d_ui_query() override;

	virtual CCustomDetector* cast_custom_detector() override { return this; }
	virtual CCustomDevice* cast_custom_device() override { return this; }

protected:
	virtual void CreateUI() override;
	CUIDosimeter& ui();
};