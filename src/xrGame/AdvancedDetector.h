#pragma once
#include "CustomDetector.h"
class CUIArtefactDetectorAdv;

class CAdvancedDetector final : public CCustomDetector
{
	using inherited = CCustomDetector;
public:
	CAdvancedDetector();
	virtual ~CAdvancedDetector() override = default;
	virtual void on_a_hud_attach() override;
	virtual void on_b_hud_detach() override;

	virtual CCustomDetector* cast_custom_detector() override { return this; }
	virtual CCustomDevice* cast_custom_device() override { return this; }
	virtual void shedule_Update(u32 dt) override;
protected:
	virtual void UpdateAf() override;
	virtual void CreateUI() override;
	CUIArtefactDetectorAdv& ui();

};
