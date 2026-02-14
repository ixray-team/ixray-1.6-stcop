#pragma once
#include "CustomDetector.h"
class CUIArtefactDetectorAdv;

class CAdvancedDetector final : public CCustomDetector
{
	using inherited = CCustomDetector;
public:
	CAdvancedDetector();
	~CAdvancedDetector() override = default;
	void on_a_hud_attach() override;
	void on_b_hud_detach() override;

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }
	virtual void shedule_Update(u32 dt) override;
protected:
	void UpdateAf() override;
	void CreateUI() override;
	CUIArtefactDetectorAdv& ui();

};
