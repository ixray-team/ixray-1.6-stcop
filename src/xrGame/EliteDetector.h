#pragma once
#include "CustomDetector.h"

class CUIArtefactDetectorElite;

class CEliteDetector : public CCustomDetector
{
	using inherited = CCustomDetector;
public:
	CEliteDetector();
	~CEliteDetector() override = default;
	void Load(const char* section) override;
	void render_item_3d_ui() final override;
	bool render_item_3d_ui_query() final override;
	const char* ui_xml_tag() const { return m_ui_xml_tag; }

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }

protected:
	void UpdateAf() final override;
	void CreateUI() final override;
	CUIArtefactDetectorElite& ui();
	const char* m_ui_xml_tag;
};

class CScientificDetector final : public CEliteDetector
{
	using inherited = CEliteDetector;
public:
	CScientificDetector();
	~CScientificDetector() override;
	void Load(const char* section) override;
	void OnH_B_Independent(bool just_before_destroy) override;
	void shedule_Update(u32 dt) override;

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }

protected:
	void UpdateWork() override;
	CZoneList m_zones;
};

