#pragma once
#include "CustomDetector.h"

class CUIArtefactDetectorElite;

class CEliteDetector : 
	public CCustomDetector
{
	using inherited = CCustomDetector;
public:
	CEliteDetector();
	~CEliteDetector() override = default;
	void render_item_3d_ui() final override;
	bool render_item_3d_ui_query() final override;
	const SEliteDetectorDesc& EliteDesc() const { return static_cast<const SEliteDetectorDesc&>(DetectorDesc()); }
	const char* ui_xml_tag() const { return EliteDesc().UiXmlTag.c_str(); }

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }

protected:
	void UpdateAf() final override;
	void CreateUI() final override;
	CUIArtefactDetectorElite& ui();
	const SCustomDetectorDesc& AcquireDetectorDesc(const shared_str& Section) const override { return SEliteDetectorDesc::Registry::Get(Section); }
	bool NeedDetectSounds() const override { return false; }
};

class CScientificDetector final : 
	public CEliteDetector
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
	const SCustomDetectorDesc& AcquireDetectorDesc(const shared_str& Section) const override { return SScientificDetectorDesc::Registry::Get(Section); }
	const SScientificDetectorDesc& ScientificDesc() const { return static_cast<const SScientificDetectorDesc&>(DetectorDesc()); }
	CZoneList m_zones;
};

