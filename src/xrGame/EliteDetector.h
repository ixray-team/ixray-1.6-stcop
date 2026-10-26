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
	virtual void render_item_3d_ui() final;
	virtual bool render_item_3d_ui_query() final;
	const SEliteDetectorDesc& EliteDesc() const { return static_cast<const SEliteDetectorDesc&>(DetectorDesc()); }
	const char* ui_xml_tag() const { return EliteDesc().UiXmlTag.c_str(); }

	virtual CCustomDetector* cast_custom_detector() override { return this; }
	virtual CCustomDevice* cast_custom_device() override { return this; }

protected:
	virtual void UpdateAf() final;
	virtual void CreateUI() final;
	CUIArtefactDetectorElite& ui();
	virtual const SCustomDetectorDesc& AcquireDetectorDesc(const shared_str& Section) const override { return SEliteDetectorDesc::Registry::Get(Section); }
	virtual bool NeedDetectSounds() const override { return false; }
};

class CScientificDetector final : 
	public CEliteDetector
{
	using inherited = CEliteDetector;
public:
	CScientificDetector();
	virtual ~CScientificDetector() override;
	virtual void Load(const char* section) override;
	virtual void OnH_B_Independent(bool just_before_destroy) override;
	virtual void shedule_Update(u32 dt) override;

	virtual CCustomDetector* cast_custom_detector() override { return this; }
	virtual CCustomDevice* cast_custom_device() override { return this; }

protected:
	virtual void UpdateWork() override;
	virtual const SCustomDetectorDesc& AcquireDetectorDesc(const shared_str& Section) const override { return SScientificDetectorDesc::Registry::Get(Section); }
	const SScientificDetectorDesc& ScientificDesc() const { return static_cast<const SScientificDetectorDesc&>(DetectorDesc()); }
	CZoneList m_zones;
};

