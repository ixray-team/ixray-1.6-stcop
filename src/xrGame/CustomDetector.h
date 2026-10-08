#pragma once
#include "CustomDevice.h"
#include "AnomalyZone.h"
#include "CustomDetectorZones.h"
#include "ui/ArtefactDetectorUI.h"
#include "IPowerManager.h"

class CUIArtefactDetectorBase;

class CCustomDetector : public CCustomDevice, public IPowerManager
{
	using inherited = CCustomDevice;

	const SCustomDetectorDesc* CurrentDetectorDesc = nullptr;
protected:
	CUIArtefactDetectorBase* m_ui = nullptr;
	CAfList	m_artefacts;
public:
	bool m_need_refresh = false;
public:
	CCustomDetector() = default;
	~CCustomDetector() override;

	bool IsNeedReloadUI() { return m_bWorking && m_need_refresh; }
	void Load(const char* section) override;
	void OnH_B_Independent(bool just_before_destroy) override;
	void shedule_Update(u32 dt) override;
	void TurnDetectorInternal(bool b) final override;

	const SCustomDetectorDesc& DetectorDesc() const { VERIFY(CurrentDetectorDesc); return *CurrentDetectorDesc; }
	float AfVisibleRadius() const { return DetectorDesc().AfVisRadius; }
	float AfDetectRadius() const { return DetectorDesc().AfDetectRadius; }

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }

	void save(NET_Packet& output_packet) override;
	void load(IReader& input_packet) override;

protected:
	void UpdateWork() override;
	virtual void UpdateAf() {};
	virtual void CreateUI() {};
	virtual const SCustomDetectorDesc& AcquireDetectorDesc(const shared_str& Section) const { return SCustomDetectorDesc::Registry::Get(Section); }
	virtual bool NeedDetectSounds() const { return true; }
};