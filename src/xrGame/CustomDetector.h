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

	float m_fAfVisRadius = 0.0f;
	float m_fAfDetectRadius = 0.0f;
protected:
	CUIArtefactDetectorBase* m_ui = nullptr;
	CAfList	m_artefacts;
public:
	bool m_need_refresh = false;
public:
	CCustomDetector() = default;
	~CCustomDetector() override;

	bool IsNeedReloadUI() { return m_bWorking && m_need_refresh; }
	void Load(LPCSTR section) override;
	void OnH_B_Independent(bool just_before_destroy) override;
	void shedule_Update(u32 dt) override;
	void TurnDetectorInternal(bool b) final override;

	float AfVisibleRadius() const { return m_fAfVisRadius; }
	float AfDetectRadius() const { return m_fAfDetectRadius; }

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }

	void save(NET_Packet& output_packet) override;
	void load(IReader& input_packet) override;

protected:
	void UpdateWork() override;
	virtual void UpdateAf() {};
	virtual void CreateUI() {};
};