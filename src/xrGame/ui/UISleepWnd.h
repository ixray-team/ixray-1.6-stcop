#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "../../xrSound/Sound.h"

class CUIStatic;
class CUITrackBar;
class CUI3tButton;
class CUIMessageBoxEx;
class CUIWindow;

class CUISleepWnd final : public CUIDialogWnd, public CUIWndCallback
{
public:
	CUISleepWnd();
	~CUISleepWnd() override;

	bool HasInitializedLayout() const { return m_timeTrack != nullptr; }
	bool IsSleepDialogReady() const { return HasInitializedLayout(); }
	bool IsSleepDialogShown() const { return GetVisible(); }

	void Init(CUIXml& uiXml);
	void ShowSleepDialog();
	void ShowSleepDialog(int hours);

	void SetHourPresets(xr_vector<int> hours);
	void ClearHourPresets() { SetHourPresets({}); }

	void HideSleepDialog();
	void CancelSleepDialog() { HideSleepDialog(); }
	bool ConfirmSleep();
	void ForceSleep(int hours);
	bool AbortSleep();

	int  GetSleepSelectedHours() const { return SelectedHours(); }
	void SetSleepSelectedHours(int hours) { SetSelectedHours(hours, true); }

	bool IsActorSleeping() const;
	u8   GetSleepPhase() const { return m_camPhase; }

	void SetSleepHoursRange(int minHours, int maxHours);
	void ClearSleepHoursRange();
	void SetSleepAllowBleeding(bool allow);
	void ClearSleepAllowBleeding();
	void SetSleepRestorePower(float power);
	void ClearSleepRestorePower();
	void SetSleepMute(bool muteMusic, bool muteEffects);
	void ClearSleepMute();
	void ClearSleepSessionOverrides();

	void SetSleepBlocked(bool blocked, LPCSTR warningText = nullptr);

	void Update() override;
	void SendMessage(CUIWindow* pWnd, s16 msg, void* pData = nullptr) override;
	bool OnKeyboardAction(int dik, EUIMessages keyboard_action) override;
	bool OnGamepadKeyAction(int id, EUIMessages action) override;
	bool OnGamepadKeyHold(int id) override;
	CUIWindow* ui_cast_window() override { return this; }

private:
	struct SleepParams
	{
		shared_str panoramaTexture = "ui_inGame2_sky_panorama";
		int  panoramaHours = 24;
		bool panoramaBindWake = false;
		float panoramaSmoothSpeed = 0.f;
		bool panoramaNativeScale = false;
		float panoramaTexScale = 1.f;
		bool panoramaWrap = true;

		shared_str hourLabelSuffix = "st_sleep_hours";
		shared_str warningBoxTemplate = "message_box_ok";
		shared_str warningBleeding = "sleep_warning_bleeding";
		shared_str warningRadiation = "sleep_warning_radiation";
		shared_str warningBoth = "sleep_warning_all_pleasures";
		bool allowSleepWithBleeding = false;

		int minHours = 1;
		int maxHours = 24;

		shared_str camAnm = "camera_effects\\sleep.anm";
		shared_str ppEffector = "sleep_fade.ppe";
		int camId = 10;
		int ppId = 11;

		float markerMinX = 5.f;
		float restorePower = 1.f;
		bool muteMusic = true;
		bool muteEffects = true;

		float presetSpacing = 4.f;
		bool presetsConfirm = false;

		shared_str fmtTimeNow = "%s";
		shared_str fmtSleepDuration = "%d%s";
		shared_str fmtWakeTime = "%s";
		char timeSeparator = ':';
		u8 timePrecision = 1;

		float trackSmoothSpeed = 12.f;
	};

	struct SleepSessionOverrides
	{
		bool hasHoursRange = false;
		int minHours = 1;
		int maxHours = 24;

		bool hasAllowBleeding = false;
		bool allowSleepWithBleeding = false;

		bool hasRestorePower = false;
		float restorePower = 1.f;

		bool hasMute = false;
		bool muteMusic = true;
		bool muteEffects = true;
	};

	struct PresetEntry { CUI3tButton* btn = nullptr; int hours = 1; };

	enum EUiSound : u8
	{
		eSndOpen = 0, eSndClose, eSndSleep, eSndCancel, eSndTrack, eSndPreset, eSndWarning, eSndCount
	};

	void LoadSleepParams(CUIXml& xml);
	void BuildHourLabels(CUIXml& xml);
	void RebuildPresetButtons();
	void ApplyTrackBounds();
	void ApplySessionOverrides();
	void ClearSessionFlag(bool& flag);

	void InitializeLayout();
	void UpdateMarker();
	void UpdatePanorama(bool instant = false);
	void ApplyPanoramaHours(float hoursMod);
	void SetPanoPanel(CUIStatic* st, float x, float w, float h, float u0, float v0, float u1, float v1, bool show);
	float PanoramaTargetHours() const;
	static float WrapHoursDelta(float from, float to, float hoursN);
	void UpdateTimeInfo();
	void TestAndShow();
	void ShowWarning(const shared_str& text);

	bool CanSleepNow(int hours, shared_str& outWarning) const;

	void OnConfirmSleep();
	void OnCamPhase1Done();
	void OnCamEffectorDone();
	void WakeUp();
	void RestoreSleepAudio();
	void RemoveSleepEffectors();

	int  SelectedHours() const;
	void SetSelectedHours(int hours, bool instant = false);
	void StepTrack(bool right);
	void SnapTrack(float hours);
	void UpdateTrackSmooth();
	void PlaySnd(EUiSound id);
	bool HandleUiAction(int action, bool gamepad = false, bool hold = false);

	void OnButtonSleep(CUIWindow* w, void* d);
	void OnButtonCancel(CUIWindow* w, void* d);
	void OnMessageBoxOk(CUIWindow* w, void* d);
	void OnPresetClicked(CUIWindow* w, void* d);

	friend class CSleepCamEffectorCB;

	CUIStatic* m_background = nullptr;
	CUIStatic* m_sleepStatic = nullptr;
	CUIStatic* m_sleepStatic2 = nullptr;
	CUIStatic* m_marker = nullptr;
	Fvector2 m_markerBase{};
	CUIStatic* m_stTimeNow = nullptr;
	CUIStatic* m_stSleepDuration = nullptr;
	CUIStatic* m_stWakeTime = nullptr;
	xr_vector<CUIStatic*> m_hourLabels;

	CUITrackBar* m_timeTrack = nullptr;
	CUIMessageBoxEx* m_warningBox = nullptr;

	xr_vector<PresetEntry> m_presets;
	xr_vector<int> m_presetHours;
	bool m_hasPresetTpl = false;
	Fvector2 m_presetTplPos{};
	Fvector2 m_presetTplSize{};

	SleepParams m_paramsBase;
	SleepParams m_params;
	SleepSessionOverrides m_session;
	ref_sound m_snd[eSndCount];

	bool m_sleepBlocked = false;
	shared_str m_sleepBlockedWarning;

	float m_savedMusic = 0.f;
	float m_savedEffects = 0.f;
	int m_lastTimeInfoHours = -1;
	u8 m_camPhase = 0;

	Frect m_panoTex{};
	Fvector2 m_panoDisplay{};
	Fvector2 m_panoBasePos{};
	float m_hourStepPx = 0.f;
	float m_panoScrollHours = 0.f;

	float m_trackTarget = 1.f;
	float m_trackVisual = 1.f;
	bool m_trackSmoothActive = false;
	bool m_trackWasCapturing = false;
};
