#pragma once

#include "IInputReceiver.h"
#include "Effector.h"

class ENGINE_API CDemoRecord :
	public CEffectorCam,
	public IInputReceiver,
	public pureRender
{
	static struct force_position
	{
		bool set_position;
		Fvector p;
	} g_position;

	IWriter* file;
	Fmatrix Camera;

	Fvector p_lap;

	Fvector p_cam_pos;
	Fvector p_cam_pos_smoothed;

	Fvector hpb_current;
	Fvector hpb_smoothed;

	u32 Stage;

	Fvector FrameTopDelta;
	Fvector FrameRightDelta;
	Fvector Velocity;
	Fvector AngularVelocity;

	xr_vector<Fvector> KeyframesPositions;

	bool m_bMakeCubeMap;
	bool m_bMakeScreenshot;
	int m_iLMScreenshotFragment;
	bool m_bMakeLevelMap;
	bool m_bEnableAcceleration = false;
	bool NewInputSchema;
	bool lap_lock;

	float CameraTransformFactor;
	float m_fSpeed0;
	float m_fSpeed1;
	float m_fSpeed2;
	float m_fSpeed3;

	void MakeCubeMapFace(Fvector& D, Fvector& N);
	void MakeLevelMapProcess();
	void MakeScreenshotFace();
	void RecordKey();
	void MakeCubemap();
	void MakeScreenshot();
	void MakeLevelMapScreenshot(bool bHQ);

public:
	CDemoRecord(const char* name, float life_time = 60 * 60 * 1000);
	virtual ~CDemoRecord();

	virtual void IR_OnKeyboardPress(int dik);
	virtual void IR_OnKeyboardRelease(int dik);
	virtual void IR_OnKeyboardHold(int dik);
	virtual void IR_OnMouseMove(int dx, int dy);
	virtual void IR_OnMouseWheel(int direction);
	virtual void IR_OnMouseHold(int btn);
	virtual void IR_GamepadUpdateStick(int id, Fvector2 value);
	virtual void IR_GamepadKeyPress(int id);

	virtual bool ProcessCam(SCamEffectorInfo& info);
	void UpdateLookAtPoint();
	void UpdateLookUp();
	static void SetGlobalPosition(const Fvector& p) { g_position.p.set(p), g_position.set_position = true; }
	static void GetGlobalPosition(Fvector& p) { p.set(g_position.p); }
	bool m_b_redirect_input_to_level;
	virtual void OnRender();
};