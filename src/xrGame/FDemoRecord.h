#pragma once

#include "LevelInspector.h"
#include "../xrEngine/IInputReceiver.h"
#include "../xrEngine/Effector.h"

class CDemoRecord :
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
	CObject* bone_holder;
	IKinematics* bone_holder_kinematics;
	u16 bone_id;

	Fvector p_cam_pos;
	Fvector p_cam_pos_smoothed;

	Fvector hpb;
	Fvector hpb_current;
	Fvector hpb_view_from_bone_offset;

	collide::rq_result rq_result;

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
	bool draw_skeleton = false;
	bool attach_to_bone_mode = false;
	bool view_from_bone_mode = false;

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
	~CDemoRecord() override;

	virtual void IR_OnKeyboardPress(int dik) override;
	virtual void IR_OnKeyboardRelease(int dik) override;
	virtual void IR_OnKeyboardHold(int dik) override;
	virtual void IR_OnMousePress(int btn) override;
	virtual void IR_OnMouseMove(int dx, int dy) override;
	virtual void IR_OnMouseRelease(int btn) override;
	virtual void IR_OnMouseWheel(int direction) override;
	virtual void IR_OnMouseHold(int btn) override;
	virtual void IR_GamepadUpdateStick(int id, Fvector2 value) override;
	virtual void IR_GamepadKeyPress(int id) override;

	bool ProcessCam(SCamEffectorInfo& info) override;
	void UpdateLookAtPoint();
	void UpdateFreeLook();
	void UpdateLookFromBone();
	void ParseActorCam();
	static void SetGlobalPosition(const Fvector& p) { g_position.p.set(p), g_position.set_position = true; }
	static void GetGlobalPosition(Fvector& p) { p.set(g_position.p); }
	bool m_b_redirect_input_to_level;
	void OnRender() override;
};