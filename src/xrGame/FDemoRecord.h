#pragma once

#include "LevelInspector.h"
#include "../xrEngine/IInputReceiver.h"
#include "../xrEngine/Effector.h"

class CDemoRecord :
	public CEffectorCam,
	public IInputReceiver,
	public pureRender
{
public:
	enum
	{
		// IR_OnKeyPress
		K_RESET_FOV				= SDL_SCANCODE_Z,
		K_TOGGLE_BONE_ATTACH	= SDL_SCANCODE_U,
		K_TOGGLE_SKELETON		= SDL_SCANCODE_K,
		K_TOGGLE_LOOKAT_LOCK	= SDL_SCANCODE_J,
		K_TOGGLE_REDIRECT_INPUT = SDL_SCANCODE_0,
		K_SHOW_CONSOLE			= SDL_SCANCODE_GRAVE,
		K_TOGGLE_PAUSE			= SDL_SCANCODE_PAUSE,
		K_ENABLE_ACCELERATION	= SDL_SCANCODE_LCTRL,
		K_ENABLE_ACCELERATION_R = SDL_SCANCODE_RCTRL,
		K_RECORD_KEYFRAME		= SDL_SCANCODE_F,
		K_RECORD_KEYFRAME_OLD	= SDL_SCANCODE_SPACE,
		K_MAKE_CUBEMAP			= SDL_SCANCODE_BACKSPACE,
		K_MAKE_LEVELMAP			= SDL_SCANCODE_F11,
		K_MAKE_SCREENSHOT		= SDL_SCANCODE_F12,
		K_QUIT					= SDL_SCANCODE_ESCAPE,
		K_FORCE_TRANSFORM		= SDL_SCANCODE_RETURN,

		// IR_OnKeyHold
		K_MOVE_FORWARD			= SDL_SCANCODE_W,
		K_MOVE_LEFT				= SDL_SCANCODE_A,
		K_MOVE_BACKWARD			= SDL_SCANCODE_S,
		K_MOVE_RIGHT			= SDL_SCANCODE_D,
		K_ROLL_LEFT				= SDL_SCANCODE_Q,
		K_ROLL_RIGHT			= SDL_SCANCODE_E,
		K_SLOW_SPEED			= SDL_SCANCODE_LSHIFT,
		K_FAST_SPEED			= SDL_SCANCODE_LALT,

		// IR_OnMouseMove
		M_MOVE_FORWARD			= 0,
		M_MOVE_BACKWARD			= 1,

		GP_QUIT					= SDL_GAMEPAD_BUTTON_EAST,
		GP_TOGGLE_ACCELERATION	= SDL_GAMEPAD_BUTTON_LEFT_STICK,
		GP_RECORD_KEYFRAME		= SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER,
	};

	bool draw_skeleton = false;
	bool view_from_bone_mode = false;
	bool look_at_point_mode = false;
	bool enable_acceleration = false;
	bool new_input_schema = false;
	bool show_help = false;
	bool fov_auto_scale = true;
	bool redirect_input_to_level;

	float camera_transform_speed = 3.f;
	float m_fSpeed0 = 0.f;
	float m_fSpeed1 = 0.f;
	float m_fSpeed2 = 0.f;
	float m_fSpeed3 = 0.f;

	float stored_fov = 0.f;
	float fov_scale_speed = 5.f;
	float stored_camera_transform_speed = 3.f;
	float stored_fSpeed0 = 0.f;
	float stored_fSpeed1 = 0.f;
	float stored_fSpeed2 = 0.f;
	float stored_fSpeed3 = 0.f;
	float stored_fov_scale_speed = 5.f;

	xr_vector<Fvector> keyframes;

	Fvector hpb;
	Fvector p_cam_pos;
	Fvector hpb_view_from_bone_offset;
	Fvector p_cam_pos_view_from_bone_offset;

	CObject* bone_holder = nullptr;
	IKinematics* bone_holder_kinematics = nullptr;
	u16 bone_id = BI_NONE;
	collide::rq_result rq_result{};

	Fvector look_at_point{};

	bool try_attach_bone();
	void detach_bone();

	void get_camera_hpb(Fvector& out) { camera.getHPB(out); }
	void set_camera_hpb(float h, float p, float b) { camera.setHPB(h, p, b); }

	void make_cubemap();
	void make_screenshot();
	void make_level_map_screenshot(bool bHQ);
	void record_keyframe();

private:
	static struct force_position
	{
		bool set_position;
		Fvector p;
	} g_position;

	IWriter* file;
	Fmatrix camera;

	Fvector hpb_current;
	Fvector p_cam_pos_current;

	u32 Stage;

	Fvector frame_pos_delta;
	Fvector frame_hpb_delta;

	bool m_bMakeCubeMap;
	bool m_bMakeScreenshot;
	int m_iLMScreenshotFragment;
	bool m_bMakeLevelMap;
	bool attach_to_bone_mode = false;

	float dt;

	void make_cube_map_face(Fvector& D, Fvector& N);
	void make_level_map_process();
	void make_screenshot_face();

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
	void update_look_at_point();
	void update_free_look();
	void update_look_from_bone();
	void parse_actor_cam();
	static void SetGlobalPosition(const Fvector& p) { g_position.p.set(p), g_position.set_position = true; }
	static void GetGlobalPosition(Fvector& p) { p.set(g_position.p); }
	void OnRender() override;
};

extern CDemoRecord* demo_record;