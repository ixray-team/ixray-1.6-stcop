#ifndef __XR_INPUT__
#define __XR_INPUT__

#define DIRECTINPUT_VERSION 0x0800

#define DIK_L2_TRIGGER (SDL_GamepadButton::SDL_GAMEPAD_BUTTON_MAX + 0)
#define DIK_R2_TRIGGER (SDL_GamepadButton::SDL_GAMEPAD_BUTTON_MAX + 1)

class	ENGINE_API				IInputReceiver;

//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//описание класса
const int mouse_device_key		= 1;
const int keyboard_device_key	= 2;
const int all_device_key		= mouse_device_key | keyboard_device_key;
const int default_key			= mouse_device_key | keyboard_device_key ;

class ENGINE_API CInput
	:
	public pureFrame,
	public pureAppActivate,
	public pureAppDeactivate
{
public:
	enum {
		COUNT_MOUSE_BUTTONS			= 8,
		COUNT_MOUSE_AXIS			= 3,
		COUNT_KB_BUTTONS			= 256,
		COUNT_GP_BUTTONS			= SDL_GamepadButton::SDL_GAMEPAD_BUTTON_MAX + 2,
	};

	SDL_Gamepad* pGamePad = nullptr;

private:
	bool						mouseMoved = false;
	bool						mouseScrolled = false;
	char						mouseState[COUNT_MOUSE_BUTTONS] = {};
	char						KBState[COUNT_KB_BUTTONS] = {};
	char						GPState[COUNT_GP_BUTTONS] = {};
	int 						offs[COUNT_MOUSE_AXIS] = {};
	char						old_mouseState[COUNT_MOUSE_BUTTONS] = {};
	char						old_KBState[COUNT_KB_BUTTONS] = {};
	char						old_GPState[COUNT_GP_BUTTONS] = {};

	Fvector2 LeftAxis = { 0, 0 };
	Fvector2 RightAxis = { 0, 0 };

	Fvector2 AdaptiveTrigger = { 0, 0 };

	xr_vector<IInputReceiver*>	cbStack;

	void						NoInputUpdate				();
	void						MouseUpdate					();
	void						KeyboardUpdate				();
	void						GamepadUpdate				();

public:
	u32							dwCurTime;
	
	void						MouseMotion					(float dx, float dy);
	void						MouseScroll					(float d);
	void						MousePressed				(int button);
	void						MouseReleased				(int button);
		
	void                        KeyboardButtonUpdate        (SDL_Scancode scancode, bool IsPressed);
	
	void						GamepadButtonUpdate			(int SDLCode, bool IsPressed);
	void						LeftAxisUpdate				(bool IsX, float value);
	void						RightAxisUpdate				(bool IsX, float value);
	void						AdaptiveTriggerUpdate		(bool IsX, float value);

	void						iCapture					( IInputReceiver *pc );
	void						iRelease					( IInputReceiver *pc );
	BOOL						iGetAsyncKeyState			( int dik );
	BOOL						iGetAsyncBtnState			( int btn );
	void						iGetLastMouseDelta			( Ivector2& p );

	CInput						( BOOL bExclusive = true, int deviceForInit = default_key);
	~CInput						( );

	virtual void	_BCL		OnFrame						(void);
	virtual void				OnAppActivate				(void);
	virtual void				OnAppDeactivate				(void);

	IInputReceiver*				CurrentIR					();

public:
	bool IsAcquire = false;

	void						unacquire					();
	void						acquire						();
	bool						get_dik_name				(int dik, LPSTR dest, int dest_sz);

	void						feedback					(u16 s1, u16 s2, float time);
};

extern ENGINE_API CInput *		pInput;

#endif //__XR_INPUT__
