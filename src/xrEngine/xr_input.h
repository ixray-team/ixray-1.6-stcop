#ifndef __XR_INPUT__
#define __XR_INPUT__

#define DIRECTINPUT_VERSION 0x0800
#include <dinput.h>

class	ENGINE_API				IInputReceiver;

//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//описание класса
const int mouse_device_key		= 1;
const int keyboard_device_key	= 2;
const int all_device_key		= mouse_device_key | keyboard_device_key;
const int default_key			= mouse_device_key | keyboard_device_key ;

class ENGINE_API CInput
#ifndef M_BORLAND
	:
	public pureFrame,
	public pureAppActivate,
	public pureAppDeactivate
#endif
{
public:
	enum {
		COUNT_MOUSE_BUTTONS			= 8,
		COUNT_MOUSE_AXIS			= 3,
		COUNT_KB_BUTTONS			= 256
	};
private:
	bool						mouseMoved = false;
	bool						mouseScrolled = false;
	char						mouseState[COUNT_MOUSE_BUTTONS] = {};
	char						KBState[COUNT_KB_BUTTONS] = {};
	int 						offs[COUNT_MOUSE_AXIS] = {};
	char						old_mouseState[COUNT_MOUSE_BUTTONS] = {};
	char						old_KBState[COUNT_KB_BUTTONS] = {};


//	xr_stack<IInputReceiver*>	cbStack;
	xr_vector<IInputReceiver*>	cbStack;

	void						NoInputUpdate				( );
	void						MouseUpdate					( );
	void						KeyboardUpdate				( );

public:
	u32							dwCurTime;
	
	void						MouseMotion					(float dx, float dy);
	void						MouseScroll					(float d);
	void						MousePressed				(int button);
	void						MouseReleased				(int button);
	void						KeyPressed					(int SDLCode);	
	void						KeyReleased					(int SDLCode);	

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
			void				unacquire					();
			void				acquire						();
			bool				get_dik_name				(int dik, LPSTR dest, int dest_sz);

			void				feedback					(u16 s1, u16 s2, float time);
};

extern ENGINE_API CInput *		pInput;

#endif //__XR_INPUT__
