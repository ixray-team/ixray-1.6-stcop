#include "stdafx.h"
#pragma hdrstop

#include "xr_input.h"
#include "IInputReceiver.h"

CInput *	pInput	= nullptr;
IInputReceiver		dummyController;

ENGINE_API float	psMouseSens			= 1.f;
ENGINE_API float	psMouseSensScale	= 1.f;
ENGINE_API Flags32	psMouseInvert		= {FALSE};

float stop_vibration_time				= flt_max;

#define MOUSEBUFFERSIZE			64
#define KEYBOARDBUFFERSIZE		64
#define _KEYDOWN(name,key)		( name[key] & 0x80 )


#define DECLARE_KEY_ENTRY(keyName) { (u8)keyName, xstring(#keyName)},

const xr_hash_map<u8, xr_string> KeyNamesTable =
{
	{(u8)SDL_SCANCODE_TAB,		"Tab"},
	{(u8)SDL_SCANCODE_RETURN,	"Enter"},
	{(u8)SDL_SCANCODE_LSHIFT,    "Shift"},
	{(u8)SDL_SCANCODE_LCTRL,  "Ctrl"},
	{(u8)SDL_SCANCODE_CAPSLOCK,	"Caps Lock"},
	{(u8)SDL_SCANCODE_PAUSE,		"Pause"},
	{(u8)SDL_SCANCODE_BACKSPACE,		"Backspace"},
	{(u8)SDL_SCANCODE_PAGEUP,		"Pg Up"},
	{(u8)SDL_SCANCODE_PAGEDOWN,		"Pg Down"},
	{(u8)SDL_SCANCODE_HOME,		"Home"},
	{(u8)SDL_SCANCODE_LEFT,		"Left"},
	{(u8)SDL_SCANCODE_UP,		"Up"},
	{(u8)SDL_SCANCODE_RIGHT,		"Right"},
	{(u8)SDL_SCANCODE_DOWN,		"Down"},
	{(u8)SDL_SCANCODE_SPACE,		"Space"},
	{(u8)SDL_SCANCODE_ESCAPE,	"Esc"},
	{(u8)SDL_SCANCODE_INSERT,	"Insert"},
	{(u8)SDL_SCANCODE_DELETE,	"Del"},
	{(u8)SDL_SCANCODE_0,			"0"},
	{(u8)SDL_SCANCODE_1,			"1"},
	{(u8)SDL_SCANCODE_2,			"2"},
	{(u8)SDL_SCANCODE_3,			"3"},
	{(u8)SDL_SCANCODE_4,			"4"},
	{(u8)SDL_SCANCODE_5,			"5"},
	{(u8)SDL_SCANCODE_6,			"6"},
	{(u8)SDL_SCANCODE_7,			"7"},
	{(u8)SDL_SCANCODE_8,			"8"},
	{(u8)SDL_SCANCODE_9,			"9"},
	{(u8)SDL_SCANCODE_A,			"A"},
	{(u8)SDL_SCANCODE_B,			"B"},
	{(u8)SDL_SCANCODE_C,			"C"},
	{(u8)SDL_SCANCODE_D,			"D"},
	{(u8)SDL_SCANCODE_E,			"E"},
	{(u8)SDL_SCANCODE_F,			"F"},
	{(u8)SDL_SCANCODE_G,			"G"},
	{(u8)SDL_SCANCODE_H,			"H"},
	{(u8)SDL_SCANCODE_I,			"I"},
	{(u8)SDL_SCANCODE_J,			"J"},
	{(u8)SDL_SCANCODE_K,			"K"},
	{(u8)SDL_SCANCODE_L,			"L"},
	{(u8)SDL_SCANCODE_M,			"M"},
	{(u8)SDL_SCANCODE_N,			"N"},
	{(u8)SDL_SCANCODE_O,			"O"},
	{(u8)SDL_SCANCODE_P,			"P"},
	{(u8)SDL_SCANCODE_Q,			"Q"},
	{(u8)SDL_SCANCODE_R,			"R"},
	{(u8)SDL_SCANCODE_S,			"S"},
	{(u8)SDL_SCANCODE_T,			"T"},
	{(u8)SDL_SCANCODE_U,			"U"},
	{(u8)SDL_SCANCODE_V,			"V"},
	{(u8)SDL_SCANCODE_W,			"W"},
	{(u8)SDL_SCANCODE_X,			"X"},
	{(u8)SDL_SCANCODE_Y,			"Y"},
	{(u8)SDL_SCANCODE_Z,			"Z"},
	{(u8)SDL_SCANCODE_LGUI,       "lWin"},
	{(u8)SDL_SCANCODE_RGUI,       "lWin"},
	{(u8)SDL_SCANCODE_SLEEP,      "Sleep"},
	{(u8)SDL_SCANCODE_APPLICATION,"Apps"},
	{(u8)SDL_SCANCODE_KP_0,       "NumPad0"},
	{(u8)SDL_SCANCODE_KP_1,       "NumPad1"},
	{(u8)SDL_SCANCODE_KP_2,       "NumPad2"},
	{(u8)SDL_SCANCODE_KP_3,       "NumPad3"},
	{(u8)SDL_SCANCODE_KP_4,       "NumPad4"},
	{(u8)SDL_SCANCODE_KP_5,       "NumPad5"},
	{(u8)SDL_SCANCODE_KP_6,       "NumPad6"},
	{(u8)SDL_SCANCODE_KP_7,       "NumPad7"},
	{(u8)SDL_SCANCODE_KP_8,       "NumPad8"},
	{(u8)SDL_SCANCODE_KP_9,       "NumPad9"},
	{(u8)SDL_SCANCODE_KP_MULTIPLY,"*"},
	{(u8)SDL_SCANCODE_KP_PLUS,    "+"},
	{(u8)SDL_SCANCODE_KP_MINUS,   "-"},
	{(u8)SDL_SCANCODE_KP_DIVIDE,  "/"},
	{(u8)SDL_SCANCODE_KP_DECIMAL, "."},
	{(u8)SDL_SCANCODE_F1,		"F1"},
	{(u8)SDL_SCANCODE_F2,		"F2"},
	{(u8)SDL_SCANCODE_F3,		"F3"},
	{(u8)SDL_SCANCODE_F4,		"F4"},
	{(u8)SDL_SCANCODE_F5,		"F5"},
	{(u8)SDL_SCANCODE_F6,		"F6"},
	{(u8)SDL_SCANCODE_F7,		"F7"},
	{(u8)SDL_SCANCODE_F8,		"F8"},
	{(u8)SDL_SCANCODE_F9,		"F9"},
	{(u8)SDL_SCANCODE_F10,		"F10"},
	{(u8)SDL_SCANCODE_F11,		"F11"},
	{(u8)SDL_SCANCODE_F12,		"F12"},
	{(u8)SDL_SCANCODE_F13,		"F13"},
	{(u8)SDL_SCANCODE_F14,		"F14"},
	{(u8)SDL_SCANCODE_F15,		"F15"},
	{(u8)SDL_SCANCODE_NUMLOCKCLEAR,	"NumLock"},
	{(u8)SDL_SCANCODE_SCROLLLOCK,	"ScrollLock"},
	{(u8)SDL_SCANCODE_LSHIFT,	"Left Shift"},
	{(u8)SDL_SCANCODE_RSHIFT,	"Right Shift"},
	{(u8)SDL_SCANCODE_LCTRL,	"Left Ctrl"},
	{(u8)SDL_SCANCODE_RCTRL,	"Right Ctrl"},
};

static bool g_exclusive	= true;
static void on_error_dialog			(bool before)
{
	if (!pInput || !g_exclusive)
		return;

	if (before) {
		pInput->unacquire	();
		return;
	}

	pInput->acquire();
}

CInput::CInput						( BOOL bExclusive, int deviceForInit)
{
	g_exclusive							= !!bExclusive;

	Log("Starting INPUT device...");

	ZeroMemory							( mouseState,	sizeof(mouseState) );
	ZeroMemory							( KBState,		sizeof(KBState) );

	iCapture	(&dummyController);
	Debug.set_on_dialog				(&on_error_dialog);

#ifdef ENGINE_BUILD
	Device.seqAppActivate.Add		(this);
	Device.seqAppDeactivate.Add		(this, REG_PRIORITY_HIGH);
	Device.seqFrame.Add				(this, REG_PRIORITY_HIGH);
#endif
}

CInput::~CInput()
{
	if (pGamePad != nullptr)
		SDL_CloseGamepad(pGamePad);

#ifdef ENGINE_BUILD
	Device.seqFrame.Remove			(this);
	Device.seqAppDeactivate.Remove	(this);
	Device.seqAppActivate.Remove	(this);
#endif

}

//-----------------------------------------------------------------------

void CInput::MouseMotion(float dx, float dy)
{
	mouseMoved = true;
	offs[0] += (int)dx;
	offs[1] += (int)dy;
}

void CInput::MouseScroll(float d)
{
	mouseScrolled = true;
	offs[2] += (int)d;
}

void CInput::MousePressed(int button)
{
	mouseState[button] = 1;
}

void CInput::MouseReleased(int button)
{
	mouseState[button] = 0;
}

void CInput::KeyboardButtonUpdate(SDL_Scancode scancode, bool IsPressed)
{
	KBState[scancode] = IsPressed;
}

void CInput::GamepadButtonUpdate(int SDLCode, bool IsPressed)
{
	GPState[SDLCode] = IsPressed;
}

void CInput::LeftAxisUpdate(bool IsX, float value)
{
	if (IsX)
	{
		LeftAxis.x = value;
	}
	else
	{
		LeftAxis.y = value * -1;
	}
}

void CInput::RightAxisUpdate(bool IsX, float value)
{
	if (IsX)
	{
		RightAxis.x = value;
	}
	else
	{
		RightAxis.y = value;
	}
}

void CInput::AdaptiveTriggerUpdate(bool IsX, float value)
{
	if (IsX)
	{
		AdaptiveTrigger.x = value;
	}
	else
	{
		AdaptiveTrigger.y = value;
	}
}

void CInput::KeyboardUpdate()
{
	for (size_t i = 0; i < COUNT_KB_BUTTONS; i++)
	{
		bool Pressed = !!KBState[i];
		if (KBState[i] != old_KBState[i])
		{
			old_KBState[i] = KBState[i];
			if (Pressed)
			{
				cbStack.back()->IR_OnKeyboardPress((int)i);
			}
			else
			{
				cbStack.back()->IR_OnKeyboardRelease((int)i);
			}
		}
	}

	for (int i = 0; i < COUNT_KB_BUTTONS; i++)
	{
		if (KBState[i])
		{
			cbStack.back()->IR_OnKeyboardHold((int)i);
		}
	}
}

bool dsEnableGamepad = false;
void CInput::GamepadUpdate()
{
	if (pGamePad == nullptr || !dsEnableGamepad)
		return;

	if (cbStack.empty())
		return;

	auto KeyHolder = cbStack.back();

	KeyHolder->IR_GamepadUpdateStick(0, LeftAxis);
	KeyHolder->IR_GamepadUpdateStick(1, RightAxis);

	KeyHolder->IR_GamepadUpdateStick(2, AdaptiveTrigger);

	for (size_t i = 0; i < COUNT_GP_BUTTONS; i++)
	{
		bool Pressed = !!GPState[i];
		if (GPState[i] != old_GPState[i])
		{
			old_GPState[i] = GPState[i];

			if (Pressed)
			{
				cbStack.back()->IR_GamepadKeyPress((int)i);
			}
			else
			{
				cbStack.back()->IR_GamepadKeyRelease((int)i);
			}
		}
	}
}

const xr_map<int, char> russian_lookup_key_table = {
	{ SDL_SCANCODE_F, 0xE0 },
	{ SDL_SCANCODE_COMMA, 0xE1 },
	{ SDL_SCANCODE_D, 0xE2 },
	{ SDL_SCANCODE_U, 0xE3 },
	{ SDL_SCANCODE_L, 0xE4 },
	{ SDL_SCANCODE_T, 0xE5 },
	{ SDL_SCANCODE_SEMICOLON, 0xE6 },
	{ SDL_SCANCODE_P, 0xE7 },
	{ SDL_SCANCODE_B, 0xE8 },
	{ SDL_SCANCODE_Q, 0xE9 },
	{ SDL_SCANCODE_R, 0xEA },
	{ SDL_SCANCODE_K, 0xEB },
	{ SDL_SCANCODE_V, 0xEC },
	{ SDL_SCANCODE_Y, 0xED }, 
	{ SDL_SCANCODE_J, 0xEE }, 
	{ SDL_SCANCODE_G, 0xEF }, 	
	
	{ SDL_SCANCODE_H, 0xF0 },
	{ SDL_SCANCODE_C, 0xF1 },
	{ SDL_SCANCODE_N, 0xF2 },
	{ SDL_SCANCODE_E, 0xF3 },
	{ SDL_SCANCODE_A, 0xF4 },
	{ SDL_SCANCODE_LEFTBRACKET, 0xF5 },
	{ SDL_SCANCODE_W, 0xF6 },
	{ SDL_SCANCODE_X, 0xF7 },
	{ SDL_SCANCODE_I, 0xF8 },
	{ SDL_SCANCODE_O, 0xF9 },
	{ SDL_SCANCODE_RIGHTBRACKET, 0xFA },
	{ SDL_SCANCODE_S, 0xFB },
	{ SDL_SCANCODE_M, 0xFC },
	{ SDL_SCANCODE_APOSTROPHE, 0xFD },
	{ SDL_SCANCODE_PERIOD, 0xFE },
	{ SDL_SCANCODE_Z, 0xFF }, 
};

bool CInput::get_dik_name(int dik, LPSTR dest_str, int dest_sz)
{
	LANGID lang_locale = PRIMARYLANGID(LOWORD(HandleToLong(GetKeyboardLayout(0))));
	if (lang_locale != LANG_RUSSIAN) {
		return false;
	}

	if (!russian_lookup_key_table.contains(dik)) {
		return false;
	}

	char sym = russian_lookup_key_table.at(dik);
	dest_str[0] = sym;
	dest_str[1] = 0;

	return true;
}

#define MOUSE_1		(0xED + 100)
#define MOUSE_8		(0xED + 107)

BOOL CInput::iGetAsyncKeyState( int dik )
{
	if(dik<COUNT_KB_BUTTONS)
		return !!KBState[dik];
	else
	if(dik>=MOUSE_1 && dik<=MOUSE_8)
	{
		int mk = dik-MOUSE_1;
		return iGetAsyncBtnState(mk);
	}else
		return FALSE; //unknown key ???
}

BOOL CInput::iGetAsyncBtnState( int btn )
{
	return !!mouseState[btn];
}

#pragma warning(push)
#pragma warning(disable: 4644)
void CInput::NoInputUpdate()
{
	for (size_t i = 0; i < COUNT_KB_BUTTONS; i++) 
	{
		bool Pressed = !!KBState[i];
		if (KBState[i] != old_KBState[i])
		{
			if (!Pressed) 
			{
				cbStack.back()->IR_OnKeyboardRelease((int)i);
			}

			old_KBState[i] = KBState[i];
		}
	}

	for (size_t i = 0; i < COUNT_MOUSE_BUTTONS; i++) 
	{
		bool Pressed = !!mouseState[i];
		if (mouseState[i] != old_mouseState[i]) 
		{
			if (!Pressed) 
			{
				cbStack.back()->IR_OnMouseRelease((int)i);
			}

			old_mouseState[i] = mouseState[i];
		}
	}

	offs[0] = offs[1] = offs[2] = 0;
}

void CInput::MouseUpdate( )
{
	if (Device.dwPrecacheFrame)
		return;

	for (size_t i = 0; i < COUNT_MOUSE_BUTTONS; i++) {
		bool Pressed = !!mouseState[i];
		if (mouseState[i] != old_mouseState[i]) {
			if (Pressed) {
				cbStack.back()->IR_OnMousePress((int)i);
			} else {
				cbStack.back()->IR_OnMouseRelease((int)i);
			}
		}
	}

	for (int i = 0; i < COUNT_MOUSE_BUTTONS; i++) {
		if (mouseState[i] && old_mouseState[i]) {
			cbStack.back()->IR_OnMouseHold(i);
		}
	}

	if (mouseMoved) {
		cbStack.back()->IR_OnMouseMove(offs[0], offs[1]);
		mouseMoved = false;
	}

	if (mouseScrolled) {
		cbStack.back()->IR_OnMouseWheel(offs[2]);
		mouseScrolled = false;
	}

	std::memcpy(old_mouseState, mouseState, sizeof(mouseState));
	offs[0] = offs[1] = offs[2] = 0;
}

#pragma warning(pop)

//-------------------------------------------------------
void CInput::iCapture(IInputReceiver *p)
{
	VERIFY(p);
#ifndef _EDITOR
	if (KBState[SDL_SCANCODE_LALT] || CImGuiManager::Instance().IsCapturingInputs())
	{
		NoInputUpdate();
	} 
	else 
#endif
	{
		MouseUpdate();
		GamepadUpdate();
		KeyboardUpdate();
	}

    // change focus
	if (!cbStack.empty())
		cbStack.back()->IR_OnDeactivate();

	cbStack.push_back(p);
	cbStack.back()->IR_OnActivate();
}

void CInput::iGetLastMouseDelta(Ivector2& p)
{
	R_ASSERT(false);
}

void CInput::iRelease(IInputReceiver *p)
{
	if (p == cbStack.back())
	{
		cbStack.back()->IR_OnDeactivate();
		cbStack.pop_back();
		IInputReceiver * ir = cbStack.back();
		ir->IR_OnActivate();
	}else{// we are not topmost receiver, so remove the nearest one
		u32 cnt = (u32)cbStack.size();
		for(;cnt>0;--cnt)
			if( cbStack[cnt-1] == p ){
				xr_vector<IInputReceiver*>::iterator it = cbStack.begin();
				std::advance	(it,cnt-1);
				cbStack.erase	(it);
				break;
			}
	}
}

void CInput::OnAppActivate		(void)
{
	if (CurrentIR())
		CurrentIR()->IR_OnActivate();

	acquire();

	ZeroMemory		( mouseState,	sizeof(mouseState) );
	ZeroMemory		( KBState,		sizeof(KBState) );
}

void CInput::OnAppDeactivate	(void)
{
	if (CurrentIR())
		CurrentIR()->IR_OnDeactivate();

	unacquire();

	ZeroMemory		( mouseState,	sizeof(mouseState) );
	ZeroMemory		( KBState,		sizeof(KBState) );
}

void CInput::OnFrame()
{
	CScopeTimer Input(RDEVICE.Statistic->Input);

	dwCurTime = RDEVICE.TimerAsync_MMT();
#if !defined(_EDITOR) && !defined(MASTER_GOLD)
	if (KBState[SDL_SCANCODE_LALT] || CImGuiManager::Instance().IsCapturingInputs())
	{
		NoInputUpdate();
	} 
	else 
#endif
	{
		MouseUpdate();
		GamepadUpdate();
		KeyboardUpdate();
	}
}

IInputReceiver* CInput::CurrentIR()
{
	if(cbStack.size())
		return cbStack.back();
	else
		return nullptr;
}

void CInput::unacquire()
{
	SDL_SetRelativeMouseMode(false);
	IsAcquire = false;
}

void CInput::acquire()
{
	IsAcquire = true;
	SDL_SetRelativeMouseMode(true);
}

void  CInput::feedback(u16 s1, u16 s2, float time)
{
	stop_vibration_time = RDEVICE.fTimeGlobal + time;
#ifndef _EDITOR
//.	set_vibration (s1, s2);
#endif
}
