#include "stdafx.h"
#pragma hdrstop

#include "xr_input.h"
#include "IInputReceiver.h"
#include "../include/editor/ide.hpp"

#ifndef _EDITOR
#	include "xr_input_xinput.h"
#endif
CInput *	pInput	= NULL;
IInputReceiver		dummyController;

ENGINE_API float	psMouseSens			= 1.f;
ENGINE_API float	psMouseSensScale	= 1.f;
ENGINE_API Flags32	psMouseInvert		= {FALSE};

float stop_vibration_time				= flt_max;

#define MOUSEBUFFERSIZE			64
#define KEYBOARDBUFFERSIZE		64
#define _KEYDOWN(name,key)		( name[key] & 0x80 )


#define DECLARE_KEY_ENTRY(keyName) { (u8)keyName, xstring(#keyName)},

const xr_map<u8, xr_string> KeyNamesTable =
{
	{(u8)DIK_TAB,	  "Tab"},
	{(u8)DIK_RETURN,   "Enter"},
	{(u8)DIK_LSHIFT,    "Shift"},
	{(u8)DIK_LCONTROL,  "Ctrl"},
	{(u8)DIK_CAPITAL,  "Caps Lock"},
	{(u8)DIK_PAUSE,  "Pause"},
{
(u8)DIK_BACK,   "Backspace"
},
{
(u8)DIK_PRIOR,   "Pg Up"
},
{(u8)DIK_NEXT,   "Pg Down"},
{
(u8)DIK_HOME,   "Home"
},
{(u8)DIK_LEFT,	"Left"},
{(u8)DIK_UP,		"Up"},
{(u8)DIK_RIGHT,	"Right"},
{(u8)DIK_DOWN,	"Down"},
{(u8)DIK_SPACE, "Space"},
{(u8)DIK_ESCAPE, "Esc"},
{
(u8)DIK_INSERT,   "Insert"
},
{(u8)DIK_DELETE,   "Del"},
{
(u8)DIK_0, "0"
},
{(u8)DIK_1, "1"},
{(u8)DIK_2, "2"},
{(u8)DIK_3, "3"},
{(u8)DIK_4, "4"},
{(u8)DIK_5, "5"},
{(u8)DIK_6, "6"},
{(u8)DIK_7, "7"},
{(u8)DIK_8, "8"},
{(u8)DIK_9, "9"},
{(u8)DIK_A, "A"},
{(u8)DIK_B, "B"},
{(u8)DIK_C, "C"},
{(u8)DIK_D, "D"},
{(u8)DIK_E, "E"},
{(u8)DIK_F, "F"},
{(u8)DIK_G, "G"},
{(u8)DIK_H, "H"},
{(u8)DIK_I, "I"},
{(u8)DIK_J, "J"},
{(u8)DIK_K, "K"},
{(u8)DIK_L, "L"},
{(u8)DIK_M, "M"},
{(u8)DIK_N, "N"},
{(u8)DIK_O, "O"},
{(u8)DIK_P, "P"},
{(u8)DIK_Q, "Q"},
{(u8)DIK_R, "R"},
{(u8)DIK_S, "S"},
{(u8)DIK_T, "T"},
{(u8)DIK_U, "U"},
{(u8)DIK_V, "V"},
{(u8)DIK_W, "W"},
{(u8)DIK_X, "X"},
{(u8)DIK_Y, "Y"},
{(u8)DIK_Z, "Z"},
{(u8)DIK_LWIN, "lWin"},
{(u8)DIK_RWIN, "rWin"},
{(u8)DIK_SLEEP, "Sleep"},
{(u8)DIK_APPS, "Apps"},
{(u8)DIK_NUMPAD0,  "NumPad0"},
{(u8)DIK_NUMPAD1,  "NumPad1"},
{(u8)DIK_NUMPAD2,  "NumPad2"},
{(u8)DIK_NUMPAD3,  "NumPad3"},
{(u8)DIK_NUMPAD4,  "NumPad4"},
{(u8)DIK_NUMPAD5,  "NumPad5"},
{(u8)DIK_NUMPAD6,  "NumPad6"},
{(u8)DIK_NUMPAD7,  "NumPad7"},
{(u8)DIK_NUMPAD8,  "NumPad8"},
{(u8)DIK_NUMPAD9,  "NumPad9"},
{(u8)DIK_MULTIPLY,  "*"},
{(u8)DIK_ADD,  "+"},
{(u8)DIK_SUBTRACT,  "-"},
{(u8)DIK_DIVIDE,  "/"},
{
(u8)DIK_DECIMAL,   "."
},
{(u8)DIK_F1,  "F1"},
{(u8)DIK_F2,  "F2"},
{(u8)DIK_F3,  "F3"},
{(u8)DIK_F4,  "F4"},
{(u8)DIK_F5,  "F5"},
{(u8)DIK_F6,  "F6"},
{(u8)DIK_F7,  "F7"},
{(u8)DIK_F8,  "F8"},
{(u8)DIK_F9,  "F9"},
{(u8)DIK_F10, "F10"},
{(u8)DIK_F11, "F11"},
{(u8)DIK_F12, "F12"},
{(u8)DIK_F13, "F13"},
{(u8)DIK_F14, "F14"},
{(u8)DIK_F15, "F15"},
{(u8)DIK_NUMLOCK, "NumLock"},
{(u8)DIK_SCROLL, "ScrollLock"},
{(u8)DIK_LSHIFT,   "Left Shift"},
{(u8)DIK_RSHIFT,   "Right Shift"},
{(u8)DIK_LCONTROL, "Left Ctrl"},
{(u8)DIK_RCONTROL, "Right Ctrl"},
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

CInput::~CInput(void)
{
#ifdef ENGINE_BUILD
	Device.seqFrame.Remove			(this);
	Device.seqAppDeactivate.Remove	(this);
	Device.seqAppActivate.Remove	(this);
#endif

}

//-----------------------------------------------------------------------

void						
CInput::MouseMotion(float dx, float dy)
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

void					
CInput::MousePressed(int button)
{
	mouseState[button] = 1;
}

void				
CInput::MouseReleased(int button)
{
	mouseState[button] = 0;
}

void
CInput::KeyPressed(int SDLCode)
{
	auto Key = ToDIK(SDLCode);
	KBState[Key] = 1;
}

void
CInput::KeyReleased(int SDLCode)
{
	auto Key = ToDIK(SDLCode);
	KBState[Key] = 0;
}

void CInput::KeyboardUpdate( )
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
	
	//std::memcpy(old_KBState, KBState, sizeof(KBState));
}

const xr_map<int, char> russian_lookup_key_table = {
	{ DIK_F, 0xE0 },
	{ DIK_COMMA, 0xE1 },
	{ DIK_D, 0xE2 },
	{ DIK_U, 0xE3 },
	{ DIK_L, 0xE4 },
	{ DIK_T, 0xE5 },
	{ DIK_SEMICOLON, 0xE6 },
	{ DIK_P, 0xE7 },
	{ DIK_B, 0xE8 },
	{ DIK_Q, 0xE9 },
	{ DIK_R, 0xEA },
	{ DIK_K, 0xEB },
	{ DIK_V, 0xEC },
	{ DIK_Y, 0xED }, 
	{ DIK_J, 0xEE }, 
	{ DIK_G, 0xEF }, 	
	
	{ DIK_H, 0xF0 },
	{ DIK_C, 0xF1 },
	{ DIK_N, 0xF2 },
	{ DIK_E, 0xF3 },
	{ DIK_A, 0xF4 },
	{ DIK_LBRACKET, 0xF5 },
	{ DIK_W, 0xF6 },
	{ DIK_X, 0xF7 },
	{ DIK_I, 0xF8 },
	{ DIK_O, 0xF9 },
	{ DIK_RBRACKET, 0xFA },
	{ DIK_S, 0xFB },
	{ DIK_M, 0xFC },
	{ DIK_APOSTROPHE, 0xFD },
	{ DIK_PERIOD, 0xFE },
	{ DIK_Z, 0xFF }, 
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
	for (size_t i = 0; i < COUNT_KB_BUTTONS; i++) {
		bool Pressed = !!KBState[i];
		if (KBState[i] != old_KBState[i]) {
			if (!Pressed) {
				cbStack.back()->IR_OnKeyboardRelease((int)i);
			}

			old_KBState[i] = KBState[i];
		}
	}

	for (size_t i = 0; i < COUNT_MOUSE_BUTTONS; i++) {
		bool Pressed = !!mouseState[i];
		if (mouseState[i] != old_mouseState[i]) {
			if (!Pressed) {
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

	if (KBState[DIK_LALT] || Device.IsCapturingInputs()) {
		NoInputUpdate();
	} else {
		MouseUpdate();
		KeyboardUpdate();
	}

    // change focus
	if (!cbStack.empty())
		cbStack.back()->IR_OnDeactivate();
	cbStack.push_back(p);
	cbStack.back()->IR_OnActivate();
}
void						 
CInput::iGetLastMouseDelta(Ivector2& p)
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

void CInput::OnFrame			(void)
{
	RDEVICE.Statistic->Input.Begin();
	dwCurTime = RDEVICE.TimerAsync_MMT();
	if (KBState[DIK_LALT] || Device.IsCapturingInputs()) {
		NoInputUpdate();
	} else {
		MouseUpdate();
		KeyboardUpdate();
	}

	RDEVICE.Statistic->Input.End();
}

IInputReceiver*	 CInput::CurrentIR()
{
	if(cbStack.size())
		return cbStack.back();
	else
		return NULL;
}

void CInput::unacquire()
{
	SDL_SetRelativeMouseMode(false);
}

void CInput::acquire()
{
	SDL_SetRelativeMouseMode(true);
}

void  CInput::feedback(u16 s1, u16 s2, float time)
{
	stop_vibration_time = RDEVICE.fTimeGlobal + time;
#ifndef _EDITOR
//.	set_vibration (s1, s2);
#endif
}
