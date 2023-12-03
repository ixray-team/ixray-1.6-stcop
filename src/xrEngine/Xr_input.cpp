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

static bool g_exclusive	= true;
static void on_error_dialog			(bool before)
{
#ifdef INGAME_EDITOR
	if (Device.editor())
		return;
#endif
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
	offs[0] += dx;
	offs[1] += dy;
}

void CInput::MouseScroll(float d)
{
	mouseScrolled = true;
	offs[2] += d;
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
	for (size_t i = 0; i < COUNT_KB_BUTTONS; i++) {
		bool Pressed = !!KBState[i];
		if (KBState[i] != old_KBState[i]) {
			if (Pressed) {
				cbStack.back()->IR_OnKeyboardPress(i);
			} else {
				cbStack.back()->IR_OnKeyboardRelease(i);
			}
		}
	}

	for (int i = 0; i < COUNT_KB_BUTTONS; i++) {
		if (KBState[i]) {
			cbStack.back()->IR_OnKeyboardHold(i);
		}
	}	
	
	std::memcpy(old_KBState, KBState, sizeof(KBState));
}

bool CInput::get_dik_name(int dik, LPSTR dest_str, int dest_sz)
{
	// TODO:
	//std::memcpy(dest_str, "DICK", 5);
	return false;
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
void CInput::MouseUpdate( )
{
	if (Device.dwPrecacheFrame)
		return;

	for (size_t i = 0; i < COUNT_MOUSE_BUTTONS; i++) {
		bool Pressed = !!mouseState[i];
		if (KBState[i] != old_mouseState[i]) {
			if (Pressed) {
				cbStack.back()->IR_OnMousePress(i);
			} else {
				cbStack.back()->IR_OnMouseRelease(i);
			}
		}
	}

	for (int i = 0; i < COUNT_MOUSE_BUTTONS; i++) {
		if (mouseState[i]) {
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
	MouseUpdate();
	KeyboardUpdate();

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
	KeyboardUpdate();
	MouseUpdate();
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
