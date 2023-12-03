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
#endif // #ifdef INGAME_EDITOR
	if (!pInput || !g_exclusive)
		return;

	if (before) {
		pInput->unacquire	();
		return;
	}

	pInput->acquire			(true);
}

CInput::CInput						( BOOL bExclusive, int deviceForInit)
{
	g_exclusive							= !!bExclusive;

	Log("Starting INPUT device...");

	ZeroMemory							( mouseState,	sizeof(mouseState) );
	ZeroMemory							( KBState,		sizeof(KBState) );

	//===================== Dummy pack
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
	//_______________________

}

//-----------------------------------------------------------------------

void CInput::SetAllAcquire( BOOL bAcquire )
{

}

void CInput::SetMouseAcquire( BOOL bAcquire )
{

}
void CInput::SetKBDAcquire( BOOL bAcquire )
{

}

void						
CInput::MouseMotion(float dx, float dy, float scroll)
{
	mouseMoved = true;
	offs[0] += dx;
	offs[1] += dy;
	offs[2] = scroll;
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

//-----------------------------------------------------------------------
BOOL b_altF4 = FALSE;
void CInput::KeyboardUpdate( )
{
	if (b_altF4) { 
		return;
	}

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
	std::memcpy(old_mouseState, mouseState, sizeof(mouseState));

	bool b_alt_tab				= false;
	if (!b_altF4 && KBState[DIK_F4] && (KBState[DIK_RMENU] || KBState[DIK_LMENU])) {
		b_altF4				= TRUE;
		g_pEventManager->Event.Defer	("KERNEL:disconnect");
		g_pEventManager->Event.Defer	("KERNEL:quit");
	}

	if (b_altF4) { 
		return;
	}

	if (b_alt_tab) {
		SDL_MinimizeWindow(g_AppInfo.Window);
	}
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
			}
			else {
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
		//cbStack.back()->IR_OnMouseWheel(offs[2]);
		mouseMoved = false;
	}
	offs[0] = offs[1] = offs[2] = 0;

#if 0
	HRESULT hr;
	DWORD dwElements	= MOUSEBUFFERSIZE;
	DIDEVICEOBJECTDATA	od[MOUSEBUFFERSIZE];

	VERIFY(pMouse);

	hr = pMouse->GetDeviceData( sizeof(DIDEVICEOBJECTDATA), &od[0], &dwElements, 0 );
	if (( hr == DIERR_INPUTLOST )||( hr == DIERR_NOTACQUIRED )){
		hr = pMouse->Acquire();
		if ( hr != S_OK ) return;
		hr = pMouse->GetDeviceData( sizeof(DIDEVICEOBJECTDATA), &od[0], &dwElements, 0 );
		if ( hr != S_OK ) return;
	};

	#ifndef _EDITOR
    #endif
	BOOL				mouse_prev[COUNT_MOUSE_BUTTONS];

	mouse_prev[0]		= mouseState[0];
	mouse_prev[1]		= mouseState[1];
	mouse_prev[2]		= mouseState[2];
	mouse_prev[3]		= mouseState[3];
	mouse_prev[4]		= mouseState[4];
	mouse_prev[5]		= mouseState[5];
	mouse_prev[6]		= mouseState[6];
	mouse_prev[7]		= mouseState[7];

	offs[0] = offs[1] = offs[2] = 0;
	for (u32 i = 0; i < dwElements; i++){
		switch (od[i].dwOfs){
		case DIMOFS_X:	offs[0]	+= od[i].dwData; timeStamp[0] = od[i].dwTimeStamp;	break;
		case DIMOFS_Y:	offs[1]	+= od[i].dwData; timeStamp[1] = od[i].dwTimeStamp;	break;
		case DIMOFS_Z:	offs[2]	+= od[i].dwData; timeStamp[2] = od[i].dwTimeStamp;	break;
		case DIMOFS_BUTTON0:
			if ( od[i].dwData & 0x80 )	
			{ mouseState[0] = TRUE;				cbStack.back()->IR_OnMousePress(0);		}
			if ( !(od[i].dwData & 0x80))
			{ mouseState[0] = FALSE;			cbStack.back()->IR_OnMouseRelease(0);	}
			break;
		case DIMOFS_BUTTON1:
			if ( od[i].dwData & 0x80 )	
			{ mouseState[1] = TRUE;				cbStack.back()->IR_OnMousePress(1);		}
			if ( !(od[i].dwData & 0x80))
			{ mouseState[1] = FALSE;			cbStack.back()->IR_OnMouseRelease(1);	}
			break;
		case DIMOFS_BUTTON2:
			if ( od[i].dwData & 0x80 )	
			{ mouseState[2] = TRUE;				cbStack.back()->IR_OnMousePress(2);		}
			if ( !(od[i].dwData & 0x80))
			{ mouseState[2] = FALSE;			cbStack.back()->IR_OnMouseRelease(2);	}
			break;
		case DIMOFS_BUTTON3:
			if ( od[i].dwData & 0x80 )	
			{ mouseState[3] = TRUE;				cbStack.back()->IR_OnKeyboardPress(0xED + 103);		}
			if ( !(od[i].dwData & 0x80))
			{ mouseState[3] = FALSE;			cbStack.back()->IR_OnKeyboardRelease(0xED + 103);	}
			break;
		case DIMOFS_BUTTON4:
			if ( od[i].dwData & 0x80 )	
			{ mouseState[4] = TRUE;				cbStack.back()->IR_OnKeyboardPress(0xED + 104);		}
			if ( !(od[i].dwData & 0x80))
			{ mouseState[4] = FALSE;			cbStack.back()->IR_OnKeyboardRelease(0xED + 104);	}
			break;
		case DIMOFS_BUTTON5:
			if ( od[i].dwData & 0x80 )	
			{ mouseState[5] = TRUE;				cbStack.back()->IR_OnKeyboardPress(0xED + 105);		}
			if ( !(od[i].dwData & 0x80))
			{ mouseState[5] = FALSE;			cbStack.back()->IR_OnKeyboardRelease(0xED + 105);	}
			break;
		case DIMOFS_BUTTON6:
			if ( od[i].dwData & 0x80 )	
			{ mouseState[6] = TRUE;				cbStack.back()->IR_OnKeyboardPress(0xED + 106);		}
			if ( !(od[i].dwData & 0x80))
			{ mouseState[6] = FALSE;			cbStack.back()->IR_OnKeyboardRelease(0xED + 106);	}
			break;
		case DIMOFS_BUTTON7:
			if ( od[i].dwData & 0x80 )	
			{ mouseState[7] = TRUE;				cbStack.back()->IR_OnKeyboardPress(0xED + 107);		}
			if ( !(od[i].dwData & 0x80))
			{ mouseState[7] = FALSE;			cbStack.back()->IR_OnKeyboardRelease(0xED + 107);	}
			break;
		}
	}

	if (mouseState[0] && mouse_prev[0])
	{
		cbStack.back()->IR_OnMouseHold(0);
	}

	if (mouseState[1] && mouse_prev[1])		
	{
		cbStack.back()->IR_OnMouseHold(1);
	}

	if (mouseState[2] && mouse_prev[2])		
	{
		cbStack.back()->IR_OnMouseHold(2);
	}
	if ( dwElements ){
		if (offs[0] || offs[1]) cbStack.back()->IR_OnMouseMove	( offs[0], offs[1] );
		if (offs[2])			cbStack.back()->IR_OnMouseWheel	( offs[2] );
	} else {
		if (timeStamp[1] && ((dwCurTime-timeStamp[1])>=mouse_property.mouse_dt))	cbStack.back()->IR_OnMouseStop(DIMOFS_Y, timeStamp[1] = 0);
		if (timeStamp[0] && ((dwCurTime-timeStamp[0])>=mouse_property.mouse_dt))	cbStack.back()->IR_OnMouseStop(DIMOFS_X, timeStamp[0] = 0);
	}
#endif
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

	SetAllAcquire	( true );
	ZeroMemory		( mouseState,	sizeof(mouseState) );
	ZeroMemory		( KBState,		sizeof(KBState) );
}

void CInput::OnAppDeactivate	(void)
{
	if (CurrentIR())
		CurrentIR()->IR_OnDeactivate();

	SetAllAcquire	( false );
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

void CInput::unacquire				()
{
}

void CInput::acquire				(const bool &exclusive)
{

}

void CInput::exclusive_mode			(const bool &exclusive)
{
	g_exclusive						= exclusive;
	unacquire						();
	acquire							(exclusive);
}
bool CInput::get_exclusive_mode		()
{
	return g_exclusive;
}

void  CInput::feedback(u16 s1, u16 s2, float time)
{
	stop_vibration_time = RDEVICE.fTimeGlobal + time;
#ifndef _EDITOR
//.	set_vibration (s1, s2);
#endif
}
