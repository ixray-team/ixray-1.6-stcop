#include "stdafx.h"
#pragma hdrstop

#include "xr_input.h"
#include "iinputreceiver.h"

void	IInputReceiver::IR_Capture						(void)
{
	VERIFY(pInput);
	pInput->iCapture(this);
}

void	IInputReceiver::IR_Release						(void)
{
	VERIFY(pInput);
	pInput->iRelease(this);
}

void	IInputReceiver::IR_GetLastMouseDelta			(Ivector2& p)
{
	VERIFY(pInput);
	pInput->iGetLastMouseDelta( p );
}

void IInputReceiver::IR_OnDeactivate					(void)
{
	int i;
	for (i = 0; i < CInput::COUNT_KB_BUTTONS; i++ )
		if (IR_GetKeyState(i)) {	
			IR_OnKeyboardRelease(i);
		}
	for (i = 0; i < CInput::COUNT_MOUSE_BUTTONS; i++ )
		if (IR_GetBtnState(i)) {
			IR_OnMouseRelease(i);
		}
}

void IInputReceiver::IR_OnActivate(void)
{
}

BOOL IInputReceiver::IR_GetKeyState(int dik)
{
	VERIFY(pInput);
	return pInput->iGetAsyncKeyState(dik);
}

BOOL IInputReceiver::IR_GetBtnState(int btn)
{
	VERIFY(pInput);
	return pInput->iGetAsyncBtnState(btn);
}

void	IInputReceiver::IR_GetMousePosScreen(Ivector2& p)
{
	int mouse_x, mouse_y;
	SDL_GetGlobalMouseState(&mouse_x, &mouse_y);
	p.x = mouse_x;
	p.y = mouse_y;
}
void	IInputReceiver::IR_GetMousePosReal(Ivector2& p)
{
	int mouse_x, mouse_y;
	SDL_GetMouseState(&mouse_x, &mouse_y);
	p.x = mouse_x;
	p.y = mouse_y;
}
void	IInputReceiver::IR_GetMousePosIndependent		(Fvector2 &f)
{
	Ivector2 p;
	IR_GetMousePosReal(p);
	f.set(
		2.f*float(p.x)/float(Device.TargetWidth)-1.f,
		2.f*float(p.y)/float(Device.TargetHeight)-1.f
		);
}
void	IInputReceiver::IR_GetMousePosIndependentCrop	(Fvector2 &f)
{
	IR_GetMousePosIndependent(f);
	if (f.x<-1.f) f.x=-1.f;
	if (f.x> 1.f) f.x= 1.f;
	if (f.y<-1.f) f.y=-1.f;
	if (f.y> 1.f) f.y= 1.f;
}
