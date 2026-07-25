#include "stdafx.h"
#include "string_table.h"

#include "xr_input.h"
#include "IInputReceiver.h"
#include "GamepadService.h"

CInput *	pInput	= nullptr;
IInputReceiver		dummyController;

ENGINE_API float	psMouseSens			= 0.12f;
ENGINE_API float	psMouseUISens		= 1.f;
ENGINE_API float	psMouseSensScale	= 1.f;
ENGINE_API bool		psMouseInvert		= false;

ENGINE_API float	psGamepadSens		= 0.3f;
ENGINE_API bool		psGamepadInvert		= false;

ENGINE_API float	psGyroscopeSens		= 0.3f;
ENGINE_API bool		psGyroscopeInvertX	= false;
ENGINE_API bool		psGyroscopeInvertY	= false;
ENGINE_API bool		psGyroscopeEnabled	= true;

ENGINE_API float	psTouchpadSens		= 20.0f;

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

CInput::CInput(bool bExclusive, int deviceForInit)
{
	g_exclusive = !!bExclusive;

	Log("Starting INPUT device...");

	ZeroMemory(mouseState, sizeof(mouseState));
	ZeroMemory(KBState, sizeof(KBState));

	GGamepadService = new CGamepadService;

	iCapture(&dummyController);
	Debug.set_on_dialog(&on_error_dialog);

	Device.seqAppActivate.Add(this);
	Device.seqAppDeactivate.Add(this, REG_PRIORITY_HIGH);
	Device.seqFrame.Add(this, REG_PRIORITY_HIGH);
}

CInput::~CInput()
{
	xr_delete(GGamepadService);

	Device.seqFrame.Remove(this);
	Device.seqAppDeactivate.Remove(this);
	Device.seqAppActivate.Remove(this);
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

#define DEADZONE_RADIUS_UI 0.375f
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

	if (LeftAxis.x < -DEADZONE_RADIUS_UI)
	{
		pInput->GamepadButtonUpdate(DIK_LSTICK_LEFT, true);
	}
	else if (LeftAxis.x > DEADZONE_RADIUS_UI)
	{
		pInput->GamepadButtonUpdate(DIK_LSTICK_RIGHT, true);
	}
	else
	{
		if (GPState[DIK_LSTICK_LEFT])
			pInput->GamepadButtonUpdate(DIK_LSTICK_LEFT, false);
		if (GPState[DIK_LSTICK_RIGHT])
			pInput->GamepadButtonUpdate(DIK_LSTICK_RIGHT, false);
	}

	if (LeftAxis.y < -DEADZONE_RADIUS_UI)
	{
		pInput->GamepadButtonUpdate(DIK_LSTICK_DOWN, true);
	}
	else if (LeftAxis.y > DEADZONE_RADIUS_UI)
	{
		pInput->GamepadButtonUpdate(DIK_LSTICK_UP, true);
	}
	else
	{
		if (GPState[DIK_LSTICK_DOWN])
			pInput->GamepadButtonUpdate(DIK_LSTICK_DOWN, false);
		if (GPState[DIK_LSTICK_UP])
			pInput->GamepadButtonUpdate(DIK_LSTICK_UP, false);
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

	if (RightAxis.x < -DEADZONE_RADIUS_UI)
	{
		pInput->GamepadButtonUpdate(DIK_RSTICK_LEFT, true);
	}
	else if (RightAxis.x > DEADZONE_RADIUS_UI)
	{
		pInput->GamepadButtonUpdate(DIK_RSTICK_RIGHT, true);
	}
	else
	{
		if (GPState[DIK_RSTICK_LEFT])
			pInput->GamepadButtonUpdate(DIK_RSTICK_LEFT, false);
		if (GPState[DIK_RSTICK_RIGHT])
			pInput->GamepadButtonUpdate(DIK_RSTICK_RIGHT, false);
	}

	if (RightAxis.y < -DEADZONE_RADIUS_UI)
	{
		pInput->GamepadButtonUpdate(DIK_RSTICK_UP, true);
	}
	else if (RightAxis.y > DEADZONE_RADIUS_UI)
	{
		pInput->GamepadButtonUpdate(DIK_RSTICK_DOWN, true);
	}
	else
	{
		if (GPState[DIK_RSTICK_DOWN])
			pInput->GamepadButtonUpdate(DIK_RSTICK_DOWN, false);
		if (GPState[DIK_RSTICK_UP])
			pInput->GamepadButtonUpdate(DIK_RSTICK_UP, false);
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

void CInput::GamepadGyroscopeUpdate(Fvector3 value)
{
	Gyroscope += value;
	gyroscopeMoved = true;
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

void CInput::GamepadUpdate()
{
	if (GGamepadService->GamePadDevice == nullptr)
	{
		return;
	}

	GGamepadService->Update();
	if (cbStack.empty())
	{
		return;
	}

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
				KeyHolder->IR_GamepadKeyPress((int)i);
			}
			else
			{
				KeyHolder->IR_GamepadKeyRelease((int)i);
			}
		}
	}

	for (int i = 0; i < COUNT_GP_BUTTONS; i++)
	{
		if (GPState[i])
		{
			KeyHolder->IR_GamepadKeyHold((int)i);
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
#ifdef IXR_WINDOWS
	LANGID lang_locale = PRIMARYLANGID(LOWORD(HandleToLong(GetKeyboardLayout(0))));
	if (lang_locale != LANG_RUSSIAN) {
		return false;
	}

	if (g_AppInfo.IsLaunchedViaWineOrProton) {
		return false;
	}
#endif

	if (!russian_lookup_key_table.contains(dik)) {
		return false;
	}

	char sym = russian_lookup_key_table.at(dik);
	dest_str[0] = sym;
	dest_str[1] = 0;

	return true;
}

#define MOUSE_1		(SDL_SCANCODE_COUNT + 100)
#define MOUSE_8		(SDL_SCANCODE_COUNT + 107)

bool CInput::iGetAsyncGamepadKeyState( int dik )
{
	if(dik<COUNT_GP_BUTTONS)
		return !!GPState[dik];

	return false;
}

bool CInput::iGetAsyncKeyState( int dik )
{
	if(dik<COUNT_KB_BUTTONS)
		return !!KBState[dik];
	else
	if(dik>=MOUSE_1 && dik<=MOUSE_8)
	{
		int mk = dik-MOUSE_1;
		return iGetAsyncBtnState(mk);
	}else
		return false; //unknown key ???
}

bool CInput::iGetAsyncBtnState( int btn )
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

void CInput::GyroscopeUpdate()
{
	if (Device.dwPrecacheFrame)
	{
		return;
	}

	if (gyroscopeMoved) 
	{
		cbStack.back()->IR_OnGyroscopeMove(Gyroscope);
		gyroscopeMoved = false;
	}

	Gyroscope.set(0.0f, 0.0f, 0.0f);
}

#pragma warning(pop)

//-------------------------------------------------------
void CInput::iCapture(IInputReceiver *p)
{
	VERIFY(p);

	if (!Device.IsEditorMode() && CImGuiManager::Instance().IsCapturingInputs())
	{
		NoInputUpdate();
	} 
	else
	{
		MouseUpdate();
		GamepadUpdate();
		GyroscopeUpdate();
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
	PROF_EVENT("CInput::OnFrame");
	CScopeTimer Input(RDEVICE.Statistic->Input);

	dwCurTime = RDEVICE.TimerAsync_MMT();
#if !defined(MASTER_GOLD)
	if (!Device.IsEditorMode() && CImGuiManager::Instance().IsCapturingInputs())
	{
		NoInputUpdate();
	} 
	else 
#endif
	{
		MouseUpdate();
		GamepadUpdate();
		GyroscopeUpdate();
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


unsigned char CInput::GetConnectedInputDeviceCount(void) const noexcept
{
	R_ASSERT2(false, "todo: implement");
	return 0;
}

void CInput::GetConnectedInputDevices(CInputDevice(&devices)[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT]) noexcept
{
	GetConnectedInputKeyboards(devices);
	GetConnectedInputGamepads(devices);
	GetConnectedInputMouses(devices);
}

void CInput::GetInfoAboutConnectedInputDevices(const CInputDevice(&devices)[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT], CInputDeviceVendorInfo(&infos)[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT]) noexcept
{
	for (unsigned char i = 0; i < DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT; ++i)
	{
		CInputDeviceVendorInfo& info = infos[i];

		FillVendorInfo(devices[i],	info);
	}
}

unsigned char CInput::GetConnectedInputDeviceCount(eInputDeviceType type) const noexcept
{
	R_ASSERT2(false, "todo: implement");
	return 0;
}

bool CInput::GetVendorInfoAboutInputDevice(const CInputDevice* pDevice, CInputDeviceVendorInfo* pInfo) const noexcept
{
	R_ASSERT2(false, "todo: implement");
	return false;
}

bool CInput::GetVendorInfoAboutInputDevice(const CInputDevice& device, CInputDeviceVendorInfo* pInfo) const noexcept
{
	return GetVendorInfoAboutInputDevice(&device, pInfo);
}

void CInput::unacquire()
{
	SDL_SetWindowRelativeMouseMode(g_AppInfo.Window, false);
	IsAcquire = false;
}

void CInput::acquire()
{
	IsAcquire = true;
	SDL_SetWindowRelativeMouseMode(g_AppInfo.Window, true);
}

void  CInput::feedback(u16 s1, u16 s2, float time)
{
	if (GetControllerMode())
	{
		GGamepadService->Rumble(s1, s2, time * 1000);
	}
}

bool CInput::FillVendorInfo(const CInputDevice& device, CInputDeviceVendorInfo& info) noexcept
{
	switch (device.type)
	{
	case eInputDeviceType::keyboard:
	{
		if (device.handle != u32(-1))
		{
			const char* pName = SDL_GetKeyboardNameForID((device.handle));

			if (pName)
			{
				std::memcpy(info.name, pName, sizeof(info.name));
				info.name[(sizeof(info.name)/sizeof(info.name[0])) - 1] = 0;
			}
		}

		break;
	}
	case eInputDeviceType::gamepad:
	{
		SDL_Joystick* joystick = SDL_OpenJoystick((device.handle));

		if (joystick) {
			const char* name = SDL_GetJoystickName(joystick);
			u16 vendor = SDL_GetJoystickVendor(joystick);
			u16 product = SDL_GetJoystickProduct(joystick);
			u16 version = SDL_GetJoystickFirmwareVersion(joystick);
			const char* serial = SDL_GetJoystickSerial(joystick);

			
			if (name)
			{
				std::memcpy(info.name, name, sizeof(info.name));
				info.name[(sizeof(info.name) / sizeof(info.name[0])) - 1] = 0;
			}

			if (serial)
			{
				std::memcpy(info.data2, serial, sizeof(info.data2));
				info.name[(sizeof(info.data2) / sizeof(info.data2[0])) - 1] = 0;
			}

			u16* pWrite = (u16*)&info.data[0];

			*pWrite = vendor;

			pWrite = (u16*)&info.data[2];

			*pWrite = product;

			pWrite = (u16*)&info.data[4];

			*pWrite = version;

			SDL_CloseJoystick(joystick);
		}

		break;
	}
	case eInputDeviceType::mouse:
	{
		if (device.handle)
		{
			const char* pName = SDL_GetMouseNameForID((device.handle));

			if (pName)
			{
				std::memcpy(info.name, pName, sizeof(info.name));
				info.name[(sizeof(info.name) / sizeof(info.name[0])) - 1] = 0;
			}
		}

		break;
	}
	default:
	{
		R_ASSERT(false && "unsupported device");
		break;
	}
	}


	return true;
}

bool CInput::GetConnectedInputKeyboards(CInputDevice(&pool)[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT], unsigned char max_keyboards /*= DEF_XR_INPUT_MAX_INPUT_CONNECTED_KEYBOARD_COUNT*/) noexcept
{
	bool result = false;

	int count = -1;
	SDL_KeyboardID* pKeyboards = SDL_GetKeyboards(&count);

	result = !!(pKeyboards);

	if (!pKeyboards)
		return result;

	if (static_cast<unsigned char>(count) > max_keyboards)
	{
		count = max_keyboards;
	}

	unsigned char index = static_cast<unsigned char>(eInputDeviceType::keyboard);
	for (unsigned char i = 0; i < count; ++i)
	{
		pool[index].handle = u32(pKeyboards[i]);
		pool[index].type = eInputDeviceType::keyboard;

		index += i;
	}

	return result;
}

bool CInput::GetConnectedInputMouses(CInputDevice(&pool)[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT], unsigned char max_mouses /*= DEF_XR_INPUT_MAX_INPUT_CONNECTED_MOUSE_COUNT*/) noexcept
{
	bool result = false;

	int count = -1;
	SDL_MouseID* pMouses = SDL_GetMice(&count);

	result = !!(pMouses);

	if (!pMouses)
		return result;

	if (static_cast<unsigned char>(count) > max_mouses)
	{
		count = max_mouses;
	}

	unsigned char index = static_cast<unsigned char>(eInputDeviceType::mouse);
	for (unsigned char i = 0; i < count; ++i)
	{
		pool[index].handle = u32(pMouses[i]);
		pool[index].type = eInputDeviceType::mouse;

		index += i;
	}

	return result;
}

bool CInput::GetConnectedInputGamepads(CInputDevice(&pool)[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT], unsigned char max_gamepads /*= DEF_XR_INPUT_MAX_INPUT_CONNECTED_GAMEPAD_COUNT*/) noexcept
{
	bool result = false;

	int count = -1;
	SDL_JoystickID* pGamepads = SDL_GetGamepads(&count);

	result = !!(pGamepads);

	bool check = SDL_HasGamepad();

	if (!pGamepads)
		return result;

	if (static_cast<unsigned char>(count) > max_gamepads)
	{
		count = max_gamepads;
	}

	unsigned char index = static_cast<unsigned char>(eInputDeviceType::gamepad);
	for (unsigned char i = 0; i < count; ++i)
	{
		pool[index].handle = u32(pGamepads[i]);
		pool[index].type = eInputDeviceType::gamepad;

		index += i;
	}

	return result;
}

void CInput::SetControllerMode(bool val)
{
	if (val == controllerMode)
		return;

	controllerMode = val;
	CStringTable::ReparseKeyBindings();
}

extern u32 ps_gamepad_prefix_override;
extern xr_token gamepad_prefix_override_token[];

void CInput::SelectGamepadPrefix()
{
	if (GGamepadService->GamePadDevice == nullptr)
	{
		return;
	}

	if (ps_gamepad_prefix_override != 0)
	{
		GamepadTypeName = gamepad_prefix_override_token[ps_gamepad_prefix_override].name;
		return;
	}
	
	GamepadTypeName = GGamepadService->GetGamepadPrefix();
}
