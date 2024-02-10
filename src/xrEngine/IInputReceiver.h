// IInputReceiver.h: interface for the IInputReceiver class.
//
//////////////////////////////////////////////////////////////////////

#ifndef IINPUTRECEIVERH
#define IINPUTRECEIVERH
#pragma once


struct SDL_Window;

#define DIK_ESCAPE          0x01
#define DIK_1               0x02
#define DIK_2               0x03
#define DIK_3               0x04
#define DIK_4               0x05
#define DIK_5               0x06
#define DIK_6               0x07
#define DIK_7               0x08
#define DIK_8               0x09
#define DIK_9               0x0A
#define DIK_0               0x0B
#define DIK_MINUS           0x0C    /* - on main keyboard */
#define DIK_EQUALS          0x0D
#define DIK_BACK            0x0E    /* backspace */
#define DIK_TAB             0x0F
#define DIK_Q               0x10
#define DIK_W               0x11
#define DIK_E               0x12
#define DIK_R               0x13
#define DIK_T               0x14
#define DIK_Y               0x15
#define DIK_U               0x16
#define DIK_I               0x17
#define DIK_O               0x18
#define DIK_P               0x19
#define DIK_LBRACKET        0x1A
#define DIK_RBRACKET        0x1B
#define DIK_RETURN          0x1C    /* Enter on main keyboard */
#define DIK_LCONTROL        0x1D
#define DIK_A               0x1E
#define DIK_S               0x1F
#define DIK_D               0x20
#define DIK_F               0x21
#define DIK_G               0x22
#define DIK_H               0x23
#define DIK_J               0x24
#define DIK_K               0x25
#define DIK_L               0x26
#define DIK_SEMICOLON       0x27
#define DIK_APOSTROPHE      0x28
#define DIK_GRAVE           0x29    /* accent grave */
#define DIK_LSHIFT          0x2A
#define DIK_BACKSLASH       0x2B
#define DIK_Z               0x2C
#define DIK_X               0x2D
#define DIK_C               0x2E
#define DIK_V               0x2F
#define DIK_B               0x30
#define DIK_N               0x31
#define DIK_M               0x32
#define DIK_COMMA           0x33
#define DIK_PERIOD          0x34    /* . on main keyboard */
#define DIK_SLASH           0x35    /* / on main keyboard */
#define DIK_RSHIFT          0x36
#define DIK_MULTIPLY        0x37    /* * on numeric keypad */
#define DIK_LMENU           0x38    /* left Alt */
#define DIK_SPACE           0x39
#define DIK_CAPITAL         0x3A
#define DIK_F1              0x3B
#define DIK_F2              0x3C
#define DIK_F3              0x3D
#define DIK_F4              0x3E
#define DIK_F5              0x3F
#define DIK_F6              0x40
#define DIK_F7              0x41
#define DIK_F8              0x42
#define DIK_F9              0x43
#define DIK_F10             0x44
#define DIK_NUMLOCK         0x45
#define DIK_SCROLL          0x46    /* Scroll Lock */
#define DIK_NUMPAD7         0x47
#define DIK_NUMPAD8         0x48
#define DIK_NUMPAD9         0x49
#define DIK_SUBTRACT        0x4A    /* - on numeric keypad */
#define DIK_NUMPAD4         0x4B
#define DIK_NUMPAD5         0x4C
#define DIK_NUMPAD6         0x4D
#define DIK_ADD             0x4E    /* + on numeric keypad */
#define DIK_NUMPAD1         0x4F
#define DIK_NUMPAD2         0x50
#define DIK_NUMPAD3         0x51
#define DIK_NUMPAD0         0x52
#define DIK_DECIMAL         0x53    /* . on numeric keypad */
#define DIK_OEM_102         0x56    /* <> or \| on RT 102-key keyboard (Non-U.S.) */
#define DIK_F11             0x57
#define DIK_F12             0x58
#define DIK_F13             0x64    /*                     (NEC PC98) */
#define DIK_F14             0x65    /*                     (NEC PC98) */
#define DIK_F15             0x66    /*                     (NEC PC98) */
#define DIK_KANA            0x70    /* (Japanese keyboard)            */
#define DIK_ABNT_C1         0x73    /* /? on Brazilian keyboard */
#define DIK_CONVERT         0x79    /* (Japanese keyboard)            */
#define DIK_NOCONVERT       0x7B    /* (Japanese keyboard)            */
#define DIK_YEN             0x7D    /* (Japanese keyboard)            */
#define DIK_ABNT_C2         0x7E    /* Numpad . on Brazilian keyboard */
#define DIK_NUMPADEQUALS    0x8D    /* = on numeric keypad (NEC PC98) */
#define DIK_PREVTRACK       0x90    /* Previous Track (DIK_CIRCUMFLEX on Japanese keyboard) */
#define DIK_AT              0x91    /*                     (NEC PC98) */
#define DIK_COLON           0x92    /*                     (NEC PC98) */
#define DIK_UNDERLINE       0x93    /*                     (NEC PC98) */
#define DIK_KANJI           0x94    /* (Japanese keyboard)            */
#define DIK_STOP            0x95    /*                     (NEC PC98) */
#define DIK_AX              0x96    /*                     (Japan AX) */
#define DIK_UNLABELED       0x97    /*                        (J3100) */
#define DIK_NEXTTRACK       0x99    /* Next Track */
#define DIK_NUMPADENTER     0x9C    /* Enter on numeric keypad */
#define DIK_RCONTROL        0x9D
#define DIK_MUTE            0xA0    /* Mute */
#define DIK_CALCULATOR      0xA1    /* Calculator */
#define DIK_PLAYPAUSE       0xA2    /* Play / Pause */
#define DIK_MEDIASTOP       0xA4    /* Media Stop */
#define DIK_VOLUMEDOWN      0xAE    /* Volume - */
#define DIK_VOLUMEUP        0xB0    /* Volume + */
#define DIK_WEBHOME         0xB2    /* Web home */
#define DIK_NUMPADCOMMA     0xB3    /* , on numeric keypad (NEC PC98) */
#define DIK_DIVIDE          0xB5    /* / on numeric keypad */
#define DIK_SYSRQ           0xB7
#define DIK_RMENU           0xB8    /* right Alt */
#define DIK_PAUSE           0xC5    /* Pause */
#define DIK_HOME            0xC7    /* Home on arrow keypad */
#define DIK_UP              0xC8    /* UpArrow on arrow keypad */
#define DIK_PRIOR           0xC9    /* PgUp on arrow keypad */
#define DIK_LEFT            0xCB    /* LeftArrow on arrow keypad */
#define DIK_RIGHT           0xCD    /* RightArrow on arrow keypad */
#define DIK_END             0xCF    /* End on arrow keypad */
#define DIK_DOWN            0xD0    /* DownArrow on arrow keypad */
#define DIK_NEXT            0xD1    /* PgDn on arrow keypad */
#define DIK_INSERT          0xD2    /* Insert on arrow keypad */
#define DIK_DELETE          0xD3    /* Delete on arrow keypad */
#define DIK_LWIN            0xDB    /* Left Windows key */
#define DIK_RWIN            0xDC    /* Right Windows key */
#define DIK_APPS            0xDD    /* AppMenu key */
#define DIK_POWER           0xDE    /* System Power */
#define DIK_SLEEP           0xDF    /* System Sleep */
#define DIK_WAKE            0xE3    /* System Wake */
#define DIK_WEBSEARCH       0xE5    /* Web Search */
#define DIK_WEBFAVORITES    0xE6    /* Web Favorites */
#define DIK_WEBREFRESH      0xE7    /* Web Refresh */
#define DIK_WEBSTOP         0xE8    /* Web Stop */
#define DIK_WEBFORWARD      0xE9    /* Web Forward */
#define DIK_WEBBACK         0xEA    /* Web Back */
#define DIK_MYCOMPUTER      0xEB    /* My Computer */
#define DIK_MAIL            0xEC    /* Mail */
#define DIK_MEDIASELECT     0xED    /* Media Select */

static int ToSDLKey(int keycode)
{
    switch (keycode) {
    case DIK_TAB:                   return SDLK_TAB;
    case DIK_LEFT:                  return SDLK_LEFT;
    case DIK_RIGHT:                 return SDLK_RIGHT;
    case DIK_UP:                    return SDLK_UP;
    case DIK_DOWN:                  return SDLK_DOWN;
    case DIK_PRIOR:                 return SDLK_PAGEUP;
    case DIK_NEXT:                  return SDLK_PAGEDOWN;
    case DIK_HOME:                  return SDLK_HOME;
    case DIK_END:                   return SDLK_END;
    case DIK_INSERT:                return SDLK_INSERT;
    case DIK_DELETE:                return SDLK_DELETE;
    case DIK_BACK:                  return SDLK_BACKSPACE;
    case DIK_SPACE:                 return SDLK_SPACE;
    case DIK_RETURN:                return SDLK_RETURN;
    case DIK_ESCAPE:                return SDLK_ESCAPE;
    case DIK_COMMA:                 return SDLK_COMMA;
    case DIK_MINUS:                 return SDLK_MINUS;
    case DIK_PERIOD:                return SDLK_PERIOD;
    case DIK_SLASH:                 return SDLK_SLASH;
    case DIK_SEMICOLON:             return SDLK_SEMICOLON;
    case DIK_EQUALS:                return SDLK_EQUALS;
    case DIK_LBRACKET:              return SDLK_LEFTBRACKET;
    case DIK_BACKSLASH:             return SDLK_BACKSLASH;
    case DIK_RBRACKET:              return SDLK_RIGHTBRACKET;
    case DIK_CAPITAL:               return SDLK_CAPSLOCK;
    case DIK_SCROLL:                return SDLK_SCROLLLOCK;
    case DIK_PAUSE:                 return SDLK_PAUSE;
    case DIK_NUMPAD0:               return SDLK_KP_0;
    case DIK_NUMPAD1:               return SDLK_KP_1;
    case DIK_NUMPAD2:               return SDLK_KP_2;
    case DIK_NUMPAD3:               return SDLK_KP_3;
    case DIK_NUMPAD4:               return SDLK_KP_4;
    case DIK_NUMPAD5:               return SDLK_KP_5;
    case DIK_NUMPAD6:               return SDLK_KP_6;
    case DIK_NUMPAD7:               return SDLK_KP_7;
    case DIK_NUMPAD8:               return SDLK_KP_8;
    case DIK_NUMPAD9:               return SDLK_KP_9;
    case DIK_DIVIDE:                return SDLK_KP_DIVIDE;
    case DIK_MULTIPLY:              return SDLK_KP_MULTIPLY;
    case DIK_SUBTRACT:              return SDLK_KP_MINUS;
    case DIK_ADD:                   return SDLK_KP_PLUS;
    case DIK_NUMPADENTER:           return SDLK_KP_ENTER;
    case DIK_NUMPADEQUALS:          return SDLK_KP_EQUALS;
    case DIK_LCONTROL:              return SDLK_LCTRL;
    case DIK_LSHIFT:                return SDLK_LSHIFT;
    case DIK_LMENU:                 return SDLK_LALT;
    case DIK_LWIN:                  return SDLK_LGUI;
    case DIK_RCONTROL:              return SDLK_RCTRL;
    case DIK_RSHIFT:                return SDLK_RSHIFT;
    case DIK_RMENU:                 return SDLK_RALT;
    case DIK_RWIN:                  return SDLK_RGUI;
    case DIK_GRAVE:                 return SDLK_BACKQUOTE;
    case DIK_0:                     return SDLK_0;
    case DIK_1:                     return SDLK_1;
    case DIK_2:                     return SDLK_2;
    case DIK_3:                     return SDLK_3;
    case DIK_4:                     return SDLK_4;
    case DIK_5:                     return SDLK_5;
    case DIK_6:                     return SDLK_6;
    case DIK_7:                     return SDLK_7;
    case DIK_8:                     return SDLK_8;
    case DIK_9:                     return SDLK_9;
    case DIK_A:                     return SDLK_a;
    case DIK_B:                     return SDLK_b;
    case DIK_C:                     return SDLK_c;
    case DIK_D:                     return SDLK_d;
    case DIK_E:                     return SDLK_e;
    case DIK_F:                     return SDLK_f;
    case DIK_G:                     return SDLK_g;
    case DIK_H:                     return SDLK_h;
    case DIK_I:                     return SDLK_i;
    case DIK_J:                     return SDLK_j;
    case DIK_K:                     return SDLK_k;
    case DIK_L:                     return SDLK_l;
    case DIK_M:                     return SDLK_m;
    case DIK_N:                     return SDLK_n;
    case DIK_O:                     return SDLK_o;
    case DIK_P:                     return SDLK_p;
    case DIK_Q:                     return SDLK_q;
    case DIK_R:                     return SDLK_r;
    case DIK_S:                     return SDLK_s;
    case DIK_T:                     return SDLK_t;
    case DIK_U:                     return SDLK_u;
    case DIK_V:                     return SDLK_v;
    case DIK_W:                     return SDLK_w;
    case DIK_X:                     return SDLK_x;
    case DIK_Y:                     return SDLK_y;
    case DIK_Z:                     return SDLK_z;
    case DIK_F1:                    return SDLK_F1;
    case DIK_F2:                    return SDLK_F2;
    case DIK_F3:                    return SDLK_F3;
    case DIK_F4:                    return SDLK_F4;
    case DIK_F5:                    return SDLK_F5;
    case DIK_F6:                    return SDLK_F6;
    case DIK_F7:                    return SDLK_F7;
    case DIK_F8:                    return SDLK_F8;
    case DIK_F9:                    return SDLK_F9;
    case DIK_F10:                   return SDLK_F10;
    case DIK_F11:                   return SDLK_F11;
    case DIK_F12:                   return SDLK_F12;
        break;
    }

    return SDLK_UNKNOWN;
}
static int ToDIK(int sdlkey)
{
    switch (sdlkey) {
    case SDLK_TAB: return DIK_TAB;
    case SDLK_LEFT: return DIK_LEFT;
    case SDLK_RIGHT: return DIK_RIGHT;
    case SDLK_UP: return DIK_UP;
    case SDLK_DOWN: return DIK_DOWN;
    case SDLK_PAGEUP: return DIK_PRIOR;
    case SDLK_PAGEDOWN: return DIK_NEXT;
    case SDLK_HOME: return DIK_HOME;
    case SDLK_END: return DIK_END;
    case SDLK_INSERT: return DIK_INSERT;
    case SDLK_DELETE: return DIK_DELETE;
    case SDLK_BACKSPACE: return DIK_BACK;
    case SDLK_SPACE: return DIK_SPACE;
    case SDLK_RETURN: return DIK_RETURN;
    case SDLK_ESCAPE: return DIK_ESCAPE;
    case SDLK_COMMA: return DIK_COMMA;
    case SDLK_MINUS: return DIK_MINUS;
    case SDLK_PERIOD: return DIK_PERIOD;
    case SDLK_SLASH: return DIK_SLASH;
    case SDLK_SEMICOLON: return DIK_SEMICOLON;
    case SDLK_EQUALS: return DIK_EQUALS;
    case SDLK_LEFTBRACKET: return DIK_LBRACKET;
    case SDLK_BACKSLASH: return DIK_BACKSLASH;
    case SDLK_RIGHTBRACKET: return DIK_RBRACKET;
    case SDLK_CAPSLOCK: return DIK_CAPITAL;
    case SDLK_SCROLLLOCK: return DIK_SCROLL;
    case SDLK_PAUSE: return DIK_PAUSE;
    case SDLK_KP_0: return DIK_NUMPAD0;
    case SDLK_KP_1: return DIK_NUMPAD1;
    case SDLK_KP_2: return DIK_NUMPAD2;
    case SDLK_KP_3: return DIK_NUMPAD3;
    case SDLK_KP_4: return DIK_NUMPAD4;
    case SDLK_KP_5: return DIK_NUMPAD5;
    case SDLK_KP_6: return DIK_NUMPAD6;
    case SDLK_KP_7: return DIK_NUMPAD7;
    case SDLK_KP_8: return DIK_NUMPAD8;
    case SDLK_KP_9: return DIK_NUMPAD9;
    case SDLK_BACKQUOTE: return DIK_GRAVE;
    case SDLK_KP_DIVIDE: return DIK_DIVIDE;
    case SDLK_KP_MULTIPLY: return DIK_MULTIPLY;
    case SDLK_KP_MINUS: return DIK_SUBTRACT;
    case SDLK_KP_PLUS: return DIK_ADD;
    case SDLK_KP_ENTER: return DIK_NUMPADENTER;
    case SDLK_KP_EQUALS: return DIK_NUMPADEQUALS;
    case SDLK_LCTRL: return DIK_LCONTROL;
    case SDLK_LSHIFT: return DIK_LSHIFT;
    case SDLK_LALT: return DIK_LMENU;
    case SDLK_LGUI: return DIK_LWIN;
    case SDLK_RCTRL: return DIK_RCONTROL;
    case SDLK_RSHIFT: return DIK_RSHIFT;
    case SDLK_RALT: return DIK_RMENU;
    case SDLK_RGUI: return DIK_RWIN;
    case SDLK_0: return DIK_0;
    case SDLK_1: return DIK_1;
    case SDLK_2: return DIK_2;
    case SDLK_3: return DIK_3;
    case SDLK_4: return DIK_4;
    case SDLK_5: return DIK_5;
    case SDLK_6: return DIK_6;
    case SDLK_7: return DIK_7;
    case SDLK_8: return DIK_8;
    case SDLK_9: return DIK_9;
    case SDLK_a: return DIK_A;
    case SDLK_b: return DIK_B;
    case SDLK_c: return DIK_C;
    case SDLK_d: return DIK_D;
    case SDLK_e: return DIK_E;
    case SDLK_f: return DIK_F;
    case SDLK_g: return DIK_G;
    case SDLK_h: return DIK_H;
    case SDLK_i: return DIK_I;
    case SDLK_j: return DIK_J;
    case SDLK_k: return DIK_K;
    case SDLK_l: return DIK_L;
    case SDLK_m: return DIK_M;
    case SDLK_n: return DIK_N;
    case SDLK_o: return DIK_O;
    case SDLK_p: return DIK_P;
    case SDLK_q: return DIK_Q;
    case SDLK_r: return DIK_R;
    case SDLK_s: return DIK_S;
    case SDLK_t: return DIK_T;
    case SDLK_u: return DIK_U;
    case SDLK_v: return DIK_V;
    case SDLK_w: return DIK_W;
    case SDLK_x: return DIK_X;
    case SDLK_y: return DIK_Y;
    case SDLK_z: return DIK_Z;
    case SDLK_F1: return DIK_F1;
    case SDLK_F2: return DIK_F2;
    case SDLK_F3: return DIK_F3;
    case SDLK_F4: return DIK_F4;
    case SDLK_F5: return DIK_F5;
    case SDLK_F6: return DIK_F6;
    case SDLK_F7: return DIK_F7;
    case SDLK_F8: return DIK_F8;
    case SDLK_F9: return DIK_F9;
    case SDLK_F10: return DIK_F10;
    case SDLK_F11: return DIK_F11;
    case SDLK_F12: return DIK_F12;
        break;
    }

    return 0;
}

class ENGINE_API	IInputReceiver
{
public:
	void			IR_GetLastMouseDelta			(Ivector2& p);
	void			IR_GetMousePosScreen			(Ivector2& p);
	void			IR_GetMousePosReal				(Ivector2 &p);
	void			IR_GetMousePosIndependent		(Fvector2 &f);
	void			IR_GetMousePosIndependentCrop	(Fvector2 &f);
	BOOL			IR_GetKeyState					(int dik);
	BOOL			IR_GetBtnState					(int btn);
	void			IR_Capture						(void);
	void			IR_Release						(void);

	virtual void	IR_OnDeactivate					(void);
	virtual void	IR_OnActivate					(void);

	virtual void	IR_OnMousePress					(int btn)		{};
	virtual void	IR_OnMouseRelease				(int btn)		{};
	virtual void	IR_OnMouseHold					(int btn)		{};
	virtual void	IR_OnMouseWheel					(int direction)	{};
	virtual void	IR_OnMouseMove					(int x, int y)	{};
	virtual void	IR_OnMouseStop					(int x, int y)	{};

	virtual void	IR_OnKeyboardPress				(int dik)		{};
	virtual void	IR_OnKeyboardRelease			(int dik)		{};
	virtual void	IR_OnKeyboardHold				(int dik)		{};
    virtual void	IR_GamepadUpdateStick           (int id, Fvector2 value) {};
};

ENGINE_API extern float			psMouseSens;
ENGINE_API extern float			psMouseSensScale;
ENGINE_API extern Flags32		psMouseInvert;

#endif
