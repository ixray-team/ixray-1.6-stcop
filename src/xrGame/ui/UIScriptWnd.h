#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrScripts/script_space_forward.h"
#include "../../xrScripts/script_export_space.h"

struct SCallbackInfo;

class CUIDialogWndEx :
	public CUIDialogWnd,
	public DLL_Pure
{
typedef CUIDialogWnd				inherited;
typedef xr_vector<SCallbackInfo*>	CALLBACKS;
typedef CALLBACKS::iterator			CALLBACK_IT;

private:
			CALLBACKS			m_callbacks;
	bool StopMovement = true;
virtual void				SendMessage			(CUIWindow* pWnd, s16 msg, void* pData = NULL) override;
			SCallbackInfo*		NewCallback			();
protected:
			bool				Load				(const char* xml_name);

public:
	void SetStopAnyMove(bool Stop) { StopMovement = Stop; }
	bool StopAnyMove() override { return StopMovement; }

			void				Register			(CUIWindow* pChild);
			void				Register			(CUIWindow* pChild, const char* name);
								CUIDialogWndEx		();
virtual ~CUIDialogWndEx		() override;
			void				AddCallback			(const char* control_id, s16 event, const luabind::functor<void> &lua_function);
			void				AddCallback			(const char* control_id, s16 event, const luabind::functor<void> &functor, const luabind::object &object);
virtual void				Update				() override;
virtual bool				OnKeyboardAction			(int dik, EUIMessages keyboard_action) override;
virtual bool				Dispatch			(int cmd, int param) override {return true;}

virtual CUIWindow* ui_cast_window() override { return this; }

	template<typename T>
	IC	T*	GetControl(const char* name);
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
