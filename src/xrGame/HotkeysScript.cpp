#include "StdAfx.h"
#include "pch_script.h"
#include "HotkeysScript.h"

#include "ui/UIGameCustom.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "../xrEngine/xr_level_controller.h"

using namespace luabind;

namespace HotkeysScript
{
const char* CurrentContext()
{
	const bool UiOpen = CurrentGameUI() && CurrentGameUI()->TopInputReceiver();
	if (!UiOpen)
	{
		return "game";
	}
	const char* Active = Hotkeys::ActiveContext();
	return Active ? Active : "ui";
}

bool DispatchCustom(EGameActions Id)
{
	if (!Hotkeys::IsCustom(Id))
	{
		return false;
	}

	luabind::functor<bool> Handler;
	if (!ai().script_engine().functor("hotkeys.dispatch", Handler))
	{
		return false;
	}

	try
	{
		return Handler(id_to_action_name(Id));
	}
	catch (...)
	{
		Msg("! [hotkeys] dispatch failed for [%s]", id_to_action_name(Id));
		return false;
	}
}

bool Dispatch(int Dik)
{
	return DispatchCustom(Hotkeys::ActionAt(Dik, CurrentContext()));
}

bool Register(const char* Name, const char* Context)
{
	return Hotkeys::Register(Name, Context) != kNOTBINDED;
}

void SetInfo(const char* Name, const char* Label, const char* OptionsGroup, int Hint)
{
	const EGameActions Id = Hotkeys::Find(Name);
	if (Id != kNOTBINDED)
	{
		Hotkeys::SetInfo(Id, Label, OptionsGroup, Hint);
	}
}

void RegisterContext(const char* Name, const char* Parent)
{
	Hotkeys::RegisterContext(Name, Parent);
}

void RegisterLayer(const char* Name, const char* Parent, int Priority, bool BlockAll)
{
	Hotkeys::RegisterContext(Name, Parent);
	Hotkeys::SetLayerProperties(Name, Priority, BlockAll);
}

bool LayersEnabled()
{
	return Hotkeys::LayersEnabled();
}

void PushContext(const char* Name)
{
	Hotkeys::PushContext(Name);
}

void PopContext(const char* Name)
{
	Hotkeys::PopContext(Name);
}

const char* ActionAt(int Dik, const char* Context)
{
	const EGameActions Id = Hotkeys::ActionAt(Dik, Context && Context[0] ? Context : CurrentContext());
	return Id == kNOTBINDED ? "" : id_to_action_name(Id);
}

const char* ContextOf(const char* Name)
{
	const EGameActions Id = Hotkeys::Find(Name);
	return Id == kNOTBINDED ? "" : Hotkeys::Context(Id);
}

const char* KeyName(const char* Name)
{
	static string128 Buffer;
	Hotkeys::KeyText(Hotkeys::Find(Name), Buffer, sizeof(Buffer));
	return Buffer;
}

bool IsBound(const char* Name)
{
	return KeyName(Name)[0] != 0;
}
}

void hotkeys_registrator::script_register(lua_State* L)
{
	Hotkeys::SetCustomHandler(Hotkeys::CustomHandler(&HotkeysScript::DispatchCustom));
	module(L)
	[
		def("hotkey_register", &HotkeysScript::Register),
		def("hotkey_set_info", &HotkeysScript::SetInfo),
		def("hotkey_register_context", &HotkeysScript::RegisterContext),
		def("hotkey_register_layer", &HotkeysScript::RegisterLayer),
		def("hotkey_layers_enabled", &HotkeysScript::LayersEnabled),
		def("hotkey_push_context", &HotkeysScript::PushContext),
		def("hotkey_pop_context", &HotkeysScript::PopContext),
		def("hotkey_active_context", &HotkeysScript::CurrentContext),
		def("hotkey_action_at", &HotkeysScript::ActionAt),
		def("hotkey_context", &HotkeysScript::ContextOf),
		def("hotkey_key_name", &HotkeysScript::KeyName),
		def("hotkey_is_bound", &HotkeysScript::IsBound)
	];
}
