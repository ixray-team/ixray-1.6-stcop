////////////////////////////////////////////////////////////////////////////
//	Module 		: ef_storage_script.cpp
//	Created 	: 25.03.2002
//  Modified 	: 11.10.2002
//	Author		: Dmitriy Iassenev
//	Description : Evaluation functions storage class
////////////////////////////////////////////////////////////////////////////

#include "pch_script.h"
#include "ef_storage.h"
#include "ai_space.h"
#include "script_game_object.h"
#include "entity_alive.h"
#include "script_engine.h"
#include "ef_base.h"
#include "xrServer_Objects_ALife.h"

using namespace luabind;

CEF_Storage *ef_storage()
{
	return	(&ai().ef_storage());
}

float evaluate(CEF_Storage *ef_storage, LPCSTR function, CScriptGameObject *object0, CScriptGameObject *object1, CScriptGameObject *object2, CScriptGameObject *object3)
{
	ef_storage->alife_evaluation(false);

	CBaseFunction	*f = ef_storage->function(function);
	if (!f) {
		ai().script_engine().script_log(eLuaMessageTypeError,"Cannot find evaluation function %s",function);
		return		(0.f);
	}

	ef_storage->non_alife().member()	= smart_cast<CEntityAlive*>(object0 ? &object0->object() : 0);
	if (object0 && !ef_storage->non_alife().member()) {
		ai().script_engine().script_log(eLuaMessageTypeError,"object %s is not herited from CSE_ALifeSchedulable!",*object0->cName());
		return		(0.f);
	}
	
	ef_storage->non_alife().enemy()	= smart_cast<CEntityAlive*>(object1 ? &object1->object() : 0);
	if (object1 && !ef_storage->non_alife().enemy()) {
		ai().script_engine().script_log(eLuaMessageTypeError,"object %s is not herited from CSE_ALifeSchedulable!",*object1->cName());
		return		(0.f);
	}

	ef_storage->non_alife().member_item()	= &object2->object();
	ef_storage->non_alife().enemy_item()	= &object3->object();

	return			(f->ffGetValue());
}

float evaluate(CEF_Storage *ef_storage, LPCSTR function, CScriptGameObject *object0, CScriptGameObject *object1, CScriptGameObject *object2)
{
	return			(evaluate(ef_storage,function, object0, object1, object2,0));
}

float evaluate(CEF_Storage *ef_storage, LPCSTR function, CScriptGameObject *object0, CScriptGameObject *object1)
{
	return			(evaluate(ef_storage,function, object0, object1,0,0));
}

float evaluate(CEF_Storage *ef_storage, LPCSTR function, CScriptGameObject *_0)
{
	return			(evaluate(ef_storage,function,_0,0,0,0));
}

float evaluate(CEF_Storage *ef_storage, LPCSTR function, CSE_ALifeObject *object0, CSE_ALifeObject *object1, CSE_ALifeObject *object2, CSE_ALifeObject *object3)
{
	ef_storage->alife_evaluation(true);

	CBaseFunction	*f = ef_storage->function(function);
	if (!f) {
		ai().script_engine().script_log(eLuaMessageTypeError,"Cannot find evaluation function %s",function);
		return		(0.f);
	}

	ef_storage->alife().member()	= smart_cast<CSE_ALifeSchedulable*>(object0);
	if (object0 && !ef_storage->alife().member()) {
		ai().script_engine().script_log(eLuaMessageTypeError,"object %s is not herited from CSE_ALifeSchedulable!", object1->name_replace());
		return		(0.f);
	}

	ef_storage->alife().enemy()	= smart_cast<CSE_ALifeSchedulable*>(object1);
	if (object1 && !ef_storage->alife().enemy()) {
		ai().script_engine().script_log(eLuaMessageTypeError,"object %s is not herited from CSE_ALifeSchedulable!", object1->name_replace());
		return		(0.f);
	}

	ef_storage->alife().member_item()	= object2;
	ef_storage->alife().enemy_item()	= object3;

	return			(f->ffGetValue());
}

float evaluate(CEF_Storage *ef_storage, LPCSTR function, CSE_ALifeObject *object0, CSE_ALifeObject *object1, CSE_ALifeObject *object2)
{
	return			(evaluate(ef_storage,function, object0, object1, object2,0));
}

float evaluate(CEF_Storage *ef_storage, LPCSTR function, CSE_ALifeObject *object0, CSE_ALifeObject *object1)
{
	return			(evaluate(ef_storage,function, object0, object1,0,0));
}

float evaluate(CEF_Storage *ef_storage, LPCSTR function, CSE_ALifeObject *object0)
{
	return			(evaluate(ef_storage,function, object0,0,0,0));
}

#pragma optimize("s",on)
void CEF_Storage::script_register(lua_State *L)
{
	module(L)
	[
		def("ef_storage",&ef_storage),

		class_<CEF_Storage>("cef_storage")
			.def("evaluate",	(float (*)(CEF_Storage*,LPCSTR,CScriptGameObject*))(&evaluate))
			.def("evaluate",	(float (*)(CEF_Storage*,LPCSTR,CScriptGameObject*,CScriptGameObject*))(&evaluate))
			.def("evaluate",	(float (*)(CEF_Storage*,LPCSTR,CScriptGameObject*,CScriptGameObject*,CScriptGameObject*))(&evaluate))
			.def("evaluate",	(float (*)(CEF_Storage*,LPCSTR,CScriptGameObject*,CScriptGameObject*,CScriptGameObject*,CScriptGameObject*))(&evaluate))
			.def("evaluate",	(float (*)(CEF_Storage*,LPCSTR,CSE_ALifeObject*))(&evaluate))
			.def("evaluate",	(float (*)(CEF_Storage*,LPCSTR,CSE_ALifeObject*,CSE_ALifeObject*))(&evaluate))
			.def("evaluate",	(float (*)(CEF_Storage*,LPCSTR,CSE_ALifeObject*,CSE_ALifeObject*,CSE_ALifeObject*))(&evaluate))
			.def("evaluate",	(float (*)(CEF_Storage*,LPCSTR,CSE_ALifeObject*,CSE_ALifeObject*,CSE_ALifeObject*,CSE_ALifeObject*))(&evaluate))
	];
}