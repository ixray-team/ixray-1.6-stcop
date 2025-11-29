////////////////////////////////////////////////////////////////////////////
//	Module 		: script_binder_object_wrapper.h
//	Created 	: 29.03.2004
//  Modified 	: 29.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script object binder wrapper
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "script_binder_object.h"
#include "script_game_object.h"

class CScriptGameObject;

class CScriptBinderObjectWrapper : public CScriptBinderObject, public luabind::wrap_base {
	bool* ReflectionData;
public:
						CScriptBinderObjectWrapper	(luabind::object object);
	virtual				~CScriptBinderObjectWrapper	();
	virtual void		FinishInitialization		() override;
	virtual void		reinit						() override;
	static  void		reinit_static				(CScriptBinderObject *script_binder_object);
	virtual void		reload						(const char* section) override;
	static  void		reload_static				(CScriptBinderObject *script_binder_object, const char* section);
	virtual bool		net_Spawn					(SpawnType DC) override;
	static  bool		net_Spawn_static			(CScriptBinderObject *script_binder_object, SpawnType DC);
	virtual void		net_Destroy					() override;
	static  void		net_Destroy_static			(CScriptBinderObject *script_binder_object);
	virtual void		net_Import					(NET_Packet *net_packet) override;
	static  void		net_Import_static			(CScriptBinderObject *script_binder_object, NET_Packet *net_packet);
	virtual void		net_Export					(NET_Packet *net_packet) override;
	static  void		net_Export_static			(CScriptBinderObject *script_binder_object, NET_Packet *net_packet);
	virtual void		shedule_Update				(u32 time_delta) override;
	static  void		shedule_Update_static		(CScriptBinderObject *script_binder_object, u32 time_delta);
	virtual void		save						(NET_Packet *output_packet) override;
	static	void		save_static					(CScriptBinderObject *script_binder_object, NET_Packet *output_packet);
	virtual void		load						(IReader *input_packet) override;
	static	void		load_static					(CScriptBinderObject *script_binder_object, IReader *input_packet);
	virtual void Serialize(ISaveObject* Object) override;
	virtual void Serialize_static(CScriptBinderObject* script_binder_object, ISaveObject* Object);
	virtual bool		net_SaveRelevant			() override;
	static  bool		net_SaveRelevant_static		(CScriptBinderObject *script_binder_object);
	virtual void		net_Relcase					(CScriptGameObject *object) override;
	static	void		net_Relcase_static			(CScriptBinderObject *script_binder_object, CScriptGameObject *object);
};
