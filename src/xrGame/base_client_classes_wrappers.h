////////////////////////////////////////////////////////////////////////////
//	Module 		: base_client_classes_wrappers.h
//	Created 	: 20.12.2004
//  Modified 	: 20.12.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay base client classes wrappers
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrScripts/script_export_space.h"
#include "base_client_classes.h"
#include "../xrEngine/EngineAPI.h"
#include "../xrCore/Collision/ISpatial.h"
#include "../xrEngine/ISheduled.h"
#include "../xrEngine/IRenderable.h"
#include "../xrEngine/ICollidable.h"
#include "../xrEngine/xr_object.h"
#include "Entity.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "xrServer_Object_Base.h"

#pragma warning(push)
#pragma warning(disable: 4584)
template <typename Base, typename LuabindBase = xr_empty>
class DLL_PureWrapper : public Base, public LuabindBase
{
public:

	IC DLL_PureWrapper() {}
	virtual ~DLL_PureWrapper() {}

	virtual DLL_Pure* _construct()
	{
		return call_member<DLL_Pure*>(this, "_construct");
	}

	static DLL_Pure* _construct_static(Base* self_)
	{
		return self_->Base::_construct();
	}
};


template <typename Base, typename LuabindBase = xr_empty>
class ISheduledWrapper : public Base, public LuabindBase
{
public:
	IC ISheduledWrapper() = default;
	virtual ~ISheduledWrapper() = default;

	virtual float shedule_Scale() override
	{
		return 1.f;
	}

	virtual void shedule_Update(u32 dt) override
	{
		Base::shedule_Update(dt);
	}
};

template <typename Base, typename LuabindBase = xr_empty>
class IRenderableWrapper : public Base, public LuabindBase
{
public:
	IC IRenderableWrapper() = default;
	virtual ~IRenderableWrapper() = default;
};

using CDLL_PureWrapper = DLL_PureWrapper<DLL_Pure, luabind::wrap_base>;
using CGameObjectDLL_Pure = DLL_PureWrapper<CGameObject, luabind::wrap_base>;

using CISheduledWrapper = ISheduledWrapper<ISheduled, luabind::wrap_base>;
using CGameObjectISheduled = ISheduledWrapper<CGameObjectDLL_Pure>;

using CIRenderableWrapper = IRenderableWrapper<IRenderable, luabind::wrap_base>;
using CGameObjectIRenderable = IRenderableWrapper<CGameObjectISheduled>;

#pragma warning(pop)

class CGameObjectWrapper : 
	public CGameObjectIRenderable
{
public:
	IC						CGameObjectWrapper	() {};
	virtual					~CGameObjectWrapper	() {};
	virtual bool			use					(CGameObject* who_use)
	{
		return call<bool>("use",who_use);
	}

	static bool			use_static			(CGameObject *self, CGameObject* who_use)
	{
		return self->CGameObject::use(who_use);
	}


	virtual void			net_Import			(NET_Packet &packet)
	{
		call<void>("net_Import",&packet);
	}

	static	void			net_Import_static	(CGameObject *self, NET_Packet *packet)
	{
		self->CGameObject::net_Import(*packet);
	}

	virtual void			net_Export			(NET_Packet &packet)
	{
		call<void>("net_Export",&packet);
	}

	static	void			net_Export_static	(CGameObject *self, NET_Packet *packet)
	{
		self->CGameObject::net_Export(*packet);
	}

	virtual BOOL			net_Spawn			(CSE_Abstract* data)
	{
		return			(luabind::call_member<bool>(this,"net_Spawn",data));
	}

	static	bool			net_Spawn_static	(CGameObject *self, CSE_Abstract *abstract)
	{
		return			(!!self->CGameObject::net_Spawn(abstract));
	}
};

class CEntityWrapper : public CEntity, public luabind::wrap_base {
public:
	IC						CEntityWrapper		() {}
	virtual					~CEntityWrapper		() {}

	virtual void			HitSignal			(float P, Fvector &local_dir,	CObject* who, s16 element)
	{
		luabind::call_member<void>(this,"HitSignal",P,local_dir,who,element);
	}

	static	void			HitSignal_static	(CEntity *self, float P, Fvector &local_dir,	CObject* who, s16 element)
	{
		ai().script_engine().script_log(eLuaMessageTypeError,"You are trying to call a pure virtual function CEntity::HitSignal!");
	}

	virtual void			HitImpulse			(float P, Fvector &vWorldDir, 	Fvector& vLocalDir)
	{
		luabind::call_member<void>(this,"HitImpulse",P,vWorldDir,vLocalDir);
	}

	static	void			HitImpulse_static	(float P, Fvector &vWorldDir, 	Fvector& vLocalDir)
	{
		ai().script_engine().script_log(eLuaMessageTypeError,"You are trying to call a pure virtual function CEntity::HitImpulse!");
	}
};
