#include "pch_script.h"
#include "GameObject.h"
#include "script_game_object.h"
#include "../xrCore/Collision/xr_collide_defs.h"

XREFLECT(LuaExport, Name="rq_result")
struct script_rq_result
{
	XPROPERTY(LuaReadOnly, Name="object")
	CScriptGameObject *O;
	XPROPERTY(LuaReadOnly)
	float range;
	XPROPERTY(LuaReadOnly)
	int element;

	XPROPERTY(LuaConstructor)
	script_rq_result() {O = 0; range = 0; element = 0;};
	void set(collide::rq_result& R) 
	{
		if (R.O)
		{
			CGameObject *go = smart_cast<CGameObject *>(R.O);
			if (go)
				O = go->lua_game_object();
		}
		range = R.range; 
		element = R.element;
	};
};

// class for performing ray pick
XREFLECT(LuaExport, Name="ray_pick")
struct CRayPick
{
	Fvector					start_position;
	Fvector					direction;
	float					range;
	collide::rq_target		flags;
	script_rq_result		result;
	CObject*				ignore;
	u16						material = std::numeric_limits<u16>::max();

	XPROPERTY(LuaConstructor)
	CRayPick();
	XPROPERTY(LuaConstructor)
	CRayPick(const Fvector& P, const Fvector& D, float R, collide::rq_target F, CScriptGameObject* I);

	XPROPERTY(LuaCallable)
	IC void		set_position		(Fvector& P)			{start_position = P;};
	XPROPERTY(LuaCallable)
	IC void		set_direction		(Fvector& D)			{direction = D;};
	XPROPERTY(LuaCallable)
	IC void		set_range			(float R)				{range = R;};
	XPROPERTY(LuaCallable)
	IC void		set_flags			(collide::rq_target F)	{flags = F;};
	XPROPERTY(LuaCallable)
	void		set_ignore_object	(CScriptGameObject* I)	{if (I) ignore = smart_cast<CObject*>(&(I->object()));};

	XPROPERTY(LuaCallable)
	bool		query				();
	
	XPROPERTY(LuaCallable)
	IC script_rq_result		get_result		()		{return result;};
	XPROPERTY(LuaCallable)
	IC CScriptGameObject*	get_object		()		{return result.O;};
	XPROPERTY(LuaCallable)
	IC float				get_distance	()		{return result.range;};
	XPROPERTY(LuaCallable)
	IC int					get_element		()		{return result.element;};
	XPROPERTY(LuaCallable)
	IC u16					get_material	()		{return material;};
};