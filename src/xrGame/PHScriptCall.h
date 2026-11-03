#pragma once
#include "PHReqComparer.h"
#include "PHCommander.h"
#include "../xrScripts/script_engine.h"
#include "../xrScripts/script_space_forward.h"
#include "../xrScripts/script_callback_ex.h"
//template<>
//IC bool compare_safe(const functor<>& f1,const functor<>& f2)
//{
//	f1.typ
//}

class CPHScriptCondition:
	public CPHCondition,
	public CPHReqComparerV
{
	luabind::functor<bool>			*m_lua_function;

							CPHScriptCondition				(const CPHScriptCondition &func);

public:
							CPHScriptCondition				(const luabind::functor<bool> &func)	;
	virtual ~CPHScriptCondition				() override;
	virtual bool 			is_true							() override;
	virtual bool 			obsolete						()								const override;
	virtual bool			compare							(const	CPHReqComparerV* v)		const override {return v->compare(this);}
	virtual bool			compare							(const	CPHScriptCondition*v)	const override { return *m_lua_function == *(v->m_lua_function); }

};

class CPHScriptAction :
	public CPHAction,
	public CPHReqComparerV
{
	bool	b_obsolete							   ;
	luabind::functor<void>			*m_lua_function;
public:
							CPHScriptAction					(const luabind::functor<void> &func)	;
							CPHScriptAction					(const CPHScriptAction &action)			;
	virtual ~CPHScriptAction				() override;
	virtual void 			run								() override;
	virtual bool 			obsolete						()								const override;
	virtual bool			compare							(const	CPHReqComparerV* v)		const override {return v->compare(this);}
	virtual bool			compare							(const	CPHScriptAction* v)		const override { return *m_lua_function == *(v->m_lua_function); }
};


class CPHScriptObjectCondition:
	public CPHCondition,
	public CPHReqComparerV
{
	luabind::object					*m_lua_object;
	shared_str						m_method_name;
public:
							CPHScriptObjectCondition		(const luabind::object &lua_object, const char* method)	;
							CPHScriptObjectCondition		(const CPHScriptObjectCondition &object)			;
	virtual ~CPHScriptObjectCondition		() override;
	virtual bool 			is_true							() override;
	virtual bool 			obsolete						()										const override;
	virtual bool			compare							(const	luabind::object* v)				const		{return *m_lua_object==*v;}
	virtual bool			compare							(const	CPHReqComparerV* v)				const override {return v->compare(this);}
	virtual bool			compare							(const	CPHScriptObjectCondition* v)	const override;
};

class CPHScriptObjectAction :
	public CPHAction,
	public CPHReqComparerV
{
	bool	b_obsolete							   ;
	luabind::object					*m_lua_object;
	shared_str						m_method_name;
public:
							CPHScriptObjectAction			(const luabind::object &lua_object, const char* method)	;
							CPHScriptObjectAction			(const CPHScriptObjectAction &object)				;
	virtual ~CPHScriptObjectAction			() override;
	virtual void 			run								() override;
	virtual bool 			obsolete						()										const override;
	virtual bool			compare							(const	luabind::object* v)				const		{return *m_lua_object==*v;}
	virtual bool			compare							(const	CPHReqComparerV* v)				const override {return v->compare(this);}
	virtual bool			compare							(const	CPHScriptObjectAction* v)		const override;
};
//////////////////////////////////////////////////////////////////////////////////////////

class CPHScriptObjectConditionN:
	public CPHCondition,
	public CPHReqComparerV
{
	CScriptCallbackEx<bool>			m_callback;
public:
	CPHScriptObjectConditionN			( const luabind::object &object,const luabind::functor<bool> &functor)	;
	virtual ~CPHScriptObjectConditionN		() override;
	virtual bool 			is_true							() override;
	virtual bool 			obsolete						()										const override;
	virtual bool			compare							(const	luabind::object* v)				const		{return m_callback==(*v);}
	virtual bool			compare							(const	CPHReqComparerV* v)				const override {return v->compare(this);}
	virtual bool			compare							(const	CPHScriptObjectConditionN* v)	const override {return m_callback==v->m_callback;}
};

class CPHScriptObjectActionN :
	public CPHAction,
	public CPHReqComparerV
{
	bool	b_obsolete							   ;
	CScriptCallbackEx<void>			m_callback;
public:
	CPHScriptObjectActionN			( const luabind::object &object,const luabind::functor<void> &functor);
	virtual ~CPHScriptObjectActionN			() override;
	virtual void 			run								() override;
	virtual bool 			obsolete						()									const override;
	virtual bool			compare							(const	luabind::object* v)			const			{return m_callback==*v;}
	virtual bool			compare							(const	CPHReqComparerV* v)			const override {return v->compare(this);}
	virtual bool			compare							(const	CPHScriptObjectActionN* v)	const override {return m_callback==v->m_callback;}
};

class CPHScriptGameObjectCondition :
	public CPHScriptObjectConditionN
{
	CObject* m_obj;
	bool	 b_obsolete;
public:
	CPHScriptGameObjectCondition( const luabind::object &object,const luabind::functor<bool> &functor,CObject* gobj):
	CPHScriptObjectConditionN(object,functor)
	{
		m_obj=gobj;
		b_obsolete=false;
	}

	virtual bool 			is_true							() override
	{b_obsolete=CPHScriptObjectConditionN::is_true();return b_obsolete;}
	virtual bool			compare							(const	CObject* v)			const			{return m_obj->ID()==v->ID();}
	virtual bool			compare							(const	CPHReqComparerV* v)	const override {return v->compare(this);}
	virtual bool			obsolete						()							const override {return b_obsolete;}
};

class CPHScriptGameObjectAction :
	public CPHScriptObjectActionN
{
	CObject* m_obj;
public:
	CPHScriptGameObjectAction( const luabind::object &object,const luabind::functor<void> &functor,CObject* gobj):
	CPHScriptObjectActionN(object,functor)
	{
		m_obj=gobj;
	}

	virtual bool			compare							(const	CPHReqComparerV* v)	const override {return v->compare(this);}
	virtual bool			compare							(const	CObject* v)			const			{return m_obj->ID()==v->ID();}
};

class CPHSriptReqObjComparer :
	public CPHReqComparerV
{
	luabind::object					*m_lua_object;

public:
								CPHSriptReqObjComparer	(const luabind::object & lua_object)			{m_lua_object= new luabind::object(lua_object);}
								CPHSriptReqObjComparer	(const CPHSriptReqObjComparer &object)			{m_lua_object= new luabind::object(*object.m_lua_object);}
		virtual					~CPHSriptReqObjComparer	()												{xr_delete(m_lua_object);}
	virtual bool		compare					(const	CPHScriptObjectCondition* v)	const override {return v->compare(m_lua_object);}
	virtual bool		compare					(const	CPHScriptObjectAction* v)		const override {return v->compare(m_lua_object);}
	virtual bool		compare					(const	CPHScriptObjectConditionN* v)	const override {return v->compare(m_lua_object);}
	virtual bool		compare					(const	CPHScriptObjectActionN* v)		const override {return v->compare(m_lua_object);}
};


class CPHSriptReqGObjComparer :
	public CPHReqComparerV
{
	CObject				*m_object;

public:
	CPHSriptReqGObjComparer	(CObject* object)			{m_object= object;}
	virtual bool		compare					(const	CPHScriptGameObjectAction* v)	const override {return v->compare(m_object);}
	virtual bool		compare					(const	CPHScriptGameObjectCondition* v)const override {return v->compare(m_object);}
};