////////////////////////////////////////////////////////////////////////////
//	Module 		: script_binder.h
//	Created 	: 26.03.2004
//  Modified 	: 26.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script objects binder
////////////////////////////////////////////////////////////////////////////

#pragma once

class CSE_Abstract;
class CScriptBinderObject;
class NET_Packet;

class CScriptBinder {
protected:
	CScriptBinderObject			*m_object;

public:
								CScriptBinder	();
	virtual						~CScriptBinder	();
			void				init			();
			void				clear			();
	virtual void				reinit			();
	virtual void				Load			(const char* section);
	virtual void				reload			(const char* section);
	virtual bool				net_Spawn		(CSE_Abstract* DC);
	virtual void				net_Destroy		();
	virtual void				shedule_Update	(u32 time_delta);
	virtual void				save			(NET_Packet &output_packet);
	virtual void				load			(IReader &input_packet);
	virtual void Serialize(ISaveObject& Object);
	virtual bool				net_SaveRelevant();
	virtual void				net_Relcase		(CObject *object);
			void				set_object		(CScriptBinderObject *object);
	IC		CScriptBinderObject	*object			();
};

#include "script_binder_inline.h"