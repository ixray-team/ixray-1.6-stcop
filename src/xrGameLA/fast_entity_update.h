#pragma once
#ifndef __FAST_ENTITY_UPDATE__
#define __FAST_ENTITY_UPDATE__

#include "../xrScripts/script_callback_ex.h"

class CGameObject;

class CFastEntityUpdater
{
	typedef CScriptCallbackEx<void> CScriptCallbackExVoid;
	private:	
		struct SEntityCall
		{
			private:
				CGameObject					*m_object;
				CScriptCallbackExVoid		m_callback;
			public:
											SEntityCall							(CGameObject *game_object, const luabind::functor<void> &functor, const luabind::object &object);
				virtual						~SEntityCall						();
				CGameObject					*GetGameObject						() const { return m_object; }
				CScriptCallbackExVoid		&GetCallback						() { return m_callback; }
		};

		struct SEntityCallPred
		{
			private:
				u16							m_id;
			public:
											SEntityCallPred						(u16 id) : m_id(id)			{ }
				bool						operator()							(SEntityCall* call);	
		};

		using ENTITIES_CALLS = xr_vector<SEntityCall*>;
		using ENTITIES_CALLS_IT = ENTITIES_CALLS::iterator;

	private:
		ENTITIES_CALLS						m_calls;
	public:
											CFastEntityUpdater					();
		virtual								~CFastEntityUpdater					();

		void								AddCall								(CGameObject *game_object, const luabind::functor<void> &functor, const luabind::object &object);
		void								AddCall								(SEntityCall *call);
		void								RemoveCall							(u16 id);

		void								Update								();
		
		void								Clear								();

};



#endif