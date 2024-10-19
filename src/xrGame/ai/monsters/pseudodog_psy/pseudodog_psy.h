#pragma once
#include "../pseudodog/pseudodog.h"
#include "../../../../xrScripts/script_export_space.h"

class CPseudoPsyDogPhantomBase;

class CPseudoPsyDogBase : public CPseudoDogBase 
{
protected:
	using inherited = CPseudoDogBase;

	friend class CPsyDogAura;
	friend class CPseudoPsyDogPhantomBase;

	CPsyDogAura		*m_aura;

	CActor			*m_enemy;

	u8				m_max_phantoms_count;
	u32				m_time_phantom_respawn;

	TTime*			m_phantoms_die_time;

public:
						CPseudoPsyDogBase				();
		virtual			~CPseudoPsyDogBase			() override;

		virtual void	Load				(LPCSTR section) override;
		virtual BOOL	net_Spawn			(CSE_Abstract *dc) override;
		virtual void	reinit				() override;
		virtual void	reload				(LPCSTR section) override;
		virtual void	net_Destroy			() override;
		virtual void	Die					(CObject* who) override;

		virtual void	Think				() override;

		IStateManagerBase *create_state_manager	();

		virtual	char*	get_monster_class_name () override { return (char*) "psydog"; }

				u8		get_phantoms_count	();
				bool	must_hide			() {return get_phantoms_count() == 0;}
private:
				bool	spawn_phantom		();
				void	register_phantom	(CPseudoPsyDogPhantomBase* phantom);
				void	unregister_phantom	(CPseudoPsyDogPhantomBase* phantom);

				void	delete_all_phantoms	();

private:
	xr_vector<CPseudoPsyDogPhantomBase*> m_storage;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
