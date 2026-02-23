#ifndef TELE_WHIRLWIND
#define TELE_WHIRLWIND
#include "ai/monsters/telekinesis.h"
#include "ai/monsters/telekinetic_object.h"
#include "../xrPhysics/PHImpact.h"

class CTeleWhirlwind;
class CGameObject;
struct CTeleWhirlwindObject : STelekineticObject
{
	using inherited = STelekineticObject;
	CTeleWhirlwind		*m_telekinesis;
	bool				b_destroyable;
	float				throw_power;

	virtual							~CTeleWhirlwindObject	(){};
									CTeleWhirlwindObject	(CTelekinesis* tele, CPhysicsShellHolder* owner, float s, float h, u32 ttk, bool rot);
				void				set_throw_power			(float throw_pow);
	virtual		void				raise					(float step);
	virtual		void				raise_update			();
	virtual		void				perform_keep_object					();
	virtual		void				release					();
	virtual		void				fire					(const Fvector &target);
	virtual		void				throw_object					(const Fvector &target, float power);
	virtual		void				switch_state			(ETelekineticState new_state);
	virtual		bool				destroy_object			(const Fvector dir,float val);

	virtual CTeleWhirlwindObject* cast_whirlwind_object() { return this; }
};

class CTeleWhirlwind : public CTelekinesis
{
typedef	CTelekinesis inherited;
		Fvector				m_center;
		float				m_keep_radius;
		float				m_throw_power;
		CGameObject*		m_owner_object;
		PH_IMPACT_STORAGE	m_saved_impacts;
		shared_str			m_destroying_particles;

public: 
								CTeleWhirlwind			();
		CGameObject*    		OwnerObject				()const									{return m_owner_object;}
  const	Fvector&				Center					()const									{return m_center;}
		void					SetCenter				(const Fvector center)					{m_center.set(center);}
		void					SetOwnerObject			(CGameObject* owner_object)				{m_owner_object=owner_object;}
		void					add_impact				(const Fvector& dir,float val)			;
		void					draw_out_impact			(Fvector& dir,float& val)				;
		void					clear_impacts			()										;
		void					set_destroing_particles (const shared_str& destroying_particles){m_destroying_particles=destroying_particles;}
		const shared_str&		destroing_particles		()										{return m_destroying_particles;}
		void					play_destroy			(CTeleWhirlwindObject* obj);
virtual void					clear					()										;
virtual	void					clear_notrelevant		()										;
		float					keep_radius				()										{return m_keep_radius;}
		void					set_throw_power			(float throw_pow);
		ICF float				get_throw_power() { return m_throw_power; };
};


#endif