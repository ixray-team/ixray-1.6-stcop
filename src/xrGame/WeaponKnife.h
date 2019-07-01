#pragma once

#include "WeaponCustomPistol.h"
#include "script_export_space.h"

class CWeaponKnife: public CWeapon {
private:
	typedef CWeapon inherited;

protected:

	virtual void		switch2_Idle				();
	virtual void		switch2_Hiding				();
	virtual void		switch2_Hidden				();
	virtual void		switch2_Showing				();
			void		switch2_Attacking			(u32 state);

	virtual void		OnAnimationEnd				(u32 state);
	virtual void		OnMotionMark				(u32 state, const motion_marks&);
	virtual void		OnStateSwitch				(u32 S);

	void				state_Attacking				(float dt);

	virtual void		KnifeStrike					(const Fvector& pos, const Fvector& dir);

	float				fWallmarkSize;
	u16					knife_material_idx;

protected:	
	ALife::EHitType		m_eHitType;

	ALife::EHitType		m_eHitType_1;
	Fvector4			fvHitPower_1;
	Fvector4			fvHitPowerCritical_1;
	float				fHitImpulse_1;

	ALife::EHitType		m_eHitType_2;
	Fvector4			fvHitPower_2;
	Fvector4			fvHitPowerCritical_2;
	float				fHitImpulse_2;

	float				fCurrentHit;

	float				fHitImpulse_cur;

protected:
	virtual void		LoadFireParams					(LPCSTR section);
public:
						CWeaponKnife(); 
	virtual				~CWeaponKnife(); 

	void				Load							(LPCSTR section);

	virtual bool		IsZoomEnabled					()	const	{return false;}

			void		Fire2Start						();
	virtual void		FireStart						();


	virtual bool		Action							(u16 cmd, u32 flags);

	virtual bool		GetBriefInfo					(II_BriefInfo& info);

#ifdef DEBUG
	virtual void		OnRender						();
#endif

private:
#ifdef DEBUG
	struct dbg_draw_data
	{
		Fvector		m_pos;
		Fvector		m_endpos;
		float		m_splash_radius;
		Fvector		m_pick_vector;
	};
	dbg_draw_data	m_dbg_data;
#endif
	float			m_Hit1Distance;
	float			m_Hit2Distance;

	Fvector3		m_Hit1SpashDir;
	Fvector3		m_Hit2SpashDir;
	
	float			m_Hit1SplashRadius;
	float			m_Hit2SplashRadius;

	shared_str		m_SplashHitBone;

	float			m_hit_dist;
	Fvector3		m_splash_dir;
	float			m_splash_radius;


	void			MakeShot				(Fvector const & pos, Fvector const & dir);
	void			GetVictimPos			(CEntityAlive* victim, Fvector & pos_dest);
	CObject*		SelectBestHitVictim		(Fvector const & f_pos, Fvector & new_hit_dir);
	CObject*		TryPick					(Fvector const & start_pos,
											 Fvector const & dir,
											 float const dist);

	static BOOL		RayQueryCallback		(collide::rq_result& result, LPVOID this_ptr);
	collide::rq_results				m_ray_query_results;
	u16								m_except_id;
	CObject*						m_last_picked_obj;

	typedef xr_vector<ISpatial*>	spartial_base_t;
	xr_vector<ISpatial*>	m_spartial_query_res;

	class best_victim_selector
	{
	public:
		best_victim_selector	(u16 except_id,
								 CWeaponKnife* owner,
								 Fvector const & pos,
								 Fvector & best_dir_dest,
								 CObject* & best_obj_dest,
								 float query_distance);
		best_victim_selector(best_victim_selector const & copy);
		void operator()(spartial_base_t::value_type const & left);
	private:
		best_victim_selector & operator=(best_victim_selector const & copy) {};
		
		Fvector			m_start_pos;
		CObject* &		m_best_object_dest;
		Fvector	&		m_best_object_dir;
		CWeaponKnife*	m_owner;
		float			m_min_dist;
		float			m_query_distance;
		u16				m_except_id;
	};//struct best_victim_selector

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
add_to_type_list(CWeaponKnife)
#undef script_type_list
#define script_type_list save_type_list(CWeaponKnife)
