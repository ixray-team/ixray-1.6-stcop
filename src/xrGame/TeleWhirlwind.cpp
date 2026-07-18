#include "StdAfx.h"
#include "TeleWhirlwind.h"
#include "../xrPhysics/PhysicsShell.h"
#include "PhysicsShellHolder.h"
#include "Level.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "xrMessages.h"
#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/KinematicsAnimated.h"

CTeleWhirlwind ::CTeleWhirlwind () 
{
	m_owner_object = nullptr;
	m_center.set(0.f, 0.f, 0.f);
	m_keep_radius = 1.f;
	m_throw_power = 100.f;
}

void CTeleWhirlwind::clear_impacts()
{
	m_saved_impacts.clear();
}

void CTeleWhirlwind::add_impact(const Fvector& dir,float val)
{
	Fvector force,point;
	force.set(dir);
	force.mul(val);
	point.set(0.f,0.f,0.f);
	m_saved_impacts.push_back(SPHImpact(force,point,0));
}

void CTeleWhirlwind::set_throw_power(float throw_pow)
{
	m_throw_power=throw_pow;
}

void CTeleWhirlwind::draw_out_impact(Fvector& dir,float& val)
{
	VERIFY2(m_saved_impacts.size(),"NO IMPACTS ADDED!");

	dir.set(m_saved_impacts[0].force);
	val=dir.magnitude();
	if(!fis_zero(val))dir.mul(1.f/val);

	if (m_saved_impacts.size())
	{
		m_saved_impacts.erase(m_saved_impacts.begin());
	}
}

void CTeleWhirlwind::play_destroy(CTeleWhirlwindObject *obj)
{
	
}

CTeleWhirlwindObject::CTeleWhirlwindObject(const STelekineticObjectParams& tele_object_params) : STelekineticObject(tele_object_params)
{
	throw_power = tele_object_params.strength;

	if (tele_object_params.object->PPhysicsShell())
	{
		tele_object_params.object->PPhysicsShell()->SetAirResistance(0.f, 0.f);
		tele_object_params.object->m_pPhysicsShell->set_ApplyByGravity(TRUE);
	}

	if (tele_object_params.object->ph_destroyable() && tele_object_params.object->ph_destroyable()->CanDestroy())
	{
		b_destroyable = true;
	}
	else
	{
		b_destroyable = false;
	}
	
	set_throw_power(params.telekinesis->cast_telekinesis_telewhirlwind()->get_throw_power());
}

void CTeleWhirlwindObject::raise_update()
{

}

void CTeleWhirlwindObject::release()
{
	if (!params.object ||params.object->getDestroy() ||!params.object->m_pPhysicsShell || !params.object->m_pPhysicsShell->isActive()) 
		return;
	
	Fvector dir_inv;
	dir_inv.sub(params.object->Position(), params.telekinesis->cast_telekinesis_telewhirlwind()->Center());
	float magnitude	= dir_inv.magnitude();
	params.object->m_pPhysicsShell->set_ApplyByGravity(true);

	float impulse=0.f;
	if(magnitude>0.2f)
	{
		dir_inv.mul(1.f/magnitude);
		impulse=throw_power/magnitude/magnitude;
	}
	else
	{
		dir_inv.random_dir();
		impulse=throw_power*100.f;
	}

	bool b_destroyed=false;
	if(magnitude<2.f*params.object->Radius())
	{
		b_destroyed=destroy_object(dir_inv,throw_power*100.f);
	}

	if(!b_destroyed)params.object->m_pPhysicsShell->applyImpulse(dir_inv,impulse);
	switch_state(ETelekineticState::TS_NONE);
}

bool CTeleWhirlwindObject::destroy_object(const Fvector dir, float val)
{
	CPHDestroyable* D = params.object->ph_destroyable();
	if (D)
	{
		D->PhysicallyRemoveSelf();
		D->Destroy(params.telekinesis->cast_telekinesis_telewhirlwind()->OwnerObject()->ID());

		if (IsGameTypeSingle())
		{
			xr_vector<shared_str>::iterator i = D->m_destroyed_obj_visual_names.begin();
			xr_vector<shared_str>::iterator e = D->m_destroyed_obj_visual_names.end();
			for (; e != i; i++)
				params.telekinesis->cast_telekinesis_telewhirlwind()->add_impact(dir, val * 10.f);
		};

		u16 root = (smart_cast<IKinematics*>(params.object->Visual()))->LL_GetBoneRoot();
		TParticlesPlayer* PPlayer = params.object->GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StartParticles(params.telekinesis->cast_telekinesis_telewhirlwind()->destroing_particles(), root, Fvector().set(0, 1, 0), params.telekinesis->cast_telekinesis_telewhirlwind()->OwnerObject()->ID());

		return true;
	}
	return false;
}

void CTeleWhirlwindObject::raise(float step)
{

		CPhysicsShell*	p					=	get_object()	->PPhysicsShell();
	
		if(!p||!p->isActive())	
			return;
		else
			{
				p->SetAirResistance(0.f,0.f);
				p->set_ApplyByGravity(true);
			}
		u16				element_number		=	p				->get_ElementsNumber();
		Fvector			center				=	params.telekinesis->cast_telekinesis_telewhirlwind()	->Center();
		CPhysicsElement* maxE=p->get_ElementByStoreOrder(0);
		for(u16 element=0;element<element_number;++element)
		{
			float k=params.strength;//600.f;
			float predict_v_eps=0.1f;
			float mag_eps	   =.01f;

			CPhysicsElement* E=	p->get_ElementByStoreOrder(element);
			if(maxE->getMass()<E->getMass())	maxE=E;
			if (!E->isActive()) continue;
			Fvector pos=E->mass_Center();

			Fvector diff;
			diff.sub(center,pos);
			float mag=_sqrt(diff.x*diff.x+diff.z*diff.z);
			Fvector lc;lc.set(center);
			if(mag>1.f)
			{
				lc.y/=mag;
			}
			diff.sub(lc,pos);
			mag=diff.magnitude();
			float accel=k/mag/mag/mag;//*E->getMass()
			Fvector dir;
			if(mag<mag_eps)
			{
				accel=0.f;
				//Fvector zer;zer.set(0,0,0);
				//E->set_LinearVel(zer);
				dir.random_dir();
			}
			else
			{
				dir.set(diff);dir.mul(1.f/mag);
			}
			Fvector vel;
			E->get_LinearVel(vel);
			float delta_v=accel*fixed_step;
			Fvector delta_vel; delta_vel.set(dir);delta_vel.mul(delta_v);
			Fvector predict_vel;predict_vel.add(vel,delta_vel);
			Fvector delta_pos;delta_pos.set(predict_vel);delta_pos.mul(fixed_step);
			Fvector predict_pos;predict_pos.add(pos,delta_pos);
			
			Fvector predict_diff;predict_diff.sub(lc,predict_pos);
			float predict_mag=predict_diff.magnitude();
			float predict_v=predict_vel.magnitude();

			Fvector force;force.set(dir);
			if(predict_mag>mag && predict_vel.dotproduct(dir)>0.f && predict_v>predict_v_eps)
			{
	
				Fvector motion_dir;motion_dir.set(predict_vel);motion_dir.mul(1.f/predict_v);
				float needed_d=diff.dotproduct(motion_dir);
				Fvector needed_diff;needed_diff.set(motion_dir);needed_diff.mul(needed_d);
				Fvector nearest_p;nearest_p.add(pos,needed_diff);//
				Fvector needed_vel;needed_vel.set(needed_diff);needed_vel.mul(1.f/fixed_step);
				force.sub(needed_vel,vel);
				force.mul(E->getMass()/fixed_step);
			}
			else
			{
				force.mul(accel*E->getMass());
			}
			
			
			E->applyForce(force.x,force.y+get_object()->EffectiveGravity()*E->getMass(),force.z);
		}
		Fvector dist;dist.sub(center,maxE->mass_Center());
		if(dist.magnitude()<params.telekinesis->cast_telekinesis_telewhirlwind()->keep_radius()&&b_destroyable)
		{
			p->setTorque(Fvector().set(0,0,0));
			p->setForce(Fvector().set(0,0,0));
			p->set_LinearVel(Fvector().set(0,0,0));
			p->set_AngularVel(Fvector().set(0,0,0));
			switch_state(ETelekineticState::TS_KEEP);
		}
}

void CTeleWhirlwindObject::perform_keep_object()
{
	CPhysicsShell*	p					=	get_object()	->PPhysicsShell();
	if(!p||!p->isActive())	
		return;
	else
	{
		p->SetAirResistance(0.f,0.f);
		p->set_ApplyByGravity(false);
	}

	u16				element_number		=	p				->get_ElementsNumber();
	Fvector			center				=	params.telekinesis->cast_telekinesis_telewhirlwind()	->Center();

	CPhysicsElement* maxE=p->get_ElementByStoreOrder(0);
	for(u16 element=0;element<element_number;++element)
	{
		
		CPhysicsElement* E=	p->get_ElementByStoreOrder(element);
		if(maxE->getMass()<E->getMass())maxE=E;
		Fvector			dir;dir.sub(center,E->mass_Center());
		dir.normalize_safe();
		Fvector vel;
		E->get_LinearVel(vel);
		float force=dir.dotproduct(vel)*E->getMass()/2.f;
		if(force<0.f)
		{
			dir.mul(force);
		}
	}
	
	maxE->setTorque(Fvector().set(0,500.f,0));

	Fvector dist;dist.sub(center,maxE->mass_Center());
	if(dist.magnitude()>params.telekinesis->cast_telekinesis_telewhirlwind()->keep_radius()*1.5f)
	{
		p->setTorque(Fvector().set(0,0,0));
		p->setForce(Fvector().set(0,0,0));
		p->set_LinearVel(Fvector().set(0,0,0));
		p->set_AngularVel(Fvector().set(0,0,0));
		p->set_ApplyByGravity(true);
		switch_state(ETelekineticState::TS_RAISE);
	}
}

void CTeleWhirlwindObject::fire(const Fvector& target)
{

}

void CTeleWhirlwindObject::throw_object(const Fvector& target, float power)
{

}

void CTeleWhirlwindObject::set_throw_power(float throw_pow)
{
	throw_power=throw_pow;
}

void CTeleWhirlwindObject::switch_state(ETelekineticState new_state)
{
	inherited::switch_state(new_state);
}

