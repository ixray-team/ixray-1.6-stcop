#include "StdAfx.h"
#include "PHDynamicData.h"
#include "Physics.h"
#include "tri-colliderknoopc/dTriList.h"
#include "PHContactBodyEffector.h"
#include "../xrEngine/gamemtllib.h"
#include "PHCollideValidator.h"
#ifdef DEBUG
#include "debug_output.h"
#endif
///////////////////////////////////////////////////////////////
#pragma warning(disable:4995)
#pragma warning(disable:4267)
#include "../3rd party/ode/ode/src/collision_kernel.h"
#include "../3rd party/ode/ode/src/joint.h"
#include "../3rd party/ode/ode/src/objects.h"
#pragma warning(default:4267)
#pragma warning(default:4995)

extern CPHWorld *ph_world;
///////////////////////////////////////////////////////////////////

#include "ExtendedGeom.h"

const float 		default_w_limit									= 9.8174770f;//(M_PI/16.f/(fixed_step=0.02f));
const float 		default_l_limit									= 150.f;//(3.f/fixed_step=0.02f);
const float 		default_l_scale									= 1.01f;
const float 		default_w_scale									= 1.01f;
const float			default_k_l										= 0.0002f;//square resistance !!
const float			default_k_w										= 0.05f;


extern const u16	max_joint_allowed_for_exeact_integration		= 30;

//base	params
const float base_fixed_step											=	0.02f				;
const float base_erp												=	0.54545456f			;
const float base_cfm												=	1.1363636e-006f		;
//base params
float 			fixed_step											=	0.01f;
float 			world_cfm											=	Cfm(Spring(base_cfm,base_erp,base_fixed_step), Damping(base_cfm,base_erp));
float 			world_erp											=	Erp(Spring(base_cfm,base_erp,base_fixed_step), Damping(base_cfm,base_erp));
float			world_spring										=	1.0f* Spring(world_cfm,world_erp);
float			world_damping										=	1.0f* Damping(world_cfm,world_erp);


const float			default_world_gravity							=	2*9.81f;


/////////////////////////////////////////////////////
int			phIterations											= 18;
float		phTimefactor											= 1.f;
Fbox		phBoundaries											= {1000.f,1000.f,-1000.f,-1000.f};
dWorldID	phWorld;

/////////////////////////////////////
dJointGroupID	ContactGroup;
CBlockAllocator	<dJointFeedback,128>		ContactFeedBacks;
CBlockAllocator	<CPHContactBodyEffector,128> ContactEffectors;

/////////////////////////////////////////////////////////////////////////////
IC void add_contact_body_effector(dBodyID body,const dContact& c,SGameMtl* material)
{
	CPHContactBodyEffector* effector=(CPHContactBodyEffector*)dBodyGetData(body);
	if(effector)
		effector->Merge(c,material);
	else
	{	
		effector=ContactEffectors.add();
		effector->Init(body,c,material);
		dBodySetData(body,(void*)effector);
	}
}

IC static int CollideIntoGroup(dGeomID o1, dGeomID o2,dJointGroupID jointGroup,CPHIsland* world,const int &MAX_CONTACTS)
{
	const int RS= 800+10;
	const int N = RS;
	
	static dContact contacts[RS];
	int	collided_contacts=0;
	// get the contacts up to a maximum of N contacts
	int n;
	
	VERIFY	(o1);
	VERIFY	(o2); 
	VERIFY(&contacts[0].geom);
	n		= dCollide(o1, o2, N, &contacts[0].geom, sizeof(dContact));	

	if(n>N-1)
		n=N-1;
	int i;
	

	for(i = 0; i < n; ++i)
	{
		dContact				&c		=contacts[i];
		dContactGeom			&cgeom	=c.geom;
		dSurfaceParameters		&surface=c.surface;
		dGeomID					g1		=cgeom.g1;
		dGeomID					g2		=cgeom.g2;
		bool pushing_neg=	false;
		bool do_collide	=	true;
		dxGeomUserData* usr_data_1		=NULL;
		dxGeomUserData* usr_data_2		=NULL;
		u16				material_idx_1	=0;
		u16				material_idx_2	=0;

		surface.mu =1.f;
		surface.soft_erp=1.f;
		surface.soft_cfm=1.f;
		surface.bounce = 0.01f;//0.1f;
		surface.bounce_vel =1.5f;//0.005f;

		usr_data_1 = retrieveGeomUserData(g1);
		usr_data_2 = retrieveGeomUserData(g2);

		///////////////////////////////////////////////////////////////////////////////////////////////////
		if(usr_data_2)	material_idx_2=usr_data_2->material;
		if(usr_data_1)	material_idx_1=usr_data_1->material;
		bool is_tri_1=dTriListClass == dGeomGetClass(g1);
		bool is_tri_2=dTriListClass == dGeomGetClass(g2);
		if(!is_tri_2&&!is_tri_1) surface.mode=0;
		if(is_tri_1) material_idx_1=(u16)surface.mode;
		if(is_tri_2) material_idx_2=(u16)surface.mode;
		SGameMtl* material_1=GMLibrary().GetMaterialByIdx(material_idx_1);
		SGameMtl* material_2=GMLibrary().GetMaterialByIdx(material_idx_2);

		////////////////params can be changed in callbacks//////////////////////////////////////////////////////////////////////////
		surface.mode = dContactApprox1 | dContactSoftERP | dContactSoftCFM;
		float spring = material_2->fPHSpring * material_1->fPHSpring * world_spring;
		float damping = material_2->fPHDamping * material_1->fPHDamping * world_damping;
		surface.soft_erp = Erp(spring, damping);
		surface.soft_cfm = Cfm(spring, damping);
		surface.mu=material_2->fPHFriction*material_1->fPHFriction;

		/////////////////////////////////////////////////////////////////////////////////////////////////
		Flags32	&flags_1=material_1->Flags;
		Flags32	&flags_2=material_2->Flags;

		if(is_tri_1)
		{
#pragma warning(push)
#pragma warning(disable:4245)
			if(material_1->Flags.test(SGameMtl::flSlowDown)&&!(usr_data_2->pushing_neg||usr_data_2->pushing_b_neg))
#pragma warning(pop)
			{
				dBodyID body=dGeomGetBody(g2);
				R_ASSERT2(body,"static - static collision !!!");
				if(material_1->Flags.test(SGameMtl::flLiquid))
				{
					add_contact_body_effector(body,c,material_1);
				}
				else
				{
					if(!usr_data_2 || !usr_data_2->ph_object || !usr_data_2->ph_object->IsRayMotion())
					{
						add_contact_body_effector(body,c,material_1);
					}
				}
				
			}
			if(material_1->Flags.test(SGameMtl::flPassable)) 
				do_collide=false;
		}
		if(is_tri_2)
		{
#pragma warning(push)
#pragma warning(disable:4245)
			if(material_2->Flags.test(SGameMtl::flSlowDown)&&!(usr_data_1->pushing_neg||usr_data_1->pushing_b_neg))
#pragma warning(pop)
			{

				dBodyID body=dGeomGetBody(g1);
				R_ASSERT2(body,"static - static collision !!!");
				if(material_2->Flags.test(SGameMtl::flLiquid))
				{
					add_contact_body_effector(body,c,material_2);
				}
				else
				{
					if(!usr_data_1 || !usr_data_1->ph_object || !usr_data_1->ph_object->IsRayMotion())
					{
						add_contact_body_effector(body,c,material_2);
					}
				}

			}
			if(material_2->Flags.test(SGameMtl::flPassable)) 
				do_collide=false;

		}
	
		if(flags_1.test(SGameMtl::flBounceable)&&flags_2.test(SGameMtl::flBounceable))
		{
			surface.mode		|=	dContactBounce;
			surface.bounce_vel	=	_max(material_1->fPHBounceStartVelocity,material_2->fPHBounceStartVelocity);
			surface.bounce		=	_min(material_1->fPHBouncing,material_2->fPHBouncing);
		}
		/////////////////////////////////////////////////////////////////////////////////////////////////
		if(usr_data_2&&usr_data_2->object_callbacks){
			usr_data_2->object_callbacks->Call(do_collide,false,c,material_1,material_2);
		}

		if(usr_data_1&&usr_data_1->object_callbacks){
			usr_data_1->object_callbacks->Call(do_collide,true,c,material_1,material_2);
		}

		if(usr_data_2){
			usr_data_2->pushing_b_neg	=	usr_data_2->pushing_b_neg	&& !GMLibrary().GetMaterialByIdx(usr_data_2->b_neg_tri->material)->Flags.test(SGameMtl::flPassable);
			usr_data_2->pushing_neg		=	usr_data_2->pushing_neg		&& !GMLibrary().GetMaterialByIdx(usr_data_2->neg_tri->material)->Flags.test(SGameMtl::flPassable);
			pushing_neg=usr_data_2->pushing_b_neg||usr_data_2->pushing_neg;
			if(usr_data_2->ph_object){
				usr_data_2->ph_object->InitContact(&c,do_collide,material_idx_1,material_idx_2);
			}

		}
		///////////////////////////////////////////////////////////////////////////////////////
		if(usr_data_1){ 
			usr_data_1->pushing_b_neg	=	usr_data_1->pushing_b_neg	&& !GMLibrary().GetMaterialByIdx(usr_data_1->b_neg_tri->material)->Flags.test(SGameMtl::flPassable);
			usr_data_1->pushing_neg		=	usr_data_1->pushing_neg		&& !GMLibrary().GetMaterialByIdx(usr_data_1->neg_tri->material)->Flags.test(SGameMtl::flPassable);
			pushing_neg=usr_data_1->pushing_b_neg||usr_data_1->pushing_neg;
			if(usr_data_1->ph_object){
				usr_data_1->ph_object->InitContact(&c,do_collide,material_idx_1,material_idx_2);

			}
		}

		if (pushing_neg) {
#ifdef _M_X64
			surface.mu = FLT_MAX;
#else
			surface.mu = dInfinity;
#endif
		}
		if	(do_collide && collided_contacts<MAX_CONTACTS)
		{
			++collided_contacts;
			#ifdef DEBUG
			if( debug_output().ph_dbg_draw_mask().test(phDbgDrawContacts) )
				debug_output().DBG_DrawContact(c);
			#endif
			dJointID contact_joint	= dJointCreateContact(0, jointGroup, &c);
			world->ConnectJoint(contact_joint);
			dJointAttach			(contact_joint, dGeomGetBody(g1), dGeomGetBody(g2));
		}
	}
	return collided_contacts;
}

void NearCallback(CPHObject* obj1,CPHObject* obj2, dGeomID o1, dGeomID o2)
{	
	
	CPHIsland* island1=obj1->DActiveIsland();
	CPHIsland* island2=obj2->DActiveIsland();
	obj2->near_callback(obj1);
	int MAX_CONTACTS=-1;
	if(!island1->CanMerge(island2,MAX_CONTACTS)) return;
	if(CollideIntoGroup(o1,o2,ContactGroup,island1,MAX_CONTACTS)!=0)
	{	
		obj1->MergeIsland(obj2);
		if(!obj2->is_active())obj2->EnableObject(obj1);
	}
}

void CollideStatic(dGeomID o2,CPHObject* obj2)
{
	
	CPHIsland* island2=obj2->DActiveIsland();
	CollideIntoGroup(ph_world->GetMeshGeom(),o2,ContactGroup,island2,island2->MaxJoints());
}

//limit for angular accel
void dBodyAngAccelFromTorqu(const dBodyID body, dReal* ang_accel, const dReal* torque){
	dMass m;
	dMatrix3 invI;
	dBodyGetMass(body,&m);
	dInvertPDMatrix (m.I, invI, 3);
	dMULTIPLY1_333(ang_accel,invI, torque);
}

void FixBody(dBodyID body,float ext_param,float mass_param)
{
	dMass m;
	dMassSetSphere(&m,1.f,ext_param);
	dMassAdjust(&m,mass_param);
	dBodySetMass(body,&m);
	dBodySetGravityMode(body,0);
	dBodySetLinearVel(body,0,0,0);
	dBodySetAngularVel(body,0,0,0);
	dBodySetForce(body,0,0,0);
	dBodySetTorque(body,0,0,0);
}

void FixBody(dBodyID body)
{
	FixBody(body,fix_ext_param,fix_mass_param);
}

void BodyCutForce(dBodyID body,float l_limit,float w_limit)
{
	const dReal wa_limit=w_limit/fixed_step;
	const dReal* force=	dBodyGetForce(body);
	dReal force_mag=dSqrt(dDOT(force,force));

	//body mass
	dMass m;
	dBodyGetMass(body,&m);

	dReal force_limit =l_limit/fixed_step*m.mass;

	if(force_mag>force_limit)
	{
		dBodySetForce
		(
			body,
			force[0]/force_mag*force_limit,
			force[1]/force_mag*force_limit,
			force[2]/force_mag*force_limit
		);
	}

	const dReal* torque=dBodyGetTorque(body);
	dReal torque_mag=dSqrt(dDOT(torque,torque));

	if(torque_mag<0.001f) return;

	dMatrix3 tmp,invI,I;

	// compute inertia tensor in global frame
	dMULTIPLY2_333 (tmp,m.I,body->R);
	dMULTIPLY0_333 (I,body->R,tmp);

	// compute inverse inertia tensor in global frame
	dMULTIPLY2_333 (tmp,body->invI,body->R);
	dMULTIPLY0_333 (invI,body->R,tmp);

	//angular accel
	dVector3 wa;
	dMULTIPLY0_331(wa,invI,torque);
	dReal wa_mag=dSqrt(dDOT(wa,wa));

	if(wa_mag>wa_limit)
	{
		//scale w 
		for(int i=0;i<3;++i)
			wa[i]*=wa_limit/wa_mag;

		dVector3 new_torqu;

		dMULTIPLY0_331(new_torqu,I,wa);

		dBodySetTorque 
		(
			body,
			new_torqu[0],
			new_torqu[1],
			new_torqu[2]
		);
	}
}

void dMassSub(dMass *a,const dMass *b)
{
	int i;
	VERIFY (a && b);
	dReal denom = dRecip (a->mass-b->mass);
	for (i=0; i<3; ++i) a->c[i] = (a->c[i]*a->mass - b->c[i]*b->mass)*denom;
	a->mass-=b->mass;
	for (i=0; i<12; ++i) a->I[i] -= b->I[i];
}


////Energy of non Elastic collision;
//body - static case
float E_NlS(dBodyID body,const dReal* norm,float norm_sign)		//if body c.geom.g1 norm_sign + else -
{																//norm*norm_sign - to body
	const dReal* vel=dBodyGetLinearVel(body);
	dReal prg=-dDOT(vel,norm)*norm_sign;
	prg=prg<0.f ? prg=0.f : prg;
	dMass mass;
	dBodyGetMass(body,&mass);
	return mass.mass*prg*prg/2;
}

//body - body case
float E_NLD(dBodyID b1,dBodyID b2,const dReal* norm)// norm - from 2 to 1
{
	dMass m1,m2;
	dBodyGetMass(b1,&m1);dBodyGetMass(b2,&m2);
	const dReal* vel1   =dBodyGetLinearVel(b1);
	const dReal* vel2   =dBodyGetLinearVel(b2);

	dReal vel_pr1=dDOT(vel1,norm);
	dReal vel_pr2=dDOT(vel2,norm);

	if(vel_pr1>vel_pr2) return 0.f; //exit if the bodies are departing

	dVector3 impuls1={vel1[0]*m1.mass,vel1[1]*m1.mass,vel1[2]*m1.mass};
	dVector3 impuls2={vel2[0]*m2.mass,vel2[1]*m2.mass,vel2[2]*m2.mass};

	dVector3 c_mas_impuls={impuls1[0]+impuls2[0],impuls1[1]+impuls2[1],impuls1[2]+impuls2[2]};
	dReal cmass=m1.mass+m2.mass;
	dVector3 c_mass_vel={c_mas_impuls[0]/cmass,c_mas_impuls[1]/cmass,c_mas_impuls[2]/cmass};

	dReal c_mass_vel_prg=dDOT(c_mass_vel,norm);

	dReal kin_energy_start=vel_pr1*vel_pr1*m1.mass/2.f+vel_pr2*vel_pr2*m2.mass/2.f;
	dReal kin_energy_end=c_mass_vel_prg*c_mass_vel_prg*cmass/2.f;

	return (kin_energy_start-kin_energy_end);
}

float E_NL(dBodyID b1, dBodyID b2, const dReal* norm)
{
	VERIFY(b1 || b2);
	if (b1)
	{
		if (b2)
			return E_NLD(b1, b2, norm);
		else
			return E_NlS(b1, norm, 1);
	}
	else
		return E_NlS(b2, norm, -1);
}

void ApplyGravityAccel(dBodyID body,const dReal* accel)
{
	dMass m;
	dBodyGetMass(body,&m);
	dBodyAddForce(body,accel[0]*m.mass,accel[1]*m.mass,accel[2]*m.mass);
}

const dReal* dJointGetPositionContact(dJointID joint)
{
	VERIFY2(dJointGetType(joint)==dJointTypeContact,"not a contact!");
	dxJointContact* c_joint=(dxJointContact*)joint;
	return c_joint->contact.geom.pos;
}
