/////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///////////////////////////Implemetation//for//CPhysicsElement//////////////////
////////////////////////////////////////////////////////////////////////////////
#include "Geometry.h"
#include "PHDefs.h"
#include "PhysicsCommon.h"
#include "../../xrServerEntities/PHSynchronize.h"
#include "PHDisabling.h"
#include "PHGeometryOwner.h"
#include "PHInterpolation.h"
#include "PHFracture.h"
#include "physics_scripted.h"
#ifndef PH_ELEMENT
#define PH_ELEMENT
class CPHElement;
class CPHShell;


struct SPHImpact;
class CPHFracturesHolder;

class CPHElement	:  
	public	CPhysicsElement ,
	public	CPHSynchronize,
	public	CPHDisablingFull,
	public	CPHGeometryOwner,
	public	cphysics_scripted
{
	friend class CPHFracturesHolder;

	//float						m_start_time;				//uu ->to shell ??	//aux
	dMass						m_mass;						//e ??				//bl
	dBodyID						m_body;						//e					//st
	dReal						m_l_scale;					// ->to shell ??	//bl
	dReal						m_w_scale;					// ->to shell ??	//bl
	CPHElement					*m_parent_element;			//bool !			//bl
	CPHShell					*m_shell;					//e					//bl
	CPHInterpolation			m_body_interpolation;		//e					//bl
	CPHFracturesHolder			*m_fratures_holder;			//e					//bl

	dReal						m_w_limit ;					//->to shell ??		//bl
	dReal						m_l_limit ;					//->to shell ??		//bl
//	dVector3					m_safe_position;			//e					//st
//	dQuaternion					m_safe_quaternion;
//	dVector3					m_safe_velocity;			//e					//st
//	Fmatrix						m_inverse_local_transform;	//e				//bt
	dReal						k_w;						//->to shell ??		//st
	dReal						k_l;						//->to shell ??		//st
	//ObjectContactCallbackFun*	temp_for_push_out;			//->to shell ??		//aux
	//u32							push_untill;				//->to shell ??		//st
	Flags8						m_flags;					//
	enum				
	{
		flActive				=	1<<0,
		flActivating			=	1<<1,
		flUpdate				=	1<<2,
		flWasEnabledBeforeFreeze=	1<<3,
		flEnabledOnStep			=	1<<4,
		flFixed					=	1<<5,
		flAnimated				=	1<<6
	};
//	bool						was_enabled_before_freeze;
//	bool						bUpdate;					//->to shell ??		//st
//	bool						b_enabled_onstep;
private:
////////////////////////////////////////////Interpolation/////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	void					FillInterpolation				()																				//interpolation called anywhere visual influent
	{
		m_body_interpolation.ResetPositions();
		m_body_interpolation.ResetRotations();
		//bUpdate=true;
		m_flags.set(flUpdate,true);
	}
IC	void					UpdateInterpolation				()																				//interpolation called from ph update visual influent
	{
		///VERIFY(dBodyStateValide(m_body));
		m_body_interpolation.UpdatePositions();
		m_body_interpolation.UpdateRotations();
		//bUpdate=true;
		m_flags.set(flUpdate,true);
	}
public:
////////////////////////////////////////////////Geometry/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	virtual	void						add_Sphere								(const Fsphere&		V) override;															//aux
	virtual	void						add_Box									(const Fobb&		V) override;															//aux
	virtual	void						add_Cylinder							(const Fcylinder&	V) override;															//aux
	virtual	void						add_Shape								(const SBoneShape& shape) override;														//aux
	virtual	void						add_Shape								(const SBoneShape& shape,const Fmatrix& offset) override;								//aux
	virtual	CODEGeom*					last_geom								() override {return CPHGeometryOwner::last_geom();}
	virtual	CODEGeom*					geometry								( u16 i ) override { return CPHGeometryOwner::Geom( i ); }
	virtual	const IPhysicsGeometry*		geometry								( u16 i )const override { return CPHGeometryOwner::Geom( i ); };
	virtual	void						add_geom								( CODEGeom* g ) override;
	virtual	void						remove_geom								( CODEGeom* g ) override;
	virtual	bool						has_geoms								() override {return CPHGeometryOwner::has_geoms();}
	virtual	void						set_ContactCallback						(ContactCallbackFun* callback) override;													//aux (may not be)
	virtual	void						set_ObjectContactCallback				(ObjectContactCallbackFun* callback) override;											//called anywhere ph state influent
	virtual	void						add_ObjectContactCallback				(ObjectContactCallbackFun* callback) override;											//called anywhere ph state influent
	virtual	void						remove_ObjectContactCallback			(ObjectContactCallbackFun* callback) override;
	virtual	void						set_CallbackData						(void * cd) override;
	virtual	void						*get_CallbackData						() override;
	virtual	ObjectContactCallbackFun	*get_ObjectContactCallback				() override;
	virtual	void						set_PhysicsRefObject					(IPhysicsShellHolder* ref_object) override;												//aux
	virtual	IPhysicsShellHolder*		PhysicsRefObject						() override {return m_phys_ref_object;}													//aux
	virtual	void						SetMaterial								(u16 m) override;																		//aux
	virtual	void						SetMaterial								(const char* m) override {CPHGeometryOwner::SetMaterial(m);}									//aux
	virtual	u16							numberOfGeoms							()const override;																				//aux
	virtual	const Fvector&				local_mass_Center						() override {return CPHGeometryOwner::local_mass_Center();}							//aux
	virtual	float						getVolume								() override {return CPHGeometryOwner::get_volume();}								//aux
	virtual	void						get_Extensions							(const Fvector& axis,float center_prg,float& lo_ext, float& hi_ext) const override;			//aux
	virtual	void						get_MaxAreaDir							(Fvector& dir) override {CPHGeometryOwner::get_MaxAreaDir(dir);}
	virtual	float						getRadius								() override;
	virtual	void						GetPointVel								( Fvector	 &res_vel, const Fvector & point ) const override;
////////////////////////////////////////////////////Mass/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
private:
	void								calculate_it_data						(const Fvector& mc,float mass);													//aux
	void								calculate_it_data_use_density			(const Fvector& mc,float density);												//aux
	void								calc_it_fract_data_use_density  		(const Fvector& mc,float density);//sets element mass and fractures parts mass	//aux
	dMass								recursive_mass_summ						(u16 start_geom,FRACTURE_I cur_fracture);										//aux
public:																																				//
	virtual	const Fvector&				mass_Center								()const override;														//aux
	virtual	void						setDensity								(float M) override;																		//aux
	virtual	float						getDensity								() override {return m_mass.mass/m_volume;}												//aux
	virtual	void						setMassMC								(float M,const Fvector& mass_center) override;											//aux
	virtual	void						setDensityMC							(float M,const Fvector& mass_center) override;											//aux
	virtual	void						set_local_mass_center					(const Fvector &mc ) override;
	virtual	void						setInertia								(const dMass& M);																//aux
	virtual	void						addInertia								(const dMass& M);
	virtual	void						add_Mass								(const SBoneShape& shape,const Fmatrix& offset,const Fvector& mass_center,float mass,CPHFracture* fracture=NULL) override;//aux
	virtual	void						set_BoxMass								(const Fobb& box, float mass) override;													//aux
	virtual	void						setMass									(float M) override;																		//aux
	virtual	float						getMass									() override {return m_mass.mass;}															//aux
	virtual	dMass*						getMassTensor							();	//aux
			void						ReAdjustMassPositions					(const Fmatrix &shift_pivot,float density);										//aux
			void						ResetMass								(float density);																//aux
			void						CutVelocity								(float l_limit,float a_limit);
///////////////////////////////////////////////////PushOut///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
private:																																			//
		
public:																																				//
//////////////////////////////////////////////Disable/////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	virtual	void						Disable									() override;																			//
	virtual	void						ReEnable								() override;																			//
	virtual	void						Enable									() override;																			//aux
	virtual	bool						isEnabled								() const override {return isActive()&&dBodyIsEnabled(m_body);}
	virtual	bool						isFullActive							() const override {return isActive()&&!m_flags.test(flActivating);}
	virtual	bool						isActive								() const override {return !!m_flags.test(flActive);}
	virtual void						Freeze									()	;																			//
	virtual void						UnFreeze								()	;																			//
	virtual bool						EnabledStateOnStep						()  {return dBodyIsEnabled(m_body)||m_flags.test(flEnabledOnStep);}							//
////////////////////////////////////////////////Updates///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
			bool						AnimToVel								( float dt, float l_limit,float a_limit );
			//void						BoneGlPos								(Fmatrix &m, const CBoneInstance* B)const;
			void						BoneGlPos								(Fmatrix &m, const Fmatrix &BoneTransform)const;
			void						ToBonePos								(const CBoneInstance* B, motion_history_state history_state );
			void						ToBonePos								(const Fmatrix &BoneTransform, motion_history_state history_state );
	IC		void						ActivatingPos							(const Fmatrix &BoneTransform);
	IC		void						CalculateBoneTransform					( Fmatrix &bone_transform )const;
			
#ifdef		DEBUG
	virtual	void						dbg_draw_velocity						( float scale, u32 color ) override;
	virtual	void						dbg_draw_force							( float scale, u32 color ) override;
	virtual	void						dbg_draw_geometry						( float scale, u32 color, Flags32 flags = Flags32().assign( 0 ) ) const override;
#endif
			void						SetBoneCallbackOverwrite				(bool v);
			void		_BCL			BonesCallBack							(CBoneInstance* B);																//called from updateCL visual influent
			void						StataticRootBonesCallBack				(CBoneInstance* B);
			void						PhDataUpdate							(dReal step);																	//ph update
			void						PhTune									(dReal step);																	//ph update
	virtual void						Update									();																				//called update CL visual influence
//////////////////////////////////////////////////Dynamics////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	virtual	void						SetAirResistance						(dReal linear=default_k_l, dReal angular=default_k_w) override
	//aux (may not be)
	{																																				//
		k_w= angular;																																//
		k_l=linear;																																	//
	}																																				//
	virtual	void						GetAirResistance				(float &linear, float &angular) override
	//
	{																																				//
		linear= k_l;																																//
		angular=k_w;																																//
	}	
	virtual void						applyImpact						(const SPHImpact& impact);																																	//
	virtual	void						applyImpulseTrace				(const Fvector& pos, const Fvector& dir, float val,const u16 id) override;					//called anywhere ph state influent
	virtual	void						set_DisableParams				(const SAllDDOParams& params) override;					//
	virtual	void						set_DynamicLimits				(float l_limit=default_l_limit,float w_limit=default_w_limit) override;							//aux (may not be)
	virtual	void						set_DynamicScales				(float l_scale=default_l_scale,float w_scale=default_w_scale) override;							//aux (may not be)
	virtual	void						Fix								() override;
	virtual	void						SetAnimated						( bool v ) override;
	virtual	void						ReleaseFixed					() override;
	virtual	bool						isFixed							() override {return !!(m_flags.test(flFixed));}
	virtual	void						applyForce						(const Fvector& dir, float val) override;															//aux
	virtual	void						applyForce						(float x,float y,float z) override;																//called anywhere ph state influent
	virtual	void						applyImpulse					(const Fvector& dir, float val) override;//aux
	virtual	void						applyImpulseVsMC				(const Fvector& pos,const Fvector& dir, float val) override;										//
	virtual	void						applyImpulseVsGF				(const Fvector& pos,const Fvector& dir, float val) override;										//
	virtual	void		_BCL			applyGravityAccel				(const Fvector& accel) override;
	virtual void						getForce						(Fvector& force);
	virtual void						getTorque						(Fvector& torque);
	virtual	void						get_LinearVel					(Fvector& velocity) const override;															//aux
	virtual	void						get_AngularVel					(Fvector& velocity)	const override;															//aux
	virtual	void						set_LinearVel					(const Fvector& velocity) override;														//called anywhere ph state influent
	virtual	void						set_AngularVel					(const Fvector& velocity) override;														//called anywhere ph state influent
	virtual	void						setForce						(const Fvector& force) override;															//
	virtual	void						setTorque						(const Fvector& torque) override;														//
	virtual	void						set_ApplyByGravity				(bool flag) override;															//
	virtual	bool						get_ApplyByGravity				() override;															//
///////////////////////////////////////////////////Net////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	virtual	void						get_State						(		SPHNetState&	state) override;													//
	virtual	void						set_State						(const	SPHNetState&	state) override;													//
	virtual	void						net_Import						(NET_Packet& P) override;
	virtual	void						net_Export						(NET_Packet& P) override;
///////////////////////////////////////////////////Position///////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	virtual	void						SetTransform					(const Fmatrix& m0, motion_history_state history_state  ) override;															//
	virtual	void						TransformPosition				(const Fmatrix &form, motion_history_state history_state ) override;
	virtual void						getQuaternion					(Fquaternion& quaternion);														//
	virtual	void						setQuaternion					(const Fquaternion& quaternion) override;												//
	virtual void						SetGlobalPositionDynamic		(const Fvector& position);														//
	virtual	void						GetGlobalPositionDynamic		(Fvector* v) override;																	//
	virtual	void						cv2obj_Xfrom					(const Fquaternion& q,const Fvector& pos, Fmatrix& xform) override;						//
	virtual	void						cv2bone_Xfrom					(const Fquaternion& q,const Fvector& pos, Fmatrix& xform) override;						//
	virtual	void		_BCL			InterpolateGlobalTransform		(Fmatrix* m) override;																	//called UpdateCL vis influent
	virtual	void						InterpolateGlobalPosition		(Fvector* v) override;																	//aux
	virtual	void						GetGlobalTransformDynamic		(Fmatrix* m) const override;																	//aux
IC			void						InverceLocalForm				(Fmatrix&)	;
IC			void						MulB43InverceLocalForm			(Fmatrix&) const;

////////////////////////////////////////////////////Structure/////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	virtual	CPhysicsShell*				PhysicsShell					() override;																				//aux
			CPHShell*					PHShell							();
	virtual	void						set_ParentElement				(CPhysicsElement* p) override { m_parent_element=(CPHElement*)p; }							//aux
#ifdef	DEBUG
	virtual	CPhysicsElement*					parent_element					() override { return m_parent_element; }
#endif
	void								SetShell						(CPHShell* p);																	//aux
	virtual	dBodyID						get_body				() override {return m_body;}																//aux
	virtual	const dBodyID				get_bodyConst			()const	{return m_body;}																//aux
//////////////////////////////////////////////////////Breakable//////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	IC CPHFracturesHolder*				FracturesHolder							(){return m_fratures_holder;}											//aux
	IC const CPHFracturesHolder*		constFracturesHolder				()const{return m_fratures_holder;}										//aux
	void								DeleteFracturesHolder					();																		//
	virtual	bool						isBreakable								() override;																		//aux
	virtual	u16							setGeomFracturable						(CPHFracture& fracture) override;												//aux
	virtual	CPHFracture&				Fracture								(u16 num) override;																//aux
			void						SplitProcess							(ELEMENT_PAIR_VECTOR &new_elements);									//aux
			void						PassEndGeoms							(u16 from,u16 to,CPHElement* dest);										//aux
////////////////////////////////////////////////////Build/Activate////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	virtual	void						Activate				(const Fmatrix& m0, float dt01, const Fmatrix& m2,bool disable=false) override;					//some isues not to be aux
	virtual	void						Activate				(const Fmatrix &transform,const Fvector& lin_vel,const Fvector& ang_vel,bool disable=false) override;//some isues not to be aux
	virtual	void						Activate				(bool disable=false, bool not_set_bone_callbacks = false) override;									//some isues not to be aux
	virtual	void						Activate				(const Fmatrix& start_from, bool disable=false) override;										//some isues not to be aux
	virtual	void						Deactivate				() override;																						//aux																																			//aux
			void						SetBoneCallback			();
			void						ClearBoneCallback		();
			void						CreateSimulBase			();//create body & cpace																//aux
			void						ReInitDynamics			(const Fmatrix &shift_pivot,float density);												//set body & geom positions					
			void						PresetActive			();																						//
			void						build									();																		//aux
			void						build									(bool disable);															//aux
			void						destroy									();																		//called anywhere ph state influent
			void						Start									();																		//aux
			void						RunSimulation							();																		//called anywhere ph state influent
			void						RunSimulation							(const Fmatrix& start_from);											//
			void						ClearDestroyInfo						();
			void						GetAnimBonePos							(Fmatrix &bp);
	//		bool						CheckBreakConsistent					()
	CPHElement										();																						//aux
	virtual	~CPHElement								() override;																						//aux
private:
	virtual	iphysics_scripted			&get_scripted							() override { return *this ;}
public:
};






IC CPHElement* cast_PHElement(CPhysicsElement* e){return static_cast<CPHElement*>(static_cast<CPhysicsElement*>(e));}
IC CPHElement* cast_PHElement(void* e){return static_cast<CPHElement*>(static_cast<CPhysicsElement*>(e));}
IC CPhysicsElement* cast_PhysicsElement(CPHElement* e){return static_cast<CPhysicsElement*>(static_cast<CPHElement*>(e));}
#endif