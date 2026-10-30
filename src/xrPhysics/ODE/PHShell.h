///////////////////////////////////////////////////////////////////////

#ifndef PH_SHELL
#define PH_SHELL

class CPHShell;
class CPHShellSplitterHolder;
#include "PHJoint.h"
#include "PHElement.h"
#include "PHDefs.h"
#include "PHShellSplitter.h"
#include "PHMoveStorage.h"
#include "physics_scripted.h"


class CPhysicsShellAnimator;


class CPHShell: 
	public CPhysicsShell,
	public CPHObject,
	public cphysics_scripted
{

	friend class CPHShellSplitterHolder;
	enum				
	{
		flActive									=	1<<0,
		flActivating								=	1<<1,
		flRemoveCharacterCollisionAfterDisable		=	1<<2,
	};
	s16						m_active_count;
	Flags8					m_flags;					
	bool					SelfCollision;

	xr_vector<u32>			SelfCollisionPairs;
	u16						SelfCollisionElements;
	bool					SelfCollisionPairsBuilt;
	u32						m_last_self_contacts;

	ELEMENT_STORAGE			elements;
	JOINT_STORAGE			joints;
	CPHShellSplitterHolder	*m_spliter_holder;
	CPHMoveStorage			m_traced_geoms;


	CPhysicsShellAnimator*	m_pPhysicsShellAnimatorC;

private:

#ifdef	DEBUG
	virtual	IPhysicsShellHolder	*ref_object					() override { return PhysicsRefObject() ;}
#endif

protected:
	dSpaceID			    m_space;
public:
	Fmatrix					m_object_in_root;
	CPHShell								();
	virtual	~CPHShell						() override;
	virtual	void			applyImpulseTrace		(const Fvector& pos, const Fvector& dir, float val,const u16 id) override;
	virtual	void			applyHit				(const Fvector& pos, const Fvector& dir, float val,const u16 id,ALife::EHitType hit_type) override;

	static void 	_BCL	BonesCallback				(CBoneInstance* B);
	static void 	_BCL	StataticRootBonesCallBack	(CBoneInstance* B);
	virtual	BoneCallbackFun* GetBonesCallback		() override {return BonesCallback ;}
	virtual	BoneCallbackFun* GetStaticObjectBonesCallback() override
	{ VERIFY( false ); return StataticRootBonesCallBack; }

	virtual	void			add_Element				(CPhysicsElement* E) override;
	virtual	void			ToAnimBonesPositions	( motion_history_state history_state ) override;
	virtual	bool			AnimToVelocityState		(float dt, float l_limit, float a_limit ) override;
	virtual	void			SetBonesCallbacksOverwrite(bool v) override;
	void					SetPhObjectInElements	();
	virtual	void			EnableObject			(CPHObject* obj) override;
	virtual	void			DisableObject			();

	virtual	void			SetAirResistance		(dReal linear=default_k_l, dReal angular=default_k_w) override
	{
		xr_vector<CPHElement*>::iterator i;
		for(i=elements.begin();elements.end()!=i;++i)
			(*i)->SetAirResistance(linear,angular);
	}

	virtual	void			GetAirResistance		(float& linear, float& angular) override
	{
		(*elements.begin())->GetAirResistance(linear,angular);
	}

	virtual	void			add_Joint				(CPhysicsJoint* J) override;

	virtual	CPHIsland*		PIsland					() override {return &Island();};
	virtual	void			applyImpulseTrace		(const Fvector& pos, const Fvector& dir, float val) override;

	virtual	void			Update					() override;

	virtual	void			Activate				(const Fmatrix& m0, float dt01, const Fmatrix& m2,bool disable=false) override;
	virtual	void			Activate				(const Fmatrix &transform,const Fvector& lin_vel,const Fvector& ang_vel,bool disable=false) override;
	virtual	void			Activate				(bool disable=false, bool not_set_bone_callbacks = false) override;
	virtual	void			Activate				(const Fmatrix& start_from, bool disable=false) override {};


	virtual	CPhysicsShellAnimator*	PPhysicsShellAnimator() override {return	m_pPhysicsShellAnimatorC;};

private:
			void			activate				(bool disable);	
public:
	virtual	void			Build					(bool disable=false) override;
	virtual	void			RunSimulation			(bool place_current_forms=true) override;
	virtual	void			net_Import				(NET_Packet& P) override;
	virtual	void			net_Export				(NET_Packet& P) override;
			void			PresetActive			();
			void			AfterSetActive			();
			void			PureActivate			();
	virtual	void			Deactivate				() override;
	virtual	const	CGID&	GetCLGroup				()const override;
	virtual	void	RegisterToCLGroup		(CGID g) override;
	virtual	bool	IsGroupObject			() override;
	virtual	void	SetIgnoreStatic			() override;
	virtual	void	SetIgnoreDynamic		() override;
	virtual	void	SetRagDoll				() override;
	virtual	void	SetIgnoreRagDoll		() override;
	virtual	void	SetSelfCollision		(bool Enabled) override { SelfCollision = Enabled; SelfCollisionPairsBuilt = false; }
	virtual	bool	IsSelfCollision			() const override { return SelfCollision; }
	virtual	u32		LastSelfCollisionContacts	() const override { return m_last_self_contacts; }
			void	BuildSelfCollisionPairs	()	;

	virtual	void	CreateShellAnimator		( CInifile const * ini, const char* section ) override;
	virtual	void	SetIgnoreAnimated		() override;

	virtual	void	SetSmall				() override;
	virtual	void	SetIgnoreSmall			() override;

	virtual	const CLBits&					collide_bits								()const override { return CPHObject::collide_bits();};
	virtual	const _flags<CLClassBits>&		collide_class_bits 							()const override {return CPHObject::collide_class_bits(); }

	virtual	void			setMass					(float M) override;

	virtual	void			setMass1				(float M) override;
	virtual	void			setEquelInertiaForEls	(const dMass& M);
	virtual	void			addEquelInertiaToEls	(const dMass& M);
	virtual	void			MassAddBox				( float mass, const Fvector &full_size ) override;
	virtual	float			getMass					() override;
	virtual	void			setDensity				(float M) override;
	virtual	float			getDensity				() override;
	virtual	float			getVolume				() override;
	virtual	void			get_Extensions			(const Fvector& axis,float center_prg,float& lo_ext, float& hi_ext) const override;
	virtual	void			applyForce				(const Fvector& dir, float val) override;
	virtual	void			applyForce				(float x,float y,float z) override;
	virtual	void			applyImpulse			(const Fvector& dir, float val) override;
	virtual	void	_BCL	applyGravityAccel		(const Fvector& accel) override;
	virtual	void			setTorque				(const Fvector& torque) override;
	virtual	void			setForce				(const Fvector& force) override;

	virtual	void			set_JointResistance		(float force) override
	{
		JOINT_I i;
		for(i=joints.begin();joints.end() != i;++i)
		{
			(*i)->SetForce(force);
			(*i)->SetVelocity();
		}
		//(*i)->SetForceAndVelocity(force);
	}

	virtual	void				set_DynamicLimits				(float l_limit=default_l_limit,float w_limit=default_w_limit) override;
	virtual	void				set_DynamicScales				(float l_scale=default_l_scale,float w_scale=default_w_scale) override;
	virtual	void				set_ContactCallback				(ContactCallbackFun* callback) override;
	virtual	void				set_ObjectContactCallback		(ObjectContactCallbackFun* callback) override;
	virtual	void				SetAnimated						( bool v ) override;
	virtual	void				add_ObjectContactCallback		(ObjectContactCallbackFun* callback) override;
	virtual	void				remove_ObjectContactCallback		(ObjectContactCallbackFun* callback) override;
	virtual	void				set_CallbackData				(void * cd) override;
	virtual	void				*get_CallbackData				() override;
	virtual	void				set_PhysicsRefObject			(IPhysicsShellHolder* ref_object) override;
				IPhysicsShellHolder*PhysicsRefObject				(){ return (*elements.begin())->PhysicsRefObject(); }
	
	//breakbable interface
	virtual	bool				isBreakable						() override;
	virtual	bool				isFractured						() override;
	virtual	CPHShellSplitterHolder*	SplitterHolder				() override {return m_spliter_holder;}
	virtual	void				SplitterHolderActivate			() override;
	virtual	void				SplitterHolderDeactivate		() override;
	virtual	void				SplitProcess					(PHSHELL_PAIR_VECTOR &out_shels) override;
	virtual	void				BlockBreaking					() override {if(m_spliter_holder)m_spliter_holder->SetUnbreakable();}
	virtual	void				UnblockBreaking					() override {if(m_spliter_holder)m_spliter_holder->SetBreakable();}
	virtual	bool				IsBreakingBlocked				() override {return m_spliter_holder&&m_spliter_holder->IsUnbreakable();}
	///////	////////////////////////////////////////////////////////////////////////////////////////////
	virtual	void				get_LinearVel					(Fvector& velocity) const override;
	virtual	void				get_AngularVel					(Fvector& velocity) const override;
	virtual	void				set_LinearVel					(const Fvector& velocity) override;
	virtual	void				set_AngularVel					(const Fvector& velocity) override;
	virtual	void				TransformPosition				(const Fmatrix &form, motion_history_state history_state  ) override;
	virtual	void				SetGlTransformDynamic			(const Fmatrix &form) override;
	virtual	void				set_ApplyByGravity				(bool flag) override;
	virtual	bool				get_ApplyByGravity				() override;
	virtual	void				SetMaterial						(u16 m) override;
	virtual	void				SetMaterial						(const char* m) override;
	virtual	ELEMENT_STORAGE		&Elements						() override {return elements;}
	virtual	CPhysicsElement		*get_Element					(u16 bone_id) override;
	virtual	CPhysicsElement		*get_Element					(const shared_str & bone_name) override;
	virtual	CPhysicsElement		*get_Element					(const char* bone_name) override;
	virtual	const CPhysicsElement	*get_ElementByStoreOrder		(u16 num) const override;
	virtual	CPhysicsElement		*get_ElementByStoreOrder		(u16 num) override;
	virtual CPhysicsElement		*get_PhysicsParrentElement		( u16 bone_id ) override;
	virtual	u16					get_ElementsNumber				()const override {return (u16)elements.size();}
	virtual	CPHSynchronize		*get_ElementSync				(u16 element) override;
	virtual	u16					get_elements_number				() override {return get_ElementsNumber();}
	virtual	CPHSynchronize		*get_element_sync				(u16 element) override {return get_ElementSync(element);}
	virtual	CPhysicsElement		*NearestToPoint					(const Fvector& point, NearestToPointCallback *cb = 0 ) override;
	virtual	CPhysicsJoint		*get_Joint						(u16 bone_id) override;
	virtual	CPhysicsJoint		*get_Joint						(const shared_str & bone_name) override;
	virtual	CPhysicsJoint		*get_Joint						(const char* bone_name) override;
	virtual	CPhysicsJoint		*get_JointByStoreOrder			(u16 num) override;
	virtual	u16					get_JointsNumber				() override;
	virtual	CODEGeom			*get_GeomByID					(u16 bone_id) override;

	virtual	void				Enable							() override;
	virtual	void				Disable							() override;
	virtual	void				DisableCollision				() override;
	virtual	void				EnableCollision					() override;
	virtual	bool				IsCollisionDisabled				()const override {return CPHObject::IsCollisionDisabled();}
	virtual	void				DisableCharacterCollision		() override;
	virtual	void				SetRemoveCharacterCollLADisable	() override {m_flags.set(flRemoveCharacterCollisionAfterDisable,true);}
	virtual	bool				isEnabled						()const override {return CPHObject::is_active();}
	virtual	bool				isActive						()const override {return !!m_flags.test(flActive);}
	virtual	bool				isFullActive					()const override {return isActive()&&!m_flags.test(flActivating);}	
				void				SetNotActivating				(){m_flags.set(flActivating,false);}
	IC			void				SetObjVsShellTransform			(const Fmatrix & root_transform);
//CPHObject	 
	virtual	void				vis_update_activate				() override;
	virtual	void				vis_update_deactivate	  		() override;
	virtual	void				PureStep						(float step) override;
	virtual	void				CollideAll						() override;
	virtual	void				CollideSelf						() override;
	virtual	void				PhDataUpdate					(dReal step) override;
	virtual	void				PhTune							(dReal step) override;
	virtual	void				InitContact						(dContact* c,bool &do_collide,u16 /*material_idx_1*/,u16 /*material_idx_2*/) override {};
	virtual	void				FreezeContent					() override;
	virtual	void				UnFreezeContent					() override;
	virtual	void				Freeze							() override;
	virtual	void				UnFreeze						() override;
	virtual	void				NetInterpolationModeON			() override {CPHObject::NetInterpolationON();}
	virtual	void				NetInterpolationModeOFF			() override {CPHObject::NetInterpolationOFF();}
	virtual		void				StepFrameUpdate					(dReal step){};
	virtual	CPHMoveStorage*		MoveStorage						() override {return &m_traced_geoms;}
	virtual	void				build_FromKinematics			(IKinematics* K,BONE_P_MAP* p_geting_map=NULL) override;
	virtual	void				preBuild_FromKinematics			(IKinematics* K,BONE_P_MAP* p_geting_map) override;
	virtual	void	_BCL		ActivatingBonePoses				(IKinematics &K) override;
	virtual	void                ZeroCallbacks					() override;
	virtual	void				ResetCallbacks(u16 id, VisMask& mask) override;
				void				PlaceBindToElForms				();
	virtual	void				SetCallbacks					( ) override;
	virtual	void				EnabledCallbacks				(bool val) override;
	virtual	void				set_DisableParams				(const SAllDDOParams& params) override;
	virtual	void				UpdateRoot						() override;
	virtual	void				SmoothElementsInertia			(float k) override;
	virtual	void	_BCL		InterpolateGlobalTransform		(Fmatrix* m) override;
	virtual	void				InterpolateGlobalPosition		(Fvector* v) override;
	virtual	void				AnimatorOnFrame					(bool calculate_bones = true) override;
	virtual	void				GetGlobalTransformDynamic		(Fmatrix* m) override;
	virtual	void				GetGlobalPositionDynamic		(Fvector* v) override;
	virtual	Fmatrix&			ObjectInRoot					() override {return m_object_in_root;}
	virtual	void				ObjectToRootForm				(const Fmatrix& form) override;
	virtual		dSpaceID			dSpace							(){return m_space;}
	virtual	void				SetTransform					(const Fmatrix& m0, motion_history_state history_state  ) override;

	virtual	void				AddTracedGeom					(u16 element=0,u16 geom=0) override;
	virtual	void				SetAllGeomTraced				() override;

	virtual	void				ClearTracedGeoms				() override;
	virtual	void				DisableGeomTrace				() override;
	virtual	void				EnableGeomTrace					() override;
	virtual	bool				HasTracedGeoms					() override { return !m_traced_geoms.empty(); }

	virtual	void				SetPrefereExactIntegration		() override;
	virtual	void				UnSetPrefereExactIntegration	() override;
	virtual	bool				IsPrefereExactIntegration		() const override { return CPHObject::IsPrefereExactIntegration(); }
	virtual	void				CutVelocity						(float l_limit,float a_limit) override;
///////////	//////////////////////////////////////////////////////////////////////////////////////////
				void				CreateSpace						()																				;
				void				PassEndElements					(u16 from,u16 to,CPHShell *dest)												;
				void				PassEndJoints					(u16 from,u16 to,CPHShell *dest)												;
				void				DeleteElement					(u16 element)																	;
				void				DeleteJoint						(u16 joint)																		;
				u16					BoneIdToRootGeom				(u16 id)																		;
/////////////////////////////////////////////////////////////////////////////////////////////////////
protected:
	virtual	void				get_spatial_params				() override;
	virtual	dGeomID				dSpacedGeom						() override {return (dGeomID)m_space;}

	virtual	void				ClearRecentlyDeactivated		() override;
				void				ClearCashedTries				()																				;
				bool				CanSelfCollide					(const CPHElement& Element1,const CPHElement& Element2) const;
				bool				IsSelfCollisionPairExcluded		(u16 Element1,u16 Element2) const;
				void				InvalidateSelfCollisionPairs	() { SelfCollisionPairsBuilt = false; }
private:
				//breakable
				void				setEndElementSplitter	  		()																				;
				void				setElementSplitter		  		(u16 element_number,u16 splitter_position)										;
				void				setEndJointSplitter	  			()																				;
				void				AddSplitter			  			(CPHShellSplitter::EType type,u16 element,u16 joint)							;
				void				AddSplitter			  			(CPHShellSplitter::EType type,u16 element,u16 joint,u16 position)				;
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
				void				AddElementRecursive				(CPhysicsElement* root_e, u16 id,Fmatrix global_parent,u16 element_number,bool *vis_check);
				void				PlaceBindToElFormsRecursive		(Fmatrix parent, u16 id, u16 element, VisMask& mask);
				void				BonesBindCalculate				(u16 id_from=0);
				void				BonesBindCalculateRecursive		(Fmatrix parent,u16 id);
				void				ZeroCallbacksRecursive			(u16 id)																		;
				void				SetCallbacksRecursive			(u16 id,u16 element)															;
				void				ResetCallbacksRecursive			(u16 id, u16 element, VisMask& mask);
				void				SetJointRootGeom				(CPhysicsElement* root_e,CPhysicsJoint* J)										;
				void				ReanableObject					()																				;
				void				ExplosionHit					(const Fvector& pos, const Fvector& dir, float val,const u16 id)				;
				void				ClearBreakInfo					();
				Fmatrix&			get_animation_root_matrix		( Fmatrix& m );
				void				update_root_transforms			();
IC				CPHElement			&root_element					() { VERIFY( !elements.empty() ); return *(*elements.begin()); }
	private:
	virtual	iphysics_scripted	&get_scripted					() override { return *this ;}
	public:
#ifdef		DEBUG
	virtual	void				dbg_draw_velocity				( float scale, u32 color ) override;
	virtual	void				dbg_draw_force					( float scale, u32 color ) override;
	virtual	void				dbg_draw_geometry				( float scale, u32 color, Flags32 flags = Flags32().assign( 0 ) ) const override;
#endif
};


IC	void	CPHShell::SetObjVsShellTransform			(const Fmatrix & root_transform)
{
	m_object_in_root.set( root_transform );
	m_object_in_root.invert( );
	SetNotActivating( );
}


#endif