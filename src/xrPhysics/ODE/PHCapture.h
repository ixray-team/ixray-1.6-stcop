#pragma	 once
#ifndef PH_CAPTURE_H
#define PH_CAPTURE_H

class CPHContact;

//#include "phobject.h"
#include "PHUpdateObject.h"
//#include "gameobject.h"
//#include "physicsshellholder.h"
#include "PHIsland.h"
#include "../../3rd-party/ode/include/ode/common.h"
#include "IPHCapture.h"
class	IPhysicsShellHolder;
class	CPHCharacter;
class	CPhysicsElement;
class	CPHElement;
struct	NearestToPointCallback;
class CPHCapture : 
	public CPHUpdateObject,
	public IPHCapture
{
public:
					CPHCapture	(CPHCharacter     *a_character,IPhysicsShellHolder	  *a_taget_object, NearestToPointCallback* cb /*=0*/);
					CPHCapture	(CPHCharacter     *a_character,IPhysicsShellHolder	  *a_taget_object,u16 a_taget_elemrnt);
					CPHCapture	(CPHCharacter     *a_character,IPhysicsShellHolder	  *a_taget_object,u16 a_taget_elemrnt,const Fvector& global_position,bool free_chain);
virtual	~CPHCapture	() override;

virtual	bool				Failed		() override {return e_state == cstFree;}
bool				isCaptured	() override {return e_state == cstCaptured;}
bool				isPulling	() override {return e_state == cstPulling;}
Fvector&			capture_pos	() override {return m_global_pos;}

void				ApplyForce	(Fvector dir, float force) override;
void				SetRotation	(Fmatrix form) override;
void				SetAMotorVel	(Fvector dir) override;
void				EnableAMotor	(bool val) override;
void				SetFreeChain	(bool val) override;
void				SetTargetRotation(const Fmatrix& camera_rotation) override;
void				RotateTarget	(const Fmatrix& delta) override;
void				SetTargetAngularVelocity(const Fvector& velocity) override;

virtual	void				Release		() override;
virtual	void				RemoveConnection(IPhysicsShellHolder* O) override;

protected:
bool				m_free_chain;
Fvector				m_global_pos;
CPHCharacter		*m_character;
CPhysicsElement*	m_taget_element;
IPhysicsShellHolder*	m_taget_object;
dJointID			m_joint;
dJointID			m_ajoint;
dJointFeedback		m_joint_feedback;
Fvector				m_capture_pos;
float				m_back_force;
float				m_pull_force;
float				m_capture_force;
float				m_capture_distance;
float				m_pull_distance;
u32					m_capture_time;
u32					m_time_start;
CBoneInstance		*m_capture_bone;
dBodyID				m_body;
CPHIsland			m_island;
//bool				b_failed;
bool				b_collide;
bool				b_disabled;
bool				b_character_feedback;
Fmatrix				m_camera_rotation;
Fmatrix				m_rel_rotation;
Fvector				m_camera_angular_velocity;
bool				m_rel_rotation_valid;
bool				m_camera_velocity_valid;
bool				m_gravity_disabled;
int					m_saved_gravity_mode;
bool				m_shell_collision_taken;
bool				m_shell_collision_was_disabled;
bool				m_self_collision_taken;
bool				m_self_collision_was;
bool				m_exact_integration_taken;
bool				m_exact_integration_was;

private:
	enum 
	{
	 cstPulling,
	 cstCaptured,
	 cstReleased,
	 cstFree,
	 cstFailed
	} e_state;

			void PullingUpdate();
			void CapturedUpdate(dReal step);
			void ReleasedUpdate();
			void ReleaseInCallBack();
			void Init( );

			void Deactivate();
			void CreateBody();
			bool Invalid();
			void ApplyAnchorRotation(CPHElement* elem, dReal step);
			void DisableShellCollisions();
			void RestoreShellCollisions();
			void EnableShellSelfCollision();
			void RestoreShellSelfCollision();
			void EnableShellExactIntegration();
			void RestoreShellExactIntegration();
			void DisableGravity();
			void RestoreGravity();
static void object_contactCallbackFun(bool& do_colide,bool bo1,CPHContact& contact,SGameMtl* /*material_1*/,SGameMtl* /*material_2*/);

///////////CPHObject/////////////////////////////
	virtual	void PhDataUpdate(dReal step) override;
	virtual	void PhTune(dReal step) override;
	virtual	void NetRelcase		(CPhysicsShell *s) override;

};
#endif