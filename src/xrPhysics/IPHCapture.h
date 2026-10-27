#pragma once
class IPhysicsShellHolder;
class IPHCapture
{
public:
	virtual bool				Failed			()	=0;
	virtual bool				isCaptured		()	=0;
	virtual bool				isPulling		()	=0;
	virtual Fvector&			capture_pos		()	=0;

	virtual void				ApplyForce		(Fvector dir, float force) = 0;
	virtual void				SetRotation		(Fmatrix form) = 0;
	virtual void				SetAMotorVel	(Fvector dir) = 0;
	virtual void				EnableAMotor	(bool val) = 0;

	virtual void				SetFreeChain	(bool val) = 0;
	virtual void				SetTargetRotation(const Fmatrix& camera_rotation) = 0;
	virtual void				RotateTarget	(const Fmatrix& delta) = 0;
	virtual void				SetTargetAngularVelocity(const Fvector& velocity) = 0;

	virtual void				RemoveConnection( IPhysicsShellHolder* O ) = 0;
	virtual void				Release		()	=0;
protected:
	virtual						~IPHCapture() {}
};
class	CPHCharacter;
struct	NearestToPointCallback;
XRPHYSICS_API IPHCapture	*phcapture_create(CPHCharacter *ch, IPhysicsShellHolder* object, NearestToPointCallback* cb /*=0*/ );
XRPHYSICS_API IPHCapture	*phcapture_create(CPHCharacter *ch, IPhysicsShellHolder* object,u16 element);
XRPHYSICS_API IPHCapture	*phcapture_create(CPHCharacter *ch, IPhysicsShellHolder* object,u16 element, const Fvector& global_pos, bool free_chain);
XRPHYSICS_API void			phcapture_destroy(IPHCapture* &c);

XRPHYSICS_API extern float g_CameraCaptureMotorForce;
XRPHYSICS_API extern bool g_CameraCaptureNoCollision;
XRPHYSICS_API extern bool g_CameraCaptureSelfCollision;
XRPHYSICS_API extern bool g_CameraCaptureExactIntegration;

struct SCameraCaptureShellInfo
{
	int Elements;
	int SelfCollisionContacts;
	bool Animator;
};
XRPHYSICS_API extern SCameraCaptureShellInfo g_CameraCaptureShellInfo;
