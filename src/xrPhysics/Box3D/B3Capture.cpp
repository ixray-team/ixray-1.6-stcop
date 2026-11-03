#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"
#include "IPHCapture.h"
#include "IPHCharacter.h"
#include "IPhysicsShellHolder.h"
#include "PHContact.h"
#include "IActivationShape.h"
#include "ActorCameraCollision.h"
#include "MovementBoxDynamicActivate.h"
#include "PHUpdateObject.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../xrEngine/bone.h"
#include "../../xrEngine/CameraBase.h"
#include "../../xrEngine/device.h"

// Capture: the target element is pulled to the capture point with a force-limited velocity controller.
class CB3Capture :
	public IPHCapture,
	public CPHUpdateObject
{
	enum class EState
	{
		Free,
		Pulling,
		Captured,
		Released
	};

	IPHCharacter* Character = nullptr;
	IPhysicsShellHolder* Target = nullptr;
	CPhysicsElement* Element = nullptr;
	CBoneInstance* CaptureBone = nullptr;
	Fvector CapturePos = {0.f, 0.f, 0.f};
	EState State = EState::Free;
	float PullDistance = 20.f;
	float CaptureDistance = 0.4f;
	float CaptureForce = 20500.f;
	float PullForce = 8000.f;
	u32 TimeLimit = 3000;
	u32 TimeStart = 0;
	bool FreeChain = false;

	// Character drag, as CPHCapture: ODE holds the grabbed element with a ball joint to a body
	// that is teleported to the hand, and an angular motor keeps the grabbed orientation. Box3D
	// has no AMotor: a weld joint with a rigid linear part and a soft angular part is the same
	// constraint, so the object follows the hand instead of lagging behind a force spring.
	b3BodyId Helper = b3_nullBodyId;
	b3JointId Joint = b3_nullJointId;
	// CPHCapture::Init reads velocity_scale: the element's velocities are limited while dragging.
	float VelocityScale = 1.f;
	// CPHCapture::EnableAMotor / SetFreeChain: the camera motor drives the grabbed orientation.
	bool MotorEnabled = true;
	// Element orientation at the moment the joint was created: the joint frame on the element.
	b3Quat CaptureRotation0 = b3Quat_identity;
	// CPHSimpleCharacter drag: the grabbed orientation is held with a soft angular spring.
	static constexpr float CaptureAngularHertz = 6.f;
	static constexpr float CaptureAngularDamping = 1.f;
	// Camera grab: ODE drives the orientation with a very strong AMotor, so the angular hold of
	// the joint is as stiff as Box3D allows.
	static constexpr float CameraCaptureAngularHertz = 60.f;

	// Camera grab (no character): the element is glued to the capture point and follows
	// the camera orientation, exactly like the ODE capture. The grabbed orientation is
	// kept relative to the camera, so TAB rotation and the view turn stay in sync.
	Fmatrix CameraRotation = Fidentity;
	Fmatrix RelRotation = Fidentity;
	bool RelRotationValid = false;
	Fvector CameraAngularVelocity = {0.f, 0.f, 0.f};
	bool CameraVelocityValid = false;

	bool GravityTaken = false;
	bool GravityWas = true;
	bool ShellCollisionTaken = false;
	bool ShellCollisionWas = false;
	bool SelfCollisionTaken = false;
	bool SelfCollisionWas = false;
	bool ExactTaken = false;
	bool ExactWas = false;

	void UpdateCapturePos()
	{
		if (Character && CaptureBone && Character->PhysicsRefObject())
		{
			CapturePos.set(CaptureBone->mTransform.c);
			Character->PhysicsRefObject()->ObjectXFORM().transform_tiny(CapturePos);
		}
	}

	bool ReadCharacterParams()
	{
		IPhysicsShellHolder* Owner = Character->PhysicsRefObject();
		IKinematics* K = Owner ? Owner->ObjectKinematics() : nullptr;
		CInifile* Ini = K ? K->LL_UserData() : nullptr;
		if (Ini == nullptr || !Ini->section_exist("capture"))
		{
			return false;
		}

		const u16 BoneId = K->LL_BoneID(Ini->r_string("capture", "bone"));
		if (BoneId == BI_NONE)
		{
			return false;
		}

		CaptureBone = &K->LL_GetBoneInstance(BoneId);
		PullDistance = Ini->r_float("capture", "pull_distance");
		CaptureDistance = Ini->r_float("capture", "distance");
		CaptureForce = Ini->r_float("capture", "capture_force");
		TimeLimit = Ini->r_u32("capture", "time_limit") * 1000;
		PullForce = Ini->r_float("capture", "pull_force");
		VelocityScale = Ini->line_exist("capture", "velocity_scale") ? Ini->r_float("capture", "velocity_scale") : 1.f;
		return true;
	}

	void Init()
	{
		if (Element == nullptr || !Element->isActive())
		{
			return;
		}

		if (Character)
		{
			if (!ReadCharacterParams())
			{
				return;
			}
		}
		else
		{
			// CPHCapture::Init for the camera grab (no [capture] ini): the joint is created
			// as soon as the element is within 10 m, i.e. at the first step after the grab,
			// which is what keeps the body fixed to the camera at the aimed point.
			PullDistance = 20.f;
			CaptureDistance = 10.f;
			CaptureForce = 20500.f;
			PullForce = 8000.f;
			TimeLimit = 3000;
			MotorEnabled = !FreeChain;
		}
		UpdateCapturePos();

		Fvector Position;
		Element->GetGlobalPositionDynamic(&Position);
		if (Position.distance_to(CapturePos) > PullDistance)
		{
			return;
		}

		const float Mass = Element->PhysicsShell() ? Element->PhysicsShell()->getMass() : Element->getMass();
		PullForce = std::min(PullForce, 4.f * g_B3World->Gravity() * Mass);
		TimeStart = Device.dwTimeGlobal;

		if (Character)
		{
			// CPHCapture::Init: the dragged element's velocities are limited (velocity_scale),
			// and it must ignore the actor's own body while it is in his hands.
			Element->set_DynamicLimits(default_l_limit * VelocityScale, default_w_limit * VelocityScale);
			Character->SetObjectContactCallback(ObjectContactCallback);
		}
		else
		{
			// CPHCapture::Init for the camera grab: ODE limits the linear speed with 0.022.
			// The angular limit stays default, because the camera orientation is driven by the
			// angular velocity here and a quarter radian per second would freeze the rotation.
			static constexpr float CameraVelocityScale = 0.022f;
			Element->set_DynamicLimits(default_l_limit * CameraVelocityScale, default_w_limit);
		}

		if (Character && Character->PhysicsRefObject() && Character->PhysicsRefObject()->IsActor())
		{
			Character->PhysicsRefObject()->HideAllWeapons(true);
		}

		CPHUpdateObject::Activate();
		State = EState::Pulling;
	}

	// CPHCapture::object_contactCallbackFun: the grabbed object never collides with the object
	// that holds it, otherwise the actor pushes away what he is carrying.
	static void ObjectContactCallback(bool& DoCollide, bool First, CPHContact& Contact, SGameMtl*, SGameMtl*)
	{
		for (int Side = 0; Side < 2; ++Side)
		{
			IPhysicsShellHolder* Holder = Contact.GetObject(Side == 0);
			IPhysicsShellHolder* Other = Contact.GetObject(Side != 0);
			if (Holder == nullptr || Other == nullptr)
			{
				continue;
			}
			// The holder is the actor: its capture knows which element it drags.
			IPHCapture* Capture = Holder->PHCapture();
			if (Capture == nullptr)
			{
				continue;
			}
			if (static_cast<CB3Capture*>(Capture)->Target == Other)
			{
				DoCollide = false;
				return;
			}
		}
	}

	// CPHCapture::PullingUpdate: the ball joint and the motor are created at the capture point
	// once the element is close enough. The helper body is kinematic, so the joint drags the
	// element exactly where the hand is, as ODE does by teleporting a heavy body.
	bool CreateAnchor()
	{
		if (g_B3World == nullptr || Element == nullptr || g_B3World->IsStepping())
		{
			return false;
		}

		CPHElement* E = static_cast<CPHElement*>(Element);
		if (!b3Body_IsValid(E->Body))
		{
			return false;
		}

		DestroyAnchor();

		// A sleeping element would ignore the joint until something else wakes it.
		B3_SAFE(b3Body_SetAwake(E->Body, true));

		b3BodyDef Def = b3DefaultBodyDef();
		Def.type = b3_kinematicBody;
		Def.position = B3Math::ToB3(CapturePos);
		Def.isAwake = true;
		Helper = b3CreateBody(g_B3World->Id(), &Def);

		// Local frame on the element: the capture point, rotated so that its world rotation is
		// identity at creation, as the ODE motor takes the current relative pose as zero.
		const b3WorldTransform Body = b3Body_GetTransform(E->Body);
		const b3Vec3 LocalAnchor = b3InvRotateVector(Body.q, b3SubPos(B3Math::ToB3(CapturePos), Body.p));
		CaptureRotation0 = Body.q;

		b3WeldJointDef JointDef = b3DefaultWeldJointDef();
		JointDef.base.bodyIdA = Helper;
		JointDef.base.bodyIdB = E->Body;
		JointDef.base.localFrameA.p = b3Vec3_zero;
		JointDef.base.localFrameA.q = b3Quat_identity;
		JointDef.base.localFrameB.p = LocalAnchor;
		JointDef.base.localFrameB.q = b3Conjugate(Body.q);
		// Rigid position (the ODE ball joint) and an angular hold: soft for the actor drag, as
		// stiff as Box3D allows for the camera (the ODE AMotor with a huge motor force).
		JointDef.linearHertz = 0.f;
		JointDef.linearDampingRatio = 1.f;
		if (Character)
		{
			JointDef.angularHertz = CaptureAngularHertz;
			JointDef.angularDampingRatio = CaptureAngularDamping;
			Joint = b3CreateWeldJoint(g_B3World->Id(), &JointDef);
		}
		else if (MotorEnabled)
		{
			JointDef.angularHertz = CameraCaptureAngularHertz;
			JointDef.angularDampingRatio = 1.f;
			Joint = b3CreateWeldJoint(g_B3World->Id(), &JointDef);
		}
		else
		{
			// Motor blocked (free chain): the element must rotate freely, so only the ball part
			// of the capture is constrained (the ODE AMotor with zero force).
			b3SphericalJointDef SphereDef = b3DefaultSphericalJointDef();
			SphereDef.base.bodyIdA = Helper;
			SphereDef.base.bodyIdB = E->Body;
			SphereDef.base.localFrameA.p = b3Vec3_zero;
			SphereDef.base.localFrameA.q = b3Quat_identity;
			SphereDef.base.localFrameB.p = LocalAnchor;
			SphereDef.base.localFrameB.q = b3Quat_identity;
			SphereDef.enableSpring = false;
			SphereDef.enableConeLimit = false;
			Joint = b3CreateSphericalJoint(g_B3World->Id(), &SphereDef);
		}
		if (!b3Joint_IsValid(Joint))
		{
			DestroyAnchor();
			return false;
		}

		// The joint holds the element now, so the pull velocity limit is not needed any more and
		// would only cap the speed the body keeps when it is released.
		Element->set_DynamicLimits();

		// CPHCapture::PullingUpdate stops the element before the joint takes it. A rebuilt joint
		// (motor toggle) must not stop the body again.
		if (State != EState::Captured)
		{
			Element->set_LinearVel(Fvector().set(0.f, 0.f, 0.f));
			Element->set_AngularVel(Fvector().set(0.f, 0.f, 0.f));
		}
		return true;
	}

	void DestroyAnchor()
	{
		const b3JointId OldJoint = Joint;
		Joint = b3_nullJointId;
		if (b3Joint_IsValid(OldJoint))
		{
			B3_SAFE(b3DestroyJoint(OldJoint, true));
		}

		const b3BodyId OldHelper = Helper;
		Helper = b3_nullBodyId;
		if (b3Body_IsValid(OldHelper))
		{
			B3_SAFE(b3DestroyBody(OldHelper));
		}
	}

	// CPHCapture::CapturedUpdate: the helper body is moved to the hand before the step, so the
	// joint drags the element within the same step. Its rotation is the camera orientation offset
	// by the grabbed orientation, which is what the ODE motor held.
	void TuneAnchor(float Step)
	{
		if (!b3Body_IsValid(Helper))
		{
			return;
		}

		b3Quat Rotation = b3Quat_identity;
		if (Character == nullptr && MotorEnabled && RelRotationValid)
		{
			Fmatrix Desired;
			Desired.mul(CameraRotation, RelRotation);
			Desired.c.set(0.f, 0.f, 0.f);
			Rotation = b3MulQuat(B3Math::ToB3(Desired).q, b3Conjugate(CaptureRotation0));
		}

		B3_SAFE(b3Body_SetTargetTransform(Helper, {b3ToPos(B3Math::ToB3(CapturePos)), Rotation}, Step, true));
	}

	// CPHCapture::CapturedUpdate: a stuck element releases the capture, and the actor feels the
	// load of what he drags.
	void UpdateAnchor()
	{
		if (!b3Joint_IsValid(Joint))
		{
			return;
		}

		const b3Vec3 Force = b3Joint_GetConstraintForce(Joint); // the force applied to the element
		const float Magnitude = b3Length(Force);
		if (Magnitude > CaptureForce)
		{
			Release();
			return;
		}

		if (Character && Magnitude > CaptureForce / 2.2f)
		{
			// The reaction pulls the actor towards the object: capture_force / 15 is constant,
			// exactly the m_joint_feedback force ODE applies to the character.
			const Fvector Reaction = B3Math::FromB3(b3MulSV(-(CaptureForce / 15.f) / Magnitude, Force));
			Character->ApplyForce(Reaction);
		}
	}

	// CPHCapture::PullingUpdate / CapturedUpdate for the character grab.
	void TuneCharacter(float Step)
	{
		if (!Element->isActive())
		{
			Release();
			return;
		}

		Fvector Position;
		Element->GetGlobalPositionDynamic(&Position);
		const float Distance = Position.distance_to(CapturePos);

		if (State == EState::Pulling)
		{
			if (Distance > PullDistance || Device.dwTimeGlobal - TimeStart > TimeLimit)
			{
				Release();
				return;
			}

			if (Distance < CaptureDistance)
			{
				if (CreateAnchor())
				{
					State = EState::Captured;
				}
				return;
			}

			// A constant force towards the hand, as m_taget_element->applyForce(dir, m_pull_force).
			Fvector Dir;
			Dir.sub(CapturePos, Position);
			if (Dir.magnitude() < EPS_L)
			{
				return;
			}
			Dir.normalize();
			Element->applyForce(Dir, PullForce);
			return;
		}

		// Captured: CPHCapture does not recheck the distance (a big element is grabbed at its
		// edge, so its center is far from the hand); the joint holds it.
		if (!b3Joint_IsValid(Joint))
		{
			// The element's body is gone (shell deactivated): nothing holds it any more.
			Release();
			return;
		}

		TuneAnchor(Step);
	}

	CPhysicsShell* TargetShell() const
	{
		return Element ? Element->PhysicsShell() : nullptr;
	}

	void DisableGravity()
	{
		if (GravityTaken || !Element)
		{
			return;
		}

		GravityWas = Element->get_ApplyByGravity();
		Element->set_ApplyByGravity(false);
		GravityTaken = true;
	}

	void RestoreGravity()
	{
		if (!GravityTaken)
		{
			return;
		}

		GravityTaken = false;
		if (Element)
		{
			Element->set_ApplyByGravity(GravityWas);
		}
	}

	void DisableShellCollisions()
	{
		if (ShellCollisionTaken)
		{
			return;
		}

		CPhysicsShell* Shell = TargetShell();
		if (Shell == nullptr)
		{
			return;
		}

		ShellCollisionWas = Shell->IsCollisionDisabled();
		if (!ShellCollisionWas)
		{
			Shell->DisableCollision();
		}

		ShellCollisionTaken = true;
	}

	void RestoreShellCollisions()
	{
		if (!ShellCollisionTaken)
		{
			return;
		}

		ShellCollisionTaken = false;
		if (ShellCollisionWas)
		{
			return;
		}

		if (CPhysicsShell* Shell = TargetShell())
		{
			Shell->EnableCollision();
		}
	}

	void EnableShellSelfCollision()
	{
		CPhysicsShell* Shell = TargetShell();
		if (Shell == nullptr || SelfCollisionTaken)
		{
			return;
		}

		SelfCollisionWas = Shell->IsSelfCollision();
		SelfCollisionTaken = true;
		Shell->SetSelfCollision(true);
	}

	void RestoreShellSelfCollision()
	{
		if (!SelfCollisionTaken)
		{
			return;
		}

		SelfCollisionTaken = false;
		if (CPhysicsShell* Shell = TargetShell())
		{
			Shell->SetSelfCollision(SelfCollisionWas);
		}
	}

	void EnableShellExactIntegration()
	{
		CPhysicsShell* Shell = TargetShell();
		if (Shell == nullptr || ExactTaken)
		{
			return;
		}

		ExactWas = Shell->IsPrefereExactIntegration();
		ExactTaken = true;
		Shell->SetPrefereExactIntegration();
	}

	void RestoreShellExactIntegration()
	{
		if (!ExactTaken)
		{
			return;
		}

		ExactTaken = false;
		if (CPhysicsShell* Shell = TargetShell())
		{
			if (ExactWas)
			{
				Shell->SetPrefereExactIntegration();
			}
			else
			{
				Shell->UnSetPrefereExactIntegration();
			}
		}
	}

	// Camera mode: the element is held by the joint at the aimed point, and the rotation follows
	// the camera like the ODE AMotor. Nothing is teleported per step, so the shape does not
	// flicker when the view turns, and the grabbed point stays under the crosshair.
	void TuneCamera(float Step)
	{
		if (!Element || Character != nullptr || State != EState::Captured)
		{
			return;
		}

		const bool Motor = MotorEnabled && RelRotationValid;

		// CPHCapture: gravity is off only while the rotation motor holds the camera orientation.
		if (Motor)
		{
			DisableGravity();
		}
		else
		{
			RestoreGravity();
		}

		if (g_CameraCaptureNoCollision)
		{
			DisableShellCollisions();
		}
		else
		{
			RestoreShellCollisions();
		}

		if (g_CameraCaptureSelfCollision)
		{
			EnableShellSelfCollision();
		}
		else
		{
			RestoreShellSelfCollision();
		}

		if (g_CameraCaptureExactIntegration)
		{
			EnableShellExactIntegration();
		}
		else
		{
			RestoreShellExactIntegration();
		}

		if (CPhysicsShell* Shell = TargetShell())
		{
			g_CameraCaptureShellInfo.Elements = int(Shell->get_ElementsNumber());
			g_CameraCaptureShellInfo.SelfCollisionContacts = int(Shell->LastSelfCollisionContacts());
			g_CameraCaptureShellInfo.Animator = Shell->PPhysicsShellAnimator() != nullptr;
		}

		TuneAnchor(Step);
	}

public:
	CB3Capture(IPHCharacter* InCharacter, IPhysicsShellHolder* InTarget, CPhysicsElement* InElement, const Fvector* GlobalPos, bool InFreeChain) :
		Character(InCharacter),
		Target(InTarget),
		Element(InElement),
		FreeChain(InFreeChain)
	{
		if (GlobalPos)
		{
			CapturePos.set(*GlobalPos);
		}
		if (g_B3World && Target && Target->ObjectPPhysicsShell())
		{
			Init();
		}
	}

	virtual ~CB3Capture() override
	{
		// A capture destroyed without an explicit release must not leave the joint, the velocity
		// limits or the actor's contact callback behind.
		Release();
		CPHUpdateObject::Deactivate();
	}

	virtual bool Failed() override
	{
		// CPHCapture::Failed: true once the capture is over. A released capture is spent, and
		// CPHMovementControl::in_shedule_Update destroys it, which is what clears PHCapture()
		// and lets the actor grab the next object. Without it the first drag disables the key.
		return State == EState::Free || State == EState::Released;
	}
	virtual bool isCaptured() override
	{
		return State == EState::Captured;
	}
	virtual bool isPulling() override
	{
		return State == EState::Pulling;
	}
	virtual Fvector& capture_pos() override
	{
		return CapturePos;
	}

	virtual void ApplyForce(Fvector dir, float force) override
	{
		if (Element)
		{
			Element->applyForce(dir, force);
		}
	}
	virtual void SetRotation(Fmatrix form) override
	{
		// ODE rotates the helper body, which the joint transfers to the element. Rotating the
		// element directly gives the same result for the camera grab (new = form * current).
		if (Element && State == EState::Captured)
		{
			Fmatrix Current;
			Element->GetGlobalTransformDynamic(&Current);

			Fmatrix Result;
			Result.mul_43(form, Current);
			Element->SetTransform(Result, mh_clear);
		}
	}
	virtual void SetAMotorVel(Fvector dir) override
	{
		dir.clamp(Fvector().set(-10.f, -10.f, -10.f), Fvector().set(10.f, 10.f, 10.f));

		if (Element && State == EState::Captured)
		{
			Element->set_AngularVel(dir);
		}
	}
	virtual void EnableAMotor(bool val) override
	{
		// CPHCapture::EnableAMotor: on/off for the rotation hold. The ball joint can not change
		// its angular part, so the capture joint is rebuilt: weld with the motor, ball without.
		if (MotorEnabled == val)
		{
			return;
		}

		MotorEnabled = val;

		if (Character == nullptr && State == EState::Captured && g_B3World != nullptr && !g_B3World->IsStepping())
		{
			CreateAnchor();
		}
	}
	virtual void SetFreeChain(bool val) override
	{
		if (FreeChain == val)
		{
			return;
		}

		if (val)
		{
			RestoreGravity();
			RestoreShellCollisions();
			RestoreShellSelfCollision();
			RestoreShellExactIntegration();
		}

		FreeChain = val;
		EnableAMotor(!val);

		RelRotationValid = false;
	}
	virtual void SetTargetRotation(const Fmatrix& camera_rotation) override
	{
		CameraRotation.set(camera_rotation);

		if (RelRotationValid || Element == nullptr)
		{
			return;
		}

		// Keep the grabbed orientation relative to the camera, like ODE: the first call
		// only records the offset between the body and the view.
		Fmatrix ElementRotation;
		Element->GetGlobalTransformDynamic(&ElementRotation);
		ElementRotation.c.set(0.f, 0.f, 0.f);

		Fmatrix CameraBasis;
		CameraBasis.set(camera_rotation.i, camera_rotation.j, camera_rotation.k, Fvector().set(0.f, 0.f, 0.f));

		Fmatrix InverseCamera;
		InverseCamera.transpose(CameraBasis);

		RelRotation.mul(InverseCamera, ElementRotation);
		RelRotationValid = true;
	}
	virtual void RotateTarget(const Fmatrix& delta) override
	{
		if (!RelRotationValid)
		{
			return;
		}

		Fmatrix Rotated;
		Rotated.mul(delta, RelRotation);
		RelRotation.set(Rotated);
	}
	virtual void SetTargetAngularVelocity(const Fvector& velocity) override
	{
		CameraAngularVelocity.set(velocity);
		CameraVelocityValid = true;
	}

	virtual void RemoveConnection(IPhysicsShellHolder* O) override
	{
		if (O == Target)
		{
			Release();
			Element = nullptr;
		}
	}
	virtual void NetRelcase(CPhysicsShell* s) override
	{
		if (Element && Element->PhysicsShell() == s)
		{
			Release();
			Element = nullptr;
		}
	}
	// CPHCapture: a released body flies on with the motion of the hand. The joint already gives
	// the element the hand velocity, but the velocity limit and a hand that stopped inside the
	// last step can leave it slower, so the hand velocity is transferred explicitly.
	bool TakeReleaseVelocity(Fvector& Linear, Fvector& Angular) const
	{
		if (Character != nullptr || !b3Joint_IsValid(Joint) || !b3Body_IsValid(Helper) || Element == nullptr)
		{
			return false;
		}

		const CPHElement* E = static_cast<const CPHElement*>(Element);
		if (!b3Body_IsValid(E->Body))
		{
			return false;
		}

		const b3Vec3 HandVelocity = b3Body_GetLinearVelocity(Helper);
		const b3Vec3 HandAngular = b3Body_GetAngularVelocity(Helper);
		if (b3LengthSquared(HandVelocity) < 0.0025f && b3LengthSquared(HandAngular) < 0.0025f)
		{
			// The hand stood still: keep whatever velocity the element already has.
			return false;
		}

		const b3Vec3 R = b3SubPos(b3Body_GetPosition(Helper), b3Body_GetWorldCenterOfMass(E->Body));
		// Velocity of the grabbed point -> velocity of the centre of mass.
		Linear = B3Math::FromB3(b3Sub(HandVelocity, b3Cross(HandAngular, R)));
		Angular = B3Math::AngularFromB3(HandAngular);
		return true;
	}

	virtual void Release() override
	{
		if (State == EState::Free || State == EState::Released)
		{
			return;
		}

		State = EState::Released;
		CPHUpdateObject::Deactivate();

		Fvector ReleaseVelocity = {0.f, 0.f, 0.f};
		Fvector ReleaseAngular = {0.f, 0.f, 0.f};
		const bool KeepVelocity = TakeReleaseVelocity(ReleaseVelocity, ReleaseAngular);

		DestroyAnchor();

		// CPHCapture::Release: the element is free again.
		if (Element && Target && Target->ObjectPPhysicsShell() && Target->ObjectPPhysicsShell()->isActive())
		{
			Element->set_DynamicLimits();
			if (KeepVelocity)
			{
				// After the limits are back, otherwise CutVelocity would clamp the throw.
				Element->set_LinearVel(ReleaseVelocity);
				Element->set_AngularVel(ReleaseAngular);
			}
		}
		if (Character)
		{
			Character->SetObjectContactCallback(nullptr);
		}

		RestoreGravity();
		RestoreShellCollisions();
		RestoreShellSelfCollision();
		RestoreShellExactIntegration();

		g_CameraCaptureShellInfo.Elements = 0;
		g_CameraCaptureShellInfo.SelfCollisionContacts = 0;
		g_CameraCaptureShellInfo.Animator = false;

		if (Character && Character->PhysicsRefObject() && Character->PhysicsRefObject()->IsActor())
		{
			Character->PhysicsRefObject()->HideAllWeapons(false);
		}
	}

	virtual void PhTune(float Step) override
	{
		if (Element == nullptr || (State != EState::Pulling && State != EState::Captured))
		{
			return;
		}

		UpdateCapturePos();

		// The character grab is a joint on the hand: the element is rigidly held, as in ODE.
		if (Character)
		{
			TuneCharacter(Step);
			return;
		}

		Fvector Position;
		Element->GetGlobalPositionDynamic(&Position);
		const float Distance = Position.distance_to(CapturePos);

		if (State == EState::Pulling)
		{
			if (Distance > PullDistance)
			{
				Release();
				return;
			}
			if (Distance < CaptureDistance)
			{
				if (CreateAnchor())
				{
					State = EState::Captured;
				}
				return;
			}

			// CPHCapture::PullingUpdate: a constant force towards the captured point.
			Fvector Dir;
			Dir.sub(CapturePos, Position);
			if (Dir.magnitude() < EPS_L)
			{
				return;
			}
			Dir.normalize();
			Element->applyForce(Dir, PullForce);
			return;
		}

		TuneCamera(Step);
	}

	virtual void PhDataUpdate(float Step) override
	{
		if (Element == nullptr || Character == nullptr || State != EState::Captured)
		{
			return;
		}

		// The joint force is known after the step: it releases a stuck element and tugs the actor.
		UpdateAnchor();
	}
};

static CPhysicsElement* ElementOfBone(IPhysicsShellHolder* Object, u16 BoneId, const Fvector* Anchor)
{
	IKinematics* K = Object ? Object->ObjectKinematics() : nullptr;
	CPhysicsShell* Shell = Object ? Object->ObjectPPhysicsShell() : nullptr;
	if (Shell == nullptr)
	{
		return nullptr;
	}

	// CPHCapture: the grabbed element is the physics element of the aimed bone.
	if (K && BoneId != BI_NONE && BoneId < K->LL_BoneCount())
	{
		CBoneInstance& Bone = K->LL_GetBoneInstance(BoneId);
		if (Bone.callback_type() == bctPhysics && Bone.callback_param())
		{
			return static_cast<CPhysicsElement*>(Bone.callback_param());
		}
	}

	// A bone without a physics element: the element under the aimed point (CPHCapture uses
	// NearestToPoint too). Falling back to element 0 grabbed the wrong part of the body.
	if (Anchor)
	{
		return Shell->NearestToPoint(*Anchor);
	}

	return Shell->get_Element(BoneId);
}

IPHCapture* phcapture_create(IPHCharacter* ch, IPhysicsShellHolder* object, NearestToPointCallback* cb)
{
	CPhysicsShell* Shell = object ? object->ObjectPPhysicsShell() : nullptr;
	if (ch == nullptr || Shell == nullptr || !Shell->isActive())
	{
		return nullptr;
	}

	Fvector From;
	ch->GetBodyPosition(From);
	return new CB3Capture(ch, object, Shell->NearestToPoint(From, cb), nullptr, false);
}

IPHCapture* phcapture_create(IPHCharacter* ch, IPhysicsShellHolder* object, u16 element)
{
	CPhysicsElement* Element = ElementOfBone(object, element, nullptr);
	return Element ? new CB3Capture(ch, object, Element, nullptr, false) : nullptr;
}

IPHCapture* phcapture_create(IPHCharacter* ch, IPhysicsShellHolder* object, u16 element, const Fvector& global_pos, bool free_chain)
{
	CPhysicsElement* Element = ElementOfBone(object, element, &global_pos);
	return Element ? new CB3Capture(ch, object, Element, &global_pos, free_chain) : nullptr;
}

void phcapture_destroy(IPHCapture*& c)
{
	if (c)
	{
		c->Release();
		CB3Capture* Capture = static_cast<CB3Capture*>(c);
		xr_delete(Capture);
	}
	c = nullptr;
}

// Box overlap against level and static shells.
static bool BoxOverlaps(const Fvector& HalfSize, const Fmatrix& Form, u64 Mask)
{
	if (g_B3World == nullptr)
	{
		return false;
	}

	b3Vec3 Points[8];
	for (int I = 0; I < 8; ++I)
	{
		Fvector Local;
		Local.set((I & 1) ? HalfSize.x : -HalfSize.x, (I & 2) ? HalfSize.y : -HalfSize.y, (I & 4) ? HalfSize.z : -HalfSize.z);
		Fvector Global;
		Form.transform_tiny(Global, Local);
		Points[I] = B3Math::ToB3(Global);
	}

	b3ShapeProxy Proxy = {};
	Proxy.points = Points;
	Proxy.count = 8;
	Proxy.radius = 0.f;

	b3QueryFilter Filter = b3DefaultQueryFilter();
	Filter.categoryBits = B3Category::Dynamic;
	Filter.maskBits = Mask;
	bool Hit = false;
	const auto Callback = [](b3ShapeId ShapeId, void* Context) -> bool
	{
		*static_cast<bool*>(Context) = true;
		return false;
	};
	b3World_OverlapShape(g_B3World->Id(), b3Vec3_zero, &Proxy, Filter, Callback, &Hit);
	return Hit;
}

// Finds a free spot for a box near Position: as is, then lifted, then around.
static bool FindFreeBox(const Fvector& HalfSize, Fmatrix Form, u64 Mask, Fvector& Result)
{
	static const Fvector Offsets[] = {
		{0.f, 0.f, 0.f}, {0.f, 0.1f, 0.f}, {0.f, 0.25f, 0.f}, {0.f, 0.5f, 0.f},
		{0.25f, 0.1f, 0.f}, {-0.25f, 0.1f, 0.f}, {0.f, 0.1f, 0.25f}, {0.f, 0.1f, -0.25f},
		{0.5f, 0.25f, 0.f}, {-0.5f, 0.25f, 0.f}, {0.f, 0.25f, 0.5f}, {0.f, 0.25f, -0.5f},
		{0.f, 1.f, 0.f}};

	const Fvector Start = Form.c;
	for (const Fvector& Offset : Offsets)
	{
		Form.c.add(Start, Offset);
		if (!BoxOverlaps(HalfSize, Form, Mask))
		{
			Result.set(Form.c);
			return true;
		}
	}
	Result.set(Start);
	return false;
}

static constexpr u64 SolidMask = B3Category::Level | B3Category::Static;

void ActivateShapeExplosive(IPhysicsShellHolder* self_obj, const Fvector& size, Fvector& out_size, Fvector& in_out_pos)
{
	out_size.set(size);
	Fmatrix Form = Fidentity;
	Form.c.set(in_out_pos);
	FindFreeBox(Fvector().mul(size, 0.5f), Form, SolidMask, in_out_pos);
}

void ActivateShapePhysShellHolder(IPhysicsShellHolder* obj, const Fmatrix& in_xform, const Fvector& in_size, Fvector& in_pos, Fvector& out_pos)
{
	Fmatrix Form = in_xform;
	Form.c.set(in_pos);
	FindFreeBox(Fvector().mul(in_size, 0.5f), Form, SolidMask, out_pos);
}

bool ActivateShapeCharacterPhysicsSupport(Fvector& out_pos, const Fvector& vbox, const Fvector& activation_pos, const Fmatrix& mXFORM, bool not_collide_characters, bool set_rotation, IPhysicsShellHolder* m_EntityAlife)
{
	Fmatrix Form = set_rotation ? mXFORM : Fidentity;
	Form.c.set(activation_pos);
	const u64 Mask = not_collide_characters ? SolidMask : (SolidMask | B3Category::Character);
	return FindFreeBox(Fvector().mul(vbox, 0.5f), Form, Mask, out_pos);
}

bool ActivateBoxDynamic(IPHMovementControl* mov_control, bool character_exist, DWORD id, int num_it, int num_steps, float resolve_depth)
{
	IPHCharacter* Character = mov_control->character();
	if (!character_exist || Character == nullptr)
	{
		return true;
	}

	const Fbox& Box = mov_control->Boxes()[id];
	Fvector Size, Center;
	Box.getsize(Size);
	Box.getcenter(Center);

	Fvector Position;
	Character->GetPosition(Position);
	Fmatrix Form = Fidentity;
	Form.c.set(Position.x + Center.x, Position.y + Center.y + 0.01f, Position.z + Center.z);
	Fvector Free;
	if (!FindFreeBox(Fvector().mul(Size, 0.5f), Form, SolidMask, Free))
	{
		return false;
	}

	Fvector NewPosition;
	NewPosition.sub(Free, Center);
	NewPosition.y -= 0.01f;
	Character->SetPosition(NewPosition);
	return true;
}

bool test_camera_box(const Fvector& box_size, const Fmatrix& xform, IPhysicsShellHolder* l_actor)
{
	return BoxOverlaps(Fvector().mul(box_size, 0.5f), xform, SolidMask);
}

// Pulls the camera towards the actor head when level geometry is between them.
void collide_camera(CCameraBase& camera, float _viewport_near, IPhysicsShellHolder* l_actor)
{
	if (g_B3World == nullptr || l_actor == nullptr)
	{
		return;
	}

	Fvector Head = l_actor->ObjectXFORM().c;
	Head.y += 1.5f;
	Fvector Dir;
	Dir.sub(camera.vPosition, Head);
	const float Distance = Dir.magnitude();
	if (Distance < EPS_L)
	{
		return;
	}
	Dir.div(Distance);

	SB3RayHit Hit;
	if (g_B3World->RayPick(Head, Dir, Distance + _viewport_near, SolidMask, Hit))
	{
		camera.vPosition.mad(Head, Dir, std::max(0.f, Hit.Distance - _viewport_near));
	}
}
