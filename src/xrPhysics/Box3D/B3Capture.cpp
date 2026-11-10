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
#include "../../xrEngine/GameMtlLib.h"

// Capture: the target element is pulled to the capture point, then held there by a joint.
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

	// CPHCapture ties the element to a body teleported to the hand with a ball joint and an AMotor
	// (a velocity lock with a capped torque). One spherical joint to a kinematic helper that never
	// rotates is both: its motor drives the element's angular velocity to the target.
	b3BodyId Helper = b3_nullBodyId;
	b3JointId Joint = b3_nullJointId;
	// CPHCapture::Init reads velocity_scale: the element's velocities are limited while dragging.
	float VelocityScale = 1.f;
	// CPHCapture::EnableAMotor / SetFreeChain: the camera motor drives the grabbed orientation.
	bool MotorEnabled = true;
	// The release test needs the net constraint force on the element over the step.
	b3Vec3 PreStepVelocity = b3Vec3_zero;
	// CPHCapture b_collide / b_disabled: a released object ignores its holder until a step without contact.
	bool ReleasedTouch = false;
	bool ElementSlept = false;

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

		// CPHCapture measures from the body position, which in ODE is the centre of mass.
		if (Element->mass_Center().distance_to(CapturePos) > PullDistance)
		{
			return;
		}

		PullForce = std::min(PullForce, 4.f * g_B3World->Gravity() * ShellMass());
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
			if (CB3Capture* Capture = Holder ? Holding(Holder, Contact.GetObject(Side != 0)) : nullptr)
			{
				DoCollide = false;
				Capture->ReleasedTouch = true;
				return;
			}
		}
	}

	float ShellMass() const
	{
		return Element->PhysicsShell() ? Element->PhysicsShell()->getMass() : Element->getMass();
	}

	float MotorTorque() const
	{
		return MotorEnabled ? (Character ? 0.2f * CaptureForce : g_CameraCaptureMotorForce) : 0.f;
	}

	// CPHCapture::ApplyAnchorRotation: the camera grab turns the element towards its camera-relative orientation.
	b3Vec3 MotorVelocity(float Step) const
	{
		if (Character || !MotorEnabled || !RelRotationValid)
		{
			return b3Vec3_zero;
		}

		Fmatrix Desired, Current, Error;
		Desired.mul(CameraRotation, RelRotation);
		Element->GetGlobalTransformDynamic(&Current);
		Error.mul(Desired, Fmatrix().transpose(Current));

		Fvector Velocity = CameraVelocityValid ? CameraAngularVelocity : Fvector().set(0.f, 0.f, 0.f);
		Velocity.mad(Fvector().set(Error._23 - Error._32, Error._31 - Error._13, Error._12 - Error._21), 0.5f * std::min(0.25f / Step, 60.f));
		Velocity.clamp(Fvector().set(-20.f, -20.f, -20.f), Fvector().set(20.f, 20.f, 20.f));
		return B3Math::AngularToB3(Velocity);
	}

	void Pull(const Fvector& Position)
	{
		// A constant force towards the hand, as m_taget_element->applyForce(dir, m_pull_force).
		Fvector Dir;
		Dir.sub(CapturePos, Position);
		if (Dir.magnitude() >= EPS_L)
		{
			Element->applyForce(Dir.normalize(), PullForce);
		}
	}

	// CPHCapture::PullingUpdate: the joint is created at the capture point once the element is close enough.
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

		const b3WorldTransform Body = b3Body_GetTransform(E->Body);
		b3SphericalJointDef JointDef = b3DefaultSphericalJointDef();
		JointDef.base.bodyIdA = Helper;
		JointDef.base.bodyIdB = E->Body;
		JointDef.base.localFrameB.p = b3InvRotateVector(Body.q, b3SubPos(B3Math::ToB3(CapturePos), Body.p));
		JointDef.enableMotor = true;
		JointDef.maxMotorTorque = MotorTorque();
		Joint = b3CreateSphericalJoint(g_B3World->Id(), &JointDef);
		if (!b3Joint_IsValid(Joint))
		{
			DestroyAnchor();
			return false;
		}

		// The joint holds the element now, so the pull velocity limit is not needed any more and
		// would only cap the speed the body keeps when it is released.
		Element->set_DynamicLimits();

		// CPHCapture::PullingUpdate stops the element before the joint takes it, unless the chain is free.
		if (!FreeChain)
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

	// CPHCapture::CapturedUpdate teleports the hand, and its ball joint closes world_erp of the gap
	// per step: the helper moves as much, or spreads the gap over the frame when it has more steps.
	// Called before the step only, so the world is not locked.
	void TuneAnchor(float Step)
	{
		if (!b3Joint_IsValid(Joint))
		{
			return;
		}

		const float Horizon = Step * std::max(float(g_B3World->StepsLeft()), 1.f / world_erp);
		b3Body_SetTargetTransform(Helper, {b3ToPos(B3Math::ToB3(CapturePos)), b3Quat_identity}, Horizon, true);
		b3SphericalJoint_SetMaxMotorTorque(Joint, MotorTorque());
		b3SphericalJoint_SetMotorVelocity(Joint, MotorVelocity(Step));
	}

	// CPHCapture::CapturedUpdate: ODE's joint feedback is the net constraint force on the element,
	// which releases it; the ball joint stretch (absolute world_spring) tugs the holder.
	void UpdateAnchor(float Step)
	{
		const CPHElement* E = static_cast<const CPHElement*>(Element);
		if (!b3Joint_IsValid(Joint) || !b3Body_IsValid(E->Body))
		{
			return;
		}

		const float Mass = b3Body_GetMass(E->Body);
		b3Vec3 Force = b3MulSV(Mass / Step, b3Sub(b3Body_GetLinearVelocity(E->Body), PreStepVelocity));
		Force.y += Element->get_ApplyByGravity() ? Mass * g_B3World->Gravity() : 0.f;
		if (b3LengthSquared(Force) > CaptureForce * CaptureForce)
		{
			Release();
			return;
		}

		const b3Vec3 JointForce = b3Joint_GetConstraintForce(Joint);
		const float Magnitude = b3Length(JointForce);
		if (Magnitude > EPS && b3Joint_GetLinearSeparation(Joint) > CaptureForce / 2.2f / world_spring)
		{
			// The reaction pulls the actor towards the object with capture_force / 15, as ODE.
			Character->ApplyForce(B3Math::FromB3(b3MulSV(-(CaptureForce / 15.f) / Magnitude, JointForce)));
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

		const Fvector Position = Element->mass_Center();
		const float Distance = Position.distance_to(CapturePos);
		if (State == EState::Pulling)
		{
			if (Distance > PullDistance || Device.dwTimeGlobal - TimeStart > TimeLimit)
			{
				Release();
				return;
			}
			if (Distance >= CaptureDistance)
			{
				Pull(Position);
				return;
			}
			if (!CreateAnchor())
			{
				return;
			}
			State = EState::Captured;
		}
		else if (!b3Joint_IsValid(Joint))
		{
			// The element's body is gone (shell deactivated): nothing holds it any more.
			Release();
			return;
		}

		// Captured: CPHCapture does not recheck the distance (a big element is grabbed at its
		// edge, so its center is far from the hand); the joint holds it.
		TuneAnchor(Step);
		PreStepVelocity = b3Body_GetLinearVelocity(static_cast<CPHElement*>(Element)->Body);
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

	// CPHCapture::Deactivate: the capture is over and the holder collides with the object again.
	void Finish()
	{
		if (Character && State != EState::Free)
		{
			Character->SetObjectContactCallback(nullptr);
		}
		State = EState::Free;
		CPHUpdateObject::Deactivate();
	}

public:
	// The capture of Holder while it holds Object, released or not.
	static CB3Capture* Holding(IPhysicsShellHolder* Holder, const IPhysicsShellHolder* Object)
	{
		CB3Capture* Capture = static_cast<CB3Capture*>(Holder->PHCapture());
		return Capture && Capture->State != EState::Free && Capture->Target == Object ? Capture : nullptr;
	}

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
		Finish();
	}

	virtual bool Failed() override
	{
		// CPHCapture::Failed: a released capture lives until the holder and the object separate,
		// then CPHMovementControl::in_shedule_Update destroys it and the actor can grab again.
		return State == EState::Free;
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
		// CPHCapture::ApplyForce: the force is in shell weights, capped as ODE's BodyCutForce.
		if (CPHElement* E = g_B3World ? static_cast<CPHElement*>(Element) : nullptr)
		{
			E->applyForce(dir, std::min(force * g_B3World->Gravity() * ShellMass(), E->LinearLimit / fixed_step * E->getMass()));
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
		// CPHCapture::EnableAMotor: TuneAnchor applies the motor torque before the next step.
		MotorEnabled = val;
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
			Finish();
			Element = nullptr;
		}
	}
	virtual void NetRelcase(CPhysicsShell* s) override
	{
		if (Element && Element->PhysicsShell() == s)
		{
			Release();
			Finish();
			Element = nullptr;
		}
	}

	virtual void Release() override
	{
		if (State == EState::Free || State == EState::Released)
		{
			return;
		}

		// CPHCapture::Release: the element is free again and keeps its velocity, but it ignores
		// the holder until they separate.
		State = EState::Released;
		ReleasedTouch = true;
		DestroyAnchor();
		if (Element && Target && Target->ObjectPPhysicsShell() && Target->ObjectPPhysicsShell()->isActive())
		{
			Element->set_DynamicLimits();
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
		if (Element == nullptr || State == EState::Free)
		{
			return;
		}

		// CPHCapture::PhTune: a sleeping element is woken, and a released one waits a step for its contacts.
		ElementSlept = Element->isActive() && !Element->isEnabled();
		if (ElementSlept)
		{
			Element->Enable();
		}
		if (State == EState::Released)
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

		const Fvector Position = Element->mass_Center();
		const float Distance = Position.distance_to(CapturePos);
		if (State == EState::Pulling)
		{
			if (Distance > PullDistance)
			{
				Release();
			}
			else if (Distance >= CaptureDistance)
			{
				Pull(Position);
			}
			else if (CreateAnchor())
			{
				State = EState::Captured;
			}
			return;
		}

		TuneCamera(Step);
	}

	virtual void PhDataUpdate(float Step) override
	{
		if (State == EState::Released)
		{
			// CPHCapture::ReleasedUpdate: the holder collides with the object again after a step without contact.
			if (!ElementSlept && !std::exchange(ReleasedTouch, false))
			{
				Finish();
			}
			return;
		}

		if (Element && Character && State == EState::Captured)
		{
			// The joint state is known after the step: it releases a stuck element and tugs the actor.
			UpdateAnchor(Step);
		}
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
	// NearestToPoint too). Without an aimed point ODE's can_capture refuses the bone.
	return Anchor ? Shell->NearestToPoint(*Anchor) : nullptr;
}

// CPHCapture can_capture: an active shell, and a character grab neither takes inventory items
// nor starts without the character's controller.
static bool CanCapture(IPHCharacter* Character, IPhysicsShellHolder* Object)
{
	CPhysicsShell* Shell = Object ? Object->ObjectPPhysicsShell() : nullptr;
	return Shell && Shell->isActive() && (Character == nullptr || (Character->Exists() && !Object->IsInventoryItem()));
}

// The monster drag picks the element nearest to the monster body. ODE passed the model-space
// capture bone position to a world-space search, which picked an arbitrary limb.
IPHCapture* phcapture_create(IPHCharacter* ch, IPhysicsShellHolder* object, NearestToPointCallback* cb)
{
	if (ch == nullptr || !CanCapture(ch, object))
	{
		return nullptr;
	}

	Fvector From;
	ch->GetBodyPosition(From);
	return new CB3Capture(ch, object, object->ObjectPPhysicsShell()->NearestToPoint(From, cb), nullptr, false);
}

IPHCapture* phcapture_create(IPHCharacter* ch, IPhysicsShellHolder* object, u16 element)
{
	CPhysicsElement* Element = CanCapture(ch, object) ? ElementOfBone(object, element, nullptr) : nullptr;
	return Element ? new CB3Capture(ch, object, Element, nullptr, false) : nullptr;
}

IPHCapture* phcapture_create(IPHCharacter* ch, IPhysicsShellHolder* object, u16 element, const Fvector& global_pos, bool free_chain)
{
	CPhysicsElement* Element = CanCapture(ch, object) ? ElementOfBone(object, element, &global_pos) : nullptr;
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

static constexpr u64 SolidMask = B3Category::Level | B3Category::Static;
// ODE resolve_depth: a resting contact is not an overlap.
static constexpr float ActivationDepth = 0.01f;
// ActorCameraCollision camera_collision_sckin_depth.
static constexpr float CameraSkin = 0.04f;
static constexpr int QueryPlanes = 32;

struct SB3Query
{
	IPhysicsShellHolder* Self;
	bool Hit = false;
	int PlaneCount = 0;
	b3CollisionPlane Planes[QueryPlanes];
};

// The shapes of Self, of what Self holds and of passable materials do not block its queries.
static bool Blocks(b3ShapeId ShapeId, IPhysicsShellHolder* Self)
{
	const CODEGeom* Geom = static_cast<const CODEGeom*>(b3Shape_GetUserData(ShapeId));
	if (Geom == nullptr || Geom->Owner == nullptr)
	{
		return true;
	}

	IPhysicsShellHolder* Owner = Geom->Owner->RefObject;
	if (Self && Owner && (Owner == Self || CB3Capture::Holding(Self, Owner)))
	{
		return false;
	}

	const SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(Geom->Owner->Material);
	return Mtl == nullptr || !Mtl->Flags.test(SGameMtl::flPassable);
}

static b3QueryFilter QueryFilter(u64 Mask)
{
	b3QueryFilter Filter = b3DefaultQueryFilter();
	Filter.categoryBits = B3Category::Dynamic | ((Mask & B3Category::ActorObstacle) ? B3Category::ActorObstacleQuery : 0ull);
	Filter.maskBits = Mask;
	return Filter;
}

static bool Overlaps(b3Vec3 Origin, const b3ShapeProxy& Proxy, u64 Mask, IPhysicsShellHolder* Self)
{
	SB3Query Query = {Self};
	b3World_OverlapShape(g_B3World->Id(), Origin, &Proxy, QueryFilter(Mask), [](b3ShapeId ShapeId, void* Context) -> bool
	{
		SB3Query& Data = *static_cast<SB3Query*>(Context);
		Data.Hit = Blocks(ShapeId, Data.Self);
		return !Data.Hit;
	}, &Query);
	return Query.Hit;
}

static bool Casts(b3Vec3 Origin, const b3ShapeProxy& Proxy, b3Vec3 Translation, u64 Mask, IPhysicsShellHolder* Self)
{
	SB3Query Query = {Self};
	b3World_CastShape(g_B3World->Id(), Origin, &Proxy, Translation, QueryFilter(Mask), [](b3ShapeId ShapeId, b3Pos, b3Vec3, float, uint64_t, int, int, void* Context) -> float
	{
		SB3Query& Data = *static_cast<SB3Query*>(Context);
		Data.Hit = Blocks(ShapeId, Data.Self);
		return Data.Hit ? 0.f : -1.f;
	}, &Query);
	return Query.Hit;
}

// One mover pass: the translation that pushes the capsule out of what it overlaps.
static b3Vec3 PushOut(b3Vec3 Origin, const b3Capsule& Capsule, u64 Mask, IPhysicsShellHolder* Self)
{
	SB3Query Query = {Self};
	b3World_CollideMover(g_B3World->Id(), Origin, &Capsule, QueryFilter(Mask), [](b3ShapeId ShapeId, const b3PlaneResult* Results, int Count, void* Context) -> bool
	{
		SB3Query& Data = *static_cast<SB3Query*>(Context);
		if (Blocks(ShapeId, Data.Self))
		{
			for (int I = 0; I < Count && Data.PlaneCount < QueryPlanes; ++I)
			{
				Data.Planes[Data.PlaneCount++] = {Results[I].plane, FLT_MAX, 0.f, true};
			}
		}
		return Data.PlaneCount < QueryPlanes;
	}, &Query);
	return b3SolvePlanes(b3Vec3_zero, Query.Planes, Query.PlaneCount).delta;
}

// Box corners at Form's rotation, relative to the box centre, shrunk by Inset.
static b3ShapeProxy BoxProxy(const Fvector& HalfSize, const Fmatrix& Form, float Inset, b3Vec3 (&Points)[8])
{
	const Fvector Half = {std::max(HalfSize.x - Inset, 0.f), std::max(HalfSize.y - Inset, 0.f), std::max(HalfSize.z - Inset, 0.f)};
	for (int I = 0; I < 8; ++I)
	{
		Fvector Corner;
		Form.transform_dir(Corner, Fvector().set((I & 1) ? Half.x : -Half.x, (I & 2) ? Half.y : -Half.y, (I & 4) ? Half.z : -Half.z));
		Points[I] = B3Math::ToB3(Corner);
	}
	return {Points, 8, 0.f};
}

// PHActivationShape: the box takes the first free spot near its start. The path of its centre
// is cast too, so a lateral offset never carries it through a thin wall.
static bool FindFreeBox(const Fvector& HalfSize, const Fmatrix& Form, u64 Mask, IPhysicsShellHolder* Self, Fvector& Result)
{
	static const Fvector Offsets[] = {
		{0.f, 0.f, 0.f}, {0.f, 0.1f, 0.f}, {0.f, 0.25f, 0.f}, {0.f, 0.5f, 0.f},
		{0.25f, 0.1f, 0.f}, {-0.25f, 0.1f, 0.f}, {0.f, 0.1f, 0.25f}, {0.f, 0.1f, -0.25f},
		{0.5f, 0.25f, 0.f}, {-0.5f, 0.25f, 0.f}, {0.f, 0.25f, 0.5f}, {0.f, 0.25f, -0.5f},
		{0.f, 1.f, 0.f}};

	Result.set(Form.c);
	if (g_B3World == nullptr)
	{
		return true;
	}

	b3Vec3 Points[8];
	const b3ShapeProxy Box = BoxProxy(HalfSize, Form, ActivationDepth, Points);
	const b3ShapeProxy Centre = {&b3Vec3_zero, 1, 0.f};
	const b3Vec3 Start = B3Math::ToB3(Form.c);
	for (const Fvector& Offset : Offsets)
	{
		const b3Vec3 Move = B3Math::ToB3(Offset);
		if (!Overlaps(b3Add(Start, Move), Box, Mask, Self) && (&Offset == &Offsets[0] || !Casts(Start, Centre, Move, Mask, Self)))
		{
			Result.add(Offset);
			return true;
		}
	}
	return false;
}

void ActivateShapeExplosive(IPhysicsShellHolder* self_obj, const Fvector& size, Fvector& out_size, Fvector& in_out_pos)
{
	out_size.set(size);
	Fmatrix Form = Fidentity;
	Form.c.set(in_out_pos);
	FindFreeBox(Fvector().mul(size, 0.5f), Form, SolidMask, self_obj, in_out_pos);
}

void ActivateShapePhysShellHolder(IPhysicsShellHolder* obj, const Fmatrix& in_xform, const Fvector& in_size, Fvector& in_pos, Fvector& out_pos)
{
	Fmatrix Form = in_xform;
	Form.c.set(in_pos);
	FindFreeBox(Fvector().mul(in_size, 0.5f), Form, SolidMask, obj, out_pos);
}

bool ActivateShapeCharacterPhysicsSupport(Fvector& out_pos, const Fvector& vbox, const Fvector& activation_pos, const Fmatrix& mXFORM, bool not_collide_characters, bool set_rotation, IPhysicsShellHolder* m_EntityAlife)
{
	Fmatrix Form = set_rotation ? mXFORM : Fidentity;
	Form.c.set(activation_pos);
	const u64 Mask = not_collide_characters ? SolidMask : (SolidMask | B3Category::Character);
	return FindFreeBox(Fvector().mul(vbox, 0.5f), Form, Mask, m_EntityAlife, out_pos);
}

bool ActivateBoxDynamic(IPHMovementControl* mov_control, bool character_exist, u32 id, int num_it, int num_steps, float resolve_depth)
{
	IPHCharacter* Character = mov_control->character();
	if (Character == nullptr || g_B3World == nullptr)
	{
		return true;
	}

	// MovementBoxDynamicActivate: a shrinking box always fits. A growing one, or a character just
	// created, may be pushed out by at most the change of the box radius, else the box is refused.
	const Fbox& From = mov_control->Box();
	const Fbox& To = mov_control->Boxes()[id];
	Fvector FromSize, ToSize;
	From.getsize(FromSize);
	To.getsize(ToSize);
	if (character_exist && ToSize.x <= FromSize.x && ToSize.y <= FromSize.y && ToSize.z <= FromSize.z)
	{
		return true;
	}

	IPhysicsShellHolder* Self = Character->PhysicsRefObject();
	const u64 Mask = SolidMask | B3Category::Dynamic | B3Category::Character | (Character->IsActorCharacter() ? B3Category::ActorObstacle : 0ull);
	const float Radius = std::max(0.1f, std::min(ToSize.x, ToSize.z) * 0.5f);
	const b3Capsule Capsule = {{0.f, Radius, 0.f}, {0.f, std::max(ToSize.y - Radius, Radius + 0.01f), 0.f}, Radius - (character_exist ? resolve_depth : 0.1f)};
	const b3ShapeProxy Proxy = {&Capsule.center1, 2, Capsule.radius};

	Fvector Position;
	Character->GetPosition(Position);
	const b3Vec3 Start = B3Math::ToB3(Position);
	if (!Overlaps(Start, Proxy, Mask, Self))
	{
		return true;
	}

	const b3Vec3 Delta = PushOut(Start, Capsule, Mask, Self);
	const float Pass = character_exist ? std::abs(From.getradius() - To.getradius()) : To.getradius();
	if (b3Length(Delta) > Pass || Overlaps(b3Add(Start, Delta), Proxy, Mask, Self))
	{
		return false;
	}

	Character->SetPosition(B3Math::FromB3(b3Add(Start, Delta)));
	return true;
}

bool test_camera_box(const Fvector& box_size, const Fmatrix& xform, IPhysicsShellHolder* l_actor)
{
	if (g_B3World == nullptr)
	{
		return false;
	}

	// ActorCameraCollision set_camera_collision: box_size holds half extents, and the box is
	// taller and deeper, shifted down and back. Only contacts deeper than half the skin count.
	Fvector Centre;
	Centre.mad(xform.c, xform.j, -0.5f * box_size.y).mad(xform.k, -box_size.z);
	b3Vec3 Points[8];
	const b3ShapeProxy Box = BoxProxy(Fvector().set(box_size.x, box_size.y * 1.5f, box_size.z * 2.f), xform, CameraSkin * 0.5f, Points);
	return Overlaps(B3Math::ToB3(Centre), Box, SolidMask | B3Category::Dynamic | B3Category::Character, l_actor);
}

// The camera stays in sight of the actor (a ray from the actor axis at eye height, at most 1.5 m),
// then the near-plane volume is pushed out of everything but the actor, as ODE's camera box.
void collide_camera(CCameraBase& camera, float _viewport_near, IPhysicsShellHolder* l_actor)
{
	if (g_B3World == nullptr || l_actor == nullptr)
	{
		return;
	}

	const Fvector& Origin = l_actor->ObjectXFORM().c;
	const Fvector Head = {Origin.x, Origin.y + std::min(1.5f, camera.vPosition.y - Origin.y), Origin.z};
	Fvector Dir;
	Dir.sub(camera.vPosition, Head);
	const float Distance = Dir.magnitude();
	SB3RayHit Hit;
	if (Distance > EPS_L && g_B3World->RayPick(Head, Dir.div(Distance), Distance + _viewport_near, SolidMask, Hit))
	{
		camera.vPosition.mad(Head, Dir, std::max(0.f, Hit.Distance - _viewport_near));
	}

	// A capsule across the near plane: as wide as the box, no deeper than the near plane plus a
	// little, so a first-person eye 0.35 m from a wall is not pushed back.
	float HalfWidth, HalfHeight;
	tviewport_size(_viewport_near, camera, HalfWidth, HalfHeight);
	const b3Vec3 Side = B3Math::ToB3(Fvector().mul(camera.Right(), std::max(HalfWidth - HalfHeight, 0.f)));
	const b3Capsule Volume = {b3Neg(Side), Side, HalfHeight + CameraSkin};
	b3Vec3 Centre = B3Math::ToB3(Fvector().mad(camera.vPosition, camera.Direction(), _viewport_near * 0.5f));
	for (int I = 0; I < 5; ++I)
	{
		const b3Vec3 Delta = PushOut(Centre, Volume, SolidMask | B3Category::Dynamic | B3Category::Character, l_actor);
		if (b3LengthSquared(Delta) < 1e-8f)
		{
			break;
		}
		Centre = b3Add(Centre, Delta);
		camera.vPosition.add(B3Math::FromB3(Delta));
	}
}
