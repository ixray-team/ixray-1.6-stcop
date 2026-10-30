#include "StdAfx.h"
#include "B3Shell.h"
#include "B3World.h"
#include "B3Math.h"
#include "IPHCapture.h"
#include "IPHCharacter.h"
#include "IPhysicsShellHolder.h"
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
	Fvector AngularTarget = {0.f, 0.f, 0.f};
	Fmatrix TargetRotation = Fidentity;
	EState State = EState::Free;
	float PullDistance = 20.f;
	float CaptureDistance = 0.4f;
	float CaptureForce = 20500.f;
	float PullForce = 8000.f;
	u32 TimeLimit = 3000;
	u32 TimeStart = 0;
	bool AngularMotor = false;
	bool HasTargetRotation = false;
	bool FreeChain = false;
	bool GravityWas = true;

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
		return true;
	}

	void Init()
	{
		if (Element == nullptr || !Element->isActive())
		{
			return;
		}

		if (Character && !ReadCharacterParams())
		{
			return;
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

		if (Character && Character->PhysicsRefObject() && Character->PhysicsRefObject()->IsActor())
		{
			Character->PhysicsRefObject()->HideAllWeapons(true);
		}

		CPHUpdateObject::Activate();
		State = EState::Pulling;
	}

	void Drive(float Step, float MaxForce)
	{
		CPHElement* E = static_cast<CPHElement*>(Element);
		if (!b3Body_IsValid(E->Body))
		{
			Release();
			return;
		}

		Fvector Position, Velocity;
		Element->GetGlobalPositionDynamic(&Position);
		Element->get_LinearVel(Velocity);
		const float Mass = E->getMass();

		// Critically damped spring towards the capture point, then counter gravity.
		Fvector Wanted;
		Wanted.sub(CapturePos, Position).mul(1.f / std::max(Step * 8.f, 0.05f));
		Fvector Force;
		Force.sub(Wanted, Velocity).mul(Mass / Step);
		Force.y += Mass * g_B3World->Gravity();
		const float Magnitude = Force.magnitude();
		if (Magnitude > MaxForce)
		{
			Force.mul(MaxForce / Magnitude);
		}
		Element->applyForce(Force.x, Force.y, Force.z);

		if (AngularMotor || HasTargetRotation)
		{
			Fvector Angular = AngularTarget;
			if (HasTargetRotation)
			{
				// Rotate towards the target with the error divided over a few steps.
				Fmatrix Current;
				Element->GetGlobalTransformDynamic(&Current);
				Fquaternion Q0, Q1, Delta;
				Q0.set(Current);
				Q1.set(TargetRotation);
				Fquaternion Inverse;
				Inverse.inverse(Q0);
				Delta.mul(Q1, Inverse);
				Fvector Axis;
				float Angle;
				Delta.get_axis_angle(Axis, Angle);
				if (Angle > PI)
				{
					Angle -= PI_MUL_2;
				}
				Angular.mad(Axis, Angle / std::max(Step * 8.f, 0.05f));
			}
			Element->set_AngularVel(Angular);
		}
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
		CPHUpdateObject::Deactivate();
	}

	virtual bool Failed() override
	{
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
		if (Element)
		{
			Element->applyForce(dir, force);
		}
	}
	virtual void SetRotation(Fmatrix form) override
	{
		SetTargetRotation(form);
	}
	virtual void SetAMotorVel(Fvector dir) override
	{
		AngularTarget.set(dir);
	}
	virtual void EnableAMotor(bool val) override
	{
		AngularMotor = val;
	}
	virtual void SetFreeChain(bool val) override
	{
		FreeChain = val;
	}
	virtual void SetTargetRotation(const Fmatrix& camera_rotation) override
	{
		TargetRotation.set(camera_rotation);
		TargetRotation.c.set(0.f, 0.f, 0.f);
		HasTargetRotation = true;
	}
	virtual void RotateTarget(const Fmatrix& delta) override
	{
		Fmatrix Rotated;
		Rotated.mul_43(delta, TargetRotation);
		TargetRotation.set(Rotated);
		HasTargetRotation = true;
	}
	virtual void SetTargetAngularVelocity(const Fvector& velocity) override
	{
		AngularTarget.set(velocity);
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
	virtual void Release() override
	{
		if (State == EState::Free || State == EState::Released)
		{
			return;
		}

		State = EState::Released;
		CPHUpdateObject::Deactivate();
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
		Fvector Position;
		Element->GetGlobalPositionDynamic(&Position);
		const float Distance = Position.distance_to(CapturePos);
		if (State == EState::Pulling)
		{
			if (Distance > PullDistance || (Character && Device.dwTimeGlobal - TimeStart > TimeLimit))
			{
				Release();
				return;
			}
			if (Distance < CaptureDistance)
			{
				State = EState::Captured;
			}
		}
		else if (Distance > PullDistance)
		{
			Release();
			return;
		}

		Drive(Step, State == EState::Captured ? (Character ? CaptureForce : g_CameraCaptureMotorForce) : PullForce);
	}

	virtual void PhDataUpdate(float Step) override {}
};

static CPhysicsElement* ElementOfBone(IPhysicsShellHolder* Object, u16 BoneId)
{
	IKinematics* K = Object ? Object->ObjectKinematics() : nullptr;
	CPhysicsShell* Shell = Object ? Object->ObjectPPhysicsShell() : nullptr;
	if (Shell == nullptr)
	{
		return nullptr;
	}
	if (K && BoneId < K->LL_BoneCount())
	{
		if (void* Param = K->LL_GetBoneInstance(BoneId).callback_param())
		{
			return static_cast<CPhysicsElement*>(Param);
		}
	}
	CPhysicsElement* Element = Shell->get_Element(BoneId);
	return Element ? Element : Shell->get_ElementByStoreOrder(0);
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
	CPhysicsElement* Element = ElementOfBone(object, element);
	return Element ? new CB3Capture(ch, object, Element, nullptr, false) : nullptr;
}

IPHCapture* phcapture_create(IPHCharacter* ch, IPhysicsShellHolder* object, u16 element, const Fvector& global_pos, bool free_chain)
{
	CPhysicsElement* Element = ElementOfBone(object, element);
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
