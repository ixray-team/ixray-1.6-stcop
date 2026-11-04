#pragma once

#include <box3d/box3d.h>

#include "PhysicsShell.h"
#include "PHItemList.h"
#include "physics_scripted.h"
#include "../../xrServerEntities/PHSynchronize.h"
#include "../../xrEngine/IPhysicsGeometry.h"

class CPHElement;
class CPHShell;
class IPHCharacter;
class IClimableObject;

// Logs the first call of a function the Box3D backend does not implement yet.
#define B3_NOT_IMPLEMENTED()                                              \
	do                                                                    \
	{                                                                     \
		static bool Logged = false;                                       \
		if (!Logged)                                                      \
		{                                                                 \
			Logged = true;                                                \
			Msg("! [Box3D] not implemented: %s", __FUNCTION__);           \
		}                                                                 \
	} while (false)

// Box3D contact record behind CPHContact. Index 0 is the "first" side.
// A null geom is the level mesh.
struct dContact
{
	Fvector Position;
	Fvector Normal; // from the second side to the first, as in ODE
	float Depth = 0.f;
	CODEGeom* Geom[2] = {nullptr, nullptr};
	u16 Material[2] = {0, 0};
	float Friction = 1.f;
	float Bounce = 0.f;
	float BounceVel = 0.f;
	float ApproachSpeed = -1.f; // pre-solve approach along the normal; negative: use the live body velocities
	u8 Side = 0; // side whose callbacks run; SetFriction and SetBounce edit its shape
	bool FrictionRead = false;
};

// Bits above the game material in b3SurfaceMaterial::userMaterialId: the shape's value
// replaces the friction or restitution mix (CPHContact::SetFriction, SetBounce).
namespace B3MaterialFlag
{
	constexpr u64 Friction = 1ull << 32;
	constexpr u64 Bounce = 1ull << 33;
}

// CPHCollideValidator class bits (its enum is private): every NC bit is its class bit << 1.
namespace B3CollideClass
{
	constexpr CLClassBits Dynamic = 1 << 2;
	constexpr CLClassBits Character = 1 << 4;
	constexpr CLClassBits NCCharacter = 1 << 5;
	constexpr CLClassBits Small = 1 << 6;
	constexpr CLClassBits NCSmall = 1 << 7;
	constexpr CLClassBits RagDoll = 1 << 8;
	constexpr CLClassBits NCRagDoll = 1 << 9;
	constexpr CLClassBits Animated = 1 << 10;
	constexpr CLClassBits NCAnimated = 1 << 11;
	constexpr CLClassBits Classes = Dynamic | Character | Small | RagDoll | Animated;

	inline bool DoCollide(CLClassBits A, CLClassBits B)
	{
		return !((A & (Classes << 1) & ((B & Classes) << 1)) || (B & (Classes << 1) & ((A & Classes) << 1)));
	}
}

// Contact callbacks and links shared by every geom of one element or static shell.
struct SB3GeomOwner
{
	IPhysicsShellHolder* RefObject = nullptr;
	void* CallbackData = nullptr;
	IPHCharacter* Character = nullptr;
	CPHShell* Shell = nullptr;
	IClimableObject* Climable = nullptr;
	ContactCallbackFun* ContactCallback = nullptr;
	xr_vector<ObjectContactCallbackFun*> ObjectCallbacks;
	u16 Material = 0;

	void AddObjectCallback(ObjectContactCallbackFun* Callback);
	void RemoveObjectCallback(ObjectContactCallbackFun* Callback);
};

enum class EB3GeomType : u8
{
	Sphere,
	Box,
	Cylinder
};

// One collision primitive. Box and cylinder become convex hulls.
class CODEGeom : public IPhysicsGeometry
{
public:
	EB3GeomType Type = EB3GeomType::Sphere;
	Fmatrix LocalForm = Fidentity; // in owner space
	Fvector HalfSize = {0.f, 0.f, 0.f}; // box half sizes; sphere: x radius; cylinder: x radius, y half height
	u16 BoneId = u16(-1);
	SB3GeomOwner* Owner = nullptr;
	const Fmatrix* OwnerForm = nullptr; // global form of the owner
	xr_vector<ObjectContactCallbackFun*> ObjectCallbacks;
	b3ShapeId Shape = b3_nullShapeId;

	virtual ~CODEGeom() = default;

	static CODEGeom* MakeSphere(const Fsphere& Sphere);
	static CODEGeom* MakeBox(const Fobb& Box);
	static CODEGeom* MakeCylinder(const Fcylinder& Cylinder);

	void CreateShape(b3BodyId Body, float Density, bool Dynamic, u64 Mask);
	void SetMask(u64 Mask);
	// Box3D pre-solve cannot edit a contact: a callback's surface edit stays on the shape.
	void SetSurface(u64 Flag, float Value, bool Override);
	// Convex proxy at the owner's global Form, shrunk by Inset; returns the point count.
	int MakeProxy(const Fmatrix& Form, float Inset, b3Vec3 (&Points)[B3_MAX_SHAPE_CAST_POINTS], float& ProxyRadius) const;
	void DestroyShape();
	float Volume() const;
	float Radius() const;
	void GetGlobalForm(Fmatrix& Form) const;
	void GetExtensions(const Fvector& Axis, float Center, float& Lo, float& Hi) const;

	virtual void get_Box(Fmatrix& Form, Fvector& Size) const override;
	virtual bool collide_fluids() const override
	{
		return true;
	}
};

// Rigid body with its geoms. Before activation XFORM is in shell space, after it is global.
class CPHElement :
	public CPhysicsElement,
	public CPHSynchronize,
	public cphysics_scripted
{
public:
	CPHShell* Shell = nullptr;
	SB3GeomOwner Owner;
	xr_vector<CODEGeom*> Geoms;
	b3BodyId Body = b3_nullBodyId;
	float Mass = 10.f;
	Fvector MassCenterLocal = {0.f, 0.f, 0.f};
	mutable Fvector MassCenterGlobal = {0.f, 0.f, 0.f};
	Fmatrix PrevForm = Fidentity;
	Fmatrix CurForm = Fidentity;
	// One-step force accumulator (ODE impulse = force over one fixed step), flushed in FlushForces.
	Fvector Force = {0.f, 0.f, 0.f};
	Fvector Torque = {0.f, 0.f, 0.f};
	// Explicit geom masses (rigid child bones); 0 marks a massless geom moved in from another element.
	xr_vector<std::pair<CODEGeom*, float>> GeomMasses;
	float AddedMass = 0.f;
	float InertiaKeep = 1.f;
	b3Matrix3 ExtraInertia = b3Mat3_zero;
	bool HasMassCenter = false;
	float AirLinear = default_k_l;
	float AirAngular = default_k_w;
	float LinearLimit = default_l_limit;
	float AngularLimit = default_w_limit;
	float LinearScale = default_l_scale;
	float AngularScale = default_w_scale;
	bool Active = false;
	bool Fixed = false;
	bool ApplyGravity = true;
	bool Animated = false;
	bool Activating = false;
	bool HasPendingForm = false;
	bool HasAnimTarget = false;
	// Wheels: ODE caps only by the element limit, Box3D also by 0.25*pi per step unless set at creation.
	bool AllowFastRotation = false;
	Fmatrix AnimTarget = Fidentity;
	Fmatrix PendingForm = Fidentity;
	xrCriticalSection PendingLock;
	SAllDDOParams DisableParams;

	CPHElement();
	virtual ~CPHElement() override;

	void CreateBody(const Fmatrix& Form, bool Disable);
	void DestroyBody();
	void PullState(bool StorePrev);
	void PhTune(float Step);
	// As ODE BodyCutForce: the accumulated step force changes velocity by at most the element limits.
	void FlushForces(float Step);
	// Point is global; the torque is taken about the center of mass.
	void AddForce(const Fvector& F, const Fvector* Point = nullptr);
	// Mass from the geoms, moved to the authored center, plus the smoothing and box terms.
	void ApplyMassData();
	float SleepThreshold() const;
	bool IsAwake() const;
	// Bone callback: the first call places the element at the animated bone, later calls drive the bone.
	void BonesCallBack(CBoneInstance* B);
	// Moves the element to a bone pose from any thread; applied before the next step.
	void ToBonePos(const Fmatrix& BoneTransform);
	bool AnimToVel(float dt, float l_limit, float a_limit);
	void ApplyPendingForm();
	// Kinematic target from the shell animator, applied before the next step.
	void SetAnimTarget(const Fmatrix& Form);

	// CPhysicsBase
	virtual void Activate(const Fmatrix& m0, float dt01, const Fmatrix& m2, bool disable = false) override;
	virtual void Activate(const Fmatrix& transform, const Fvector& lin_vel, const Fvector& ang_vel, bool disable = false) override;
	virtual void Activate(bool disable = false, bool not_set_bone_callbacks = false) override;
	virtual void Activate(const Fmatrix& form, bool disable = false) override;
	virtual void InterpolateGlobalTransform(Fmatrix* m) override;
	virtual void InterpolateGlobalPosition(Fvector* v) override;
	virtual void net_Import(NET_Packet& P) override;
	virtual void net_Export(NET_Packet& P) override;
	virtual void GetGlobalPositionDynamic(Fvector* v) override;
	virtual bool isBreakable() override
	{
		return false;
	}
	virtual bool isEnabled() const override;
	virtual bool isActive() const override
	{
		return Active;
	}
	virtual bool isFullActive() const override
	{
		return Active;
	}
	virtual void Deactivate() override;
	virtual void Enable() override;
	virtual void setMass(float M) override;
	virtual void setDensity(float M) override;
	virtual float getMass() override
	{
		return Geoms.empty() ? 0.f : Mass + AddedMass;
	}
	virtual float getDensity() override;
	virtual float getVolume() override;
	virtual void get_Extensions(const Fvector& axis, float center_prg, float& lo_ext, float& hi_ext) const override;
	virtual void applyForce(const Fvector& dir, float val) override;
	virtual void applyForce(float x, float y, float z) override;
	virtual void applyImpulse(const Fvector& dir, float val) override;
	virtual void setTorque(const Fvector& torque) override;
	virtual void setForce(const Fvector& force) override;
	virtual void applyGravityAccel(const Fvector& accel) override;
	virtual void SetAirResistance(float linear = default_k_l, float angular = default_k_w) override;
	virtual void GetAirResistance(float& linear, float& angular) override;
	virtual void set_DynamicLimits(float l_limit = default_l_limit, float w_limit = default_w_limit) override
	{
		LinearLimit = l_limit;
		AngularLimit = w_limit;
	}
	virtual void set_DynamicScales(float l_scale = default_l_scale, float w_scale = default_w_scale) override
	{
		LinearScale = l_scale;
		AngularScale = w_scale;
	}
	void CutVelocity();
	virtual void set_ContactCallback(ContactCallbackFun* callback) override
	{
		Owner.ContactCallback = callback;
	}
	virtual void set_ObjectContactCallback(ObjectContactCallbackFun* callback) override;
	virtual void SetAnimated(bool v) override
	{
		Animated = v;
	}
	virtual void add_ObjectContactCallback(ObjectContactCallbackFun* callback) override
	{
		Owner.AddObjectCallback(callback);
	}
	virtual void remove_ObjectContactCallback(ObjectContactCallbackFun* callback) override
	{
		Owner.RemoveObjectCallback(callback);
	}
	virtual void set_CallbackData(void* cd) override
	{
		Owner.CallbackData = cd;
	}
	virtual void* get_CallbackData() override
	{
		return Owner.CallbackData;
	}
	virtual void set_PhysicsRefObject(IPhysicsShellHolder* ref_object) override
	{
		Owner.RefObject = ref_object;
	}
	virtual void set_LinearVel(const Fvector& velocity) override;
	virtual void set_AngularVel(const Fvector& velocity) override;
	virtual void TransformPosition(const Fmatrix& form, motion_history_state history_state) override;
	virtual void set_ApplyByGravity(bool flag) override;
	virtual bool get_ApplyByGravity() override
	{
		return ApplyGravity;
	}
	virtual void SetMaterial(u16 m) override;
	virtual void SetMaterial(const char* m) override;
	virtual void set_DisableParams(const SAllDDOParams& params) override;
	virtual void SetTransform(const Fmatrix& m0, motion_history_state history_state) override;
#ifdef DEBUG
	virtual void dbg_draw_velocity(float scale, u32 color) override {}
	virtual void dbg_draw_force(float scale, u32 color) override {}
	virtual void dbg_draw_geometry(float scale, u32 color, Flags32 flags = Flags32().assign(0)) const override {}
#endif

	// CPhysicsElement
	virtual CPhysicsShell* PhysicsShell() override;
	virtual IPhysicsShellHolder* PhysicsRefObject() override
	{
		return Owner.RefObject;
	}
	virtual void add_Sphere(const Fsphere& V) override;
	virtual void add_Box(const Fobb& V) override;
	virtual void add_Cylinder(const Fcylinder& V) override;
	virtual void add_Shape(const SBoneShape& shape) override;
	virtual void add_Shape(const SBoneShape& shape, const Fmatrix& offset) override;
	virtual CODEGeom* last_geom() override
	{
		return Geoms.empty() ? nullptr : Geoms.back();
	}
	virtual CODEGeom* geometry(u16 i) override
	{
		return Geoms[i];
	}
	virtual void add_geom(CODEGeom* g) override;
	virtual void remove_geom(CODEGeom* g) override;
	virtual const IPhysicsGeometry* geometry(u16 i) const override
	{
		return Geoms[i];
	}
#ifdef DEBUG
	virtual CPhysicsElement* parent_element() override
	{
		return nullptr;
	}
#endif
	virtual bool has_geoms() override
	{
		return !Geoms.empty();
	}
	virtual void add_Mass(const SBoneShape& shape, const Fmatrix& offset, const Fvector& mass_center, float mass, CPHFracture* fracture = NULL) override;
	virtual void set_ParentElement(CPhysicsElement* p) override {}
	virtual void set_BoxMass(const Fobb& box, float mass) override;
	virtual void setMassMC(float M, const Fvector& mass_center) override;
	virtual void applyImpulseVsMC(const Fvector& pos, const Fvector& dir, float val) override;
	virtual void applyImpulseVsGF(const Fvector& pos, const Fvector& dir, float val) override;
	virtual void applyImpulseTrace(const Fvector& pos, const Fvector& dir, float val, const u16 id) override;
	virtual void setDensityMC(float M, const Fvector& mass_center) override;
	virtual void set_local_mass_center(const Fvector& mc) override
	{
		MassCenterLocal.set(mc);
		HasMassCenter = true;
	}
	virtual void setQuaternion(const Fquaternion& quaternion) override;
	virtual u16 setGeomFracturable(CPHFracture& fracture) override;
	virtual CPHFracture& Fracture(u16 num) override;
	virtual u16 numberOfGeoms() const override
	{
		return u16(Geoms.size());
	}
	virtual const Fvector& mass_Center() const override;
	virtual const Fvector& local_mass_Center() override
	{
		return MassCenterLocal;
	}
	virtual float getRadius() override;
	virtual void GetGlobalTransformDynamic(Fmatrix* m) const override;
	virtual void get_MaxAreaDir(Fvector& dir) override;
	virtual ObjectContactCallbackFun* get_ObjectContactCallback() override
	{
		return Owner.ObjectCallbacks.empty() ? nullptr : Owner.ObjectCallbacks.front();
	}
	virtual void Fix() override;
	virtual void ReleaseFixed() override;
	virtual bool isFixed() override
	{
		return Fixed;
	}
	virtual void GetPointVel(Fvector& res_vel, const Fvector& point) const override;
	virtual void get_LinearVel(Fvector& velocity) const override;
	virtual void get_AngularVel(Fvector& velocity) const override;

	// CPHSynchronize
	virtual void get_State(SPHNetState& state) override;
	virtual void set_State(const SPHNetState& state) override;
	virtual void cv2obj_Xfrom(const Fquaternion& q, const Fvector& pos, Fmatrix& xform) override;
	virtual void cv2bone_Xfrom(const Fquaternion& q, const Fvector& pos, Fmatrix& xform) override;

	virtual iphysics_scripted& get_scripted() override
	{
		return *this;
	}
};

// Joint description. Simulation of joints comes with skeleton shells.
class CPHJoint : public CPhysicsJoint, public cphysics_scripted
{
	struct SAxis
	{
		Fvector Direction = {0.f, 0.f, 1.f};
		eVs Vs = vs_first;
		float Low = -M_PI;
		float High = M_PI;
		float Force = 0.f;
		float Velocity = 0.f;
		float Spring = 1.f;
		float Damping = 1.f;
	};

	CPhysicsElement* First = nullptr;
	CPhysicsElement* Second = nullptr;
	CPhysicsJoint** BackRef = nullptr;
	Fvector Anchor = {0.f, 0.f, 0.f};
	eVs AnchorVs = vs_global;
	SAxis Axes[3];
	u16 BoneId = u16(-1);
	float Spring = 1.f;
	float Damping = 1.f;
	b3JointId Joint = b3_nullJointId;
	// Hinge2 steering is a spring to an angle in Box3D, while ODE drives the axis with a
	// velocity motor. The target angle is advanced here at Axes[0].Velocity, so the
	// authored steering_speed is kept and the spring only tracks it.
	float SteerTarget = 0.f;
	// The axis that carries the spherical twist limit; chosen once, when the native
	// joint is built (the frame direction depends on it).
	u8 FullControlTwist = 0;
	// A full_control joint with one dominant axis is built as a revolute joint: a hinge
	// is well conditioned, while the spherical swing limits degenerate near the rest pose.
	bool FullControlAsHinge = false;
	// First element had no body and the joint hangs on the ground body (ODE body_for_joint(first) == 0).
	bool FirstIsGround = false;
	bool FirstIsReversed() const;
	bool Breakable = false;
	float ForceThreshold = FLT_MAX;
	float TorqueThreshold = FLT_MAX;

	void GlobalAnchor(Fvector& Result) const;
	void GlobalAxis(int Num, Fvector& Result) const;
	void AxisRanges(float Low[3], float High[3], bool Creation) const;
	void CreateNative();
	void DestroyNative();
	void UpdateMotor(bool Wake = true);
	void UpdateLimits();

	// The three Euler axes of a full_control joint: 0 and 2 are explicit, 1 is their
	// cross product, exactly as the ODE AMotor computes it.
	void FullControlAxes(Fvector Dir[3]) const;

public:
	CPHShell* OwnerShell = nullptr;
	bool Broken = false;
	// A rigid breakable bone (ODE CPHFracture): welded to its parent, split like a broken joint.
	bool Weld = false;

	CPHJoint(enumType Type, CPhysicsElement* InFirst, CPhysicsElement* InSecond);
	virtual ~CPHJoint() override;

	virtual u16 BoneID() override
	{
		return BoneId;
	}
	virtual void SetBoneID(u16 bone_id) override
	{
		BoneId = bone_id;
	}
	virtual CPhysicsElement* PFirst_element() override
	{
		return First;
	}
	virtual CPhysicsElement* PSecond_element() override
	{
		return Second;
	}
	virtual u16 GetAxesNumber() override;
	b3JointId NativeId() const
	{
		return Joint;
	}
	virtual void Activate() override;
	virtual void Create() override;
	virtual void RunSimulation() override {}
	virtual void Deactivate() override;
	// Per-step update driven by the owning shell (the hinge2 steering rate).
	void PhTune(float Step);
	virtual void SetBackRef(CPhysicsJoint** j) override
	{
		BackRef = j;
	}
	virtual void SetAnchor(const Fvector& position) override;
	virtual void SetAxisSDfactors(float spring_factor, float damping_factor, int axis_num) override;
	virtual void SetJointSDfactors(float spring_factor, float damping_factor) override;
	virtual void SetJointFudgefactorActive(float factor) override {}
	virtual void SetAnchorVsFirstElement(const Fvector& position) override;
	virtual void SetAnchorVsSecondElement(const Fvector& position) override;
	virtual void SetAxisDir(const Fvector& orientation, const int axis_num) override;
	virtual void SetAxisDirVsFirstElement(const Fvector& orientation, const int axis_num) override;
	virtual void SetAxisDirVsSecondElement(const Fvector& orientation, const int axis_num) override;
	virtual void SetAnchor(const float x, const float y, const float z) override;
	virtual void SetAnchorVsFirstElement(const float x, const float y, const float z) override;
	virtual void SetAnchorVsSecondElement(const float x, const float y, const float z) override;
	virtual void SetAxisDir(const float x, const float y, const float z, const int axis_num) override;
	virtual void SetAxisDirVsFirstElement(const float x, const float y, const float z, const int axis_num) override;
	virtual void SetAxisDirVsSecondElement(const float x, const float y, const float z, const int axis_num) override;
	virtual void SetLimits(const float low, const float high, const int axis_num) override;
	virtual void SetLimitsVsFirstElement(const float low, const float high, const int axis_num) override;
	virtual void SetLimitsVsSecondElement(const float low, const float high, const int axis_num) override;
	virtual void SetHiLimitDynamic(int axis_num, float limit) override;
	virtual void SetLoLimitDynamic(int axis_num, float limit) override;
	virtual void SetBreakable(float force, float torque) override;
	void SetFracture(float Force, float Torque);
	void SetResistance(float Force);
	virtual CPHJointDestroyInfo* JointDestroyInfo() override
	{
		return nullptr;
	}
	virtual bool isBreakable() override
	{
		return Breakable;
	}
	virtual void SetForceAndVelocity(const float force, const float velocity = 0.f, const int axis_num = -1) override;
	virtual void GetMaxForceAndVelocity(float& force, float& velocity, int axis_num) override;
	virtual float GetAxisAngle(int axis_num) override;
	virtual float GetAxisAngleRate(int axis_num) override;
	virtual void GetAxisSDfactors(float& spring_factor, float& damping_factor, int axis_num) override;
	virtual void GetJointSDfactors(float& spring_factor, float& damping_factor) override;
	virtual void GetLimits(float& lo_limit, float& hi_limit, int axis_num) override;
	virtual void GetAxisDir(int num, Fvector& axis, eVs& vs) override;
	virtual void GetAxisDirDynamic(int num, Fvector& axis) override;
	virtual void GetAnchorDynamic(Fvector& anchor) override;
	virtual bool IsWheelJoint() override
	{
		return eType == hinge2;
	}
	virtual bool IsHingeJoint() override
	{
		return eType == hinge;
	}
	virtual void SetForce(const float force, const int axis_num = -1) override;
	virtual void SetVelocity(const float velocity = 0.f, const int axis_num = -1) override;

	virtual iphysics_scripted& get_scripted() override
	{
		return *this;
	}
};

class CPHShell :
	public CPhysicsShell,
	public cphysics_scripted
{
	DECLARE_PHLIST_ITEM(CPHShell)

public:
	ELEMENT_STORAGE ElementsList;
	JOINT_STORAGE JointsList;
	Fmatrix ObjectInRootForm = Fidentity;
	xr_hash_map<u16, CPHElement*> BoneElements;
	u64 CollideMask = ~0ull;
	CGID CLGroup = 0;
	CLBits CollideBits = 0;
	_flags<CLClassBits> CollideClassBits;
	bool Active = false;
	bool InWorld = false;
	bool CollisionDisabled = false;
	bool SelfCollision = false;
	bool PreferExactIntegration = false;
	bool BreakingBlocked = false;
	bool CallbacksEnabled = true;
	bool SkipBoneCallbacks = false;
	bool SplitterSuspended = false;
	// ODE m_active_count: below zero the next InterpolateGlobalTransform puts the object to processing sleep.
	int ActiveCount = 0;
	bool WasEnabled = true;
	float SmoothInertia = 0.f;
	u32 SelfContacts = 0;
	CPhysicsShellAnimator* Animator = nullptr;
	// Steps while the world is frozen (ODE: the shell is back in the step list).
	bool Thawed = false;
	bool RemoveCharCollOnDisable = false;
	bool TracedGeoms = false;
	bool GeomTrace = true;
	bool SelfCollisionDirty = false;
	// Element pairs already overlapping when self collision was enabled never collide (ODE BuildSelfCollisionPairs).
	xr_vector<std::pair<const SB3GeomOwner*, const SB3GeomOwner*>> SelfCollisionExcluded;

	CPHShell();
	virtual ~CPHShell() override;

	void PhTune(float Step);
	void PhDataUpdate(float Step);
	CPHElement* Root() const
	{
		return ElementsList.empty() ? nullptr : ElementsList.front();
	}
	void SetGeomFilter();
	// Box3D runs the custom filter only when a pair is created: recreate the pairs after its inputs change.
	void ResetPairs();
	void SetClassBit(CLClassBits Bit);
	void SetBullets();
	void BuildSelfCollisionPairs();
	bool SelfCollides(const SB3GeomOwner* A, const SB3GeomOwner* B) const;
	bool IsBullet() const
	{
		return TracedGeoms && GeomTrace;
	}
	void RemoveJoint(CPHJoint* Joint);
	void AddElementRecursive(CPhysicsElement* RootElement, u16 Id, Fmatrix GlobalParent, BONE_P_MAP* GettingMap);
	void FinishActivate(bool Disable);
	bool OwnsBody(b3BodyId Id) const
	{
		return std::find(ElementsList.begin(), ElementsList.end(), b3Body_GetUserData(Id)) != ElementsList.end();
	}

	// CPhysicsBase
	virtual void Activate(const Fmatrix& m0, float dt01, const Fmatrix& m2, bool disable = false) override;
	virtual void Activate(const Fmatrix& transform, const Fvector& lin_vel, const Fvector& ang_vel, bool disable = false) override;
	virtual void Activate(bool disable = false, bool not_set_bone_callbacks = false) override;
	virtual void Activate(const Fmatrix& form, bool disable = false) override;
	virtual void InterpolateGlobalTransform(Fmatrix* m) override;
	virtual void InterpolateGlobalPosition(Fvector* v) override;
	virtual void net_Import(NET_Packet& P) override;
	virtual void net_Export(NET_Packet& P) override;
	virtual void GetGlobalPositionDynamic(Fvector* v) override;
	virtual bool isBreakable() override;
	virtual bool isEnabled() const override;
	virtual bool isActive() const override
	{
		return Active;
	}
	virtual bool isFullActive() const override
	{
		return Active;
	}
	virtual void Deactivate() override;
	virtual void Enable() override;
	virtual void setMass(float M) override;
	virtual void setDensity(float M) override;
	virtual float getMass() override;
	virtual float getDensity() override;
	virtual float getVolume() override;
	virtual void get_Extensions(const Fvector& axis, float center_prg, float& lo_ext, float& hi_ext) const override;
	virtual void applyForce(const Fvector& dir, float val) override;
	virtual void applyForce(float x, float y, float z) override;
	virtual void applyImpulse(const Fvector& dir, float val) override;
	virtual void setTorque(const Fvector& torque) override;
	virtual void setForce(const Fvector& force) override;
	virtual void applyGravityAccel(const Fvector& accel) override;
	virtual void SetAirResistance(float linear = default_k_l, float angular = default_k_w) override;
	virtual void GetAirResistance(float& linear, float& angular) override;
	virtual void set_DynamicLimits(float l_limit = default_l_limit, float w_limit = default_w_limit) override;
	virtual void set_DynamicScales(float l_scale = default_l_scale, float w_scale = default_w_scale) override;
	virtual void set_ContactCallback(ContactCallbackFun* callback) override;
	virtual void set_ObjectContactCallback(ObjectContactCallbackFun* callback) override;
	virtual void SetAnimated(bool v) override;
	virtual void add_ObjectContactCallback(ObjectContactCallbackFun* callback) override;
	virtual void remove_ObjectContactCallback(ObjectContactCallbackFun* callback) override;
	virtual void set_CallbackData(void* cd) override;
	virtual void* get_CallbackData() override;
	virtual void set_PhysicsRefObject(IPhysicsShellHolder* ref_object) override;
	virtual void set_LinearVel(const Fvector& velocity) override;
	virtual void set_AngularVel(const Fvector& velocity) override;
	virtual void TransformPosition(const Fmatrix& form, motion_history_state history_state) override;
	virtual void set_ApplyByGravity(bool flag) override;
	virtual bool get_ApplyByGravity() override;
	virtual void SetMaterial(u16 m) override;
	virtual void SetMaterial(const char* m) override;
	virtual void set_DisableParams(const SAllDDOParams& params) override;
	virtual void SetTransform(const Fmatrix& m0, motion_history_state history_state) override;
#ifdef DEBUG
	virtual void dbg_draw_velocity(float scale, u32 color) override {}
	virtual void dbg_draw_force(float scale, u32 color) override {}
	virtual void dbg_draw_geometry(float scale, u32 color, Flags32 flags = Flags32().assign(0)) const override {}
#endif

	// CPhysicsShell
	virtual void GetGlobalTransformDynamic(Fmatrix* m) override;
	virtual CPhysicsShellAnimator* PPhysicsShellAnimator() override
	{
		return Animator;
	}
	virtual void set_JointResistance(float force) override
	{
		for (CPHJoint* Joint : JointsList)
		{
			Joint->SetResistance(force);
		}
	}
	virtual void add_Element(CPhysicsElement* E) override;
	virtual void add_Joint(CPhysicsJoint* E) override;
	virtual CPHIsland* PIsland() override
	{
		return nullptr;
	}
	virtual const CGID& GetCLGroup() const override
	{
		return CLGroup;
	}
	virtual void RegisterToCLGroup(CGID g) override
	{
		CLGroup = g;
		ResetPairs();
	}
	virtual bool IsGroupObject() override
	{
		return CLGroup != 0;
	}
	virtual void SetIgnoreStatic() override;
	virtual void SetIgnoreDynamic() override;
	virtual void SetRagDoll() override;
	virtual void SetIgnoreRagDoll() override;
	virtual void SetSelfCollision(bool Enabled) override
	{
		SelfCollisionDirty = SelfCollisionDirty || SelfCollision != Enabled;
		SelfCollision = Enabled;
	}
	virtual bool IsSelfCollision() const override
	{
		return SelfCollision;
	}
	virtual u32 LastSelfCollisionContacts() const override
	{
		return SelfContacts;
	}
	virtual const CLBits& collide_bits() const override
	{
		return CollideBits;
	}
	virtual const _flags<CLClassBits>& collide_class_bits() const override
	{
		return CollideClassBits;
	}
	virtual void CreateShellAnimator(CInifile const* ini, const char* section) override;
	virtual void SetIgnoreAnimated() override;
	virtual void AnimatorOnFrame(bool calculate_bones = true) override;
	virtual void SetSmall() override;
	virtual void SetIgnoreSmall() override;
	virtual bool isFractured() override;
	virtual CPHShellSplitterHolder* SplitterHolder() override
	{
		return nullptr;
	}
	virtual void SplitterHolderActivate() override
	{
		SplitterSuspended = false;
	}
	virtual void SplitterHolderDeactivate() override
	{
		SplitterSuspended = true;
	}
	virtual void SplitProcess(PHSHELL_PAIR_VECTOR& out_shels) override;
	virtual void BlockBreaking() override
	{
		BreakingBlocked = true;
	}
	virtual void UnblockBreaking() override
	{
		BreakingBlocked = false;
	}
	virtual bool IsBreakingBlocked() override
	{
		return BreakingBlocked || SplitterSuspended;
	}
	virtual void applyImpulseTrace(const Fvector& pos, const Fvector& dir, float val) override;
	virtual void applyImpulseTrace(const Fvector& pos, const Fvector& dir, float val, const u16 id) override;
	virtual void applyHit(const Fvector& pos, const Fvector& dir, float val, const u16 id, ALife::EHitType hit_type) override;
	virtual BoneCallbackFun* GetBonesCallback() override;
	virtual BoneCallbackFun* GetStaticObjectBonesCallback() override;
	virtual void Update() override;
	virtual void get_LinearVel(Fvector& velocity) const override;
	virtual void get_AngularVel(Fvector& velocity) const override;
	virtual void setMass1(float M) override;
	virtual void SmoothElementsInertia(float k) override;
	virtual void MassAddBox(float mass, const Fvector& full_size) override;
	virtual ELEMENT_STORAGE& Elements() override
	{
		return ElementsList;
	}
	virtual CPhysicsElement* get_Element(u16 bone_id) override;
	virtual CPhysicsElement* get_Element(const shared_str& bone_name) override;
	virtual CPhysicsElement* get_Element(const char* bone_name) override;
	virtual CPhysicsElement* get_ElementByStoreOrder(u16 num) override;
	virtual const CPhysicsElement* get_ElementByStoreOrder(u16 num) const override;
	virtual CPhysicsElement* get_PhysicsParrentElement(u16 bone_id) override;
	virtual u16 get_ElementsNumber() const override
	{
		return u16(ElementsList.size());
	}
	virtual CPHSynchronize* get_ElementSync(u16 element) override;
	virtual CPhysicsJoint* get_Joint(u16 bone_id) override;
	virtual CPhysicsJoint* get_Joint(const shared_str& bone_name) override;
	virtual CPhysicsJoint* get_Joint(const char* bone_name) override;
	virtual CPhysicsJoint* get_JointByStoreOrder(u16 num) override;
	virtual u16 get_JointsNumber() override
	{
		return u16(JointsList.size());
	}
	virtual CODEGeom* get_GeomByID(u16 bone_id) override;
	virtual void Freeze() override
	{
		Thawed = false;
	}
	virtual void UnFreeze() override
	{
		Thawed = true;
	}
	virtual void NetInterpolationModeON() override {}
	virtual void NetInterpolationModeOFF() override {}
	virtual void Disable() override;
	virtual void DisableCollision() override;
	virtual void EnableCollision() override;
	virtual bool IsCollisionDisabled() const override
	{
		return CollisionDisabled;
	}
	virtual void SetRemoveCharacterCollLADisable() override
	{
		RemoveCharCollOnDisable = true;
	}
	virtual void DisableCharacterCollision() override;
	virtual void PureStep(float step = fixed_step) override;
	virtual void SetGlTransformDynamic(const Fmatrix& form) override;
	virtual void CollideAll() override;
	virtual CPhysicsElement* NearestToPoint(const Fvector& point, NearestToPointCallback* cb = 0) override;
	virtual void build_FromKinematics(IKinematics* K, BONE_P_MAP* p_geting_map = NULL) override;
	virtual void preBuild_FromKinematics(IKinematics* K, BONE_P_MAP* p_geting_map = NULL) override;
	virtual void Build(bool disable = false) override;
	virtual void ActivatingBonePoses(IKinematics& K) override;
	// Traced geoms are Box3D bullets: continuous collision against dynamic bodies too. Per body, not per geom.
	virtual void AddTracedGeom(u16 element = 0, u16 geom = 0) override
	{
		SetAllGeomTraced();
	}
	virtual void SetAllGeomTraced() override
	{
		TracedGeoms = GeomTrace = true;
		SetBullets();
	}
	virtual void ClearTracedGeoms() override
	{
		TracedGeoms = false;
		SetBullets();
	}
	virtual void DisableGeomTrace() override
	{
		GeomTrace = false;
		SetBullets();
	}
	virtual void EnableGeomTrace() override
	{
		GeomTrace = true;
		SetBullets();
	}
	virtual bool HasTracedGeoms() override
	{
		return TracedGeoms;
	}
	virtual void RunSimulation(bool place_current_forms = true) override;
	virtual void UpdateRoot() override;
	virtual void ZeroCallbacks() override;
	virtual void ResetCallbacks(u16 id, VisMask& mask) override;
	virtual void SetCallbacks() override;
	virtual void EnabledCallbacks(bool val) override;
	virtual void ToAnimBonesPositions(motion_history_state history_state) override;
	virtual bool AnimToVelocityState(float dt, float l_limit, float a_limit) override;
	virtual void SetBonesCallbacksOverwrite(bool v) override;
	virtual Fmatrix& ObjectInRoot() override
	{
		return ObjectInRootForm;
	}
	virtual void ObjectToRootForm(const Fmatrix& form) override;
	virtual void SetPrefereExactIntegration() override
	{
		PreferExactIntegration = true;
	}
	virtual void UnSetPrefereExactIntegration() override
	{
		PreferExactIntegration = false;
	}
	virtual bool IsPrefereExactIntegration() const override
	{
		return PreferExactIntegration;
	}
	virtual void vis_update_activate() override;

	virtual iphysics_scripted& get_scripted() override
	{
		return *this;
	}
};

// Drives the controlled elements as kinematic bodies along the object animation.
class CPhysicsShellAnimator
{
	CPHShell* Shell;
	Fmatrix StartXFORM;
	xr_vector<CPHElement*> ControlledElements;

public:
	CPhysicsShellAnimator(CPHShell* InShell, CInifile const* Ini, const char* Section);
	~CPhysicsShellAnimator();
	void OnFrame(bool CalculateBones);
};

DEFINE_PHITEM_LIST(CPHShell, PH_SHELL_STORAGE, PH_SHELL_I)
