#include "StdAfx.h"
#include "PHMovementControl.h"

#include "../xrCore/Collision/cl_intersect.h"
#include "../xrEngine/AI/alife_space.h"
#include "../xrPhysics/IPHCharacter.h"
#include "../xrPhysics/IPHCapture.h"
#include "../xrPhysics/IPhysicsShellHolder.h"
#include "../xrPhysics/ElevatorState.h"
#include "../xrPhysics/IColisiondamageInfo.h"
#include "../xrPhysics/phvalide.h"
#include "../xrPhysics/PhysicsShell.h"
#include "../xrPhysics/IPHWorld.h"
#include "detail_path_manager.h"
#include "../xrEngine/GameMtlLib.h"
#include "../xrEngine/xr_object.h"
#include "CaptureBoneCallback.h"
#include "Level.h"
#include "PhysicsShellHolder.h"
#include "Actor.h"
#ifdef DEBUG
#include "PHDebug.h"
#endif

#include "../Include/xrRender/Kinematics.h"
#include "../xrScripts/script_callback_ex.h"

#define GROUND_FRICTION	10.0f
#define AIR_FRICTION	0.01f
#define WALL_FRICTION	3.0f
//#define AIR_RESIST		0.001f

#define def_X_SIZE_2	0.35f
#define def_Y_SIZE_2	0.8f
#define def_Z_SIZE_2	0.35f

const u64 after_creation_collision_hit_block_steps_number=100;

// Car slide direction stability is configured per car in vehicles.ltx.
// How long the body normal/axis stays authoritative after the last body
// contact. If only a wheel touches for longer, the wheel's face becomes the
// slide source instead of keeping a stale body face forever.
constexpr u32 CarBodyContactAuthorityMs = 150;

void CPHMovementControl::ResetCarSlideDirection()
{
	m_car_slide_direction.set(0.f, 0.f, 0.f);
	m_car_slide_direction_valid = false;
	m_car_slide_direction_time = 0;
	m_car_slide_direction_expire = 0;
	m_car_slide_opposite_time = 0;
	m_car_face_switch_time = 0;
}

CPHMovementControl::CPHMovementControl(CObject* parent)
{
	pObject = parent;

#ifdef DEBUG
	if (debug_output().ph_dbg_draw_mask1().test(ph_m1_DbgTrackObject) && (!!pObject->cName()) && _stricmp(debug_output().PH_DBG_ObjectTrackName(), *pObject->cName()) == 0)
	{
		Msg("CPHMovementControl::CPHMovementControl %s (constructor) %f,%f,%pObjectf", debug_output().PH_DBG_ObjectTrackName(), pObject->Position().x, pObject->Position().y, pObject->Position().z);
	}
#endif

	m_material = 0;
	m_capture = nullptr;
	b_exect_position = true;
	m_start_index = 0;
	eOldEnvironment = peInAir;
	eEnvironment = peInAir;
	aabb.set(-def_X_SIZE_2, 0, -def_Z_SIZE_2, def_X_SIZE_2, def_Y_SIZE_2 * 2, def_Z_SIZE_2);

	fMass = 100;
	fMinCrashSpeed = 12.0f;
	fMaxCrashSpeed = 25.0f;
	vVelocity.set(0, 0, 0);
	vPosition.set(0, 0, 0);
	vExternalImpulse.set(0, 0, 0);
	bExernalImpulse = false;
	m_car_contact.normal.set(0.f, 0.f, 1.f);
	m_car_contact.car_velocity.set(0.f, 0.f, 0.f);
	m_car_contact.slide_axis.set(0.f, 0.f, 0.f);
	m_car_contact.outward_axis.set(0.f, 0.f, 0.f);
	m_car_contact.separation_bias = 0.f;
	m_car_contact.ram_scale = 1.f;
	m_car_contact.mutant_push_speed = 0.f;
	m_car_contact.min_car_speed = 0.f;
	m_car_contact.max_push_speed = 0.f;
	m_car_contact.slide_scale = 0.f;
	m_car_contact.slide_speed = 0.f;
	m_car_contact.slide_lock_ms = 400.f;
	m_car_contact.slide_flip = 0.5f;
	m_car_contact.slide_confirm_ms = 150.f;
	m_car_contact.slide_memory_ms = 600.f;
	m_car_contact.side_bias = 0.f;
	m_car_contact.start_push_speed = 0.f;
	m_car_contact.body_contact = false;
	m_car_contact_end = 0;
	m_car_contact_active = false;
	m_car_body_contact_time = 0;
	m_car_slide_speed = 0.f;
	ResetCarSlideDirection();

	fLastMotionMag = 1.f;
	SetPathDir(Fvector().set(0, 0, 1));

	bIsAffectedByGravity = true;
	fActualVelocity = 0;
	m_fGroundDelayFactor = 1.f;
	gcontact_HealthLost = 0;

	fContactSpeed = 0.f;
	fAirControlParam = 0.f;
	m_character = nullptr;
	m_dwCurBox = 0xffffffff;
	m_lastSpeedCrashDamageTime = 0;
	fCollisionDamageFactor = 1.f;
	in_dead_area_count = 0;
	bNonInteractiveMode = false;
	block_damage_step_end = u64(-1);
}

CPHMovementControl::~CPHMovementControl(void)
{
	if (m_character)
		m_character->Destroy();

	DeleteCharacterObject();
	phcapture_destroy(m_capture);
}

static ALife::EHitType	 DefineCollisionHitType(u16 material_idx)
{
	if (GMLib.GetMaterialByIdx(material_idx)->Flags.test(SGameMtl::flInjurious) && IsGameTypeSingle())
		return ALife::eHitTypeRadiation;

	return ALife::eHitTypeStrike;
}

void CPHMovementControl::AddControlVel	(const Fvector& vel)
{
	vExternalImpulse.add(vel);
	bExernalImpulse=true;
}

void CPHMovementControl::ApplyImpulse(const Fvector& dir,const float P)
{
	VERIFY( m_character );
	if(fis_zero(P))
		return;

	Fvector force;
	force.set(dir);
	force.mul(P/fixed_step);
	
	AddControlVel(force);
 	m_character->ApplyImpulse(dir,P);
}

void CPHMovementControl::SetCarContact(const Fvector& normal, const Fvector& car_velocity,
	float separation_bias, float ram_scale, float mutant_push_speed,
	float min_car_speed, float max_push_speed, const Fvector& slide_axis,
	const Fvector& outward_axis, float slide_scale, float slide_speed,
	float side_bias, float start_push_speed, float slide_lock_ms, float slide_flip,
	float slide_confirm_ms, float slide_memory_ms,
	bool body_contact, u32 duration_ms)
{
	if (!m_character || !m_character->Exists() || m_character->IsActorCharacter() || duration_ms == 0)
		return;

	const bool was_active = m_car_contact_active;

	// A body contact is the authoritative slide source. Wheel contacts keep
	// the physical push alive, but must not replace the body normal/axis with
	// a rotating wheel normal. This priority expires: if only a wheel touches
	// for a while, its face becomes the source instead of a stale body face.
	const bool body_authoritative =
		was_active && m_car_contact.body_contact && !body_contact &&
		Device.dwTimeGlobal < m_car_body_contact_time + CarBodyContactAuthorityMs;

	if (body_authoritative)
	{
		Fvector wheel_outward = outward_axis;
		wheel_outward.y = 0.f;
		if (wheel_outward.magnitude() > EPS_L)
		{
			wheel_outward.normalize_safe();
			m_car_contact.outward_axis = wheel_outward;
		}

		m_car_contact_end = Device.dwTimeGlobal + duration_ms;
		m_character->SetExternalPush(true);
		return;
	}

	// The body has not been touched for a while. Let the current wheel contact
	// replace the stale body face instead of keeping its expired lock.
	if (was_active && m_car_contact.body_contact && !body_contact)
		m_car_face_switch_time = 0;

	if (!was_active)
		m_car_slide_speed = 0.f;

	Fvector contact_normal = normal;
	contact_normal.y = 0.f;
	if (contact_normal.magnitude() < EPS_L)
		return;
	contact_normal.normalize_safe();

	Fvector contact_slide = slide_axis;
	contact_slide.y = 0.f;
	if (contact_slide.magnitude() > EPS_L)
		contact_slide.normalize_safe();
	else
		contact_slide.set(0.f, 0.f, 0.f);

	Fvector contact_outward = outward_axis;
	contact_outward.y = 0.f;
	if (contact_outward.magnitude() > EPS_L)
		contact_outward.normalize_safe();
	else
		contact_outward = contact_normal;

	// Keep the previous face while the contact is near a corner. Switch only
	// after a short lock and when the contact normal clearly belongs to the
	// new face, otherwise alternating contact points make the mutant jitter.
	// The outward axis is kept together with the slide axis so that escape
	// never judges the mutant against a different face than the one it slides
	// along (body/bumper seams are the main case).
	bool keep_previous_face = false;
	if (was_active && m_car_contact.slide_axis.magnitude() > EPS_L &&
		contact_slide.magnitude() > EPS_L)
	{
		const float alignment = std::fabs(m_car_contact.slide_axis.dotproduct(contact_slide));
		if (alignment < 0.5f)
		{
			keep_previous_face = Device.dwTimeGlobal < m_car_face_switch_time;
			if (!keep_previous_face)
			{
				Fvector candidate_face;
				candidate_face.crossproduct(contact_slide, Fvector().set(0.f, 1.f, 0.f));
				if (candidate_face.magnitude() > EPS_L)
					candidate_face.normalize_safe();

				if (std::fabs(contact_normal.dotproduct(candidate_face)) < 0.6f)
					keep_previous_face = true;
			}

			if (!keep_previous_face)
				m_car_face_switch_time = Device.dwTimeGlobal + 250;
		}
	}

	if (keep_previous_face)
	{
		contact_slide = m_car_contact.slide_axis;
		if (m_car_contact.outward_axis.magnitude() > EPS_L)
			contact_outward = m_car_contact.outward_axis;
	}

	// Keep the chosen slide side across short contact losses and body/wheel
	// transitions. It is dropped only when the slide axis changed (another
	// car face) or the direction memory expired.
	if (m_car_slide_direction_valid)
	{
		Fvector stored_direction = m_car_slide_direction;
		stored_direction.y = 0.f;

		const bool aligned = stored_direction.magnitude() > EPS_L &&
			contact_slide.magnitude() > EPS_L &&
			std::fabs(stored_direction.dotproduct(contact_slide)) >= 0.5f;
		const bool expired = !was_active &&
			Device.dwTimeGlobal >= m_car_slide_direction_expire;

		if (!aligned || expired)
			ResetCarSlideDirection();
	}

	if (m_car_slide_direction_valid)
		m_car_slide_direction_expire = 0;

	if (!was_active)
	{
		m_car_slide_opposite_time = 0;
		m_car_face_switch_time = Device.dwTimeGlobal + 250;
	}

	m_car_contact.normal = contact_normal;
	m_car_contact.car_velocity = car_velocity;
	m_car_contact.car_velocity.y = 0.f;
	m_car_contact.slide_axis = contact_slide;
	m_car_contact.outward_axis = contact_outward;
	m_car_contact.separation_bias = separation_bias;
	m_car_contact.ram_scale = ram_scale;
	m_car_contact.mutant_push_speed = mutant_push_speed;
	m_car_contact.min_car_speed = min_car_speed;
	m_car_contact.max_push_speed = max_push_speed;
	m_car_contact.slide_scale = slide_scale;
	if (m_car_contact.slide_scale < 0.f)
		m_car_contact.slide_scale = 0.f;
	else if (m_car_contact.slide_scale > 1.f)
		m_car_contact.slide_scale = 1.f;
	m_car_contact.slide_speed = slide_speed;
	if (m_car_contact.slide_speed < 0.f)
		m_car_contact.slide_speed = 0.f;
	else if (m_car_contact.slide_speed > 50.f)
		m_car_contact.slide_speed = 50.f;
	m_car_contact.slide_lock_ms = slide_lock_ms;
	clamp(m_car_contact.slide_lock_ms, 0.f, 5000.f);
	m_car_contact.slide_flip = slide_flip;
	clamp(m_car_contact.slide_flip, 0.f, 1.f);
	m_car_contact.slide_confirm_ms = slide_confirm_ms;
	clamp(m_car_contact.slide_confirm_ms, 0.f, 2000.f);
	m_car_contact.slide_memory_ms = slide_memory_ms;
	clamp(m_car_contact.slide_memory_ms, 0.f, 5000.f);
	m_car_contact.side_bias = side_bias;
	m_car_contact.start_push_speed = start_push_speed;
	clamp(m_car_contact.start_push_speed, 0.f, 5.f);
	m_car_contact.body_contact = body_contact;
	m_car_contact_end = Device.dwTimeGlobal + duration_ms;
	m_car_contact_active = true;

	if (body_contact)
		m_car_body_contact_time = Device.dwTimeGlobal;

	m_character->SetExternalPush(true);
}

void CPHMovementControl::UpdateCarContact()
{
	if (!m_car_contact_active)
		return;

	if (!m_character || !m_character->Exists() || Device.dwTimeGlobal >= m_car_contact_end)
	{
		const bool character_gone = !m_character || !m_character->Exists();

		m_car_contact_active = false;
		m_car_contact_end = 0;
		m_car_body_contact_time = 0;
		m_car_slide_speed = 0.f;
		m_car_contact.body_contact = false;
		m_car_face_switch_time = 0;
		m_car_slide_opposite_time = 0;

		if (character_gone)
			ResetCarSlideDirection();
		else
			m_car_slide_direction_expire = Device.dwTimeGlobal +
				static_cast<u32>(std::max(0.f, m_car_contact.slide_memory_ms));

		if (m_character)
			m_character->SetExternalPush(false);
	}
}

void CPHMovementControl::ValidateCarContact()
{
	if (!m_character)
		return;

	// The normal movement update clears the contact through UpdateCarContact().
	// A stuck AI/animation state can skip that update, leaving external push
	// enabled forever. This watchdog runs from the creature's client update and
	// guarantees that an expired contact can never keep external push alive.
	if (m_car_contact_active && m_character->Exists() &&
		Device.dwTimeGlobal < m_car_contact_end)
		return;

	if (m_car_contact_active)
		UpdateCarContact();

	m_character->SetExternalPush(false);
}

bool CPHMovementControl::IsCarContactActive() const
{
	return m_car_contact_active
		&& m_character
		&& m_character->Exists()
		&& Device.dwTimeGlobal < m_car_contact_end;
}

Fvector CPHMovementControl::ApplyCarContact(const Fvector& path_velocity, float desired_speed)
{
	if (!IsCarContactActive())
		return path_velocity;

	const SCarContact& contact = m_car_contact;
	const float path_normal = path_velocity.dotproduct(contact.normal);
	const float car_normal = contact.car_velocity.dotproduct(contact.normal);
	const bool car_pushes = car_normal > contact.min_car_speed;

	float target_normal = path_normal;
	if (car_pushes)
	{
		const float car_target = car_normal + contact.separation_bias;
		if (car_target > target_normal)
			target_normal = car_target;
	}
	else if (contact.start_push_speed > 0.f && target_normal < contact.start_push_speed)
	{
		// The car is still at zero speed but the engine and transmission are
		// already applying force toward the mutant.
		target_normal = contact.start_push_speed;
	}

	// A mutant may still push a car, but only slowly. This prevents it from
	// being a kinematic bulldozer while keeping it from getting stuck.
	if (target_normal < 0.f)
	{
		const float slow_push = -contact.mutant_push_speed;
		if (slow_push > target_normal)
			target_normal = slow_push;
	}

	float external_normal = target_normal - path_normal;
	if (external_normal < 0.f)
		external_normal = 0.f;

	if ((car_pushes || contact.start_push_speed > 0.f) && external_normal > 0.f)
		external_normal *= contact.ram_scale;

	if (external_normal > contact.max_push_speed)
		external_normal = contact.max_push_speed;

	Fvector result = path_velocity;
	result.mad(contact.normal, external_normal);

	Fvector current_velocity;
	if (m_character)
		m_character->GetVelocity(current_velocity);
	else
		current_velocity.set(0.f, 0.f, 0.f);
	const float current_speed = current_velocity.magnitude();

	Fvector facing_dir;
	facing_dir.set(0.f, 0.f, 0.f);
	if (pObject != nullptr)
	{
		facing_dir = pObject->Direction();
		facing_dir.y = 0.f;
		facing_dir.normalize_safe();
	}

	const float path_speed = path_velocity.magnitude();

	if (contact.slide_scale <= 0.f || contact.slide_axis.magnitude() <= EPS_L)
		return result;

	Fvector slide = contact.slide_axis;
	slide.mad(contact.normal, -slide.dotproduct(contact.normal));
	if (slide.magnitude() <= EPS_L)
	{
		// The face axis can be parallel to a wheel/edge contact normal. Fall
		// back to a horizontal tangent of the contact normal.
		slide.crossproduct(contact.normal, Fvector().set(0.f, 1.f, 0.f));
		slide.y = 0.f;
	}
	if (slide.magnitude() <= EPS_L)
		return result;
	slide.normalize();

	float path_tangent_speed = path_velocity.dotproduct(slide);
	float current_tangent_speed = current_velocity.dotproduct(slide);

	// Direction sources must be comparable: normalize the tangential velocity
	// components, otherwise the thresholds below are speed-dependent and any
	// fast movement always overrides the facing.
	float path_tangent = path_speed > EPS_L ? path_tangent_speed / path_speed : 0.f;
	float current_tangent = current_speed > EPS_L ? current_tangent_speed / current_speed : 0.f;

	// Keep a slide speed from the approach/path even after the path has
	// completed; otherwise the movement manager's no-path branch would stop
	// the mutant dead against the car.
	if (desired_speed > 0.05f)
		m_car_slide_speed = desired_speed;

	float slide_speed = (desired_speed > 0.05f) ? desired_speed : m_car_slide_speed;
	if (slide_speed <= 0.05f)
		slide_speed = contact.slide_speed;
	if (slide_speed <= 0.05f)
		slide_speed = std::fabs(current_tangent_speed);

	// Only start a forced slide when there is speed or the mutant is actually
	// pressing into the car. A standing/attacking mutant should not drift.
	if (slide_speed <= 0.05f && path_normal >= -0.01f &&
		std::fabs(current_tangent_speed) <= 0.05f)
		return result;

	// Prefer the body facing: the mutant should slide where it is looking.
	// If it looks straight into the car, fall back to path/current velocity.
	// All three values are normalized so the thresholds are comparable.
	const float facing = facing_dir.dotproduct(slide);

	float intent = 0.f;
	if (std::fabs(facing) > 0.35f)
		intent = facing;
	else if (std::fabs(path_tangent) > 0.25f)
		intent = path_tangent;
	else if (std::fabs(current_tangent) > 0.25f)
		intent = current_tangent;

	const float flip_threshold = std::max(0.f,
		std::min(1.f, contact.slide_flip));
	const u32 lock_ms = static_cast<u32>(std::max(0.f, contact.slide_lock_ms));
	const u32 confirm_ms = static_cast<u32>(std::max(0.f, contact.slide_confirm_ms));

	Fvector previous_direction = m_car_slide_direction;
	previous_direction.y = 0.f;
	const bool has_previous = m_car_slide_direction_valid &&
		previous_direction.magnitude() > EPS_L &&
		std::fabs(previous_direction.dotproduct(slide)) >= 0.5f;
	const float previous = has_previous ? previous_direction.dotproduct(slide) : 0.f;

	float direction = 0.f;
	if (!has_previous)
	{
		// Choose the side once. On an exactly head-on hit the contact point
		// offset along the face picks the nearer end deterministically.
		float chosen = 0.f;
		if (intent != 0.f)
			chosen = intent;
		else if (std::fabs(contact.side_bias) > 0.05f)
			chosen = contact.side_bias;
		else
			chosen = 1.f;

		direction = (chosen < 0.f) ? -1.f : 1.f;
		m_car_slide_direction_time = Device.dwTimeGlobal;
		m_car_slide_opposite_time = 0;
	}
	else
	{
		direction = previous;

		const bool opposite = intent != 0.f && intent * previous < 0.f;
		if (!opposite)
		{
			m_car_slide_opposite_time = 0;
			if (intent != 0.f)
				direction = (intent < 0.f) ? -1.f : 1.f;
		}
		else if (std::fabs(intent) >= flip_threshold)
		{
			// Opposite intent must survive the initial lock and then stay
			// stable for the confirmation time before the side can flip.
			if (m_car_slide_opposite_time == 0)
				m_car_slide_opposite_time = Device.dwTimeGlobal;

			const bool lock_elapsed =
				Device.dwTimeGlobal >= m_car_slide_direction_time + lock_ms;
			const bool confirm_elapsed =
				Device.dwTimeGlobal >= m_car_slide_opposite_time + confirm_ms;

			if (lock_elapsed && confirm_elapsed)
			{
				direction = (intent < 0.f) ? -1.f : 1.f;
				m_car_slide_direction_time = Device.dwTimeGlobal;
				m_car_slide_opposite_time = 0;
			}
		}
		else
		{
			m_car_slide_opposite_time = 0;
		}
	}

	if (direction < 0.f)
		slide.invert();

	m_car_slide_direction = slide;
	m_car_slide_direction_valid = true;
	m_car_slide_direction_expire = 0;

	path_tangent = path_velocity.dotproduct(slide);
	current_tangent = current_velocity.dotproduct(slide);

	const float target_tangent = std::max(std::fabs(path_tangent),
		std::max(0.f, slide_speed) * contact.slide_scale);

	// Do not snap from a head-on push to full sideways speed in one frame;
	// the character has no path acceleration in external push.
	float blend = Device.fTimeDelta * 12.f;
	if (blend < 0.f)
		blend = 0.f;
	else if (blend > 1.f)
		blend = 1.f;
	const float smooth_tangent = current_tangent + (target_tangent - current_tangent) * blend;
	result.mad(slide, smooth_tangent - path_tangent);

	m_car_slide_direction = slide;
	m_car_slide_direction_valid = true;

	return result;
}

void CPHMovementControl::SetVelocityLimit(float val)
{
	if(m_character)
		m_character->SetMaximumVelocity(val);
}

float CPHMovementControl::VelocityLimit()
{
	if(!m_character || !m_character->Exists()) return 0.f;
	return m_character->GetMaximumVelocity();
}

void CPHMovementControl::in_shedule_Update(u32 DT)
{
	if (!m_capture)
		return;

	if (m_capture->Failed())
		phcapture_destroy(m_capture);
}

void CPHMovementControl::Calculate(Fvector& vAccel, const Fvector& camDir, float /**ang_speed/**/, float jump, float /**dt/**/, bool /**bLight/**/)
{
	Fvector previous_position; previous_position.set(vPosition);
	m_character->IPosition(vPosition);
	if (bExernalImpulse)
	{

		vAccel.add(vExternalImpulse);
		m_character->ApplyForce(vExternalImpulse);
		vExternalImpulse.set(0.f, 0.f, 0.f);

		bExernalImpulse = false;
	}

	float mAccel = vAccel.magnitude();
	m_character->SetCamDir(camDir);
	m_character->SetMaximumVelocity(mAccel / 10.f);

	m_character->SetAcceleration(vAccel);

	if (!fis_zero(jump))
	{
		m_character->SetJupmUpVelocity(jump);
		m_character->Jump(vAccel);
	}

	m_character->GetSavedVelocity(vVelocity);
	fActualVelocity = vVelocity.magnitude();

	gcontact_Was = m_character->ContactWas();
	UpdateCollisionDamage();

	ICollisionDamageInfo* cdi = CollisionDamageInfo();
	if (cdi->HitCallback())
	{
		cdi->HitCallback()->call((m_character->PhysicsRefObject()), fMinCrashSpeed, fMaxCrashSpeed, fContactSpeed, gcontact_HealthLost, CollisionDamageInfo());
	}

	TraceBorder(previous_position);
	CheckEnvironment(vPosition);
	bSleep = false;
	m_character->Reinit();
}

void CPHMovementControl::UpdateCollisionDamage( )
{
//reset old
	fContactSpeed				= 0.f;
	gcontact_HealthLost			= 0;
	gcontact_Power				= 0;
	const ICollisionDamageInfo* di=m_character->CollisionDamageInfo();
 	fContactSpeed=di->ContactVelocity();

	if (fContactSpeed > 0.f)
	{
		CObject* InitiatorObject = di->DamageInitiator();
		CPhysicsShellHolder* Car = nullptr;
		if (InitiatorObject != nullptr)
		{
			if (CActor* InitiatorActor = InitiatorObject->cast_actor())
			{
				if (CObject* CarObject = InitiatorActor->GetCarObject())
					Car = CarObject->cast_physics_shell_holder();
			}
			else
			{
				Car = InitiatorObject->cast_physics_shell_holder();
			}
		}

		if (Car != nullptr)
		{
			const float MinSpeed = Car->GetRamDamageMinSpeed();
			if (MinSpeed > 0.f)
			{
				float CarSpeed = 0.f;
				if (Car->PPhysicsShell() != nullptr && Car->PPhysicsShell()->isActive() && Car->PPhysicsShell()->isEnabled())
				{
					Fvector Velocity;
					Car->PPhysicsShell()->get_LinearVel(Velocity);
					CarSpeed = Velocity.magnitude() * 3.6f;
				}
				if (CarSpeed < MinSpeed)
				{
					fContactSpeed = 0.f;
					return;
				}
			}
		}
	}

	if(block_damage_step_end!=u64(-1))
	{
		if( physics_world()->StepsNum() < block_damage_step_end )
		{
			fContactSpeed = 0.f;
			return;
		} else 
			block_damage_step_end = u64(-1);
	}

// calc new
	gcontact_Power				= fContactSpeed/fMaxCrashSpeed;
	if (fContactSpeed > fMinCrashSpeed) 
	{
			bool applyHighSpeedProtection = false;
			bool isFallSequence = true;
			if (CActor* actor = smart_cast<CActor*>(m_character->PhysicsRefObject()))
			{
				applyHighSpeedProtection = !fis_zero(actor->GetArtefactMovementSpeedModifier());
				if (applyHighSpeedProtection)
				{
					const u32 moveState = actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);
					isFallSequence = !!(moveState & (ACTOR_DEFS::mcJump | ACTOR_DEFS::mcFall | ACTOR_DEFS::mcLanding | ACTOR_DEFS::mcLanding2));
				}
			}

			if (applyHighSpeedProtection)
			{
				Fvector currentVelocity = {};
				m_character->GetVelocity(currentVelocity);

				const bool isStrongDownwardVelocity = currentVelocity.y < -2.0f;
				if (!isFallSequence && !isStrongDownwardVelocity)
				{
					gcontact_HealthLost = 0.0f;
					gcontact_Power = 0.0f;
					return;
				}

				if (Device.dwTimeGlobal < (m_lastSpeedCrashDamageTime + 250))
				{
					gcontact_HealthLost = 0.0f;
					gcontact_Power = 0.0f;
					return;
				}

				m_lastSpeedCrashDamageTime = Device.dwTimeGlobal;
			}

			gcontact_HealthLost = ((fContactSpeed - fMinCrashSpeed)) / (fMaxCrashSpeed - fMinCrashSpeed);

			VERIFY( m_character );
			m_character->SetHitType( DefineCollisionHitType( m_character->LastMaterialIDX() ) );

			if (m_character->PhysicsRefObject() == Actor())
			{
				Actor()->callback(GameObject::eFallHit)(
					gcontact_HealthLost,
					gcontact_Power
				);
			}
	}
}

#include <ai/monsters/basemonster/base_monster.h>
#include <ai_object_location.h>
#include "../xrCore/math/_vector3d_ext.h"

bool CPHMovementControl::MakeJumpPath(xr_vector<DetailPathManager::STravelPathPoint> &	out_path, 
									  u32 &												travel_point,
									  Fvector &											out_deviation)
{
	if ( !m_character->JumpState() )
		return					false;

	CBaseMonster* monster = pObject != nullptr ? pObject->cast_base_monster() : nullptr;
	if ( !monster )
		return					false;

	CControlJump * jump_control	=	monster->com_man().get_jump_control();
	if ( !jump_control || !jump_control->in_auto_aim() )
		return					false;

	CEntityAlive const * const enemy	=	monster->EnemyMan.get_enemy();
	if ( !enemy )
		return					false;


	Fvector self_vel		=	{ 0, 0, 0 };
	m_character->GetVelocity	(self_vel);

	if ( self_vel.magnitude() < 0.00001f )
		return					false;

	Fvector const self_pos	=	monster->Position();
	Fvector const enemy_pos	=	enemy->Position();

	Fvector const self_to_enemy	=	enemy_pos - self_pos;

	if ( angle_between_vectors(self_vel, self_to_enemy) > deg2rad(90.f) )
		return					false;

	Fvector const self_vel_normalized	=	normalize(self_vel);
	Fvector const self_to_enemy_projection_on_self_vel	=	self_vel_normalized * dotproduct(self_to_enemy, self_vel_normalized);
	Fvector const deviation	=	self_to_enemy - self_to_enemy_projection_on_self_vel;

	float const factor		=	jump_control->relative_time();

	out_deviation			=	deviation * 8.f * factor;
	out_deviation.y			=	0;

	DetailPathManager::STravelPathPoint start_point, target_point;
	start_point.position	=	self_pos;
	start_point.vertex_id	=	monster->ai_location().level_vertex_id();
	start_point.velocity	=	0;			
	out_path.push_back			(start_point);

	target_point.position	=	enemy_pos;
	target_point.vertex_id	=	enemy->ai_location().level_vertex_id();
	target_point.velocity	=	0;
	out_path.push_back			(target_point);
	travel_point			=	0;

	return						true;
}

void CPHMovementControl::Calculate(const xr_vector<DetailPathManager::STravelPathPoint>& in_path, float speed, u32& travel_point, float& precision)
{
#ifdef DEBUG
	if (debug_output().ph_dbg_draw_mask1().test(ph_m1_DbgTrackObject) &&
		(!!pObject->cName()) && _stricmp(debug_output().PH_DBG_ObjectTrackName(), *pObject->cName()) == 0)
	{
		Msg("CPHMovementControl::Calculate in %s (Object Position) %f,%f,%f", debug_output().PH_DBG_ObjectTrackName(), pObject->Position().x, pObject->Position().y, pObject->Position().z);
		Msg("CPHMovementControl::Calculate in %s (CPHMovementControl::vPosition) %f,%f,%f", debug_output().PH_DBG_ObjectTrackName(), vPosition.x, vPosition.y, vPosition.z);
	}
#endif

	xr_vector<DetailPathManager::STravelPathPoint>	replacing_path;
	Fvector		deviation = { 0, 0, 0 };
	bool const	add_deviation = MakeJumpPath(replacing_path, travel_point, deviation);

	xr_vector<DetailPathManager::STravelPathPoint> const& path = add_deviation ? replacing_path : in_path;

	if (bNonInteractiveMode)
	{
		vPosition.set(pObject->Position());
	}

	if (!m_character->Exists())	return;

	if (bNonInteractiveMode)
	{
		VERIFY(pObject);
		m_character->SetPosition(vPosition);
		return;
	}

	Fvector new_position;
	m_character->IPosition(new_position);

	int index = 0;//nearest point

	bool  near_line;
	m_path_size = (u32)path.size();
	Fvector dir;
	dir.set(0, 0, 0);
	if (m_path_size == 0)
	{
		speed = 0;
		vPosition.set(new_position);
	}
	else if (b_exect_position)
	{
		m_start_index = travel_point;

		if ((m_path_size - 1) > (int)travel_point)
			dir.sub(path[travel_point + 1].position, path[travel_point].position);
		else
			dir.sub(path[travel_point].position, new_position);

		m_start_index = travel_point;
		dir.y = 0.f;
		dir.normalize_safe();
		vPosition.set(new_position);
		m_path_distance = 0;
		SetPathDir(dir);
		vPathPoint.set(vPosition);


	}
	else
	{
		Fvector dif;

		dif.sub(new_position, vPathPoint);
		float radius = dif.magnitude() * 2.f;
		if (m_path_size == 1)
		{
			speed = 0.f;
			vPosition.set(new_position);	//todo - insert it in PathNearestPoint
			index = 0;
			vPathPoint.set(path[0].position);
			Fvector _d;
			_d.sub(path[0].position, new_position);
			SetPathDir(_d);
			m_path_distance = GetPathDir().magnitude();
			if (m_path_distance > EPS)
			{
				Fvector _d_ = GetPathDir();
				_d_.mul(1.f / m_path_distance);
				SetPathDir(_d_);
			}
			near_line = false;
		}
		else
		{
			m_path_distance = phInfinity;
			near_line = true;
			if (m_start_index < m_path_size)
			{
				PathNearestPointFindUp(path, new_position, index, radius, near_line);
				PathNearestPointFindDown(path, new_position, index, radius, near_line);
			}
			if (m_path_distance > radius)
			{
				m_start_index = 0;
				PathNearestPoint(path, new_position, index, near_line);
			}
			vPosition.set(new_position);//for PathDirLine && PathDirPoint
			if (near_line) PathDIrLine(path, index, m_path_distance, precision, dir);
			else		  PathDIrPoint(path, index, m_path_distance, precision, dir);


			travel_point = (u32)index;
			m_start_index = index;
			if (fis_zero(speed)) dir.set(0, 0, 0);
		}

	}

	dir.y = 0.f;
	dir.normalize_safe();

	if (bExernalImpulse)
	{
		Fvector V;
		V.set(dir);

		V.mul(speed * 10.f);
		V.add(vExternalImpulse);
		m_character->ApplyForce(vExternalImpulse);
		speed = V.magnitude();

		if (!fis_zero(speed))
		{
			dir.set(V);
			dir.mul(1.f / speed);
		}
		speed /= 10.f;
		vExternalImpulse.set(0.f, 0.f, 0.f);
		bExernalImpulse = false;
	}

	m_character->SetMaximumVelocity(speed);

	if (add_deviation)
	{
		dir = deviation;
	}

	m_character->SetAcceleration(dir);
	m_character->GetSmothedVelocity(vVelocity);
	fActualVelocity = vVelocity.magnitude();

	gcontact_Was = m_character->ContactWas();
	UpdateCollisionDamage();

	CheckEnvironment(vPosition);
	bSleep = false;
	b_exect_position = false;
}

void CPHMovementControl::PathNearestPoint(const xr_vector<DetailPathManager::STravelPathPoint>  &path,	//in path
										  const Fvector					&new_position,					//in position
										  int							&index,							//in start from; out nearest
										  bool							&near_line						//out type
										  )
{

	Fvector from_first,from_second,dir;
	bool after_line=true;//to check first point

	Fvector path_point,vtemp;
	float temp;

	int i = 0;
	for(int i_=0;i_<m_path_size-1;++i_)
	{
		const Fvector &first=path[i_].position, &second=path[i_+1].position;
		from_first.sub(new_position,first);
		from_second.sub(new_position,second);
		dir.sub(second,first);
		dir.normalize_safe();

		//befor this line
		if(from_first.dotproduct(dir)<0.f)
		{
			//after previous line && befor this line = near first point
			if(after_line)
			{
				vtemp.sub(new_position,first);
				temp=vtemp.magnitude();
				if(temp<m_path_distance)
				{
					m_path_distance=temp;
					index=i_;
					vPathPoint.set(first);
					SetPathDir(dir);
					near_line=false;
				}
			}
			after_line=false;

		}
		else //after first 
		{
			//befor second && after first = near line
			if(from_second.dotproduct(dir)<0.f) 
			{
				temp=dir.dotproduct(from_first);
				vtemp.set(dir);
				vtemp.mul(temp);
				path_point.add(vtemp,first);
				vtemp.sub(path_point,new_position);
				temp=vtemp.magnitude();
				if(temp<m_path_distance)
				{
					m_path_distance=temp;
					index=i_;
					vPathPoint.set(path_point);
					SetPathDir(dir);
					near_line=true;
				}
			}
			else
			{
				//after second = after this line
				after_line=true;
			}
		}
	}

	if(m_path_distance==phInfinity)	//after whall path
	{

		R_ASSERT2(after_line,"Must be after line");
		vtemp.sub(new_position,path[i].position);
		m_path_distance=vtemp.magnitude();
		SetPathDir(dir);
		vPathPoint.set(path[i].position);
		index=i;
		near_line=false;
	}
#ifdef DEBUG
	if(ph_dbg_draw_mask1.test(ph_m1_DbgTrackObject)&&(!!pObject->cName())&&_stricmp(PH_DBG_ObjectTrackName(),*pObject->cName())==0)
	{
		Msg("CPHMovementControl::Calculate out %s (Object Position) %f,%f,%f",PH_DBG_ObjectTrackName(),pObject->Position().x,pObject->Position().y,pObject->Position().z);
		Msg("CPHMovementControl::Calculate out %s (CPHMovementControl::vPosition) %f,%f,%f",PH_DBG_ObjectTrackName(),vPosition.x,vPosition.y,vPosition.z);
	}
#endif
	return;
}

void CPHMovementControl::PathNearestPointFindUp(const xr_vector<DetailPathManager::STravelPathPoint>	&path,			//in path
												const Fvector					&new_position,  //in position
												int								&index,			//in start from; out nearest
												float							radius,	//out m_path_distance in exit radius
												bool							&near_line      //out type
										  )
{

	Fvector from_first,from_second,dir;
	bool after_line=true;//to check first point

	Fvector path_point,vtemp;
	float temp;
	dir.set		(0,0,1);

	int i = 0;
	for(i=m_start_index;i<m_path_size-1;++i)
	{
		const Fvector &first=path[i].position, &second=path[i+1].position;
		from_first.sub(new_position,first);
		from_second.sub(new_position,second);
		dir.sub(second,first);
		dir.normalize_safe();
		float from_first_dir=from_first.dotproduct(dir);
		float from_second_dir=from_second.dotproduct(dir);

		if(from_first_dir<0.f)//before this line
		{
			temp=from_first.magnitude();
			if(after_line)//after previous line && before this line = near first point
			{
				if(temp<m_path_distance)
				{
					m_path_distance=temp;
					index=i;
					vPathPoint.set(first);
					SetPathDir(dir);
					near_line=false;
				}
			}
	
			if(temp>radius) break;//exit test
			after_line=false;

		}
		else //after first 
		{
			if(from_second_dir<0.f) //befor second && after first = near line
			{
				vtemp.set(dir);
				vtemp.mul(from_first_dir);
				path_point.add(vtemp,first);
				vtemp.sub(path_point,new_position);
				temp=vtemp.magnitude();
				if(temp<m_path_distance)
				{
					m_path_distance=temp;
					index=i;
					vPathPoint.set(path_point);
					SetPathDir(dir);
					near_line=true;
				}
				if(temp>radius) break;//exit test
			}
			else							//after second = after this line
			{
				after_line=true;
				if(from_second.magnitude()>radius) break;//exit test
			}
		}
	}

	if(m_path_distance==phInfinity && i==m_path_size-1)	
	{

		R_ASSERT2															(after_line,"Must be after line");
		vtemp										.sub					(new_position,path[i].position)		;
		m_path_distance								=vtemp.magnitude		()									;
		SetPathDir									(dir);
		vPathPoint									.set					(path[i].position)					;
		index										=i															;
		near_line									=false														;
	}
	

	return;
}


void CPHMovementControl::PathNearestPointFindDown(const xr_vector<DetailPathManager::STravelPathPoint>	&path,			//in path
												const Fvector					&new_position,  //in position
												int								&index,			//in start from; out nearest
												float							radius,	//out m_path_distance in exit radius
												bool							&near_line      //out type
										  )
{

	Fvector from_first,from_second,dir;
	bool after_line=true;//to check first point

	Fvector path_point,vtemp;
	float temp;
	//(going down)
	dir.set(0,0,1);

	int i = 0;
	for(i=m_start_index;i>1;--i)
	{
		const Fvector &first=path[i-1].position, &second=path[i].position;
		from_first.sub(new_position,first);
		from_second.sub(new_position,second);
		dir.sub(second,first);
		dir.normalize_safe();
		float from_first_dir=from_first.dotproduct(dir);
		float from_second_dir=from_second.dotproduct(dir);

		if(from_second_dir>0.f)//befor this line
		{
			temp=from_second.magnitude();
			if(after_line)//after previous line && befor this line = near second point (going down)
			{
				if(temp<m_path_distance)
				{
					m_path_distance		=temp		;
					index				=i			;
					vPathPoint			.set(second);
					SetPathDir			(dir)	;
					near_line			=false		;
				}
			}

			if(temp>radius) break;//exit test
			after_line=false;

		}
		else //after second
		{

			if(from_first_dir>0.f) //after second && before first = near line (going down)
			{
				vtemp.set(dir);
				vtemp.mul(from_second_dir);
				path_point.add(second,vtemp); //from_second_dir <0.f !!
				vtemp.sub(path_point,new_position);
				temp=vtemp.magnitude();
				if(temp<m_path_distance)
				{
					m_path_distance=temp;
					index			=i-1;
					vPathPoint.set	(path_point);
					SetPathDir		(dir);
					near_line		=true;
				}
				if(temp>radius) break;//exit test
			}
			else							//after first = after this line(going down)
			{
				after_line=true;
				if(from_first.magnitude()>radius) break;//exit test
			}
		}
	}

	if(m_path_distance==phInfinity && i==1)	
	{

		R_ASSERT2				(after_line,"Must be after line");
		vtemp.sub				(new_position,path[i].position);
		m_path_distance			=vtemp.magnitude();
		SetPathDir				(dir);
		vPathPoint.set			(path[i].position);
		index					=i;
		near_line				=false;
	}


	return;
}

void		CPHMovementControl::CorrectPathDir			(const Fvector &real_path_dir,const xr_vector<DetailPathManager::STravelPathPoint> & path,int index,Fvector &corrected_path_dir)
{
	const float epsilon=0.1f;
	float plane_motion=dXZMag(real_path_dir);
	if(fis_zero(plane_motion,epsilon))
	{
		if(!fis_zero(plane_motion,EPS))
		{
			corrected_path_dir.set(real_path_dir);
			corrected_path_dir.y=0.f;
			corrected_path_dir.mul(1.f/plane_motion);
		}
		else if(index!=m_path_size-1)
		{
			corrected_path_dir.sub(path[index+1].position,path[index].position);
			corrected_path_dir.normalize_safe();
			CorrectPathDir(corrected_path_dir,path,index+1,corrected_path_dir);
		}
		else 
		{
			corrected_path_dir.set(real_path_dir);
		}
	}
	else
	{
		corrected_path_dir.set(real_path_dir);
	}
}
void CPHMovementControl::PathDIrLine(const xr_vector<DetailPathManager::STravelPathPoint> &path,  int index,  float distance,  float precesition, Fvector &dir  )
{

	Fvector to_path_point;
	Fvector corrected_path_dir;CorrectPathDir(GetPathDir(),path,index,corrected_path_dir);
	to_path_point.sub(vPathPoint,vPosition);	//_new position
	float mag=to_path_point.magnitude();
	if(mag<EPS)
	{
	dir.set(corrected_path_dir);
	return;
	}
	to_path_point.mul(1.f/mag);
	if(mag>FootRadius())to_path_point.mul(precesition);
	else to_path_point.mul(mag*precesition);
	dir.add(corrected_path_dir,to_path_point);
	dir.normalize_safe();
}

void CPHMovementControl::PathDIrPoint(const xr_vector<DetailPathManager::STravelPathPoint> &path,  int index,  float distance,  float precesition, Fvector &dir  )
{
	Fvector to_path_point;
	Fvector corrected_path_dir;CorrectPathDir(GetPathDir(),path,index,corrected_path_dir);
	to_path_point.sub(vPathPoint,vPosition);	//_new position
	float mag=to_path_point.magnitude();

	if(mag<EPS) //near the point
	{  
		if(0==index||m_path_size-1==index) //on path eidge
		{
			dir.set(corrected_path_dir);//??
			return;
		}
		dir.sub(path[index].position,path[index-1].position);
		dir.normalize_safe();
		dir.add(corrected_path_dir);
		dir.normalize_safe();
	}
	to_path_point.mul(1.f/mag);
	if(m_path_size-1==index)//on_path_edge
	{
		dir.set(to_path_point);
		return;
	}


	if(mag<EPS||fis_zero(dXZMag(to_path_point),EPS))
	{
		dir.set(corrected_path_dir);
		return;//mean dir
	}
	
	Fvector tangent;
	tangent.crossproduct(Fvector().set(0,1,0),to_path_point);

	VERIFY(!fis_zero(tangent.magnitude()));
	tangent.normalize();
	if(dir.square_magnitude()>EPS)
	{
		if(tangent.dotproduct(dir)<0.f)tangent.invert();
	}
	else
	{
		if(tangent.dotproduct(corrected_path_dir)<0.f)tangent.invert();
	}

	if(mag>FootRadius())to_path_point.mul(precesition);
	else to_path_point.mul(mag*precesition);
	dir.add(tangent,to_path_point);
	dir.normalize_safe();
}

void CPHMovementControl::SetActorRestrictorRadius(ERestrictionType rt, float r)
{
	if (m_character && eCharacterType == actor)
		(m_character)->SetRestrictorRadius(rt, r);
}

void CPHMovementControl::Load(const char* section)
{
	Fbox	bb;

	// m_PhysicMovementControl: BOX
	Fvector	vBOX1_center= pSettings->r_fvector3	(section,"ph_box1_center"	);
	Fvector	vBOX1_size	= pSettings->r_fvector3	(section,"ph_box1_size"		);
	bb.set	(vBOX1_center,vBOX1_center); bb.grow(vBOX1_size);
	SetBox		(1,bb);

	// m_PhysicMovementControl: BOX
	Fvector	vBOX0_center= pSettings->r_fvector3	(section,"ph_box0_center"	);
	Fvector	vBOX0_size	= pSettings->r_fvector3	(section,"ph_box0_size"		);
	bb.set	(vBOX0_center,vBOX0_center); bb.grow(vBOX0_size);
	SetBox		(0,bb);

	// m_PhysicMovementControl: Crash speed and mass
	float	cs_min		= pSettings->r_float	(section,"ph_crash_speed_min"	);
	float	cs_max		= pSettings->r_float	(section,"ph_crash_speed_max"	);
	float	mass		= pSettings->r_float	(section,"ph_mass"				);
	xr_token retrictor_types[]={
		{ "actor",			rtActor},
		{ "medium_monster",	rtMonsterMedium},
		{ "stalker",		rtStalker	},
		{ "none",			rtNone	},
		{ 0,							0}
	};

	if(pSettings->line_exist(section,"actor_restrictor"))
		SetRestrictionType(ERestrictionType(pSettings->r_token(section, "actor_restrictor", retrictor_types)));

	fCollisionDamageFactor = pSettings->read_if_exists<float>(section, "ph_collision_damage_factor", fCollisionDamageFactor);
	R_ASSERT3(fCollisionDamageFactor <= 1.f, "ph_collision_damage_factor >1.", section);
	SetCrashSpeeds(cs_min, cs_max);
	SetMass(mass);
}

void CPHMovementControl::CheckEnvironment(const Fvector &/**V/**/){
	eOldEnvironment=eEnvironment;
	switch (m_character->CheckInvironment()){
case peOnGround : eEnvironment=peOnGround;break;
case peInAir :		eEnvironment=peInAir		;break;
case peAtWall : eEnvironment=peAtWall		;break;
	}
}

void CPHMovementControl::GroundNormal(Fvector& norm)
{
	if (m_character && m_character->Exists())m_character->GroundNormal(norm);
	else norm.set(0.f, 1.f, 0.f);
}

void CPHMovementControl::SetEnvironment( int enviroment,int old_enviroment){
	switch(enviroment){
	case 0: eEnvironment=peOnGround;
		break;
	case 1: eEnvironment=peAtWall;
		break;
	case 2: eEnvironment=peInAir;
	}
	switch(old_enviroment){
	case 0: eOldEnvironment=peOnGround;
		break;
	case 1: eOldEnvironment=peAtWall;
		break;
	case 2: eOldEnvironment=peInAir;
	}
}

void CPHMovementControl::SetPosition(const Fvector &P){	
#ifdef DEBUG
	if(ph_dbg_draw_mask1.test(ph_m1_DbgTrackObject)&&(!!pObject->cName())&&_stricmp(PH_DBG_ObjectTrackName(),*pObject->cName())==0)
	{
		Msg("CPHMovementControl::SetPosition %s (Object Position) %f,%f,%f",PH_DBG_ObjectTrackName(),pObject->Position().x,pObject->Position().y,pObject->Position().z);
		Msg("CPHMovementControl::SetPosition %s (CPHMovementControl::vPosition) %f,%f,%f",PH_DBG_ObjectTrackName(),vPosition.x,vPosition.y,vPosition.z);
	}
#endif
	vPosition.set	(P);  VERIFY( m_character ) ;m_character->SetPosition(vPosition);

}

bool CPHMovementControl::TryPosition(Fvector& pos)															
{

VERIFY_BOUNDARIES2(pos,ph_boundaries(),m_character->PhysicsRefObject(),"CPHMovementControl::TryPosition	arqument pos");

#ifdef DEBUG
		if(ph_dbg_draw_mask1.test(ph_m1_DbgTrackObject)&&(!!pObject->cName())&&_stricmp(PH_DBG_ObjectTrackName(),*pObject->cName())==0)
		{
			Msg("CPHMovementControl::TryPosition %s (Object Position) %f,%f,%f",PH_DBG_ObjectTrackName(),pObject->Position().x,pObject->Position().y,pObject->Position().z);
			Msg("CPHMovementControl::TryPosition %s (CPHMovementControl::vPosition) %f,%f,%f",PH_DBG_ObjectTrackName(),vPosition.x,vPosition.y,vPosition.z);
		}
#endif
		if (m_character->Exists()) {	
			bool					ret = m_character->TryPosition(pos,b_exect_position)&&!bExernalImpulse;
			m_character->GetPosition(vPosition);
			return					(ret);
		}

		vPosition.set	(pos);
		return			(true);
}


void CPHMovementControl::GetPosition(Fvector &P)
{	
VERIFY_BOUNDARIES2(P,ph_boundaries(),m_character->PhysicsRefObject(),"CPHMovementControl::GetPosition	arqument pos");

#ifdef DEBUG
	if(ph_dbg_draw_mask1.test(ph_m1_DbgTrackObject)&&(!!pObject->cName())&&_stricmp(PH_DBG_ObjectTrackName(),*pObject->cName())==0)
	{
		Msg("CPHMovementControl::GetPosition %s (Object Position) %f,%f,%f",PH_DBG_ObjectTrackName(),pObject->Position().x,pObject->Position().y,pObject->Position().z);
		Msg("CPHMovementControl::GetPosition %s (CPHMovementControl::vPosition) %f,%f,%f",PH_DBG_ObjectTrackName(),vPosition.x,vPosition.y,vPosition.z);
	}
#endif
	P.set			(vPosition); 
VERIFY_BOUNDARIES2(vPosition, ph_boundaries(), m_character->PhysicsRefObject(),"CPHMovementControl::GetPosition	out pos");
}

void CPHMovementControl::AllocateCharacterObject(CharacterType type)
{
	switch(type)
	{
		case actor:		m_character = create_actor_character( true )	;	break;
		case ai:		m_character = create_ai_character()				;	break;
		default:		NODEFAULT;
	}
	eCharacterType=type;
	m_character->SetMas(fMass);
	m_character->SetPosition(vPosition);

#ifdef DEBUG
	if(ph_dbg_draw_mask1.test(ph_m1_DbgTrackObject)&&(!!pObject->cName())&&_stricmp(PH_DBG_ObjectTrackName(),*pObject->cName())==0)
	{
		Msg("CPHMovementControl::AllocateCharacterObject %s (Object Position) %f,%f,%f",PH_DBG_ObjectTrackName(),pObject->Position().x,pObject->Position().y,pObject->Position().z);
		Msg("CPHMovementControl::AllocateCharacterObject %s (CPHMovementControl::vPosition) %f,%f,%f",PH_DBG_ObjectTrackName(),vPosition.x,vPosition.y,vPosition.z);
	}
#endif
}

void CPHMovementControl::PHCaptureObject(CPhysicsShellHolder* object, CPHCaptureBoneCallback* cb /*=0*/)
{
	if (m_capture)
		return;

	if (!object || !object->PPhysicsShell() || !object->m_pPhysicsShell->isActive())
		return;

	m_capture = phcapture_create(m_character, object, static_cast<NearestToPointCallback*>(cb));
}

void	CPHMovementControl::PHCaptureObject(CPhysicsShellHolder* object, u16 element)
{
	if (m_capture)
		return;

	if (!object || !object->PPhysicsShell() || !object->PPhysicsShell()->isActive())
		return;

	m_capture = phcapture_create(m_character, object, element);
}

Fvector CPHMovementControl::PHCaptureGetNearestElemPos(const CPhysicsShellHolder* object)
{
	R_ASSERT3((object->m_pPhysicsShell != nullptr), "NO Phisics Shell for object ", *object->cName());

	CPhysicsElement *ph_elem =  object->m_pPhysicsShell->NearestToPoint(vPosition);

	Fvector v;
	ph_elem->GetGlobalPositionDynamic(&v);	
	
	return v;
}

Fmatrix CPHMovementControl::PHCaptureGetNearestElemTransform(CPhysicsShellHolder* object)
{
	CPhysicsElement *ph_elem =  object->m_pPhysicsShell->NearestToPoint(vPosition);

	Fmatrix m;
	ph_elem->GetGlobalTransformDynamic(&m);

	return m;
}


void CPHMovementControl::PHReleaseObject()
{
	if(m_capture) 
		m_capture->Release();
}

void CPHMovementControl::DestroyCharacter()
{
	VERIFY( m_character );
	m_character->Destroy();
	phcapture_destroy(m_capture);
}

void CPHMovementControl::DeleteCharacterObject()
{
	xr_delete(m_character);
	phcapture_destroy(m_capture);
}

void CPHMovementControl::JumpV(const Fvector &jump_velocity)
{
	m_character->Enable();
	m_character->Jump(jump_velocity);
}

void CPHMovementControl::Jump(const Fvector &end_point, float time)
{
	Jump(smart_cast<CGameObject*>(m_character->PhysicsRefObject())->Position(),end_point,time);
}

void CPHMovementControl::Jump(const Fvector &start_point,const Fvector &end_point, float time)
{
	Fvector velosity;
	velosity.sub(end_point,start_point);
	TransferenceToThrowVel(velosity,time,physics_world()->Gravity());
	JumpV(velosity);
}

float CPHMovementControl::Jump(const Fvector &end_point)
{
	float time =JumpMinVelTime(end_point);
	Jump(smart_cast<CGameObject*>(m_character->PhysicsRefObject())->Position(),end_point,time);
	return time;
}

void CPHMovementControl::GetJumpMinVelParam(Fvector &min_vel,float &time,JumpType &type,const Fvector &end_point)
{
	time =JumpMinVelTime(end_point);
	GetJumpParam(min_vel,type,end_point,time);
}

float CPHMovementControl::JumpMinVelTime(const Fvector &end_point)
{
	return ThrowMinVelTime(Fvector().sub(end_point,smart_cast<CGameObject*>(m_character->PhysicsRefObject())->Position()),physics_world()->Gravity());
}

void CPHMovementControl::GetJumpParam(Fvector& velocity, JumpType& type, const Fvector& end_point, float time)
{
	Fvector velosity; velosity.sub(smart_cast<CGameObject*>(m_character->PhysicsRefObject())->Position(), end_point);
	TransferenceToThrowVel(velosity, time, physics_world()->Gravity());
	if (velocity.y < 0.f)
	{
		type = jtStrait;
		return;
	}
	float rise_time = velosity.y / physics_world()->Gravity();
	if (std::abs(rise_time - time) < EPS_L)
	{
		type = jtHigh;
	}
	else if (rise_time > time)
	{
		type = jtStrait;
	}
	else
	{
		type = jtCurved;
	}
}

void CPHMovementControl::SetMaterial(u16 material)
{
	m_material=material;
	if(m_character)
	{
		m_character->SetMaterial(material);
	}
}
void CPHMovementControl::CreateCharacter()
{	
	Fvector size = {aabb.x2-aabb.x1,aabb.y2-aabb.y1,aabb.z2-aabb.z1};
	m_character->Create(size);
	m_character->SetMaterial(m_material);
	m_character->SetAirControlFactor(fAirControlParam);
#ifdef DEBUG
	if(ph_dbg_draw_mask1.test(ph_m1_DbgTrackObject)&&(!!pObject->cName())&&_stricmp(PH_DBG_ObjectTrackName(),*pObject->cName())==0)
	{
		Msg("CPHMovementControl::CreateCharacter %s (Object Position) %f,%f,%f",PH_DBG_ObjectTrackName(),pObject->Position().x,pObject->Position().y,pObject->Position().z);
		Msg("CPHMovementControl::CreateCharacter %s (CPHMovementControl::vPosition) %f,%f,%f",PH_DBG_ObjectTrackName(),vPosition.x,vPosition.y,vPosition.z);
	}
#endif
	m_character->SetPosition(vPosition);
	m_character->SetCollisionDamageFactor(fCollisionDamageFactor*fCollisionDamageFactor);
	trying_times[0]=trying_times[1]=trying_times[2]=trying_times[3]=u32(-1);
	trying_poses[0].set(vPosition);
	trying_poses[1].set(vPosition);
	trying_poses[2].set(vPosition);
	trying_poses[3].set(vPosition);
}

CPHSynchronize*	CPHMovementControl::GetSyncItem()
{
	if(m_character)
		return smart_cast<CPHSynchronize*>(m_character);

	return nullptr;
}

void CPHMovementControl::Freeze()
{
	if(m_character)
		m_character->Freeze();
}

void CPHMovementControl::UnFreeze()
{
	if(m_character)
		m_character->UnFreeze();
}

void CPHMovementControl::ActivateBox(DWORD id, bool Check/*false*/)
{
	if (Check && (m_dwCurBox == id))
		return;

	m_dwCurBox = id;
	aabb.set(boxes[id]);

	if (!m_character || !m_character->Exists())
		return;

	Fvector size = { aabb.x2 - aabb.x1,aabb.y2 - aabb.y1,aabb.z2 - aabb.z1 };
	m_character->SetBox(size);
}

void CPHMovementControl::InterpolateBox		(DWORD id, float k)
{ 
	if (m_dwCurBox == id) return;
	if(!m_character||!m_character->Exists()) return;
	Fvector size = {aabb.x2-aabb.x1,aabb.y2-aabb.y1,aabb.z2-aabb.z1};
	Fvector to_size = {boxes[id].x2-boxes[id].x1,boxes[id].y2-boxes[id].y1,boxes[id].z2-boxes[id].z1};
	size.lerp(size,to_size,k);
	m_character->SetBox(size);
}

void CPHMovementControl::ApplyHit(const Fvector& dir,const float P,ALife::EHitType hit_type)
{

	VERIFY( m_character );
	// stop-motion
	if(!m_character->IsActorCharacter())
		return;
	if ( 	
		(Environment()==CPHMovementControl::peOnGround || Environment()==CPHMovementControl::peAtWall) 
		)
	{
		switch(hit_type)
		{
			case ALife::eHitTypeShock :												;//stop
			case ALife::eHitTypeStrike:												;//stop
			case ALife::eHitTypeWound:			SetVelocity(Fvector().set(0,0,0))	; break; // stop							;
			case ALife::eHitTypeLightBurn  :										;//not stop
			case ALife::eHitTypeBurn:												;//not stop
			case ALife::eHitTypeRadiation:											;//not stop
			case ALife::eHitTypeTelepatic:											;//not stop
			case ALife::eHitTypeChemicalBurn:										;break;//not stop
			case ALife::eHitTypeExplosion:											;//stop
			case ALife::eHitTypeFireWound:											;//stop
			case ALife::eHitTypeWound_2:											;break;//stop		//knife's alternative fire
			case ALife::eHitTypePhysicStrike:	SetVelocity(Fvector().set(0,0,0))	;break;//stop
			default:																NODEFAULT	;
		}
	}
	//hit
	if(hit_type==ALife::eHitTypeExplosion||hit_type==ALife::eHitTypeWound)
																	ApplyImpulse(dir,P);
}

void CPHMovementControl::SetFrictionFactor(float f)
{
	m_character->FrictionFactor() = f;
}

float CPHMovementControl::GetFrictionFactor()
{
	return m_character->FrictionFactor();
}

void CPHMovementControl::MulFrictionFactor(float f)
{
	m_character->FrictionFactor() *= f;
}

IElevatorState	*CPHMovementControl::ElevatorState()
{
	if(!m_character || !m_character->Exists())
		return nullptr;
	return m_character->ElevatorState();
}

struct STraceBorderQParams
{
	CPHMovementControl* m_movement;
	const Fvector& m_dir;
	STraceBorderQParams(CPHMovementControl* movement, const Fvector& dir) :
		m_dir(dir)
	{
		m_movement = movement;
	}
	STraceBorderQParams& operator = (STraceBorderQParams& p) { VERIFY(false); return p; }
};

bool CPHMovementControl::BorderTraceCallback(const collide::rq_result& result, LPVOID params)
{
	STraceBorderQParams& p = *(STraceBorderQParams*)params;
	u16 mtl_idx = GAMEMTL_NONE_IDX;
	const CDB::TRI* T = nullptr;

	if (!result.IsStatic())
	{
		return true;
	}
	else
	{
		//получить треугольник и узнать его материал
		T = &result.GetStatic()->get_tris()[result.element];
		mtl_idx = T->material;
	}

	VERIFY(T);
	SGameMtl* mtl = GMLib.GetMaterialByIdx(mtl_idx);

	if (mtl->Flags.test(SGameMtl::flInjurious))
	{
		Fvector tri_norm;
		{
			Fvector Verts[3];
			result.xform.transform_tiny(Verts[0], result.GetStatic()->verts[T->verts[0]]);
			result.xform.transform_tiny(Verts[1], result.GetStatic()->verts[T->verts[1]]);
			result.xform.transform_tiny(Verts[2], result.GetStatic()->verts[T->verts[2]]);
			Verts[2].sub(Verts[1]);
			Verts[1].sub(Verts[0]);
			tri_norm.crossproduct(Verts[1],Verts[2]);
		}

		if (p.m_dir.dotproduct(tri_norm) < 0.f)
		{
			p.m_movement->in_dead_area_count++;
		}
		else
		{
			p.m_movement->in_dead_area_count--;
		}
	}

	return true;
}

void CPHMovementControl::TraceBorder(const Fvector &prev_position)
{

	const Fvector	&from_pos			=prev_position						;
	const Fvector	&to_position		=vPosition							;
	Fvector dir;	dir					.sub(to_position,from_pos)			;
	float sq_mag	=					dir.square_magnitude()				;
	if(sq_mag==0.f) return													;
	float mag=_sqrt(sq_mag)													;
	dir.mul(1.f/mag)														;
	collide::ray_defs	RD		(from_pos,dir,mag,0,collide::rqtStatic)		;
	VERIFY							(!fis_zero(RD.dir.square_magnitude()))	;

	STraceBorderQParams			p(this,dir);
	storage.r_clear				();
	g_pGameLevel->ObjectSpace.RayQuery(storage,RD,BorderTraceCallback,&p,nullptr,smart_cast<CObject*>(m_character->PhysicsRefObject()));
}

void CPHMovementControl::UpdateObjectBox(IPHCharacter* ach)
{
	if (!m_character || !m_character->Exists()) return;
	if (!ach || !ach->Exists()) return;

	Fvector cbox;
	//PKinematics(pObject->Visual())->CalculateBones();
	pObject->BoundingBox().getradius(cbox);

	const Fvector& pa = ach->BodyPosition();
	const Fvector& p = m_character->BodyPosition();
	Fvector2 poses_dir; poses_dir.set(p.x - pa.x, p.z - pa.z); float plane_dist = poses_dir.magnitude();

	if (plane_dist > 2.f)
		return;

	if (plane_dist > EPS_S)
		poses_dir.mul(1.f / plane_dist);

	Fvector2 plane_cam; plane_cam.set(Device.vCameraDirection.x, Device.vCameraDirection.z); plane_cam.normalize_safe();
	Fvector2 plane_i; plane_i.set(pObject->XFORM().i.x, pObject->XFORM().i.z);
	Fvector2 plane_k; plane_k.set(pObject->XFORM().k.x, pObject->XFORM().k.z);
	float R = std::abs(poses_dir.dotproduct(plane_i) * cbox.x) + std::abs(poses_dir.dotproduct(plane_k) * cbox.z);

	R *= poses_dir.dotproduct(plane_cam);
	Calculate(Fvector().set(0, 0, 0), Fvector().set(1, 0, 0), 0, 0, 0, 0);
	m_character->SetObjectRadius(R);
	ach->ChooseRestrictionType(rtStalker, 1.f, m_character);
	m_character->UpdateRestrictionType(ach);
}

void CPHMovementControl::SetPathDir(const Fvector& v)
{
	_vPathDir = v;

	if (std::abs(_vPathDir.x) > 1000 || std::abs(_vPathDir.y) > 1000 || std::abs(_vPathDir.z) > 1000)
	{
		Log("_vPathDir", _vPathDir);
	}
	VERIFY2(std::abs(_vPathDir.x) < 1000, " incorrect SetPathDir ");

}

const IPhysicsElement*	CPHMovementControl::IElement( )const
{
	if(!CharacterExist()) 
		return 0;
	return m_character->PhysicsElement();
}

static const u32 move_steps_max_num = 20;
static const float move_velocity = 1.f;

static const float fmove_steps_max_num = move_steps_max_num;
void CPHMovementControl::VirtualMoveTo( const Fvector	&in_pos, Fvector &out_pos )
{
	VERIFY( CharacterExist() );
	VERIFY( _valid(in_pos) );

	class ph_character_state_save
	{
	public:
		ph_character_state_save ( IPHCharacter* character ): character_(character),  
						  saved_callback_( character->ObjectContactCallBack() ) 
		{
			character_->get_State( sv_state );
			character_->SetVirtualMoveContactCallback();
			character_->SwitchOFFInitContact();
			character_->SetApplyGravity( false );
		}

		~ph_character_state_save()
		{
			character_->SetObjectContactCallback(saved_callback_);
			character_->SwitchInInitContact();
			character_->SetApplyGravity( true );
			character_->SetObjectContactCallbackData( 0 );
			character_->set_State( sv_state );
		}

	private:
		SPHNetState				  sv_state;
		IPHCharacter*             character_;
		ObjectContactCallbackFun* saved_callback_; 
		
	} cleanup( m_character );


	
	const Fvector displacement = Fvector().sub( in_pos, vPosition );
	const float dist = displacement.magnitude();
	if( fis_zero( dist ) )
	{
		out_pos.set(in_pos);
		return;
	}
	
	float		move_time		= dist/move_velocity;
	float		n				= move_time/fixed_step;
	float		fsteps_num		= ceil( n );
	u32			steps_num		= iCeil( n );
	clamp( fsteps_num, 0.f, fmove_steps_max_num );
	clamp<u32>( steps_num, u32(0), move_steps_max_num );

	
					move_time		= fixed_step * fsteps_num;
	const float		calc_velocity	= dist/move_time;
	const float		force			= calc_velocity * m_character->Mass() / fixed_step;
	const Fvector	vforce			= Fvector().mul(displacement, force/dist );


	m_character->Enable();

	for( u32 i = 0; i< steps_num; ++i )
	{
		m_character->SetVelocity( Fvector().set(0,0,0) );
		m_character->setForce( vforce );
		m_character->step( fixed_step );
	}
	
	m_character->GetPosition( out_pos );
	VERIFY( _valid(out_pos) );
}

void CPHMovementControl::SetNonInteractive(bool v)
{
	VERIFY(m_character);
	if (!m_character->Exists())
		return;
	if (bNonInteractiveMode == v)
		return;
	if (v)
	{
		m_character->SetNonInteractive(v);
		m_character->Disable();
	}
	else
	{
		m_character->SetNonInteractive(v);
	}
	bNonInteractiveMode = v;
}

void CPHMovementControl::GetCharacterVelocity(Fvector& velocity )	
{
	if(m_character)
		m_character->GetVelocity(velocity); 
	else 
		velocity.set(0.f,0.f,0.f);
}


void	CPHMovementControl::SetJumpUpVelocity(float velocity)
{
	m_character->SetJupmUpVelocity(velocity);
}
void		CPHMovementControl::EnableCharacter()
{
	if (m_character && m_character->Exists())
		m_character->Enable();
}


void		CPHMovementControl::SetOjectContactCallback(ObjectContactCallbackFun* callback)
{
	if (m_character)
		m_character->SetObjectContactCallback(callback);
}
void		CPHMovementControl::SetFootCallBack(ObjectContactCallbackFun* callback)
{
	VERIFY(m_character);
	m_character->SetWheelContactCallback(callback);
}

ObjectContactCallbackFun* CPHMovementControl::ObjectContactCallback()
{
	if (m_character)
		return m_character->ObjectContactCallBack();
	else return nullptr;
}
u16	CPHMovementControl::ContactBone()
{
	return m_character->ContactBone();
}
const ICollisionDamageInfo* CPHMovementControl::CollisionDamageInfo()const
{
	VERIFY(m_character);
	return m_character->CollisionDamageInfo();
}
ICollisionDamageInfo* CPHMovementControl::CollisionDamageInfo()
{
	VERIFY(m_character);
	return m_character->CollisionDamageInfo();
}
void	CPHMovementControl::GetDesiredPos(Fvector& dpos)
{
	m_character->GetDesiredPosition(dpos);
}
bool	CPHMovementControl::CharacterExist() const
{
	return (m_character && m_character->Exists());
}
void CPHMovementControl::update_last_material()
{
	VERIFY(m_character);
	m_character->update_last_material();
}
u16	CPHMovementControl::injurious_material_idx()
{
	VERIFY(m_character);
	return m_character->InjuriousMaterialIDX();
}

void	CPHMovementControl::SetApplyGravity(bool flag)
{
	bIsAffectedByGravity = flag;
	if (m_character && m_character->Exists())
		m_character->SetApplyGravity(flag);
}
void	CPHMovementControl::GetDeathPosition(Fvector& pos)
{
	VERIFY(m_character);
	m_character->DeathPosition(pos);
}

bool	CPHMovementControl::IsCharacterEnabled()
{
	return m_character->IsEnabled() || bExernalImpulse || bNonInteractiveMode
		|| IsCarContactActive();
}
void	CPHMovementControl::DisableCharacter()
{
	VERIFY(m_character);
	m_character->Disable();
}

void	CPHMovementControl::GetCharacterPosition(Fvector& P)
{
	VERIFY(m_character);
	m_character->GetPosition(P);
}
void		CPHMovementControl::InterpolatePosition(Fvector& P)
{
	VERIFY(m_character && m_character->Exists());
	m_character->IPosition(P);
}
void		CPHMovementControl::SetMass(float M)
{
	fMass = M;
	if (m_character)
		m_character->SetMas(fMass);
}
float		CPHMovementControl::FootRadius()
{
	if (m_character)
		return m_character->FootRadius();
	else
		return 0.f;
}
void		CPHMovementControl::CollisionEnable(bool enable)
{
	if (!m_character || !m_character->Exists())
		return;
	if (enable)
		m_character->collision_enable();
	else
		m_character->collision_disable();

}

void CPHMovementControl::CollisionDynamicEnable(bool enable)
{
	if (!m_character || !m_character->Exists())
		return;
	if (enable)
		m_character->collision_dynamic_enable();
	else
		m_character->collision_dynamic_disable();
}

void	CPHMovementControl::SetCharacterVelocity(const Fvector& v)
{
	if (m_character)
		m_character->SetVelocity(v);
}

void	CPHMovementControl::SetPhysicsRefObject(CPhysicsShellHolder* ref_object)
{
	VERIFY(m_character);
	m_character->SetPhysicsRefObject(ref_object);
}

void		CPHMovementControl::GetSmoothedVelocity(Fvector& v)
{
	if (m_character)
		m_character->GetSmothedVelocity(v);
	else
		v.set(0, 0, 0);
}

void		CPHMovementControl::SetPLastMaterialIDX(u16* p)
{
	VERIFY(m_character);
	m_character->SetPLastMaterialIDX(p);
}

#ifdef DEBUG_DRAW
	void CPHMovementControl::dbg_Draw()
	{
#if 0
		if(m_character)
			m_character->OnRender();
#endif
	};
#endif

bool CPHMovementControl::JumpState( )
{ 
	return ( m_character&&m_character->Exists()	&& 
			 m_character->IsEnabled( )			&& 
			 m_character->JumpState( ) 
			);
}
///
bool CPHMovementControl::PhysicsOnlyMode( )
{ 
	return	m_character				&& 
			m_character->Exists()	&&
			m_character->IsEnabled()&&
			( m_character->JumpState( ) || m_character->ForcedPhysicsControl( ) )
			; 
}

void CPHMovementControl::SetRestrictionType(ERestrictionType rt)
{
	if (m_character)
		m_character->SetRestrictionType(rt);
}

void CPHMovementControl::SetActorMovable(bool v)
{
	if (m_character)
		m_character->SetActorMovable(v);
}

void CPHMovementControl::SetForcedPhysicsControl(bool v)
{
	if (m_character)
		m_character->SetForcedPhysicsControl(v);
}

bool CPHMovementControl::ForcedPhysicsControl()
{
	return m_character && m_character->ForcedPhysicsControl();
}

IPHCapture* CPHMovementControl::PHCapture()
{
	return m_capture;
}

IPhysicsShellHolder* CPHMovementControl::PhysicsRefObject			()
{
	VERIFY( m_character );
	return m_character->PhysicsRefObject();
}

void CPHMovementControl::actor_calculate(Fvector& vAccel,const Fvector& camDir, float ang_speed, float jump, float dt, bool bLight)
{
	Calculate(  vAccel,camDir, ang_speed, jump, dt, bLight );
}

void CPHMovementControl::BlockDamageSet(u64 steps_num)
{
	block_damage_step_end = physics_world()->StepsNum() + steps_num;
	UpdateCollisionDamage();//reset all saved values
}

void CPHMovementControl::NetRelcase(CObject* O)
{
	CPhysicsShellHolder* sh = O->cast_physics_shell_holder();
	if (!sh)
		return;

	IPHCapture* c = PHCapture();
	if (c)
		c->RemoveConnection(sh);

	if (m_character)
		m_character->NetRelcase(sh);
}
