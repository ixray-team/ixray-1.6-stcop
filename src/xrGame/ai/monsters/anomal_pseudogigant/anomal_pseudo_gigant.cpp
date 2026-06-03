#include "stdafx.h"
#include "pch_script.h"
#include "anomal_pseudo_gigant.h"
#include "../../../sound_player.h"
#include "../burer/burer.h"
#include "../../../GamePersistent.h"
#include "../xrScripts/script_callback_ex.h"
#include "script_game_object.h"
#include "../monster_velocity_space.h"
#include "../control_animation_base.h"
#include "../control_movement_base.h"
#include "anomal_pseudo_gigant_state_manager.h"
#include "ParticlesObject.h"
#include "../abilities/poltergeist/PolterChem.h"
#include "../abilities/poltergeist/PolterFlame.h"
#include "../abilities/poltergeist/PolterTele.h"

CAnomalPseudoGigant::CAnomalPseudoGigant()
{
	StateMan = new CStateManagerAnomalPseudoGigant(this);
	m_shield_active = false;
	m_actor_ignore = false;
	com_man().add_ability(ControlCom::eControlJump);
}

CAnomalPseudoGigant::~CAnomalPseudoGigant()
{
	xr_delete(m_flame);
	xr_delete(m_tele);
	xr_delete(m_chem);
}

void CAnomalPseudoGigant::Load(str_c section)
{
	inherited::Load(section);
	if (pSettings->read_if_exists<bool>(section,"use_flame",false)) {
		m_flame = new CPolterFlame(this);
		m_flame->load(section);
	}
	if (pSettings->read_if_exists<bool>(section,"use_tele",false)) {
		m_tele = new CPolterTele(this);
		m_tele->load(section);
	}
	if (pSettings->read_if_exists<bool>(section,"use_chem",false)) {
		m_chem = new CPolterChem(this);
		m_chem->load(section);
	}

	m_flame_max_hp_to_activate = pSettings->read_if_exists<float>(section,"flame_max_hp_to_activate",0.0f);
	m_tele_max_hp_to_activate = pSettings->read_if_exists<float>(section,"tele_max_hp_to_activate",0.0f);
	m_chem_max_hp_to_activate = pSettings->read_if_exists<float>(section,"chem_max_hp_to_activate",0.0f);

	m_shield_cooldown = pSettings->read_if_exists<u32>(section,"shield_cooldown",4000);
	m_shield_time = pSettings->read_if_exists<u32>(section,"shield_time",3000);
	m_shield_keep_particle = pSettings->read_if_exists<str_c>(section,"shield_keep_particle",nullptr);
	m_shield_keep_particle_period = pSettings->read_if_exists<u32>(section,"shield_keep_particle_period",1000);
	
	m_shield_penetration_border = pSettings->read_if_exists<float>(section,"shield_penetration_border",m_shield_penetration_border);
	m_shield_penetration_damage_coeff = pSettings->read_if_exists<float>(section,"shield_penetration_damage_coeff",m_shield_penetration_damage_coeff);

	SVelocityParam& velocity_turn = move().get_velocity(MonsterMovement::eVelocityParameterStand);

	anim().AddAnim(eAnimShieldStart, "stand_lie_down_", -1, &velocity_turn, PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimShieldContinue, "stand_sleep_", -1, &velocity_turn, PS_LIE, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	particle_fire_shield = pSettings->r_string(section, "Particle_Shield");
}

void CAnomalPseudoGigant::reinit()
{
	inherited::reinit();

	move().load_velocity(*cNameSect(), "Velocity_JumpGround", MonsterMovement::eSnorkVelocityParameterJumpGround);
	//com_man().load_jump_data("stand_attack_2_0", 0, "stand_attack_2_1", "stand_somersault_0", u32(-1), MonsterMovement::eSnorkVelocityParameterJumpGround, 0);
	com_man().load_jump_data("stand_attack_jump_1", nullptr, "stand_attack_jump_2", "stand_attack_jump_3", static_cast<u32>(-1), MonsterMovement::eSnorkVelocityParameterJumpGround, 0);
}

void CAnomalPseudoGigant::UpdateCL()
{
	inherited::UpdateCL();
	if (use_fire_ability()) m_flame->update_frame();
	if (use_tele_ability()) m_tele->update_frame();
}

void CAnomalPseudoGigant::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);
	CTelekinesis::schedule_update();
	if (use_fire_ability()) m_flame->update_schedule();
	if (use_tele_ability()) m_tele->update_schedule();
}

void CAnomalPseudoGigant::HitEntityInJump(const CEntity* pEntity)
{
	//SAAParam& params = anim().AA_GetParams("stand_kick_0");
	//HitEntity(pEntity, params.hit_power, params.impulse, params.impulse_dir);
	int drop_item_chance = ::Random.randI(1, 100);

	/*if (Actor() && this->m_bDropItemAfterSuperAttack && drop_item_chance <= this->m_iSuperAttackDropItemPer)
	{
		CInventoryItem* active_item = Actor()->inventory().ActiveItem();
		active_item->SetDropManual(true);
	}*/
}

#define JUMP_DISTANCE 20.f
bool CAnomalPseudoGigant::find_geometry(Fvector& dir)
{
	// 1. trace direction
	dir = Direction();
	float	range;

	if (trace_geometry(dir, range)) {
		if (range < JUMP_DISTANCE) {
			return true;
		}
	}

	return false;
}

#define TRACE_RANGE 40.f
float CAnomalPseudoGigant::trace(const Fvector& dir)
{
	float ret_val = flt_max;

	collide::rq_result	l_rq;

	Fvector		trace_from;
	Center(trace_from);

	float		trace_dist = Radius() + TRACE_RANGE;

	if (Level().ObjectSpace.RayPick(trace_from, dir, trace_dist, collide::rqtStatic, l_rq, this)) {
		if ((l_rq.range < trace_dist))
			ret_val = l_rq.range;
	}

	return		ret_val;
}

bool CAnomalPseudoGigant::trace_geometry(const Fvector& d, float& range)
{
	Fvector				dir;
	float				h, p;

	Fvector				Pl, Pc, Pr;
	Fvector				center;
	Center(center);

	range = trace(d);
	if (range > TRACE_RANGE) return false;

	float angle = asin(1.f / range);

	// trace center ray
	dir = d;

	dir.getHP(h, p);
	p += angle;
	dir.setHP(h, p);
	dir.normalize_safe();

	range = trace(dir);
	if (range > TRACE_RANGE) return false;

	Pc.mad(center, dir, range);

	// trace left ray
	Fvector				temp_p;
	temp_p.mad(Pc, XFORM().i, Radius() / 2);
	dir.sub(temp_p, center);
	dir.normalize_safe();

	range = trace(dir);
	if (range > TRACE_RANGE) return false;

	Pl.mad(center, dir, range);

	// trace right ray
	Fvector inv = XFORM().i;
	inv.invert();
	temp_p.mad(Pc, inv, Radius() / 2);
	dir.sub(temp_p, center);
	dir.normalize_safe();

	range = trace(dir);
	if (range > TRACE_RANGE) return false;

	Pr.mad(center, dir, range);

	float				h1, p1, h2, p2;

	Fvector().sub(Pl, Pc).getHP(h1, p1);
	Fvector().sub(Pc, Pr).getHP(h2, p2);

	return (fsimilar(h1, h2, 0.1f) && fsimilar(p1, p2, 0.1f));
}

void CAnomalPseudoGigant::jump(const Fvector& position, float factor)
{
	com_man().script_jump(position, factor);
	sound().play(MonsterSound::eMonsterSoundAggressive);
}

float	CAnomalPseudoGigant::get_detection_success_level()
{
	return override_if_debug("detection_success_level", m_detection_success_level);
}

//IC		CAnomalGigPolterSpecialAbility* ability() { return (m_flame ? m_flame : m_tele); } // remake: tele on 66% hp, flame +tele on 33% hp

bool CAnomalPseudoGigant::use_tele_ability() { 
	return m_tele && GetfHealth() <= m_tele_max_hp_to_activate; 
}

bool CAnomalPseudoGigant::use_fire_ability() { 
	return m_flame && GetfHealth() <= m_flame_max_hp_to_activate;
}

bool CAnomalPseudoGigant::use_chem_ability() {
	return m_chem && GetfHealth() <= m_chem_max_hp_to_activate;
}

void CAnomalPseudoGigant::ActivateShield()
{
	m_shield_active = true;
	callback(GameObject::eShieldOn)(lua_game_object());
}

void CAnomalPseudoGigant::DeactivateShield()
{
	m_shield_active = false;
	callback(GameObject::eShieldOff)(lua_game_object());
}

/*void CAnomalPseudoGigant::on_shield_on()
{
	//luabind::call_member<void>(this, "on_shield_on");
}

void CAnomalPseudoGigant::on_shield_off()
{
	//luabind::call_member<void>(this, "on_shield_off");
}

void CAnomalPseudoGigant::on_hit()
{
	//luabind::call_member<void>(this, "on_hit");
}*/

void CAnomalPseudoGigant::on_jump()
{
	callback(GameObject::eJump)(lua_game_object());
	//luabind::call_member<void>(this, "on_jump");
}

void	CAnomalPseudoGigant::Hit(SHit* pHDS)
{
	//inherited::Hit(pHDS);
	if (conditions().GetHealth() <= 0.0f) {
		return;
	}
	if (m_shield_active &&
		pHDS->hit_type == ALife::eHitTypeFireWound &&
		Device.dwFrame != last_hit_frame)
	{
		Fmatrix pos;
		TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->MakeXFORM(this, pHDS->bone(), pHDS->dir, pHDS->p_in_bone_space, pos);

		auto ps = Particles::Details::Create(particle_fire_shield, TRUE);
		ps->UpdateParent(pos, Fvector().set(0.f, 0.f, 0.f));
		GamePersistent().ps_needtoplay.push_back(ps);

	}
	bool IsGaussHit = EngineExternal()[EEngineExternalGame::EnableBurerShieldPenetrationWithGauss]
						&& pHDS->hit_type == ALife::eHitTypeFireWound
						&& pHDS->armor_piercing > m_shield_penetration_border;
	if (!m_shield_active || IsGaussHit)
	{
		if (IsGaussHit)
		{
			pHDS->power *= m_shield_penetration_damage_coeff;
		}
		inherited::Hit(pHDS);
	}

	last_hit_frame = Device.dwFrame;
}

bool CAnomalPseudoGigant::check_start_conditions(ControlCom::EControlType type)
{
	if (m_shield_active) {
		return false;
	}
	return inherited::check_start_conditions(type);
}
