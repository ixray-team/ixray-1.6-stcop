// Level_Bullet_Manager.cpp:	для обеспечения полета пули по траектории
//								все пули и осколки передаются сюда
//								(для просчета столкновений и их визуализации)
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "Level_Bullet_Manager.h"
#include "Entity.h"
#include "../xrEngine/GameMtlLib.h"
#include "Level.h"
#include "GamePersistent.h"
#include "game_cl_base.h"
#include "xrMessages.h"
#include "../Include/xrRender/Kinematics.h"
#include "Actor.h"
#include "ai/stalker/ai_stalker.h"
#include "character_info.h"
#include "game_cl_base_weapon_usage_statistic.h"
#include "../xrCore/Collision/xr_collide_defs.h"
#include "../xrEngine/xr_collide_form.h"
#include "Weapon.h"
#include "ParticlesObject.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "AnomalyZone.h"

#include <math3d.h>

//константы ShootFactor, определяющие 
//поведение пули при столкновении с объектом
#define RICOCHET_THRESHOLD		0.1
#define STUCK_THRESHOLD			0.4

//расстояния не пролетев которого пуля не трогает того кто ее пустил
extern float gCheckHitK;

//test callback функция 
//  object - object for testing
//return true-тестировать объект / false-пропустить объект
bool CBulletManager::test_callback(const collide::ray_defs& rd, CObject* object, LPVOID params)
{
	bullet_test_callback_data* pData = (bullet_test_callback_data*)params;
	SBullet* bullet = pData->pBullet;

	if ((object->SpatialComponent->type & ESPATIAL_TYPE::SHAPE) != ESPATIAL_TYPE::NONE)
	{
		CGameObject* go = object->cast_game_object() ? object->cast_game_object() : nullptr;

		if (go != nullptr)
			if (CAnomalyZone* CZ = go->cast_anomaly_zone())
				return CZ->PlayEntranceSmallParticles(bullet->bullet_pos, bullet->dir, bullet->start_velocity, false);

		return false;
	}

	if ((object->ID() == bullet->parent_id) && (bullet->fly_dist < parent_ignore_distance) && (!bullet->flags.ricochet_was))
	{
		return false;
	}

	bool bRes = true;
	if (object != nullptr)
	{
		CEntity* entity = object->cast_entity();
		if (entity != nullptr && entity->g_Alive() && (entity->ID() != bullet->parent_id))
		{
			ICollisionForm* cform = entity->collidable.model;
			if ((nullptr != cform) && (cftObject == cform->Type()))
			{
				CActor* actor = entity->cast_actor();
				CAI_Stalker* stalker = entity->cast_stalker();
				// в кого попали?
				if (actor != nullptr && IsGameTypeSingle() || stalker != nullptr)
				{
					// попали в актера или сталкера
					Fsphere S = cform->getSphere();
					entity->XFORM().transform_tiny(S.P);
					float dist = rd.range;
					// проверим попали ли мы в описывающую сферу 
					if (Fsphere::rpNone != S.intersect_full(bullet->bullet_pos, bullet->dir, dist))
					{
						// да попали, найдем кто стрелял
						bool play_whine = true;
						CObject* initiator = Level().Objects.net_Find(bullet->parent_id);
						if (actor != nullptr)
						{
							// попали в актера
							float hpf = 1.f;
							float ahp = actor->HitProbability();
#if 1
#	if 0
							CObject* weapon_object = Level().Objects.net_Find(bullet->weapon_id);
							if (weapon_object != nullptr)
							{
								CWeapon* weapon = weapon_object->cast_weapon();
								if (weapon != nullptr)
								{
									float fly_dist = bullet->fly_dist + dist;
									float dist_factor = _min(1.f, fly_dist / Level().BulletManager().m_fHPMaxDist);
									ahp = dist_factor * weapon->hit_probability() + (1.f - dist_factor) * 1.f;
								}
							}
#	else
							float game_difficulty_hit_probability = actor->HitProbability();
							CAI_Stalker* stalker_ = initiator != nullptr ? initiator->cast_stalker() : nullptr;
							if (stalker_ != nullptr)
							{
								hpf = stalker_->SpecificCharacter().hit_probability_factor();
							}

							float dist_factor = 1.f;
							CObject* weapon_object = Level().Objects.net_Find(bullet->weapon_id);
							if (weapon_object != nullptr)
							{
								CWeapon* weapon = weapon_object->cast_weapon();
								if (weapon != nullptr)
								{
									game_difficulty_hit_probability = weapon->hit_probability();
									float fly_dist = bullet->fly_dist + dist;
									dist_factor = std::min(1.f, fly_dist / Level().BulletManager().m_fHPMaxDist);
								}
							}

							ahp = dist_factor * game_difficulty_hit_probability + (1.f - dist_factor) * 1.f;
#	endif
#else
							CAI_Stalker* i_stalker = initiator != nullptr ? initiator->cast_stalker() : nullptr;
							// если стрелял сталкер, учитываем - hit_probability_factor сталкерa иначе - 1.0
							if (i_stalker != nullptr)
							{
								hpf = i_stalker->SpecificCharacter().hit_probability_factor();
								float fly_dist = bullet->fly_dist + dist;
								float dist_factor = _min(1.f, fly_dist / Level().BulletManager().m_fHPMaxDist);
								ahp = dist_factor * actor->HitProbability() + (1.f - dist_factor) * 1.f;
							}
#endif
							if (Random.randF(0.f, 1.f) > (ahp * hpf))
							{
								bRes = false;	// don't hit actor
								play_whine = true;		// play whine sound
							}
							else
							{
								// real test actor CFORM
								Level().BulletManager().m_rq_results.r_clear();

								if (cform->_RayQuery(rd, Level().BulletManager().m_rq_results))
								{
									bRes = true;		// hit actor
									play_whine = false;	// don't play whine sound
								}
								else
								{
									bRes = false;	// don't hit actor
									play_whine = true;		// play whine sound
								}
							}
						}
						// play whine sound
						if (play_whine)
						{
							Fvector pt;
							pt.mad(bullet->bullet_pos, bullet->dir, dist);
							Level().BulletManager().PlayWhineSound(bullet, initiator, pt);
						}
					}
					else
					{
						// don't test this object again (return false)
						bRes = false;
					}

				}
			}
		}
	}

	return bRes;
}

//callback функция 
//	result.O;		// 0-static else CObject*
//	result.range;	// range from start to element 
//	result.element;	// if (O) "num tri" else "num bone"
//	params;			// user defined abstract data
//	Device.Statistic.TEST0.End();
//return true-продолжить трассировку / false-закончить трассировку

void CBulletManager::FireShotmark (SBullet* bullet, const Fvector& vDir, const Fvector &vEnd, const collide::rq_result& R, u16 target_material, const Fvector& vNormal, bool ShowMark)
{
	SGameMtlPair* mtl_pair	= GMLib.GetMaterialPair(bullet->bullet_material_idx, target_material);
	Fvector particle_dir	= vNormal;

	if (!R.IsStatic())
	{
		auto DO = R.GetDynamic();
		particle_dir = vDir;
		particle_dir.invert	();

		//на текущем актере отметок не ставим
		if(Level().CurrentEntity() && Level().CurrentEntity()->ID() == DO->ID()) return;

		if (mtl_pair && !mtl_pair->m_pCollideMarks->empty() && ShowMark)
		{
			//добавить отметку на материале
			Fvector p;
			p.mad(bullet->bullet_pos,bullet->dir,R.range-0.01f);
			if(!g_dedicated_server)
			{
				::Render->add_SkeletonWallmark	(	&DO->renderable.xform, 
													PKinematics(DO->Visual()), 
													&*mtl_pair->m_pCollideMarks,
													p, 
													bullet->dir, 
													bullet->wallmark_size);
			}
		}

	} 
	else 
	{

		if (mtl_pair && !mtl_pair->m_pCollideMarks->empty() && ShowMark)
		{
			//вычислить нормаль к пораженной поверхности
			auto& pVerts = R.GetStatic()->verts;
			auto& pTri = R.GetStatic()->tris[R.element];
			
			Fvector Verts[3];
			R.xform.transform_tiny(Verts[0], pVerts[pTri.verts[0]]);
			R.xform.transform_tiny(Verts[1], pVerts[pTri.verts[1]]);
			R.xform.transform_tiny(Verts[2], pVerts[pTri.verts[2]]);
			
			//добавить отметку на материале
			::Render->add_StaticWallmark(&*mtl_pair->m_pCollideMarks, vEnd, bullet->wallmark_size, pTri, Verts);
		}
	}

	ref_sound* pSound = (!mtl_pair || mtl_pair->CollideSounds.empty()) ?
		nullptr : &mtl_pair->CollideSounds[::Random.randI(0, (u32)mtl_pair->CollideSounds.size())];

	//проиграть звук
	if (pSound && ShowMark)
	{
		CObject* O = Level().Objects.net_Find(bullet->parent_id);
		bullet->m_mtl_snd = *pSound;
		bullet->m_mtl_snd.play_at_pos(O, vEnd);
	}

	const char* ps_name = ( !mtl_pair || mtl_pair->CollideParticles.empty() ) ? nullptr : 
		*mtl_pair->CollideParticles[ ::Random.randI(0, (int)mtl_pair->CollideParticles.size()) ];

	SGameMtl*	tgt_mtl = GMLib.GetMaterialByIdx(target_material);
	bool bStatic = !tgt_mtl->Flags.test(SGameMtl::flDynamic);

	if( (ps_name && ShowMark) || (bullet->flags.explosive && bStatic) )
	{
		//VERIFY2					(
		//	(particle_dir.x*particle_dir.x+particle_dir.y*particle_dir.y+particle_dir.z*particle_dir.z) > flt_zero,
		//	make_string("[%f][%f][%f]", VPUSH(particle_dir))
		//);
		Fmatrix pos;
		pos.k.normalize(particle_dir);
		Fvector::generate_orthonormal_basis(pos.k, pos.j, pos.i);
		pos.c.set(vEnd);
		if(ps_name && ShowMark)
		{
			//отыграть партиклы попадания в материал
			xr_shared_ptr<CParticlesObject> ps = Particles::Details::Create(ps_name,true);

			ps->UpdateParent( pos, zero_vel );
			GamePersistent().ps_needtoplay.push_back( ps );
		}

		if( bullet->flags.explosive && bStatic )
		{
			PlayExplodePS( pos );
		}
	}
}

void CBulletManager::StaticObjectHit	(CBulletManager::_event& E)
{
//	Fvector hit_normal;
	FireShotmark(&E.bullet, E.bullet.dir,	E.point, E.R, E.tgt_material, E.normal);
//	ObjectHit	(&E.bullet,					E.point, E.R, E.tgt_material, hit_normal);
}

static bool g_clear = false;
void CBulletManager::DynamicObjectHit(CBulletManager::_event& E)
{
	//только для динамических объектов
	VERIFY(!E.R.IsStatic());
	CObject* ERO = const_cast<CObject*>(E.R.GetDynamic());
	VERIFY(ERO);

	if (CEntity* entity = ERO->cast_entity())
	{
		if (!entity->in_solid_state())
		{
			return;
		}
	}

	if (g_clear)
	{
		E.Repeated = false;
	}

	if (IsGameTypeSingle())
	{
		E.Repeated = false;
	}

	bool NeedShootmark = true;

	if (ERO->cast_actor() != nullptr)
	{
		game_PlayerState* ps = Game().GetPlayerByGameID(ERO->ID());
		if (ps && ps->testFlag(GAME_PLAYER_FLAG_INVINCIBLE))
		{
			NeedShootmark = false;
		};
	}
	else if (CBaseMonster* monster = ERO->cast_base_monster())
	{
		NeedShootmark = monster->need_shotmark();
	}

	//визуальное обозначение попадание на объекте
	FireShotmark(&E.bullet, E.bullet.dir, E.point, E.R, E.tgt_material, E.normal, NeedShootmark);

	Fvector original_dir = E.bullet.dir;

	SBullet_Hit hit_param = E.hit_result;

	// object-space
	//вычислить координаты попадания
	Fvector p_in_object_space, position_in_bone_space;
	Fmatrix m_inv;
	m_inv.invert(ERO->XFORM());
	m_inv.transform_tiny(p_in_object_space, E.point);

	// bone-space
	if (IKinematics* V = PKinematics(ERO->Visual()))
	{
		VERIFY3(V->LL_GetBoneVisible(u16(E.R.element)), *ERO->cNameVisual(), V->LL_BoneName_dbg(u16(E.R.element)));
		Fmatrix& m_bone = (V->LL_GetBoneInstance(u16(E.R.element))).mTransform;
		Fmatrix  m_inv_bone;
		m_inv_bone.invert(m_bone);
		m_inv_bone.transform_tiny(position_in_bone_space, p_in_object_space);
	}
	else
	{
		position_in_bone_space.set(p_in_object_space);
	}

	//отправить хит пораженному объекту
	if (E.bullet.flags.allow_sendhit && !E.Repeated)
	{
		//-------------------------------------------------
		bool AddStatistic = false;
		if (!IsGameTypeSingle() && E.bullet.flags.allow_sendhit && Game().m_WeaponUsageStatistic->CollectData())
		{
			if (ERO->cast_actor() != nullptr)
			{
				Game().m_WeaponUsageStatistic->OnBullet_Hit(&E.bullet, ERO->ID(), (s16)E.R.element, E.point);
				AddStatistic = true;
			};
		};

		SHit Hit = SHit(hit_param.power, original_dir, nullptr,
		u16(E.R.element), position_in_bone_space, hit_param.impulse,
		E.bullet.hit_type, E.bullet.armor_piercing, E.bullet.flags.aim_bullet);

		Hit.GenHeader(u16((AddStatistic) ? GE_HIT_STATISTIC : GE_HIT) & 0xffff, ERO->ID());
		Hit.whoID = E.bullet.parent_id;
		Hit.weaponID = E.bullet.weapon_id;
		Hit.BulletID = E.bullet.m_dwID;

		NET_Packet np;
		Hit.Write_Packet(np);

		CGameObject::u_EventSend(np);
	}
}

extern void random_dir	(Fvector& tgt_dir, const Fvector& src_dir, float dispersion);

bool CBulletManager::ObjectHit( SBullet_Hit* hit_res, SBullet* bullet, const Fvector& end_point, 
							    const collide::rq_result& R, u16 target_material, Fvector& hit_normal )
{
	//----------- normal - start
	if ( !R.IsStatic() )
	{
		//вернуть нормаль по которой играть партиклы
		CCF_Skeleton* skeleton = smart_cast<CCF_Skeleton*>(R.GetDynamic()->CFORM());
		if ( skeleton )
		{
			Fvector			e_center;
			hit_normal.set	(0,0,0);
			if ( skeleton->_ElementCenter( (u16)R.element,e_center ) )
				hit_normal.sub							(end_point, e_center);
			float len		= hit_normal.square_magnitude();
			if ( !fis_zero(len) )	hit_normal.div		(_sqrt(len));
			else				hit_normal.invert	(bullet->dir);
		}
	}
	else
	{
		//вычислить нормаль к поверхности
		auto& pTri = R.GetStatic()->tris[R.element];
		Fvector Verts[3];
		R.xform.transform_tiny(Verts[0], R.GetStatic()->verts[pTri.verts[0]]);
		R.xform.transform_tiny(Verts[1], R.GetStatic()->verts[pTri.verts[1]]);
		R.xform.transform_tiny(Verts[2], R.GetStatic()->verts[pTri.verts[2]]);
		hit_normal.mknormal	(Verts[0],Verts[1],Verts[2]);
		if ( bullet->density_mode )
		{
			Fvector new_pos;
			new_pos.mad(bullet->bullet_pos, bullet->dir, R.range);
			float l = bullet->begin_density.distance_to(new_pos);
			float shootFactor = l * bullet->density;
			bullet->speed -= shootFactor;
			bullet->speed = std::max<float>(bullet->speed, 0.f);
		}
		if (hit_normal.dotproduct(bullet->dir) < 0.f)
		{
			if ( bullet->density_mode )
			{
//				Log("WARNING: Material in material found while bullet tracing. Incorrect behaviour of shooting is possible.");
			}
			bullet->density_mode = true;
			SGameMtl* mtl = GMLib.GetMaterialByIdx(target_material);
			bullet->density = mtl->fDensityFactor;
			bullet->begin_density.mad( bullet->bullet_pos, bullet->dir,R.range );
		}
		else
		{
			bullet->density_mode=false;
		}
	}		
	//----------- normal - end
	float old_speed = bullet->speed;

	//коэффициент уменьшение силы с падением скорости
	float speed_factor = bullet->speed / bullet->max_speed;
	//получить силу хита выстрела с учетом патрона
	*hit_res = bullet->hit_param; //default param
	
	hit_res->power = bullet->hit_param.power*speed_factor;
	
	//(Если = 0, то пуля либо рикошетит(если контакт идёт по касательной), либо застряёт в текущем 
	//объекте, если больше 0, то пуля прошивает объект)

	SGameMtl* mtl = GMLib.GetMaterialByIdx( target_material );
	float mtl_ap = mtl->fShootFactor;
	float shoot_factor = 0.0f; //default >> пуля НЕ пробила материал!
	float ap = bullet->armor_piercing;

	if ( ap > EPS && ap >= mtl_ap)
	{
		//пуля пробила материал
		shoot_factor = (( ap - mtl_ap ) / ap);
	}
	
	hit_res->impulse = 0.0f;
	float speed_scale = 0.0f;
	
	if ( fsimilar( mtl_ap, 0.0f ) )//Если материал полностью простреливаемый (кусты)
	{
		return true;
	}

	if (bullet->flags.magnetic_beam && (shoot_factor > EPS))
	{
		//air resistance of magnetic_beam bullet is armor resistance too
		bullet->armor_piercing	-= mtl_ap * bullet->air_resistance;
		return true;
	}

	//рикошет
	Fvector			new_dir;
	new_dir.reflect	( bullet->dir,hit_normal );
	Fvector			tgt_dir;
	random_dir		( tgt_dir, new_dir, deg2rad( 10.0f ) );
	float ricoshet_factor = bullet->dir.dotproduct( tgt_dir );

	float f			= Random.randF( 0.5f, 0.8f ); //(0.5f,1.f);
	if ( (f < ricoshet_factor) && !mtl->Flags.test(SGameMtl::flNoRicoshet) && bullet->flags.allow_ricochet )	
	{
		// уменьшение скорости полета в зависимости от угла падения пули (чем прямее угол, тем больше потеря)
		bullet->flags.allow_ricochet = 0;
		float scale = 1.0f - std::abs(bullet->dir.dotproduct(hit_normal)) * m_fCollisionEnergyMin;
		clamp(scale, 0.0f, m_fCollisionEnergyMax);
		speed_scale = scale;

		// вычисление рикошета, делается немного фейком, т.к. пуля остается в точке столкновения
		// и сразу выходит из RayQuery()
		bullet->dir.set				(tgt_dir);
		bullet->bullet_pos			= end_point;
		bullet->flags.ricochet_was	= 1;

	}
	else if ( shoot_factor < EPS )
	{
		//застрявание пули в материале
		speed_scale = 0.0f;
	}
	else
	{
		//пробивание материала
		speed_scale = shoot_factor;//mtl->fShootFactor;
		
		bullet->bullet_pos.mad(bullet->bullet_pos,bullet->dir,EPS);//fake
		//ввести коэффициент случайности при простреливании
		Fvector rand_normal;
		float cos = fabs(hit_normal.dotproduct(bullet->dir));
		float normal2dir2angle = rad2deg(acos(cos));

		if (normal2dir2angle >= 0.f && normal2dir2angle < 45.f)
		{
			rand_normal.random_dir(bullet->dir, deg2rad(8.f), Random);
		}
		else if (normal2dir2angle >= 45.0f && normal2dir2angle <= 90.0f)
		{
			float angle = Random.randF(8.f, 15.0f);
			rand_normal.random_dir(bullet->dir, deg2rad(angle), Random);
		}
		bullet->dir.set(rand_normal);
	}

	//уменьшить скорость в зависимости от простреливаемости
	bullet->speed *= speed_scale;
	//сколько энергии в процентах потеряла пуля при столкновении
	float energy_lost = 1.0f - bullet->speed / old_speed;
	//импульс переданный объекту равен прямопропорционален потерянной энергии
	hit_res->impulse = bullet->hit_param.impulse * speed_factor * energy_lost;

	return true;
}
