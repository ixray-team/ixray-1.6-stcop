#include "stdafx.h"
#include "FlamethrowerTraceCollision.h"
#include "Creature.h"
#include "Flamethrower.h"
#include "../xrEngine/xr_collide_form.h"
#include "Level_Bullet_Manager.h"
#include "../xrEngine/GameMtlLib.h"
#include "../xrCore/_vector3d_ext.h"

void FlamethrowerTrace::CPoint::UpdateAir(float delta_time)
{
	if(!IsCollided() && !fis_zero(delta_time, EPS))
	{
		GravityVelocity += Trace->GetConstants().GravityAcceleration * delta_time;
		Fvector OldPos = PointPosition;
		PointPosition = (PointPosition + PointDirection * Trace->GetConstants().Velocity * delta_time) - Fvector{0.0f, GravityVelocity* delta_time, 0.0f};
		PointDirection = (PointPosition - OldPos).GetNormalizedCopy();
		collide::rq_results storage;
		collide::ray_defs RD(OldPos, PointPosition, CDB::OPT_FULL_TEST, collide::rqtBoth);
		TraceData data;
		data.TracedObj = this;
		if (Level().ObjectSpace.RayQuery(storage, RD, hit_callback, &data, test_callback, nullptr) && !data.Penetrate)
		{
			PointPosition = OldPos + PointDirection * data.HitDist;
			//State = ETraceState::AirToGround;
			//TimeOnCollide = CurrentTime;
			Collided = true;
			//return;
		}
	}
	/*if(CurrentTime >= LifeTime)
	{
		TimeOnCollide = CurrentTime;
		State = ETraceState::End;
	}*/
}

void FlamethrowerTrace::CPoint::UpdateAirToGround(float delta_time)
{
	/*if((TimeOnCollide + RadiusCollidedInterpTime) - CurrentTime <= 0)
	{
		State = ETraceState::Ground;
	}*/
}

void FlamethrowerTrace::CPoint::UpdateGround(float delta_time)
{
	/*if(CurrentTime >= LifeTimeCollided)
	{
		TimeOnCollide = CurrentTime;
		State = ETraceState::End;
	}*/
}

void FlamethrowerTrace::CPoint::UpdateEnd(float delta_time)
{
	/*if( (TimeOnCollide+RadiusCollidedInterpTime)- CurrentTime <= 0)
	{
		//State = ETraceState::Idle;
	}*/
}

bool FlamethrowerTrace::CPoint::hit_callback(collide::rq_result& result, LPVOID params)
{
	TraceData* pData = static_cast<TraceData*>(params);
	if (!result.O)
	{
		CDB::TRI const& triangle = Level().ObjectSpace.GetStaticTris()[result.element];
		SGameMtl* mtl = GMLib.GetMaterialByIdx(triangle.material);
		if (!fsimilar(mtl->fShootFactor, 0.0f)) // if not penetrate
		{
			pData->HitDist = result.range;
			return false;
		}
	}
	pData->Penetrate = true;
	return true;
}

bool FlamethrowerTrace::CPoint::test_callback(const collide::ray_defs& rd, CObject* object, LPVOID params)
{
	TraceData* pData = static_cast<TraceData*>(params);

	if (object)
	{
		CEntity* entity = smart_cast<CEntity*>(object);
		if (!entity)
		{
			return true;
		}
		if (entity->ID() == pData->TracedObj->Trace->GetConstants().Manager->GetParent()->H_Parent()->ID())
		{
			return false;
		}
	}
	return true;
}

FlamethrowerTrace::CPoint::CPoint(CTrace& Trace) : Trace(&Trace)
{
	//State = ETraceState::Idle;
	//LifeTime = pSettings->r_float(Manager->GetSection(), "LifeTime");
	//LifeTimeCollided = pSettings->r_float(Manager->GetSection(), "LifeTimeCollided");
	//GravityAcceleration = pSettings->r_float(Manager->GetSection(), "GravityAcceleration");
	//RadiusCollidedInterpTime = pSettings->r_float(Manager->GetSection(), "RadiusCollidedInterpTime");
	//Velocity = pSettings->r_float(Manager->GetSection(), "Velocity");
}

void FlamethrowerTrace::CPoint::Activate()
{
	//State = ETraceState::Air;
	//LastUpdatedPos = PointPosition;
	Collided = false;
}

/*void FlamethrowerTrace::CPoint::Update(float DeltaTime)
{
	VERIFY(State != ETraceState::MAX);
	switch (State)
	{
	case ETraceState::Idle:
		{
			return;
		}
	case ETraceState::Air:
		{
		UpdateAir(DeltaTime);
			break;
		}
	case ETraceState::AirToGround:
		{
		UpdateAirToGround(DeltaTime);
		break;
		}
	case ETraceState::Ground:
		{
		UpdateGround(DeltaTime);
			break;
		}
	case ETraceState::End:
		{
		UpdateEnd(DeltaTime);
			break;
		}
	}
	LastUpdateTime = CurrentTime;
	CurrentTime += DeltaTime;
}*/

bool FlamethrowerTrace::CPoint::VerifySpawnPos(const Fvector& Position, const Fvector& Direction, Fvector& HitPos)
{
	collide::rq_results storage;
	collide::ray_defs RD(Position-Direction, Position, CDB::OPT_FULL_TEST, collide::rqtBoth);
	TraceData data;
	data.TracedObj = this;
	if (Level().ObjectSpace.RayQuery(storage, RD, hit_callback, &data, test_callback, nullptr))
	{
		HitPos = Position+Direction * (data.HitDist - 1.0f);
		return false;
	}
	return true;
}

void FlamethrowerTrace::CPoint::Deactivate()
{
	CurrentTime = 0.0f;
	GravityVelocity = 0.0f;
	TimeOnCollide = 0.0f;
	//LastUpdateTime = 0.0f;
	PointPosition = {};
	//LastUpdatedPos = {};
	PointDirection = {};
}

ISaveObject& FlamethrowerTrace::operator<<(ISaveObject& Object, CPoint& Data)
{
	BEGIN_CHUNK(Object, "CPoint")
	{
		Object /*<< Data.State*/ << Data.PointPosition << Data.PointDirection << Data.GravityVelocity << Data.CurrentTime << Data.TimeOnCollide;
	}
	return Object;
}

void FlamethrowerTrace::CCollision::UpdateAir(float DeltaTime)
{
	float interpTime = std::min(Trace->GetCurrentTime() / Trace->GetConstants().m_RadiusMaxTime, 1.0f);
	RadiusCurrent = Trace->GetConstants().m_RadiusMin + (Trace->GetConstants().m_RadiusMax - Trace->GetConstants().m_RadiusMin) * interpTime;
	clamp(RadiusCurrent, Trace->GetConstants().m_RadiusMin, Trace->GetConstants().m_RadiusMax);

	if (!bIsCollided&&AttachPoint->IsCollided())
	{
		//m_State = ETraceState::AirToGround;
		bIsCollided = true;
		VERIFY2(Trace->GetConstants().m_RadiusCollided > 0.01, "Too small RadiusCollided in flamethrower config!");
		RadiusOnCollide = RadiusCurrent;
		//m_time_on_collide = m_current_time;
		return;
	}
	/*if(m_current_time >= m_LifeTime)
	{
		m_State = ETraceState::End;
	}*/
}

void FlamethrowerTrace::CCollision::UpdateAirToGround(float DeltaTime)
{
	float interpTime = (Trace->GetCurrentTime() - Trace->GetTimeOnCollide()) / Trace->GetConstants().m_RadiusCollidedInterpTime;
	if(interpTime >= 1.0f)
	{
		//m_State = ETraceState::Ground;
		interpTime = 1.0f;
	}
	const float AlphaValue = 1.0f -std::pow(1.0f - interpTime, 2.0f);
	RadiusCurrent = std::max(RadiusOnCollide, AlphaValue * Trace->GetConstants().m_RadiusCollided);
}

void FlamethrowerTrace::CCollision::UpdateGround(float DeltaTime)
{
	/*if (m_current_time >= m_LifeTimeCollidedMax)
	{
		m_current_time = 0;
		m_State = ETraceState::End;
	}*/
}

void FlamethrowerTrace::CCollision::UpdateEnd(float DeltaTime)
{
	/*const float interpTime = 1.0f - (Trace->GetCurrentTime() / m_RadiusCollidedInterpTime);
	if(interpTime <= 0)
	{
		//Deactivate();
		return;
	}*/
}

FlamethrowerTrace::CCollision::CCollision(CTrace& Trace) : Trace(&Trace)
{
	//m_RadiusMin = pSettings->r_float(Manager->GetSection(), "RadiusMin");
	//m_RadiusMax = pSettings->r_float(Manager->GetSection(), "RadiusMax");
	//m_RadiusCollided = pSettings->r_float(Manager->GetSection(), "RadiusCollided");
	//m_RadiusCollidedInterpTime = pSettings->r_float(Manager->GetSection(), "RadiusCollidedInterpTime");
	//m_RadiusCollisionCoeff = pSettings->r_fvector3(Manager->GetSection(), "RadiusCollisionCoeff");
	//m_RadiusCollisionCollidedCoeff = pSettings->r_fvector3(Manager->GetSection(), "RadiusCollisionCollidedCoeff");
	//m_RadiusMaxTime = pSettings->r_float(Manager->GetSection(), "RadiusMaxTime");
	//m_LifeTime = pSettings->r_float(Manager->GetSection(), "LifeTime");
	//m_LifeTimeCollidedMax = pSettings->r_float(Manager->GetSection(), "LifeTimeCollided");
	//m_FlameFadeTime = pSettings->r_float(Manager->GetSection(), "FlameFadeTime");
	//CollidedParticlePivot = pSettings->r_fvector3(Manager->GetSection(), "CollidedParticlePivot");

	// flames
	//m_sFlameParticles = pSettings->r_string(Manager->GetSection(), "flame_particles");
	//m_sFlameParticlesGround = pSettings->r_string(Manager->GetSection(), "earth_flame_particles");
	//m_particle_size_air_PE_name = pSettings->r_string(Manager->GetSection(), "air_flame_size_bind");
	//m_particle_alpha_air_PE_name = pSettings->r_string(Manager->GetSection(), "air_flame_alpha_bind");
	//m_particle_size_ground_PE_name = pSettings->r_string(Manager->GetSection(), "earth_flame_size_bind");
	//m_particle_alpha_ground_PE_name = pSettings->r_string(Manager->GetSection(), "earth_flame_alpha_bind");
}

FlamethrowerTrace::CCollision::~CCollision()
{
	if (bIsActive) {
		Deactivate();
	}
}

void FlamethrowerTrace::CCollision::AttachToPoint(CPoint& point)
{
	AttachPoint = &point;
}

/*inline CFlamethrower* FlamethrowerTrace::CCollision::GetParentWeapon() const
{
	return Manager->GetParent();
}*/

bool FlamethrowerTrace::CCollision::IsReadyToUpdateCollisions() const
{
	//return true;
	if (!bIsActive)
	{
		return false;
	}
	if(!IsLaunched && bIsActive)
	{
		IsLaunched = true;
		return false;
	}
	//float Dist = GetCurrentRadius()*0.8;
	if(bIsCollided)
	{
		/*if(m_last_update_time > 0.2)
		{
			m_last_update_time = 0.0f;
			return true;
		}*/
		return false;
	}
	return true;
}

float FlamethrowerTrace::CCollision::GetCurrentRadius() const
{
	return RadiusCurrent;
}

/*void FlamethrowerTrace::CCollision::SetCurrentLifeTime(const float Time)
{
	m_current_time = Time;
	float interpTime = std::min(m_current_time / m_RadiusMaxTime, 1.0f);
	RadiusCurrent = m_RadiusMin + (m_RadiusMax - m_RadiusMin) * interpTime;
}*/

void FlamethrowerTrace::CCollision::feel_touch_new(CObject* O)
{
	if (!bIsActive) {
		return;
	}
	if (CCreature* Casted = O->cast_creature()) {
		Trace->GetConstants().Manager->RegisterOverlapped(Casted);
	}
}

void FlamethrowerTrace::CCollision::feel_touch_delete(CObject* O)
{
	if (CCreature* Casted = O->cast_creature()) {
		Trace->GetConstants().Manager->UnregisterOverlapped(Casted);
	}
}

bool FlamethrowerTrace::CCollision::feel_touch_contact(CObject* O)
{
	return false;
}

void FlamethrowerTrace::CCollision::Activate()
{
	//m_State = ETraceState::Air;
	RadiusCurrent = Trace->GetConstants().m_RadiusMin;
	IsLaunched = false;
	bIsActive = true;
	bIsCollided = false;
}

void FlamethrowerTrace::CCollision::Deactivate()
{
	//m_State = ETraceState::Idle;
	bIsActive = false;
	//m_current_time = 0.0f;
	//m_time_on_collide = 0.0f;
	RadiusOnCollide = 0.0f;
}

/*void FlamethrowerTrace::CCollision::Update(float DeltaTime)
{
	switch (m_State)
	{
	case ETraceState::Idle:
		{
			return;
		}
	case ETraceState::Air:
		{
		Update_Air(DeltaTime);
			break;
		}
	case ETraceState::AirToGround:
		{
		Update_AirToGround(DeltaTime);
			break;
		}
	case ETraceState::Ground:{
			Update_Ground(DeltaTime);
			break;
		}
	case ETraceState::End:
		{
		Update_End(DeltaTime);
			break;
		}
	}
	m_current_time += DeltaTime;
	m_last_update_time += DeltaTime;

}*/

const Fvector& FlamethrowerTrace::CCollision::GetPosition() const
{
	return AttachPoint->GetPosition();
}

ISaveObject& FlamethrowerTrace::operator<<(ISaveObject& Object, CCollision& Data)
{
	BEGIN_CHUNK(Object, "CCollision")
	{
		Object /*<< Data.m_State << Data.m_current_time << Data.m_time_on_collide*/ << Data.RadiusCurrent << Data.RadiusOnCollide;
	}
	return Object;
}

void FlamethrowerTrace::CTrace::UpdateAir(float DeltaTime)
{
	Point.UpdateAir(DeltaTime);
	Collision.UpdateAir(DeltaTime);
	if (Point.IsCollided())
	{
		State = ETraceState::AirToGround;
		TimeOnCollide = CurrentTime;
		return;
	}
	if(CurrentTime >= Constants->LifeTime)
	{
		State = ETraceState::End;
	}
}

void FlamethrowerTrace::CTrace::UpdateAirToGround(float DeltaTime)
{
	Point.UpdateAirToGround(DeltaTime);
	Collision.UpdateAirToGround(DeltaTime);
	if((TimeOnCollide + Constants->m_RadiusCollidedInterpTime) - CurrentTime <= 0)
	{
		State = ETraceState::Ground;
	}
}

void FlamethrowerTrace::CTrace::UpdateGround(float DeltaTime)
{
	Point.UpdateGround(DeltaTime);
	Collision.UpdateGround(DeltaTime);
	if (CurrentTime >= Constants->LifeTimeCollided)
	{
		TimeOnCollide = CurrentTime;
		State = ETraceState::End;
	}
}

void FlamethrowerTrace::CTrace::UpdateEnd(float DeltaTime)
{
	Point.UpdateEnd(DeltaTime);
	Collision.UpdateEnd(DeltaTime);
	if( (TimeOnCollide+Constants->m_RadiusCollidedInterpTime)- CurrentTime <= 0)
	{
		State = ETraceState::Idle;
	}
}

void FlamethrowerTrace::CTrace::Activate()
{
	State = ETraceState::Air;
	CurrentTime = 0.0f;
	TimeOnCollide = 0.0f;
	Point.Activate();
	Collision.Activate();
}

void FlamethrowerTrace::CTrace::Deactivate()
{
	State = ETraceState::Idle;
	Point.Deactivate();
	Collision.Deactivate();
}

void FlamethrowerTrace::CTrace::Update(float DeltaTime)
{
	VERIFY(State != ETraceState::MAX);
	switch (State)
	{
	case ETraceState::Idle:
		{
			return;
		}
	case ETraceState::Air:
		{
			UpdateAir(DeltaTime);
			break;
		}
	case ETraceState::AirToGround:
		{
			UpdateAirToGround(DeltaTime);
			break;
		}
	case ETraceState::Ground:
		{
			UpdateGround(DeltaTime);
			break;
		}
	case ETraceState::End:
		{
			UpdateEnd(DeltaTime);
			break;
		}
	}
	//LastUpdateTime = CurrentTime;
	CurrentTime += DeltaTime;
}

ISaveObject& FlamethrowerTrace::operator<<(ISaveObject& Object, CTrace& Data)
{
	BEGIN_CHUNK(Object, "CTrace")
	{
		Object << Data.Point << Data.Collision << Data.CurrentTime << Data.TimeOnCollide << Data.State;
	}
	return Object;
}

void FlamethrowerTrace::CManager::SerializeElem(ISaveObject& Object, CTrace& Elem)
{
	BEGIN_CHUNK(Object, "CFlamethrowerTrace::ActiveTrace")
	{
		Object << Elem;
		if (!Object.IsSave())
		{
			Elem.SetConstants(Constants);
			/*Elem.first->SetManager(this);
			Elem.second->SetManager(this);
			Elem.second->AttachToPoint(Elem.first.get());*/
		}
	}
}

FlamethrowerTrace::CManager::CManager(CFlamethrower* flamethrower) : m_flamethrower(flamethrower)
{

#ifdef DEBUG
	Level().BulletManager().MarkFlamethrowerTraceToDraw(this);
#endif
}

FlamethrowerTrace::CManager::~CManager()
{
#ifdef DEBUG
	Level().BulletManager().UnmarkFlamethrowerTraceToDraw(this);
#endif
	for (auto elem : InactiveTraces)
	{
		xr_delete(elem);
	}
	for (auto elem : ActiveTraces)
	{
		xr_delete(elem);
	}
}

void FlamethrowerTrace::CManager::feel_touch_new(CObject* O)
{
	Touch::feel_touch_new(O);
	if (auto casted = O->cast_creature())
	{
		Overlapped.push_back(casted);
	}
}

void FlamethrowerTrace::CManager::feel_touch_delete(CObject* O)
{
	Touch::feel_touch_delete(O);
	if (auto casted = O->cast_creature())
	{
		Overlapped.erase(std::ranges::find(Overlapped, casted));
	}
}

bool FlamethrowerTrace::CManager::feel_touch_contact(CObject* O)
{
	if (CCreature* enemy = O->cast_creature()) {
		for (auto& elem : ActiveTraces) {
			if (!elem->GetCollision().IsReadyToUpdateCollisions()) {
				continue;
			}
			Fsphere S;
			S.P = elem->GetCollision().GetPosition();
			S.R = elem->GetCollision().GetCurrentRadius();
			if (S.intersect(enemy->SpatialComponent->sphere))
			{
				return true;
			}
		}
	}
	return false;
}

void FlamethrowerTrace::CManager::Load(str_c section)
{
	for (auto& elem : ActiveTraces) {
		xr_delete(elem);
	}
	for (auto& elem : InactiveTraces) {
		xr_delete(elem);
	}
	CollisionSection = section;
	ActiveTraces.clear();
	InactiveTraces.clear();

	Constants.Manager = this;
	Constants.LifeTime = pSettings->r_float(section, "LifeTime");
	Constants.LifeTimeCollided = pSettings->r_float(section, "LifeTimeCollided");
	Constants.GravityAcceleration = pSettings->r_float(section, "GravityAcceleration");
	//Constants.m_RadiusCollidedInterpTime = pSettings->r_float(section, "RadiusCollidedInterpTime");
	Constants.Velocity = pSettings->r_float(section, "Velocity");
	
	Constants.m_RadiusMin = pSettings->r_float(section, "RadiusMin");
	Constants.m_RadiusMax = pSettings->r_float(section, "RadiusMax");
	Constants.m_RadiusCollided = pSettings->r_float(section, "RadiusCollided");
	Constants.m_RadiusCollidedInterpTime = pSettings->r_float(section, "RadiusCollidedInterpTime");
	//Constants.m_RadiusCollisionCoeff = pSettings->r_fvector3(Manager->GetSection(), "RadiusCollisionCoeff");
	//Constants.m_RadiusCollisionCollidedCoeff = pSettings->r_fvector3(Manager->GetSection(), "RadiusCollisionCollidedCoeff");
	Constants.m_RadiusMaxTime = pSettings->r_float(section, "RadiusMaxTime");
	//Constants.m_LifeTime = pSettings->r_float(Manager->GetSection(), "LifeTime");
	//Constants.m_LifeTimeCollidedMax = pSettings->r_float(Manager->GetSection(), "LifeTimeCollided");
	Constants.m_FlameFadeTime = pSettings->r_float(section, "FlameFadeTime");
	//Constants.CollidedParticlePivot = pSettings->r_fvector3(Manager->GetSection(), "CollidedParticlePivot");

	Constants.m_sFlameParticles = pSettings->r_string(section, "flame_particles");
	Constants.m_sFlameParticlesGround = pSettings->r_string(section, "earth_flame_particles");
	
	int StartNum = pSettings->r_u16(section, "trace_collision_num_start");
	for (int i = 0; i < StartNum; ++i) {
		InactiveTraces.push_back(new CTrace(Constants));
		/*InactiveTraces.back()->first = xr_make_unique<CPoint>(this);
		InactiveTraces.back()->second = xr_make_unique<CCollision>(this);
		InactiveTraces.back()->second->AttachToPoint(InactiveTraces.back()->first.get());*/
	}
	m_RadiusMax = pSettings->r_float(section, "RadiusMax");
}

void FlamethrowerTrace::CManager::save(NET_Packet& output_packet)
{
	VERIFY(false);
}

void FlamethrowerTrace::CManager::load(IReader& input_packet)
{
	VERIFY(false);
}

void FlamethrowerTrace::CManager::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object, "CFlamethrowerTrace")
	{
		((CSaveObject&)Object).Serialize(ActiveTraces, fastdelegate::MakeDelegate(this, &CManager::SerializeElem));
	}
}

void FlamethrowerTrace::CManager::Update(float DeltaTime)
{
	if(ActiveTraces.empty())
	{
		return;
	}
	
	Fvector Center{};
	float Radius = 0.0f;
	uint16_t Num = 0;
	
	auto FirstElem = ActiveTraces.front();
	ActiveTraces.pop_front();
	ActiveTraces.push_back(FirstElem);
	auto CurrentElem = FirstElem;
	do
	{
		CurrentElem = ActiveTraces.front();
		ActiveTraces.pop_front();
		CurrentElem->Update(DeltaTime);
		/*CurrentElem->first->Update(DeltaTime);
		CurrentElem->second->Update(DeltaTime);*/
		/*auto PointActive = CurrentElem->first->IsActive();
		auto CollisionActive = CurrentElem->second->IsActive();
		VERIFY(PointActive && CollisionActive);
		if(PointActive && CollisionActive)*/
		if (CurrentElem->IsActive())
		{
			Center += CurrentElem->GetCollision().GetPosition();
			++Num;
			ActiveTraces.push_back(CurrentElem);
		}
		else
		{
			CurrentElem->Deactivate();
			/*CurrentElem->first->Deactivate();
			CurrentElem->second->Deactivate();*/
			InactiveTraces.push_back(CurrentElem);
		}
	} while (CurrentElem != FirstElem);
	
	if(Num)
	{
		Center /= Num;
	}
	if (Num == 1)
	{
		Radius = ActiveTraces.front()->GetCollision().GetCurrentRadius();
	}
	else if (Num > 1)
	{
		float MaxRadius = 0.0f;
		for (auto& elem : ActiveTraces) {
			float Dist = Center.distance_to_sqr(elem->GetCollision().GetPosition());
			if(Dist > Radius)
			{
				Radius = Dist;
			}
			if (elem->GetCollision().GetCurrentRadius() > MaxRadius){
				MaxRadius = elem->GetCollision().GetCurrentRadius();
			}
		}
		Radius = sqrt(Radius) + MaxRadius;
	}
	feel_touch_update(Center, Radius);
}

void FlamethrowerTrace::CManager::RegisterOverlapped(CCreature* enemy)
{
	Overlapped.push_back(enemy);
}

void FlamethrowerTrace::CManager::UnregisterOverlapped(CCreature* enemy)
{
	Overlapped.erase(std::ranges::find(Overlapped, enemy));
}

const FlamethrowerTrace::CManager::FOverlappedObjects& FlamethrowerTrace::CManager::GetOverlapped()
{
	return Overlapped;
}

//bool Debug_block_shoot = false;

void FlamethrowerTrace::CManager::OnShootingEnd()
{
	//Debug_block_shoot = false;
	LastLaunched = nullptr;
}

void FlamethrowerTrace::CManager::LaunchTrace(const Fvector& StartPos, const Fvector& StartDir, bool Force)
{
	//if (Debug_block_shoot)
	//{
	//	return;
	//}
	//Debug_block_shoot = true;
	// Insert debug trace abort here if needed
	if(LastLaunched && LastLaunched->IsActive() && !Force)
	{
		auto dist = StartPos.distance_to_sqr(LastLaunched->GetCollision().GetPosition());
		auto RadiusSq = m_RadiusMax* m_RadiusMax;
		Msg("dist [%f], radius sq [%f]", dist, RadiusSq);
		if(dist <= RadiusSq)
		{
			return;
		}
	}
	CTrace* FirstTrace;
	if (InactiveTraces.empty())
	{
		FirstTrace = new CTrace(Constants);
		/*FirstTrace->first = xr_make_unique<CPoint>(this);
		FirstTrace->second = xr_make_unique<CCollision>(this);
		FirstTrace->second->AttachToPoint(FirstTrace->first.get());*/
	} else
	{
		FirstTrace = InactiveTraces.front();
		InactiveTraces.pop_front();
	}
	LastLaunched = FirstTrace;
	LastLaunched->GetPoint().SetTransform(StartPos, StartDir);
	FirstTrace->Activate();
	/*FirstTrace->first->Activate();
	FirstTrace->second->Activate();*/
	ActiveTraces.push_back(FirstTrace);
}

ISaveObject& FlamethrowerTrace::operator<<(ISaveObject& Object, CManager& Data)
{
	Data.Serialize(Object);
	return Object;
}
