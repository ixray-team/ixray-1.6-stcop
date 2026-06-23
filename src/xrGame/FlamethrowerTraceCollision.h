#pragma once

#include "stdafx.h"

#include "../xrCore/_stl_extensions.h"
#include "../xrEngine/Feel_Touch.h"
#include "../xrCore/Save/SaveObject.h"

class CBulletManager;

/*namespace PAPI
{
	class pVector;
}*/

class CParticlesObject;
class CFlamethrower;
class ENGINE_API CObject;

namespace FlamethrowerTrace
{
	class CManager;

	enum class ETraceState
	{
		Idle,
		Air,
		AirToGround,
		Ground,
		End,
		MAX
	};

	struct STraceConstants
	{
		CManager* Manager = nullptr;
		float LifeTime = 0.0f;
		float LifeTimeCollided = 0.0f;
		float Velocity = 0.0f;
		float GravityAcceleration = 0.0f;
		float m_FlameFadeTime = 0.0f; 
		float m_RadiusMin = 0.0f;
		float m_RadiusMax = 0.0f;
		float m_RadiusMaxTime = 0.0f;
		float m_RadiusCollided = 0.0f;
		float m_RadiusCollidedInterpTime = 0.0f;

		shared_str m_sFlameParticles;
		shared_str m_sFlameParticlesGround;
	};

	class CTrace;
	
	class CPoint
	{
		friend ISaveObject& operator<<(ISaveObject& Object, CPoint& Data);

		struct TraceData {
			CPoint* TracedObj = nullptr;
			float HitDist;
			bool Penetrate = false;
		};

		//CManager* Manager = nullptr; //--
		CTrace* Trace = nullptr;
		//ETraceState State = ETraceState::MAX;
		Fvector PointPosition{};
		//Fvector LastUpdatedPos{};
		Fvector PointDirection{};
		//float LifeTime = 0.0f; //--
		//float LifeTimeCollided = 0.0f; //--
		//float Velocity = 0.0f; //--
		float GravityVelocity = 0.0f;
		//float GravityAcceleration = 0.0f; //--
		float CurrentTime = 0.0f;
		//float LastUpdateTime = 0.0f;
		float TimeOnCollide = 0.0f;
		//float RadiusCollidedInterpTime = 0.0f;
		bool Collided = false;

		static bool	hit_callback(const collide::rq_result& result, LPVOID params);
		static bool test_callback(const collide::ray_defs& rd, CObject* object, LPVOID params);
	
	public:
		CPoint() = default;
		CPoint(CTrace& Trace);

		//void SetManager(CManager& Manager) { this->Manager = &Manager; }
		void SetTrace(CTrace& Trace) { this->Trace = &Trace; }

		void Activate();
		//void Update(float DeltaTime);

		void UpdateAir(float delta_time);
		void UpdateAirToGround(float delta_time);
		void UpdateGround(float delta_time);
		void UpdateEnd(float delta_time);
		
		bool VerifySpawnPos(const Fvector& Position, const Fvector& Direction, Fvector& HitPos);
		void Deactivate();
		const Fvector& GetPosition() const { return PointPosition; }
		const Fvector& GetDirection() const { return PointDirection; }
		void SetTransform(Fvector PointPosition, Fvector PointDirection)
		{
			this->PointPosition = PointPosition;
			this->PointDirection = PointDirection;
		}

		//void SetState(ETraceState trace_state) { State = trace_state; }
		float GetGravityVelocity() const { return GravityVelocity; }
		//float GetLastUpdateTime() const { return LastUpdateTime; }
		float GetPointCurrentTime() const { return CurrentTime; }
		void SetGravityVelocity(float GravityVelocity) { this->GravityVelocity = GravityVelocity; }
		//void SetLastUpdateTime(float LastUpdateTime) { this->LastUpdateTime = LastUpdateTime; }
		void SetCurrentTime(float CurrentTime) { this->CurrentTime = CurrentTime; }
		//bool IsActive() const { return State != ETraceState::Idle; }
		//ETraceState GetState() const { return State; }
		bool IsCollided() const { return Collided; }
	};
	
	ISaveObject& operator<<(ISaveObject& Object, CPoint& Data);

	class CCollision :
		public Feel::Touch
	{
		friend ISaveObject& operator<<(ISaveObject& Object, CCollision& Data);

		//CManager* Manager;
		CTrace* Trace;
		CPoint* AttachPoint;
		ETraceState m_State = ETraceState::Idle;
		//float m_current_time = 0.0f;
		//float m_time_on_collide = 0.0f;
		float RadiusCurrent;
		float RadiusOnCollide;

		//shared_str m_sFlameParticles;
		//shared_str m_sFlameParticlesGround;

		//float m_last_update_time;

		//float m_LifeTime = 0.0f; //--!
		//float m_LifeTimeCollidedMax = 0.0f; //--!
		//float m_FlameFadeTime = 0.0f; //--
		//float m_RadiusMin = 0.0f; //--
		//float m_RadiusMax = 0.0f; //--
		//float m_RadiusMaxTime = 0.0f; //--
		//float m_RadiusCollided = 0.0f; //--
		//float m_RadiusCollidedInterpTime = 0.0f; //--
		//Fvector m_RadiusCollisionCoeff{};
		//Fvector m_RadiusCollisionCollidedCoeff{};
		//Fvector CollidedParticlePivot{};

		//shared_str m_particle_alpha_air_PE_name;
		//shared_str m_particle_alpha_ground_PE_name;
		//shared_str m_particle_size_air_PE_name;
		//shared_str m_particle_size_ground_PE_name;

		bool bIsActive = false;
		mutable bool IsLaunched = false;
		bool bIsCollided = false;

		struct FlamethrowerTraceData {
			CCollision* TracedObj = nullptr;
			float HitDist;
		};

	public:
		CCollision() = default;
		CCollision(CTrace& Trace);
		virtual ~CCollision();

		//void SetManager(CManager& Manager) { this->Manager = &Manager; }
		void SetTrace(CTrace& Trace) { this->Trace = &Trace; }

		//inline CManager* GetParent() const { return Manager; }
		void AttachToPoint(CPoint& point);
		//inline CFlamethrower* GetParentWeapon() const;

		//inline bool IsActive() const { return m_State != ETraceState::Idle; }
		//inline bool IsCollided() const { return m_State == ETraceState::AirToGround || m_State == ETraceState::Ground; }
		bool IsReadyToUpdateCollisions() const;
		float GetCurrentRadius() const;
		//inline float GetCurrentLifeTime() const { return m_current_time; }
		//inline float GetLastUpdateTime() const { return m_last_update_time; }
		//inline ETraceState GetTraceState() const { return m_State; }

		//void SetCurrentLifeTime(const float Time);
		//void SetLastUpdateTime(const float Time) { m_last_update_time = Time; }
		//void SetTraceState(const ETraceState State) { m_State = State; }

		void	feel_touch_new(CObject* O) override;
		void	feel_touch_delete(CObject* O) override;
		bool	feel_touch_contact(CObject* O) override;

		void Activate();
		void Deactivate();
		//void Update(float DeltaTime);
		
		void UpdateAir(float DeltaTime);
		void UpdateAirToGround(float DeltaTime);
		void UpdateGround(float DeltaTime);
		void UpdateEnd(float DeltaTime);

		const Fvector& GetPosition() const;
	};
	
	ISaveObject& operator<<(ISaveObject& Object, CCollision& Data);

	class CTrace
	{
		friend ISaveObject& operator<<(ISaveObject& Object, CTrace& Data);
		
		CPoint Point;
		CCollision Collision;

		const STraceConstants* Constants = nullptr;
		ETraceState State = ETraceState::MAX;
		float CurrentTime = 0.0f;
		float TimeOnCollide = 0.0f;
		
		void UpdateAir(float DeltaTime);
		void UpdateAirToGround(float DeltaTime);
		void UpdateGround(float DeltaTime);
		void UpdateEnd(float DeltaTime);

	public:
		CTrace() : Point(*this), Collision(*this) { Collision.AttachToPoint(Point); };
		CTrace(const STraceConstants& Constants) : Point(*this), Collision(*this),
		                                                        Constants(&Constants)
		{
			Collision.AttachToPoint(Point);
		}

		void SetConstants(const STraceConstants& Constants) { this->Constants = &Constants;}
		
		void Activate();
		void Deactivate();
		void Update(float DeltaTime);
		
		inline bool IsActive() const { return State != ETraceState::Idle; }

		const STraceConstants& GetConstants() const { VERIFY(Constants); return *Constants; }
		float GetCurrentTime() const {return CurrentTime;}
		float GetTimeOnCollide() const {return TimeOnCollide;}

		CPoint& GetPoint() {return Point;}
		CCollision& GetCollision() {return Collision;}
		const CPoint& GetPoint() const {return Point;}
		const CCollision& GetCollision() const {return Collision;}
	};
	
	ISaveObject& operator<<(ISaveObject& Object, CTrace& Data);
	
	class CManager :
		public Feel::Touch
	{
		friend ISaveObject& operator<<(ISaveObject& Object, CManager& Data);
	
	#ifdef DEBUG
		friend CBulletManager;
	#endif

		STraceConstants Constants;
	
		CFlamethrower* m_flamethrower;
		shared_str CollisionSection;
	
		using FOverlappedObjects = xr_vector<CCreature*>;
		using FCollisions = xr_vector<CCollision*>;
	
		FOverlappedObjects Overlapped;

		//using CollisionTrace = xr_pair<xr_unique_ptr<CPoint>, xr_unique_ptr<CCollision>>;
		//using CollisionTracePtr = CollisionTrace*;
		xr_deque<CTrace*> InactiveTraces;
		xr_deque<CTrace*> ActiveTraces;
	
		float m_RadiusMax = 0.0f;
		
		void SerializeElem(ISaveObject& Object, CTrace& Elem);
	
	public:
	
	//#ifdef DEBUG
		CTrace* LastLaunched = nullptr;
	//#endif
	
		CManager(CFlamethrower* flamethrower);
		~CManager();
	
		void	feel_touch_new(CObject* O) override;
		void	feel_touch_delete(CObject* O) override;
		bool	feel_touch_contact(CObject* O) override;
	
		void Load(str_c section);

		void save(NET_Packet& output_packet);
		void load(IReader& input_packet);
		void Serialize(ISaveObject& Object);

		void Update(float DeltaTime);
	
		void RegisterOverlapped(CCreature* enemy);
		void UnregisterOverlapped(CCreature* enemy);
		const FOverlappedObjects& GetOverlapped();
	
		inline CFlamethrower* GetParent() const { return m_flamethrower; }
	
		void OnShootingEnd();
	
		void LaunchTrace(const Fvector& StartPos, const Fvector& StartDir, bool Force = false);
	
		const shared_str& GetSection() { return CollisionSection; }
	};

	ISaveObject& operator<<(ISaveObject& Object, CManager& Data);

}
