#include "stdafx.h"
#include "particle_actions_collection.h"
#include "particle_holder.h"

using namespace PAPI;

void PAPI::PAAvoid::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt = magnitude * dt;
	
	switch(position.type)
	{
	case PDPlane:
		{
			if(look_ahead < P_MAXFLOAT)
			{
				for(u32 i = 0; i < pHolder->p_count; i++)
				{
					Particle &m = pHolder->particles[i];
					
					// p2 stores the plane normal (the a,b,c of the plane eqn).
					// Old and new distances: dist(p,plane) = n * p + d
					// radius1 stores -n*p, which is d.
					float dist = m.pos * position.p2 + position.radius1;
					
					if(dist < look_ahead)
					{
						float vm = m.vel.magnitude();
						Fvector Vn = m.vel / vm;
						// float dot = Vn * position.p2;
						
						Fvector tmp = (position.p2 * (magdt / (dist*dist+epsilon))) + Vn;
						m.vel = tmp * (vm / tmp.magnitude());
						if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
						{
							m.rot_vel = m.vel;
							m.rot_vel.normalize_safe();
						}
					}
				}
			}
			else
			{
				for(u32 i = 0; i < pHolder->p_count; i++)
				{
					Particle &m = pHolder->particles[i];
					
					// p2 stores the plane normal (the a,b,c of the plane eqn).
					// Old and new distances: dist(p,plane) = n * p + d
					// radius1 stores -n*p, which is d.
					float dist = m.pos * position.p2 + position.radius1;
					
					float vm = m.vel.magnitude();
					Fvector Vn = m.vel / vm;
					// float dot = Vn * position.p2;
					
					Fvector tmp = (position.p2 * (magdt / (dist*dist+epsilon))) + Vn;
					m.vel = tmp * (vm / tmp.magnitude());
					if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
					{
						m.rot_vel = m.vel;
						m.rot_vel.normalize_safe();
					}
				}
			}
		}
		break;
	case PDRectangle:
		{
			// Compute the inverse matrix of the plane basis.
			Fvector &u = position.u;
			Fvector &v = position.v;
			
			// The normalized bases are needed inside the loop.
			Fvector un = u / position.radius1Sqr;
			Fvector vn = v / position.radius2Sqr;
			
			// w = u cross v
			float wx = u.y*v.z-u.z*v.y;
			float wy = u.z*v.x-u.x*v.z;
			float wz = u.x*v.y-u.y*v.x;
			
			float det = 1/(wz*u.x*v.y-wz*u.y*v.x-u.z*wx*v.y-u.x*v.z*wy+v.z*wx*u.y+u.z*v.x*wy);
			
			Fvector s1((v.y*wz-v.z*wy), (v.z*wx-v.x*wz), (v.x*wy-v.y*wx));
			s1 *= det;
			Fvector s2((u.y*wz-u.z*wy), (u.z*wx-u.x*wz), (u.x*wy-u.y*wx));
			s2 *= -det;
			
			// See which particles bounce.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// See if particle's current and next positions cross plane.
				// If not, couldn't bounce, so keep going.
				Fvector pnext(m.pos + m.vel * dt * look_ahead);
				
				// p2 stores the plane normal (the a,b,c of the plane eqn).
				// Old and new distances: dist(p,plane) = n * p + d
				// radius1 stores -n*p, which is d.
				float distold = m.pos * position.p2 + position.radius1;
				float distnew = pnext * position.p2 + position.radius1;
				
				// Opposite signs if product < 0
				// There is no faster way to do this.
				if(distold * distnew >= 0)
					continue;
				
				float nv = position.p2 * m.vel;
				float t = -distold / nv;
				
				// Actual intersection point p(t) = pos + vel t
				Fvector phit(m.pos + m.vel * t);
				
				// Offset from origin in plane, p - origin
				Fvector offset(phit - position.p1);
				
				// Dot product with basis vectors of old frame
				// in terms of new frame gives position in uv frame.
				float upos = offset * s1;
				float vpos = offset * s2;
				
				// Did it cross plane outside triangle?
				if(upos < 0 || vpos < 0 || upos > 1 || vpos > 1)
					continue;
				
				// A hit! A most palpable hit!
				// Compute distance to the three edges.
				Fvector uofs = (un * (un * offset)) - offset;
				float udistSqr = uofs.square_magnitude();
				Fvector vofs = (vn * (vn * offset)) - offset;
				float vdistSqr = vofs.square_magnitude();
				
				Fvector foffset((u + v) - offset);
				Fvector fofs = (un * (un * foffset)) - foffset;
				float fdistSqr = fofs.square_magnitude();
				Fvector gofs = (un * (un * foffset)) - foffset;
				float gdistSqr = gofs.square_magnitude();
				
				Fvector S;
				if(udistSqr <= vdistSqr && udistSqr <= fdistSqr
					&& udistSqr <= gdistSqr) S = uofs;
				else if(vdistSqr <= fdistSqr && vdistSqr <= gdistSqr) S = vofs;
				else if(fdistSqr <= gdistSqr) S = fofs;
				else S = gofs;
				
				S.normalize_safe();
				
				// We now have a vector3 to safety.
				float vm = m.vel.magnitude();
				Fvector Vn = m.vel / vm;
				
				// Blend S into V.
				Fvector tmp = (S * (magdt / (t*t+epsilon))) + Vn;
				m.vel = tmp * (vm / tmp.magnitude());
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
		break;
	case PDTriangle:
		{
			// Compute the inverse matrix of the plane basis.
			Fvector &u = position.u;
			Fvector &v = position.v;
			
			// The normalized bases are needed inside the loop.
			Fvector un = u / position.radius1Sqr;
			Fvector vn = v / position.radius2Sqr;
			
			// f is the third (non-basis) triangle edge.
			Fvector f = v - u;
			Fvector fn(f);
			fn.normalize_safe();
			
			// w = u cross v
			float wx = u.y*v.z-u.z*v.y;
			float wy = u.z*v.x-u.x*v.z;
			float wz = u.x*v.y-u.y*v.x;
			
			float det = 1/(wz*u.x*v.y-wz*u.y*v.x-u.z*wx*v.y-u.x*v.z*wy+v.z*wx*u.y+u.z*v.x*wy);
			
			Fvector s1((v.y*wz-v.z*wy), (v.z*wx-v.x*wz), (v.x*wy-v.y*wx));
			s1 *= det;
			Fvector s2((u.y*wz-u.z*wy), (u.z*wx-u.x*wz), (u.x*wy-u.y*wx));
			s2 *= -det;
			
			// See which particles bounce.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// See if particle's current and next positions cross plane.
				// If not, couldn't bounce, so keep going.
				Fvector pnext(m.pos + m.vel * dt * look_ahead);
				
				// p2 stores the plane normal (the a,b,c of the plane eqn).
				// Old and new distances: dist(p,plane) = n * p + d
				// radius1 stores -n*p, which is d.
				float distold = m.pos * position.p2 + position.radius1;
				float distnew = pnext * position.p2 + position.radius1;
				
				// Opposite signs if product < 0
				// Is there a faster way to do this?
				if(distold * distnew >= 0)
					continue;
				
				float nv = position.p2 * m.vel;
				float t = -distold / nv;
				
				// Actual intersection point p(t) = pos + vel t
				Fvector phit(m.pos + m.vel * t);
				
				// Offset from origin in plane, p - origin
				Fvector offset(phit - position.p1);
				
				// Dot product with basis vectors of old frame
				// in terms of new frame gives position in uv frame.
				float upos = offset * s1;
				float vpos = offset * s2;
				
				// Did it cross plane outside triangle?
				if(upos < 0 || vpos < 0 || (upos + vpos) > 1)
					continue;
				
				// A hit! A most palpable hit!
				// Compute distance to the three edges.
				Fvector uofs = (un * (un * offset)) - offset;
				float udistSqr = uofs.square_magnitude();
				Fvector vofs = (vn * (vn * offset)) - offset;
				float vdistSqr = vofs.square_magnitude();
				Fvector foffset(offset - u);
				Fvector fofs = (fn * (fn * foffset)) - foffset;
				float fdistSqr = fofs.square_magnitude();
				Fvector S;
				if(udistSqr <= vdistSqr && udistSqr <= fdistSqr) S = uofs;
				else if(vdistSqr <= fdistSqr) S = vofs;
				else S = fofs;
				
				S.normalize_safe();
				
				// We now have a vector3 to safety.
				float vm = m.vel.magnitude();
				Fvector Vn = m.vel / vm;
				
				// Blend S into V.
				Fvector tmp = (S * (magdt / (t*t+epsilon))) + Vn;
				m.vel = tmp * (vm / tmp.magnitude());
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
		break;
	case PDDisc:
		{
			float r1Sqr = _sqr(position.radius1);
			float r2Sqr = _sqr(position.radius2);
			
			// See which particles bounce.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// See if particle's current and next positions cross plane.
				// If not, couldn't bounce, so keep going.
				Fvector pnext(m.pos + m.vel * dt * look_ahead);
				
				// p2 stores the plane normal (the a,b,c of the plane eqn).
				// Old and new distances: dist(p,plane) = n * p + d
				// radius1 stores -n*p, which is d. radius1Sqr stores d.
				float distold = m.pos * position.p2 + position.radius1Sqr;
				float distnew = pnext * position.p2 + position.radius1Sqr;
				
				// Opposite signs if product < 0
				// Is there a faster way to do this?
				if(distold * distnew >= 0)
					continue;
				
				// Find position at the crossing point by parameterizing
				// p(t) = pos + vel * t
				// Solve dist(p(t),plane) = 0 e.g.
				// n * p(t) + D = 0 ->
				// n * p + t (n * v) + D = 0 ->
				// t = -(n * p + D) / (n * v)
				// Could factor n*v into distnew = distold + n*v and save a bit.
				// Safe since n*v != 0 assured by quick rejection test.
				// This calc is indep. of dt because we have established that it
				// will hit before dt. We just want to know when.
				float nv = position.p2 * m.vel;
				float t = -distold / nv;
				
				// Actual intersection point p(t) = pos + vel t
				Fvector phit(m.pos + m.vel * t);
				
				// Offset from origin in plane, phit - origin
				Fvector offset(phit - position.p1);
				
				float rad = offset.square_magnitude();
				
				if(rad > r1Sqr || rad < r2Sqr)
					continue;
				
				// A hit! A most palpable hit!
				Fvector S = offset;
				S.normalize_safe();
				
				// We now have a vector3 to safety.
				float vm = m.vel.magnitude();
				Fvector Vn = m.vel / vm;
				
				// Blend S into V.
				Fvector tmp = (S * (magdt / (t*t+epsilon))) + Vn;
				m.vel = tmp * (vm / tmp.magnitude());
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
		break;
	case PDSphere:
		{
			float rSqr = position.radius1 * position.radius1;
			
			// See which particles are aimed toward the sphere.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// First do a ray-sphere intersection test and
				// see if it's soon enough.
				// Can I do this faster without t?
				float vm = m.vel.magnitude();
				Fvector Vn = m.vel / vm;
				
				Fvector L = position.p1 - m.pos;
				float v = L * Vn;
				
				float disc = rSqr - (L * L) + v * v;
				if(disc < 0)
					continue; // I'm not heading toward it.
				
				// Compute length for second rejection test.
				float t = v - _sqrt(disc);
				if(t < 0 || t > (vm * look_ahead))
					continue;
				
				// Get a vector3 to safety.
				Fvector C = Vn ^ L;
				C.normalize_safe();
				Fvector S = Vn ^ C;
				
				// Blend S into V.
				Fvector tmp = (S * (magdt / (t*t+epsilon))) + Vn;
				m.vel = tmp * (vm / tmp.magnitude());
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
		break;
	}
}
void PAPI::PAAvoid::Transform(const Fmatrix& m)
{
	position.transform(positionL,m);
}

void* PAAvoid::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::position:
		{
			return &position;
		}
	case EVariable::look_ahead:
		{
			return &look_ahead;
		}
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Avoid: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}

//-------------------------------------------------------------------------------------------------

void PABounce::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	switch(position.type)
	{
	case PDTriangle:
		{
			// Compute the inverse matrix of the plane basis.
			Fvector &u = position.u;
			Fvector &v = position.v;
			
			// w = u cross v
			float wx = u.y*v.z-u.z*v.y;
			float wy = u.z*v.x-u.x*v.z;
			float wz = u.x*v.y-u.y*v.x;
			
			float det = 1/(wz*u.x*v.y-wz*u.y*v.x-u.z*wx*v.y-u.x*v.z*wy+v.z*wx*u.y+u.z*v.x*wy);
			
			Fvector s1((v.y*wz-v.z*wy), (v.z*wx-v.x*wz), (v.x*wy-v.y*wx));
			s1 *= det;
			Fvector s2((u.y*wz-u.z*wy), (u.z*wx-u.x*wz), (u.x*wy-u.y*wx));
			s2 *= -det;
			
			// See which particles bounce.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// See if particle's current and next positions cross plane.
				// If not, couldn't bounce, so keep going.
				Fvector pnext(m.pos + m.vel * dt);
				
				// p2 stores the plane normal (the a,b,c of the plane eqn).
				// Old and new distances: dist(p,plane) = n * p + d
				// radius1 stores -n*p, which is d.
				float distold = m.pos * position.p2 + position.radius1;
				float distnew = pnext * position.p2 + position.radius1;
				
				// Opposite signs if product < 0
				// Is there a faster way to do this?
				if(distold * distnew >= 0)
					continue;
				
				// Find position at the crossing point by parameterizing
				// p(t) = pos + vel * t
				// Solve dist(p(t),plane) = 0 e.g.
				// n * p(t) + D = 0 ->
				// n * p + t (n * v) + D = 0 ->
				// t = -(n * p + D) / (n * v)
				// Could factor n*v into distnew = distold + n*v and save a bit.
				// Safe since n*v != 0 assured by quick rejection test.
				// This calc is indep. of dt because we have established that it
				// will hit before dt. We just want to know when.
				float nv = position.p2 * m.vel;
				float t = -distold / nv;
				
				// Actual intersection point p(t) = pos + vel t
				Fvector phit(m.pos + m.vel * t);
				
				// Offset from origin in plane, p - origin
				Fvector offset(phit - position.p1);
				
				// Dot product with basis vectors of old frame
				// in terms of new frame gives position in uv frame.
				float upos = offset * s1;
				float vpos = offset * s2;
				
				// Did it cross plane outside triangle?
				if(upos < 0 || vpos < 0 || (upos + vpos) > 1)
					continue;
				
				// A hit! A most palpable hit!
				
				// Compute tangential and normal components of velocity
				Fvector vn(position.p2 * nv); // Normal Vn = (V.N)N
				Fvector vt(m.vel - vn); // Tangent Vt = V - Vn
				
				// Compute new velocity heading out:
				// Don't apply friction if tangential velocity < cutoff
				if(vt.square_magnitude() <= cutoffSqr)
				{
					m.vel = vt - vn * resilience;
				}
				else
				{
					m.vel = vt * oneMinusFriction - vn * resilience;
				}
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
		break;
	case PDDisc:
		{
			float r1Sqr = _sqr(position.radius1);
			float r2Sqr = _sqr(position.radius2);
			
			// See which particles bounce.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// See if particle's current and next positions cross plane.
				// If not, couldn't bounce, so keep going.
				Fvector pnext(m.pos + m.vel * dt);
				
				// p2 stores the plane normal (the a,b,c of the plane eqn).
				// Old and new distances: dist(p,plane) = n * p + d
				// radius1 stores -n*p, which is d. radius1Sqr stores d.
				float distold = m.pos * position.p2 + position.radius1Sqr;
				float distnew = pnext * position.p2 + position.radius1Sqr;
				
				// Opposite signs if product < 0
				// Is there a faster way to do this?
				if(distold * distnew >= 0)
					continue;
				
				// Find position at the crossing point by parameterizing
				// p(t) = pos + vel * t
				// Solve dist(p(t),plane) = 0 e.g.
				// n * p(t) + D = 0 ->
				// n * p + t (n * v) + D = 0 ->
				// t = -(n * p + D) / (n * v)
				// Could factor n*v into distnew = distold + n*v and save a bit.
				// Safe since n*v != 0 assured by quick rejection test.
				// This calc is indep. of dt because we have established that it
				// will hit before dt. We just want to know when.
				float nv = position.p2 * m.vel;
				float t = -distold / nv;
				
				// Actual intersection point p(t) = pos + vel t
				Fvector phit(m.pos + m.vel * t);
				
				// Offset from origin in plane, phit - origin
				Fvector offset(phit - position.p1);
				
				float rad = offset.square_magnitude();
				
				if(rad > r1Sqr || rad < r2Sqr)
					continue;
				
				// A hit! A most palpable hit!
				
				// Compute tangential and normal components of velocity
				Fvector vn(position.p2 * nv); // Normal Vn = (V.N)N
				Fvector vt(m.vel - vn); // Tangent Vt = V - Vn
				
				// Compute new velocity heading out:
				// Don't apply friction if tangential velocity < cutoff
				if(vt.square_magnitude() <= cutoffSqr)
				{
					m.vel = vt - vn * resilience;
				}
				else
				{
					m.vel = vt * oneMinusFriction - vn * resilience;
				}
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
		break;
	case PDPlane:
		{
			// See which particles bounce.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// See if particle's current and next positions cross plane.
				// If not, couldn't bounce, so keep going.
				Fvector pnext(m.pos + m.vel * dt);
				
				// p2 stores the plane normal (the a,b,c of the plane eqn).
				// Old and new distances: dist(p,plane) = n * p + d
				// radius1 stores -n*p, which is d.
				float distold = m.pos * position.p2 + position.radius1;
				float distnew = pnext * position.p2 + position.radius1;
				
				// Opposite signs if product < 0
				if(distold * distnew >= 0)
					continue;
				
				// Compute tangential and normal components of velocity
				float nmag = m.vel * position.p2;
				Fvector vn(position.p2 * nmag); // Normal Vn = (V.N)N
				Fvector vt(m.vel - vn); // Tangent Vt = V - Vn
				
				// Compute new velocity heading out:
				// Don't apply friction if tangential velocity < cutoff
				if(vt.square_magnitude() <= cutoffSqr)
				{
					m.vel = vt - vn * resilience;
				}
				else
				{
					m.vel = vt * oneMinusFriction - vn * resilience;
				}
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
		break;
	case PDRectangle:
		{
			// Compute the inverse matrix of the plane basis.
			Fvector &u = position.u;
			Fvector &v = position.v;
			
			// w = u cross v
			float wx = u.y*v.z-u.z*v.y;
			float wy = u.z*v.x-u.x*v.z;
			float wz = u.x*v.y-u.y*v.x;
			
			float det = 1/(wz*u.x*v.y-wz*u.y*v.x-u.z*wx*v.y-u.x*v.z*wy+v.z*wx*u.y+u.z*v.x*wy);
			
			Fvector s1((v.y*wz-v.z*wy), (v.z*wx-v.x*wz), (v.x*wy-v.y*wx));
			s1 *= det;
			Fvector s2((u.y*wz-u.z*wy), (u.z*wx-u.x*wz), (u.x*wy-u.y*wx));
			s2 *= -det;
			
			// See which particles bounce.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// See if particle's current and next positions cross plane.
				// If not, couldn't bounce, so keep going.
				Fvector pnext(m.pos + m.vel * dt);
				
				// p2 stores the plane normal (the a,b,c of the plane eqn).
				// Old and new distances: dist(p,plane) = n * p + d
				// radius1 stores -n*p, which is d.
				float distold = m.pos * position.p2 + position.radius1;
				float distnew = pnext * position.p2 + position.radius1;
				
				// Opposite signs if product < 0
				if(distold * distnew >= 0)
					continue;
				
				// Find position at the crossing point by parameterizing
				// p(t) = pos + vel * t
				// Solve dist(p(t),plane) = 0 e.g.
				// n * p(t) + D = 0 ->
				// n * p + t (n * v) + D = 0 ->
				// t = -(n * p + D) / (n * v)
				float t = -distold / (position.p2 * m.vel);
				
				// Actual intersection point p(t) = pos + vel t
				Fvector phit(m.pos + m.vel * t);
				
				// Offset from origin in plane, p - origin
				Fvector offset(phit - position.p1);
				
				// Dot product with basis vectors of old frame
				// in terms of new frame gives position in uv frame.
				float upos = offset * s1;
				float vpos = offset * s2;
				
				// Crossed plane outside bounce region if !(0<=[uv]pos<=1)
				if(upos < 0 || upos > 1 || vpos < 0 || vpos > 1)
					continue;
				
				// A hit! A most palpable hit!
				
				// Compute tangential and normal components of velocity
				float nmag = m.vel * position.p2;
				Fvector vn(position.p2 * nmag); // Normal Vn = (V.N)N
				Fvector vt(m.vel - vn); // Tangent Vt = V - Vn
				
				// Compute new velocity heading out:
				// Don't apply friction if tangential velocity < cutoff
				if(vt.square_magnitude() <= cutoffSqr)
				{
					m.vel = vt - vn * resilience;
				}
				else
				{
					m.vel = vt * oneMinusFriction - vn * resilience;
				}
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
		break;
	case PDSphere:
		{
			// Sphere that particles bounce off
			// The particles are always forced out of the sphere.
			for(u32 i = 0; i < pHolder->p_count; i++)
			{
				Particle &m = pHolder->particles[i];
				
				// See if particle's next position is inside domain.
				// If so, bounce it.
				Fvector pnext(m.pos + m.vel * dt);
				
				if(position.Within(pnext))
				{
					// See if we were inside on previous timestep.
					BOOL pinside = position.Within(m.pos);
					
					// Normal to surface. This works for a sphere. Isn't
					// computed quite right, should extrapolate particle
					// position to surface.
					Fvector n(m.pos - position.p1);
					n.normalize_safe();
					
					// Compute tangential and normal components of velocity
					float nmag = m.vel * n;
					
					Fvector vn(n * nmag); // Normal Vn = (V.N)N
					Fvector vt = m.vel - vn; // Tangent Vt = V - Vn
					
					if(pinside)
					{
						// Previous position was inside. If normal component of
						// velocity points in, reverse it. This effectively
						// repels particles which would otherwise be trapped
						// in the sphere.
						if(nmag < 0)
							m.vel = vt - vn;
					}
					else
					{
						// Previous position was outside -> particle will cross
						// surface boundary. Reverse normal component of velocity,
						// and apply friction (if Vt >= cutoff) and resilience.
						
						// Compute new velocity heading out:
						// Don't apply friction if tangential velocity < cutoff
						if(vt.square_magnitude() <= cutoffSqr)
						{
							m.vel = vt - vn * resilience;
						}
						else
						{
							m.vel = vt * oneMinusFriction - vn * resilience;
						}
						if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
						{
							m.rot_vel = m.vel;
							m.rot_vel.normalize_safe();
						}
					}
				}
			}
		}
	}
}
void PABounce::Transform(const Fmatrix& m)
{
	position.transform(positionL,m);
}

void* PABounce::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::position:
		{
			return &position;
		}
	case EVariable::oneMinusFriction:
		{
			return &oneMinusFriction;
		}
	case EVariable::resilience:
		{
			return &resilience;
		}
	case EVariable::cutoffSqr:
		{
			return &cutoffSqr;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Bounce: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Set the secondary position of each particle to be its position.
void PACopyVertexB::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	u32 i;
	
	if(copy_pos)
	{
		for(i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			m.posB = m.pos;
		}
	}
/*	
	if(copy_vel)
	{
		for(i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			m.velB = m.vel;
		}
	}
*/
}
void PACopyVertexB::Transform(const Fmatrix&){;}

void* PACopyVertexB::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::copy_pos:
		return &copy_pos;
	}
	R_ASSERT3(false, "Particle action CopyVertexB: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Dampen velocities
void PADamping::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	// This is important if dt is != 1.
	Fvector one(1,1,1);
	Fvector scale(one - ((one - damping) * dt));
	
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		float vSqr = m.vel.square_magnitude();
		
		if(vSqr >= vlowSqr && vSqr <= vhighSqr)
		{
			m.vel.x *= scale.x;
			m.vel.y *= scale.y;
			m.vel.z *= scale.z;
		}
		if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
		{
			m.rot_vel = m.vel;
			m.rot_vel.normalize_safe();
		}
	}
}
void PADamping::Transform(const Fmatrix&){;}

void* PADamping::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::damping:
		{
			return &damping;
		}
	case EVariable::vlowSqr:
		{
			return &vlowSqr;
		}
	case EVariable::vhighSqr:
		{
			return &vhighSqr;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Damping: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Exert force on each particle away from explosion center
void PAExplosion::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float radius 		= velocity * age;
	float magdt 		= magnitude * dt;
	float oneOverSigma 	= 1.0f / stdev;
	float inexp 		= -0.5f*_sqr(oneOverSigma);
	float outexp 		= ONEOVERSQRT2PI * oneOverSigma;
	
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		
		// Figure direction to particle.
		Fvector dir		(m.pos - center);
		float distSqr 	= dir.square_magnitude();
		float dist 		= _sqrt(distSqr);
		float DistFromWaveSqr = _sqr(radius - dist);
		
		float Gd 		= expf(DistFromWaveSqr * inexp) * outexp;
		
		m.vel 			+= dir * (Gd * magdt / ((dist+EPS) * (distSqr + epsilon)));
		if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
		{
			m.rot_vel = m.vel;
			m.rot_vel.normalize_safe();
		}
	}
	
	age += dt;
}
void PAExplosion::Transform(const Fmatrix& m)
{
	m.transform_tiny(center,centerL);
}

void* PAExplosion::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::center:
		{
			return &center;
		}
	case EVariable::velocity:
		{
			return &velocity;
		}
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::stdev:
		{
			return &stdev;
		}
	case EVariable::age:
		{
			return &age;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Explosion: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Follow the next particle in the list
void PAFollow::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt = magnitude * dt;
	float max_radiusSqr = max_radius * max_radius;
	
	if(max_radiusSqr < P_MAXFLOAT)
	{
		for(u32 i = 0; i < pHolder->p_count - 1; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Accelerate toward the particle after me in the list.
			Fvector tohim(pHolder->particles[i+1].pos - m.pos); // tohim = p1 - p0
			float tohimlenSqr = tohim.square_magnitude();
			
			if(tohimlenSqr < max_radiusSqr)
			{
				// Compute force exerted between the two bodies
				m.vel += tohim * (magdt / (_sqrt(tohimlenSqr) * (tohimlenSqr + epsilon)));
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
	}
	else
	{
		for(u32 i = 0; i < pHolder->p_count - 1; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Accelerate toward the particle after me in the list.
			Fvector tohim(pHolder->particles[i+1].pos - m.pos); // tohim = p1 - p0
			float tohimlenSqr = tohim.square_magnitude();
			
			// Compute force exerted between the two bodies
			m.vel += tohim * (magdt / (_sqrt(tohimlenSqr) * (tohimlenSqr + epsilon)));
			if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
			{
				m.rot_vel = m.vel;
				m.rot_vel.normalize_safe();
			}
		}
	}
}
void PAFollow::Transform(const Fmatrix&){;}

void* PAFollow::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::max_radius:
		{
			return &max_radius;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Follow: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Inter-particle gravitation
void PAGravitate::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt = magnitude * dt;
	float max_radiusSqr = max_radius * max_radius;
	
	if(max_radiusSqr < P_MAXFLOAT)
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Add interactions with other particles
			for(u32 j = i + 1; j < pHolder->p_count; j++)
			{
				Particle &mj = pHolder->particles[j];
				
				Fvector tohim(mj.pos - m.pos); // tohim = p1 - p0
				float tohimlenSqr = tohim.square_magnitude()+EPS_S;
				
				if(tohimlenSqr < max_radiusSqr)
				{
					// Compute force exerted between the two bodies
					Fvector acc(tohim * (magdt / (_sqrt(tohimlenSqr) * (tohimlenSqr + epsilon))));
					
					m.vel += acc;
					mj.vel -= acc;
					if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
					{
						m.rot_vel = m.vel;
						m.rot_vel.normalize_safe();
						mj.rot_vel = mj.vel;
						mj.rot_vel.normalize_safe();
					}
				}
			}
		}
	}
	else
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Add interactions with other particles
			for(u32 j = i + 1; j < pHolder->p_count; j++)
			{
				Particle &mj = pHolder->particles[j];
				
				Fvector tohim(mj.pos - m.pos); // tohim = p1 - p0
				float tohimlenSqr = tohim.square_magnitude()+EPS_S;
				
				// Compute force exerted between the two bodies
				Fvector acc(tohim * (magdt / (_sqrt(tohimlenSqr) * (tohimlenSqr + epsilon))));
				
				m.vel += acc;
				mj.vel -= acc;
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
					mj.rot_vel = mj.vel;
					mj.rot_vel.normalize_safe();
				}
			}
		}
	}
}
void PAGravitate::Transform(const Fmatrix&){;}

void* PAGravitate::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::max_radius:
		{
			return &max_radius;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Gravitate: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Acceleration in a constant direction
void PAGravity::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	Fvector ddir(direction * dt);
	
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		// Step velocity with acceleration
		pHolder->particles[i].vel += ddir;
	}
}
void PAGravity::Transform(const Fmatrix&){;}

void* PAGravity::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::direction:
		return &direction;
	}
	R_ASSERT3(false, "Particle action Gravity: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Accelerate particles along a line
void PAJet::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt = magnitude * dt;
	float max_radiusSqr = max_radius * max_radius;
	
	if(max_radiusSqr < P_MAXFLOAT)
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Figure direction to particle.
			Fvector dir(m.pos - center);
			
			// Distance to jet (force drops as 1/r^2)
			// Soften by epsilon to avoid tight encounters to infinity
			float rSqr = dir.square_magnitude();
			
			if(rSqr < max_radiusSqr)
			{
				Fvector accel;
				acc.Generate(accel);
				
				// Step velocity with acceleration
				m.vel += accel * (magdt / (rSqr + epsilon));
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
	}
	else
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Figure direction to particle.
			Fvector dir(m.pos - center);
			
			// Distance to jet (force drops as 1/r^2)
			// Soften by epsilon to avoid tight encounters to infinity
			float rSqr = dir.square_magnitude();
			
			Fvector accel;
			acc.Generate(accel);
			
			// Step velocity with acceleration
			m.vel += accel * (magdt / (rSqr + epsilon));
			if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
			{
				m.rot_vel = m.vel;
				m.rot_vel.normalize_safe();
			}
		}
	}
}
void PAJet::Transform(const Fmatrix& m)
{
	m.transform_tiny	(center,centerL);
	acc.transform_dir	(accL,m);
}

void* PAJet::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::center:
		{
			return &center;
		}
	case EVariable::acc:
		{
			return &acc;
		}
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::max_radius:
		{
			return &max_radius;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Jet: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Accelerate particles form center
void PAScatter::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt 		= magnitude * dt;
	float max_radiusSqr = max_radius * max_radius;
	
	if(max_radiusSqr < P_MAXFLOAT)
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Figure direction to particle.
			Fvector dir(m.pos - center);
			
			// Distance to jet (force drops as 1/r^2)
			// Soften by epsilon to avoid tight encounters to infinity
			float rSqr = dir.square_magnitude();
			
			if(rSqr < max_radiusSqr)
			{
				Fvector accel;
                accel = dir/_sqrt(rSqr);
                
//				acc.Generate(accel);
				
				// Step velocity with acceleration
				m.vel += accel * (magdt / (rSqr + epsilon));
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
	}
	else
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Figure direction to particle.
			Fvector dir(m.pos - center);
			
			// Distance to jet (force drops as 1/r^2)
			// Soften by epsilon to avoid tight encounters to infinity
			float rSqr = dir.square_magnitude();
			
			Fvector accel;
            accel = dir/_sqrt(rSqr);
			
			// Step velocity with acceleration
			m.vel += accel * (magdt / (rSqr + epsilon));
			if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
			{
				m.rot_vel = m.vel;
				m.rot_vel.normalize_safe();
			}
		}
	}
}
void PAScatter::Transform(const Fmatrix& m)
{
	m.transform_tiny	(center,centerL);
}

void* PAScatter::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::center:
		{
			return &center;
		}
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::max_radius:
		{
			return &max_radius;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Scatter: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Get rid of older particles
void PAKillOld::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	// Must traverse list in reverse order so Remove will work
    tm_max = age_limit;
	for(int i = pHolder->p_count-1; i >= 0; i--)
	{
		Particle &m = pHolder->particles[i];
		
		if(!((m.age < age_limit) ^ kill_less_than))   
			pHolder->RemoveParticle(i);
	}
}
void PAKillOld::Transform(const Fmatrix&){;}

void* PAKillOld::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::age_limit:
		return &age_limit;
	case EVariable::kill_less_than:
		return &kill_less_than;
	}
	R_ASSERT3(false, "Particle action KillOld: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Match velocity to near neighbors
void PAMatchVelocity::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt = magnitude * dt;
	float max_radiusSqr = max_radius * max_radius;
	
	if(max_radiusSqr < P_MAXFLOAT)
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Add interactions with other particles
			for(u32 j = i + 1; j < pHolder->p_count; j++)
			{
				Particle &mj = pHolder->particles[j];
				
				Fvector tohim(mj.pos - m.pos); // tohim = p1 - p0
				float tohimlenSqr = tohim.square_magnitude();
				
				if(tohimlenSqr < max_radiusSqr)
				{
					// Compute force exerted between the two bodies
					Fvector acc(mj.vel * (magdt / (tohimlenSqr + epsilon)));
					
					m.vel += acc;
					mj.vel -= acc;
					if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
					{
						m.rot_vel = m.vel;
						m.rot_vel.normalize_safe();
						mj.rot_vel = mj.vel;
						mj.rot_vel.normalize_safe();
					}
				}
			}
		}
	}
	else
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Add interactions with other particles
			for(u32 j = i + 1; j < pHolder->p_count; j++)
			{
				Particle &mj = pHolder->particles[j];
				
				Fvector tohim(mj.pos - m.pos); // tohim = p1 - p0
				float tohimlenSqr = tohim.square_magnitude();
				
				// Compute force exerted between the two bodies
				Fvector acc(mj.vel * (magdt / (tohimlenSqr + epsilon)));
				
				m.vel += acc;
				mj.vel -= acc;
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
					mj.rot_vel = mj.vel;
					mj.rot_vel.normalize_safe();
				}
			}
		}
	}
}
void PAMatchVelocity::Transform(const Fmatrix&){;}

void* PAMatchVelocity::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::max_radius:
		{
			return &max_radius;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action MatchVelocity: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

void PAMove::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	// Step particle positions forward by dt, and age the particles.
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		// move
		m.age	+= dt;               
        m.posB 	= m.pos;
//        m.velB 	= m.vel;
		m.pos	+= m.vel * dt;
	}
}
void PAMove::Transform(const Fmatrix&){;}

void* PAMove::GetVariableImpl(u8 VarID)
{
	R_ASSERT3(false, "Particle action Move: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Accelerate particles towards a line
void PAOrbitLine::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt = magnitude * dt;
	float max_radiusSqr = max_radius * max_radius;
	
	if(max_radiusSqr < P_MAXFLOAT)
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Figure direction to particle from base of line.
			Fvector f(m.pos - p);
			
			Fvector w(axis * (f * axis));
			
			// Direction from particle to nearest point on line.
			Fvector into = w - f;
			
			// Distance to line (force drops as 1/r^2, normalize by 1/r)
			// Soften by epsilon to avoid tight encounters to infinity
			float rSqr = into.square_magnitude();
			
			if(rSqr < max_radiusSqr)
			{
				// Step velocity with acceleration
				m.vel += into * (magdt / (_sqrt(rSqr) + (rSqr + epsilon)));
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
	}
	else
	{
		// Removed because it causes pipeline stalls.
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Figure direction to particle from base of line.
			Fvector f(m.pos - p);
			
			Fvector w(axis * (f * axis));
			
			// Direction from particle to nearest point on line.
			Fvector into = w - f;
			
			// Distance to line (force drops as 1/r^2, normalize by 1/r)
			// Soften by epsilon to avoid tight encounters to infinity
			float rSqr = into.square_magnitude();
			
			// Step velocity with acceleration
			m.vel += into * (magdt / (_sqrt(rSqr) + (rSqr + epsilon)));
			if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
			{
				m.rot_vel = m.vel;
				m.rot_vel.normalize_safe();
			}
		}
	}
}
void PAOrbitLine::Transform(const Fmatrix& m)
{
	m.transform_tiny(p,pL);
	m.transform_dir(axis,axisL);
}

void* PAOrbitLine::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::p:
		{
			return &p;
		}
	case EVariable::axis:
		{
			return &axis;
		}
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::max_radius:
		{
			return &max_radius;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action OrbitLine: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Accelerate particles towards a point
void PAOrbitPoint::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt = magnitude * dt;
	float max_radiusSqr = max_radius * max_radius;

	if(max_radiusSqr < P_MAXFLOAT)
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Figure direction to particle.
			Fvector dir(center - m.pos);
			
			// Distance to gravity well (force drops as 1/r^2, normalize by 1/r)
			// Soften by epsilon to avoid tight encounters to infinity
			float rSqr = dir.square_magnitude();
			
			// Step velocity with acceleration
			if(rSqr < max_radiusSqr)
			{
				m.vel += dir * (magdt / (_sqrt(rSqr) + (rSqr + epsilon)));
				if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
				{
					m.rot_vel = m.vel;
					m.rot_vel.normalize_safe();
				}
			}
		}
	}
	else
	{
		// Avoids pipeline stalls.
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Figure direction to particle.
			Fvector dir(center - m.pos);
			
			// Distance to gravity well (force drops as 1/r^2, normalize by 1/r)
			// Soften by epsilon to avoid tight encounters to infinity
			float rSqr = dir.square_magnitude();
			
			// Step velocity with acceleration
			m.vel += dir * (magdt / (_sqrt(rSqr) + (rSqr + epsilon)));
			if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
			{
				m.rot_vel = m.vel;
				m.rot_vel.normalize_safe();
			}
		}
	}
}
void PAOrbitPoint::Transform(const Fmatrix& m)
{
	m.transform_tiny(center,centerL);
}

void* PAOrbitPoint::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::center:
		{
			return &center;
		}
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::max_radius:
		{
			return &max_radius;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action OrbitPoint: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Accelerate in random direction each time step
void PARandomAccel::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		
		Fvector acceleration;
		gen_acc.Generate(acceleration);
		
		// dt will affect this by making a higher probability of
		// being near the original velocity after unit time. Smaller
		// dt approach a normal distribution instead of a square wave.
		m.vel += acceleration * dt;
		if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
		{
			m.rot_vel = m.vel;
			m.rot_vel.normalize_safe();
		}
	}
}
void PARandomAccel::Transform(const Fmatrix& m)
{
	gen_acc.transform_dir(gen_accL,m);
}

void* PARandomAccel::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::gen_acc:
		{
			return &gen_acc;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action RandomAccel: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Immediately displace position randomly
void PARandomDisplace::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		
		Fvector displacement;
		gen_disp.Generate(displacement);
		
		// dt will affect this by making a higher probability of
		// being near the original position after unit time. Smaller
		// dt approach a normal distribution instead of a square wave.
		m.pos += displacement * dt;
	}
}
void PARandomDisplace::Transform(const Fmatrix& m)
{
	gen_disp.transform_dir(gen_dispL,m);
}

void* PARandomDisplace::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::gen_disp:
		return &gen_disp;
	}
	R_ASSERT3(false, "Particle action RandomDisplace: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Immediately assign a random velocity
void PARandomVelocity::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		
		Fvector velocity;
		gen_vel.Generate(velocity);
		
		// Shouldn't multiply by dt because velocities are
		// invariant of dt. How should dt affect this?
		m.vel = velocity;
		if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
		{
			m.rot_vel = m.vel;
			m.rot_vel.normalize_safe();
		}
	}
}
void PARandomVelocity::Transform(const Fmatrix& m)
{
	gen_vel.transform_dir(gen_velL,m);
}

void* PARandomVelocity::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::gen_vel:
		{
			return &gen_vel;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action RandomVelocity: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

#if 0
// Produce coefficients of a velocity function v(t)=at^2 + bt + c
// satisfying initial x(0)=x0,v(0)=v0 and desired x(t)=xf,v(t)=vf,
// where x = x(0) + integrate(v(T),0,t)
static inline void _pconstrain(float x0, float v0, float xf, float vf,
							   float t, float *a, float *b, float *c)
{
	*c = v0;
	*b = 2 * (-t*vf - 2*t*v0 + 3*xf - 3*x0) / (t * t);
	*a = 3 * (t*vf + t*v0 - 2*xf + 2*x0) / (t * t * t);
}
#endif

// Over time, restore particles to initial positions
// Put all particles on the surface of a statue, explode the statue,
// and then suck the particles back to the original position. Cool!
void PARestore::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	if(time_left <= 0)
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Already constrained, keep it there.
			m.pos = m.posB;
			m.vel = Fvector(0,0,0);
		}
	}
	else
	{
		float t = time_left;
		float dtSqr = dt * dt;
		float tSqrInv2dt = dt * 2.0f / (t * t);
		float tCubInv3dtSqr = dtSqr * 3.0f / (t * t * t);
		
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
#if 1
			Particle &m = pHolder->particles[i];
			
			// Solve for a desired-behavior velocity function in each axis
			// _pconstrain(m.pos.x, m.vel.x, m.posB.x, 0., timeLeft, &a, &b, &c);
			
			// Figure new velocity at next timestep
			// m.vel.x = a * dtSqr + b * dt + c;
			
			float b = (-2*t*m.vel.x + 3*m.posB.x - 3*m.pos.x) * tSqrInv2dt;
			float a = (t*m.vel.x - m.posB.x - m.posB.x + m.pos.x + m.pos.x) * tCubInv3dtSqr;
			
			// Figure new velocity at next timestep
			m.vel.x += a + b;
			
			b = (-2*t*m.vel.y + 3*m.posB.y - 3*m.pos.y) * tSqrInv2dt;
			a = (t*m.vel.y - m.posB.y - m.posB.y + m.pos.y + m.pos.y) * tCubInv3dtSqr;
			
			// Figure new velocity at next timestep
			m.vel.y += a + b;
			
			b = (-2*t*m.vel.z + 3*m.posB.z - 3*m.pos.z) * tSqrInv2dt;
			a = (t*m.vel.z - m.posB.z - m.posB.z + m.pos.z + m.pos.z) * tCubInv3dtSqr;
			
			// Figure new velocity at next timestep
			m.vel.z += a + b;
			if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
			{
				m.rot_vel = m.vel;
				m.rot_vel.normalize_safe();
			}
#else
			Particle &m = pHolder->particles[i];
			
			// XXX Optimize this.
			// Solve for a desired-behavior velocity function in each axis
			float a, b, c; // Coefficients of velocity function needed
			
			_pconstrain(m.pos.x, m.vel.x, m.posB.x, 0.,
				timeLeft, &a, &b, &c);
			
			// Figure new velocity at next timestep
			m.vel.x = a * dtSqr + b * dt + c;
			
			_pconstrain(m.pos.y, m.vel.y, m.posB.y, 0.,
				timeLeft, &a, &b, &c);
			
			// Figure new velocity at next timestep
			m.vel.y = a * dtSqr + b * dt + c;
			
			_pconstrain(m.pos.z, m.vel.z, m.posB.z, 0.,
				timeLeft, &a, &b, &c);
			
			// Figure new velocity at next timestep
			m.vel.z = a * dtSqr + b * dt + c;
			
#endif
		}
	}
	
	time_left -= dt;
}
void PARestore::Transform(const Fmatrix&){;}

void* PARestore::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::time_left:
		{
			return &time_left;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Restore: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Kill particles with positions on wrong side of the specified domain
void PASink::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	// Must traverse list in reverse order so Remove will work
	for(int i = pHolder->p_count-1; i >= 0; i--)
	{
		Particle &m = pHolder->particles[i];
		
		// Remove if inside/outside flag matches object's flag
		if(!(position.Within(m.pos) ^ kill_inside))
			pHolder->RemoveParticle(i);
	}
}
void PASink::Transform(const Fmatrix& m)
{
	position.transform(positionL,m);
}

void* PASink::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::kill_inside:
		return &kill_inside;
	case EVariable::position:
		return &position;
	}
	R_ASSERT3(false, "Particle action Sink: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Kill particles with velocities on wrong side of the specified domain
void PASinkVelocity::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	// Must traverse list in reverse order so Remove will work
	for(int i = pHolder->p_count-1; i >= 0; i--)
	{
		Particle &m = pHolder->particles[i];
		
		// Remove if inside/outside flag matches object's flag
		if(!(velocity.Within(m.vel) ^ kill_inside))
			pHolder->RemoveParticle(i);
	}
}
void PASinkVelocity::Transform(const Fmatrix& m)
{
	velocity.transform_dir(velocityL,m);
}

void* PASinkVelocity::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::kill_inside:
		return &kill_inside;
	case EVariable::velocity:
		return &velocity;
	}
	R_ASSERT3(false, "Particle action SinkVelocity: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Randomly add particles to the system
void PASource::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	if (m_Flags.is(flSilent)) return;

	int rate = int(floor(particle_rate * dt));
	
	// Dither the fraction particle in time.
	if(drand48() < particle_rate * dt - float(rate))
		rate++;
	
	// Don't emit more than it can hold.
	if(pHolder->p_count + rate > pHolder->max_particles)
		rate = pHolder->max_particles - pHolder->p_count;
	
	Fvector pos, posB, vel, rot_velocity, col, siz, rt;
	
	if(m_Flags.is(u32(flVertexB_tracks))){
		for(int i = 0; i < rate; i++){
			position.Generate	(pos);
			size.Generate		(siz); 	if (m_Flags.is(flSingleSize)) siz.set(siz.x,siz.x,siz.x);
			rot.Generate		(rt);
			velocity.Generate	(vel);	vel += parent_vel;
			if (AlighRotVelocityToVelocity)
			{
				rot_velocity = vel;
				if (fis_zero(rot_velocity.magnitude()))
				{
					rot_velocity.x = 1;
					rot_velocity.y = 0;
					rot_velocity.z = 0;
				} else
				{
					rot_velocity.normalize();
				}
			} else
			{
				rot_vel.Generate(rot_velocity);
				if (fis_zero(rot_velocity.magnitude()))
				{
					rot_velocity.x = 1;
					rot_velocity.y = 0;
					rot_velocity.z = 0;
				}
				else
				{
					rot_velocity.normalize();
				}
			}
			color.Generate		(col);
			float ag 			= age + NRand(age_sigma);

			pHolder->AddParticle(pos, pos, siz, rt, vel, rot_velocity, color_argb_f(alpha, col.x, col.y, col.z), ag);
		}
	}else{
		for(int i = 0; i < rate; i++){
			position.Generate	(pos);
			size.Generate		(siz); 	if (m_Flags.is(flSingleSize)) siz.set(siz.x,siz.x,siz.x);
			rot.Generate		(rt);
			velocity.Generate	(vel);	vel += parent_vel;
			if (AlighRotVelocityToVelocity)
			{
				rot_velocity = vel;
				if (fis_zero(rot_velocity.magnitude()))
				{
					rot_velocity.x = 1;
					rot_velocity.y = 0;
					rot_velocity.z = 0;
				} else
				{
					rot_velocity.normalize();
				}
			} else
			{
				rot_vel.Generate(rot_velocity);
				if (fis_zero(rot_velocity.magnitude()))
				{
					rot_velocity.x = 1;
					rot_velocity.y = 0;
					rot_velocity.z = 0;
				}
				else
				{
					rot_velocity.normalize();
				}
			}
			color.Generate		(col);
			float ag 			= age + NRand(age_sigma);

			pHolder->AddParticle(pos, posB, siz, rt, vel, rot_velocity, color_argb_f(alpha, col.x, col.y, col.z), ag);
		}
	}
}
void PASource::Transform(const Fmatrix& m)
{
	position.transform(positionL,m);
	velocity.transform_dir(velocityL,m);
}

void* PASource::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::position:
		{
			return &position;
		}
	case EVariable::velocity:
		{
			return &velocity;
		}
	case EVariable::aligh_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	case EVariable::rot_vel:
		{
			return &rot_vel;
		}
	case EVariable::rot:
		{
			return &rot;
		}
	case EVariable::size:
		{
			return &size;
		}
	case EVariable::color:
		{
			return &color;
		}
	case EVariable::alpha:
		{
			return &alpha;
		}
	case EVariable::particle_rate:
		{
			return &particle_rate;
		}
	case EVariable::age:
		{
			return &age;
		}
	case EVariable::age_sigma:
		{
			return &age_sigma;
		}
	case EVariable::parent_vel:
		{
			return &parent_vel;
		}
	case EVariable::parent_motion:
		{
			return &parent_motion;
		}
	}
	R_ASSERT3(false, "Particle action Source: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

void PASpeedLimit::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float min_sqr = min_speed*min_speed;
	float max_sqr = max_speed*max_speed;
	
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		float sSqr = m.vel.square_magnitude();
		if(sSqr<min_sqr && sSqr)
		{
			float s = _sqrt(sSqr);
			m.vel *= (min_speed/s);
			if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
			{
				m.rot_vel = m.vel;
				m.rot_vel.normalize_safe();
			}
		}
		else if(sSqr>max_sqr)
		{
			float s = _sqrt(sSqr);
			m.vel *= (max_speed/s);
			if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
			{
				m.rot_vel = m.vel;
				m.rot_vel.normalize_safe();
			}
		}
	}
}
void PASpeedLimit::Transform(const Fmatrix&){;}

void* PASpeedLimit::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::min_speed:
		{
			return &min_speed;
		}
	case EVariable::max_speed:
		{
			return &max_speed;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action SpeedLimit: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Change color of all particles toward the specified color
void PATargetColor::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float scaleFac = scale * dt;
    Fcolor c_p,c_t; 
	
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		if(m.age<timeFrom*tm_max || m.age>timeTo*tm_max ) continue;

        c_p.set	(m.color);
        c_t.set	(c_p.r+(color.x-c_p.r)*scaleFac, c_p.g+(color.y-c_p.g)*scaleFac, c_p.b+(color.z-c_p.b)*scaleFac, c_p.a+(alpha-c_p.a)*scaleFac);
        m.color = c_t.get();
	}
}
void PATargetColor::Transform(const Fmatrix&){;}

void* PATargetColor::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::color:
		return &color;
	case EVariable::alpha:
		return &alpha;
	case EVariable::scale:
		return &scale;
	case EVariable::timeFrom:
		return &timeFrom;
	case EVariable::timeTo:
		return &timeTo;
	}
	R_ASSERT3(false, "Particle action TargetColor: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Change sizes of all particles toward the specified size
void PATargetSize::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float scaleFac_x = scale.x * dt;
	float scaleFac_y = scale.y * dt;
	float scaleFac_z = scale.z * dt;
	
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		Fvector dif(size - m.size);
		dif.x *= scaleFac_x;
		dif.y *= scaleFac_y;
		dif.z *= scaleFac_z;
		m.size += dif;
	}
}
void PATargetSize::Transform(const Fmatrix&){;}

void* PATargetSize::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::size:
		return &size;
	case EVariable::scale:
		return &scale;
	}
	R_ASSERT3(false, "Particle action TargetSize: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Change rotation of all particles toward the specified velocity
void PATargetRotate::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float scaleFac = scale * dt;

	float r = _abs(rot.x);

	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		float sign = m.rot.x >= 0.f ? scaleFac : -scaleFac;
		float dif = ( r - _abs( m.rot.x ) ) * sign;
		m.rot.x	+= dif;
	}
}
void PATargetRotate::Transform(const Fmatrix&){;}

void* PATargetRotate::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::rot:
		return &rot;
	case EVariable::scale:
		return &scale;
	}
	R_ASSERT3(false, "Particle action TargetRotate: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Change velocity of all particles toward the specified velocity
void PATargetVelocity::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float scaleFac = scale * dt;
	
	for(u32 i = 0; i < pHolder->p_count; i++)
	{
		Particle &m = pHolder->particles[i];
		m.vel += (velocity - m.vel) * scaleFac;
		if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
		{
			m.rot_vel = m.vel;
			m.rot_vel.normalize_safe();
		}
	}
}
void PATargetVelocity::Transform(const Fmatrix& m)
{
	m.transform_dir(velocity,velocityL);
}

void* PATargetVelocity::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::velocity:
		{
			return &velocity;
		}
	case EVariable::scale:
		{
			return &scale;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action TargetVelocity: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Immediately displace position using vortex
// Vortex tip at center, around axis, with magnitude
// and tightness exponent
void PAVortex::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	float magdt = magnitude * dt;
	float max_radiusSqr = max_radius * max_radius;
	
	if(max_radiusSqr < P_MAXFLOAT)
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Vector from tip of vortex
			Fvector offset(m.pos - center);
			
			// Compute distance from particle to tip of vortex.
			float rSqr = offset.square_magnitude();
			
			// Don't do anything to particle if too close or too far.
			if(rSqr > max_radiusSqr)
				continue;
			
			float r = _sqrt(rSqr);
			
			// Compute normalized offset vector3.
			Fvector offnorm(offset / r);
			
			// Construct orthogonal vector3 frame in which to rotate
			// transformed point around origin
			float axisProj = offnorm * axis; // offnorm . axis
			
			// Components of offset perpendicular and parallel to axis
			Fvector w(axis * axisProj); // parallel component
			Fvector u(offnorm - w); // perpendicular component
			
			// Perpendicular component completing frame:
			Fvector v(axis ^ u);
			
			// Figure amount of rotation
			// Resultant is (cos theta) u + (sin theta) v
			float theta = magdt / (rSqr + epsilon);
			float s = _sin(theta);
			float c = _cos(theta);
			
			offset = (u * c + v * s + w) * r;
			
			// Translate back to object space
			m.pos = offset + center;
		}
	}
	else
	{
		for(u32 i = 0; i < pHolder->p_count; i++)
		{
			Particle &m = pHolder->particles[i];
			
			// Vector from tip of vortex
			Fvector offset(m.pos - center);
			
			// Compute distance from particle to tip of vortex.
			float rSqr = offset.square_magnitude();
			
			float r = _sqrt(rSqr);
			
			// Compute normalized offset vector3.
			Fvector offnorm(offset / r);
			
			// Construct orthogonal vector3 frame in which to rotate
			// transformed point around origin
			float axisProj = offnorm * axis; // offnorm . axis
			
			// Components of offset perpendicular and parallel to axis
			Fvector w(axis * axisProj); // parallel component
			Fvector u(offnorm - w); // perpendicular component
			
			// Perpendicular component completing frame:
			Fvector v(axis ^ u);
			
			// Figure amount of rotation
			// Resultant is (cos theta) u + (sin theta) v
			float theta = magdt / (rSqr + epsilon);
			float s = _sin(theta);
			float c = _cos(theta);
			
			offset = (u * c + v * s + w) * r;
			
			// Translate back to object space
			m.pos = offset + center;
		}
	}
}
void PAVortex::Transform(const Fmatrix& m)
{
	m.transform_tiny(center,centerL);
	m.transform_dir(axis,axisL);
}

void* PAVortex::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::center:
		return &center;
	case EVariable::axis:
		return &axis;
	case EVariable::magnitude:
		return &magnitude;
	case EVariable::epsilon:
		return &epsilon;
	case EVariable::max_radius:
		return &max_radius;
	}
	R_ASSERT3(false, "Particle action Vortex: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

// Turbulence
#include "noise.h"

static int	noise_start = 1;
extern void	noise3Init();

#include <xmmintrin.h>

__forceinline __m128 _mm_load_fvector( const Fvector& v )
{
	__m128 R1,R2;

	R1 = _mm_load_ss( (float*) &v.x );	// R1 = 0 | 0 | 0 | v.x
	R2 = _mm_load_ss( (float*) &v.y );	// R2 = 0 | 0 | 0 | v.y
	R1 = _mm_unpacklo_ps( R1 , R2 );	// R1 = 0 | 0 | v.y | v.x
	R2 = _mm_load_ss( (float*) &v.z );	// R2 = 0 | 0 | 0 | v.z
	R1 = _mm_movelh_ps( R1 , R2 );		// R1 = 0 | v.z | v.y | v.x

	return R1;
}

__forceinline void _mm_store_fvector( Fvector& v , const __m128 R1 )
{
	__m128 R2;

	_mm_store_ss( (float*) &v.x , R1 );
	R2 = _mm_unpacklo_ps( R1 , R1 );	// R2 = v.y | v.y | v.x | v.x
	R2 = _mm_movehl_ps( R2 , R2 );		// R2 = v.y | v.y | v.y | v.y 
	_mm_store_ss( (float*) &v.y , R2 );
	R2 = _mm_movehl_ps( R1 , R1 );		// R2 = 0 | v.z | 0 | v.z
	_mm_store_ss( (float*) &v.z , R2 );
}

void PATurbulence::Execute(ParticleHolder *pHolder, const float dt, float& tm_max)
{
	if ( noise_start ) {
		noise_start = 0;
		noise3Init();
	};

    Fvector pV;
    Fvector vX;
    Fvector vY;
    Fvector vZ;
    age		+= dt;
    for(u32 i = 0; i < pHolder->p_count; i++)
    {
        Particle &m = pHolder->particles[i];

		pV.mad(m.pos,offset,age);
		vX.set(pV.x+epsilon,pV.y,pV.z);
		vY.set(pV.x,pV.y+epsilon,pV.z);
		vZ.set(pV.x,pV.y,pV.z+epsilon);

        float d	=	fractalsum3(pV, frequency, octaves);

		Fvector D;

        D.x 	= 	fractalsum3(vX, frequency, octaves);
        D.y 	= 	fractalsum3(vY, frequency, octaves);
        D.z 	= 	fractalsum3(vZ, frequency, octaves);

		__m128 _D = _mm_load_fvector( D );
		__m128 _d = _mm_set1_ps( d );
		__m128 _magnitude = _mm_set1_ps( magnitude );
		__m128 _mvel = _mm_load_fvector( m.vel );
		_D = _mm_sub_ps( _D , _d );
		_D = _mm_mul_ps( _D , _magnitude );

		__m128 _vmo = _mm_mul_ps( _mvel , _mvel );	// _vmo = 00 | zz | yy | xx
		__m128 _tmp = _mm_movehl_ps( _vmo , _vmo );	// _tmp = 00 | zz | 00 | zz 
		_vmo = _mm_add_ss( _vmo , _tmp );			// _vmo = 00 | zz | yy | xx + zz
		_tmp = _mm_unpacklo_ps( _vmo , _vmo );		// _tmp = yy | yy | xx + zz | xx + zz
		_tmp = _mm_movehl_ps( _tmp , _tmp );		// _tmp = yy | yy | yy | yy 
		_vmo = _mm_add_ss( _vmo , _tmp );			// _vmo = 00 | zz | yy | xx + yy + zz
		_vmo = _mm_sqrt_ss( _vmo );					// _vmo = 00 | zz | yy | vmo

		_mvel = _mm_add_ps( _mvel , _D );

		__m128 _vmn = _mm_mul_ps( _mvel , _mvel );	// _vmn = 00 | zz | yy | xx
		_tmp = _mm_movehl_ps( _vmn , _vmn );		// _tmp = 00 | zz | 00 | zz 
		_vmn = _mm_add_ss( _vmn , _tmp );			// _vmn = 00 | zz | yy | xx + zz
		_tmp = _mm_unpacklo_ps( _vmn , _vmn );		// _tmp = yy | yy | xx + zz | xx + zz
		_tmp = _mm_movehl_ps( _tmp , _tmp );		// _tmp = yy | yy | yy | yy 
		_vmn = _mm_add_ss( _vmn , _tmp );			// _vmn = 00 | zz | yy | xx + yy + zz
		_vmn = _mm_sqrt_ss( _vmn );					// _vmn = 00 | zz | yy | vmn

		_vmo = _mm_div_ss( _vmo , _vmn );			// _vmo = 00 | zz | yy | scale

		_vmo = _mm_shuffle_ps( _vmo , _vmo , _MM_SHUFFLE( 0 , 0 , 0 , 0 ) ); // _vmo = scale | scale | scale | scale
		_mvel = _mm_mul_ps( _mvel , _vmo );

    	_mm_store_fvector( m.vel , _mvel );
    	if (AlighRotVelocityToVelocity && !fis_zero(m.vel.magnitude()))
    	{
    		m.rot_vel = m.vel;
    		m.rot_vel.normalize_safe();
    	}
	}
}

void PATurbulence::Transform(const Fmatrix& m){}

void* PATurbulence::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::frequency:
		{
			return &frequency;
		}
	case EVariable::octaves:
		{
			return &octaves;
		}
	case EVariable::magnitude:
		{
			return &magnitude;
		}
	case EVariable::epsilon:
		{
			return &epsilon;
		}
	case EVariable::offset:
		{
			return &offset;
		}
	case EVariable::age:
		{
			return &age;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action Turbulence: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}

// Binders
void PABindVelocityValue::Transform(const Fmatrix& m) {}
void PABindVelocityValue::Execute(ParticleHolder* effect, const float dt, float& tm_max) {
	for (u32 i = 0; i < effect->p_count; i++)
	{
		Particle& m = effect->particles[i];
		m.vel = BindValue;
		if (AlighRotVelocityToVelocity)
		{
			m.rot_vel = m.vel;
			m.rot_vel.normalize_safe();
		}
	}
}
void* PABindVelocityValue::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::BindValue:
		{
			return &BindValue;
		}
	case EVariable::align_rot_vel_to_vel:
		{
			return &AlighRotVelocityToVelocity;
		}
	}
	R_ASSERT3(false, "Particle action BindVelocityValue: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
void PABindRotationValue::Transform(const Fmatrix& m) {}
void PABindRotationValue::Execute(ParticleHolder* effect, const float dt, float& tm_max) {
	for (u32 i = 0; i < effect->p_count; i++)
	{
		Particle& m = effect->particles[i];
		m.rot.x = BindValue.x;
	}
}
void* PABindRotationValue::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::BindValue:
		return &BindValue;
	}
	R_ASSERT3(false, "Particle action BindRotationValue: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
void PABindSizeValue::Transform(const Fmatrix& m) {}
void PABindSizeValue::Execute(ParticleHolder* effect, const float dt, float& tm_max)
{
	for (u32 i = 0; i < effect->p_count; i++)
	{
		Particle& m = effect->particles[i];

		m.size = BindValue;
	}
}
void* PABindSizeValue::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::BindValue:
		return &BindValue;
	case EVariable::Pivot:
		return &Pivot;
	}
	R_ASSERT3(false, "Particle action BindSizeValue: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
void PABindColorValue::Transform(const Fmatrix& m) {}
void PABindColorValue::Execute(ParticleHolder* effect, const float dt, float& tm_max) {
	Fcolor c_p, c_t;

	for (u32 i = 0; i < effect->p_count; i++)
	{
		Particle& m = effect->particles[i];

		c_p.set(m.color);
		c_t.set(BindValue.x, BindValue.y, BindValue.z, c_p.a);
		m.color = c_t.get();
	}
}
void* PABindColorValue::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::BindValue:
		return &BindValue;
	}
	R_ASSERT3(false, "Particle action BindColorValue: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
void PABindColorAlpha::Transform(const Fmatrix& m) {}
void PABindColorAlpha::Execute(ParticleHolder* effect, const float dt, float& tm_max) {
	Fcolor c_p, c_t;

	for (u32 i = 0; i < effect->p_count; i++)
	{
		Particle& m = effect->particles[i];

		c_p.set(m.color);
		c_t.set(c_p.r, c_p.g, c_p.b, BindValue);
		m.color = c_t.get();
	}
}
void* PABindColorAlpha::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::BindValue:
		return &BindValue;
	}
	R_ASSERT3(false, "Particle action BindColorAlpha: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}

// Animators
void PAColorAnimator::Transform(const Fmatrix& m)
{
}
void PAColorAnimator::Execute(ParticleHolder* effect, const float dt, float& tm_max) {
	//auto CurrentValue = AnimPtr->FastUpdateValue(CurrentIndex, CurrentTime, dt, Looped, Reverse);
	Fcolor c_t;
	for(u32 i = 0; i < effect->p_count; i++)
	{
		Particle &m = effect->particles[i];
		float CurveTime = Reverse ? AnimPtr->GetMaxTime()- m.age*1000 : m.age*1000;
		if (Looped)
		{
			while (CurveTime < 0)
			{
				CurveTime += AnimPtr->GetMaxTime();
			}
			while (CurveTime > AnimPtr->GetMaxTime())
			{
				CurveTime -= AnimPtr->GetMaxTime();
			}
		} else
		{
			clamp(CurveTime, 0.0f, AnimPtr->GetMaxTime());
		}
		Fvector4 CurrentValue = AnimPtr->GetValueOnTime(CurveTime);
		c_t.set(
			CurrentValue.x,
			CurrentValue.y,
			CurrentValue.z,
			CurrentValue.w
		);
		m.color = c_t.get();
	}
}
void* PAColorAnimator::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::Animator:
		return &Animator;
	case EVariable::Looped:
		return &Looped;
	case EVariable::Reverse:
		return &Reverse;
	}
	R_ASSERT3(false, "Particle action ColorAnimator: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
void PASizeAnimator::Transform(const Fmatrix& m)
{
}
void PASizeAnimator::Execute(ParticleHolder* effect, const float dt, float& tm_max) {
	//Fvector4 CurrentValue = AnimPtr->FastUpdateValue(CurrentIndex, CurrentTime, dt, Looped, Reverse);
	for(u32 i = 0; i < effect->p_count; i++)
	{
		Particle &m = effect->particles[i];
		float CurveTime = Reverse ? AnimPtr->GetMaxTime()- m.age*1000 : m.age*1000;
		if (Looped)
		{
			while (CurveTime < 0)
			{
				CurveTime += AnimPtr->GetMaxTime();
			}
			while (CurveTime > AnimPtr->GetMaxTime())
			{
				CurveTime -= AnimPtr->GetMaxTime();
			}
		} else
		{
			clamp(CurveTime, 0.0f, AnimPtr->GetMaxTime());
		}
		Fvector4 CurrentValue = AnimPtr->GetValueOnTime(CurveTime);
		m.size.x = CurrentValue.x;
		m.size.y = CurrentValue.y;
		m.size.z = CurrentValue.z;
	}
}
void* PASizeAnimator::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::Animator:
		return &Animator;
	case EVariable::Looped:
		return &Looped;
	case EVariable::Reverse:
		return &Reverse;
	}
	R_ASSERT3(false, "Particle action SizeAnimator: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
void PAVelocityAnimator::Transform(const Fmatrix& m)
{
}
void PAVelocityAnimator::Execute(ParticleHolder* effect, const float dt, float& tm_max) {

	for(u32 i = 0; i < effect->p_count; i++)
	{
		
		Particle &m = effect->particles[i];
		Fmatrix MDir;
		{
			Fvector Dir = m.rot_vel;
			if(fis_zero(Dir.magnitude())) continue;
			Dir.normalize_safe();
			Fvector ChooseAxis = {0, 0, 1};
			if (fsimilar(ChooseAxis.dotproduct(Dir), 1))
			{
				ChooseAxis = {1, 0, 0};
			}
			Fvector Cross;
			Cross.crossproduct(Dir, ChooseAxis);
			Cross.normalize();
			MDir.rotation(Dir, Cross);
		}
		float CurveTime = Reverse ? AnimPtr->GetMaxTime()- m.age*1000 : m.age*1000;
		if (Looped)
		{
			while (CurveTime < 0)
			{
				CurveTime += AnimPtr->GetMaxTime();
			}
			while (CurveTime > AnimPtr->GetMaxTime())
			{
				CurveTime -= AnimPtr->GetMaxTime();
			}
		} else
		{
			clamp(CurveTime, 0.0f, AnimPtr->GetMaxTime());
		}
		Fvector4 CurrentValue = AnimPtr->GetValueOnTime(CurveTime);
		Fvector LocalVelocity = {CurrentValue.x,CurrentValue.y,CurrentValue.z};
		MDir.transform(m.vel, LocalVelocity);
	}
}
void* PAVelocityAnimator::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::Animator:
		return &Animator;
	case EVariable::Looped:
		return &Looped;
	case EVariable::Reverse:
		return &Reverse;
	}
	R_ASSERT3(false, "Particle action VelocityAnimator: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
void PAVelocityRotationAnimator::Transform(const Fmatrix& m)
{
}
void PAVelocityRotationAnimator::Execute(ParticleHolder* effect, const float dt, float& tm_max) {

	for(u32 i = 0; i < effect->p_count; i++)
	{
		
		Particle &m = effect->particles[i];
		Fmatrix MDir;
		{
			Fvector Dir = m.rot_velS;
			if(fis_zero(Dir.magnitude())) continue;
			Dir.normalize_safe();
			Fvector ChooseAxis = {0, 0, 1};
			if (fsimilar(ChooseAxis.dotproduct(Dir), 1))
			{
				ChooseAxis = {1, 0, 0};
			}
			Fvector Cross;
			Cross.crossproduct(Dir, ChooseAxis);
			Cross.normalize();
			MDir.rotation(Dir, Cross);
		}
		float CurveTime = Reverse ? AnimPtr->GetMaxTime()- m.age*1000 : m.age*1000;
		if (Looped)
		{
			while (CurveTime < 0)
			{
				CurveTime += AnimPtr->GetMaxTime();
			}
			while (CurveTime > AnimPtr->GetMaxTime())
			{
				CurveTime -= AnimPtr->GetMaxTime();
			}
		} else
		{
			clamp(CurveTime, 0.0f, AnimPtr->GetMaxTime());
		}
		Fvector4 CurrentValue = AnimPtr->GetValueOnTime(CurveTime);
		Fvector LocalVelocity = {CurrentValue.x,CurrentValue.y,CurrentValue.z};
		MDir.transform(m.rot_vel, LocalVelocity);
	}
}
void* PAVelocityRotationAnimator::GetVariableImpl(u8 VarID)
{
	switch ((EVariable)VarID)
	{
	case EVariable::Animator:
		return &Animator;
	case EVariable::Looped:
		return &Looped;
	case EVariable::Reverse:
		return &Reverse;
	}
	R_ASSERT3(false, "Particle action VelocityRotationAnimator: Invalid Variable ID", std::to_string(VarID).c_str());
	return nullptr;
}
//-------------------------------------------------------------------------------------------------

