#pragma once

#include <box3d/math_functions.h>

// The engine is left-handed with +Y up, Box3D math is right-handed.
// Mirroring the Z axis maps one to the other: vectors flip Z, quaternions
// negate Z (see ToB3 below), triangles reverse their winding.
namespace B3Math
{
	inline b3Vec3 ToB3(const Fvector& V)
	{
		return {V.x, V.y, -V.z};
	}

	inline Fvector FromB3(const b3Vec3& V)
	{
		Fvector Result;
		Result.set(V.x, V.y, -V.z);
		return Result;
	}

	// Angular velocity and torque are pseudovectors: a mirror flips them once more.
	inline b3Vec3 AngularToB3(const Fvector& V)
	{
		return {-V.x, -V.y, V.z};
	}

	inline Fvector AngularFromB3(const b3Vec3& V)
	{
		Fvector Result;
		Result.set(-V.x, -V.y, V.z);
		return Result;
	}

	// Fmatrix applies to row vectors (v * M), so an engine quaternion describes the inverse
	// of the standard rotation. Conjugating and then mirroring Z leaves only Z negated.
	inline b3Quat ToB3(const Fquaternion& Q)
	{
		return {{Q.x, Q.y, -Q.z}, Q.w};
	}

	inline Fquaternion FromB3(const b3Quat& Q)
	{
		Fquaternion Result;
		Result.set(Q.s, Q.v.x, Q.v.y, -Q.v.z);
		return Result;
	}

	// Rigid transform without scale.
	inline b3Transform ToB3(const Fmatrix& M)
	{
		Fquaternion Q;
		Q.set(M);
		Q.normalize();
		return {ToB3(M.c), ToB3(Q)};
	}

	inline Fmatrix FromB3(const b3Transform& T)
	{
		Fmatrix Result;
		Result.mk_xform(FromB3(T.q), FromB3(T.p));
		return Result;
	}
}
