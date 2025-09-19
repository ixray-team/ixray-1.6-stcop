#pragma once

namespace XRay::Collision
{
	ECORE_API bool intersect(const Fmatrix& object_transform, const IKinematics& K, const Fvector& origin, const Fvector& direction, u16& bone_id, float& dist, Fvector& norm);
}