#ifndef SHAPE_COLLISION_SHAPES_H
#define SHAPE_COLLISION_SHAPES_H

#include "ShapeCollision.h"

//The exact support functions of the engine primitives. They are the only thing the
//narrow phase needs from a shape, so a sphere or a cone is never replaced by a
//polygonal copy.
namespace ShapeCollision
{
	template <class T>
	struct shape_support<_box3<T>>
	{
		using V = _vector3<T>;
		using Scalar = T;

		static V Support(const _box3<T>& s, const V& direction)
		{
			V point;
			point.set(
				(direction.x >= T(0)) ? s.max.x : s.min.x,
				(direction.y >= T(0)) ? s.max.y : s.min.y,
				(direction.z >= T(0)) ? s.max.z : s.min.z);

			return point;
		}

		static void Bounds(const _box3<T>& s, V& center, T& radius)
		{
			V extents;
			s.getsize(extents);
			s.getcenter(center);

			radius = extents.magnitude() * T(0.5);
		}
	};

	template <class T>
	struct shape_box<_box3<T>>
	{
		static constexpr bool Value = true;

		static void Axes(const _box3<T>& s, _vector3<T>& center, _vector3<T> axis[3], T half[3])
		{
			_vector3<T> extents;
			s.getsize(extents);
			s.getcenter(center);

			axis[0].set(T(1), T(0), T(0));
			axis[1].set(T(0), T(1), T(0));
			axis[2].set(T(0), T(0), T(1));

			half[0] = extents.x * T(0.5);
			half[1] = extents.y * T(0.5);
			half[2] = extents.z * T(0.5);
		}
	};

	template <class T>
	struct shape_support<_obb<T>>
	{
		using V = _vector3<T>;
		using Scalar = T;

		static V Support(const _obb<T>& s, const V& direction)
		{
			V point = s.m_translate;

			const T x = direction.dotproduct(s.m_rotate.i);
			const T y = direction.dotproduct(s.m_rotate.j);
			const T z = direction.dotproduct(s.m_rotate.k);

			point.mad(s.m_rotate.i, (x >= T(0)) ? s.m_halfsize.x : -s.m_halfsize.x);
			point.mad(s.m_rotate.j, (y >= T(0)) ? s.m_halfsize.y : -s.m_halfsize.y);
			point.mad(s.m_rotate.k, (z >= T(0)) ? s.m_halfsize.z : -s.m_halfsize.z);

			return point;
		}

		static void Bounds(const _obb<T>& s, V& center, T& radius)
		{
			center = s.m_translate;
			radius = s.m_halfsize.magnitude();
		}
	};

	template <class T>
	struct shape_box<_obb<T>>
	{
		static constexpr bool Value = true;

		static void Axes(const _obb<T>& s, _vector3<T>& center, _vector3<T> axis[3], T half[3])
		{
			center = s.m_translate;

			axis[0] = s.m_rotate.i;
			axis[1] = s.m_rotate.j;
			axis[2] = s.m_rotate.k;

			half[0] = s.m_halfsize.x;
			half[1] = s.m_halfsize.y;
			half[2] = s.m_halfsize.z;
		}
	};

	template <class T>
	struct shape_support<_sphere<T>>
	{
		using V = _vector3<T>;
		using Scalar = T;

		static V Support(const _sphere<T>& s, const V& direction)
		{
			V point = s.P;

			const T length = direction.magnitude();

			if (length > T(1e-12))
				point.mad(direction, s.R / length);
			else
				point.x += s.R;

			return point;
		}

		static void Bounds(const _sphere<T>& s, V& center, T& radius)
		{
			center = s.P;
			radius = s.R;
		}
	};

	template <class T>
	struct shape_sphere<_sphere<T>>
	{
		static constexpr bool Value = true;

		static _vector3<T> Center(const _sphere<T>& s)
		{
			return s.P;
		}

		static T Radius(const _sphere<T>& s)
		{
			return s.R;
		}
	};

	template <class T>
	struct shape_support<_cylinder<T>>
	{
		using V = _vector3<T>;
		using Scalar = T;

		static V Axis(const _cylinder<T>& s)
		{
			V axis = s.m_direction;

			if (axis.square_magnitude() > T(1e-12))
				axis.normalize();
			else
				axis.set(T(0), T(1), T(0));

			return axis;
		}

		static V Support(const _cylinder<T>& s, const V& direction)
		{
			const V axis = Axis(s);

			const T axial = direction.dotproduct(axis);

			V point;
			point.mad(s.m_center, axis, (axial >= T(0)) ? T(0.5) * s.m_height : -T(0.5) * s.m_height);

			V radial = direction;
			radial.mad(axis, -axial);

			const T length = radial.magnitude();

			if (length > T(1e-9))
				point.mad(radial, s.m_radius / length);

			return point;
		}

		static void Bounds(const _cylinder<T>& s, V& center, T& radius)
		{
			center = s.m_center;
			radius = _sqrt(T(0.25) * s.m_height * s.m_height + s.m_radius * s.m_radius);
		}
	};

	template <class T>
	struct shape_support<_cone<T>>
	{
		using V = _vector3<T>;
		using Scalar = T;

		static V Axis(const _cone<T>& s)
		{
			V axis = s.m_direction;

			if (axis.square_magnitude() > T(1e-12))
				axis.normalize();
			else
				axis.set(T(0), T(1), T(0));

			return axis;
		}

		static V Support(const _cone<T>& s, const V& direction)
		{
			const V axis = Axis(s);

			//The apex ...
			const V apex = s.m_translate;

			//... and the point of the base rim:
			V radial = direction;
			radial.mad(axis, -direction.dotproduct(axis));

			V base;
			base.mad(apex, axis, s.m_height);

			const T length = radial.magnitude();

			if (length > T(1e-9))
			{
				base.mad(radial, s.m_radius / length);
			}
			else
			{
				V u, v;
				V::generate_orthonormal_basis(axis, u, v);
				base.mad(u, s.m_radius);
			}

			return (direction.dotproduct(apex) >= direction.dotproduct(base)) ? apex : base;
		}

		static void Bounds(const _cone<T>& s, V& center, T& radius)
		{
			const V axis = Axis(s);

			center.mad(s.m_translate, axis, T(0.5) * s.m_height);
			radius = _sqrt(T(0.25) * s.m_height * s.m_height + s.m_radius * s.m_radius);
		}
	};
	template <class T>
	ICF T BoxSignedDistance(const _vector3<T>& local, const _vector3<T>& half, _vector3<T>& normal)
	{
		T inside = -std::numeric_limits<T>::max();
		u32 best = 0;

		for (u32 i = 0; i < 3; ++i)
		{
			const T offset = std::abs(local[i]) - half[i];

			if (offset > inside)
			{
				inside = offset;
				best = i;
			}
		}

		if (inside <= T(0))
		{
			normal.set(T(0), T(0), T(0));
			normal[best] = (local[best] >= T(0)) ? T(1) : T(-1);
			return inside;
		}

		_vector3<T> closest;
		closest.set(
			clampr(local.x, -half.x, half.x),
			clampr(local.y, -half.y, half.y),
			clampr(local.z, -half.z, half.z));

		_vector3<T> diff;
		diff.sub(local, closest);

		const T length = diff.magnitude();

		if (length > T(1e-9))
		{
			normal.mul(diff, T(1) / length);
			return length;
		}

		normal.set(T(0), T(0), T(0));
		normal[best] = (local[best] >= T(0)) ? T(1) : T(-1);
		return T(0);
	}

	template <class T>
	struct shape_distance<_box3<T>>
	{
		using V = _vector3<T>;

		static T Signed(const _box3<T>& s, const V& point, V& normal)
		{
			V center, extents;
			s.getcenter(center);
			s.getsize(extents);

			V half;
			half.mul(extents, T(0.5));

			V local;
			local.sub(point, center);

			return BoxSignedDistance(local, half, normal);
		}
	};

	template <class T>
	struct shape_distance<_obb<T>>
	{
		using V = _vector3<T>;

		static T Signed(const _obb<T>& s, const V& point, V& normal)
		{
			V diff;
			diff.sub(point, s.m_translate);

			V local;
			local.set(diff.dotproduct(s.m_rotate.i), diff.dotproduct(s.m_rotate.j), diff.dotproduct(s.m_rotate.k));

			V local_normal;
			const T distance = BoxSignedDistance(local, s.m_halfsize, local_normal);

			normal.set(T(0), T(0), T(0));
			normal.mad(s.m_rotate.i, local_normal.x);
			normal.mad(s.m_rotate.j, local_normal.y);
			normal.mad(s.m_rotate.k, local_normal.z);

			return distance;
		}
	};

	template <class T>
	struct shape_distance<_sphere<T>>
	{
		using V = _vector3<T>;

		static T Signed(const _sphere<T>& s, const V& point, V& normal)
		{
			V diff;
			diff.sub(point, s.P);

			const T length = diff.magnitude();

			if (length > T(1e-9))
				normal.mul(diff, T(1) / length);
			else
				normal.set(T(1), T(0), T(0));

			return length - s.R;
		}
	};

	template <class T>
	struct shape_distance<_cylinder<T>>
	{
		using V = _vector3<T>;

		static T Signed(const _cylinder<T>& s, const V& point, V& normal)
		{
			const V axis = shape_support<_cylinder<T>>::Axis(s);

			V diff;
			diff.sub(point, s.m_center);

			const T axial = diff.dotproduct(axis);

			V radial = diff;
			radial.mad(axis, -axial);

			const T rho = radial.magnitude();

			V radial_dir;

			if (rho > T(1e-9))
				radial_dir.mul(radial, T(1) / rho);
			else
				radial_dir.set(T(1), T(0), T(0));

			V axial_dir = axis;

			if (axial < T(0))
				axial_dir.invert();

			const T half = T(0.5) * s.m_height;
			const T dr = rho - s.m_radius;
			const T dz = std::abs(axial) - half;

			if (dr <= T(0) && dz <= T(0))
			{
				if (dr > dz)
				{
					normal = radial_dir;
					return dr;
				}

				normal = axial_dir;
				return dz;
			}

			if (dr > T(0) && dz > T(0))
			{
				const T distance = _sqrt(dr * dr + dz * dz);

				normal = radial_dir;
				normal.mul(dr);
				normal.mad(axial_dir, dz);

				if (distance > T(1e-9))
					normal.mul(T(1) / distance);

				return distance;
			}

			if (dr > T(0))
			{
				normal = radial_dir;
				return dr;
			}

			normal = axial_dir;
			return dz;
		}
	};

	template <class T>
	struct shape_distance<_cone<T>>
	{
		using V = _vector3<T>;

		static T Signed(const _cone<T>& s, const V& point, V& normal)
		{
			const V axis = shape_support<_cone<T>>::Axis(s);

			V diff;
			diff.sub(point, s.m_translate);

			const T axial = diff.dotproduct(axis);

			V radial = diff;
			radial.mad(axis, -axial);

			const T rho = radial.magnitude();

			V radial_dir;

			if (rho > T(1e-9))
				radial_dir.mul(radial, T(1) / rho);
			else
				radial_dir.set(T(1), T(0), T(0));

			const T height = s.m_height;
			const T radius = s.m_radius;

			if (height <= T(1e-9))
			{
				normal = radial_dir;
				return rho - radius;
			}

			const T line_length = _sqrt(radius * radius + height * height);
			const T lateral = (height * rho - radius * axial) / line_length;
			const T base = axial - height;

			//Inside.
			if (axial >= T(0) && axial <= height && rho <= radius * (axial / height))
			{
				if (lateral >= base)
				{
					normal = radial_dir;
					normal.mul(height);
					normal.mad(axis, -radius);
					normal.mul(T(1) / line_length);
					return lateral;
				}

				normal = axis;
				return base;
			}

			//Behind the apex.
			if (axial <= T(0))
			{
				const T distance = diff.magnitude();

				if (distance > T(1e-9))
					normal.mul(diff, T(1) / distance);
				else
					normal.invert(axis);

				return distance;
			}

			//Above the base plane: the base disk or its rim.
			if (axial >= height)
			{
				const T dr = rho - radius;

				if (dr <= T(0))
				{
					normal = axis;
					return base;
				}

				const T dz = axial - height;
				const T distance = _sqrt(dr * dr + dz * dz);

				normal = radial_dir;
				normal.mul(dr);
				normal.mad(axis, dz);

				if (distance > T(1e-9))
					normal.mul(T(1) / distance);

				return distance;
			}

			//Between the apex and the base, outside the lateral surface.
			const T projection = (rho * radius + axial * height) / (radius * radius + height * height);

			if (projection >= T(1))
			{
				const T dr = rho - radius;
				const T dz = axial - height;
				const T distance = _sqrt(dr * dr + dz * dz);

				normal = radial_dir;
				normal.mul(dr);
				normal.mad(axis, dz);

				if (distance > T(1e-9))
					normal.mul(T(1) / distance);

				return distance;
			}

			normal = radial_dir;
			normal.mul(height);
			normal.mad(axis, -radius);
			normal.mul(T(1) / line_length);

			return lateral;
		}
	};
	template <class T>
	struct shape_points<_box3<T>>
	{
		using V = _vector3<T>;

		static u32 Collect(const _box3<T>& s, const V&, V* out)
		{
			for (u32 i = 0; i < 8; ++i)
			{
				out[i].set(
					(i & 1) ? s.max.x : s.min.x,
					(i & 2) ? s.max.y : s.min.y,
					(i & 4) ? s.max.z : s.min.z);
			}

			return 8;
		}
	};

	template <class T>
	struct shape_points<_obb<T>>
	{
		using V = _vector3<T>;

		static u32 Collect(const _obb<T>& s, const V&, V* out)
		{
			for (u32 i = 0; i < 8; ++i)
			{
				V& point = out[i];

				point = s.m_translate;
				point.mad(s.m_rotate.i, (i & 1) ? s.m_halfsize.x : -s.m_halfsize.x);
				point.mad(s.m_rotate.j, (i & 2) ? s.m_halfsize.y : -s.m_halfsize.y);
				point.mad(s.m_rotate.k, (i & 4) ? s.m_halfsize.z : -s.m_halfsize.z);
			}

			return 8;
		}
	};

	template <class T>
	struct shape_points<_sphere<T>>
	{
		using V = _vector3<T>;

		static u32 Collect(const _sphere<T>& s, const V& direction, V* out)
		{
			out[0].mad(s.P, direction, s.R);
			return 1;
		}
	};

	template <class T>
	struct shape_points<_cylinder<T>>
	{
		using V = _vector3<T>;

		static u32 Collect(const _cylinder<T>& s, const V& direction, V* out)
		{
			const V axis = shape_support<_cylinder<T>>::Axis(s);

			V radial = direction;
			radial.mad(axis, -direction.dotproduct(axis));

			//The rim is sampled around the direction towards the other body: a lying
			//cylinder gets its lowest generatrix exactly, a standing one gets the whole cap
			//rim instead of the single point that made it dance on the floor.
			V u, v;

			if (radial.magnitude() > T(1e-9))
			{
				u.mul(radial, T(1) / radial.magnitude());
				v.crossproduct(axis, u);
				v.normalize_safe();
			}
			else
			{
				V::generate_orthonormal_basis(axis, u, v);
			}

			const u32 sides = 8;
			u32 count = 0;

			for (u32 cap = 0; cap < 2; ++cap)
			{
				V center;
				center.mad(s.m_center, axis, (cap == 0) ? -T(0.5) * s.m_height : T(0.5) * s.m_height);

				for (u32 i = 0; i < sides; ++i)
				{
					const T angle = T(6.283185307179586) * T(i) / T(sides);

					out[count] = center;
					out[count].mad(u, s.m_radius * std::cos(angle));
					out[count].mad(v, s.m_radius * std::sin(angle));
					++count;
				}
			}

			//The middle of the lateral surface: a cylinder poking into the side of a box
			//touches it there, not with its rims.
			for (u32 i = 0; i < 4; ++i)
			{
				const T angle = T(1.5707963267948966) * T(i);

				out[count] = s.m_center;
				out[count].mad(u, s.m_radius * std::cos(angle));
				out[count].mad(v, s.m_radius * std::sin(angle));
				++count;
			}

			return count;
		}
	};

	template <class T>
	struct shape_points<_cone<T>>
	{
		using V = _vector3<T>;

		static u32 Collect(const _cone<T>& s, const V& direction, V* out)
		{
			const V axis = shape_support<_cone<T>>::Axis(s);

			u32 count = 0;

			out[count++] = s.m_translate;

			V radial = direction;
			radial.mad(axis, -direction.dotproduct(axis));

			V u, v;

			if (radial.magnitude() > T(1e-9))
			{
				u.mul(radial, T(1) / radial.magnitude());
				v.crossproduct(axis, u);
				v.normalize_safe();
			}
			else
			{
				V::generate_orthonormal_basis(axis, u, v);
			}

			V base;
			base.mad(s.m_translate, axis, s.m_height);

			const u32 sides = 8;

			for (u32 i = 0; i < sides; ++i)
			{
				const T angle = T(6.283185307179586) * T(i) / T(sides);

				out[count] = base;
				out[count].mad(u, s.m_radius * std::cos(angle));
				out[count].mad(v, s.m_radius * std::sin(angle));
				++count;
			}

			//The middle of the lateral surface, the cone radius is halved there.
			V middle;
			middle.mad(s.m_translate, axis, T(0.5) * s.m_height);

			for (u32 i = 0; i < 4; ++i)
			{
				const T angle = T(1.5707963267948966) * T(i);

				out[count] = middle;
				out[count].mad(u, T(0.5) * s.m_radius * std::cos(angle));
				out[count].mad(v, T(0.5) * s.m_radius * std::sin(angle));
				++count;
			}

			return count;
		}
	};
}

#endif // SHAPE_COLLISION_SHAPES_H
