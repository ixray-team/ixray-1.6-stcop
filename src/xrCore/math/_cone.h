#ifndef _CONE_H
#define _CONE_H

//The cone: the apex is m_translate, the axis is m_direction (unit length), the base
//radius m_radius is at the distance m_height from the apex.
//
//Everything below is analytic: the infinite cone is the quadric
//  |p_perp|^2 - k^2*(p.a)^2 <= 0,  k = radius/height,  p = point - apex,
//so the intersection of the cone with a triangle plane is a conic section. The
//triangles are tested by minimizing that quadric over the triangle clipped by the
//axial band 0 <= p.a <= height.
template <class T>
class _cone
{
public:
	typedef T			TYPE;
	typedef _cone<T>	Self;
	typedef Self&		SelfRef;
	typedef const Self&	SelfCRef;

public:
	_vector3<T>	m_translate;	//apex
	_vector3<T>	m_direction;	//axis, unit length
	T			m_height = 0;
	T			m_radius = 0;

public:
	ICF bool operator==(SelfCRef Left)
	{
		return Left.m_translate == m_translate && Left.m_direction == m_direction && Left.m_height == m_height && Left.m_radius == m_radius;
	}

	ICF SelfRef	invalidate	()	{ m_translate.set(0,0,0); m_direction.set(0,0,0); m_height=0; m_radius=0; return *this; }

	//A cheap distance reject first, then the cone is tested as its bounding OBB:
	//the OBB encloses the cone, so a reported separation is a real one.
	ICF bool	intersectAABB	(const _vector3<T>& aabbCenter, const _vector3<T>& aabbExtents) const
	{
		const T maxConeSize = std::max(m_radius, T(0.5) * m_height);
		const T maxBoxSize = aabbExtents.magnitude();

		_vector3<T> center;
		center.mad(m_translate, m_direction, T(0.5) * m_height);

		_vector3<T> diff; diff.sub(aabbCenter, center);

		if (diff.magnitude() > maxConeSize + maxBoxSize + T(1))
			return false;

		_vector3<T> kW, kU, kV;
		local_basis(kW, kU, kV);

		_obb<T> obb;
		obb.m_translate.set(center);
		obb.m_halfsize.set(m_radius, m_radius, T(0.5) * m_height);

		obb.m_rotate.i = kU;
		obb.m_rotate.j = kV;
		obb.m_rotate.k = kW;

		return obb.intersectAABB(aabbCenter, aabbExtents);
	}

	ICF bool	intersectTri	(const _vector3<T>* verts, bool bClass3 = true) const
	{
		if (m_height <= T(1e-6))
			return false;

		//Thirteen separating axes as a fast reject. The cone projection intervals are
		//exact, bClass3 enables the cross product axes of its basis with the edges.
		_vector3<T> axes[13];
		u32 total_axes = 3;

		_vector3<T> kW = m_direction;
		_vector3<T> kU, kV;
		_vector3<T>::generate_orthonormal_basis(kW, kU, kV);

		axes[0] = kU;
		axes[1] = kV;
		axes[2] = kW;

		_vector3<T> edge1, edge2;
		edge1.sub(verts[1], verts[0]);
		edge2.sub(verts[2], verts[0]);

		_vector3<T> tri_normal;
		tri_normal.crossproduct(edge1, edge2);

		if (tri_normal.square_magnitude() > T(0.0001))
		{
			axes[total_axes].normalize(tri_normal);
			total_axes++;
		}

		if (bClass3)
		{
			_vector3<T> edges[3];
			edges[0] = edge1;
			edges[1].sub(verts[2], verts[1]);
			edges[2].sub(verts[0], verts[2]);

			for (u8 i = 0; i < 3; i++)
			{
				for (u8 j = 0; j < 3; j++)
				{
					_vector3<T> axis;
					axis.crossproduct(axes[j], edges[i]);

					if (axis.square_magnitude() > T(0.0001) && total_axes < 13)
					{
						axes[total_axes].normalize(axis);
						total_axes++;
					}
				}
			}
		}

		for (u32 i = 0; i < total_axes; i++)
		{
			if (axes[i].square_magnitude() < T(0.0001))
				continue;

			const T p0 = verts[0].dotproduct(axes[i]);
			const T p1 = verts[1].dotproduct(axes[i]);
			const T p2 = verts[2].dotproduct(axes[i]);

			const T minTri = std::min(p0, std::min(p1, p2));
			const T maxTri = std::max(p0, std::max(p1, p2));

			T coneMin, coneMax;
			project_interval(axes[i], coneMin, coneMax);

			const T epsilon = T(0.0001);
			if (maxTri < coneMin - epsilon || minTri > coneMax + epsilon)
				return false;
		}

		//A degenerate triangle is a segment: test its edges against the cone.
		if (tri_normal.square_magnitude() <= T(1e-12))
		{
			return	IsPointInCone(verts[0]) ||
					segment_intersects_cone(verts[0], verts[1]) ||
					segment_intersects_cone(verts[1], verts[2]) ||
					segment_intersects_cone(verts[0], verts[2]);
		}

		//Exact answer: the minimum of the cone quadric over the triangle clipped by the
		//axial band. It is not positive only when the triangle touches the cone.
		T min_value;
		_vector3<T> point;

		if (!plane_section_min(verts, min_value, point))
			return false;

		return min_value <= T(0);
	}

	ICF bool	IsPointInCone	(const _vector3<T>& point) const
	{
		if (m_height <= T(0))
			return false;

		_vector3<T> to_point;
		to_point.sub(point, m_translate);

		const T height_proj = to_point.dotproduct(m_direction);

		//Behind the apex.
		if (height_proj < T(0))
			return false;

		const T allowed_radius = (m_radius / m_height) * height_proj;

		_vector3<T> radial_component;
		radial_component.mad(to_point, m_direction, -height_proj);

		const T actual_radius_sqr = radial_component.square_magnitude();
		const T allowed_radius_sqr = allowed_radius * allowed_radius;

		return actual_radius_sqr <= allowed_radius_sqr;
	}

	//Contact points of the triangle with the cone: the triangle vertices inside, the
	//points where its edges cross the cone surface and the deepest point of the
	//section (the minimum of the cone quadric over the triangle, which is the center of
	//the conic section for the ellipse case).
	ICF void	FindContactsClipping	(const _vector3<T>* tri, void (*callback)(const _vector3<T>& contact, void* user_data), void* user_data = nullptr) const
	{
		if (!callback)
			return;

		for (u8 i = 0; i < 3; ++i)
		{
			if (IsPointInCone(tri[i]))
				callback(tri[i], user_data);
		}

		for (u8 i = 0; i < 3; ++i)
		{
			const u8 next = (i + 1) % 3;

			T hits[3];
			const u8 count = segment_cone_intersections(tri[i], tri[next], hits);

			_vector3<T> dir;
			dir.sub(tri[next], tri[i]);

			for (u8 k = 0; k < count; ++k)
			{
				_vector3<T> point;
				point.mad(tri[i], dir, hits[k]);

				callback(point, user_data);
			}
		}

		if (m_height <= T(1e-6))
			return;

		T min_value;
		_vector3<T> point;

		if (plane_section_min(tri, min_value, point) && min_value <= T(0))
			callback(point, user_data);
	}
//----------------------------------------------------------------------------
private:
	struct point2
	{
		T u, v;
	};

	//Q(u, v) = A*u^2 + 2*B*u*v + C*v^2 + D*u + E*v + F
	struct quadric2
	{
		T A, B, C, D, E, F;
	};

	ICF void	local_basis	(_vector3<T>& kW, _vector3<T>& kU, _vector3<T>& kV) const
	{
		kW = m_direction;
		if (kW.square_magnitude() > T(1e-12))
			kW.normalize();
		else
			kW.set(T(0), T(1), T(0));

		_vector3<T>::generate_orthonormal_basis(kW, kU, kV);
	}

	//Exact projection interval of the cone onto the unit axis w.
	ICF void	project_interval	(const _vector3<T>& w, T& out_min, T& out_max) const
	{
		const T dot_axis = w.dotproduct(m_direction);
		const T dot_radial = _sqrt(std::max(T(0), T(1) - dot_axis * dot_axis));

		const T k = m_radius / m_height;

		//For a fixed distance t from the apex the projection is
		//w*A + t*(w*a +- k*|w_perp|), so the low and the high border grow with
		//different slopes.
		const T slope_min = dot_axis - k * dot_radial;
		const T slope_max = dot_axis + k * dot_radial;

		const T apex_proj = w.dotproduct(m_translate);

		out_min = apex_proj + std::min(T(0), slope_min * m_height);
		out_max = apex_proj + std::max(T(0), slope_max * m_height);
	}

	//The cone quadric in the triangle plane: Q(u, v) <= 0 inside the infinite cone,
	//the axial coordinate is t(u, v) = a0 + au*u + av*v.
	ICF void	build_plane_quadric	(const _vector3<T>* verts, _vector3<T>& e1, _vector3<T>& e2, quadric2& q, T& out_au, T& out_av, T& out_a0) const
	{
		_vector3<T> edge1, edge2, normal;
		edge1.sub(verts[1], verts[0]);
		edge2.sub(verts[2], verts[0]);
		normal.crossproduct(edge1, edge2);
		normal.normalize();

		e1 = edge1;
		e1.normalize();

		e2.crossproduct(normal, e1);
		e2.normalize();

		_vector3<T> diff; diff.sub(verts[0], m_translate);

		const T k = m_radius / m_height;
		const T s = T(1) + k * k;

		const T au = e1.dotproduct(m_direction);
		const T av = e2.dotproduct(m_direction);
		const T a0 = diff.dotproduct(m_direction);

		q.A = T(1) - s * au * au;
		q.B = -s * au * av;
		q.C = T(1) - s * av * av;
		q.D = T(2) * (diff.dotproduct(e1) - s * a0 * au);
		q.E = T(2) * (diff.dotproduct(e2) - s * a0 * av);
		q.F = diff.square_magnitude() - s * a0 * a0;

		out_au = au;
		out_av = av;
		out_a0 = a0;
	}

	ICF static T	eval_quadric	(const quadric2& q, T u, T v)
	{
		return q.A * u * u + T(2) * q.B * u * v + q.C * v * v + q.D * u + q.E * v + q.F;
	}

	//Clips a convex 2D polygon by the half space normal*p <= offset.
	ICF static u8	clip2d	(const point2* src, u8 count, T normal_u, T normal_v, T offset, point2* dst)
	{
		u8 out = 0;

		for (u8 i = 0; i < count; ++i)
		{
			const point2& a = src[i];
			const point2& b = src[(i + 1) % count];

			const T da = normal_u * a.u + normal_v * a.v - offset;
			const T db = normal_u * b.u + normal_v * b.v - offset;

			if (da <= T(0) && out < 8)
				dst[out++] = a;

			if (((da > T(0)) != (db > T(0))) && out < 8)
			{
				const T t = da / (da - db);

				point2 point;
				point.u = a.u + (b.u - a.u) * t;
				point.v = a.v + (b.v - a.v) * t;

				dst[out++] = point;
			}
		}

		return out;
	}

	ICF static bool	point_in_polygon2d	(T u, T v, const point2* poly, u8 count)
	{
		for (u8 i = 0; i < count; ++i)
		{
			const point2& a = poly[i];
			const point2& b = poly[(i + 1) % count];

			//The polygons here are counter clockwise in the (u, v) frame.
			const T cross = (b.u - a.u) * (v - a.v) - (b.v - a.v) * (u - a.u);

			if (cross < T(-1e-6))
				return false;
		}

		return true;
	}

	//Minimum of the quadric over a convex polygon and the point where it is reached.
	//For a positive definite form the interior stationary point is the candidate, the
	//indefinite and the singular forms have no interior minimum, so the vertices and
	//the edges of the polygon cover them.
	ICF T	min_over_polygon	(const quadric2& q, const point2* poly, u8 count, point2& out_point) const
	{
		T best = type_max(T);

		out_point = poly[0];

		for (u8 i = 0; i < count; ++i)
		{
			const T value = eval_quadric(q, poly[i].u, poly[i].v);

			if (value < best)
			{
				best = value;
				out_point = poly[i];
			}
		}

		for (u8 i = 0; i < count; ++i)
		{
			const point2& a = poly[i];
			const point2& b = poly[(i + 1) % count];

			const T edge_u = b.u - a.u;
			const T edge_v = b.v - a.v;

			//Q(a + s*edge) = qa*s^2 + qb*s + qc
			const T qa = q.A * edge_u * edge_u + T(2) * q.B * edge_u * edge_v + q.C * edge_v * edge_v;
			const T qb = T(2) * (q.A * a.u * edge_u + q.B * (a.u * edge_v + a.v * edge_u) + q.C * a.v * edge_v) + q.D * edge_u + q.E * edge_v;
			const T qc = eval_quadric(q, a.u, a.v);

			T s = T(0);

			if (qa > T(1e-12))
				s = std::max(T(0), std::min(T(1), -qb / (T(2) * qa)));

			const T value = qa * s * s + qb * s + qc;

			if (value < best)
			{
				best = value;

				out_point.u = a.u + edge_u * s;
				out_point.v = a.v + edge_v * s;
			}
		}

		const T det = q.A * q.C - q.B * q.B;

		if (det > T(1e-12) && q.A > T(0))
		{
			const T u = (q.B * q.E - q.C * q.D) / (T(2) * det);
			const T v = (q.B * q.D - q.A * q.E) / (T(2) * det);

			if (point_in_polygon2d(u, v, poly, count))
			{
				const T value = eval_quadric(q, u, v);

				if (value < best)
				{
					best = value;

					out_point.u = u;
					out_point.v = v;
				}
			}
		}

		return best;
	}

	//Minimum of the cone quadric over the triangle clipped by the axial band.
	ICF bool	plane_section_min	(const _vector3<T>* verts, T& out_min, _vector3<T>& out_world) const
	{
		_vector3<T> edge1, edge2, normal;
		edge1.sub(verts[1], verts[0]);
		edge2.sub(verts[2], verts[0]);
		normal.crossproduct(edge1, edge2);

		if (normal.square_magnitude() <= T(1e-12))
			return false;

		_vector3<T> e1, e2;
		quadric2 quadric;
		T au, av, a0;

		build_plane_quadric(verts, e1, e2, quadric, au, av, a0);

		point2 triangle[3];
		triangle[0].u = T(0);
		triangle[0].v = T(0);
		triangle[1].u = edge1.magnitude();
		triangle[1].v = T(0);
		triangle[2].u = edge2.dotproduct(e1);
		triangle[2].v = edge2.dotproduct(e2);

		//The axial band: 0 <= a0 + au*u + av*v <= height.
		point2 band[8];
		point2 clipped[8];

		u8 count = clip2d(triangle, 3, -au, -av, a0, band);
		if (count < 3)
			return false;

		count = clip2d(band, count, au, av, m_height - a0, clipped);
		if (count < 3)
			return false;

		point2 minimum;
		out_min = min_over_polygon(quadric, clipped, count, minimum);

		out_world = verts[0];
		out_world.mad(e1, minimum.u);
		out_world.mad(e2, minimum.v);

		return true;
	}

	//Parameters t in [0, 1] where the segment crosses the cone surface.
	ICF u8		segment_cone_intersections	(const _vector3<T>& start, const _vector3<T>& end, T out_t[3]) const
	{
		if (m_height <= T(1e-6))
			return 0;

		u8 count = 0;

		const _vector3<T>& axis = m_direction;

		_vector3<T> dir; dir.sub(end, start);
		_vector3<T> from_apex; from_apex.sub(start, m_translate);

		const T dir_axis = dir.dotproduct(axis);
		const T start_axis = from_apex.dotproduct(axis);

		_vector3<T> dir_radial = dir;
		dir_radial.mad(dir_radial, axis, -dir_axis);

		_vector3<T> start_radial = from_apex;
		start_radial.mad(start_radial, axis, -start_axis);

		const T k = m_radius / m_height;

		//Lateral surface: |start_radial + t*dir_radial|^2 == k^2 * (start_axis + t*dir_axis)^2
		const T qua = dir_radial.square_magnitude() - k * k * dir_axis * dir_axis;
		const T qub = dir_radial.dotproduct(start_radial) - k * k * dir_axis * start_axis;
		const T quc = start_radial.square_magnitude() - k * k * start_axis * start_axis;

		if (std::abs(qua) > T(1e-12))
		{
			const T discr = qub * qub - qua * quc;

			if (discr >= T(0))
			{
				const T root = _sqrt(discr);
				const T inv = T(1) / qua;

				const T roots[2] = { (-qub - root) * inv, (-qub + root) * inv };

				for (int i = 0; i < 2; ++i)
				{
					const T t = roots[i];
					if (t < T(0) || t > T(1))
						continue;

					const T axial = start_axis + t * dir_axis;
					if (axial >= T(0) && axial <= m_height && count < 3)
						out_t[count++] = t;
				}
			}
		}
		else if (std::abs(qub) > T(1e-12))
		{
			const T t = -quc / (T(2) * qub);

			if (t >= T(0) && t <= T(1))
			{
				const T axial = start_axis + t * dir_axis;
				if (axial >= T(0) && axial <= m_height && count < 3)
					out_t[count++] = t;
			}
		}

		//Base disk.
		if (std::abs(dir_axis) > T(1e-12))
		{
			const T t = (m_height - start_axis) / dir_axis;

			if (t >= T(0) && t <= T(1))
			{
				_vector3<T> point;
				point.mad(start, dir, t);

				_vector3<T> radial; radial.sub(point, m_translate);
				radial.mad(radial, axis, -m_height);

				if (radial.square_magnitude() <= m_radius * m_radius && count < 3)
					out_t[count++] = t;
			}
		}

		return count;
	}

	ICF bool	segment_intersects_cone	(const _vector3<T>& start, const _vector3<T>& end) const
	{
		if (IsPointInCone(start) || IsPointInCone(end))
			return true;

		T hits[3];
		return segment_cone_intersections(start, end, hits) > 0;
	}
};

typedef _cone<float>	Fcone;
typedef _cone<double>	Dcone;

template <class T>
bool	_valid			(const _cone<T>& c)	{ return _valid(c.m_translate) && _valid(c.m_direction) && _valid(c.m_height) && _valid(c.m_radius);	}

#endif // _CONE_H
