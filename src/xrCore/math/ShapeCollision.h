#ifndef SHAPE_COLLISION_H
#define SHAPE_COLLISION_H

//Narrow phase for the convex primitives of math/: a box, an OBB, a sphere, a cylinder
//and a cone. It is built the way a physics engine builds it:
//
//  * no shape is ever split into polygons, every shape provides its exact support
//    function (see ShapeCollisionShapes.h),
//  * GJK answers the overlap question and gives the closest points of the two shapes,
//  * the penetration depth is the smallest support plane distance of the Minkowski
//    difference, refined by a local search over the direction,
//  * a pair of boxes additionally gets the separating axis test and a clipped face
//    manifold of up to eight points, which is what a solver needs for a resting
//    contact.
//
//The algorithm does not know the shapes: it only needs shape_support<Shape> and, for
//the clipped manifold, shape_box<Shape>.
//
//	ShapeCollision::Manifold<f32> manifold;
//	if (ShapeCollision::ComputeManifold(box, cylinder, manifold))
//	{
//		// manifold.Normal points from the box to the cylinder
//		// manifold.Points[0..manifold.Count) are the contact points
//	}
namespace ShapeCollision
{
	enum class EOverlap : u8
	{
		None,
		Touch,
		Penetrate
	};

	//One contact point, the depth is measured along the manifold normal.
	template <class T>
	struct Contact
	{
		_vector3<T>	Position;
		T		Depth;
	};

	template <class T>
	struct Manifold
	{
		static constexpr u32 MaxPoints = 8;

		_vector3<T>	Normal;		//from the first shape to the second one
		T		Depth;		//the deepest point of the manifold
		Contact<T>	Points[MaxPoints];
		u32		Count;
	};

	//A shape has to provide Support(shape, direction) and Bounds(shape, center, radius).
	//See ShapeCollisionShapes.h.
	template <class Shape>
	struct shape_support;

	//Box like shapes (a box and an OBB) have three axes and three half extents, so their
	//pair can be resolved with the separating axis test and a face clip.
	template <class Shape>
	struct shape_box
	{
		static constexpr bool Value = false;
	};

	//Spheres meet each other analytically, the pair is the only one without a corner.
	template <class Shape>
	struct shape_sphere
	{
		static constexpr bool Value = false;
	};

	//Signed distance from a point to the shape (negative inside) and the outward normal
	//of the closest surface point. A sphere pair and a sphere against any other shape are
	//resolved with it, so they are exact.
	template <class Shape>
	struct shape_distance;

	//The surface points of the shape that can touch the other body when it lies in the
	//given direction. They turn a single EPA point into a contact patch, so a cylinder
	//lying on the floor rests on a line instead of one rocking point.
	template <class Shape>
	struct shape_points;

	namespace Detail
	{
		template <class T>
		struct SimplexVertex
		{
			_vector3<T>	Point;		//PointA - PointB
			_vector3<T>	PointA;
			_vector3<T>	PointB;
		};

		template <class T>
		struct Simplex
		{
			SimplexVertex<T>	Vertices[4];
			u32			Count;

			void Set(const SimplexVertex<T>& vertex)
			{
				Vertices[0] = vertex;
				Count = 1;
			}

			void Push(const SimplexVertex<T>& vertex)
			{
				Vertices[Count++] = vertex;
			}
		};

		//The support point of the Minkowski difference A - B along the direction.
		template <class T, class A, class B>
		ICF void Support(const A& a, const B& b, const _vector3<T>& direction, SimplexVertex<T>& vertex)
		{
			_vector3<T> opposite;
			opposite.invert(direction);

			vertex.PointA = shape_support<A>::Support(a, direction);
			vertex.PointB = shape_support<B>::Support(b, opposite);
			vertex.Point.sub(vertex.PointA, vertex.PointB);
		}

		//Closest point on the triangle [a, b, c] to the origin and its barycentric
		//weights. Returns the squared distance.
		template <class T>
		ICF T ClosestTriangle(const _vector3<T>& a, const _vector3<T>& b, const _vector3<T>& c,
			_vector3<T>& closest, T weights[3])
		{
			using V = _vector3<T>;

			V ab;
			ab.sub(b, a);

			V ac;
			ac.sub(c, a);

			const T d1 = -ab.dotproduct(a);
			const T d2 = -ac.dotproduct(a);

			if (d1 <= T(0) && d2 <= T(0))
			{
				closest = a;
				weights[0] = T(1); weights[1] = T(0); weights[2] = T(0);
				return closest.square_magnitude();
			}

			const T d3 = -ab.dotproduct(b);
			const T d4 = -ac.dotproduct(b);

			if (d3 >= T(0) && d4 <= d3)
			{
				closest = b;
				weights[0] = T(0); weights[1] = T(1); weights[2] = T(0);
				return closest.square_magnitude();
			}

			const T vc = d1 * d4 - d3 * d2;

			if (vc <= T(0) && d1 >= T(0) && d3 <= T(0))
			{
				const T v = d1 / (d1 - d3);

				closest.mad(a, ab, v);
				weights[0] = T(1) - v; weights[1] = v; weights[2] = T(0);
				return closest.square_magnitude();
			}

			const T d5 = -ab.dotproduct(c);
			const T d6 = -ac.dotproduct(c);

			if (d6 >= T(0) && d5 <= d6)
			{
				closest = c;
				weights[0] = T(0); weights[1] = T(0); weights[2] = T(1);
				return closest.square_magnitude();
			}

			const T vb = d5 * d2 - d1 * d6;

			if (vb <= T(0) && d2 >= T(0) && d6 <= T(0))
			{
				const T w = d2 / (d2 - d6);

				closest.mad(a, ac, w);
				weights[0] = T(1) - w; weights[1] = T(0); weights[2] = w;
				return closest.square_magnitude();
			}

			V bc;
			bc.sub(c, b);

			const T va = d3 * d6 - d5 * d4;

			if (va <= T(0) && (d4 - d3) >= T(0) && (d5 - d6) >= T(0))
			{
				const T w = (d4 - d3) / ((d4 - d3) + (d5 - d6));

				closest.mad(b, bc, w);
				weights[0] = T(0); weights[1] = T(1) - w; weights[2] = w;
				return closest.square_magnitude();
			}

			const T denom = T(1) / (va + vb + vc);
			const T v = vb * denom;
			const T w = vc * denom;

			closest.mad(a, ab, v);
			closest.mad(ac, w);

			weights[0] = T(1) - v - w; weights[1] = v; weights[2] = w;

			return closest.square_magnitude();
		}

		//Closest point on the tetrahedron to the origin. On success inside is false, for a
		//contained origin inside is true.
		template <class T>
		ICF T ClosestTetrahedron(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2, const _vector3<T>& p3,
			_vector3<T>& closest, T weights[4], bool& inside)
		{
			using V = _vector3<T>;

			const V* points[4] = { &p0, &p1, &p2, &p3 };

			static const u32 face[4][4] =
			{
				{ 0, 1, 2, 3 },
				{ 0, 3, 1, 2 },
				{ 0, 2, 3, 1 },
				{ 1, 3, 2, 0 }
			};

			V e1; e1.sub(p1, p0);
			V e2; e2.sub(p2, p0);
			V e3; e3.sub(p3, p0);
			V cross; cross.crossproduct(e2, e3);

			//A relative flatness test: the volume divided by the edges, a sliver tetrahedron
			//has no reliable face normals and is handled as the flat one.
			const T volume = std::abs(e1.dotproduct(cross));
			const T edge_scale = e1.magnitude() * e2.magnitude() * e3.magnitude();

			const bool flat = (edge_scale < T(1e-12)) || (volume < T(1e-4) * edge_scale);

			T best = std::numeric_limits<T>::max();
			bool found = false;

			for (u32 f = 0; f < 4; ++f)
			{
				const V& a = *points[face[f][0]];
				const V& b = *points[face[f][1]];
				const V& c = *points[face[f][2]];
				const V& opposite = *points[face[f][3]];

				if (!flat)
				{
					V edge1; edge1.sub(b, a);
					V edge2; edge2.sub(c, a);
					V normal; normal.crossproduct(edge1, edge2);

					const T area = normal.magnitude();
					const T edges = edge1.magnitude() * edge2.magnitude();

					//A degenerate face can not classify the origin, the other faces decide.
					if (area <= T(1e-9) || area < T(1e-6) * edges)
						continue;

					normal.mul(T(1) / area);

					V to_opposite; to_opposite.sub(opposite, a);

					const T side_opposite = normal.dotproduct(to_opposite);
					const T side_origin = -normal.dotproduct(a);

					if (side_opposite * side_origin >= T(0))
						continue;
				}

				T local[3];
				V candidate;

				const T distance = ClosestTriangle(a, b, c, candidate, local);

				if (!found || distance < best)
				{
					found = true;
					best = distance;
					closest = candidate;

					weights[0] = weights[1] = weights[2] = weights[3] = T(0);
					weights[face[f][0]] = local[0];
					weights[face[f][1]] = local[1];
					weights[face[f][2]] = local[2];
				}
			}

			if (!found)
			{
				inside = true;
				closest.set(T(0), T(0), T(0));
				return T(0);
			}

			inside = false;
			return best;
		}

		//Closest point on the simplex to the origin, the simplex is reduced to the feature
		//that holds the point. Returns the squared distance.
		template <class T>
		ICF T ClosestOnSimplex(Simplex<T>& simplex, _vector3<T>& closest, T weights[4])
		{
			using V = _vector3<T>;

			T local[4] = { T(0), T(0), T(0), T(0) };
			T distance = T(0);

			switch (simplex.Count)
			{
			case 1:
			{
				closest = simplex.Vertices[0].Point;
				local[0] = T(1);
				distance = closest.square_magnitude();
				break;
			}
			case 2:
			{
				V edge;
				edge.sub(simplex.Vertices[1].Point, simplex.Vertices[0].Point);

				const T length_sqr = edge.square_magnitude();
				T t = T(0);

				if (length_sqr > T(1e-12))
					t = clampr(-simplex.Vertices[0].Point.dotproduct(edge) / length_sqr, T(0), T(1));

				closest.mad(simplex.Vertices[0].Point, edge, t);

				local[0] = T(1) - t;
				local[1] = t;
				distance = closest.square_magnitude();
				break;
			}
			case 3:
			{
				distance = ClosestTriangle(simplex.Vertices[0].Point, simplex.Vertices[1].Point, simplex.Vertices[2].Point, closest, local);
				break;
			}
			default:
			{
				bool inside = false;

				distance = ClosestTetrahedron(simplex.Vertices[0].Point, simplex.Vertices[1].Point,
					simplex.Vertices[2].Point, simplex.Vertices[3].Point, closest, local, inside);

				if (inside)
				{
					closest.set(T(0), T(0), T(0));
					distance = T(0);
					local[0] = local[1] = local[2] = local[3] = T(0.25);
				}

				break;
			}
			}

			//Drop the vertices that do not take part in the closest point.
			SimplexVertex<T> kept[4];
			T kept_weights[4];

			u32 count = 0;
			T sum = T(0);

			for (u32 i = 0; i < simplex.Count; ++i)
			{
				if (local[i] <= T(0))
					continue;

				kept[count] = simplex.Vertices[i];
				kept_weights[count] = local[i];
				sum += local[i];
				++count;
			}

			if (count == 0)
			{
				kept[0] = simplex.Vertices[0];
				kept_weights[0] = T(1);
				sum = T(1);
				count = 1;
			}

			if (sum > T(1e-12))
			{
				for (u32 i = 0; i < count; ++i)
					kept_weights[i] /= sum;
			}

			simplex.Count = count;

			for (u32 i = 0; i < count; ++i)
				simplex.Vertices[i] = kept[i];

			for (u32 i = 0; i < 4; ++i)
				weights[i] = (i < count) ? kept_weights[i] : T(0);

			return distance;
		}

		//GJK: the closest points of the two shapes and their distance.
		template <class T, class A, class B>
		ICF EOverlap Gjk(const A& a, const B& b, Simplex<T>& simplex, T tolerance,
			_vector3<T>& point_a, _vector3<T>& point_b, _vector3<T>& normal)
		{
			using V = _vector3<T>;

			V center_a, center_b;
			T radius_a = T(0), radius_b = T(0);

			shape_support<A>::Bounds(a, center_a, radius_a);
			shape_support<B>::Bounds(b, center_b, radius_b);

			const T scale = std::max(T(1), center_a.distance_to(center_b) + radius_a + radius_b);
			const T eps = tolerance * scale;
			const T eps_sqr = eps * eps;

			V direction;
			direction.sub(center_b, center_a);

			if (direction.square_magnitude() <= eps_sqr)
				direction.set(T(1), T(0), T(0));

			SimplexVertex<T> vertex;
			Support(a, b, direction, vertex);
			simplex.Set(vertex);

			V closest = vertex.Point;
			T weights[4] = { T(1), T(0), T(0), T(0) };

			for (u32 iteration = 0; iteration < 64; ++iteration)
			{
				if (closest.square_magnitude() <= eps_sqr)
				{
					normal.sub(center_b, center_a).normalize_safe();
					point_a.set(T(0), T(0), T(0));
					point_b.set(T(0), T(0), T(0));
					return EOverlap::Penetrate;
				}

				V search;
				search.invert(closest);

				Support(a, b, search, vertex);

				//No progress: no support point can bring the difference closer to the origin.
				//The test is relative to the current distance, a huge shape would otherwise
				//stop the iteration on the float noise of its coordinates.
				const T progress = closest.square_magnitude() - vertex.Point.dotproduct(closest);

				if (progress <= eps_sqr + tolerance * std::max(closest.square_magnitude(), T(1e-6)))
					break;

				if (simplex.Count >= 4)
					break;

				simplex.Push(vertex);
				ClosestOnSimplex(simplex, closest, weights);

				if (closest.square_magnitude() <= eps_sqr)
				{
					normal.sub(center_b, center_a).normalize_safe();
					point_a.set(T(0), T(0), T(0));
					point_b.set(T(0), T(0), T(0));
					return EOverlap::Penetrate;
				}
			}

			point_a.set(T(0), T(0), T(0));
			point_b.set(T(0), T(0), T(0));

			for (u32 i = 0; i < simplex.Count; ++i)
			{
				point_a.mad(simplex.Vertices[i].PointA, weights[i]);
				point_b.mad(simplex.Vertices[i].PointB, weights[i]);
			}

			normal.sub(point_b, point_a);

			if (normal.square_magnitude() > eps_sqr)
				normal.normalize();
			else
				normal.sub(center_b, center_a).normalize_safe();

			if (point_a.distance_to(point_b) <= eps)
				return EOverlap::Touch;

			return EOverlap::None;
		}

		//The penetration depth is the smallest support plane distance of the Minkowski
		//difference:
		//
		//	d(normal) = dot(Support(A - B, normal), normal)
		//
		//Every direction gives an upper bound and the minimum over the unit directions is
		//the depth. The minimum is smooth around the solution, so a local search over the
		//direction, starting from the centre direction, converges quickly and never
		//overshoots.
		template <class T, class A, class B>
		ICF T RefinePenetration(const A& a, const B& b, _vector3<T>& normal,
			_vector3<T>& point_a, _vector3<T>& point_b, T tolerance)
		{
			using V = _vector3<T>;

			//The centre direction, the axes and the diagonals seed the search, the best one
			//is refined.
			V seeds[15];
			seeds[0] = normal;
			seeds[1].set(T(1), T(0), T(0));
			seeds[2].set(T(-1), T(0), T(0));
			seeds[3].set(T(0), T(1), T(0));
			seeds[4].set(T(0), T(-1), T(0));
			seeds[5].set(T(0), T(0), T(1));
			seeds[6].set(T(0), T(0), T(-1));

			const T diagonal_seed = T(0.5773502691896258);

			for (u32 i = 0; i < 8; ++i)
			{
				seeds[7 + i].set(
					(i & 1) ? diagonal_seed : -diagonal_seed,
					(i & 2) ? diagonal_seed : -diagonal_seed,
					(i & 4) ? diagonal_seed : -diagonal_seed);
			}

			T best = std::numeric_limits<T>::max();

			for (u32 i = 0; i < 15; ++i)
			{
				if (seeds[i].square_magnitude() < T(1e-12))
					continue;

				seeds[i].normalize_safe();

				SimplexVertex<T> probe;
				Support(a, b, seeds[i], probe);

				const T depth = probe.Point.dotproduct(seeds[i]);

				if (depth < best)
				{
					best = depth;
					normal = seeds[i];
					point_a = probe.PointA;
					point_b = probe.PointB;
				}
			}

			if (best == std::numeric_limits<T>::max())
			{
				normal.set(T(1), T(0), T(0));
				point_a.set(T(0), T(0), T(0));
				point_b.set(T(0), T(0), T(0));
				return T(0);
			}

			V direction = normal;

			if (best <= tolerance)
				return best;

			T angle = T(0.7853981633974483);	//45 degrees

			for (u32 round = 0; round < 10; ++round)
			{
				V u, v;
				V::generate_orthonormal_basis(direction, u, v);

				V sides[4];
				sides[0] = u;
				sides[1].invert(u);
				sides[2] = v;
				sides[3].invert(v);

				const T offset = std::tan(angle);

				bool improved = false;

				for (u32 i = 0; i < 4; ++i)
				{
					for (u32 sign_index = 0; sign_index < 2; ++sign_index)
					{
						V candidate = direction;
						candidate.mad(sides[i], sign_index ? -offset : offset);
						candidate.normalize_safe();

						SimplexVertex<T> probe;
						Support(a, b, candidate, probe);

						const T depth = probe.Point.dotproduct(candidate);

						if (depth < best - tolerance)
						{
							best = depth;
							direction = candidate;
							point_a = probe.PointA;
							point_b = probe.PointB;
							improved = true;
						}
					}
				}

				if (!improved)
					angle *= T(0.5);
			}

			normal = direction;
			return best;
		}

		//The origin is on the inner side of the face (a, b, c) and at least margin away
		//from its plane. With margin == 0 the origin may also lie on the plane.
		template <class T>
		ICF bool FaceHoldsOrigin(const _vector3<T>& a, const _vector3<T>& b, const _vector3<T>& c,
			const _vector3<T>& opposite, T margin)
		{
			using V = _vector3<T>;

			V e1;
			e1.sub(b, a);

			V e2;
			e2.sub(c, a);

			V normal;
			normal.crossproduct(e1, e2);

			const T length = normal.magnitude();

			if (length < T(1e-9))
				return true;

			V to_opposite;
			to_opposite.sub(opposite, a);

			const T side_opposite = normal.dotproduct(to_opposite) / length;
			const T side_origin = -normal.dotproduct(a) / length;

			return side_opposite * side_origin >= T(0) && std::abs(side_origin) >= margin;
		}

		template <class T>
		ICF bool TetrahedronContainsOrigin(const _vector3<T>& a, const _vector3<T>& b, const _vector3<T>& c,
			const _vector3<T>& d, T margin)
		{
			return	FaceHoldsOrigin(a, b, c, d, margin) &&
				FaceHoldsOrigin(a, d, b, c, margin) &&
				FaceHoldsOrigin(a, c, d, b, margin) &&
				FaceHoldsOrigin(b, d, c, a, margin);
		}

		//The tetrahedron of the largest volume that holds the origin among the pool points.
		//margin > 0 requires the origin to be strictly inside, a tetrahedron that only
		//touches the origin would give a zero penetration depth.
		template <class T>
		ICF bool FindTetrahedron(const SimplexVertex<T>* pool, u32 count, T margin, u32 best[4])
		{
			using V = _vector3<T>;

			T best_volume = T(0);
			bool found = false;

			for (u32 i = 0; i < count; ++i)
			{
				for (u32 j = i + 1; j < count; ++j)
				{
					for (u32 k = j + 1; k < count; ++k)
					{
						for (u32 l = k + 1; l < count; ++l)
						{
							V e1; e1.sub(pool[j].Point, pool[i].Point);
							V e2; e2.sub(pool[k].Point, pool[i].Point);
							V e3; e3.sub(pool[l].Point, pool[i].Point);

							V cross; cross.crossproduct(e2, e3);

							const T volume = std::abs(e1.dotproduct(cross));

							if (volume <= best_volume)
								continue;

							if (!TetrahedronContainsOrigin(pool[i].Point, pool[j].Point,
								pool[k].Point, pool[l].Point, margin))
								continue;

							best[0] = i;
							best[1] = j;
							best[2] = k;
							best[3] = l;

							best_volume = volume;
							found = true;
						}
					}
				}
			}

			return found;
		}

		template <class T>
		struct EpaFace
		{
			u32		A, B, C;
			_vector3<T>	Normal;
			T		Distance;
		};

		//EPA: the minimum translation vector of an overlap and the point where it is
		//reached. The polytope is grown from a tetrahedron that strictly holds the origin,
		//every step adds the support point of the closest face. Fails when the overlap is
		//only a touch and no tetrahedron with a volume exists.
		template <class T, class A, class B>
		ICF bool Epa(const A& a, const B& b, const Simplex<T>& input, T eps,
			_vector3<T>& normal, T& depth, _vector3<T>& point)
		{
			using V = _vector3<T>;

			constexpr u32 MaxVertices = 96;
			constexpr u32 MaxFaces = 192;

			//The support points around the origin: the GJK simplex, the six axes and, when
			//that is not enough, the eight diagonals.
			SimplexVertex<T> pool[20];
			u32 pool_count = 0;

			for (u32 i = 0; i < input.Count && pool_count < 20; ++i)
				pool[pool_count++] = input.Vertices[i];

			V directions[14];
			directions[0].set(T(1), T(0), T(0));
			directions[1].set(T(-1), T(0), T(0));
			directions[2].set(T(0), T(1), T(0));
			directions[3].set(T(0), T(-1), T(0));
			directions[4].set(T(0), T(0), T(1));
			directions[5].set(T(0), T(0), T(-1));

			const T diagonal = T(0.5773502691896258);

			for (u32 i = 0; i < 8; ++i)
			{
				directions[6 + i].set(
					(i & 1) ? diagonal : -diagonal,
					(i & 2) ? diagonal : -diagonal,
					(i & 4) ? diagonal : -diagonal);
			}

			auto add_supports = [&](u32 from, u32 to)
			{
				for (u32 i = from; i < to && pool_count < 20; ++i)
				{
					SimplexVertex<T> candidate;
					Support(a, b, directions[i], candidate);

					bool duplicate = false;

					for (u32 k = 0; k < pool_count && !duplicate; ++k)
						duplicate = pool[k].Point.similar(candidate.Point, eps);

					if (!duplicate)
						pool[pool_count++] = candidate;
				}
			};

			add_supports(0, 6);

			u32 best[4] = { 0, 1, 2, 3 };

			bool found = FindTetrahedron(pool, pool_count, eps, best);

			if (!found)
			{
				add_supports(6, 14);
				found = FindTetrahedron(pool, pool_count, eps, best);
			}

			if (!found)
				found = FindTetrahedron(pool, pool_count, T(0), best);

			if (!found)
				return false;

			SimplexVertex<T> vertices[MaxVertices];
			u32 vertex_count = 4;

			for (u32 i = 0; i < 4; ++i)
				vertices[i] = pool[best[i]];

			EpaFace<T> faces[MaxFaces];
			u32 face_count = 0;

			static const u32 face_vertices[4][4] =
			{
				{ 0, 1, 2, 3 },
				{ 0, 3, 1, 2 },
				{ 0, 2, 3, 1 },
				{ 1, 3, 2, 0 }
			};

			for (u32 f = 0; f < 4; ++f)
			{
				u32 i = face_vertices[f][0];
				u32 j = face_vertices[f][1];
				u32 k = face_vertices[f][2];
				const u32 opposite = face_vertices[f][3];

				V e1; e1.sub(vertices[j].Point, vertices[i].Point);
				V e2; e2.sub(vertices[k].Point, vertices[i].Point);
				V face_normal; face_normal.crossproduct(e1, e2);

				const T length = face_normal.magnitude();

				if (length < eps)
					continue;

				face_normal.mul(T(1) / length);

				V to_opposite; to_opposite.sub(vertices[opposite].Point, vertices[i].Point);

				if (face_normal.dotproduct(to_opposite) > T(0))
				{
					face_normal.invert();

					const u32 swap = j;
					j = k;
					k = swap;
				}

				faces[face_count].A = i;
				faces[face_count].B = j;
				faces[face_count].C = k;
				faces[face_count].Normal = face_normal;
				faces[face_count].Distance = face_normal.dotproduct(vertices[i].Point);

				//The origin is inside the polytope, a negative distance is a rounding
				//artifact: point the face away from it.
				if (faces[face_count].Distance < T(0))
				{
					faces[face_count].Normal.invert();
					faces[face_count].Distance = -faces[face_count].Distance;

					const u32 swap_index = faces[face_count].B;
					faces[face_count].B = faces[face_count].C;
					faces[face_count].C = swap_index;
				}

				++face_count;
			}

			if (face_count < 4)
				return false;

			for (u32 iteration = 0; iteration < 128; ++iteration)
			{
				u32 closest = 0;

				for (u32 f = 1; f < face_count; ++f)
				{
					if (faces[f].Distance < faces[closest].Distance)
						closest = f;
				}

				SimplexVertex<T> support;
				Support(a, b, faces[closest].Normal, support);

				const T reach = faces[closest].Normal.dotproduct(support.Point);

				if (reach - faces[closest].Distance <= eps)
					break;

				if (vertex_count >= MaxVertices)
					break;

				//A support point that is already a vertex means the face can not grow any
				//further.
				bool duplicate = false;

				for (u32 i = 0; i < vertex_count && !duplicate; ++i)
					duplicate = vertices[i].Point.similar(support.Point, eps);

				if (duplicate)
					break;

				bool removed[MaxFaces] = {};

				for (u32 f = 0; f < face_count; ++f)
					removed[f] = (faces[f].Normal.dotproduct(support.Point) - faces[f].Distance) > eps;

				//The border between the removed and the kept faces.
				u32 border[MaxFaces][2];
				u32 border_count = 0;

				for (u32 f = 0; f < face_count; ++f)
				{
					if (!removed[f])
						continue;

					const u32 index[3] = { faces[f].A, faces[f].B, faces[f].C };

					for (u32 e = 0; e < 3; ++e)
					{
						const u32 from = index[e];
						const u32 to = index[(e + 1) % 3];

						bool shared = false;

						for (u32 g = 0; g < face_count && !shared; ++g)
						{
							if (!removed[g] || g == f)
								continue;

							const u32 other[3] = { faces[g].A, faces[g].B, faces[g].C };

							for (u32 h = 0; h < 3; ++h)
							{
								if (other[h] == to && other[(h + 1) % 3] == from)
								{
									shared = true;
									break;
								}
							}
						}

						if (!shared && border_count < MaxFaces)
						{
							border[border_count][0] = from;
							border[border_count][1] = to;
							++border_count;
						}
					}
				}

				//The new faces are built before the polytope is modified: a degenerate one
				//would leave a hole and the search would never recover from it.
				EpaFace<T> fresh[MaxFaces];
				u32 fresh_count = 0;
				bool degenerate = false;

				for (u32 e = 0; e < border_count; ++e)
				{
					const u32 i = border[e][0];
					const u32 j = border[e][1];

					V e1; e1.sub(vertices[j].Point, vertices[i].Point);
					V e2; e2.sub(support.Point, vertices[i].Point);
					V face_normal; face_normal.crossproduct(e1, e2);

					const T length = face_normal.magnitude();
					const T edges = e1.magnitude() * e2.magnitude();

					if (length < T(1e-6) * edges)
					{
						degenerate = true;
						break;
					}

					face_normal.mul(T(1) / length);

					fresh[fresh_count].A = i;
					fresh[fresh_count].B = j;
					fresh[fresh_count].C = 0;
					fresh[fresh_count].Normal = face_normal;
					fresh[fresh_count].Distance = face_normal.dotproduct(vertices[i].Point);
					++fresh_count;
				}

				u32 kept_count = 0;

				for (u32 f = 0; f < face_count; ++f)
				{
					if (!removed[f])
						++kept_count;
				}

				if (degenerate || fresh_count == 0 || kept_count + fresh_count > MaxFaces)
					break;

				u32 kept = 0;

				for (u32 f = 0; f < face_count; ++f)
				{
					if (!removed[f])
						faces[kept++] = faces[f];
				}

				face_count = kept;

				const u32 new_index = vertex_count;
				vertices[vertex_count++] = support;

				for (u32 e = 0; e < fresh_count && face_count < MaxFaces; ++e)
				{
					fresh[e].C = new_index;
					faces[face_count++] = fresh[e];
				}
			}

			//The face has to be picked from the final polytope: the last step can replace the
			//closest face, and the stale index would report a wrong depth.
			u32 closest = 0;

			for (u32 f = 1; f < face_count; ++f)
			{
				if (faces[f].Distance < faces[closest].Distance)
					closest = f;
			}

			const EpaFace<T>& face = faces[closest];

			normal = face.Normal;
			depth = face.Distance;

			const SimplexVertex<T>& v0 = vertices[face.A];
			const SimplexVertex<T>& v1 = vertices[face.B];
			const SimplexVertex<T>& v2 = vertices[face.C];

			//The projection of the origin onto the face, in the barycentric coordinates of
			//the face triangle.
			V projected;
			projected.mul(normal, depth);

			V e0; e0.sub(v1.Point, v0.Point);
			V e1; e1.sub(v2.Point, v0.Point);
			V d; d.sub(projected, v0.Point);

			const T d00 = e0.dotproduct(e0);
			const T d01 = e0.dotproduct(e1);
			const T d11 = e1.dotproduct(e1);
			const T d20 = d.dotproduct(e0);
			const T d21 = d.dotproduct(e1);

			const T denom = d00 * d11 - d01 * d01;

			T l0 = T(1), l1 = T(0), l2 = T(0);

			if (std::abs(denom) > T(1e-12))
			{
				l1 = (d11 * d20 - d01 * d21) / denom;
				l2 = (d00 * d21 - d01 * d20) / denom;
				l0 = T(1) - l1 - l2;
			}

			l0 = std::max(T(0), l0);
			l1 = std::max(T(0), l1);
			l2 = std::max(T(0), l2);

			const T sum = l0 + l1 + l2;

			if (sum > T(1e-12))
			{
				l0 /= sum;
				l1 /= sum;
				l2 /= sum;
			}
			else
			{
				l0 = T(1);
				l1 = l2 = T(0);
			}

			V witness_a;
			witness_a.set(T(0), T(0), T(0));
			witness_a.mad(v0.PointA, l0);
			witness_a.mad(v1.PointA, l1);
			witness_a.mad(v2.PointA, l2);

			V witness_b;
			witness_b.set(T(0), T(0), T(0));
			witness_b.mad(v0.PointB, l0);
			witness_b.mad(v1.PointB, l1);
			witness_b.mad(v2.PointB, l2);

			point.add(witness_a, witness_b);
			point.mul(T(0.5));

			return true;
		}

		//Adds a contact point without duplicates.
		template <class T>
		ICF void AddManifoldPoint(Manifold<T>& out, const _vector3<T>& point, T depth, T eps)
		{
			//The duplicate check comes first: the same point can be reported with a deeper
			//value from another pass even when the manifold is already full.
			for (u32 i = 0; i < out.Count; ++i)
			{
				if (out.Points[i].Position.similar(point, eps * 4.f))
				{
					if (depth > out.Points[i].Depth)
						out.Points[i].Depth = depth;

					out.Depth = std::max(out.Depth, depth);
					return;
				}
			}

			if (out.Count >= Manifold<T>::MaxPoints)
			{
				//The manifold is full, but the depth still has to reflect the deepest point.
				out.Depth = std::max(out.Depth, depth);
				return;
			}

			out.Points[out.Count].Position = point;
			out.Points[out.Count].Depth = depth;
			out.Depth = std::max(out.Depth, depth);
			++out.Count;
		}

		//The support plane distance along a direction: the highest point of the first shape
		//minus the lowest point of the second one. Positive when the shapes overlap along
		//this axis, and it is exactly the translation of the second shape that makes them
		//touch.
		template <class T, class A, class B>
		ICF T SupportPlane(const A& a, const B& b, const _vector3<T>& direction)
		{
			using V = _vector3<T>;

			V opposite;
			opposite.invert(direction);

			const T high = shape_support<A>::Support(a, direction).dotproduct(direction);
			const T low = shape_support<B>::Support(b, opposite).dotproduct(opposite);

			return high + low;
		}

		//The push direction of a box against another shape and the length of the push. The
		//gradient of the box at the shape centre is not enough: next to a face it points
		//along the face while the shortest way out is diagonal, the two numbers disagree
		//and the shape stays inside the box. A body that is swallowed is driven out along
		//the axis where the overlap is the smallest: that direction always separates the
		//pair by construction.
		template <class T, class Box, class Shape>
		ICF T BestSeparatingAxis(const Box& box, const Shape& shape, _vector3<T>& normal)
		{
			using V = _vector3<T>;

			V box_center, shape_center;
			T box_radius = T(0), shape_radius = T(0);

			shape_support<Box>::Bounds(box, box_center, box_radius);
			shape_support<Shape>::Bounds(shape, shape_center, shape_radius);

			V box_axis[3];
			V box_axes_center;
			T box_half[3];

			shape_box<Box>::Axes(box, box_axes_center, box_axis, box_half);

			T best = T(1e30f);
			normal.set(T(1), T(0), T(0));

			auto test = [&](const V& direction)
			{
				if (direction.square_magnitude() < T(1e-12))
					return;

				V unit = direction;
				unit.normalize_safe();

				const T distance = SupportPlane(box, shape, unit);

				if (distance < best)
				{
					best = distance;
					normal = unit;
				}
			};

			for (u32 i = 0; i < 3; ++i)
			{
				test(box_axis[i]);

				V opposite;
				opposite.invert(box_axis[i]);
				test(opposite);
			}

			//The axes of the other shape: their own three for a box, the single axis for a
			//cylinder or a cone. The cross products with the box axes cover an edge of the
			//box or the side of the shape against a face.
			V shape_axis[3];
			u32 shape_axis_count = 0;

			if constexpr (shape_box<Shape>::Value)
			{
				V shape_axes_center;
				T shape_half[3];

				shape_box<Shape>::Axes(shape, shape_axes_center, shape_axis, shape_half);
				shape_axis_count = 3;
			}
			else if constexpr (requires(const Shape& s) { shape_support<Shape>::Axis(s); })
			{
				shape_axis[0] = shape_support<Shape>::Axis(shape);
				shape_axis_count = 1;
			}

			for (u32 i = 0; i < shape_axis_count; ++i)
			{
				test(shape_axis[i]);

				V opposite;
				opposite.invert(shape_axis[i]);
				test(opposite);

				for (u32 k = 0; k < 3; ++k)
				{
					V cross;
					cross.crossproduct(shape_axis[i], box_axis[k]);
					test(cross);

					cross.invert();
					test(cross);
				}
			}

			//The direction from the closest point of the box to the shape centre: exact for
			//a sphere and a good start for every other shape.
			{
				V gradient;
				shape_distance<Box>::Signed(box, shape_center, gradient);

				test(gradient);

				gradient.invert();
				test(gradient);
			}

			//The direction from the box to the shape, both ways: for a shape that left the
			//box through a corner this is the only candidate close to the answer.
			V line;
			line.sub(shape_center, box_center);
			test(line);
			line.invert();
			test(line);

			if (best <= T(0))
				return best;

			//A short walk downhill: the smallest distance often sits between two candidates.
			for (T step = T(0.35); step > T(0.02); step *= T(0.5))
			{
				for (u32 round = 0; round < 6; ++round)
				{
					V u, v;
					V::generate_orthonormal_basis(normal, u, v);

					V sides[4];
					sides[0] = u;
					sides[1].invert(u);
					sides[2] = v;
					sides[3].invert(v);

					bool improved = false;

					for (u32 i = 0; i < 4; ++i)
					{
						V candidate = normal;
						candidate.mad(sides[i], step);
						candidate.normalize_safe();

						const T distance = SupportPlane(box, shape, candidate);

						if (distance < best - T(1e-5))
						{
							best = distance;
							normal = candidate;
							improved = true;
							break;
						}
					}

					if (!improved)
						break;
				}
			}

			return best;
		}

		//A box like shape against another shape. The overlap decision comes from GJK, the
		//contact points from the exact signed distances of the surface points of both
		//shapes, the push direction from the support plane scan. GJK alone is too coarse
		//for a huge floor box, the scan alone can not tell an overlap from a gap.
		template <class T, class Box, class Shape>
		ICF EOverlap BoxVsShape(const Box& box, const Shape& shape, bool box_is_first, Manifold<T>& out, T tolerance)
		{
			using V = _vector3<T>;

			V shape_center, box_center;
			T shape_radius = T(0), box_radius = T(0);

			shape_support<Shape>::Bounds(shape, shape_center, shape_radius);
			shape_support<Box>::Bounds(box, box_center, box_radius);

			V to_box;
			to_box.sub(box_center, shape_center);

			if (to_box.square_magnitude() < T(1e-12))
				to_box.set(T(0), T(-1), T(0));
			else
				to_box.normalize();

			V to_shape;
			to_shape.invert(to_box);

			V shape_buffer[24];
			V box_buffer[24];

			const u32 shape_count = shape_points<Shape>::Collect(shape, to_box, shape_buffer);
			const u32 box_count = shape_points<Box>::Collect(box, to_shape, box_buffer);

			//The overlap decision: GJK is exact, the surface sampling below still adds the
			//contacts it finds, so either of them is enough.
			V witness_a, witness_b;
			Detail::Simplex<T> simplex;

			const EOverlap overlap = Detail::Gjk(box, shape, simplex, tolerance, witness_a, witness_b, out.Normal);

			//The push direction: the axis of the smallest support plane distance. The
			//gradient of the box at the shape centre points along a face when the shape
			//sits near an edge, and a push along it does not separate the pair.
			V normal = out.Normal;

			if (overlap == EOverlap::Penetrate)
				Detail::BestSeparatingAxis(box, shape, normal);

			if (!box_is_first)
				normal.invert();

			out.Normal = normal;
			out.Depth = T(0);
			out.Count = 0;

			//The surface points of the shape inside the box.
			for (u32 i = 0; i < shape_count; ++i)
			{
				V gradient;
				const T signed_distance = shape_distance<Box>::Signed(box, shape_buffer[i], gradient);

				if (signed_distance <= tolerance)
					AddManifoldPoint(out, shape_buffer[i], std::max(T(0), -signed_distance), tolerance);
			}

			//The surface points of the box inside the shape.
			for (u32 i = 0; i < box_count; ++i)
			{
				V gradient;
				const T signed_distance = shape_distance<Shape>::Signed(shape, box_buffer[i], gradient);

				if (signed_distance <= tolerance)
					AddManifoldPoint(out, box_buffer[i], std::max(T(0), -signed_distance), tolerance);
			}

			//When the centre of the shape is inside the box the depth of every surface
			//point says nothing about the way out: a cylinder as tall as the box touches
			//the top and the bottom at once. The distance from the centre to the nearest
			//face plus the extent of the shape along that direction is used then.
			{
				V gradient;
				const T signed_center = shape_distance<Box>::Signed(box, shape_center, gradient);

				if (signed_center < T(0))
				{
					V inward;
					inward.invert(gradient);

					const V support = shape_support<Shape>::Support(shape, inward);

					V offset;
					offset.sub(support, shape_center);

					const T extent = std::max(T(0), offset.dotproduct(inward));
					const T depth = -signed_center + extent;

					if (depth > out.Depth)
						AddManifoldPoint(out, support, depth, tolerance);
				}
			}

			//GJK sees an overlap the surface sampling missed (the contact is between the
			//samples): its witness points are the contact then.
			if (out.Count == 0)
			{
				if (overlap == EOverlap::None)
					return EOverlap::None;

				V middle;
				middle.add(witness_a, witness_b);
				middle.mul(T(0.5));

				out.Points[0].Position = middle;
				out.Points[0].Depth = T(0);
				out.Count = 1;

				return EOverlap::Touch;
			}

			return (out.Depth > T(0)) ? EOverlap::Penetrate : EOverlap::Touch;
		}


		template <class T, class Shape, class Other>
		ICF void AppendShapeContacts(const Shape& shape, const Other& other, const _vector3<T>& direction,
			T depth, Manifold<T>& out, T eps)
		{
			using V = _vector3<T>;

			V points[24];
			const u32 count = shape_points<Shape>::Collect(shape, direction, points);

			for (u32 i = 0; i < count && out.Count < Manifold<T>::MaxPoints; ++i)
			{
				V gradient;
				const T signed_distance = shape_distance<Other>::Signed(other, points[i], gradient);

				if (signed_distance > eps)
					continue;

				bool duplicate = false;

				for (u32 k = 0; k < out.Count && !duplicate; ++k)
					duplicate = out.Points[k].Position.similar(points[i], eps * 4.f);

				if (duplicate)
					continue;

				out.Points[out.Count].Position = points[i];
				out.Points[out.Count].Depth = std::min(std::max(T(0), -signed_distance), depth);
				++out.Count;
			}
		}
		//Closest points of the two segments [p1, q1] and [p2, q2].
		template <class T>
		ICF void ClosestSegments(const _vector3<T>& p1, const _vector3<T>& q1,
			const _vector3<T>& p2, const _vector3<T>& q2,
			_vector3<T>& c1, _vector3<T>& c2)
		{
			using V = _vector3<T>;

			V d1; d1.sub(q1, p1);
			V d2; d2.sub(q2, p2);
			V r;  r.sub(p1, p2);

			const T a = d1.dotproduct(d1);
			const T e = d2.dotproduct(d2);
			const T f = d2.dotproduct(r);

			T s = T(0);
			T t = T(0);

			if (a <= T(1e-12) && e <= T(1e-12))
			{
				c1 = p1;
				c2 = p2;
				return;
			}

			if (a <= T(1e-12))
			{
				s = T(0);
				t = clampr(f / e, T(0), T(1));
			}
			else
			{
				const T c = d1.dotproduct(r);

				if (e <= T(1e-12))
				{
					t = T(0);
					s = clampr(-c / a, T(0), T(1));
				}
				else
				{
					const T b = d1.dotproduct(d2);
					const T denom = a * e - b * b;

					s = (denom > T(1e-12)) ? clampr((b * f - c * e) / denom, T(0), T(1)) : T(0);

					t = (b * s + f) / e;

					if (t < T(0))
					{
						t = T(0);
						s = clampr(-c / a, T(0), T(1));
					}
					else if (t > T(1))
					{
						t = T(1);
						s = clampr((b - c) / a, T(0), T(1));
					}
				}
			}

			c1.mad(p1, d1, s);
			c2.mad(p2, d2, t);
		}

		template <class T>
		struct BoxShape
		{
			_vector3<T>	Center;
			_vector3<T>	Axis[3];
			T		Half[3];
		};

		//Keeps the part of the convex polygon where dot(point, plane_normal) <= plane_offset.
		template <class T>
		ICF u32 ClipPolygon(const _vector3<T>* input, u32 count, const _vector3<T>& plane_normal,
			T plane_offset, _vector3<T>* output)
		{
			u32 out_count = 0;

			if (count == 0)
				return 0;

			for (u32 i = 0; i < count; ++i)
			{
				const _vector3<T>& a = input[i];
				const _vector3<T>& b = input[(i + 1) % count];

				const T da = a.dotproduct(plane_normal) - plane_offset;
				const T db = b.dotproduct(plane_normal) - plane_offset;

				if (da <= T(0) && out_count < 16)
					output[out_count++] = a;

				if (((da > T(0)) != (db > T(0))) && out_count < 16)
				{
					_vector3<T> edge;
					edge.sub(b, a);

					_vector3<T> point;
					point.mad(a, edge, da / (da - db));

					output[out_count++] = point;
				}
			}

			return out_count;
		}

		//Separating axis test of two boxes: the axis of the smallest overlap, the depth
		//along it and the features it comes from.
		template <class T>
		ICF bool SatBoxes(const BoxShape<T>& a, const BoxShape<T>& b, _vector3<T>& normal, T& depth,
			u32& feature, u32& index_a, u32& index_b)
		{
			using V = _vector3<T>;

			V delta;
			delta.sub(b.Center, a.Center);

			T best = std::numeric_limits<T>::max();
			feature = 0;
			index_a = 0;
			index_b = 0;

			auto test_axis = [&](const V& axis, u32 kind, u32 axis_a, u32 axis_b) -> bool
			{
				const T length = axis.magnitude();

				if (length < T(1e-9))
					return true;

				V direction;
				direction.mul(axis, T(1) / length);

				T radius_a = T(0);
				T radius_b = T(0);

				for (u32 i = 0; i < 3; ++i)
				{
					radius_a += a.Half[i] * std::abs(direction.dotproduct(a.Axis[i]));
					radius_b += b.Half[i] * std::abs(direction.dotproduct(b.Axis[i]));
				}

				const T distance = delta.dotproduct(direction);
				const T overlap = radius_a + radius_b - std::abs(distance);

				if (overlap <= T(0))
					return false;

				if (overlap < best)
				{
					best = overlap;
					normal = direction;

					if (distance < T(0))
						normal.invert();

					feature = kind;
					index_a = axis_a;
					index_b = axis_b;
				}

				return true;
			};

			for (u32 i = 0; i < 3; ++i)
			{
				if (!test_axis(a.Axis[i], 1, i, 0))
					return false;
			}

			for (u32 i = 0; i < 3; ++i)
			{
				if (!test_axis(b.Axis[i], 2, 0, i))
					return false;
			}

			for (u32 i = 0; i < 3; ++i)
			{
				for (u32 j = 0; j < 3; ++j)
				{
					V axis;
					axis.crossproduct(a.Axis[i], b.Axis[j]);

					if (!test_axis(axis, 3, i, j))
						return false;
				}
			}

			if (feature == 0)
				return false;

			depth = best;
			return true;
		}

		//The face clipped manifold of two boxes.
		template <class A, class B, class T>
		ICF bool BoxManifold(const A& a, const B& b, Manifold<T>& out, T eps)
		{
			using V = _vector3<T>;

			BoxShape<T> box_a;
			BoxShape<T> box_b;

			shape_box<A>::Axes(a, box_a.Center, box_a.Axis, box_a.Half);
			shape_box<B>::Axes(b, box_b.Center, box_b.Axis, box_b.Half);

			V normal;
			T depth = T(0);
			u32 feature = 0;
			u32 index_a = 0;
			u32 index_b = 0;

			if (!SatBoxes(box_a, box_b, normal, depth, feature, index_a, index_b))
				return false;

			//An edge against an edge: the SAT depth is exact, the contact points come from the
			//corners of both boxes that lie inside the other one.
			if (feature == 3)
			{
				out.Normal = normal;
				out.Depth = T(0);
				out.Count = 0;

				V points[24];

				const u32 count_a = shape_points<A>::Collect(a, normal, points);

				for (u32 i = 0; i < count_a; ++i)
				{
					V gradient;
					const T signed_distance = shape_distance<B>::Signed(b, points[i], gradient);

					if (signed_distance <= eps)
						AddManifoldPoint(out, points[i], std::min(std::max(T(0), -signed_distance), depth), eps);
				}

				V opposite;
				opposite.invert(normal);

				const u32 count_b = shape_points<B>::Collect(b, opposite, points);

				for (u32 i = 0; i < count_b; ++i)
				{
					V gradient;
					const T signed_distance = shape_distance<A>::Signed(a, points[i], gradient);

					if (signed_distance <= eps)
						AddManifoldPoint(out, points[i], std::min(std::max(T(0), -signed_distance), depth), eps);
				}

				if (out.Count == 0)
				{
					//The two edges that produced the cross axis: their closest points lie on
					//both boxes, the support point alone could be outside the other one.
					const u32 pad_a = (index_a + 1) % 3;
					const u32 pad_a2 = (index_a + 2) % 3;
					const u32 pad_b = (index_b + 1) % 3;
					const u32 pad_b2 = (index_b + 2) % 3;

					V to_b;
					to_b.sub(box_b.Center, box_a.Center);

					V to_a;
					to_a.invert(to_b);

					V offset_a;
					offset_a.set(T(0), T(0), T(0));
					offset_a.mad(box_a.Axis[pad_a], (to_b.dotproduct(box_a.Axis[pad_a]) >= T(0) ? box_a.Half[pad_a] : -box_a.Half[pad_a]));
					offset_a.mad(box_a.Axis[pad_a2], (to_b.dotproduct(box_a.Axis[pad_a2]) >= T(0) ? box_a.Half[pad_a2] : -box_a.Half[pad_a2]));

					V offset_b;
					offset_b.set(T(0), T(0), T(0));
					offset_b.mad(box_b.Axis[pad_b], (to_a.dotproduct(box_b.Axis[pad_b]) >= T(0) ? box_b.Half[pad_b] : -box_b.Half[pad_b]));
					offset_b.mad(box_b.Axis[pad_b2], (to_a.dotproduct(box_b.Axis[pad_b2]) >= T(0) ? box_b.Half[pad_b2] : -box_b.Half[pad_b2]));

					V center_a;
					center_a.add(box_a.Center, offset_a);

					V center_b;
					center_b.add(box_b.Center, offset_b);

					V edge_a[2];
					edge_a[0].mad(center_a, box_a.Axis[index_a], -box_a.Half[index_a]);
					edge_a[1].mad(center_a, box_a.Axis[index_a], box_a.Half[index_a]);

					V edge_b[2];
					edge_b[0].mad(center_b, box_b.Axis[index_b], -box_b.Half[index_b]);
					edge_b[1].mad(center_b, box_b.Axis[index_b], box_b.Half[index_b]);

					V point_a, point_b;
					ClosestSegments(edge_a[0], edge_a[1], edge_b[0], edge_b[1], point_a, point_b);

					V middle;
					middle.add(point_a, point_b);
					middle.mul(T(0.5));

					AddManifoldPoint(out, middle, depth, eps);
				}

				out.Depth = std::max(out.Depth, depth);
				return true;
			}

			//Face against face: clip the incident face of one box by the reference face of
			//the other one.
			const bool reference_is_a = (feature == 1);

			const BoxShape<T>& reference = reference_is_a ? box_a : box_b;
			const BoxShape<T>& incident = reference_is_a ? box_b : box_a;

			const u32 reference_axis = reference_is_a ? index_a : index_b;

			V face_normal = normal;

			if (!reference_is_a)
				face_normal.invert();

			V face_point;
			face_point.mad(reference.Center, face_normal, reference.Half[reference_axis]);

			const u32 side_axis[2] =
			{
				(reference_axis + 1) % 3,
				(reference_axis + 2) % 3
			};

			u32 incident_axis = 0;
			T best_dot = T(-1);

			for (u32 i = 0; i < 3; ++i)
			{
				const T dot = std::abs(face_normal.dotproduct(incident.Axis[i]));

				if (dot > best_dot)
				{
					best_dot = dot;
					incident_axis = i;
				}
			}

			V incident_normal = incident.Axis[incident_axis];

			if (incident_normal.dotproduct(face_normal) > T(0))
				incident_normal.invert();

			const u32 incident_side[2] =
			{
				(incident_axis + 1) % 3,
				(incident_axis + 2) % 3
			};

			V incident_center;
			incident_center.mad(incident.Center, incident_normal, incident.Half[incident_axis]);

			V polygon[16];
			V clipped[16];

			polygon[0] = incident_center;
			polygon[0].mad(incident.Axis[incident_side[0]], incident.Half[incident_side[0]]);
			polygon[0].mad(incident.Axis[incident_side[1]], incident.Half[incident_side[1]]);

			polygon[1] = incident_center;
			polygon[1].mad(incident.Axis[incident_side[0]], -incident.Half[incident_side[0]]);
			polygon[1].mad(incident.Axis[incident_side[1]], incident.Half[incident_side[1]]);

			polygon[2] = incident_center;
			polygon[2].mad(incident.Axis[incident_side[0]], -incident.Half[incident_side[0]]);
			polygon[2].mad(incident.Axis[incident_side[1]], -incident.Half[incident_side[1]]);

			polygon[3] = incident_center;
			polygon[3].mad(incident.Axis[incident_side[0]], incident.Half[incident_side[0]]);
			polygon[3].mad(incident.Axis[incident_side[1]], -incident.Half[incident_side[1]]);

			u32 count = 4;

			for (u32 i = 0; i < 2 && count > 0; ++i)
			{
				const V& side = reference.Axis[side_axis[i]];
				const T half = reference.Half[side_axis[i]];

				const T offset = face_point.dotproduct(side);

				count = ClipPolygon(polygon, count, side, offset + half, clipped);

				for (u32 k = 0; k < count; ++k)
					polygon[k] = clipped[k];

				V opposite;
				opposite.invert(side);

				if (count > 0)
					count = ClipPolygon(polygon, count, opposite, -offset + half, clipped);

				for (u32 k = 0; k < count; ++k)
					polygon[k] = clipped[k];
			}

			if (count == 0)
				return false;

			out.Normal = normal;
			out.Depth = depth;
			out.Count = 0;

			const T face_offset = face_normal.dotproduct(face_point);

			for (u32 i = 0; i < count && out.Count < Manifold<T>::MaxPoints; ++i)
			{
				const T point_depth = face_offset - face_normal.dotproduct(polygon[i]);

				if (point_depth < -eps)
					continue;

				out.Points[out.Count].Position = polygon[i];
				out.Points[out.Count].Depth = std::max(T(0), point_depth);
				++out.Count;
			}

			return out.Count > 0;
		}
	}

	//The overlap test only.
	template <class A, class B>
	ICF bool Intersect(const A& a, const B& b)
	{
		using T = typename shape_support<A>::Scalar;

		//Spheres are analytic, they never go through GJK: the huge static floor makes the
		//GJK tolerance coarse and the float precision of its simplices poor.
		if constexpr (shape_sphere<A>::Value && shape_sphere<B>::Value)
		{
			return shape_sphere<A>::Center(a).distance_to(shape_sphere<B>::Center(b)) <
				shape_sphere<A>::Radius(a) + shape_sphere<B>::Radius(b);
		}
		else if constexpr (shape_sphere<A>::Value)
		{
			_vector3<T> gradient;
			return shape_distance<B>::Signed(b, shape_sphere<A>::Center(a), gradient) < shape_sphere<A>::Radius(a);
		}
		else if constexpr (shape_sphere<B>::Value)
		{
			_vector3<T> gradient;
			return shape_distance<A>::Signed(a, shape_sphere<B>::Center(b), gradient) < shape_sphere<B>::Radius(b);
		}
		else if constexpr ((shape_box<A>::Value || shape_box<B>::Value) && !(shape_box<A>::Value && shape_box<B>::Value))
		{
			//The same analytic path as ComputeManifold, so the two never disagree.
			Manifold<T> manifold;

			if constexpr (shape_box<A>::Value)
				return Detail::BoxVsShape(a, b, true, manifold, T(0.001)) != EOverlap::None;
			else
				return Detail::BoxVsShape(b, a, false, manifold, T(0.001)) != EOverlap::None;
		}
		else if constexpr (shape_box<A>::Value && shape_box<B>::Value)
		{
			Manifold<T> manifold;

			if (Detail::BoxManifold(a, b, manifold, T(0.001)))
				return true;

			//A pair that only touches has no overlap for the separating axis test, GJK
			//still reports the contact.
			return Detail::BoxVsShape(a, b, true, manifold, T(0.001)) != EOverlap::None;
		}
		else
		{
			Detail::Simplex<T> simplex;
			_vector3<T> point_a, point_b, normal;

			return Detail::Gjk(a, b, simplex, T(1e-4), point_a, point_b, normal) != EOverlap::None;
		}
	}

	//The contact manifold: the normal from the first shape to the second one, the
	//penetration depth and the contact points. Returns EOverlap::None when the shapes do
	//not touch, the manifold is filled for every other result.
	template <class A, class B, class T>
	ICF EOverlap ComputeManifold(const A& a, const B& b, Manifold<T>& out, T tolerance = T(1e-4))
	{
		using V = _vector3<T>;

		out.Normal.set(T(0), T(1), T(0));
		out.Depth = T(0);
		out.Count = 0;

		//Two spheres: the closest points sit on the line of centers. The whole pair is
		//analytic, so no GJK tolerance can produce a phantom contact far from the sphere.
		if constexpr (shape_sphere<A>::Value && shape_sphere<B>::Value)
		{
			const V center_a2 = shape_sphere<A>::Center(a);
			const V center_b2 = shape_sphere<B>::Center(b);
			const T radius_a2 = shape_sphere<A>::Radius(a);
			const T radius_b2 = shape_sphere<B>::Radius(b);

			V axis;
			axis.sub(center_b2, center_a2);

			const T distance = axis.magnitude();

			if (distance > tolerance)
				axis.mul(T(1) / distance);
			else
				axis.set(T(1), T(0), T(0));

			const T penetration = radius_a2 + radius_b2 - distance;

			if (penetration <= T(0))
				return EOverlap::None;

			out.Normal = axis;
			out.Depth = penetration;

			V surface_a, surface_b;
			surface_a.mad(center_a2, axis, radius_a2);
			surface_b.mad(center_b2, axis, -radius_b2);

			out.Points[0].Position.add(surface_a, surface_b);
			out.Points[0].Position.mul(T(0.5));
			out.Points[0].Depth = penetration;
			out.Count = 1;

			return EOverlap::Penetrate;
		}

		//A sphere against any other shape: the signed distance from the centre to the
		//surface gives the depth, the normal and the surface point without any iteration.
		if constexpr (shape_sphere<A>::Value != shape_sphere<B>::Value)
		{
			V gradient;
			T signed_distance = T(0);
			T radius = T(0);
			V center;

			if constexpr (shape_sphere<A>::Value)
			{
				center = shape_sphere<A>::Center(a);
				radius = shape_sphere<A>::Radius(a);
				signed_distance = shape_distance<B>::Signed(b, center, gradient);
			}
			else
			{
				center = shape_sphere<B>::Center(b);
				radius = shape_sphere<B>::Radius(b);
				signed_distance = shape_distance<A>::Signed(a, center, gradient);
			}

			const T penetration = radius - signed_distance;

			if (penetration <= T(0))
				return EOverlap::None;

			V axis = gradient;

			if constexpr (shape_sphere<A>::Value)
				axis.invert();

			out.Normal = axis;
			out.Depth = penetration;

			V surface_sphere, surface_other;
			surface_sphere.mad(center, gradient, -radius);
			surface_other.mad(center, gradient, -signed_distance);

			out.Points[0].Position.add(surface_sphere, surface_other);
			out.Points[0].Position.mul(T(0.5));
			out.Points[0].Depth = penetration;
			out.Count = 1;

			return EOverlap::Penetrate;
		}

		//A box like shape against a curved one: the signed distance from the surface points
		//of both shapes is exact, so a contact does not depend on the GJK tolerance, which is
		//far too coarse for a huge floor box. Both shapes contribute their points, so the
		//answer does not depend on the order of the arguments.
		if constexpr ((shape_box<A>::Value || shape_box<B>::Value) && !(shape_box<A>::Value && shape_box<B>::Value))
		{
			if constexpr (shape_box<A>::Value)
				return Detail::BoxVsShape(a, b, true, out, T(0.001));
			else
				return Detail::BoxVsShape(b, a, false, out, T(0.001));
		}

		//A pair of boxes is exact with the separating axis test, GJK is not needed there and
		//its tolerance can disagree with the real overlap.
		if constexpr (shape_box<A>::Value && shape_box<B>::Value)
		{
			//The separating axis test is exact for a pair of boxes, a second opinion from
			//GJK only adds contacts for the pairs that are not overlapping.
			if (Detail::BoxManifold(a, b, out, T(0.001)))
				return EOverlap::Penetrate;

			return EOverlap::None;
		}

		V point_a, point_b, normal;
		Detail::Simplex<T> simplex;

		const EOverlap overlap = Detail::Gjk(a, b, simplex, tolerance, point_a, point_b, normal);

		if (overlap == EOverlap::None)
			return EOverlap::None;

		V center_a, center_b;
		T radius_a = T(0), radius_b = T(0);

		shape_support<A>::Bounds(a, center_a, radius_a);
		shape_support<B>::Bounds(b, center_b, radius_b);

		const T scale = std::max(T(1), center_a.distance_to(center_b) + radius_a + radius_b);
		const T eps = tolerance * scale;

		if (overlap == EOverlap::Touch)
		{
			out.Normal = normal;
			out.Depth = T(0);
			out.Points[0].Position.add(point_a, point_b);
			out.Points[0].Position.mul(T(0.5));
			out.Points[0].Depth = T(0);
			out.Count = 1;

			return EOverlap::Touch;
		}

		if constexpr (shape_box<A>::Value && shape_box<B>::Value)
		{
			if (Detail::BoxManifold(a, b, out, eps))
				return EOverlap::Penetrate;
		}

		//The general case: EPA gives the minimum translation vector of the polytope the
		//support points build around the origin.
		{
			V epa_normal, epa_point;
			T epa_depth = T(0);

			if (Detail::Epa(a, b, simplex, eps, epa_normal, epa_depth, epa_point))
			{
				out.Normal = epa_normal;
				out.Depth = std::max(T(0), epa_depth);
				out.Points[0].Position = epa_point;
				out.Points[0].Depth = out.Depth;
				out.Count = 1;

				//A single point is not enough for a resting body: a cylinder lying on the
				//floor gets its contact line from the rim points.
				Detail::AppendShapeContacts(a, b, epa_normal, out.Depth, out, eps);

				V opposite;
				opposite.invert(epa_normal);
				Detail::AppendShapeContacts(b, a, opposite, out.Depth, out, eps);

				return EOverlap::Penetrate;
			}
		}

		//The polytope could not be built (a very flat overlap): fall back to the direct
		//search of the smallest support plane distance.
		normal.sub(center_b, center_a).normalize_safe();

		V surface_a, surface_b;
		const T depth = Detail::RefinePenetration(a, b, normal, surface_a, surface_b, eps);

		out.Normal = normal;
		out.Depth = std::max(T(0), depth);
		out.Points[0].Position.add(surface_a, surface_b);
		out.Points[0].Position.mul(T(0.5));
		out.Points[0].Depth = out.Depth;
		out.Count = 1;

		return EOverlap::Penetrate;
	}
}

#endif // SHAPE_COLLISION_H