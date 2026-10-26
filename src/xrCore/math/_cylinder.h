#ifndef _CYLINDER_H
#define _CYLINDER_H

template <XRay::Concepts::Arithmetic T>
class _cylinder
{
public:
	typedef T			TYPE;
	typedef _cylinder<T>Self;
	typedef Self&		SelfRef;
	typedef const Self&	SelfCRef;

public:
	_vector3<T>	m_center;
	_vector3<T>	m_direction;
	T			m_height = 0;
	T			m_radius = 0;


public:
    ICF bool operator==(SelfCRef Left)
    {
        return Left.m_center == m_center && Left.m_direction == m_direction && m_height == Left.m_height && m_radius == Left.m_radius;
    }

public:
	ICF SelfRef	invalidate	()	{ m_center.set(0,0,0); m_direction.set(0,0,0); m_height=0; m_radius=0; return *this; }
	enum ecode { cyl_cap, cyl_wall, cyl_none };
    ICF int		intersect	(const _vector3<T>& start, const _vector3<T>& dir, T afT[2], ecode code[2] ) const
    {
        T fEpsilon = 1e-12f;

        // set up quadratic Q(t) = a*t^2 + 2*b*t + c
        _vector3<T> kU, kV, kW = m_direction;
        _vector3<T>::generate_orthonormal_basis(kW,kU,kV);
        _vector3<T> kD; kD.set(kU.dotproduct(dir),kV.dotproduct(dir),kW.dotproduct(dir));

        T fDLength = kD.normalize_magn();
        T fInvDLength = 1.0f/fDLength;
        _vector3<T> kDiff; kDiff.sub(start,m_center);
        _vector3<T> kP; kP.set(kU.dotproduct(kDiff),kV.dotproduct(kDiff),kW.dotproduct(kDiff));
        T fHalfHeight = 0.5f*m_height;
        T fRadiusSqr = m_radius*m_radius;

        T fInv, fA, fB, fC, fDiscr, fRoot, fT, fT0, fT1, fTmp0, fTmp1;

        if (std::abs(kD.z) >= 1.0f - fEpsilon ){
            // line is parallel to cylinder axis
            if ( kP.x*kP.x+kP.y*kP.y <= fRadiusSqr ){
                fTmp0 = fInvDLength/kD.z;
                afT[0] = (+fHalfHeight - kP.z)*fTmp0;
                afT[1] = (-fHalfHeight - kP.z)*fTmp0;
				code[0] = cyl_cap;
				code[1] = cyl_cap;
                return 2;
            }
            else{
                return 0;
            }
        }

        if (std::abs(kD.z) <= fEpsilon ){
            // line is perpendicular to axis of cylinder
            if (std::abs(kP.z) > fHalfHeight ){
                // line is outside the planar caps of cylinder
                return 0;
            }

            fA = kD.x*kD.x + kD.y*kD.y;
            fB = kP.x*kD.x + kP.y*kD.y;
            fC = kP.x*kP.x + kP.y*kP.y - fRadiusSqr;
            fDiscr = fB*fB - fA*fC;
            if ( fDiscr < 0.0f ){
                // line does not intersect cylinder wall
                return 0;
            }
            else if ( fDiscr > 0.0f ){
                fRoot = _sqrt(fDiscr);
                fTmp0 = fInvDLength/fA;
                afT[0] = (-fB - fRoot)*fTmp0;
                afT[1] = (-fB + fRoot)*fTmp0;
				code[0] = cyl_wall;
				code[1] = cyl_wall;
                return 2;	//wall
            }else{
                afT[0] = -fB*fInvDLength/fA;
				code[0] = cyl_wall;
                return 1; //wall
            }
        }

        // test plane intersections first
        int iQuantity = 0;
        fInv = 1.0f/kD.z;
        fT0 = (+fHalfHeight - kP.z)*fInv;
        fTmp0 = kP.x + fT0*kD.x;
        fTmp1 = kP.y + fT0*kD.y;
        if ( fTmp0*fTmp0 + fTmp1*fTmp1 <= fRadiusSqr )
		{
            code[iQuantity]	= cyl_cap;
			afT[iQuantity++]= fT0*fInvDLength;
			
		}

        fT1 = (-fHalfHeight - kP.z)*fInv;
        fTmp0 = kP.x + fT1*kD.x;
        fTmp1 = kP.y + fT1*kD.y;
        if ( fTmp0*fTmp0 + fTmp1*fTmp1 <= fRadiusSqr )
		{
            code[iQuantity]	= cyl_cap;
			afT[iQuantity++] = fT1*fInvDLength;
		}

        if ( iQuantity == 2 ){
            // line intersects both top and bottom
            return 2;//both caps
        }

        // If iQuantity == 1, then line must intersect cylinder wall
        // somewhere between caps in a single point.  This case is detected
        // in the following code that tests for intersection between line and
        // cylinder wall.

        fA = kD.x*kD.x + kD.y*kD.y;
        fB = kP.x*kD.x + kP.y*kD.y;
        fC = kP.x*kP.x + kP.y*kP.y - fRadiusSqr;
        fDiscr = fB*fB - fA*fC;
        if ( fDiscr < 0.0f ){
            // line does not intersect cylinder wall
            //VERIFY( iQuantity == 0 );
            return 0;
        }else if ( fDiscr > 0.0f ){
            fRoot = _sqrt(fDiscr);
            fInv = 1.0f/fA;
            fT = (-fB - fRoot)*fInv;
            if ( fT0 <= fT1 ){
                if ( fT0 <= fT && fT <= fT1 )
				{
					code[iQuantity]	= cyl_wall;                    
					afT[iQuantity++] = fT*fInvDLength;

				}
            }else{
                if ( fT1 <= fT && fT <= fT0 )
				{
					code[iQuantity]	= cyl_wall;
					afT[iQuantity++] = fT*fInvDLength;

				}
            }

            if ( iQuantity == 2 ){
                // Line intersects one of top/bottom of cylinder and once on
                // cylinder wall.
                return 2;
            }

            fT = (-fB + fRoot)*fInv;
            if ( fT0 <= fT1 ){
                if ( fT0 <= fT && fT <= fT1 )
				{
                    code[iQuantity]	= cyl_wall;
					afT[iQuantity++] = fT*fInvDLength;
				}
            }else{
                if ( fT1 <= fT && fT <= fT0 )
				{
                    code[iQuantity]	= cyl_wall;
					afT[iQuantity++] = fT*fInvDLength;
				}
            }
        }else{
            fT = -fB/fA;
            if ( fT0 <= fT1 ){
                if ( fT0 <= fT && fT <= fT1 )
				{
                    code[iQuantity]	= cyl_wall;
					afT[iQuantity++] = fT*fInvDLength;
				}
            }else{
                if ( fT1 <= fT && fT <= fT0 )
				{
					code[iQuantity]	= cyl_wall;
                    afT[iQuantity++] = fT*fInvDLength;
				}
            }
        }

        return iQuantity;
    }
	enum ERP_Result{
		rpNone			= 0,
		rpOriginInside	= 1,
		rpOriginOutside	= 2,
		fcv_forcedword	= u32(-1)
	};
    ICF ERP_Result	intersect	(const _vector3<T>& start, const _vector3<T>& dir, T& dist) const
    {
    	T				afT[2];
		ecode 			code[2];
        int cnt;
		if (0!=(cnt=intersect(start,dir,afT,code))){
			bool		o_inside	= false;
			bool		b_result	= false;
			for (int k=0; k<cnt; k++){
				if (afT[k]<0.f)		{if(cnt==2)o_inside=true;	continue;	}
				if (afT[k]<dist)	{dist=afT[k];		b_result=true;				}
			}
			return		b_result?(o_inside?rpOriginInside:rpOriginOutside):rpNone;
		}else{
			return		rpNone;
		}
    }
//----------------------------------------------------------------------------
	//The axis is m_direction (unit length), the cylinder is centered at m_center with
	//the full height m_height and the radius m_radius.

	//A cheap distance reject first, then the cylinder is tested as its bounding OBB:
	//the OBB encloses the cylinder, so a reported separation is a real one.
	ICF bool	intersectAABB	(const _vector3<T>& aabbCenter, const _vector3<T>& aabbExtents) const
	{
		const T maxCylSize = std::max(m_radius, T(0.5) * m_height);
		const T maxBoxSize = aabbExtents.magnitude();

		_vector3<T> diff; diff.sub(aabbCenter, m_center);

		if (diff.magnitude() > maxCylSize + maxBoxSize + T(1))
			return false;

		_vector3<T> kW, kU, kV;
		local_basis(kW, kU, kV);

		_obb<T> obb;
		obb.m_translate.set(m_center);
		obb.m_halfsize.set(m_radius, m_radius, T(0.5) * m_height);

		obb.m_rotate.i = kU;
		obb.m_rotate.j = kV;
		obb.m_rotate.k = kW;

		return obb.intersectAABB(aabbCenter, aabbExtents);
	}
	
	ICF bool intersectAABB(const _box3<T>& aabb) const
    {
    	_vector3<T> aabbCenter, aabbExtents;
    	aabb.get_CD(aabbCenter, aabbExtents);
    	return intersectAABB(aabbCenter, aabbExtents);
    }

	//Thirteen separating axes, the same scheme as _obb::intersectTri: the cylinder
	//basis, the triangle normal and the cross products of the basis axes with the
	//triangle edges. bClass3 enables the cross product axes.
	ICF bool	intersectTri	(const _vector3<T>* verts, bool bClass3 = true) const
	{
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

			const T cylProj = m_center.dotproduct(axes[i]);
			T cylHalf = T(0);

			if (i < 3)
			{
				cylHalf = (i == 2) ? T(0.5) * m_height : m_radius;
			}
			else
			{
				const T dotU = axes[i].dotproduct(kU);
				const T dotV = axes[i].dotproduct(kV);
				const T dotW = axes[i].dotproduct(kW);

				cylHalf = m_radius * _sqrt(dotU * dotU + dotV * dotV) + T(0.5) * m_height * std::abs(dotW);
			}

			const T cylMin = cylProj - cylHalf;
			const T cylMax = cylProj + cylHalf;

			const T epsilon = T(0.0001);
			if (maxTri < cylMin - epsilon || minTri > cylMax + epsilon)
				return false;
		}

		return true;
	}

	//Contact points of the triangle with the cylinder: the triangle vertices inside,
	//the edge intersections, the cap crossings, the side generatrices and, when
	//nothing was found, the cylinder axis crossing the triangle.
	ICF void	FindContactsClipping	(_vector3<T>* tri, void (*callback)(const _vector3<T>& contact, void* user_data), void* user_data = nullptr)
	{
		_vector3<T> kW = m_direction;
		_vector3<T> kU, kV;
		_vector3<T>::generate_orthonormal_basis(kW, kU, kV);

		const T fHalfHeight = T(0.5) * m_height;
		_vector3<T> topCenter, bottomCenter;
		topCenter.mad(m_center, kW, fHalfHeight);
		bottomCenter.mad(m_center, kW, -fHalfHeight);

		_vector3<T> contacts[8];
		int numContacts = 0;
		const T contactEpsilon = T(0.001);
		const T fRadiusSqr = m_radius * m_radius;

		bool bContactFound = false;

		for (u8 i = 0; i < 3; i++)
		{
			if (IsPointInCylinder(tri[i], kW, kU, kV, fHalfHeight, fRadiusSqr))
			{
				if (AddContactWithDeduplication(contacts, numContacts, tri[i], callback, user_data, contactEpsilon))
					bContactFound = true;
			}
		}

		for (u8 i = 0; i < 3; i++)
		{
			CheckTriangleEdgeIntersection(tri[i], tri[(i + 1) % 3], kW, kU, kV, fHalfHeight, fRadiusSqr,
				contacts, numContacts, bContactFound, callback, user_data, contactEpsilon);
		}

		CheckCapIntersection(topCenter, kW, kU, kV, tri, fRadiusSqr, contacts, numContacts, bContactFound, callback, user_data, contactEpsilon);
		CheckCapIntersection(bottomCenter, kW, kU, kV, tri, fRadiusSqr, contacts, numContacts, bContactFound, callback, user_data, contactEpsilon);

		{
			const int numEdges = 8;

			for (int i = 0; i < numEdges; i++)
			{
				const T angle = (T(i) * T(PI_MUL_2)) / T(numEdges);
				const T x = std::cos(angle) * m_radius;
				const T y = std::sin(angle) * m_radius;

				_vector3<T> edgeStart, edgeEnd;
				edgeStart.mad(topCenter, kU, x);
				edgeStart.mad(edgeStart, kV, y);
				edgeEnd.mad(bottomCenter, kU, x);
				edgeEnd.mad(edgeEnd, kV, y);

				_vector3<T> edgeDir;
				edgeDir.sub(edgeEnd, edgeStart);
				const T edgeLength = edgeDir.normalize_magn();

				_vector3<T> edge0; edge0.sub(tri[1], tri[0]);
				_vector3<T> edge1; edge1.sub(tri[2], tri[0]);
				_vector3<T> pvec; pvec.crossproduct(edgeDir, edge1);
				const T det = edge0.dotproduct(pvec);

				if (std::abs(det) < T(0.001))
					continue;

				const T invDet = T(1) / det;

				_vector3<T> tvec; tvec.sub(edgeStart, tri[0]);
				const T u = tvec.dotproduct(pvec) * invDet;

				if (u < T(0) || u > T(1))
					continue;

				_vector3<T> qvec; qvec.crossproduct(tvec, edge0);
				const T v = edgeDir.dotproduct(qvec) * invDet;

				if (v < T(0) || u + v > T(1))
					continue;

				const T t = edge1.dotproduct(qvec) * invDet;

				if (t >= T(0) && t <= edgeLength)
				{
					_vector3<T> contact;
					contact.mad(edgeStart, edgeDir, t);

					if (AddContactWithDeduplication(contacts, numContacts, contact, callback, user_data, contactEpsilon))
						bContactFound = true;
				}
			}
		}

		if (!bContactFound)
		{
			_vector3<T> axisStart = bottomCenter;
			_vector3<T> axisDir = kW;
			const T axisLength = m_height;
			_vector3<T> edge1; edge1.sub(tri[1], tri[0]);
			_vector3<T> edge2; edge2.sub(tri[2], tri[0]);
			_vector3<T> pvec; pvec.crossproduct(axisDir, edge2);
			const T det = edge1.dotproduct(pvec);

			if (std::abs(det) > T(0.001))
			{
				const T invDet = T(1) / det;

				_vector3<T> tvec; tvec.sub(axisStart, tri[0]);
				const T u = tvec.dotproduct(pvec) * invDet;

				if (u >= T(0) && u <= T(1))
				{
					_vector3<T> qvec; qvec.crossproduct(tvec, edge1);
					const T v = axisDir.dotproduct(qvec) * invDet;

					if (v >= T(0) && u + v <= T(1))
					{
						const T t = edge2.dotproduct(qvec) * invDet;

						if (t >= T(0) && t <= axisLength)
						{
							_vector3<T> axisContact;
							axisContact.mad(axisStart, axisDir, t);

							_vector3<T> toContact; toContact.sub(axisContact, m_center);
							T z = toContact.dotproduct(kW);
							z = std::min(fHalfHeight, std::max(-fHalfHeight, z));

							T projU = toContact.dotproduct(kU);
							T projV = toContact.dotproduct(kV);
							const T dist2D = _sqrt(projU * projU + projV * projV);

							if (dist2D > T(0.001))
							{
								projU /= dist2D;
								projV /= dist2D;
							}
							else
							{
								projU = T(1);
								projV = T(0);
							}

							_vector3<T> surfaceContact;
							surfaceContact.mad(m_center, kU, projU * m_radius);
							surfaceContact.mad(surfaceContact, kV, projV * m_radius);
							surfaceContact.mad(surfaceContact, kW, z);

							AddContactWithDeduplication(contacts, numContacts, surfaceContact, callback, user_data, contactEpsilon);
						}
					}
				}
			}
		}
	}

	ICF bool	IsPointInCylinder	(const _vector3<T>& point, const _vector3<T>& kW, const _vector3<T>& kU, const _vector3<T>& kV, T fHalfHeight, T fRadiusSqr) const
	{
		_vector3<T> local;
		local.sub(point, m_center);

		const T z = local.dotproduct(kW);
		const T x = local.dotproduct(kU);
		const T y = local.dotproduct(kV);

		return (x * x + y * y <= fRadiusSqr && std::abs(z) <= fHalfHeight);
	}
//----------------------------------------------------------------------------
private:
	ICF void	local_basis	(_vector3<T>& kW, _vector3<T>& kU, _vector3<T>& kV) const
	{
		kW = m_direction;
		if (kW.square_magnitude() > T(1e-12))
			kW.normalize();
		else
			kW.set(T(0), T(1), T(0));

		_vector3<T>::generate_orthonormal_basis(kW, kU, kV);
	}

	ICF bool	AddContactWithDeduplication	(_vector3<T>* contacts, int& numContacts, const _vector3<T>& point,
		void (*callback)(const _vector3<T>&, void*), void* user_data, T contactEpsilon) const
	{
		for (int j = 0; j < numContacts; j++)
		{
			if (contacts[j].similar(point, contactEpsilon))
				return false;
		}

		if (numContacts < 8)
		{
			contacts[numContacts++] = point;
			callback(point, user_data);
			return true;
		}

		return false;
	}

	ICF void	CheckCapIntersection	(const _vector3<T>& capCenter, const _vector3<T>& kW, const _vector3<T>& kU, const _vector3<T>& kV,
		_vector3<T>* tri, T fRadiusSqr, _vector3<T>* contacts, int& numContacts, bool& bContactFound,
		void (*callback)(const _vector3<T>&, void*), void* user_data, T contactEpsilon) const
	{
		const T planeD = -capCenter.dotproduct(kW);

		_vector3<T> intersectionPoints[3];
		int numPoints = 0;

		for (u8 i = 0; i < 3; i++)
		{
			const _vector3<T>& p1 = tri[i];
			const _vector3<T>& p2 = tri[(i + 1) % 3];

			const T d1 = p1.dotproduct(kW) + planeD;
			const T d2 = p2.dotproduct(kW) + planeD;

			if (d1 * d2 < T(0))
			{
				const T t = d1 / (d1 - d2);

				_vector3<T> intersectPt;
				intersectPt.lerp(p1, p2, t);
				intersectionPoints[numPoints++] = intersectPt;
			}
			else if (std::abs(d1) < T(0.001))
			{
				intersectionPoints[numPoints++] = p1;
			}
		}

		for (int i = 0; i < numPoints; i++)
		{
			_vector3<T> toPoint;
			toPoint.sub(intersectionPoints[i], capCenter);

			const T projU = toPoint.dotproduct(kU);
			const T projV = toPoint.dotproduct(kV);

			if (projU * projU + projV * projV <= fRadiusSqr)
			{
				if (AddContactWithDeduplication(contacts, numContacts, intersectionPoints[i], callback, user_data, contactEpsilon))
					bContactFound = true;
			}
		}
	}

	ICF void	CheckTriangleEdgeIntersection	(const _vector3<T>& start, const _vector3<T>& end,
		const _vector3<T>& kW, const _vector3<T>& kU, const _vector3<T>& kV,
		T fHalfHeight, T fRadiusSqr, _vector3<T>* contacts, int& numContacts, bool& bContactFound,
		void (*callback)(const _vector3<T>&, void*), void* user_data, T contactEpsilon) const
	{
		_vector3<T> edge; edge.sub(end, start);

		if (edge.square_magnitude() <= T(1e-12))
			return;

		const T edgeLength = edge.normalize_magn();

		T afT[2];
		ecode code[2];
		const int count = intersect(start, edge, afT, code);

		for (int j = 0; j < count; j++)
		{
			if (afT[j] >= T(-0.001) && afT[j] <= edgeLength + T(0.001))
			{
				_vector3<T> contact;
				contact.mad(start, edge, afT[j]);

				if (IsPointInCylinder(contact, kW, kU, kV, fHalfHeight, fRadiusSqr))
				{
					if (AddContactWithDeduplication(contacts, numContacts, contact, callback, user_data, contactEpsilon))
						bContactFound = true;
				}
			}
		}
	}
//----------------------------------------------------------------------------
};

typedef _cylinder<float>	Fcylinder;
typedef _cylinder<double>	Dcylinder;

template <class T>
bool	_valid			(const _cylinder<T>& c)	{ return _valid(c.m_center) && _valid(c.m_direction) && _valid(c.m_height) && _valid(c.m_radius);	}

#endif // _DEBUG
