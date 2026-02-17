#ifndef FOBB_H
#define FOBB_H
#include "_plane.h"
template <class T>
struct _obb{
public:
	typedef _obb<T>		Self;
	typedef Self&		SelfRef;
	typedef const Self&	SelfCRef;
	typedef _vector3<T>	Tvector;
	typedef _matrix<T>	Tmatrix;
protected:
    static ICF bool		clip		(T fDenom, T fNumer, T& rfT0, T& rfT1)
    {
        // Return value is 'true' if line segment intersects the current test
        // plane.  Otherwise 'false' is returned in which case the line segment
        // is entirely clipped.

        if ( fDenom > 0.0f ){
            if ( fNumer > fDenom*rfT1 ) return false;
            if ( fNumer > fDenom*rfT0 ) rfT0 = fNumer/fDenom;
            return true;
        }else if ( fDenom < 0.0f ){
            if ( fNumer > fDenom*rfT0 ) return false;
            if ( fNumer > fDenom*rfT1 ) rfT1 = fNumer/fDenom;
            return true;
        }else{
            return fNumer <= 0.0f;
        }
    }
	static ICF bool 	intersect	(const Tvector& start, const Tvector& dir, const Tvector& extent, T& rfT0, T& rfT1)
	{
		T fSaveT0 = rfT0, fSaveT1 = rfT1;

		bool bNotEntirelyClipped =
			clip(+dir.x,-start.x-extent[0],rfT0,rfT1) &&
			clip(-dir.x,+start.x-extent[0],rfT0,rfT1) &&
			clip(+dir.y,-start.y-extent[1],rfT0,rfT1) &&
			clip(-dir.y,+start.y-extent[1],rfT0,rfT1) &&
			clip(+dir.z,-start.z-extent[2],rfT0,rfT1) &&
			clip(-dir.z,+start.z-extent[2],rfT0,rfT1);

		return bNotEntirelyClipped && ( rfT0 != fSaveT0 || rfT1 != fSaveT1 );
	}
public:
	_matrix33<T>	m_rotate;
	Tvector			m_translate;
	Tvector			m_halfsize;

    ICF bool operator==(SelfCRef Left)
	{
		return Left.m_translate == m_translate && Left.m_rotate == m_rotate && Left.m_halfsize == m_halfsize;
	}

    ICF SelfRef		invalidate() {
		m_rotate.identity	();
		m_translate.set		(0,0,0);
		m_halfsize.set		(0,0,0);
		return *this;
	}
    ICF SelfRef		identity() {
		invalidate();
		m_halfsize.set( T(0.5), T(0.5), T(0.5) );
		return *this;
	}
    ICF void			xform_get(Tmatrix& D) const
	{
		D.i.set(m_rotate.i); D._14_ = 0;
		D.j.set(m_rotate.j); D._24_ = 0;
		D.k.set(m_rotate.k); D._34_ = 0;
		D.c.set(m_translate);D._44_ = 1;
	}
    ICF SelfRef		xform_set(const Tmatrix& S)
	{
		m_rotate.i.set	(S.i);
		m_rotate.j.set	(S.j);
		m_rotate.k.set	(S.k);
		m_translate.set	(S.c);
		return *this;
	}
    ICF void			xform_full(Tmatrix& D) const
	{
		Tmatrix		R,S;
		xform_get	(R);
		S.scale		(m_halfsize);
		D.mul_43	(R,S);
	}

	// NOTE: Unoptimized
    ICF SelfRef		transform(SelfCRef src, const Tmatrix& M)
	{
		Tmatrix	srcR,destR;

		src.xform_get	(srcR);
		destR.mul_43	(M,srcR);
		xform_set		(destR);
		m_halfsize.set	(src.m_halfsize);
		return *this;
	}

    ICF bool 		intersect(const Tvector& start, const Tvector& dir, T& dist) const
    {
        // convert ray to box coordinates
        Tvector kDiff; 
        kDiff.sub(start,m_translate);
        Tvector kOrigin;
        kOrigin.set(kDiff.dotproduct(m_rotate.i), kDiff.dotproduct(m_rotate.j), kDiff.dotproduct(m_rotate.k));
        Tvector kDirection;
        kDirection.set(dir.dotproduct(m_rotate.i),dir.dotproduct(m_rotate.j),dir.dotproduct(m_rotate.k));

        T fT0 = 0.0f, fT1 = type_max(T);
        if (intersect(kOrigin,kDirection,m_halfsize, fT0,fT1)){
            bool bPick=false;
            if ( fT0 > 0.0f ){
                if (fT0<dist){ dist = fT0; bPick=true;}
                if (fT1<dist){ dist = fT1; bPick=true;}
            }else{
                if (fT1<dist){ dist = fT1; bPick=true;}
            }
            return bPick;
        }
        return false;
    }

    ICF bool contains(const Tvector& point) const
    {
        Tvector localPoint;
        localPoint.sub(point, m_translate);

        T x = localPoint.dotproduct(m_rotate.i);
        T y = localPoint.dotproduct(m_rotate.j);
        T z = localPoint.dotproduct(m_rotate.k);

        return (x >= -m_halfsize.x) && (x <= m_halfsize.x) &&
            (y >= -m_halfsize.y) && (y <= m_halfsize.y) &&
            (z >= -m_halfsize.z) && (z <= m_halfsize.z);
    }

    ICF bool intersectAABB(const _box3<T>& aabb) const
    {
        Tvector aabbCenter, aabbExtents;
        aabb.get_CD(aabbCenter, aabbExtents);
        return intersectAABB(aabbCenter, aabbExtents);
    }


    ICF bool intersectAABB(const Tvector& aabbCenter, const Tvector& aabbExtents) const
    {
        Tvector diff;
        diff.sub(m_translate, aabbCenter);

        const _matrix33<T>& R = m_rotate;

        T absR[3][3];
        for (int i = 0; i < 3; i++)
        {
            absR[0][i] = std::abs(R.i[i]);
            absR[1][i] = std::abs(R.j[i]);
            absR[2][i] = std::abs(R.k[i]);
        }

        for (int i = 0; i < 3; i++)
        {
            T r = m_halfsize.x * absR[0][i] +
                m_halfsize.y * absR[1][i] +
                m_halfsize.z * absR[2][i];
            if (std::abs(diff[i]) > aabbExtents[i] + r)
                return false;
        }

        for (int i = 0; i < 3; i++)
        {
            T r = aabbExtents.x * absR[i][0] +
                aabbExtents.y * absR[i][1] +
                aabbExtents.z * absR[i][2];

            T projection;
            if (i == 0) projection = std::abs(diff.dotproduct(R.i));
            else if (i == 1) projection = std::abs(diff.dotproduct(R.j));
            else projection = std::abs(diff.dotproduct(R.k));

            if (projection > m_halfsize[i] + r)
                return false;
        }

        return true;
    }

    ICF bool intersectTri(const Tvector* verts, bool bClass3 = true)
    {
        Tvector axes[13];
        u32 total_axes = 3;

        axes[0] = m_rotate.i;
        axes[1] = m_rotate.j;
        axes[2] = m_rotate.k;

        Tvector edge1, edge2;
        edge1.sub(verts[1], verts[0]);
        edge2.sub(verts[2], verts[0]);

        Tvector tri_normal;
        tri_normal.crossproduct(edge1, edge2);

        if (tri_normal.square_magnitude() > 0.0001f) {
            axes[total_axes].normalize(tri_normal);
            total_axes++;
        }

        if (bClass3)
        {
            Tvector edges[3];
            edges[0] = edge1;
            edges[1].sub(verts[2], verts[1]);
            edges[2].sub(verts[0], verts[2]);

            for (u8 i = 0; i < 3; i++)
            {
                for (u8 j = 0; j < 3; j++)
                {
                    Tvector axis;
                    axis.crossproduct(m_rotate.row[j], edges[i]);

                    if (axis.square_magnitude() > 0.0001f && total_axes < 13) {
                        axes[total_axes].normalize(axis);
                        total_axes++;
                    }
                }
            }
        }

        for (u32 i = 0; i < total_axes; i++)
        {
            if (axes[i].square_magnitude() < 0.0001f) continue;

            T p0 = verts[0].dotproduct(axes[i]);
            T p1 = verts[1].dotproduct(axes[i]);
            T p2 = verts[2].dotproduct(axes[i]);

            T minTri = std::min(p0, std::min(p1, p2));
            T maxTri = std::max(p0, std::max(p1, p2));

            T obbProj = m_translate.dotproduct(axes[i]);
            T obbHalf =
                m_halfsize.x * std::abs(m_rotate.i.dotproduct(axes[i])) +
                m_halfsize.y * std::abs(m_rotate.j.dotproduct(axes[i])) +
                m_halfsize.z * std::abs(m_rotate.k.dotproduct(axes[i]));

            T obbMin = obbProj - obbHalf;
            T obbMax = obbProj + obbHalf;

            const T epsilon = 0.0001f;
            if (maxTri < obbMin - epsilon || minTri > obbMax + epsilon)
                return false;
        }

        return true;
    }

    ICF void ClampPointOBB(Tvector& clamped_point)
    {
        Tvector local_point;
        local_point.sub(clamped_point, m_translate);

        T x = local_point.dotproduct(m_rotate.i);
        T y = local_point.dotproduct(m_rotate.j);
        T z = local_point.dotproduct(m_rotate.k);

        int outside_count = 0;
        if (x < -m_halfsize.x || x > m_halfsize.x) outside_count++;
        if (y < -m_halfsize.y || y > m_halfsize.y) outside_count++;
        if (z < -m_halfsize.z || z > m_halfsize.z) outside_count++;

        if (outside_count == 0)
            return;

        if (outside_count == 1)
        {
            x = std::max(-m_halfsize.x, std::min(x, m_halfsize.x));
            y = std::max(-m_halfsize.y, std::min(y, m_halfsize.y));
            z = std::max(-m_halfsize.z, std::min(z, m_halfsize.z));
        }
        else if (outside_count == 2)
        {
            T dist[6];
            dist[0] = std::abs(x + m_halfsize.x);
            dist[1] = std::abs(x - m_halfsize.x);
            dist[2] = std::abs(y + m_halfsize.y);
            dist[3] = std::abs(y - m_halfsize.y);
            dist[4] = std::abs(z + m_halfsize.z);
            dist[5] = std::abs(z - m_halfsize.z);

            int closest_face = 0;
            T min_dist = dist[0];
            for (int i = 1; i < 6; i++)
            {
                if (dist[i] < min_dist)
                {
                    min_dist = dist[i];
                    closest_face = i;
                }
            }

            switch (closest_face)
            {
            case 0: x = -m_halfsize.x; break;
            case 1: x = m_halfsize.x; break;
            case 2: y = -m_halfsize.y; break;
            case 3: y = m_halfsize.y; break;
            case 4: z = -m_halfsize.z; break;
            case 5: z = m_halfsize.z; break;
            }

            x = std::max(-m_halfsize.x, std::min(x, m_halfsize.x));
            y = std::max(-m_halfsize.y, std::min(y, m_halfsize.y));
            z = std::max(-m_halfsize.z, std::min(z, m_halfsize.z));
        }
        else
        {
            T vertex_x = (x > 0) ? m_halfsize.x : -m_halfsize.x;
            T vertex_y = (y > 0) ? m_halfsize.y : -m_halfsize.y;
            T vertex_z = (z > 0) ? m_halfsize.z : -m_halfsize.z;

            x = vertex_x;
            y = vertex_y;
            z = vertex_z;
        }

        clamped_point = m_translate;
        clamped_point.mad(m_rotate.i, x);
        clamped_point.mad(m_rotate.j, y);
        clamped_point.mad(m_rotate.k, z);
    }

    ICF void FindContactsClipping(Tvector* tri, void (*callback)(const Tvector& contact, void* user_data), void* user_data = nullptr)
    {
        Tvector polygon[12];
        u8 poly_count = 3;

        for (u8 i = 0; i < 3; i++)
        {
            Tvector local; local.sub(tri[i], m_translate);
            polygon[i].set(local.dotproduct(m_rotate.i), local.dotproduct(m_rotate.j), local.dotproduct(m_rotate.k));
        }

        _plane<T> planes[6]
        {
            {{-1.f,0.f,0.f}, m_halfsize.x},
            {{1.f,0.f,0.f},  m_halfsize.x},
            {{0.f,-1.f,0.f}, m_halfsize.y},
            {{0.f,+1.f,0.f}, m_halfsize.y},
            {{0.f,0.f,-1.f}, m_halfsize.z},
            {{0.f,0.f,+1.f}, m_halfsize.z}
        };

        Tvector temp[12];

        for (u8 i = 0; i < 6 && poly_count >= 3; i++)
        {
            u8 temp_count = 0;

            for (u8 j = 0; j < poly_count; j++)
            {
                Tvector& a = polygon[j];
                Tvector& b = polygon[(j + 1) % poly_count];

                T da = planes[i].classify(a);
                T db = da * planes[i].classify(b);

                if (da >= -EPS)
                {
                    if (temp_count < 12)
                        temp[temp_count++] = a;
                }

                if (db < 0.0f)
                {
                    Tvector ab; ab.sub(b, a);
                    T denom = planes[i].n.dotproduct(ab);

                    if (std::abs(denom) > EPS && temp_count < 12)
                    {
                        T t = std::max(0.0f, std::min(1.0f, -da / denom));
                        temp[temp_count++].mad(a, ab, t);
                    }
                }
            }

            poly_count = temp_count;
            CopyMemory(polygon, temp, poly_count * sizeof(Tvector));
        }

        if (poly_count >= 3)
        {
            for (u8 i = 0; i < poly_count; i++)
            {
                Tvector world_point = m_translate;
                world_point.mad(m_rotate.i, polygon[i].x);
                world_point.mad(m_rotate.j, polygon[i].y);
                world_point.mad(m_rotate.k, polygon[i].z);
                callback(world_point, user_data);
            }
        }
    }
};

typedef		_obb<float>		Fobb;
typedef		_obb<double>	Dobb;

template <class T>
ICF BOOL	_valid			(const _obb<T>& m)
{ 
	return _valid(m.m_rotate) && _valid(m.m_translate) && _valid(m.m_halfsize);
}

#endif