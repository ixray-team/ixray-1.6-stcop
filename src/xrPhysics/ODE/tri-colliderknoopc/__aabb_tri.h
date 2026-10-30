#pragma  once

	// HACK: renamed from Point - a global Carbon type on macOS.
	class  TriPoint
	{
		public:

		//! Empty constructor
		IC				TriPoint() = default;
		//! Constructor from floats
		IC				TriPoint(float _x, float _y, float _z) : x(_x), y(_y), z(_z)	{}
		//! Constructor from array
		IC				TriPoint(float f[3]) : x(f[0]), y(f[1]), z(f[2])				{}
		//! Constructor from array
		IC				TriPoint(const float f[3]) : x(f[0]), y(f[1]), z(f[2])			{}
		//! Copy constructor
		IC				TriPoint(const TriPoint& p) : x(p.x), y(p.y), z(p.z)				{}
		//! Destructor
		IC				~TriPoint() = default;

		//! Returns MIN(x, y, z);
		//IC	float		Min()								const		{ return MIN(x, MIN(y, z));												}
		//! Returns MAX(x, y, z);
		//IC	float		Max()								const		{ return MAX(x, MAX(y, z));												}
		//! TO BE DOCUMENTED
		//IC	TriPoint&		Min(const TriPoint& p)								{ x = MIN(x, p.x); y = MIN(y, p.y); z = MIN(z, p.z);	return *this;	}
		//! TO BE DOCUMENTED
		//IC	TriPoint&		Max(const TriPoint& p)								{ x = MAX(x, p.x); y = MAX(y, p.y); z = MAX(z, p.z);	return *this;	}

		//! Computes square magnitude
		IC	float		SquareMagnitude()					const		{ return x*x + y*y + z*z;												}
		//! Computes magnitude
		IC	float		Magnitude()							const		{ return _sqrt(x*x + y*y + z*z);										}

		//! Return largest axis
		IC	u32			LargestAxis()						const
						{
							const float* Vals = &x;
							u32 m = 0;
							if(Vals[1] > Vals[m]) m = 1;
							if(Vals[2] > Vals[m]) m = 2;
							return m;
						}

		// Arithmetic operators
		//! Operator for TriPoint Negate = - TriPoint
		ICF	TriPoint		operator-()							const		{ return TriPoint(-x, -y, -z);							}

		//! Operator for TriPoint Plus = TriPoint + TriPoint.
		ICF	TriPoint		operator+(const TriPoint& p)			const		{ return TriPoint(x + p.x, y + p.y, z + p.z);			}
		//! Operator for TriPoint Minus = TriPoint - TriPoint.
		ICF	TriPoint		operator-(const TriPoint& p)			const		{ return TriPoint(x - p.x, y - p.y, z - p.z);			}
		//! Operator for TriPoint Scale = TriPoint * float.
		ICF	TriPoint		operator*(float s)					const		{ return TriPoint(x * s,   y * s,   z * s );			}
		//! Operator for TriPoint Scale = float * TriPoint.
		friend			TriPoint		operator*(float s, const TriPoint& p)				{ return TriPoint(s * p.x, s * p.y, s * p.z);			}
		//! Operator for TriPoint Scale = TriPoint / float.
		ICF	TriPoint		operator/(float s)					const		{ s = 1.0f / s; return TriPoint(x * s, y * s, z * s);	}

		//! Operator for float DotProd = TriPoint | TriPoint.
		ICF	float		operator|(const TriPoint& p)			const		{ return x*p.x + y*p.y + z*p.z;						}
		//! Operator for TriPoint VecProd = TriPoint ^ TriPoint.
		ICF	TriPoint		operator^(const TriPoint& p)			const
						{
							return TriPoint(
							y * p.z - z * p.y,
							z * p.x - x * p.z,
							x * p.y - y * p.x );
						}

		//! Operator for TriPoint += TriPoint.
		ICF	TriPoint&		operator+=(const TriPoint& p)						{ x += p.x; y += p.y; z += p.z;	return *this;		}
		//! Operator for TriPoint += float.
		ICF	TriPoint&		operator+=(float s)								{ x += s;   y += s;   z += s;	return *this;		}

		//! Operator for TriPoint -= TriPoint.
		ICF	TriPoint&		operator-=(const TriPoint& p)						{ x -= p.x; y -= p.y; z -= p.z;	return *this;		}
		//! Operator for TriPoint -= float.
		ICF	TriPoint&		operator-=(float s)								{ x -= s;   y -= s;   z -= s;	return *this;		}
		//! Operator for TriPoint *= float.
		ICF	TriPoint&		operator*=(float s)								{ x *= s; y *= s; z *= s;		return *this;		}
		//! Operator for TriPoint /= float.
		ICF	TriPoint&		operator/=(float s)								{ s = 1.0f/s; x *= s; y *= s; z *= s; return *this; }

		// Arithmetic operators
		//! Operator for TriPoint Mul = TriPoint * Matrix3x3.
	//					TriPoint		operator*(const Matrix3x3& mat)		const;
		//! Operator for TriPoint Mul = TriPoint * Matrix4x4.
	//					TriPoint		operator*(const Matrix4x4& mat)		const;
		//! Operator for TriPoint *= Matrix3x3.
	///					TriPoint&		operator*=(const Matrix3x3& mat);
		//! Operator for TriPoint *= Matrix4x4.
	//					TriPoint&		operator*=(const Matrix4x4& mat);

		//! Access as array
		ICF				operator	const	float*() const	{ return &x; }
		//! Access as array
		ICF				operator			float*()		{ return &x; }

		public:
						float		x;		//!< x coordinate
						float		y;		//!< y coordinate
						float		z;		//!< z coordinate
	};
//using namespace CDB;
using namespace Opcode;
//typedef float*	TriPoint;	
//typedef	dVector3 TriPoint
//! This macro quickly finds the min & max values among 3 variables
#define FINDMINMAX(x0, x1, x2, min, max)	\
	min = max = x0;							\
	if(x1<min) min=x1;						\
	if(x1>max) max=x1;						\
	if(x2<min) min=x2;						\
	if(x2>max) max=x2;

#define EXITMINMAX(x0, x1, x2, min, max)		\
	if(x0<min&&x1<min&&x2<min)return false;		\
	if(x0>max&&x1>max&&x2>max)return false;

//! TO BE DOCUMENTED
ICF bool planeBoxOverlap_slow(const TriPoint& normal, const float d, const TriPoint& maxbox)
{
	TriPoint vmin, vmax;
	if(normal[0]>0.0f)	{ vmin[0] =-maxbox[0]; vmax[0]= maxbox[0]; }
	else				{ vmin[0] = maxbox[0]; vmax[0]=-maxbox[0]; }
	if(normal[1]>0.0f)	{ vmin[1] =-maxbox[1]; vmax[1]= maxbox[1]; }
	else				{ vmin[1] = maxbox[1]; vmax[1]=-maxbox[1]; }
	if(normal[2]>0.0f)	{ vmin[2] =-maxbox[2]; vmax[2]= maxbox[2]; }
	else				{ vmin[2] = maxbox[2]; vmax[2]=-maxbox[2]; }
	if((normal|vmin)+d >= 0.0f)	return false;
	if((normal|vmax)+d >  0.0f)	return true;
	return	false		;
}

ICF bool planeBoxOverlap(const TriPoint& normal, const float d, const TriPoint& maxbox)
{
	float norm_box_proj= std::abs(maxbox.x*normal.x)+ std::abs(maxbox.y*normal.y)+ std::abs(maxbox.z*normal.z);
	if(d>-norm_box_proj&&d<norm_box_proj)return true;
	else return false;
}

//! TO BE DOCUMENTED
#define AXISTEST_X01(a, b, fa, fb)							\
	min = a*v0.y - b*v0.z;									\
	max = a*v2.y - b*v2.z;									\
	if(min>max) {const float tmp=max; max=min; min=tmp;	}	\
	rad = fa * extents.y + fb * extents.z;					\
	if(min>rad || max<-rad) return false;

//! TO BE DOCUMENTED
#define AXISTEST_X2(a, b, fa, fb)							\
	min = a*v0.y - b*v0.z;									\
	max = a*v1.y - b*v1.z;									\
	if(min>max) {const float tmp=max; max=min; min=tmp;	}	\
	rad = fa * extents.y + fb * extents.z;					\
	if(min>rad || max<-rad) return false;

//! TO BE DOCUMENTED
#define AXISTEST_Y02(a, b, fa, fb)							\
	min = b*v0.z - a*v0.x;									\
	max = b*v2.z - a*v2.x;									\
	if(min>max) {const float tmp=max; max=min; min=tmp;	}	\
	rad = fa * extents.x + fb * extents.z;					\
	if(min>rad || max<-rad) return false;

//! TO BE DOCUMENTED
#define AXISTEST_Y1(a, b, fa, fb)							\
	min = b*v0.z - a*v0.x;									\
	max = b*v1.z - a*v1.x;									\
	if(min>max) {const float tmp=max; max=min; min=tmp;	}	\
	rad = fa * extents.x + fb * extents.z;					\
	if(min>rad || max<-rad) return false;

//! TO BE DOCUMENTED
#define AXISTEST_Z12(a, b, fa, fb)							\
	min = a*v1.x - b*v1.y;									\
	max = a*v2.x - b*v2.y;									\
	if(min>max) {const float tmp=max; max=min; min=tmp;	}	\
	rad = fa * extents.x + fb * extents.y;					\
	if(min>rad || max<-rad) return false;

//! TO BE DOCUMENTED
#define AXISTEST_Z0(a, b, fa, fb)							\
	min = a*v0.x - b*v0.y;									\
	max = a*v1.x - b*v1.y;									\
	if(min>max) {const float tmp=max; max=min; min=tmp;	}	\
	rad = fa * extents.x + fb * extents.y;					\
	if(min>rad || max<-rad) return false;
	


IC	bool		aabb_tri_aabb(TriPoint center,TriPoint extents,const TriPoint* mLeafVerts)
{
	TriPoint v0, v1, v2;
	//Fvector v0,v1,v2;
	v0.x = mLeafVerts[0].x - center.x;
	v1.x = mLeafVerts[1].x - center.x;
	v2.x = mLeafVerts[2].x - center.x;

	// First, test overlap in the {x,y,z}-directions
	//float min,max;
	// Find min, max of the triangle in x-direction, and test for overlap in X
	//FINDMINMAX(v0.x, v1.x, v2.x, min, max);
	//if(min>extents.x || max<-extents.x) return false;
	EXITMINMAX(v0.x, v1.x, v2.x,-extents.x,extents.x)
	// Same for Y
	v0.y = mLeafVerts[0].y - center.y;
	v1.y = mLeafVerts[1].y - center.y;
	v2.y = mLeafVerts[2].y - center.y;

	//FINDMINMAX(v0.y, v1.y, v2.y, min, max);
	//if(min>extents.y || max<-extents.y) return false;
	EXITMINMAX(v0.y, v1.y, v2.y,-extents.y,extents.y)
	// Same for Z
	v0.z = mLeafVerts[0].z - center.z;
	v1.z = mLeafVerts[1].z - center.z;
	v2.z = mLeafVerts[2].z - center.z;

	//FINDMINMAX(v0.z, v1.z, v2.z, min, max);
	//if(min>extents.z || max<-extents.z) return false;
	EXITMINMAX(v0.z, v1.z, v2.z,-extents.z,extents.z)
	// 2) Test if the box intersects the plane of the triangle
	// compute plane equation of triangle: normal*x+d=0
	// ### could be precomputed since we use the same leaf triangle several times
	const TriPoint e0 = v1 - v0;
	const TriPoint e1 = v2 - v1;
	const TriPoint normal = e0 ^ e1;
	const float d = -normal|v0;
	bool	r0 = planeBoxOverlap(normal, d, extents);
	
#ifdef DEBUG
	if	(r0 != planeBoxOverlap_slow(normal, d, extents))
	{
		Msg("planeBoxOverlap != planeBoxOverlap_slow");
		Msg("normal %f,%f,%f", normal.x,normal.y,normal.z);
		Msg("dist %f",d);
		Msg("extents %f,%f,%f", extents.x,extents.y,extents.z);
	}
#endif
	if(!r0)		return false;
	return true;
}
IC	bool		__aabb_tri		(TriPoint center,TriPoint extents,const TriPoint* mLeafVerts)
	{
		// move everything so that the boxcenter is in (0,0,0) 
		TriPoint v0, v1, v2;
		//Fvector v0,v1,v2;
		v0.x = mLeafVerts[0].x - center.x;
		v1.x = mLeafVerts[1].x - center.x;
		v2.x = mLeafVerts[2].x - center.x;
		
		// First, test overlap in the {x,y,z}-directions
		float min,max;
		// Find min, max of the triangle in x-direction, and test for overlap in X
		FINDMINMAX(v0.x, v1.x, v2.x, min, max);
		if(min>extents.x || max<-extents.x) return false;
		
		// Same for Y
		v0.y = mLeafVerts[0].y - center.y;
		v1.y = mLeafVerts[1].y - center.y;
		v2.y = mLeafVerts[2].y - center.y;
		
		FINDMINMAX(v0.y, v1.y, v2.y, min, max);
		if(min>extents.y || max<-extents.y) return false;
		
		// Same for Z
		v0.z = mLeafVerts[0].z - center.z;
		v1.z = mLeafVerts[1].z - center.z;
		v2.z = mLeafVerts[2].z - center.z;
		
		FINDMINMAX(v0.z, v1.z, v2.z, min, max);
		if(min>extents.z || max<-extents.z) return false;
		
		// 2) Test if the box intersects the plane of the triangle
		// compute plane equation of triangle: normal*x+d=0
		// ### could be precomputed since we use the same leaf triangle several times
		const TriPoint e0 = v1 - v0;
		const TriPoint e1 = v2 - v1;
		const TriPoint normal = e0 ^ e1;
		const float d = -normal|v0;
		bool	r0 = planeBoxOverlap(normal, d, extents);
#ifdef DEBUG
		if	(r0 != planeBoxOverlap_slow(normal, d, extents))
		{
			Msg("planeBoxOverlap != planeBoxOverlap_slow");
			Msg("normal %f,%f,%f", normal.x,normal.y,normal.z);
			Msg("dist %f",d);
			Msg("extents %f,%f,%f", extents.x,extents.y,extents.z);
		}
#endif
		if		(!r0)	return false;
		
		// 3) "Class III" tests
		//if(bClass3)
		{
			float rad;
			min = 0.0f;
			max = 0.0f;
			// compute triangle edges
			// - edges lazy evaluated to take advantage of early exits
			// - fabs precomputed (half less work, possible since extents are always >0)
			// - customized macros to take advantage of the null component
			// - axis vector3 discarded, possibly saves useless movs
			
			const float fey0 = std::abs(e0.y);
			const float fez0 = std::abs(e0.z);
			AXISTEST_X01(e0.z, e0.y, fez0, fey0);
			const float fex0 = std::abs(e0.x);
			AXISTEST_Y02(e0.z, e0.x, fez0, fex0);
			AXISTEST_Z12(e0.y, e0.x, fey0, fex0);
			
			const float fey1 = std::abs(e1.y);
			const float fez1 = std::abs(e1.z);
			AXISTEST_X01(e1.z, e1.y, fez1, fey1);
			const float fex1 = std::abs(e1.x);
			AXISTEST_Y02(e1.z, e1.x, fez1, fex1);
			AXISTEST_Z0(e1.y, e1.x, fey1, fex1);
			
			const TriPoint e2 = mLeafVerts[0] - mLeafVerts[2];
			const float fey2 = std::abs(e2.y);
			const float fez2 = std::abs(e2.z);
			AXISTEST_X2(e2.z, e2.y, fez2, fey2);
			const float fex2 = std::abs(e2.x);
			AXISTEST_Y1(e2.z, e2.x, fez2, fex2);
			AXISTEST_Z12(e2.y, e2.x, fey2, fex2);
		}
		return true;
	}
	

