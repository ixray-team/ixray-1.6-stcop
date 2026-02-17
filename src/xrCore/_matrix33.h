#ifndef _matrix33H_
#define _matrix33H_

template <class T>
struct _matrix33{
public:
	typedef _matrix33<T>Self;
	typedef Self&		SelfRef;
	typedef const Self&	SelfCRef;
	typedef _vector3<T>	Tvector;
public:
	union {
		struct {						// Direct definition
            T _11, _12, _13;
            T _21, _22, _23;
            T _31, _32, _33;
		};
    	struct {
    		Tvector i;
    		Tvector j;
    		Tvector k;
        };
        Tvector row[3];
        float m[3][3];
        float mm[9];
	};

    ICF _matrix33()
	{
		for (auto row = 0; row < 3; ++row)
			for (auto col = 0; col < 3; ++col)
				m[row][col] = static_cast<T>(0);
	}

    ICF bool operator==(SelfCRef Left)
    {
        return Left.i == i && Left.j == j && Left.k == k;
    }

	// Class members
    ICF SelfRef set_rapid(const _matrix<T> &a)
	{
        m[0][0]	=  a.m[0][0];	m[0][1]	=  a.m[0][1];	m[0][2]	= -a.m[0][2];
        m[1][0]	=  a.m[1][0];	m[1][1]	=  a.m[1][1];	m[1][2]	= -a.m[1][2];
        m[2][0]	= -a.m[2][0];	m[2][1]	= -a.m[2][1];	m[2][2]	=  a.m[2][2];
		return *this;
	}
    ICF SelfRef set(SelfCRef a)
	{
		CopyMemory(this,&a,9*sizeof(float));
		return *this;
	}
    ICF SelfRef set(const _matrix<T> &a)
	{
    	_11=a._11; _12=a._12; _13=a._13;
    	_21=a._21; _22=a._22; _23=a._23;
    	_31=a._31; _32=a._32; _33=a._33;
		return *this;
	}
    ICF SelfRef identity(void) {
    	_11=1.f; _12=0.f; _13=0.f;
    	_21=0.f; _22=1.f; _23=0.f;
    	_31=0.f; _32=0.f; _33=1.f;
		return *this;
	}

    ICF SelfRef transpose(SelfCRef matSource)	// faster version of transpose
	{
		_11=matSource._11;	_12=matSource._21;	_13=matSource._31;
		_21=matSource._12;	_22=matSource._22;	_23=matSource._32;
		_31=matSource._13;	_32=matSource._23;	_33=matSource._33;
		return *this;
	}
    ICF SelfRef transpose(const _matrix<T> &matSource)		// faster version of transpose
	{
		_11=matSource._11;	_12=matSource._21;	_13=matSource._31;
		_21=matSource._12;	_22=matSource._22;	_23=matSource._32;
		_31=matSource._13;	_32=matSource._23;	_33=matSource._33;
		return *this;
	}
    ICF SelfRef transpose(void)						// self transpose - slower
	{
		_matrix33 a;
		CopyMemory(&a,this,9*sizeof(float));					// save matrix
		transpose(a);
		return *this;
	}

    ICF SelfRef MxM(SelfCRef M1, SelfCRef M2)
    {
      m[0][0] = (   M1.m[0][0] * M2.m[0][0] +
                    M1.m[0][1] * M2.m[1][0] +
                    M1.m[0][2] * M2.m[2][0]);
      m[1][0] = (   M1.m[1][0] * M2.m[0][0] +
                    M1.m[1][1] * M2.m[1][0] +
                    M1.m[1][2] * M2.m[2][0]);
      m[2][0] = (   M1.m[2][0] * M2.m[0][0] +
                    M1.m[2][1] * M2.m[1][0] +
                    M1.m[2][2] * M2.m[2][0]);
      m[0][1] = (   M1.m[0][0] * M2.m[0][1] +
                    M1.m[0][1] * M2.m[1][1] +
                    M1.m[0][2] * M2.m[2][1]);
      m[1][1] = (   M1.m[1][0] * M2.m[0][1] +
                    M1.m[1][1] * M2.m[1][1] +
                    M1.m[1][2] * M2.m[2][1]);
      m[2][1] = (   M1.m[2][0] * M2.m[0][1] +
                    M1.m[2][1] * M2.m[1][1] +
                    M1.m[2][2] * M2.m[2][1]);
      m[0][2] = (   M1.m[0][0] * M2.m[0][2] +
                    M1.m[0][1] * M2.m[1][2] +
                    M1.m[0][2] * M2.m[2][2]);
      m[1][2] = (   M1.m[1][0] * M2.m[0][2] +
                    M1.m[1][1] * M2.m[1][2] +
                    M1.m[1][2] * M2.m[2][2]);
      m[2][2] = (   M1.m[2][0] * M2.m[0][2] +
                    M1.m[2][1] * M2.m[1][2] +
                    M1.m[2][2] * M2.m[2][2]);
	  return *this;
	}

    ICF SelfRef MTxM(SelfCRef M1, SelfCRef M2)
    {
      m[0][0] = (   M1.m[0][0] * M2.m[0][0] +
                    M1.m[1][0] * M2.m[1][0] +
                    M1.m[2][0] * M2.m[2][0]);
      m[1][0] = (   M1.m[0][1] * M2.m[0][0] +
                    M1.m[1][1] * M2.m[1][0] +
                    M1.m[2][1] * M2.m[2][0]);
      m[2][0] = (   M1.m[0][2] * M2.m[0][0] +
                    M1.m[1][2] * M2.m[1][0] +
                    M1.m[2][2] * M2.m[2][0]);
      m[0][1] = (   M1.m[0][0] * M2.m[0][1] +
                    M1.m[1][0] * M2.m[1][1] +
                    M1.m[2][0] * M2.m[2][1]);
      m[1][1] = (   M1.m[0][1] * M2.m[0][1] +
                    M1.m[1][1] * M2.m[1][1] +
                    M1.m[2][1] * M2.m[2][1]);
      m[2][1] = (   M1.m[0][2] * M2.m[0][1] +
                    M1.m[1][2] * M2.m[1][1] +
                    M1.m[2][2] * M2.m[2][1]);
      m[0][2] = (   M1.m[0][0] * M2.m[0][2] +
                    M1.m[1][0] * M2.m[1][2] +
                    M1.m[2][0] * M2.m[2][2]);
      m[1][2] = (   M1.m[0][1] * M2.m[0][2] +
                    M1.m[1][1] * M2.m[1][2] +
                    M1.m[2][1] * M2.m[2][2]);
      m[2][2] = (   M1.m[0][2] * M2.m[0][2] +
                    M1.m[1][2] * M2.m[1][2] +
                    M1.m[2][2] * M2.m[2][2]);
	  return *this;
    }


#define ROT(a,i,j,k,l) g=a.m[i][j]; h=a.m[k][l]; a.m[i][j]=g-s*(h+g*tau); a.m[k][l]=h+s*(g-h*tau);

    ICF int Meigen(Tvector& dout, SelfRef a)
    {
        int i;
        float tresh,theta,tau,t,sm,s,h,g,c;
        Tvector b;
        Tvector z;
        _matrix33 v;
        Tvector d;

        v.identity();

        b.set(a.m[0][0], a.m[1][1], a.m[2][2]);
        d.set(a.m[0][0], a.m[1][1], a.m[2][2]);
        z.set(0,0,0);

        for(i=0; i<50; i++){
            sm=0.0f; sm+= std::abs(a.m[0][1]); sm+= std::abs(a.m[0][2]); sm+= std::abs(a.m[1][2]);
            if (sm == 0.0) { set(v); dout.set(d); return i; }
            if (i < 3) tresh=0.2f*sm/(3.0f*3.0f); else tresh=0.0f;
            {
                g = 100.0f*std::abs(a.m[0][1]);
                if (i>3 && std::abs(d.x)+g== std::abs(d.x) && std::abs(d.y)+g== std::abs(d.y))
                    a.m[0][1]=0.0;
                else if (std::abs(a.m[0][1])>tresh){
                    h = d.y-d.x;
                    if (std::abs(h)+g == std::abs(h)) t=(a.m[0][1])/h;
                    else{
                        theta=0.5f*h/(a.m[0][1]);
                        t=1.0f/(std::abs(theta)+_sqrt(1.0f+theta*theta));
                        if (theta < 0.0f) t = -t;
                    }
                    c=1.0f/_sqrt(1+t*t); s=t*c; tau=s/(1.0f+c); h=t*a.m[0][1];
                    z.x -= h; z.y += h; d.x -= h; d.y += h;
                    a.m[0][1]=0.0f;
                    ROT(a,0,2,1,2); ROT(v,0,0,0,1); ROT(v,1,0,1,1); ROT(v,2,0,2,1);
                }
            }
            {
                g = 100.0f* std::abs(a.m[0][2]);
                if (i>3 && std::abs(d.x)+g== std::abs(d.x) && std::abs(d.z)+g== std::abs(d.z))
                    a.m[0][2]=0.0f;
                else if (std::abs(a.m[0][2])>tresh){
                    h = d.z-d.x;
                    if (std::abs(h)+g == std::abs(h)) t=(a.m[0][2])/h;
                    else{
                        theta=0.5f*h/(a.m[0][2]);
                        t=1.0f/(std::abs(theta)+_sqrt(1.0f+theta*theta));
                        if (theta < 0.0f) t = -t;
                    }
                    c=1.0f/_sqrt(1+t*t); s=t*c; tau=s/(1.0f+c); h=t*a.m[0][2];
                    z.x -= h; z.z += h; d.x -= h; d.z += h;
                    a.m[0][2]=0.0f;
                    ROT(a,0,1,1,2); ROT(v,0,0,0,2); ROT(v,1,0,1,2); ROT(v,2,0,2,2);
                }
            }
            {
                g = 100.0f* std::abs(a.m[1][2]);
                if (i>3 && std::abs(d.y)+g== std::abs(d.y) && std::abs(d.z)+g== std::abs(d.z))
                    a.m[1][2]=0.0f;
                else if (std::abs(a.m[1][2])>tresh){
                    h = d.z-d.y;
                    if (std::abs(h)+g == std::abs(h)) t=(a.m[1][2])/h;
                    else{
                        theta=0.5f*h/(a.m[1][2]);
                        t=1.0f/(std::abs(theta)+_sqrt(1.0f+theta*theta));
                        if (theta < 0.0) t = -t;
                    }
                    c=1.0f/_sqrt(1+t*t); s=t*c; tau=s/(1.0f+c); h=t*a.m[1][2];
                    z.y -= h; z.z += h; d.y -= h; d.z += h;
                    a.m[1][2]=0.0f;
                    ROT(a,0,1,0,2); ROT(v,0,1,0,2); ROT(v,1,1,1,2); ROT(v,2,1,2,2);
                }
            }
            b.add(z);
            d.set(b);
            z.set(0,0,0);
        }
//        Log.Msg("eigen: too many iterations in Jacobi transform (%d).\n", i);
        return i;
    }
#undef ROT

//--------------------------------------------------------------------------------
// other unused function
//--------------------------------------------------------------------------------
    ICF SelfRef McolcMcol(int cr, SelfCRef M, int c)
    {
        m[0][cr] = M.m[0][c];
        m[1][cr] = M.m[1][c];
        m[2][cr] = M.m[2][c];
		return *this;
    }

    ICF SelfRef MxMpV(SelfCRef M1, SelfCRef M2, const Tvector& Tv)
    {
        m[0][0] = ( M1.m[0][0] * M2.m[0][0] +
                    M1.m[0][1] * M2.m[1][0] +
                    M1.m[0][2] * M2.m[2][0] + Tv.x);
        m[1][0] = ( M1.m[1][0] * M2.m[0][0] +
                    M1.m[1][1] * M2.m[1][0] +
                    M1.m[1][2] * M2.m[2][0] + Tv.y);
        m[2][0] = ( M1.m[2][0] * M2.m[0][0] +
                    M1.m[2][1] * M2.m[1][0] +
                    M1.m[2][2] * M2.m[2][0] + Tv.z);
        m[0][1] = ( M1.m[0][0] * M2.m[0][1] +
                    M1.m[0][1] * M2.m[1][1] +
                    M1.m[0][2] * M2.m[2][1] + Tv.x);
        m[1][1] = ( M1.m[1][0] * M2.m[0][1] +
                    M1.m[1][1] * M2.m[1][1] +
                    M1.m[1][2] * M2.m[2][1] + Tv.y);
        m[2][1] = ( M1.m[2][0] * M2.m[0][1] +
                    M1.m[2][1] * M2.m[1][1] +
                    M1.m[2][2] * M2.m[2][1] + Tv.z);
        m[0][2] = ( M1.m[0][0] * M2.m[0][2] +
                    M1.m[0][1] * M2.m[1][2] +
                    M1.m[0][2] * M2.m[2][2] + Tv.x);
        m[1][2] = ( M1.m[1][0] * M2.m[0][2] +
                    M1.m[1][1] * M2.m[1][2] +
                    M1.m[1][2] * M2.m[2][2] + Tv.y);
        m[2][2] = ( M1.m[2][0] * M2.m[0][2] +
                    M1.m[2][1] * M2.m[1][2] +
                    M1.m[2][2] * M2.m[2][2] + Tv.z);
		return *this;
    }

    ICF SelfRef Mqinverse(SelfCRef M)
    {
        int i,j;

        for(i=0; i<3; i++)
            for(j=0; j<3; j++){
                int i1 = (i+1)%3;
                int i2 = (i+2)%3;
                int j1 = (j+1)%3;
                int j2 = (j+2)%3;
                m[i][j] = (M.m[j1][i1]*M.m[j2][i2] - M.m[j1][i2]*M.m[j2][i1]);
          }
			return *this;
    }

    ICF SelfRef MxMT(SelfCRef M1, SelfCRef M2)
    {
        m[0][0] = ( M1.m[0][0] * M2.m[0][0] +
                    M1.m[0][1] * M2.m[0][1] +
                    M1.m[0][2] * M2.m[0][2]);
        m[1][0] = ( M1.m[1][0] * M2.m[0][0] +
                    M1.m[1][1] * M2.m[0][1] +
                    M1.m[1][2] * M2.m[0][2]);
        m[2][0] = ( M1.m[2][0] * M2.m[0][0] +
                    M1.m[2][1] * M2.m[0][1] +
                    M1.m[2][2] * M2.m[0][2]);
        m[0][1] = ( M1.m[0][0] * M2.m[1][0] +
                    M1.m[0][1] * M2.m[1][1] +
                    M1.m[0][2] * M2.m[1][2]);
        m[1][1] = ( M1.m[1][0] * M2.m[1][0] +
                    M1.m[1][1] * M2.m[1][1] +
                    M1.m[1][2] * M2.m[1][2]);
        m[2][1] = ( M1.m[2][0] * M2.m[1][0] +
                    M1.m[2][1] * M2.m[1][1] +
                    M1.m[2][2] * M2.m[1][2]);
        m[0][2] = ( M1.m[0][0] * M2.m[2][0] +
                    M1.m[0][1] * M2.m[2][1] +
                    M1.m[0][2] * M2.m[2][2]);
        m[1][2] = ( M1.m[1][0] * M2.m[2][0] +
                    M1.m[1][1] * M2.m[2][1] +
                    M1.m[1][2] * M2.m[2][2]);
        m[2][2] = ( M1.m[2][0] * M2.m[2][0] +
                    M1.m[2][1] * M2.m[2][1] +
                    M1.m[2][2] * M2.m[2][2]);
		return *this;
    }

    ICF SelfRef MskewV(const Tvector& v)
    {
        m[0][0] = m[1][1] = m[2][2] = 0.0;
        m[1][0] = v.z;
        m[0][1] = -v.z;
        m[0][2] = v.y;
        m[2][0] = -v.y;
        m[1][2] = -v.x;
        m[2][1] = v.x;
		return *this;
    }
    ICF SelfRef sMxVpV(Tvector& R, float s1, const Tvector& V1, const Tvector& V2) const
	{
		R.x = s1 * (m[0][0] * V1.x + m[0][1] * V1.y + m[0][2] * V1.z) + V2.x;
		R.y = s1 * (m[1][0] * V1.x + m[1][1] * V1.y + m[1][2] * V1.z) + V2.y;
		R.z = s1 * (m[2][0] * V1.x + m[2][1] * V1.y + m[2][2] * V1.z) + V2.z;
		return *this;
	}
    ICF void MTxV(Tvector& R, const Tvector& V1) const
	{
		R.x = (m[0][0] * V1.x + m[1][0] * V1.y + m[2][0] * V1.z);
		R.y = (m[0][1] * V1.x + m[1][1] * V1.y + m[2][1] * V1.z);
		R.z = (m[0][2] * V1.x + m[1][2] * V1.y + m[2][2] * V1.z);
	}
    ICF void MTxVpV(Tvector& R, const Tvector& V1, const Tvector& V2) const
	{
		R.x = (m[0][0] * V1.x + m[1][0] * V1.y + m[2][0] * V1.z + V2.x);
		R.y = (m[0][1] * V1.x + m[1][1] * V1.y + m[2][1] * V1.z + V2.y);
		R.z = (m[0][2] * V1.x + m[1][2] * V1.y + m[2][2] * V1.z + V2.z);
	}
    ICF SelfRef MTxVmV(Tvector& R, const Tvector& V1, const Tvector& V2) const
	{
		R.x = (m[0][0] * V1.x + m[1][0] * V1.y + m[2][0] * V1.z - V2.x);
		R.y = (m[0][1] * V1.x + m[1][1] * V1.y + m[2][1] * V1.z - V2.y);
		R.z = (m[0][2] * V1.x + m[1][2] * V1.y + m[2][2] * V1.z - V2.z);
		return *this;
	}
    ICF SelfRef sMTxV(Tvector& R, float s1, const Tvector& V1) const
	{
		R.x = s1*(m[0][0] * V1.x + m[1][0] * V1.y + m[2][0] * V1.z);
		R.y = s1*(m[0][1] * V1.x + m[1][1] * V1.y + m[2][1] * V1.z);
		R.z = s1*(m[0][2] * V1.x + m[1][2] * V1.y + m[2][2] * V1.z);
		return *this;
	}
    ICF SelfRef MxV(Tvector& R, const Tvector& V1) const
	{
		R.x = (m[0][0] * V1.x + m[0][1] * V1.y + m[0][2] * V1.z);
		R.y = (m[1][0] * V1.x + m[1][1] * V1.y + m[1][2] * V1.z);
		R.z = (m[2][0] * V1.x + m[2][1] * V1.y + m[2][2] * V1.z);
		return *this;
	}
    ICF	void transform_dir		(_vector2<T> &dest, const _vector2<T> &v)	const 	// preferred to use
	{
		dest.x = v.x*_11 + v.y*_21;
		dest.y = v.x*_12 + v.y*_22;
		dest.z = v.x*_13 + v.y*_23;
	}
    ICF	void transform_dir		(_vector2<T> &v) const
	{
		_vector2<T>		res;
		transform_dir	(res,v);
		v.set			(res);
	}
    ICF SelfRef MxVpV(Tvector& R, const Tvector& V1, const Tvector& V2) const
	{
		R.x = (m[0][0] * V1.x + m[0][1] * V1.y + m[0][2] * V1.z + V2.x);
		R.y = (m[1][0] * V1.x + m[1][1] * V1.y + m[1][2] * V1.z + V2.y);
		R.z = (m[2][0] * V1.x + m[2][1] * V1.y + m[2][2] * V1.z + V2.z);
		return *this;
	}
};

typedef		_matrix33<float>	Fmatrix33;
typedef		_matrix33<double>	Dmatrix33;

template <class T>
ICF BOOL	_valid			(const _matrix33<T>& m)
{ 
	return 
		_valid(m.i)&& 
		_valid(m.j)&&
		_valid(m.k);
}

#endif

