#pragma once

template <class T>
struct _rect {
public:
	typedef T			TYPE;
	typedef _rect<T>	Self;
	typedef Self&		SelfRef;
	typedef const Self&	SelfCRef;
	typedef _vector2<T>	Tvector;
public:
	union{
		struct{
			T 	x1, y1, x2, y2;
		};
		struct{
			T 	left, top, right, bottom;
		};

		struct{
			Tvector lt;
			Tvector rb;
		};
		T		m[4];
	};

	ICF _rect()
	{
		for (auto row = 0; row < 4; ++row)
			m[row] = static_cast<T>(0);
	}
	
	ICF _rect(const std::initializer_list<T>& list)
	{
		R_ASSERT2(list.size() == 4, "Initializer list must contain exactly 4 elements.");
		auto it = list.begin();
		for (auto row = 0; row < 4; ++row)
			m[row] = *it++;
	}

	ICF	SelfRef	set(const T _x1, const T _y1, const T _x2, const T _y2)	{ x1=_x1;	y1=_y1;		x2=_x2;		y2=_y2;		return *this;	};
	ICF	SelfRef	set(const Tvector &mn, const Tvector &mx)		{ x1=mn.x;	y1=mn.y;	x2=mx.x;	y2=mx.y;	return *this;	};
	ICF	SelfRef	set(const Self &r)										{ x1=r.x1;	y1=r.y1;	x2=r.x2;	y2=r.y2;	return *this;	};
	ICF	SelfRef	null( )										{ x1=T(0);	y1=T(0);	x2=T(0);	y2=T(0);		return *this;	};
	
	ICF	SelfRef	invalidate()								{ lt.x=type_max(T); lt.y=type_max(T);	rb.x=type_min(T);	rb.y=type_min(T);	return *this;	};
	ICF	bool	valide	  ()								{ return lt.x < rb.x && lt.y < rb.y; }
	ICF 	SelfRef	set_empty ()								{ return invalidate(); }
	ICF	bool	is_empty  ()								{ return !valide(); }
	ICF	SelfRef	add(T x, T y)								{ x1+=x;	y1+=y;		x2+=x;		y2+=y;			return *this;	};
	ICF	SelfRef	sub(T x, T y)								{ x1-=x;	y1-=y;		x2-=x;		y2-=y;			return *this;	};
	ICF	SelfRef	mul(T x, T y)								{ x1*=x;	y1*=y;		x2*=x;		y2*=y;			return *this;	};
	ICF	SelfRef	div(T x, T y)								{ x1/=x;	y1/=y;		x2/=x;		y2/=y;			return *this;	};

	ICF	SelfRef	add(const Self& r, T x, T y)				{ x1=r.x1+x;	y1=r.y1+y;	x2=r.x2+x;	y2=r.y2+y;	return *this;	};
	ICF	SelfRef	sub(const Self& r, T x, T y)				{ x1=r.x1-x;	y1=r.y1-y;	x2=r.x2-x;	y2=r.y2-y;	return *this;	};
	ICF	SelfRef	mul(const Self& r, T x, T y)				{ x1=r.x1*x;	y1=r.y1*y;	x2=r.x2*x;	y2=r.y2*y;	return *this;	};
	ICF	SelfRef	div(const Self& r, T x, T y)				{ x1=r.x1/x;	y1=r.y1/y;	x2=r.x2/x;	y2=r.y2/y;	return *this;	};

	ICF	bool	in (T  x, T  y)		const					{ return (x>=x1) && (x<=x2) && (y>=y1) && (y<=y2);	};
	ICF	bool	in (Tvector &p)		const					{ return (p.x>=x1) && (p.x<=x2) && (p.y>=y1) && (p.y<=y2);	};
	ICF	bool	cmp(_rect<int> &r)							{ return x1==r.x1 && y1==r.y1 && x2==r.x2 && y2==r.y2; };
	ICF	bool	cmp(_rect<float> &r)						{ return fsimilar(x1,r.x1) && fsimilar(y1,r.y1) && fsimilar(x2,r.x2) && fsimilar(y2,r.y2); };
	
	ICF	void	getcenter(Tvector& center)	const			{ center.add(rb,lt); center.div(2); }
	ICF	void	getsize(Tvector& sz)	const				{ sz.sub(rb,lt); }

	ICF	T		width()		const							{return rb.x-lt.x;}
	ICF	T		height()	const							{return rb.y-lt.y;}

	ICF	SelfRef	shrink(T x, T y)							{ lt.x+=x; lt.y+=y; rb.x-=x; rb.y-=y;	return *this;	};
	ICF	SelfRef	grow(T x, T y)								{ lt.x-=x; lt.y-=y; rb.x+=x; rb.y+=y;	return *this;	};



	ICF bool intersected(SelfCRef b1, SelfCRef b2) const {return !(b1.x1>b2.x2 || b1.x2<b2.x1 || b1.y1>b2.y2 ||  b1.y2<b2.y1);}
	ICF bool intersected(SelfCRef b) const {return intersected(*this,b);}

	ICF bool intersection(SelfCRef b1, SelfCRef b2)
	{
		if (!intersected(b1,b2))
			return	(false);

		x1	= std::max(b1.x1,b2.x1);
		y1	= std::max(b1.y1,b2.y1);
		x2	= std::min(b1.x2,b2.x2);
		y2	= std::min(b1.y2,b2.y2);
		return		(true);
	}

};

typedef _rect<float>	Frect;
typedef _rect<double>	Drect;
typedef _rect<int>		Irect;

template <class T>
ICF bool	_valid			(const _rect<T>& m)
{ 
	return _valid(m.lt) && _valid(m.rb);
}

template<typename T> ISaveObject& operator<<(ISaveObject& Object, _rect<T>& Value) {
	return Object << Value.x1 << Value.x2 << Value.y1 << Value.y2;
}
