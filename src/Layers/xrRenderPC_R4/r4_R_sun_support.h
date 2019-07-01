#ifndef	r3_R_sun_support_included
#define	r3_R_sun_support_included
#pragma once

//////////////////////////////////////////////////////////////////////////
// OLES: naive builder of infinite volume expanded from base frustum towards 
//		 light source. really slow, but it works for our simple usage :)
// note: normals points to 'outside'
//////////////////////////////////////////////////////////////////////////
template <bool _debug>
class	DumbConvexVolume
{
public:
	struct	_poly
	{
		xr_vector<int>	points;
		Fvector3		planeN;
		float			planeD;
		float			classify	(Fvector3& p)	{	return planeN.dotproduct(p)+planeD; 	}
	};
	struct	_edge
	{
		int				p0,p1;
		int				counter;
		_edge		(int _p0, int _p1, int m) : p0(_p0), p1(_p1), counter(m){ if (p0>p1)	swap(p0,p1); 	}
		bool			equal		(_edge& E)												{ return p0==E.p0 && p1==E.p1;	}
	};
public:
	xr_vector<Fvector3>		points;
	xr_vector<_poly>		polys;
	xr_vector<_edge>		edges;
public:
	void				compute_planes	()
	{
		for (int it=0; it<int(polys.size()); it++)
		{
			_poly&			P	=	polys[it];
			Fvector3		t1,t2;
			float			len;

			t1.sub					(points[P.points[0]], points[P.points[1]]);
			t2.sub					(points[P.points[0]], points[P.points[2]]);

			P.planeN.crossproduct	(t1,t2);
			
			len = P.planeN.magnitude();

			if (len > std::numeric_limits<float>::min())
			{
				P.planeN.mul(1/len);
			}
			else
			{
				t2.sub					(points[P.points[0]], points[P.points[3]]);
				P.planeN.crossproduct	(t1,t2);
				if (len > std::numeric_limits<float>::min())
				{
					P.planeN.mul(1/len);
				}
				else
				{
					//	HACK:	Remove plane.
					//VERIFY(!"Can't build normal to plane!");
					polys.erase(polys.begin()+it);
					--it;
					continue;
				}
			}

			P.planeD			= -	P.planeN.dotproduct(points[P.points[0]]);

			// verify
			if (_debug)
			{
				Fvector&		p0	= points[P.points[0]];
				Fvector&		p1	= points[P.points[1]];
				Fvector&		p2	= points[P.points[2]];
				Fvector&		p3	= points[P.points[3]];
				Fplane	p012;	p012.build(p0,p1,p2);
				Fplane	p123;	p123.build(p1,p2,p3);
				Fplane	p230;	p230.build(p2,p3,p0);
				Fplane	p301;	p301.build(p3,p0,p1);
				VERIFY	(p012.n.similar(p123.n) && p012.n.similar(p230.n) && p012.n.similar(p301.n));
			}

			/*
			t1.sub					(points[P.points[0]], points[P.points[1]]);


			//	HACK: Igor: just make sure we calculated the plane

			t2.sub					(points[P.points[0]], points[P.points[2]]);
			P.planeN.crossproduct	(t1,t2).normalize();


			P.planeD			= -	P.planeN.dotproduct(points[P.points[0]]);

			// verify
			if (_debug)
			{
				Fvector&		p0	= points[P.points[0]];
				Fvector&		p1	= points[P.points[1]];
				Fvector&		p2	= points[P.points[2]];
				Fvector&		p3	= points[P.points[3]];
				Fplane	p012;	p012.build(p0,p1,p2);
				Fplane	p123;	p123.build(p1,p2,p3);
				Fplane	p230;	p230.build(p2,p3,p0);
				Fplane	p301;	p301.build(p3,p0,p1);
				VERIFY	(p012.n.similar(p123.n) && p012.n.similar(p230.n) && p012.n.similar(p301.n));
			}
			*/
		}
	}
	void				compute_caster_model	(xr_vector<Fplane>& dest, Fvector3 direction)
	{
		CRenderTarget&	T	= *RImplementation.Target;

		// COG
		Fvector3	cog	= {0,0,0};
		for			(int it=0; it<int(points.size()); it++)	cog.add	(points[it]);
		cog.div		(float(points.size()));

		// planes
		compute_planes	();
		for (int it=0; it<int(polys.size()); it++)
		{
			_poly&	base				= polys	[it];
			if (base.classify(cog)>0)	std::reverse(base.points.begin(),base.points.end());
		}

		// remove faceforward polys, build list of edges -> find open ones
		compute_planes	();
		for (int it=0; it<int(polys.size()); it++)
		{
			_poly&	base		= polys	[it];
			VERIFY	(base.classify(cog)<0);								// debug

			int		marker		= (base.planeN.dotproduct(direction)<=0)?-1:1;

			// register edges
			xr_vector<int>&	plist		= polys[it].points;
			for (int p=0; p<int(plist.size()); p++)	{
				_edge	E		(plist[p],plist[ (p+1)%plist.size() ], marker);
				bool	found	= false;
				for (int e=0; e<int(edges.size()); e++)	
					if (edges[e].equal(E))	{ edges[e].counter += marker; found=true; break; }
					if		(!found)	{
						edges.push_back	(E);
						if	(_debug)	T.dbg_addline(points[E.p0],points[E.p1],color_rgba(255,0,0,255));
					}
			}

			// remove if unused
			if (marker<0)	{
				polys.erase	(polys.begin()+it);
				it--;
			}
		}

		// Extend model to infinity, the volume is not capped, so this is indeed up to infinity
		for (int e=0; e<int(edges.size()); e++)
		{
			if	(edges[e].counter != 0)	continue;
			_edge&		E		= edges[e];
			if		(_debug)	T.dbg_addline(points[E.p0],points[E.p1],color_rgba(255,255,255,255));
			Fvector3	point;
			points.push_back	(point.sub(points[E.p0],direction));
			points.push_back	(point.sub(points[E.p1],direction));
			polys.push_back		(_poly());
			_poly&		P		= polys.back();	
			int			pend	= int(points.size());
			P.points.push_back	(E.p0);
			P.points.push_back	(E.p1);
			P.points.push_back	(pend-1);	//p1 mod
			P.points.push_back	(pend-2);	//p0 mod
			if		(_debug)	T.dbg_addline(points[E.p0],point.mad(points[E.p0],direction,-1000),color_rgba(0,255,0,255));
			if		(_debug)	T.dbg_addline(points[E.p1],point.mad(points[E.p1],direction,-1000),color_rgba(0,255,0,255));
		}

		// Reorient planes (try to write more inefficient code :)
		compute_planes	();
		for (int it=0; it<int(polys.size()); it++)
		{
			_poly&	base				= polys	[it];
			if (base.classify(cog)>0)	std::reverse(base.points.begin(),base.points.end());
		}

		// Export
		compute_planes	();
		for (int it=0; it<int(polys.size()); it++)
		{
			_poly&			P	= polys[it];
			Fplane			pp	= {P.planeN,P.planeD};
			dest.push_back	(pp);
		}
	}
};

#endif	//	r3_R_sun_support_included