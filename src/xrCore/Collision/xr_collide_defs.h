#pragma once

#include "xrCDB.h"

class CObject;
namespace collide 
{
	struct ray_cache
	{
		// previous state
		Fvector verts[3];
		Fvector start;
		Fvector dir;
		float range = 0.f;
		bool result = false;

		// cached vertices
		ICF void set(const Fvector& _start, const Fvector& _dir, const float _range,const bool _result)
		{
			start	= _start;
			dir		= _dir;
			range	= _range;
			result	= _result;
		}
		ICF bool similar(const Fvector& _start, const Fvector& _dir, const float _range)
		{
			if (!_start.similar(start)) return false;
			if (!fsimilar(1.f,dir.dotproduct(_dir))) return false;
			if (!fsimilar(_range,range)) return false;
			return true;
		}
	};
	enum rq_target
	{
		rqtNone		= (0),
		rqtObject	= (1<<0),
		rqtStatic	= (1<<1),
		rqtShape	= (1<<2),
		rqtObstacle	= (1<<3),
		rqtBoth		= (rqtObject|rqtStatic),
		rqtDyn		= (rqtObject|rqtShape|rqtObstacle)
	};
	struct ray_defs
	{
		Fvector start;
		Fvector dir;
		float range = 0.f;
		u32 flags{0u};
		rq_target tgt=rqtNone;
		ICF ray_defs(const Fvector& _start, const Fvector& _dir, float _range, u32 _flags, rq_target _tgt) :
		start(_start),dir(_dir),range(_range),flags(_flags),tgt(_tgt){}

		IC ray_defs(const Fvector& _start, const Fvector& _end, u32 _flags, rq_target _tgt)
		{
			start	= _start;
			Fvector temp = _end;
			dir		= temp.sub(_start).normalize();
			range	= _start.distance_to(_end);
			flags	= _flags;
			tgt		= _tgt;
		}
	};
	struct rq_result 
	{
		CObject* O = nullptr; // if NULL - static
		float range = 0.f; // range to intersection
		int element = -1; // номер кости/номер треугольника
		ICF rq_result& set(CObject* _O, float _range, int _element)
		{
			O = _O;
			range = _range;
			element	= _element;
			return *this;
		}
		ICF bool set_if_less(CDB::RESULT* I){if (I->range<range){ set(nullptr,I->range,I->id); return true;}else return false;}
		ICF bool set_if_less(rq_result*	R){if (R->range<range){ set(R->O,R->range,R->element); return true;}else return false;}
		ICF bool set_if_less(CObject* _who, float _range, int _element)	{ if (_range<range) { set(_who,_range,_element); return true;}else return false;}
		ICF bool valid() {return (element>=0);}
	};

	using rqVec = xr_vector<rq_result>;
	using rqIt = rqVec::iterator;

	struct rq_results
	{
		rqVec results;
		ICF bool append_result(CObject* _who, float _range, int _element, bool bNearest)
		{
			if (bNearest&&!results.empty()){
				rq_result& R		= results.back();
				if (_range<R.range){
					R.O				=_who;
					R.range			=_range;
					R.element		=_element;
					return			true;
				}
				return				false;
			}
			results.push_back		(rq_result());
			rq_result& rq			= results.back();
			rq.range	=_range;
			rq.element	=_element;
			rq.O		=_who;
			return true	;
		}
		ICF void		append_result	(rq_result& res)
		{
			if (0==results.capacity())	results.reserve(8);
			results.push_back			(res);
		}
		ICF int r_count() { return (int)results.size();	}
		ICF rq_result* r_begin() { return &*results.begin();	}
		ICF rq_result* r_end() { return &*results.end();	}
		ICF void r_clear() { results.resize(0);	}
		ICF void r_sort() { std::sort(results.begin(),results.end(),[](const rq_result& a, const rq_result& b) { return a.range < b.range; }); }
		ICF rqVec &r_results() { return results; }

	};
	typedef bool rq_callback(rq_result& result, LPVOID user_data);
	typedef bool test_callback(const ray_defs& rd, CObject* object, LPVOID user_data);
};