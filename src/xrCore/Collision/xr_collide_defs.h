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
	public:
		Fmatrix xform{Fmatrix::EIdentity::Identity};
	private:
		const void* ptr = nullptr;
		
		ICF rq_result& set(const Fmatrix& _xform, const void* _O, float _range, int _element, bool isStatic)
		{
			xform = _xform;
			ptr = _O;
			range = _range;
			element	= _element;
			is_static = isStatic;
			return *this;
		}
		
	public:
		float range = 0.f;
		int element:31 = -1;
	private:
		int is_static:1 = false;
	public:
		
		ICF rq_result& reset()
		{
			xform.identity();
			ptr = nullptr;
			return *this;
		}
		
		ICF rq_result& set(const Fmatrix& _xform, const CObject& _O, float _range, int _element)
		{
			xform = _xform;
			ptr = &_O;
			range = _range;
			element	= _element;
			is_static = false;
			return *this;
		}
		
		ICF rq_result& set(const Fmatrix& _xform, const CDB::MODEL& _O, float _range, int _element)
		{
			xform = _xform;
			ptr = &_O;
			range = _range;
			element	= _element;
			is_static = true;
			return *this;
		}
		
		ICF bool IsStatic() const { return is_static; }
		
		ICF const CObject* GetDynamic() const
		{
			if (!IVERIFY(!IsStatic()))
			{
				return nullptr;
			} 
			return (CObject*)ptr;
		}
		
		ICF const CDB::MODEL* GetStatic() const
		{
			if (!IVERIFY(IsStatic()))
			{
				return nullptr;
			} 
			return (CDB::MODEL*)ptr;
		}
		
		ICF bool set_if_less(const CDB::RESULT& I)
		{
			if (I.range<range)
			{
				set(I.ModelWorldTransform, *I.model,I.range,I.tris_id); 
				return true;
			}
			return false;
		}
		
		ICF bool set_if_less(const rq_result&	R)
		{
			if (R.range<range)
			{
				set(R.xform, R.ptr,R.range,R.element,R.is_static); 
				return true;
			}
			return false;
		}
		ICF bool set_if_less(const Fmatrix& _xform, const CObject& _who, float _range, int _element)
		{
			if (_range<range)
			{
				set(_xform, _who,_range,_element); 
				return true;
			}
			return false;
		}
		ICF bool set_if_less(const Fmatrix& _xform, const CDB::MODEL& _who, float _range, int _element)
		{
			if (_range<range)
			{
				set(_xform, _who,_range,_element); 
				return true;
			}
			return false;
		}
		ICF bool valid() const {return ptr;}
	};

	using rqVec = xr_vector<rq_result>;
	using rqIt = rqVec::iterator;

	struct rq_results
	{
		rqVec results;
		ICF bool append_result(const Fmatrix& _xform, const CObject& _who, float _range, int _element, bool bNearest)
		{
			if (bNearest && !results.empty())
			{
				auto& R = results.back();
				return R.set_if_less(_xform,_who,_range,_element);
			}
			rq_result& rq = results.emplace_back();
			rq.set(_xform,_who,_range,_element);
			return true	;
		}
		ICF bool append_result(const Fmatrix& _xform, const CDB::MODEL& _who, float _range, int _element, bool bNearest)
		{
			if (bNearest && !results.empty())
			{
				auto& R = results.back();
				return R.set_if_less(_xform,_who,_range,_element);
			}
			rq_result& rq = results.emplace_back();
			rq.set(_xform,_who,_range,_element);
			return true	;
		}
		ICF void append_result(const rq_result& res)
		{
			if (!results.capacity())
			{
				results.reserve(8);
			}
			results.push_back(res);
		}
		ICF int r_count() const { return (int)results.size();	}
		ICF rq_result& r_any() { VERIFY(r_count()); return results[0];}
		ICF const rq_result& r_any() const { VERIFY(r_count()); return results[0];}
		ICF void r_clear() { results.resize(0);	}
		ICF void r_sort() { std::ranges::sort(results,[](const rq_result& a, const rq_result& b) { return a.range < b.range; }); }
		ICF rqVec& r_results() { return results; }
		ICF const rqVec& r_results() const { return results; }

	};
	typedef bool rq_callback(const rq_result& result, LPVOID user_data);
	typedef bool test_callback(const ray_defs& rd, CObject* object, LPVOID user_data);
};