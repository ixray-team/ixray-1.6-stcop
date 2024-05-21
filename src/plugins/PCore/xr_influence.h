#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_INFLUENCE_H__
#define __XR_INFLUENCE_H__

#include <algorithm>
#include "xr_fixed_vector.h"
#include "xr_skeleton.h"

namespace xray_re {

template<typename Tb, typename Tw> struct _bone_weight {
			_bone_weight();
			_bone_weight(Tb _bone, Tw _weight);
	bool		operator==(const _bone_weight<Tb, Tw>& right) const;
	bool		operator<(const _bone_weight<Tb, Tw>& right) const;
	Tb		bone;
	Tw		weight;
};

typedef _bone_weight<uint32_t, float> fbone_weight;

// up to 2 influences in SoC, and up to 4 in CS.
template<typename Tb, typename Tw> struct _influence: public _svector<_bone_weight<Tb, Tw>, 4> {
	void		set(uint32_t bone0);
	void		set(uint16_t bone0, uint16_t bone1, float weight0);
	void		set_wo_reorder(uint16_t bone0, uint16_t bone1, float weight0);
	void		set(size_t num_bones, const uint16_t* bones, const float* weights);
	void		reorder();
};

typedef _influence<uint32_t, float> finfluence;

template<typename Tb, typename Tw> inline _bone_weight<Tb, Tw>::_bone_weight():
	bone(0), weight(Tw(1)) {}

template<typename Tb, typename Tw> inline _bone_weight<Tb, Tw>::_bone_weight(Tb _bone, Tw _weight):
	bone(_bone), weight(_weight) {}

template<typename Tb, typename Tw> inline
bool _bone_weight<Tb, Tw>::operator==(const _bone_weight<Tb, Tw>& right) const
{
	return bone == right.bone && weight == right.weight;
}

template<typename Tb, typename Tw> inline
bool _bone_weight<Tb, Tw>::operator<(const _bone_weight<Tb, Tw>& right) const
{
	return bone < right.bone || (bone == right.bone && weight < right.weight);
}

template<typename Tb, typename Tw> inline void _influence<Tb, Tw>::set(uint32_t bone0)
{
	this->push_back(_bone_weight<Tb, Tw>(bone0, Tw(1)));
}

template<typename Tb, typename Tw>
void _influence<Tb, Tw>::set(uint16_t bone0, uint16_t bone1, float weight0)
{
#if 0
	assert(weight0 >= 0.f && weight0 <= 1.f);
	assert(bone0 < MAX_BONES && bone1 < MAX_BONES);
#endif
	if (bone0 == bone1) {
		this->push_back(_bone_weight<Tb, Tw>(bone0, Tw(1)));
	} else {
		_bone_weight<Tb, Tw> bw0(bone0, Tw(1.f - weight0));
		_bone_weight<Tb, Tw> bw1(bone1, Tw(weight0));
#if 0
		xr_assert(weight0 != 0);
		xr_assert(1.f != weight0);
		xr_assert(!equivalent<Tw>(weight0, 0));
		xr_assert(!equivalent<Tw>(1.f - weight0, 0));
#endif
		if (bw0 < bw1) {
			this->push_back(bw0);
			this->push_back(bw1);
		} else {
			this->push_back(bw1);
			this->push_back(bw0);
		}
	}
}

template<typename Tb, typename Tw>
void _influence<Tb, Tw>::set_wo_reorder(uint16_t bone0, uint16_t bone1, float weight0)
{
	if (bone0 == bone1) {
		this->push_back(_bone_weight<Tb, Tw>(bone0, Tw(1)));
	} else {
		_bone_weight<Tb, Tw> bw0(bone0, Tw(1.f - weight0));
		_bone_weight<Tb, Tw> bw1(bone1, Tw(weight0));
		this->push_back(bw0);
		this->push_back(bw1);
	}
}

template<typename Tb, typename Tw>
void _influence<Tb, Tw>::set(size_t num_bones, const uint16_t* bones, const float* weights)
{
	for (float sum = 0, w; num_bones;) {
		if (--num_bones == 0) {
			w = 1.f - sum;
		} else {
			w = *weights++;
			sum += w;
		}
		_bone_weight<Tb, Tw> bw(*bones++, w);
		for (typename _influence<Tb, Tw>::iterator it = _influence<Tb, Tw>::begin(),
				last = _influence<Tb, Tw>::end(); it != last; ++it) {
			if (it->bone == bw.bone) {
#if 0
				if (!equivalent<Tw>(w, 0)) {
					msg("bone%u, w=%f", bw.bone, w);
				}
				xr_assert(equivalent<Tw>(w, 0));
#endif
				goto skip;
			}
		}
		this->push_back(bw);
skip:;
	}
	reorder();
}

template<typename Tb, typename Tw> void _influence<Tb, Tw>::reorder()
{
	std::sort(_influence<Tb, Tw>::begin(), _influence<Tb, Tw>::end());
}

} // end of namespace xray_re

#endif
