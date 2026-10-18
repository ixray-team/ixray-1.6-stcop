////////////////////////////////////////////////////////////////////////////
//	Module 		: space_restriction_shape.h
//	Created 	: 17.08.2004
//  Modified 	: 27.08.2004
//	Author		: Dmitriy Iassenev
//	Description : Space restriction shape
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "space_restriction_base.h"
#include "../xrEngine/xr_collide_form.h"

class CSpaceRestrictor;

class CSpaceRestrictionShape : public CSpaceRestrictionBase {
private:
	friend struct CBorderMergePredicate;

public:
	using CSpaceRestrictionBase::inside;

protected:
	CSpaceRestrictor		*m_restrictor;
	bool					m_default;
	xrCriticalSection		InitLock;

protected:
	IC			Fvector		position				(const CCF_Shape::shape_def &data) const;
	IC			float		radius					(const CCF_Shape::shape_def &data) const;
				void		build_border			();
				void		fill_shape				(const CCF_Shape::shape_def &shape);

public:
	IC						CSpaceRestrictionShape	(CSpaceRestrictor *space_restrictor, bool default_restrictor);
	virtual IC void		initialize				() override;
	virtual bool		inside					(const Fsphere &sphere) override;
	virtual shared_str	name					() const override;
	virtual IC bool		shape					() const override;
	virtual IC bool		default_restrictor		() const override;
	virtual Fsphere		sphere					() const override;
#ifdef DEBUG
				void		test_correctness		();
#endif
};

#include "space_restriction_shape_inline.h"
