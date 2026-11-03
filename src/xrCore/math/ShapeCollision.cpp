#include "stdafx.h"

#include "ShapeCollisionShapes.h"

//Every supported pair is instantiated here, so a change in one of the shapes can not
//silently break a pair that the game does not use yet.
#define INSTANTIATE_SHAPE_COLLISION(First, Second) \
	template ShapeCollision::EOverlap ShapeCollision::ComputeManifold<First, Second, f32>( \
		const First&, const Second&, ShapeCollision::Manifold<f32>&, f32);

INSTANTIATE_SHAPE_COLLISION(Fbox, Fbox)
INSTANTIATE_SHAPE_COLLISION(Fbox, Fobb)
INSTANTIATE_SHAPE_COLLISION(Fbox, Fsphere)
INSTANTIATE_SHAPE_COLLISION(Fbox, Fcylinder)
INSTANTIATE_SHAPE_COLLISION(Fbox, Fcone)

INSTANTIATE_SHAPE_COLLISION(Fobb, Fbox)
INSTANTIATE_SHAPE_COLLISION(Fobb, Fobb)
INSTANTIATE_SHAPE_COLLISION(Fobb, Fsphere)
INSTANTIATE_SHAPE_COLLISION(Fobb, Fcylinder)
INSTANTIATE_SHAPE_COLLISION(Fobb, Fcone)

INSTANTIATE_SHAPE_COLLISION(Fsphere, Fbox)
INSTANTIATE_SHAPE_COLLISION(Fsphere, Fobb)
INSTANTIATE_SHAPE_COLLISION(Fsphere, Fsphere)
INSTANTIATE_SHAPE_COLLISION(Fsphere, Fcylinder)
INSTANTIATE_SHAPE_COLLISION(Fsphere, Fcone)

INSTANTIATE_SHAPE_COLLISION(Fcylinder, Fbox)
INSTANTIATE_SHAPE_COLLISION(Fcylinder, Fobb)
INSTANTIATE_SHAPE_COLLISION(Fcylinder, Fsphere)
INSTANTIATE_SHAPE_COLLISION(Fcylinder, Fcylinder)
INSTANTIATE_SHAPE_COLLISION(Fcylinder, Fcone)

INSTANTIATE_SHAPE_COLLISION(Fcone, Fbox)
INSTANTIATE_SHAPE_COLLISION(Fcone, Fobb)
INSTANTIATE_SHAPE_COLLISION(Fcone, Fsphere)
INSTANTIATE_SHAPE_COLLISION(Fcone, Fcylinder)
INSTANTIATE_SHAPE_COLLISION(Fcone, Fcone)

#undef INSTANTIATE_SHAPE_COLLISION
