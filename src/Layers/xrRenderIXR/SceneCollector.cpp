/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "GpuSceneInternal.h"
#include "ParticleGroup.h"
#include "ParticleEffect.h"

namespace GpuSceneInternal
{
SceneCollector Collector;
} // namespace GpuSceneInternal

// The light a draw hands an object (CRender::apply_object) is smoothed here, with the collect: the draw only reads it
void SceneCollector::set_Object(IRenderable* NewObject)
{
	Object = NewObject;

	if (CROS_impl* Track = NewObject ? static_cast<CROS_impl*>(NewObject->renderable_ROS()) : nullptr)
	{
		Track->update_smooth(NewObject);
	}
}

void SceneCollector::Begin(const GraphView& NewView, const bool (&NewMask)[3], R_dsgraph_structure* NewGraph)
{
	View = NewView;
	std::copy(std::begin(NewMask), std::end(NewMask), Mask);
	Graph = NewGraph;
	Object = nullptr;
	Transform = nullptr;
	IsHud = false;
	IsInvisible = false;
	Id = SCENE_OBJECT_NONE;
	Visuals.clear();
}

void SceneCollector::add_Dynamic(IRenderVisual* Visual, Fmatrix* Xform, bool IgnoreOptimize, bool Force)
{
	Transform = Xform;
	RImplementation.Skinning.Mark(PCKinematics(static_cast<dxRender_Visual*>(Visual)), *Xform);
	Add(static_cast<dxRender_Visual*>(Visual), IgnoreOptimize, Force);
}

float SceneCollector::SsaOf(const Fvector& Centre, float Radius, float& Distance) const
{
	Distance = (IsHud ? Centre.square_magnitude() : View.CameraPosition.distance_to_sqr(Centre)) + EPS;
	return Radius / Distance;
}

void SceneCollector::Add(dxRender_Visual* Visual, bool IgnoreOptimize, bool Force)
{
	if (!Visual || !(IgnoreOptimize || View.FullDetail || IsHud || IsMovingValuable(*Visual, *Transform, View.CameraPosition, View.Fov)))
	{
		return;
	}

	Fvector Centre;

	switch (Visual->Type)
	{
	case MT_PARTICLE_GROUP:
	{
		PS::CParticleGroup* Group = static_cast<PS::CParticleGroup*>(Visual);
		xrCriticalSectionGuard Guard(Group->onframe_lock);

		for (PS::CParticleGroup::SItem& Item : Group->items)
		{
			if (Item.root_effect)
			{
				Add(Item.root_effect, false, false);
			}

			xrCriticalSectionGuard ChildGuard(Item.childs_cs);

			for (dxRender_Visual* Effect : Item.children_related)
			{
				Add(Effect, false, false);
			}

			for (dxRender_Visual* Effect : Item.children_free)
			{
				Add(Effect, false, false);
			}
		}

		return;
	}
	case MT_SKELETON_ANIM:
	case MT_SKELETON_RIGID:
	{
		// A moving object never fills its whole sphere
		CKinematics* Skeleton = static_cast<CKinematics*>(Visual);
		float Distance;
		Transform->transform_tiny(Centre, Skeleton->vis.sphere.P);

		if (Skeleton->m_lod && SsaOf(Centre, Skeleton->vis.sphere.R / 2.f, Distance) < View.SsaLodA)
		{
			Add(Skeleton->m_lod, IgnoreOptimize, false);
			return;
		}
	}
		[[fallthrough]];
	case MT_HIERRARHY:
		for (dxRender_Visual* Child : static_cast<FHierrarhyVisual*>(Visual)->children)
		{
			Add(Child, IgnoreOptimize, false);
		}

		return;
	default:
		Transform->transform_tiny(Centre, Visual->vis.sphere.P);
		Insert(Visual, Centre, Force);
	}
}

void SceneCollector::Insert(dxRender_Visual* Visual, const Fvector& Centre, bool Force)
{
	if (!Visuals.insert(Visual).second)
	{
		return;
	}

	MovingLeaf Leaf = { Visual, Object, *Transform, 0.f, 0.f, Id, IsHud };
	Leaf.Ssa = SsaOf(Centre, Visual->vis.sphere.R, Leaf.Distance);

	if ((Leaf.Ssa <= View.SsaDiscard && !Force) || !Visual->shader._get())
	{
		return;
	}

	IParticleCustom* Particle = Visual->dcast_ParticleCustom();
	Leaf.IsHud = IsHud || (Particle && Particle->GetHudMode());

	if (View.Phase == CRender::PHASE_SMAP)
	{
		AddShadowLeaf(Leaf, IsInvisible, Mask);
		return;
	}

	AddMainLeaf(*Graph, Leaf, IsInvisible, View.Distortion, Mask);
}
