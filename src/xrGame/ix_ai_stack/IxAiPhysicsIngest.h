#pragma once

#include "../../xrCore/_types.h"
#include "../../xrCore/_vector3d.h"

struct dContact;
struct SGameMtl;

void IxAiStackIngestBoltImpact(const Fvector& position, u16 sourceObjectId, f32 linearSpeed);

void IxAiBoltContactCallback(bool& do_collide, bool bo1, dContact& c, SGameMtl* material_1, SGameMtl* material_2);
