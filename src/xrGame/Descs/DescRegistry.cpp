#include "StdAfx.h"
#include "DescRegistry.h"
#include "HudItemDesc.h"
#include "DetectorDesc.h"
#include "OutfitDesc.h"
#include "BottleItemDesc.h"
#include "EatableItemDesc.h"
#include "EatableEffectsDesc.h"
#include "InventoryItemDesc.h"
#include "ArtefactDesc.h"

void ClearDescRegistries()
{
	SHudItemDesc::Registry::Clear();
	SCustomDetectorDesc::Registry::Clear();
	SEliteDetectorDesc::Registry::Clear();
	SScientificDetectorDesc::Registry::Clear();
	SCustomOutfitDesc::Registry::Clear();
	SBottleItemDesc::Registry::Clear();
	SEatableItemDesc::Registry::Clear();
	SEatableEffectsDesc::Registry::Clear();
	SInventoryItemDesc::Registry::Clear();
	SArtefactDesc::Registry::Clear();
}
