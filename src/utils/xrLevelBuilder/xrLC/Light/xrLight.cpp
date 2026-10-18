#include "StdAfx.h"
#include "../Build/Build.h"

#include "../../UI/CompilersUI.h"
#include "xrDeflector.h"
#include "../Build/xrLC_GlobalData.h"
#include "xrLightVertex.h"
#include "../Mesh/xrFace.h"

#include "../MUModels/mu_model_light.h"
#include "light_point.h"

#include "LightingBackend.h"

extern XRCORE_API bool			g_bEnableStatGather;

void	CBuild::LMaps					()
{
	g_bEnableStatGather = true;

	UpdateCurrentPhase("LMaps");

	GetLightingBackend().LightDeflectors();
	GetLightingBackend().OnStageFinished(ELightingStage::Deflectors);

	xrPhase_MergeLM();
}
 
void CBuild::Light()
{
	auto InitModel = [this]()
	{
		InitializeEmbreeDevice();
		GetLightingBackend().InitializeLevel();
	};

	auto BuildingUV = [this]()
	{
		CalcNormals();
 
  		xrPhase_TangentBasis();

		Phase("Building UV...");
		//****************************************** Resolve materials
		xrPhase_ResolveMaterials();
		IsolateVertices(true);

		//****************************************** UV mapping
		xrPhase_UVmap();
		IsolateVertices(true);

		//****************************************** Subdivide geometry
		xrPhase_Subdivide();
		IsolateVertices(true);
	};

	// Hemi MT - Calculate
	Light_prepare();

	// ***************************************** Computing UV
	BuildingUV();
	InitModel();
  
	//****************************************** AdaptiveHT ùùùùùù
	xrPhase_AdaptiveHT_calculate();		
	
	//****************************************** Implicit
	ImplicitLighting();
	  
	//****************************************** LMAPS
	LMaps();

	//****************************************** Vertex
	LightVertex();
	GetLightingBackend().OnStageFinished(ELightingStage::Vertices);

	//****************************************** Starting MU
	run_mu_light();
	GetLightingBackend().OnStageFinished(ELightingStage::MUModels);

	//****************************************** Merge geometry
	Phase("Merging geometry...");
 	xrPhase_MergeGeometry();

	// Unloading Ray Casting Models !
	EmbreeMain.IntelEmbereUnloadAll();
	GetLightingBackend().ReleaseLevel();
}

void CBuild::LightVertex	()
{
	::LightVertex();
}