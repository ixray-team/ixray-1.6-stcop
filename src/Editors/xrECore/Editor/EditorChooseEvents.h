#ifndef EditorChooseEventsH
#define EditorChooseEventsH
#include "SoundManager.h"
#include "Library.h"
#include "../../xrEngine/GameMtlLib.h"
#include "../../xrEngine/LightAnimLibrary.h"
#include "../../Layers/xrRender/SkeletonAnimated.h"
#include "../../Layers/xrRender/ResourceManager.h"
#include "../../Layers/xrRender/ParticleEffect.h"
#include "../../Layers/xrRender/ParticleGroup.h"
#include "../../Layers/xrRender/ParticleAnimCurve.h"
#include "../../xrEngine/defines.h"
#include "EditObject.h"
#include "EditorRenderBackend.h"
ref_sound* choose_snd;

namespace ChoseEvents
{
void ReleaseChooseTexture(SChooseTexture& Texture)
{
	GUIManager->DestroyEditorTexture(Texture.Editor);
	Texture.Revision = 0;
}

void UpdateImageThumbnail(EImageThumbnail& Thumbnail, SChooseTexture& Texture, const char* DebugName)
{
	if (!Thumbnail.Valid())
	{
		ReleaseChooseTexture(Texture);
		return;
	}

	xr_vector<std::byte> Flipped(
		static_cast<std::size_t>(THUMB_WIDTH) * THUMB_HEIGHT * 4
	);
	const auto* Source = reinterpret_cast<const std::byte*>(Thumbnail.Pixels());
	constexpr std::size_t RowPitch = THUMB_WIDTH * 4;
	for (u32 Y = 0; Y < THUMB_HEIGHT; ++Y)
	{
		memcpy(Flipped.data() + static_cast<std::size_t>(Y) * RowPitch, Source + static_cast<std::size_t>(THUMB_HEIGHT - Y - 1) * RowPitch, RowPitch);
	}

	FEditorTextureUpload Upload;
	Upload.Width = THUMB_WIDTH;
	Upload.Height = THUMB_HEIGHT;
	Upload.RowPitch = static_cast<std::uint32_t>(RowPitch);
	Upload.Format = EEditorTextureFormat::Bgra8Unorm;
	Upload.Pixels = Flipped;
	Upload.Revision = ++Texture.Revision;
	Upload.DebugName = DebugName;
	(void)GUIManager->UpdateEditorTexture(Texture.Editor, Upload);
}

void FillEntity(ChooseItemVec& items, void* param)
{
	//.    AppendItem	   					(RPOINT_CHOOSE_NAME);
	CInifile::Root& sections = pSettings->sections();
	for (CInifile::Sect& sect : sections)
	{
		const char* val;
		if (sect.line_exist("$spawn", &val))
		{
			items.push_back(SChooseItem(*sect.Name, ""));
		}
	}
}
//---------------------------------------------------------------------------
void SelectSoundSource(SChooseItem* item, PropItemVec& info_items)
{
	choose_snd->stop();
	choose_snd->create(item->name.c_str(), st_Effect, sg_Undefined);
	choose_snd->play(nullptr, sm_2D);
	//    snd.pla
	/*
	//.
		ECustomThumbnail*& thm, ref_sound& snd,
		thm 		= new ESoundThumbnail(item->name.c_str());
	*/
}
void CloseSoundSource()
{
	choose_snd->destroy();
}
void FillSoundSource(ChooseItemVec& items, void* param)
{
	FS_FileSet lst;
	if (SndLib->GetGameSounds(lst))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->name.c_str(), ""));
		}
	}
}
//---------------------------------------------------------------------------
void FillSoundEnv(ChooseItemVec& items, void* param)
{
	AStringVec lst;
	if (SndLib->GetSoundEnvs(lst))
	{
		AStringIt it = lst.begin();
		AStringIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->c_str(), ""));
		}
	}
}
//---------------------------------------------------------------------------
void FillObject(ChooseItemVec& items, void* param)
{
	FS_FileSet lst;
	if (Lib.GetObjects(lst))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->name.c_str(), ""));
		}
	}
}
void SelectObject(SChooseItem* item, PropItemVec& info_items)
{
	EObjectThumbnail* thm = new EObjectThumbnail(*item->name);
	if (thm->Valid())
	{
		thm->FillInfo(info_items);
	}
	xr_delete(thm);
}
void UpdateObjectTHM(const char* name, SChooseTexture& Texture)
{
	EObjectThumbnail* thm = new EObjectThumbnail(name);
	UpdateImageThumbnail(*thm, Texture, "choose-object-thumbnail");
	xr_delete(thm);
}
//---------------------------------------------------------------------------
void FillGroup(ChooseItemVec& items, void* param)
{
	FS_FileSet lst;
	if (FS.file_list(lst, _groups_, FS_ListFiles | FS_ClampExt, "*.group"))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->name.c_str(), ""));
		}
	}
}
void SelectGroup(SChooseItem* item, PropItemVec& info_items)
{
	EGroupThumbnail* thm = new EGroupThumbnail(*item->name);
	if (thm->Valid())
	{
		thm->FillInfo(info_items);
	}
	xr_delete(thm);
}
void UpdateGroupTHM(const char* name, SChooseTexture& Texture)
{
	EGroupThumbnail* thm = new EGroupThumbnail(name);
	UpdateImageThumbnail(*thm, Texture, "choose-group-thumbnail");
	xr_delete(thm);
}
//---------------------------------------------------------------------------
void FillVisual(ChooseItemVec& items, void* param)
{
	FS_FileSet lst;
	if (FS.file_list(lst, _game_meshes_, FS_ListFiles | FS_ClampExt, "*.ogf"))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->name.c_str(), ""));
		}
	}
}
void SelectVisual(SChooseItem* item, PropItemVec& info_items)
{
	/*
	//.
		AnsiString fn					= ChangeFileExt(item->name.c_str(),".ogf");
		IRender_Visual* visual			= ::Render->model_Create(fn.c_str());
		if (visual){
			PHelper().CreateCaption	(info_items,	"Source",	*visual->desc.source_file?*visual->desc.source_file:"unknown");
			PHelper().CreateCaption	(info_items, 	"Creator N",*visual->desc.create_name?*visual->desc.create_name:"unknown");
			PHelper().CreateCaption	(info_items,	"Creator T",Trim(AnsiString(ctime(&visual->desc.create_time))).c_str());
			PHelper().CreateCaption	(info_items,	"Modif N",	*visual->desc.modif_name ?*visual->desc.modif_name :"unknown");
			PHelper().CreateCaption	(info_items,	"Modif T",	Trim(AnsiString(ctime(&visual->desc.modif_time))).c_str());
			PHelper().CreateCaption	(info_items,	"Build N",	*visual->desc.build_name ?*visual->desc.build_name :"unknown");
			PHelper().CreateCaption	(info_items,	"Build T",	Trim(AnsiString(ctime(&visual->desc.build_time))).c_str());
		}
		::Render->model_Delete(visual);
	*/
}
//---------------------------------------------------------------------------
void FillGameObjectMots(ChooseItemVec& items, void* param)
{
	FS_FileSet lst;
	if (FS.file_list(lst, _game_meshes_, FS_ListFiles | FS_ClampExt, "*.omf"))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->name.c_str(), ""));
		}
	}
}
void SelectGameObjectMots(SChooseItem* item, PropItemVec& info_items)
{
}
//---------------------------------------------------------------------------
void FillGameAnim(ChooseItemVec& items, void* param)
{
	FS_FileSet lst;
	if (FS.file_list(lst, "$game_anims$", FS_ListFiles, "*.anm,*.anms"))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->name.c_str(), ""));
		}
	}
}
//---------------------------------------------------------------------------
void FillLAnim(ChooseItemVec& items, void* param)
{
	LAItemVec& lst = LALib.Objects();
	LAItemIt it = lst.begin();
	LAItemIt _E = lst.end();
	for (; it != _E; it++)
	{
		items.push_back(SChooseItem(*(*it)->cName, ""));
	}
}

void UpdateLAnim(const char* Name, SChooseTexture& Texture)
{
	CLAItem* Item = LALib.FindItem(Name);
	if (!Item)
	{
		return;
	}

	xr_vector<u32> Pixels(THUMB_WIDTH * THUMB_HEIGHT);

	int Frame = 0;
	for (u32 Y = 0; Y < THUMB_HEIGHT; ++Y)
	{
		for (u32 X = 0; X < THUMB_WIDTH; ++X)
		{
			u32 Color = Item->CalculateRGB(EDevice->fTimeGlobal, Frame);
			Color = subst_alpha(Color, 0xFF); // фиксируем альфу
			Pixels[Y * THUMB_WIDTH + X] = Color;
		}
	}

	FEditorTextureUpload Upload;
	Upload.Width = THUMB_WIDTH;
	Upload.Height = THUMB_HEIGHT;
	Upload.RowPitch = THUMB_WIDTH * 4;
	Upload.Format = EEditorTextureFormat::Bgra8Unorm;
	Upload.Pixels = std::as_bytes(std::span(Pixels));
	Upload.Revision = ++Texture.Revision;
	Upload.DebugName = "choose-light-animation";
	(void)GUIManager->UpdateEditorTexture(Texture.Editor, Upload);
}

//---------------------------------------------------------------------------
void FillEShader(ChooseItemVec& items, void* param)
{
	if (!EDevice->Resources)
	{
		return;
	}
	CResourceManager::map_Blender& blenders = EDevice->Resources->_GetBlenders();
	CResourceManager::map_BlenderIt _S = blenders.begin();
	CResourceManager::map_BlenderIt _E = blenders.end();
	for (; _S != _E; _S++)
	{
		items.push_back(SChooseItem(_S->first, ""));
	}
}
//---------------------------------------------------------------------------
void FillCShader(ChooseItemVec& items, void* param)
{
	Shader_xrLCVec& shaders = EDevice->ShaderXRLC.Library();
	Shader_xrLCIt _F = shaders.begin();
	Shader_xrLCIt _E = shaders.end();
	for (; _F != _E; _F++)
	{
		items.push_back(SChooseItem(_F->Name, ""));
	}
}
//---------------------------------------------------------------------------
void FillPE(ChooseItemVec& items, void* param)
{
	FEditorParticleLibrarySnapshot Snapshot;
	GetEditorRenderBackend().CopyParticleLibrary(Snapshot);
	for (const FEditorParticleAssetInfo& Asset : Snapshot.Assets)
	{
		if (Asset.Type == EEditorParticleAssetType::Effect)
		{
			items.push_back(SChooseItem(Asset.Name.c_str(), "EFFECT"));
		}
	}
}
//---------------------------------------------------------------------------
void FillPAC(ChooseItemVec& items, void* param)
{
	FEditorParticleLibrarySnapshot Snapshot;
	GetEditorRenderBackend().CopyParticleLibrary(Snapshot);
	for (const FEditorParticleAssetInfo& Asset : Snapshot.Assets)
	{
		if (Asset.Type == EEditorParticleAssetType::AnimationCurve)
		{
			items.push_back(SChooseItem(Asset.Name.c_str(), "ANIM_CURVE"));
		}
	}
}
//---------------------------------------------------------------------------
void FillParticles(ChooseItemVec& items, void* param)
{
	FEditorParticleLibrarySnapshot Snapshot;
	GetEditorRenderBackend().CopyParticleLibrary(Snapshot);
	for (const FEditorParticleAssetInfo& Asset : Snapshot.Assets)
	{
		if (Asset.Type == EEditorParticleAssetType::Effect)
		{
			items.push_back(SChooseItem(Asset.Name.c_str(), "EFFECT"));
		}
		else if (Asset.Type == EEditorParticleAssetType::Group)
		{
			items.push_back(SChooseItem(Asset.Name.c_str(), "GROUP"));
		}
	}
}

void SelectPE(SChooseItem* item, PropItemVec& info_items)
{
	string64 str;
	u32 i = 0;
	PHelper().CreateCaption(info_items, "", "used in groups");
	FEditorParticleLibrarySnapshot Snapshot;
	GetEditorRenderBackend().CopyParticleLibrary(Snapshot);
	for (const FEditorParticleAssetInfo& Asset : Snapshot.Assets)
	{
		if (Asset.Type != EEditorParticleAssetType::Group)
		{
			continue;
		}
		if (std::ranges::find(Asset.Dependencies, item->name.c_str()) !=
			Asset.Dependencies.end())
		{
			xr_sprintf(str, sizeof(str), "%d", ++i);
			PHelper().CreateCaption(info_items, str, Asset.Name.c_str());
		}
	}
}

void SelectPG(SChooseItem* item, PropItemVec& info_items)
{
	string64 str;
	u32 i = 0;
	PHelper().CreateCaption(info_items, "", "using effects");
	FEditorParticleLibrarySnapshot Snapshot;
	GetEditorRenderBackend().CopyParticleLibrary(Snapshot);
	for (const FEditorParticleAssetInfo& Asset : Snapshot.Assets)
	{
		if (Asset.Type == EEditorParticleAssetType::Group &&
			Asset.Name == item->name.c_str())
		{
			for (const xr_string& Dependency : Asset.Dependencies)
			{
				xr_sprintf(str, sizeof(str), "%d", ++i);
				PHelper().CreateCaption(info_items, str, Dependency.c_str());
			}
			break;
		}
	}
}

//---------------------------------------------------------------------------
void FillTexture(ChooseItemVec& items, void* param)
{
	FS_FileSet lst;
	if (ImageLib.GetTextures(lst))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->name.c_str(), ""));
		}
	}
}

void UpdateTextureTHM(const char* name, SChooseTexture& Texture)
{
	if (name && name[0])
	{
		ETextureThumbnail* thm = new ETextureThumbnail(name);
		UpdateImageThumbnail(*thm, Texture, "choose-texture-thumbnail");
		xr_delete(thm);
	}
}

//---------------------------------------------------------------------------
void FillTextureRaw(ChooseItemVec& items, void* param)
{
	FS_FileSet lst;
	if (ImageLib.GetTexturesRaw(lst))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			items.push_back(SChooseItem(it->name.c_str(), ""));
		}
	}
}

void UpdateTextureTHMRaw(const char* name, SChooseTexture& Texture)
{
	if (name && name[0])
	{
		ETextureThumbnail* thm = new ETextureThumbnail(name);
		UpdateImageThumbnail(*thm, Texture, "choose-raw-texture-thumbnail");
		xr_delete(thm);
	}
}

void SelectTexture(SChooseItem* item, PropItemVec& info_items)
{
	if (item->name.size())
	{
		ETextureThumbnail* thm = new ETextureThumbnail(*item->name);
		if (thm->Valid())
		{
			thm->FillInfo(info_items);
		}
		xr_delete(thm);
	}
}
void SelectTextureRaw(SChooseItem* item, PropItemVec& info_items)
{
	if (item->name.size())
	{
		ETextureThumbnail* thm = new ETextureThumbnail(*item->name);
		if (thm->Valid())
		{
			thm->FillInfo(info_items);
		}
		xr_delete(thm);
	}
}
//---------------------------------------------------------------------------
void FillGameMaterial(ChooseItemVec& items, void* param)
{
	GameMtlIt _F = PGMLib->FirstMaterial();
	GameMtlIt _E = PGMLib->LastMaterial();
	for (; _F != _E; _F++)
	{
		items.push_back(SChooseItem(*(*_F)->m_Name, ""));
	}
}
//---------------------------------------------------------------------------

void FillSkeletonAnims(ChooseItemVec& items, void* param)
{
	IRenderVisual* V = ::Render->model_Create((const char*)param);
	if (PKinematicsAnimated(V))
	{
		u32 cnt = PKinematicsAnimated(V)->LL_MotionsSlotCount();
		for (u32 k = 0; k < cnt; k++)
		{
			accel_map* ll_motions = PKinematicsAnimated(V)->LL_Motions(k);
			accel_map::iterator _I, _E;
			_I = ll_motions->begin();
			_E = ll_motions->end();
			for (; _I != _E; ++_I)
			{
				bool bFound = false;
				for (ChooseItemVecIt it = items.begin(); it != items.end(); it++)
				{
					if (it->name == _I->first)
					{
						bFound = true;
						break;
					}
				}
				if (!bFound)
				{
					items.push_back(SChooseItem(*_I->first, ""));
				}
			}
		}
	}
	::Render->model_Delete(V);
}

void FillSkeletonBones(ChooseItemVec& items, void* param)
{
	IRenderVisual* V = ::Render->model_Create((const char*)param);
	if (PKinematics(V))
	{
		CKinematicsAnimated::accel* ll_bones = PKinematics(V)->LL_Bones();
		CKinematicsAnimated::accel::iterator _I, _E;
		_I = ll_bones->begin();
		_E = ll_bones->end();
		for (; _I != _E; ++_I)
		{
			items.push_back(SChooseItem(*_I->first, ""));
		}
	}
	::Render->model_Delete(V);
}

void FillSkeletonBonesObject(ChooseItemVec& items, void* param)
{
	CEditableObject* eo = (CEditableObject*)param;

	BoneIt _I = eo->FirstBone();
	BoneIt _E = eo->LastBone();
	for (; _I != _E; ++_I)
	{
		items.push_back(SChooseItem((*_I)->Name().c_str(), ""));
	}
}

} // namespace ChoseEvents

void FillChooseEvents()
{
	UIChooseForm::AppendEvents(smSoundSource, "Select Sound Source", ChoseEvents::FillSoundSource, ChoseEvents::SelectSoundSource, nullptr, ChoseEvents::CloseSoundSource, 0);
	UIChooseForm::AppendEvents(smSoundEnv, "Select Sound Environment", ChoseEvents::FillSoundEnv, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smObject, "Select Library Object", ChoseEvents::FillObject, ChoseEvents::SelectObject, ChoseEvents::UpdateObjectTHM, nullptr, 0);
	UIChooseForm::AppendEvents(smGroup, "Select Group", ChoseEvents::FillGroup, ChoseEvents::SelectGroup, ChoseEvents::UpdateGroupTHM, nullptr, 0);
	UIChooseForm::AppendEvents(smEShader, "Select Engine Shader", ChoseEvents::FillEShader, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smCShader, "Select Compiler Shader", ChoseEvents::FillCShader, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smPE, "Select Particle Effect", ChoseEvents::FillPE, nullptr /*ChoseEvents::SelectPE*/, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smParticles, "Select Particle System", ChoseEvents::FillParticles, nullptr /*ChoseEvents::SelectPG*/, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smTextureRaw, "Select Source Texture", ChoseEvents::FillTextureRaw, ChoseEvents::SelectTextureRaw, ChoseEvents::UpdateTextureTHMRaw, nullptr, SChooseEvents::flClearTexture);
	UIChooseForm::AppendEvents(smTexture, "Select Texture", ChoseEvents::FillTexture, ChoseEvents::SelectTexture, ChoseEvents::UpdateTextureTHM, nullptr, SChooseEvents::flClearTexture);
	UIChooseForm::AppendEvents(smEntityType, "Select Entity", ChoseEvents::FillEntity, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smLAnim, "Select Light Animation", ChoseEvents::FillLAnim, nullptr, ChoseEvents::UpdateLAnim, nullptr, SChooseEvents::flAnimated);
	UIChooseForm::AppendEvents(smVisual, "Select Visual", ChoseEvents::FillVisual, ChoseEvents::SelectVisual, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smSkeletonAnims, "Select Skeleton Animation", ChoseEvents::FillSkeletonAnims, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smSkeletonBones, "Select Skeleton Bones", ChoseEvents::FillSkeletonBones, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smSkeletonBonesInObject, "Select Skeleton Bones", ChoseEvents::FillSkeletonBonesObject, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smGameMaterial, "Select Game Material", ChoseEvents::FillGameMaterial, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smGameAnim, "Select Animation", ChoseEvents::FillGameAnim, nullptr, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smGameSMotions, "Select Game Object Motions", ChoseEvents::FillGameObjectMots, ChoseEvents::SelectGameObjectMots, nullptr, nullptr, 0);
	UIChooseForm::AppendEvents(smPAC, "Select Animation Curve", ChoseEvents::FillPAC, nullptr, nullptr, nullptr, 0);
	choose_snd = new ref_sound();
}

void ClearChooseEvents()
{
	UIChooseForm::ClearEvents();
	xr_delete(choose_snd);
}

//---------------------------------------------------------------------------
#endif
