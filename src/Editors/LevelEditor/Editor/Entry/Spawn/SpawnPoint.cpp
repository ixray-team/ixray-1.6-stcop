#include "stdafx.h"
#include "../../../../../xrServerEntities/xrServer_Objects_ALife.h"
#include "../xrServerEntities/xrServer_Objects_Abstract.h"
#include "../xrServerEntities/xrServer_Object_Base.h"
#include "../xrServerEntities/xrServer_Objects.h"

#define SPAWNPOINT_CHUNK_VERSION		0xE411
#define SPAWNPOINT_CHUNK_POSITION		0xE412
#define SPAWNPOINT_CHUNK_RPOINT			0xE413
#define SPAWNPOINT_CHUNK_DIRECTION		0xE414
#define SPAWNPOINT_CHUNK_SQUADID		0xE415
#define SPAWNPOINT_CHUNK_GROUPID		0xE416
#define SPAWNPOINT_CHUNK_TYPE			0xE417
#define SPAWNPOINT_CHUNK_FLAGS			0xE418

#define SPAWNPOINT_CHUNK_ENTITYREF		0xE419
#define SPAWNPOINT_CHUNK_SPAWNDATA		0xE420

#define SPAWNPOINT_CHUNK_ATTACHED_OBJ	0xE421

#define SPAWNPOINT_CHUNK_ENVMOD			0xE422
#define SPAWNPOINT_CHUNK_ENVMOD2		0xE423
#define SPAWNPOINT_CHUNK_ENVMOD3		0xE424
#define SPAWNPOINT_CHUNK_FLAGS			0xE425
#define SPAWNPOINT_CHUNK_ENVMOD4		0xE426


#define RPOINT_SIZE 0.5f
#define ENVMOD_SIZE 0.25f
#define MAX_TEAM 6
const u32 RP_COLORS[MAX_TEAM]={0xff0000,0x00ff00,0x0000ff,0xffff00,0x00ffff,0xff00ff};
#define ENVMOD_COLOR 0x00DF8E00
#define ENVMOD_SEL_COLOR1 0x30FFBE10
#define ENVMOD_SEL_COLOR2 0x00FFBE10
// CLE_Visual
CLE_Visual::CLE_Visual(CSE_Visual* src)
{
	source				= src;
	visual				= 0;
}

bool CLE_Visual::g_tmp_lock = false;

CLE_Visual::~CLE_Visual()
{
	::Render->model_Delete	(visual,TRUE);
}

void CLE_Visual::OnDrawUI()
{
	bool ok;
	shared_str name;
	if (UIChooseForm::GetResult(ok, name))
	{
		if (ok)
		{
			source->visual_name = name;
			visual = ::Render->model_Create(source->visual_name.c_str());
		}
		EDevice->seqDrawUI.Remove(this);
	}
	UIChooseForm::Update();
}

void CLE_Visual::OnChangeVisual	()
{
	::Render->model_Delete	(visual,TRUE);
	if (source->visual_name.size())
	{
		visual				= ::Render->model_Create(source->visual_name.c_str());

		if(NULL==visual && !g_tmp_lock)
		{
		 xr_string _msg = "Model [" + xr_string(source->visual_name.c_str())+"] not found. Do you want to select it from library?";
			  int mr = ELog.DlgMsg(mtConfirmation,mbYes |mbNo, _msg.c_str());
			  LPCSTR _new_val = 0;
			  g_tmp_lock = true;
			  if (mr == mrYes)
			  {
				  UIChooseForm::SelectItem(smVisual, 1);
				  EDevice->seqDrawUI.Add(this);
			  }

			  g_tmp_lock = false;

		}
		PlayAnimationFirstFrame		();
	}
	ExecCommand				(COMMAND_UPDATE_PROPERTIES);
}

void CLE_Visual::PlayAnimation ()
{
	 if(g_tmp_lock) 			return;
	// play motion if skeleton
	StopAllAnimations			();

	CKinematicsAnimated* KA = PKinematicsAnimated(visual);
	IKinematics*		K 	= PKinematics(visual);
	if (KA)
	{
		MotionID M 			= KA->ID_Cycle_Safe(source->startup_animation.c_str());
		if (M.valid())		
			KA->PlayCycle	(M);
	}
	if (K)
		K->CalculateBones();
}

void CLE_Visual::StopAllAnimations()
{
	 if(g_tmp_lock) return;
	// play motion if skeleton
	CKinematicsAnimated* KA = PKinematicsAnimated(visual);
	if (KA)
	{
		for (u16 i=0; i<MAX_PARTS; ++i)
		   KA->LL_CloseCycle(i, u8(-1));
	}
}

void CLE_Visual::PlayAnimationFirstFrame()
{
	 if(g_tmp_lock) return;
	// play motion if skeleton

	StopAllAnimations		();
	
	CKinematicsAnimated* KA = PKinematicsAnimated(visual);
	IKinematics*		K 	= PKinematics(visual);
	if (KA)
	{
		MotionID M 			= KA->ID_Cycle_Safe(source->startup_animation.c_str());
		if (M.valid())
		{		
			KA->PlayCycle	(M);
			PauseAnimation	();
		}else
		 Msg("! visual [%s] has no animation [%s]", source->visual_name.c_str(), source->startup_animation.c_str());
	}
	if (K)
		K->CalculateBones();
}
struct SetBlendLastFrameCB : public IterateBlendsCallback
{
	virtual	void	operator () ( CBlend &B )
	{
		B.timeCurrent = B.timeTotal-0.4444f;
		B.blendAmount = 1.0f;
		B.playing = !B.playing;
	}
} g_Set_blend_last_frame_CB;

void CLE_Visual::PlayAnimationLastFrame()
{
	 if(g_tmp_lock) return;
	// play motion if skeleton

	StopAllAnimations		();
	
	CKinematicsAnimated* KA = PKinematicsAnimated(visual);
	IKinematics*		K 	= PKinematics(visual);
	if (KA)
	{
		MotionID M 			= KA->ID_Cycle_Safe(source->startup_animation.c_str());
		if (M.valid())
		{		
			KA->PlayCycle		(M);
			KA->LL_IterateBlends(g_Set_blend_last_frame_CB);
		}
	}
	if (K)
		K->CalculateBones();
}

struct TogglelendCB : public IterateBlendsCallback
{
	virtual	void	operator () ( CBlend &B )
	{
		B.playing = !B.playing;
	}
} g_toggle_pause_blendCB;

void CLE_Visual::PauseAnimation ()
{
	 if(g_tmp_lock) return;
	 
	CKinematicsAnimated* KA = PKinematicsAnimated(visual);
	IKinematics*		K 	= PKinematics(visual);

	if (KA)
		KA->LL_IterateBlends(g_toggle_pause_blendCB);

	if (K)
		K->CalculateBones();
}

// CLE_Motion
CSpawnPoint::CLE_Motion::CLE_Motion	(CSE_Motion* src)
{
	source			= src;
	animator		= 0;
}
CSpawnPoint::CLE_Motion::~CLE_Motion()
{
}
void 	CSpawnPoint::CLE_Motion::OnChangeMotion	()
{
	xr_delete					(animator);
	if (source->motion_name.size()){
		animator				= new CObjectAnimator();
		animator->Load			(*source->motion_name);
		PlayMotion				();
	}
	ExecCommand					(COMMAND_UPDATE_PROPERTIES);
}
void CSpawnPoint::CLE_Motion::PlayMotion()
{
	// play motion if skeleton
	if (animator) animator->Play(true);
}
// SpawnData
void CSpawnPoint::SSpawnData::Create(LPCSTR _entity_ref)
{
	m_Data = g_SEFactoryManager->create_entity(_entity_ref);
	if (m_Data != nullptr)
	{
		m_Data->set_name(_entity_ref);
		if (m_Data->visual())
		{
			m_Visual = new CLE_Visual(m_Data->visual());
			m_Data->set_editor_flag(CSE_Abstract::flVisualChange | CSE_Abstract::flVisualAnimationChange);
		}
		if (m_Data->motion())
		{
			m_Motion = new CLE_Motion(m_Data->motion());
			m_Data->set_editor_flag(CSE_Abstract::flMotionChange);
		}
		if (pSettings->line_exist(_entity_ref, "$player"))
		{
			if (pSettings->r_bool(_entity_ref, "$player"))
			{
				m_Data->flags().set(M_SPAWN_OBJECT_ASPLAYER, TRUE);
			}
		}
		m_ClassID = pSettings->r_clsid(_entity_ref, "class");
	}
	else 
	{
		Log("!Can't create entity: ", _entity_ref);
	}

	m_owner->SetRenderIfSelected(pSettings->line_exist(_entity_ref, "$render_if_selected"));
}

void CSpawnPoint::SSpawnData::Destroy()
{
	g_SEFactoryManager->destroy_entity		(m_Data);
	xr_delete			(m_Visual);
	xr_delete			(m_Motion);
}
void CSpawnPoint::SSpawnData::get_bone_xform	(LPCSTR name, Fmatrix& xform)
{
	xform.identity		();
	if (name&&name[0]&&m_Visual&&m_Visual->visual){
		IKinematics* P 	= PKinematics(m_Visual->visual);
		if (P){
			u16 id 		= P->LL_BoneID(name);
			if (id!=BI_NONE)
				xform 	= P->LL_GetTransform(id);
		}    	
	}
}

bool CSpawnPoint::SSpawnData::LoadLTX	(CInifile& ini, LPCSTR sect_name)
{
	xr_string temp 		= ini.r_string		(sect_name, "name");
	Create				(temp.c_str());

	if(ini.line_exist(sect_name,"fl"))
		m_flags.assign		(ini.r_u8(sect_name,"fl"));
		
	NET_Packet 				Packet;
	SIniFileStream 			ini_stream;
	ini_stream.ini 			= &ini;
	ini_stream.sect 		= sect_name;
	ini_stream.move_begin	();
	Packet.inistream 		= &ini_stream;
	
	if (Valid())
		if (!m_Data->Spawn_Read(Packet))
			Destroy		();

	return Valid();
}

void CSpawnPoint::SSpawnData::SaveLTX	(CInifile& ini, LPCSTR sect_name)
{
	ini.w_string(sect_name, "name", m_Data->name());
	ini.w_u8			(sect_name,"fl", m_flags.get());
	
	NET_Packet 				Packet;
	SIniFileStream 			ini_stream;
	ini_stream.ini 			= &ini;
	ini_stream.sect 		= sect_name;
	ini_stream.move_begin	();
	Packet.inistream 		= &ini_stream;
	
	m_Data->Spawn_Write	(Packet,TRUE);
}

void CSpawnPoint::SSpawnData::SaveStream(IWriter& F)
{
	F.open_chunk		(SPAWNPOINT_CHUNK_ENTITYREF);
	F.w_stringZ			(m_Data->name());
	F.close_chunk		();

	F.open_chunk		(SPAWNPOINT_CHUNK_FLAGS);
	F.w_u8				(m_flags.get());
	F.close_chunk		();

	F.open_chunk		(SPAWNPOINT_CHUNK_SPAWNDATA);
	NET_Packet 			Packet;
	m_Data->Spawn_Write	(Packet,TRUE);
	F.w_u32				(Packet.B.count);
	F.w					(Packet.B.data,Packet.B.count);
	F.close_chunk		();
}

bool CSpawnPoint::SSpawnData::LoadStream(IReader& F)
{
	string64 			temp;
	R_ASSERT			(F.find_chunk(SPAWNPOINT_CHUNK_ENTITYREF));
	F.r_stringZ			(temp,sizeof(temp));

	if(F.find_chunk(SPAWNPOINT_CHUNK_FLAGS))
		m_flags.assign	(F.r_u8());

	NET_Packet 			Packet;
	R_ASSERT(F.find_chunk(SPAWNPOINT_CHUNK_SPAWNDATA));
	Packet.B.count 		= F.r_u32();
	F.r					(Packet.B.data,Packet.B.count);
	Create				(temp);
	if (Valid())
		if (!m_Data->Spawn_Read(Packet))
			Destroy		();

	return Valid();
}
bool CSpawnPoint::SSpawnData::ExportGame(SExportStreams* F, CSpawnPoint* owner)
{
	// set params
	m_Data->set_name_replace	(owner->GetName());
	m_Data->position().set		(owner->GetPosition());
	m_Data->angle().set			(owner->GetRotation());

	// export cform (if needed)
	ISE_Shape* cform 			= m_Data->shape();
// SHAPE
	if (cform&&!(owner->m_AttachedObject&&(owner->m_AttachedObject->FClassID==OBJCLASS_SHAPE))){
		ELog.DlgMsg				(mtError,"Spawn Point: '%s' must contain attached shape.",owner->GetName());
		return false;
	}
	if (cform){
		CEditShape* shape		= dynamic_cast<CEditShape*>(owner->m_AttachedObject); R_ASSERT(shape);
		shape->ApplyScale		();
		owner->SetScale( 			 shape->GetScale());
		cform->assign_shapes	(&*shape->GetShapes().begin(),shape->GetShapes().size());
	}
	// end

	NET_Packet					Packet;
	m_Data->Spawn_Write			(Packet,TRUE);

	SExportStreamItem& tgt 		= (m_flags.test(eSDTypeRespawn))? F->spawn_rs : F->spawn;
	tgt.stream.open_chunk		(tgt.chunk++);
	tgt.stream.w				(Packet.B.data,Packet.B.count);
	tgt.stream.close_chunk		();

	return true;
}

void CSpawnPoint::SSpawnData::ExportSpawn(xr_vector<NET_Packet>& Ps, CSpawnPoint* owner)
{
	m_Data->set_name_replace(owner->GetName());
	m_Data->position().set(owner->GetPosition());
	m_Data->angle().set(owner->GetRotation());
	Ps.push_back(NET_Packet());
	m_Data->Spawn_Write(Ps.back(), TRUE);
}

void CSpawnPoint::SSpawnData::PreExportSpawn(CSpawnPoint* owner)
{
	m_Data->set_name_replace(owner->GetName());
	m_Data->position().set(owner->GetPosition());
	m_Data->angle().set(owner->GetRotation());
	// export cform (if needed)
	ISE_Shape* cform = m_Data->shape();
	// SHAPE
	if (cform && !(owner->m_AttachedObject && (owner->m_AttachedObject->FClassID == OBJCLASS_SHAPE))) {
		ELog.DlgMsg(mtError, "Spawn Point: '%s' must contain attached shape.", owner->GetName());
		;
	}
	if (cform) {
		CEditShape* shape = dynamic_cast<CEditShape*>(owner->m_AttachedObject); R_ASSERT(shape);
		shape->ApplyScale();
		owner->SetScale(shape->GetScale());
		cform->assign_shapes(&*shape->GetShapes().begin(), shape->GetShapes().size());
	}
	// end
}

void CSpawnPoint::SSpawnData::OnAnimControlClick(ButtonValue* value, bool& bModif, bool& bSafe)
{
	ButtonValue* B				= dynamic_cast<ButtonValue*>(value); R_ASSERT(B);
	switch(B->btn_num)
	{
//		"First,Play,Pause,Stop,Last",
		case 0: //first
		{
			m_Visual->PlayAnimationFirstFrame();
		}break;
		case 1: //play
		{
			m_Visual->PlayAnimation();
		}break;
		case 2: //pause
		{
			m_Visual->PauseAnimation();
		}break;
		case 3: //stop
		{
			m_Visual->StopAllAnimations();
		}break;
		case 4: //last
		{
			m_Visual->PlayAnimationLastFrame();
		}break;
		
	}
}

void CSpawnPoint::SSpawnData::FillProp(LPCSTR pref, PropItemVec& items)
{
	xrCriticalSectionGuard guard(mLuaEnter);

	m_Data->FillProp			(pref,items);

	if(Scene->m_LevelOp.m_mapUsage.MatchType(eGameIDDeathmatch|eGameIDTeamDeathmatch|eGameIDArtefactHunt|eGameIDCaptureTheArtefact))
		PHelper().CreateFlag8		(items, PrepareKey(pref,"MP respawn"), &m_flags, eSDTypeRespawn);

   if(m_Visual)
   {
	ButtonValue* BV = PHelper().CreateButton	    (	items, 
									PrepareKey(pref,m_Data->name(),"Model\\AnimationControl"),
									"|<<,Play,Pause,Stop,>>|",
									0);
   BV->OnBtnClickEvent.bind			(this,&CSpawnPoint::SSpawnData::OnAnimControlClick);

   }
}

void CSpawnPoint::SSpawnData::Render(bool bSelected, const Fmatrix& parent,int priority, bool strictB2F)
{
	if (m_Visual&&m_Visual->visual)
		::RImplementation.model_Render	(m_Visual->visual,parent,priority,strictB2F,1.f);

	if (m_Motion&&m_Motion->animator&&bSelected&&(1==priority)&&(false==strictB2F))
		m_Motion->animator->DrawPath();

	RCache.set_xform_world		(Fidentity);
	EDevice->SetShader			(EDevice->m_WireShader);
	m_Data->on_render			(&DU_impl,this,bSelected,parent,priority,strictB2F);

	if(bSelected)
	{
		xr_vector<CLE_Visual*>::iterator it 	= m_VisualHelpers.begin();
		xr_vector<CLE_Visual*>::iterator it_e 	= m_VisualHelpers.end();
		Fmatrix M;
		u32 idx = 0;

		xrCriticalSectionGuard guard(mLuaEnter);

		for(;it!=it_e;++it,++idx)
		{
			visual_data* vc 		= m_Data->visual_collection()+idx;
			M.mul					(parent, vc->matrix);
			CLE_Visual* v 			= *it;
			::RImplementation.model_Render	(v->visual,M,priority,strictB2F,1.f);
		}
	}
}

void CSpawnPoint::SSpawnData::OnFrame()
{
	if (m_Data->m_editor_flags.is(CSE_Abstract::flUpdateProperties))
		ExecCommand				(COMMAND_UPDATE_PROPERTIES);
	// visual part
	if (m_Visual)
	{
		if (m_Data->m_editor_flags.is(CSE_Abstract::flVisualChange))
			m_Visual->OnChangeVisual();

		if(m_Data->m_editor_flags.is(CSE_Abstract::flVisualAnimationChange))
		{
			m_Visual->PlayAnimationFirstFrame();
			m_Data->m_editor_flags.set(CSE_Abstract::flVisualAnimationChange, FALSE);
		}

		if (m_Visual->visual&&PKinematics(m_Visual->visual))
			PKinematics			(m_Visual->visual)->CalculateBones(TRUE);
	}
	// motion part
	if (m_Motion)
	{
		if (m_Data->m_editor_flags.is(CSE_Abstract::flMotionChange))
			m_Motion->OnChangeMotion();
		if (m_Motion->animator)
			m_Motion->animator->Update(EDevice->fTimeDelta);
	}

	if (m_Data->m_editor_flags.is(CSE_Abstract::flVisualChange))
	{
		xr_vector<CLE_Visual*>::iterator it 	= m_VisualHelpers.begin();
		xr_vector<CLE_Visual*>::iterator it_e 	= m_VisualHelpers.end();
		for(;it!=it_e;++it)
		{
			CLE_Visual* v 			= *it;
			xr_delete				(v);
		}
		m_VisualHelpers.clear		();


		u32 cnt 				= m_Data->visual_collection_size();
		visual_data* vc 		= m_Data->visual_collection();

		for(u32 i=0; i<cnt; ++i,++vc)
		{
			CLE_Visual* V = new CLE_Visual(vc->visual);
			V->OnChangeVisual();
			m_VisualHelpers.push_back(V);
			V->PlayAnimation();
		}
	}

	xr_vector<CLE_Visual*>::iterator it 	= m_VisualHelpers.begin();
	xr_vector<CLE_Visual*>::iterator it_e 	= m_VisualHelpers.end();
	for(;it!=it_e;++it)
	{
		CLE_Visual* v 			= *it;
		if (PKinematics(v->visual))
			PKinematics			(v->visual)->CalculateBones(TRUE);
	}

	// reset editor flags
	m_Data->m_editor_flags.zero	();
}
CSpawnPoint::CSpawnPoint(LPVOID data, LPCSTR name):CCustomObject(data,name),m_SpawnData(this)
{
	m_rpProfile			= "";
	m_EM_Ptr			= NULL;
	Construct			(data);
}

void CSpawnPoint::Construct(LPVOID data)
{
	FClassID			= OBJCLASS_SPAWNPOINT;
	m_AttachedObject= 0;
	if (data){
		if (strcmp(LPSTR(data),RPOINT_CHOOSE_NAME)==0)
		{
			m_Type 				= ptRPoint;
			m_RP_Type			= rptActorSpawn;
			m_GameType.SetDefaults();
			m_RP_TeamID			= 1;
		}else if (strcmp(LPSTR(data),ENVMOD_CHOOSE_NAME)==0)
		{
			m_Type 				= ptEnvMod;
			m_EM_Flags.one		();
			m_EM_Radius			= 10.f;
			m_EM_Power			= 1.f;
			m_EM_ViewDist		= 300.f;
			m_EM_FogColor		= 0x00808080;
			m_EM_FogDensity		= 1.f;
			m_EM_AmbientColor	= 0x00000000;
			m_EM_SkyColor		= 0x00FFFFFF;
			m_EM_HemiColor		= 0x00FFFFFF;
			m_EM_ShapeType		= CShapeData::cfSphere;
		}else{
			CreateSpawnData(LPCSTR(data));
			if (!m_SpawnData.Valid())
			{
				SetValid(false);
			}else{
				m_Type			= ptSpawnPoint;
			}
		}
	}else{
		SetValid(false);
	}
}
void  CSpawnPoint::	OnSceneRemove	()
{
	DeletePhysicsShell();
	inherited::OnSceneRemove	();
}
CSpawnPoint::~CSpawnPoint()
{
	if(m_Type == ptEnvMod && m_EM_Ptr)
    	g_pGamePersistent->Environment().remove_modifier(m_EM_Ptr);

	xr_delete(m_AttachedObject);
	OnDeviceDestroy();
}

void CSpawnPoint::Select(int  flag)
{
	inherited::Select(flag);
	if (m_AttachedObject) m_AttachedObject->Select(flag);
}

void  CSpawnPoint::Move( Fvector& amount )
{
	inherited::Move( amount );
	const float f_drag_factor = 200.f;
	if (m_physics_shell)
		ApplyDragForce(Fvector().mul(amount, f_drag_factor));
}

void CSpawnPoint::SetPosition(const Fvector& pos)
{
	if(m_physics_shell)
		return;
	inherited::SetPosition	(pos);
	if (m_AttachedObject) m_AttachedObject->SetPosition( pos);
}
void CSpawnPoint::SetRotation(const Fvector& rot)
{
	if(m_physics_shell)
		return;
	inherited::SetRotation	(rot);
	if (m_AttachedObject) m_AttachedObject->SetRotation( rot);
}
void CSpawnPoint::SetScale(const Fvector& scale)
{
	if(m_physics_shell)
		return;
	inherited::SetScale		(scale);
	if (m_AttachedObject) m_AttachedObject->SetScale( scale);
}

bool CSpawnPoint::AttachObject(CCustomObject* obj)
{
	bool bAllowed = false;
	//  
	if (m_SpawnData.Valid()){
		switch(obj->FClassID){
		case OBJCLASS_SHAPE:
			bAllowed = !!m_SpawnData.m_Data->shape();
		break;
//        case OBJCLASS_SCENEOBJECT:
//	    	bAllowed = !!dynamic_cast<xrSE_Visualed*>(m_SpawnData.m_Data);
//        break;
		}
	}
	//  
	if (bAllowed)
	{
		DetachObject				();
		OnAppendObject				(obj);
		m_AttachedObject->OnAttach	(this);
		SetPosition( m_AttachedObject->GetPosition());
		FRotation 	= m_AttachedObject->GetRotation();
		SetScale( 		 m_AttachedObject->GetScale());
	}
	return bAllowed;
}

void CSpawnPoint::DetachObject()
{
	if (m_AttachedObject)
	{
		m_AttachedObject->OnDetach();
		Scene->AppendObject(m_AttachedObject,false);
		m_AttachedObject = 0;
	}
}

bool CSpawnPoint::RefCompare	(LPCSTR ref)
{
	return ref&&ref[0]&&m_SpawnData.Valid()?(strcmp(ref,m_SpawnData.m_Data->name())==0):false; 
}

LPCSTR CSpawnPoint::RefName	() 			
{
	return m_SpawnData.Valid()?m_SpawnData.m_Data->name():0;
}

bool CSpawnPoint::CreateSpawnData(LPCSTR entity_ref)
{
	R_ASSERT(entity_ref&&entity_ref[0]);
	m_SpawnData.Destroy	();
	m_SpawnData.Create	(entity_ref);
	if (m_SpawnData.Valid()) m_Type = ptSpawnPoint;
	return m_SpawnData.Valid();
}


bool CSpawnPoint::GetBox( Fbox& box ) 
{
	switch (m_Type){
	case ptRPoint: 	
		box.set		( GetPosition(), GetPosition() );
		box.min.x 	-= RPOINT_SIZE;
		box.min.y 	-= 0;
		box.min.z 	-= RPOINT_SIZE;
		box.max.x 	+= RPOINT_SIZE;
		box.max.y 	+= RPOINT_SIZE*2.f;
		box.max.z 	+= RPOINT_SIZE;
	break;
	case ptEnvMod: 	
    	if (Selected()){
    		switch(m_EM_ShapeType){
        	case CShapeData::cfSphere:
        		box.set		(GetPosition(), GetPosition());
        		box.grow	(m_EM_Radius);
        	break;
        	case CShapeData::cfBox:
        		box.identity();
            	box.xform	(FTransform);
        	break;
        	default:
        		THROW;
        	}
    	}else{
        	box.set			(GetPosition(), GetPosition());
            box.grow		(ENVMOD_SIZE);

        }

	break;
	case ptSpawnPoint:
		if (m_SpawnData.Valid()){
			if (m_SpawnData.m_Visual&&m_SpawnData.m_Visual->visual)
			{
				box.set		(m_SpawnData.m_Visual->visual->getVisData().box);
				Fmatrix		transform = FTransformRP;
				if(m_physics_shell)
						UpdateObjectXform( transform );
					
				box.xform	(transform);
			}else{
				CEditShape* shape	= dynamic_cast<CEditShape*>(m_AttachedObject);
				if (shape&&!shape->GetShapes().empty()){
					CShapeData::ShapeVec& SV	= shape->GetShapes();
					box.invalidate();
					Fvector p;
					for (CShapeData::ShapeIt it=SV.begin(); it!=SV.end(); it++){
						switch (it->type){
						case CShapeData::cfSphere:
							p.add(it->data.sphere.P,it->data.sphere.R); shape->_Transform().transform_tiny(p); box.modify(p);
							p.sub(it->data.sphere.P,it->data.sphere.R); shape->_Transform().transform_tiny(p); box.modify(p);
						break;
						case CShapeData::cfBox:
							p.set( 0.5f, 0.5f, 0.5f);it->data.box.transform_tiny(p); shape->_Transform().transform_tiny(p); box.modify(p);
							p.set(-0.5f,-0.5f,-0.5f);it->data.box.transform_tiny(p); shape->_Transform().transform_tiny(p); box.modify(p);
						break;
						}
					}
				}else{
					box.set		( GetPosition(), GetPosition() );
					box.min.x 	-= RPOINT_SIZE;
					box.min.y 	-= 0;
					box.min.z 	-= RPOINT_SIZE;
					box.max.x 	+= RPOINT_SIZE;
					box.max.y 	+= RPOINT_SIZE*2.f;
					box.max.z 	+= RPOINT_SIZE;
				}
			}
		}else{
			box.set		( GetPosition(), GetPosition() );
			box.min.x 	-= RPOINT_SIZE;
			box.min.y 	-= 0;
			box.min.z 	-= RPOINT_SIZE;
			box.max.x 	+= RPOINT_SIZE;
			box.max.y 	+= RPOINT_SIZE*2.f;
			box.max.z 	+= RPOINT_SIZE;
		}
	break;
	default: NODEFAULT;
	}
	if (m_AttachedObject){ 		
		Fbox 					bb;
		m_AttachedObject->GetBox(bb);
		box.merge				(bb);
	}
	return true;
}
Fvector u32_3f(u32);

void CSpawnPoint::OnFrame()
{
	inherited::OnFrame();
	if (m_AttachedObject) 		m_AttachedObject->OnFrame	();
	if (m_SpawnData.Valid())
	{
		if(m_physics_shell&&m_SpawnData.m_Data->m_editor_flags.is(CSE_Abstract::flVisualAnimationChange))
		{
			DeletePhysicsShell			();
			m_SpawnData.OnFrame			();
			CreatePhysicsShell			( &FTransform );
		}
		else 
		  m_SpawnData.OnFrame			();
	}

	if(m_Type == ptEnvMod)
	{
    	if(Visible())
        {
        	if(!m_EM_Ptr)
            	m_EM_Ptr			= g_pGamePersistent->Environment().new_modifier();

        	m_EM_Ptr->position 		= FPosition;
        	m_EM_Ptr->radius 		= m_EM_Radius;
        	m_EM_Ptr->power 		= m_EM_Power;
        	m_EM_Ptr->far_plane 	= m_EM_ViewDist;
        	m_EM_Ptr->fog_color 	= u32_3f(m_EM_FogColor);
        	m_EM_Ptr->fog_density 	= m_EM_FogDensity;
        	m_EM_Ptr->ambient 		= u32_3f(m_EM_AmbientColor);
        	m_EM_Ptr->sky_color 	= u32_3f(m_EM_SkyColor);
        	m_EM_Ptr->hemi_color 	= u32_3f(m_EM_HemiColor);
        	m_EM_Ptr->use_flags 	= m_EM_Flags;

        	m_EM_Ptr->shape_type	= m_EM_ShapeType;

        	Fobb obb;
        	obb.m_translate.set		(FPosition);
        	obb.m_halfsize.set		(FScale).mul(0.5f);
        	obb.m_rotate.set		(FTransformR);
        	m_EM_Ptr->obb			= obb;
        }
        else
        {
        	if(m_EM_Ptr)
            {
            	g_pGamePersistent->Environment().remove_modifier(m_EM_Ptr);
                m_EM_Ptr			= NULL;
            }
        }
    }

}
void CSpawnPoint::RenderSimBox()
{
	Fbox box;
	GetBox( box );

	Fvector c,s;

	box.get_CD( c, s );
	Fmatrix	m;
	m.scale(Fvector().mul(s,2));
	m.c.set(c);
				 
			   //     B.mulA_43			(_Transform());
	RCache.set_xform_world(m);
	u32 clr = 0x06005000;
	DU_impl.DrawIdentBox(true,false,clr,clr);
}
void CSpawnPoint::Render( int priority, bool strictB2F )
{

	Fmatrix SaveTransform =   FTransformRP;

	if( m_physics_shell )
	{
		UpdateObjectXform( FTransformRP );
		RenderSimBox( );
	}
	inherited::Render			(priority, strictB2F);
	Scene->SelectLightsForObject(this);

	
	// render attached object
	if (m_AttachedObject)
			m_AttachedObject->Render(priority, strictB2F);
	if (m_SpawnData.Valid())
			m_SpawnData.Render(Selected(),FTransformRP,priority, strictB2F);
	// render spawn point
	if (1==priority){
		if (strictB2F){
			RCache.set_xform_world(FTransformRP);
			if (m_SpawnData.Valid()){
				// render icon
				ESceneSpawnTool* st	= dynamic_cast<ESceneSpawnTool*>(FParentTools); VERIFY(st);
				ref_shader s 	   	= st->GetIcon(m_SpawnData.m_Data->name());
				DU_impl.DrawEntity		(0xffffffff,s);
			}else{
				switch (m_Type)
				{
					case ptRPoint:
					{
						ESceneSpawnTool* st	= dynamic_cast<ESceneSpawnTool*>(FParentTools); VERIFY(st);
						if( NULL==st->get_draw_visual(m_RP_TeamID, m_RP_Type, m_GameType) )
						{
							float k = 1.f/(float(m_RP_TeamID+1)/float(MAX_TEAM));
							int r = m_RP_TeamID%MAX_TEAM;
							Fcolor c;
							c.set(RP_COLORS[r]);
							c.mul_rgb(k*0.9f+0.1f);
							DU_impl.DrawEntity(c.get(),EDevice->m_WireShader);
						}
					}break;
					case ptEnvMod:
					{
						Fvector pos={0,0,0};
						EDevice->SetShader(EDevice->m_WireShader);
						DU_impl.DrawCross(pos,0.25f,Selected()?ENVMOD_SEL_COLOR2:ENVMOD_COLOR,true);
						if (Selected())
							switch(m_EM_ShapeType)
                            {
                            	case CShapeData::cfSphere:
                                	DU_impl.DrawSphere(Fidentity,GetPosition(),m_EM_Radius,ENVMOD_SEL_COLOR1,ENVMOD_SEL_COLOR2,true,true);
                                break;
                                case CShapeData::cfBox:
                                {
                                	Fobb obb;
									obb.invalidate();
                                    obb.m_halfsize.set(0.5f,0.5f,0.5f);
                                	DU_impl.DrawOBB(FTransform,obb,0x30FFAE00,0x00FFAE00);
                                }break;
                                default:
                                	THROW;
                            }

					}break;

					default: THROW2("CSpawnPoint:: Unknown Type");
				}
			}
		}else{
			ESceneSpawnTool* st = dynamic_cast<ESceneSpawnTool*>(FParentTools); VERIFY(st);
			if (st->m_Flags.is(ESceneSpawnTool::flShowSpawnType))
			{
				xr_string s_name;
				if (m_SpawnData.Valid())
				{
					s_name	= m_SpawnData.m_Data->name();
				}else{
					switch (m_Type)
					{
					case ptRPoint: 	s_name = "RPoint T:" + xr_string::ToString(m_RP_TeamID); break;
					case ptEnvMod:
					{
						string256 Data = {};
						sprintf(Data, "EnvMod V:%3.2f, F:%3.2f", m_EM_ViewDist, m_EM_FogDensity);
						s_name = Data;
					}
					break;
					default: THROW2("CSpawnPoint:: Unknown Type");
					}
				}
				
				Fvector D;	D.sub(EDevice->vCameraPosition,GetPosition());
				float dist 	= D.normalize_magn();
				if (!st->m_Flags.is(ESceneSpawnTool::flPickSpawnType)||
					!Scene->RayPickObject(dist,GetPosition(),D,OBJCLASS_SCENEOBJECT,0,0))
						DU_impl.OutText	(GetPosition(),s_name.c_str(),0xffffffff,0xff000000);
			}
			if(Selected())
			{
				RCache.set_xform_world(Fidentity);
				Fbox bb; GetBox(bb);
				u32 clr = 0xFFFFFFFF;
				EDevice->SetShader(EDevice->m_WireShader);
				DU_impl.DrawSelectionBoxB(bb,&clr);
			}
		}
	}
	
	if(m_Type==ptRPoint)
	{
		ESceneSpawnTool* st		= dynamic_cast<ESceneSpawnTool*>(FParentTools); VERIFY(st);
		CEditableObject* v		= st->get_draw_visual(m_RP_TeamID, m_RP_Type, m_GameType); 
		if(v)
			v->Render				(FTransformRP, priority, strictB2F);
	}
	FTransformRP = SaveTransform;
}

bool CSpawnPoint::FrustumPick(const CFrustum& frustum)
{
	if (m_AttachedObject&&m_AttachedObject->FrustumPick(frustum)) return true;
	Fbox bb; GetBox(bb);
	u32 mask=0xff;
	return (frustum.testAABB(bb.data(),mask));
}

bool CSpawnPoint::RayPick(float& distance, const Fvector& start, const Fvector& direction, SRayPickInfo* pinf)
{
	bool bPick 	= false;
	if (m_AttachedObject){
		bPick 	= m_AttachedObject->RayPick(distance, start, direction, pinf);
		return 	bPick;
	}

	Fbox 		bb;
	Fvector 	pos;
	float 		radius;
	GetBox		(bb);
	bb.getsphere(pos,radius);

	Fvector ray2;
	ray2.sub	(pos, start);

	float d = ray2.dotproduct(direction);
	if( d > 0  ){
		float d2 = ray2.magnitude();
		if( ((d2*d2-d*d) < (radius*radius)) && (d>radius) ){
			Fvector pt;
			if (Fbox::rpOriginOutside==bb.Pick2(start,direction,pt)){
				d	= start.distance_to(pt);
				if (d<distance){
					distance	= d;
					bPick 		= true;
					if( pinf && m_SpawnData.m_Visual && m_SpawnData.m_Visual->visual )
					{

						IKinematics*  K = m_SpawnData.m_Visual->visual->dcast_PKinematics();
						u16 b_id = u16(-1);
						Fvector norm;
						if( K )
						{
						   bPick =	ETOOLS::intersect( FTransformRP, *K, start,  direction, b_id, distance,  norm );
						   if( bPick )
						   {
							   pinf->visual_inf.K = K;
							   pinf->visual_inf.normal = norm;
							   pt.mad( start, direction, distance ); 
						   }

						}


						pinf->s_obj = this;
						pinf->e_obj = 0;
						pinf->e_mesh = 0;
						pinf->pt = pt;
						pinf->inf.range = distance;
					}
				}
			}
		}
	}

	return bPick;
}

bool CSpawnPoint::OnAppendObject(CCustomObject* object)
{
	R_ASSERT(!m_AttachedObject);
	if (object->FClassID!=OBJCLASS_SHAPE) return false;
	// all right
	m_AttachedObject 		= object;
	object->m_pOwnerObject	= this;
	Scene->RemoveObject		(object, false, false);

	CEditShape* sh = dynamic_cast<CEditShape*>(m_AttachedObject);
	if(m_SpawnData.Valid())
	{
		if(pSettings->line_exist(m_SpawnData.m_Data->name(),"shape_transp_color"))
		{
			sh->m_DrawTranspColor = pSettings->r_color(m_SpawnData.m_Data->name(),"shape_transp_color");
			sh->m_DrawEdgeColor = pSettings->r_color(m_SpawnData.m_Data->name(),"shape_edge_color");
		}
	}

	return true;
}

bool CSpawnPoint::LoadLTX(CInifile& ini, LPCSTR sect_name)
{

	u32 version = ini.r_u32(sect_name, "version");

	if(version<0x0014)
	{
		ELog.Msg( mtError, "SPAWNPOINT: Unsupported version.");
		return false;
	}

	CCustomObject::LoadLTX(ini, sect_name);
	m_Type 			= (EPointType)ini.r_u32(sect_name, "type");

	if (m_Type>=ptMaxType)
	{
		ELog.Msg( mtError, "SPAWNPOINT: Unsupported spawn version.");
		return false;
	}
	switch (m_Type)
	{
	case ptSpawnPoint:
	{
		string128	buff;
		xr_strconcat(buff, sect_name, "_spawndata");
		if (!m_SpawnData.LoadLTX(ini, buff))
		{
			ELog.Msg( mtError, "SPAWNPOINT: Can't load Spawn Data.");
			return false;
		}
		SetValid		(true);
	}break;
	case ptRPoint:
		{
			if(version>=0x0017)
				m_rpProfile				= ini.r_string(sect_name, "rp_profile");
				
			m_RP_TeamID					= ini.r_u8	(sect_name, "team_id");
			m_RP_Type					= ini.r_u8	(sect_name, "rp_type");
			m_GameType.LoadLTX			(ini, sect_name, (version==0x0014) );
		}
	break;
	case ptEnvMod:
		{
			m_EM_Radius			= ini.r_float(sect_name, "em_radius");
			m_EM_Power			= ini.r_float(sect_name, "em_power");
			m_EM_ViewDist		= ini.r_float(sect_name, "view_dist");
			m_EM_FogColor		= ini.r_u32(sect_name, 	 "fog_color");
			m_EM_FogDensity		= ini.r_float(sect_name, "fog_density");
			m_EM_AmbientColor	= ini.r_u32(sect_name, 	"ambient_color");
			m_EM_SkyColor		= ini.r_u32(sect_name, "sky_color");
			m_EM_HemiColor		= ini.r_u32(sect_name, "hemi_color");
			if(version>=0x0016)
				m_EM_Flags.assign	(ini.r_u16(sect_name, "em_flags"));
			if(version>=0x0018)
				m_EM_ShapeType		= ini.r_u8(sect_name, "shape_type");
		}
	break;
	default: THROW;
	}

	// objects
	if(ini.line_exist(sect_name, "attached_count"))
		Scene->ReadObjectsLTX(ini, sect_name, "attached", EScene::TAppendObject(this, &CSpawnPoint::OnAppendObject),0);

	UpdateTransform	();

	// BUG fix
	CEditShape* shape	= dynamic_cast<CEditShape*>(m_AttachedObject);
	if (shape)
		SetScale 	( shape->GetScale());
	
	return true;
}

void CSpawnPoint::SaveLTX(CInifile& ini, LPCSTR sect_name)
{
	CCustomObject::SaveLTX(ini, sect_name);

	ini.w_u32			(sect_name, "version", SPAWNPOINT_VERSION);

	// save attachment
	if (m_AttachedObject)
	{
		ObjectList 					lst;
		lst.push_back				(m_AttachedObject);
		Scene->SaveObjectsLTX		(lst, sect_name, "attached", ini);
	}

	ini.w_u32						(sect_name, "type", m_Type);
	
	switch (m_Type)
	{
	case ptSpawnPoint:
	{
		string128	buff;
		m_SpawnData.SaveLTX(ini, xr_strconcat(buff, sect_name, "_spawndata"));
	}break;
	case ptRPoint:
	{
		ini.w_u8		(sect_name, "team_id", m_RP_TeamID);
		ini.w_string	(sect_name, "rp_profile", m_rpProfile.c_str());
		ini.w_u8		(sect_name, "rp_type", m_RP_Type);
		m_GameType.SaveLTX(ini, sect_name);
	}break;
	case ptEnvMod:
	{
		ini.w_float		(sect_name, "em_radius", m_EM_Radius);
		ini.w_float		(sect_name, "em_power", m_EM_Power);
		ini.w_float		(sect_name, "view_dist", m_EM_ViewDist);
		ini.w_u32		(sect_name, "fog_color", m_EM_FogColor);
		ini.w_float		(sect_name, "fog_density", m_EM_FogDensity);
		ini.w_u32		(sect_name, "ambient_color", m_EM_AmbientColor);
		ini.w_u32		(sect_name, "sky_color", m_EM_SkyColor);
		ini.w_u32		(sect_name, "hemi_color", m_EM_HemiColor);
		ini.w_u16		(sect_name, "em_flags", m_EM_Flags.get());
		ini.w_u8		(sect_name, "shape_type", m_EM_ShapeType);
	}break;

	default: THROW;
	}
}

bool CSpawnPoint::LoadStream(IReader& F)
{
	u16 version = 0;

	R_ASSERT(F.r_chunk(SPAWNPOINT_CHUNK_VERSION,&version));
	if(version<0x0014)
	{
		ELog.Msg( mtError, "SPAWNPOINT: Unsupported version.");
		return false;
	}

	CCustomObject::LoadStream(F);

	// new generation
	if (F.find_chunk(SPAWNPOINT_CHUNK_ENTITYREF))
	{
		if (!m_SpawnData.LoadStream(F))
		{
			ELog.Msg( mtError, "SPAWNPOINT: Can't load Spawn Data.");
			return false;
		}
		SetValid	(true);
		m_Type			= ptSpawnPoint;
	}else{
		if (F.find_chunk(SPAWNPOINT_CHUNK_TYPE))     m_Type 		= (EPointType)F.r_u32();
		if (m_Type>=ptMaxType){
			ELog.Msg( mtError, "SPAWNPOINT: Unsupported spawn version.");
			return false;
		}
		switch (m_Type){
		case ptRPoint:
			if (F.find_chunk(SPAWNPOINT_CHUNK_RPOINT))
			{

				m_RP_TeamID				= F.r_u8();
				m_RP_Type				= F.r_u8();
				m_GameType.LoadStream	(F);
				if(version>=0x0017)
					F.r_stringZ			(m_rpProfile);
			}
		break;
		case ptEnvMod:
			if (F.find_chunk(SPAWNPOINT_CHUNK_ENVMOD)){
				m_EM_Radius			= F.r_float();
				m_EM_Power			= F.r_float();
				m_EM_ViewDist		= F.r_float();
				m_EM_FogColor		= F.r_u32();
				m_EM_FogDensity		= F.r_float();
				m_EM_AmbientColor	= F.r_u32();
				m_EM_SkyColor		= F.r_u32();
				if (F.find_chunk(SPAWNPOINT_CHUNK_ENVMOD2))
					m_EM_HemiColor	= F.r_u32();
				if (F.find_chunk(SPAWNPOINT_CHUNK_ENVMOD3))
					m_EM_Flags.assign(F.r_u16());
				if (F.find_chunk(SPAWNPOINT_CHUNK_ENVMOD4))
					m_EM_ShapeType	= F.r_u8();
			}
		break;
		default: THROW;
		}
	}

	// objects
	Scene->ReadObjectsStream(F,SPAWNPOINT_CHUNK_ATTACHED_OBJ, EScene::TAppendObject(this, &CSpawnPoint::OnAppendObject),0);

	UpdateTransform	();

	// BUG fix
	CEditShape* shape	= dynamic_cast<CEditShape*>(m_AttachedObject);
	if (shape) 	SetScale(shape->GetScale());
	
	return true;
}

void CSpawnPoint::SaveStream(IWriter& F)
{
	CCustomObject::SaveStream(F);

	F.open_chunk		(SPAWNPOINT_CHUNK_VERSION);
	F.w_u16				(SPAWNPOINT_VERSION);
	F.close_chunk		();

	// save attachment
	if (m_AttachedObject)
	{
		ObjectList lst; lst.push_back(m_AttachedObject);
		Scene->SaveObjectsStream(lst,SPAWNPOINT_CHUNK_ATTACHED_OBJ,F);
	}

	if (m_SpawnData.Valid())
	{
		m_SpawnData.SaveStream(F);
	}else{
		F.w_chunk	(SPAWNPOINT_CHUNK_TYPE,		&m_Type,	sizeof(u32));
		switch (m_Type){
		case ptRPoint:
			F.open_chunk			(SPAWNPOINT_CHUNK_RPOINT);
			F.w_u8					(m_RP_TeamID);
			F.w_u8					(m_RP_Type);
			m_GameType.SaveStream	(F);
			F.w_stringZ				(m_rpProfile);            
			F.close_chunk			();
		break;
		case ptEnvMod:
			F.open_chunk(SPAWNPOINT_CHUNK_ENVMOD);
			F.w_float	(m_EM_Radius);
			F.w_float	(m_EM_Power);
			F.w_float	(m_EM_ViewDist);
			F.w_u32		(m_EM_FogColor);
			F.w_float	(m_EM_FogDensity);
			F.w_u32		(m_EM_AmbientColor);
			F.w_u32		(m_EM_SkyColor);
			F.close_chunk();
			F.open_chunk(SPAWNPOINT_CHUNK_ENVMOD2);
			F.w_u32		(m_EM_HemiColor);
			F.close_chunk();

			F.open_chunk(SPAWNPOINT_CHUNK_ENVMOD3);
			F.w_u16		(m_EM_Flags.get());
			F.close_chunk();

			F.open_chunk(SPAWNPOINT_CHUNK_ENVMOD4);
			F.w_u8		(m_EM_ShapeType);
			F.close_chunk();
		break;
		default: THROW;
		}
	}
}


Fvector3 u32_3f(u32 clr)
{
	Fvector	tmp;
	float f	= 1.f / 255.f;
	tmp.x 	= f * float((clr >> 16)& 0xff);
	tmp.y 	= f * float((clr >>  8)& 0xff);
	tmp.z 	= f * float((clr >>  0)& 0xff);
	return	tmp;
}

bool CSpawnPoint::ExportGame(SExportStreams* F)
{
	// spawn
	if (m_SpawnData.Valid()){
		if (m_SpawnData.m_Data->validate()){
			m_SpawnData.ExportGame		(F,this);
		}else{
			Log	("!Invalid spawn data:",GetName());
			return false;
		}
	}else{
		// game
		switch (m_Type){
		case ptRPoint:
			F->rpoint.stream.open_chunk	(F->rpoint.chunk++);
			F->rpoint.stream.w_fvector3	(GetPosition());
			F->rpoint.stream.w_fvector3	(GetRotation());
			F->rpoint.stream.w_u8		(m_RP_TeamID);
			F->rpoint.stream.w_u8		(m_RP_Type);
			F->rpoint.stream.w_u16		(m_GameType.m_GameType.get());
			F->rpoint.stream.w_stringZ	(m_rpProfile);
			F->rpoint.stream.close_chunk	();
		break;
		case ptEnvMod:
			Fcolor tmp;
			F->envmodif.stream.open_chunk(F->envmodif.chunk++);
			F->envmodif.stream.w_fvector3(GetPosition());
			F->envmodif.stream.w_float	(m_EM_Radius);
			F->envmodif.stream.w_float	(m_EM_Power);
			F->envmodif.stream.w_float	(m_EM_ViewDist);
			F->envmodif.stream.w_fvector3(u32_3f(m_EM_FogColor));
			F->envmodif.stream.w_float	(m_EM_FogDensity);
			F->envmodif.stream.w_fvector3(u32_3f(m_EM_AmbientColor));
			F->envmodif.stream.w_fvector3(u32_3f(m_EM_SkyColor));
			F->envmodif.stream.w_fvector3(u32_3f(m_EM_HemiColor));
			F->envmodif.stream.w_u16(m_EM_Flags.get());
			F->envmodif.stream.w_u8(m_EM_ShapeType);
			F->envmodif.stream.w_fvector3(GetRotation());
			F->envmodif.stream.w_fvector3(Fvector().set(GetScale()).mul(0.5f));
			F->envmodif.stream.close_chunk();
		break;
		}
	}
	return true;
}

void CSpawnPoint::OnFillRespawnItemProfile(ChooseValue* val)
{
	val->m_Items->clear		();
	string_path				fn;
	FS.update_path			(fn,"$game_config$", "mp\\respawn_items.ltx");
	CInifile				ini(fn);
	CInifile::RootIt it 	= ini.sections().begin();
	CInifile::RootIt it_e 	= ini.sections().end();

	for(;it!=it_e;++it)
	{
		shared_str name		= (*it)->Name;
		shared_str hint;
		if(ini.line_exist(name,"description"))
			hint 			= ini.r_string(name,"description");
		else
			hint 			= "<empty>";
		
		val->m_Items->push_back( SChooseItem( name.c_str(), hint.c_str() ) );
	}
}

void CSpawnPoint::OnFillChooseItems		(ChooseValue* val)
{
	ESceneSpawnTool* st 		= dynamic_cast<ESceneSpawnTool*>(FParentTools); VERIFY(st);
	CLASS_ID cls_id				= m_SpawnData.m_ClassID;
	ESceneSpawnTool::ClassSpawnMapIt cls_it = st->m_Classes.find(cls_id); VERIFY(cls_it!=st->m_Classes.end());
	*val->m_Items				= cls_it->second;
}

shared_str CSpawnPoint::SectionToEditor(shared_str nm)
{
	ESceneSpawnTool* st 		= dynamic_cast<ESceneSpawnTool*>(FParentTools); 			VERIFY(st);
	ESceneSpawnTool::ClassSpawnMapIt cls_it = st->m_Classes.find(m_SpawnData.m_ClassID);	VERIFY(cls_it!=st->m_Classes.end());
	for (ESceneSpawnTool::SSVecIt ss_it=cls_it->second.begin(); ss_it!=cls_it->second.end(); ++ss_it)
		if (nm.equal(ss_it->hint)) return ss_it->name;
	return 0;
}

shared_str CSpawnPoint::EditorToSection(shared_str nm)
{
	ESceneSpawnTool* st  	= dynamic_cast<ESceneSpawnTool*>(FParentTools); 			VERIFY(st);
	ESceneSpawnTool::ClassSpawnMapIt cls_it = st->m_Classes.find(m_SpawnData.m_ClassID);	VERIFY(cls_it!=st->m_Classes.end());
	for (ESceneSpawnTool::SSVecIt ss_it=cls_it->second.begin(); ss_it!=cls_it->second.end(); ++ss_it)
		if (nm.equal(ss_it->name)) return ss_it->hint;
	return 0;
}

void CSpawnPoint::OnRPointTypeChange(PropValue* prop)
{
	ExecCommand				(COMMAND_UPDATE_PROPERTIES);
}

void CSpawnPoint::OnProfileChange(PropValue* prop)
{
	if (m_SpawnData.m_Profile.size()!=0){
		shared_str s_name		= EditorToSection(m_SpawnData.m_Profile);
		VERIFY					(s_name.size());
		if (0!=strcmp(m_SpawnData.m_Data->name(),*s_name))
		{
			CSE_Abstract* tmp	= g_SEFactoryManager->create_entity	(*s_name);
			VERIFY				(tmp);
			NET_Packet 			Packet;
			tmp->Spawn_Write	(Packet,TRUE);
			R_ASSERT			(m_SpawnData.m_Data->Spawn_Read(Packet));
			m_SpawnData.m_Data->set_editor_flag(CSE_Abstract::flVisualChange|CSE_Abstract::flVisualAnimationChange);
			g_SEFactoryManager->destroy_entity		(tmp);
		}
	}else{
		m_SpawnData.m_Profile	= SectionToEditor(m_SpawnData.m_Data->name());
	}
}

void CSpawnPoint::FillProp(LPCSTR pref, PropItemVec& items)
{
	inherited::FillProp(pref,items);

	if (m_SpawnData.Valid())
	{
		shared_str pref1			= PrepareKey(pref,m_SpawnData.m_Data->name());
		m_SpawnData.m_Profile 		= SectionToEditor(m_SpawnData.m_Data->name());
		ChooseValue* C				= PHelper().CreateChoose(items,PrepareKey(pref1.c_str(),"Profile (spawn section)"),&m_SpawnData.m_Profile,smCustom,0,0,1,cfFullExpand);
		C->OnChooseFillEvent.bind	(this,&CSpawnPoint::OnFillChooseItems);
		C->OnChangeEvent.bind		(this,&CSpawnPoint::OnProfileChange);
		m_SpawnData.FillProp		(pref,items);
	}else{
		switch (m_Type)
		{
		case ptRPoint:
		{

			if(m_RP_Type==rptItemSpawn)
			{                                                            
				ChooseValue* C				= PHelper().CreateChoose(items,PrepareKey(pref,"Respawn Point\\Profile"),&m_rpProfile,smCustom,0,0,10,cfMultiSelect);
				C->OnChooseFillEvent.bind	(this,&CSpawnPoint::OnFillRespawnItemProfile);
			 }else
			{
				PHelper().CreateU8		(items, PrepareKey(pref,"Respawn Point\\Team"), 		&m_RP_TeamID, 	0,32);
			}
			Token8Value* TV = PHelper().CreateToken8	(items, PrepareKey(pref,"Respawn Point\\Spawn Type"),	&m_RP_Type, 	rpoint_type);
			TV->OnChangeEvent.bind		(this,&CSpawnPoint::OnRPointTypeChange);

		m_GameType.FillProp			(pref, items);
		}break;
		case ptEnvMod:{
			static xr_token shape_type[] = {
            	{"Sphere", 	CShapeData::cfSphere},
                {"Box", 	CShapeData::cfBox},
                {0}
			};

			PHelper().CreateToken8	(items, PrepareKey(pref,"Environment Modificator\\Shape Type"),		&m_EM_ShapeType,	shape_type);
			PHelper().CreateFloat	(items, PrepareKey(pref,"Environment Modificator\\Radius"),			&m_EM_Radius, 	EPS_L,10000.f);
			PHelper().CreateFloat	(items, PrepareKey(pref,"Environment Modificator\\Power"), 			&m_EM_Power, 	EPS,1000.f);

			Flag16Value* FV 		= NULL;
			
			FV = PHelper().CreateFlag16(items, PrepareKey(pref,"Environment Modificator\\View Distance"), &m_EM_Flags, eViewDist);
			FV->OnChangeEvent.bind	 (this,&CSpawnPoint::OnEnvModFlagChange);
			if(m_EM_Flags.test(eViewDist))
				PHelper().CreateFloat(items, PrepareKey(pref,"Environment Modificator\\View Distance\\ "),	&m_EM_ViewDist, EPS_L,10000.f);
				
			FV = PHelper().CreateFlag16(items, PrepareKey(pref,"Environment Modificator\\Fog Color"), &m_EM_Flags, eFogColor);
			FV->OnChangeEvent.bind	 (this,&CSpawnPoint::OnEnvModFlagChange);
			if(m_EM_Flags.test(eFogColor))
				PHelper().CreateColor	(items, PrepareKey(pref,"Environment Modificator\\Fog Color\\ "), 		&m_EM_FogColor);
				
			FV = PHelper().CreateFlag16(items, PrepareKey(pref,"Environment Modificator\\Fog Density"), &m_EM_Flags, eFogDensity);
			FV->OnChangeEvent.bind	 (this,&CSpawnPoint::OnEnvModFlagChange);
			if(m_EM_Flags.test(eFogDensity))
				PHelper().CreateFloat	(items, PrepareKey(pref,"Environment Modificator\\Fog Density\\ "), 	&m_EM_FogDensity, 0.f,10000.f);
				
			FV = PHelper().CreateFlag16(items, PrepareKey(pref,"Environment Modificator\\Ambient Color"), &m_EM_Flags, eAmbientColor);
			FV->OnChangeEvent.bind	 (this,&CSpawnPoint::OnEnvModFlagChange);
			if(m_EM_Flags.test(eAmbientColor))
				PHelper().CreateColor	(items, PrepareKey(pref,"Environment Modificator\\Ambient Color\\ "), 	&m_EM_AmbientColor);

			FV = PHelper().CreateFlag16(items, PrepareKey(pref,"Environment Modificator\\Sky Color"), &m_EM_Flags, eSkyColor);
			FV->OnChangeEvent.bind	 (this,&CSpawnPoint::OnEnvModFlagChange);
			if(m_EM_Flags.test(eSkyColor))
				PHelper().CreateColor	(items, PrepareKey(pref,"Environment Modificator\\Sky Color\\ "), 		&m_EM_SkyColor);
				
			FV = PHelper().CreateFlag16(items, PrepareKey(pref,"Environment Modificator\\Hemi Color"), &m_EM_Flags, eHemiColor);
			FV->OnChangeEvent.bind	 (this,&CSpawnPoint::OnEnvModFlagChange);
			if(m_EM_Flags.test(eHemiColor))
				PHelper().CreateColor	(items, PrepareKey(pref,"Environment Modificator\\Hemi Color\\ "), 	&m_EM_HemiColor);
		}break;
		default: THROW;
		}
	}
}
void CSpawnPoint::OnEnvModFlagChange(PropValue* prop)
{
	LTools->UpdateProperties(FALSE);
}


bool CSpawnPoint::OnChooseQuery(LPCSTR specific)
{
	return (m_SpawnData.Valid()&&(0==strcmp(m_SpawnData.m_Data->name(),specific)));
}
 void  CSpawnPoint::UseSimulatePose ()
 {
	 if(m_physics_shell)
	 {
		 Fmatrix	m;
		 UpdateObjectXform(m);
		 FPosition.set(m.c);
		 //m.getXYZi(	FRotation );
		 m.getXYZ (	FRotation );
		 UpdateTransform();
	 }
 }
 void CSpawnPoint::ExportSpawn(xr_vector<NET_Packet>& Ps)
 {
	 m_SpawnData.ExportSpawn(Ps,this);
 }
 void CSpawnPoint::PreExportSpawn()
 {
	 m_SpawnData.PreExportSpawn(this);
 }
 bool CSpawnPoint::IsGraphPoint() const
 {
	 CSE_Abstract* SEAbstract = m_SpawnData.GetEntity();
	 if (SEAbstract)
	 {
		 return dynamic_cast<CSE_ALifeGraphPoint*>(SEAbstract)!=nullptr;
	 }
	 return false;
 }
 void CSpawnPoint::OnUpdateTransform()
 {
  
	  inherited::OnUpdateTransform();
  
 }