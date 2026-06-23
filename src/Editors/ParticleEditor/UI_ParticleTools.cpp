//---------------------------------------------------------------------------

#include "stdafx.h"


#include "UI_ParticleTools.h"
#include "IconsFontAwesome6.h"

#include "../../Layers/xrRender/ParticleAnimCurve.h"
#include "../../xrEngine/ObjectAnimator.h"
#include "../xrECore/Editor/ParticleEffectActions.h"
//------------------------------------------------------------------------------
CParticleTool*	PTools=(CParticleTool*)Tools;
//------------------------------------------------------------------------------
#define CHECK_SNAP(R,A,C){ R+=A; if(fabsf(R)>=C){ A=snapto(R,C); R=0; }else{A=0;}}
//static Fvector zero_vec={0.f,0.f,0.f};
 
EParticleAction* 	pCreateEActionImpl(PAPI::PActionEnum type);

CParticleTool::CParticleTool()
{
    m_CreatingParticle = false;
	m_EditMode			= emNone;
    m_ItemProps 		= 0;
	m_EditObject		= 0;
    m_bModified			= false;
    m_bReady			= false;
    m_Transform.identity();
    m_Vel.set			(0,0,0);
    fFogness			= 0.9f;
    dwFogColor			= 0xffffffff;
    m_Flags.zero		();
    pCreateEAction		= pCreateEActionImpl;
    m_LibPED = 0;
    m_EditPG = 0;
    m_EditPE = 0;
}
//---------------------------------------------------------------------------

CParticleTool::~CParticleTool()
{
}
//---------------------------------------------------------------------------

bool CParticleTool::OnCreate()
{


    m_bReady 		= true;

    Load			(0);

    SetAction		(etaSelect);


    m_EditPE 		= (PS::CParticleEffect*)((CRender*)::Render)->Models->CreatePE(0);
    m_EditPG		= (PS::CParticleGroup*)((CRender*)::Render)->Models->CreatePG(0);
    m_ItemProps = new UIPropertiesForm();
    m_ItemProps->SetModifiedEvent(TOnModifiedEvent(this, &CParticleTool::OnItemModified));

    // item list
    auto ListInitFunc = [&](UIItemListForm*& ListProp)
    {
        R_ASSERT(!ListProp);
        ListProp = new UIItemListForm();
        ListProp->m_Flags.set(UIItemListForm::fMenuEdit, true);
        ListProp->SetOnItemFocusedEvent	({this,&CParticleTool::OnParticleItemFocused});
        ListProp->SetVerifyItemClone({this, &CParticleTool::VerifyParticleCloneItem});
        ListProp->SetOnItemCloneEvent({this, &CParticleTool::OnParticleCloneItem});
        ListProp->SetVerifyItemCreate({this, &CParticleTool::VerifyParticleCreateItem});
        ListProp->SetOnItemCreaetEvent({this, &CParticleTool::OnParticleCreateItem});
        ListProp->SetOnItemRenameEvent	({this,&CParticleTool::OnParticleItemRename});
        ListProp->SetOnItemPreRemoveEvent({this, &CParticleTool::OnParticlePreItemRemove});
        ListProp->SetOnItemRemoveEvent	({this,&CParticleTool::OnParticleItemRemove});
        ListProp->SetVerifyFolderCreate({this, &CParticleTool::VerifyParticleCreateFolder});
        ListProp->SetVerifyItemRename({this, &CParticleTool::VerifyParticleRenameItem});
        ListProp->SetVerifyItemMove({this, &CParticleTool::VerifyParticleMoveItem});
        ListProp->SetGetItemMoveActionSlot({this, &CParticleTool::GetItemMoveActionSlot});
        ListProp->SetOnMoveItemEvent(ENodeMoveActionSlot::First, {this, &CParticleTool::ActionItemMoveAction}); // Move action
    };
    ListInitFunc(m_PList[PEd::ListTypeBase(PEd::LisType::All)]);
    ListInitFunc(m_PList[PEd::ListTypeBase(PEd::LisType::Groups)]);
    ListInitFunc(m_PList[PEd::ListTypeBase(PEd::LisType::Effects)]);
    ListInitFunc(m_PList[PEd::ListTypeBase(PEd::LisType::AnimCurve)]);
    //
    m_ParentAnimator= new CObjectAnimator();

    m_ObjectProps = new UIPropertiesForm();
    FillObjectPrefs();
    return true;
}

void CParticleTool::OnDestroy()
{
	VERIFY				(m_bReady);
    m_bReady			= false;

    xr_delete			(m_ParentAnimator);

	Lib.RemoveEditObject(m_EditObject);

    xr_delete(m_ObjectProps);
    xr_delete(m_ItemProps);
    for (auto& elem : m_PList)
    {
        xr_delete(elem.second);
    }
    m_PList.clear();
    xr_delete			(m_EditPG);
    xr_delete			(m_EditPE);
}
//---------------------------------------------------------------------------

bool CParticleTool::IfModified()
{
    if (m_bModified){
        int mr = ELog.DlgMsg(mtConfirmation, mbYes|mbNo|mbCancel, "The particles has been modified.\nDo you want to save your changes?");
        switch(mr){
        case mrYes: if (!ExecCommand(COMMAND_SAVE)) return false; else m_bModified = false; break;
        case mrNo: m_bModified = false; break;
        case mrCancel: return false;
        }
    }
    return true;
}

void CParticleTool::Modified()
{
	m_bModified = true;
}
//---------------------------------------------------------------------------

void CParticleTool::OnItemModified()
{
	Modified();
    if (m_LibPED)
    	CompileEffect				();
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
}
#include "../../xrEngine/IGame_Persistent.h"

void CParticleTool::RenderEnvironment()
{
/*
    if (psDeviceFlags.is(rsEnvironment)){
        g_pGamePersistent->Environment().RenderSky	();
        g_pGamePersistent->Environment().RenderClouds	();
    }
*/    
}

void CParticleTool::Render()
{
	if (!m_bReady) return;

    PrepareLighting		();

	if (m_EditObject)	m_EditObject->RenderSingle(NULL, Fidentity);
	// draw parent axis
    DU_impl.DrawObjectAxis(m_Transform,0.05f,true);
	// draw domains
    switch(m_EditMode){
    case emNone:
        {
            break;
        }
    case emAction:
        {
            break;
        }
    case emEffect:
        {
		    if (m_EditPE&&m_EditPE->GetDefinition())
		    {
		        m_EditPE->GetDefinition()->Render(m_Transform);
		    }
            break;
        }
    case emEffectSlot:
        {
            break;
        }
    case emGroup:
        {
        	if (m_EditPG){
             	int cnt 		= m_EditPG->items.size();
                for (int k=0; k<cnt; k++){
                    PS::CParticleEffect* E		= (PS::CParticleEffect*)m_EditPG->items[k].root_effect;
    
                    if (m_LibPGD == nullptr || m_LibPGD->m_Effects[k] == nullptr)
                    {
                        continue;
                    }
    
	    			if (E&&E->GetDefinition()&&m_LibPGD->m_Effects[k]->m_Flags.is(PS::CPGDef::SEffect::flEnabled))
	    			{
	    			    E->GetDefinition()->Render(m_Transform);
	    			}
                }
            }
            break;
        }
    case emAnimCurve:
        {
            break;
        }
    default:
        {
            THROW;
        }
    }
	// Draw the particles.
    ((CRender*)::Render)->Models->RenderSingle(m_EditPG,Fidentity,1.f);
    ((CRender*)::Render)->Models->RenderSingle(m_EditPE,Fidentity,1.f);

    if (m_Flags.is(flAnimatedPath))
    {
        m_ParentAnimator->DrawPath();
    }

//.    if (psDeviceFlags.is(rsEnvironment)) g_pGamePersistent->Environment().RenderLast	();
    inherited::Render	();
}

void CParticleTool::OnFrame()
{
	if (!m_bReady) return;
	if (m_EditObject)
    	m_EditObject->OnFrame();

    switch (m_PreviewType)
    {
    case PreviewTypes::Object:
        {
            if (m_Flags.is(flAnimatedParent)){
                m_ParentAnimator->Update(EDevice->fTimeDelta);
                if (m_ParentAnimator->IsPlaying()){
                    Fvector new_vel;
                    new_vel.sub (m_ParentAnimator->XFORM().c,m_Transform.c);
                    new_vel.div (EDevice->fTimeDelta);
                    m_Vel.lerp	(m_Vel,new_vel,0.9);
                    m_Transform	= m_ParentAnimator->XFORM();
                    m_Flags.set	(flApplyParent,true);
                }
            }
            break;
        }
    case PreviewTypes::Transform:
        {
            m_Transform.identity();
            m_Transform.setHPB(m_Rotation.y, m_Rotation.x, m_Rotation.z);
            m_Transform.c = m_Position;
            m_Flags.set(flApplyParent,true);
            break;
        }
    }

    if (m_Flags.is(flRemoveAction))
    	RealRemoveAction();
	if (m_Flags.is(flApplyParent))
    	RealApplyParent();
	if (m_Flags.is(flCompileEffect))
    	RealCompileEffect();

    m_EditPE->OnFrame(EDevice->dwTimeDelta);
    m_EditPG->OnFrame(EDevice->dwTimeDelta);

	if (m_Flags.is(flRefreshProps))
    	RealUpdateProperties();

    if (m_Flags.is(flSelectEffect)){
        auto CurrentList = GetCurrentList();
        R_ASSERT(CurrentList);
        CurrentList->SelectItem(sel_eff_name.c_str());
        m_Flags.set			(flSelectEffect,false);
        sel_eff_name		= "";
    }

    xr_string tmp;
    switch(m_EditMode){
    case emNone:
        {
            break;
        }
    case emAction:
        {
            break;
        }
    case emEffect:
        {
            if (m_EditPE->IsPlaying())
            {
                xr_string nn;
                nn.resize(64);
                sprintf(nn.data(), " PE Playing...[%d]", m_EditPE->SpriteCount());
                UI->SetStatus(nn.c_str(), false);
            }
        
            else
            {
                UI->SetStatus(" Stopped.",false);
            }
            break;
        }
    case emEffectSlot:
        {
            break;
        }
    case emGroup:
        {
            if (m_EditPG->IsPlaying())
            {
                xr_string nn;
                nn.resize(64);
                sprintf(nn.data(), " PE Playing...[%d]", m_EditPG->SpriteCount());
                UI->SetStatus(nn.c_str(),false);
            }
            else
            {
                UI->SetStatus(" Stopped.",false);
            }
            break;
        }
    case emAnimCurve:
        {
            break;
        }
    default:
        {
            THROW;
        }
    }
}

void CParticleTool::ZoomObject(bool bSelOnly)
{
	VERIFY(m_bReady);
    if (!bSelOnly&&m_EditObject){
        UI->CurrentView().m_Camera.ZoomExtents(m_EditObject->GetBox());
	}else{
    	Fbox box; box.invalidate();
        switch(m_EditMode){
        case emNone:
            {
                break;
            }
        case emAction:
            {
                R_ASSERT(false);
                break;
            }
        case emEffect:
            {
                box.set(m_EditPE->vis.box);
                break;
            }
        case emEffectSlot:
            {
                R_ASSERT(false);
                break;
            }
        case emGroup:
            {
                box.set(m_EditPG->vis.box);
                break;
            }
        case emAnimCurve:
            {
                R_ASSERT(false);
                break;
            }
	    default:
            {
                THROW;
            }
        }
        if (box.is_valid()){ box.grow(1.f); UI->CurrentView().m_Camera.ZoomExtents(box); }
    }
}

void CParticleTool::PrepareLighting()
{
    // add directional light
    Flight L;
    ZeroMemory(&L,sizeof(Flight));
    L.type = D3DLIGHT_DIRECTIONAL;
    L.diffuse.set(1,1,1,1);
    L.direction.set(1,-1,1); L.direction.normalize();
	EDevice->SetLight(0,L);
	EDevice->LightEnable(0,true);

    L.diffuse.set(0.3,0.3,0.3,1);
    L.direction.set(-1,-1,-1); L.direction.normalize();
	EDevice->SetLight(1,L);
	EDevice->LightEnable(1,true);

    L.diffuse.set(0.3,0.3,0.3,1);
    L.direction.set(1,-1,-1); L.direction.normalize();
	EDevice->SetLight(2,L);
	EDevice->LightEnable(2,true);

    L.diffuse.set(0.3,0.3,0.3,1);
    L.direction.set(-1,-1,1); L.direction.normalize();
	EDevice->SetLight(3,L);
	EDevice->LightEnable(3,true);

	L.diffuse.set(1.0,0.8,0.7,1);
    L.direction.set(0,1,0); L.direction.normalize();
	EDevice->SetLight(4,L);
	EDevice->LightEnable(4,true);
}

void CParticleTool::OnDeviceCreate()
{
}

void CParticleTool::OnDeviceDestroy()
{
}

void CParticleTool::SelectPreviewObject(int p)
{

}

void CParticleTool::ResetPreviewObject()
{
	VERIFY(m_bReady);
    UI->RedrawScene();
}

bool CParticleTool::Load(const char* name)
{
	VERIFY(m_bReady);
    UpdateProperties();
    return true;
}


bool CParticleTool::Save(bool bAsXR)
{
	VERIFY			(m_bReady);

    // validate
    if (!Validate(true))
    {
        if (!bAsXR)
        {
            int RetVal = ELog.DlgMsg(mtError, mbOK | mbCancel, "Should I save only valid parts or cancel saving?");
            if (RetVal == mrCancel)
            {
                ELog.Msg(mtConfirmation, ">>> Cancel");
                return false;
            }
        }
        else
        {
            ELog.DlgMsg(mtError, "Invalid particle's found. Validate library and try again.");
            return false;
        }
    }
	bool bRes			= false;
	if(bAsXR)
    {
        bRes 			= RImplementation.PSLibrary.Save();
    }else
    {
        bRes 			= RImplementation.PSLibrary.Save2();
    }

    if (bRes)		m_bModified = false;

    return bRes;
}

void CParticleTool::Reload()
{
	VERIFY(m_bReady);
    ResetCurrent	();
	RImplementation.PSLibrary.Reload();
    // visual part
    m_EditPE->Compile(nullptr);
    m_EditPG->Compile(nullptr);

    m_ItemProps->ClearProperties();
    UpdateProperties(true);
}

void CheckEffect(const xr_string& group_path, const shared_str& eff_full_name, xr_string& res_name, bool bRenameOnly)
{
 	res_name						= group_path + "effects\\" + EFS.ExtractFileName(eff_full_name.c_str());

    if(0!=stricmp(res_name.c_str(),eff_full_name.c_str()))
    {
        PS::CPEDef* old_ped			= RImplementation.PSLibrary.FindPED(eff_full_name.c_str());
        PS::CPEDef* new_ped			= RImplementation.PSLibrary.FindPED(res_name.c_str());
        if(bRenameOnly)
        {
       		RImplementation.PSLibrary.Remove	(res_name.c_str());
            new_ped					= NULL;
        }

        if(!new_ped)
        {
            new_ped					= (bRenameOnly)? old_ped : RImplementation.PSLibrary.AppendPED(old_ped);
        	new_ped->m_Name			= res_name.c_str();
            if(bRenameOnly)
        		Msg						("rename effect [%s]->[%s]", eff_full_name.c_str(), res_name.c_str());
            else
        		Msg						("create new effect [%s]", res_name.c_str());
        }
        VERIFY( 0==stricmp(new_ped->m_Name.c_str(), res_name.c_str()) );
    }

}

CCommandVar CParticleTool::CreateGroupFromSelected(CCommandVar p1, CCommandVar p2)
{
	/*PS::CPEDef* curr = m_LibPED;
	if(!curr)
    {
    	ELog.DlgMsg	(mtError,"Select Effect first.");
        return false;
    }
    const shared_str& eff_name		= curr->m_Name;
    PS::CPGDef* pg					= AppendPG(0);

    xr_string grp_name				= eff_name.c_str();
    pg->m_Name						= grp_name.c_str();

    pg->m_fTimeLimit				= 0.0f;
    PS::CPGDef::SEffect* eff 		= xr_new<PS::CPGDef::SEffect>();
    pg->m_Effects.push_back	   		(eff);
    eff->m_EffectName				= eff_name;

    eff->m_Flags.set				(PS::CPGDef::SEffect::flEnabled,true);
    eff->m_Time0					= 0.0f;
    eff->m_Time1					= 0.0f;


    xr_string						tmp;
    xr_string group_path 			= EFS.ExtractFilePath(grp_name.c_str());
    CheckEffect						(group_path, eff->m_EffectName, tmp, true);

    eff->m_EffectName				= tmp.c_str();

    curr->m_Name					= tmp.c_str();
    
    ExecCommand						(COMMAND_UPDATE_PROPERTIES);
    
   	m_PList->SelectItem				(grp_name.c_str());*/
   
	return 							true;
}

CCommandVar CParticleTool::Compact(CCommandVar p1, CCommandVar p2)
{
    if (!Validate(true))
    {
    	ELog.DlgMsg	(mtError,"Invalid particle's found. Validate library and try again.");
        return false;
    }
    
    for (PS::PGDIt g_it= RImplementation.PSLibrary.FirstPGD(); g_it!=RImplementation.PSLibrary.LastPGD(); ++g_it)
    {
    	PS::CPGDef*	pg 		= (*g_it);
        shared_str& group_name	= pg->m_Name;
        xr_string group_path 	= EFS.ExtractFilePath(group_name.c_str());
        
        xr_vector<PS::CPGDef::SEffect*>::const_iterator pe_it 		= pg->m_Effects.begin();
        xr_vector<PS::CPGDef::SEffect*>::const_iterator pe_it_e 	= pg->m_Effects.end();

        xr_string							tmp;

        for(;pe_it!=pe_it_e;++pe_it)
        {	
        	PS::CPGDef::SEffect* Eff		= (*pe_it);
            CheckEffect						(group_path, Eff->m_EffectName, tmp, false);
            Eff->m_EffectName				= tmp.c_str();
        
            if(Eff->m_Flags.test(PS::CPGDef::SEffect::flOnPlayChild))
            {
                CheckEffect						(group_path, Eff->m_OnPlayChildName, tmp, false);
                Eff->m_OnPlayChildName			= tmp.c_str();
            }
            if(Eff->m_Flags.test(PS::CPGDef::SEffect::flOnBirthChild))
            {
                CheckEffect						(group_path, Eff->m_OnBirthChildName, tmp, false);
                Eff->m_OnBirthChildName			= tmp.c_str();
            }
            if(Eff->m_Flags.test(PS::CPGDef::SEffect::flOnDeadChild))
            {
                CheckEffect						(group_path, Eff->m_OnDeadChildName, tmp, false);
                Eff->m_OnDeadChildName			= tmp.c_str();
            }
        }
    }

    ResetCurrent		();
    UpdateProperties	(true);

	return true;
}

bool CParticleTool::Validate(bool bMsg)
{
    if (bMsg)		ELog.Msg	(mtInformation,"Begin validation...");
    PS::PEDIt _eI 	= RImplementation.PSLibrary.FirstPED();
    PS::PEDIt _eE 	= RImplementation.PSLibrary.LastPED();
    u32 error_cnt	= 0;
    for (; _eI!=_eE; ++_eI)
    {
    	if (!(*_eI)->Validate(bMsg)) 
        	error_cnt++;
    }
    for (PS::PGDIt g_it= RImplementation.PSLibrary.FirstPGD(); g_it!=RImplementation.PSLibrary.LastPGD(); ++g_it)
    {
    	PS::CPGDef*	pg 		= (*g_it);
    	if (!pg->Validate(bMsg)) 
        	error_cnt++;
	}
    for (auto elem : RImplementation.PSLibrary.VecPACDs())
    {
        if (!elem->Validate(bMsg))
        {
            error_cnt++;
        }
    }

    if (bMsg){
        if (error_cnt>0)ELog.DlgMsg	(mtError,"Validation FAILED! Found %d error's.",error_cnt);
        else			ELog.DlgMsg	(mtInformation,"Validation OK.");
    }
    return error_cnt==0;
}

/*void CParticleTool::Rename(const char* old_full_name, const char* ren_part, int level)
{
    VERIFY(level<_GetItemCount(old_full_name,'\\'));
    xr_string new_full_name;
    Rename(old_full_name, new_full_name.c_str());
}*/

void CParticleTool::Rename(UIItemListForm::Node& Node, const char* old_full_name, const char* new_full_name)
{
	VERIFY(m_bReady);
    if (!Node.Object)
    {
        return;
    }
    switch (Node.Object->Type())
    {
    case emEffect:
        {
            // is effect
            PS::CPEDef* E = RImplementation.PSLibrary.FindPED(old_full_name);
            if (E){
                RImplementation.PSLibrary.RenamePED(E,new_full_name);
            }
            break;
        }
    case emGroup:
        {
            // is group
            PS::CPGDef* G = RImplementation.PSLibrary.FindPGD(old_full_name);
            if (G){
                RImplementation.PSLibrary.RenamePGD(G,new_full_name);
            }
            break;
        }
    case emAction:
        {
            auto PA = (EParticleAction*)(Node.Object->m_Object);
            string_path buffer;
            PA->actionName = _GetItem(new_full_name, _GetItemCount(new_full_name, '\\')-1, buffer, '\\');
        }
    case emAnimCurve:
        {
            PS::CPACDef* PAC = RImplementation.PSLibrary.FindPACD(old_full_name);
            if (PAC)
            {
                RImplementation.PSLibrary.RenamePACD(PAC,new_full_name);
            }
            break;
        }
    }
}

void CParticleTool::Remove(UIItemListForm::Node& Node)
{
    if (!Node.Object)
    {
        return;
    }
    switch (Node.Object->Type())
    {
    case emEffect:
        {
            auto RealObj = (PS::CPEDef*)(Node.Object->m_Object);
            if (RImplementation.PSLibrary.FindPED(RealObj->Name()) == m_LibPED)
            {
                m_ItemProps->ClearProperties();
            }
            VERIFY(m_bReady);
            SetCurrentPE(0);
            SetCurrentPG(0);
            RImplementation.PSLibrary.Remove(RealObj->Name());
            break;
        }
    case emGroup:
        {
            auto RealObj = (PS::CPGDef*)(Node.Object->m_Object);
            if (RImplementation.PSLibrary.FindPGD(RealObj->m_Name.c_str()) == m_LibPGD)
            {
                m_ItemProps->ClearProperties();
            }

            VERIFY(m_bReady);
            SetCurrentPE(0);
            SetCurrentPG(0);
            RImplementation.PSLibrary.Remove(RealObj->m_Name.c_str());
            break;
        }
    case emAnimCurve:
        {
            auto RealObj = (PS::CPACDef*)(Node.Object->m_Object);
            VERIFY(m_bReady);
            RImplementation.PSLibrary.Remove(RealObj->getName());
            break;
        }
    case emAction:
    case emEffectSlot:
        {
            VERIFY(m_bReady);
            break;
        }
    }
    
}

void CParticleTool::RemoveCurrent()
{
    auto CurrentList = GetCurrentList();
    R_ASSERT(CurrentList);
    CurrentList->RemoveSelectItem();
}

void CParticleTool::CloneCurrent()
{
    auto CurrentList = GetCurrentList();
    R_ASSERT(CurrentList);
    auto& Items = CurrentList->m_SelectedItems;

    if (!Items.empty()) 
    {
        auto Item = Items[0];

        PS::CPEDef* PE = FindPE(Item->Key());

        xr_string CloneName = Item->Key();
        CloneName += "_clone";

        if (PE)
        {
            AppendPE(PE, CloneName.c_str());
            Modified();
        }
        else
        {
            PS::CPGDef* PG = FindPG(Item->Key());
            if (PG) 
            {
                AppendPG(PG, CloneName.c_str());
                Modified();
            }
        }
    }
    else {
        ELog.DlgMsg(mtInformation, "At first select object.");
    }
}

void CParticleTool::ResetCurrent()
{
	VERIFY(m_bReady);
    if (m_LibPED) m_EditPE->Stop(false);
    if (m_LibPGD) m_EditPG->Stop(false);
    m_LibPED= 0;
    m_LibPGD= 0;
    m_CurrentEf = nullptr;
    m_CurrentPA = nullptr;
    m_LibAC = nullptr;
}

void CParticleTool::SetCurrentPE(PS::CPEDef* P, EParticleAction* CurrentPA)
{
	VERIFY(m_bReady);
    m_EditPG->Compile		(0);
    m_CurrentEf = nullptr;
    m_LibAC = nullptr;
	if (m_LibPED!=P){
	    m_LibPED = P;
        m_EditPE->Compile	(m_LibPED);
		if (m_LibPED)
			m_EditMode		= emEffect;
    }
    if (m_CurrentPA != CurrentPA)
    {
        m_CurrentPA = CurrentPA;
        m_EditMode = emAction;
    }
}

void CParticleTool::SetCurrentPG(PS::CPGDef* P, PS::CPGDef::SEffect* Ef)
{
	VERIFY(m_bReady);
	m_EditPE->Compile		(0);
    m_CurrentPA = nullptr;
    m_LibAC = nullptr;
	if (m_LibPGD!=P){
	    m_LibPGD = P;
        m_EditPG->Compile	(m_LibPGD);
        if (m_LibPGD)
			m_EditMode		= emGroup;
	}
    if (m_CurrentEf != Ef)
    {
        m_CurrentEf = Ef;
        m_EditMode = emEffectSlot;
    }
}

void CParticleTool::DrawReferenceList()
{
    switch (m_EditMode)
    {
    case emEffectSlot:
        {
            R_ASSERT(false);
            break;
        }
    case emGroup:
        {
            if (m_EditPG->GetDefinition())
            {
                xr_vector<PS::CPGDef::SEffect*>::const_iterator pe_it = m_EditPG->GetDefinition()->m_Effects.begin();
                xr_vector<PS::CPGDef::SEffect*>::const_iterator pe_it_e = m_EditPG->GetDefinition()->m_Effects.end();
                for (; pe_it != pe_it_e; ++pe_it)
                {
                    ImGui::Text((*pe_it)->m_EffectName.c_str()? (*pe_it)->m_EffectName.c_str() :0);
                }
                if (m_EditPG->GetDefinition()->m_Flags.test(PS::CPGDef::SEffect::flOnPlayChild))
                    ImGui::Text((*pe_it)->m_OnPlayChildName.c_str());
                if (m_EditPG->GetDefinition()->m_Flags.test(PS::CPGDef::SEffect::flOnBirthChild))
                    ImGui::Text((*pe_it)->m_OnBirthChildName.c_str());
                if (m_EditPG->GetDefinition()->m_Flags.test(PS::CPGDef::SEffect::flOnDeadChild))
                    ImGui::Text((*pe_it)->m_OnDeadChildName.c_str());
            }
            break;
        }
    case emAction:
        {
            R_ASSERT(false);
            break;
        }
    case emEffect:
        {
            if (m_EditPE->GetDefinition())
            {
                PS::PGDIt G = RImplementation.PSLibrary.FirstPGD();
                PS::PGDIt G_e = RImplementation.PSLibrary.LastPGD();
                for (; G != G_e; ++G)
                {
                    PS::CPGDef* def = (*G);
                    PS::CPGDef::EffectIt pe_it = def->m_Effects.begin();
                    PS::CPGDef::EffectIt pe_it_e = def->m_Effects.end();
                    for (; pe_it != pe_it_e; ++pe_it)
                    {
                        if ((*pe_it)->m_EffectName == m_EditPE->Name())
                        {
                            ImGui::Text(def->m_Name.c_str());
                        }
                        else if ((*pe_it)->m_OnPlayChildName == m_EditPE->Name())
                        {
                            ImGui::Text(def->m_Name.c_str());
                        }
                        else if ((*pe_it)->m_OnBirthChildName == m_EditPE->Name())
                        {
                            ImGui::Text(def->m_Name.c_str());
                        }
                        else if ((*pe_it)->m_OnDeadChildName == m_EditPE->Name())
                        {
                            ImGui::Text(def->m_Name.c_str());
                        }
                    }
                }
            }
            break;
        }
    case emAnimCurve:
        {
            R_ASSERT(false);
            break;
        }
    }
}

PS::CPACDef* CParticleTool::FindPAC(const char* name)
{
	return RImplementation.PSLibrary.FindPACD(name);
}

PS::CPACDef* CParticleTool::AppendPAC(PS::CPACDef* src, const char* path)
{
    VERIFY(m_bReady);
    PS::CPACDef* S 		= RImplementation.PSLibrary.AppendPACD(src);
    S->setName(path);

    ExecCommand			(COMMAND_UPDATE_PROPERTIES,true);
    SelectListItem(0, path,true,false,true);
    return S;
}

void CParticleTool::SetCurrentPAC(PS::CPACDef* P)
{
    VERIFY(m_bReady);
    m_EditPE->Compile(nullptr);
    m_EditPG->Compile(nullptr);
    m_CurrentPA = nullptr;
    m_CurrentEf = nullptr;
    m_LibAC = P;
    m_EditMode = emAnimCurve;
}

void CParticleTool::EditPAC(PS::CPACDef* PAC)
{
    UIPACEditorForm::Open(PAC);
}


void CParticleTool::CommandJumpToItem()
{
  /* for(int i=0; i<fraLeftBar->refLB->Count; ++i)
   {
        if(fraLeftBar->refLB->Selected[i])
        {
        	m_PList->SelectItem((fraLeftBar->refLB->Items->Strings[i]).c_str(),true,false,true);
        	break;
        }
    }*/
}

void CParticleTool::ImportPE()
{
    xr_string Path;
    if (EFS.GetOpenName(_server_data_root_, Path, false, 0, -1, "*.pe"))
    {
        PS::CPEDef* def = new PS::CPEDef();
        FS.TryLoad(Path);

        CInifile ini(Path.c_str(), true, true, false);
        if (def->Load2(ini))
        {
            AppendPE(def, xr_path(Path.c_str()).xfilename().c_str());
        }
    }
}

PS::CPEDef*	CParticleTool::FindPE(const char* name)
{
	return RImplementation.PSLibrary.FindPED(name);
}

PS::CPGDef*	CParticleTool::FindPG(const char* name)
{
	return RImplementation.PSLibrary.FindPGD(name);
}

void CParticleTool::PlayCurrent(int idx)
{
	VERIFY(m_bReady);
    StopCurrent(false);
    switch(m_EditMode){
    case emNone:
    case emAnimCurve:
        {
            break;
        }
    case emEffect:
        {
            m_EditPE->Play();
            break;
        }
    case emEffectSlot:
        {
            VERIFY(m_CurrentEf);
            for (auto& item : m_EditPG->items)
            {
                VERIFY(item.root_effect);
                auto CastedEffect = (PS::CParticleEffect*)item.root_effect;
                if (CastedEffect->m_Def->Name() == m_CurrentEf->m_EffectName)
                {
                    m_LibPED = CastedEffect->GetDefinition();
                    m_EditPE->Compile(m_LibPED);
                    m_EditPE->Play();
                    break;
                }
            }
            break;
        }
    case emGroup:
        {
            if (idx>-1){
                VERIFY(idx<(int)m_EditPG->items.size());
                m_LibPED = ((PS::CParticleEffect*)m_EditPG->items[idx].root_effect)->GetDefinition();
                m_EditPE->Compile(m_LibPED);
                m_EditPE->Play();
            }else{
                // play all
                m_EditPG->Play();
            }
            break;
        }
    default:
        {
            THROW;
        }
    }
    ApplyParent		();
}

void CParticleTool::StopCurrent(bool bFinishPlaying)
{
	VERIFY(m_bReady);
    m_EditPE->Stop(bFinishPlaying);
    m_EditPG->Stop(bFinishPlaying);
}

void CParticleTool::SelectEffect(const char* name)
{
	sel_eff_name 	= name;
    m_Flags.set		(flSelectEffect,true);
}

void CParticleTool::OnShowHint(AStringVec& SS)
{
}

float m_MoveSnap = 1;
bool CParticleTool::MouseStart(TShiftState Shift)
{
	inherited::MouseStart(Shift);
	switch(m_Action)
    {
        case etaSelect:
        break;
        case etaAdd:
        break;
        case etaMove:
        {
            if (Shift | ssCtrl)
            {
                if (m_EditObject)
                {
                    float dist = UI->ZFar();
                    SRayPickInfo pinf;
                    if (m_EditObject->RayPick(dist, UI->m_CurrentRStart, UI->m_CurrentRDir, Fidentity, &pinf))
                        m_Transform.c.set(pinf.pt);
                }
                else
                {
                    // pick grid
                    Fvector normal = { 0.f, 1.f, 0.f };
                    float clcheck = UI->m_CurrentRDir.dotproduct(normal);
                    if (fis_zero(clcheck)) return false;
                    float alpha = -UI->m_CurrentRStart.dotproduct(normal) / clcheck;
                    if (alpha <= 0) return false;

                    m_Transform.c.mad(UI->m_CurrentRStart, UI->m_CurrentRDir, alpha);

                    if (m_Settings.is(etfGSnap))
                    {
                        m_Transform.c.x = snapto(m_Transform.c.x, m_MoveSnap);
                        m_Transform.c.z = snapto(m_Transform.c.z, m_MoveSnap);
                        m_Transform.c.y = 0.f;
                    }
                }
            }
        }
        break;
        case etaRotate:
        break;
        case etaScale:
        break;
    }
    ApplyParent();
	return m_bHiddenMode;
}

bool CParticleTool::MouseEnd(TShiftState Shift)
{
	inherited::MouseEnd(Shift);
	return true;
}

void CParticleTool::MouseMove(TShiftState Shift)
{
    inherited::MouseMove(Shift);
    switch (m_Action)
    {
        case etaSelect:
        break;
        case etaAdd:
        break;
        case etaMove:
        m_Transform.c.add(m_MovedAmount);
        break;
        case etaRotate:
        {
            Fmatrix mR; mR.identity();
            if (!fis_zero(m_RotateVector.x))
                mR.rotateX(m_RotateAmount);
            else if (!fis_zero(m_RotateVector.y))
                mR.rotateY(m_RotateAmount);
            else if (!fis_zero(m_RotateVector.z))
                mR.rotateZ(m_RotateAmount);
            m_Transform.mulB_43(mR);
        }
        break;
        case etaScale:
        break;
    }
    ApplyParent();
}
//------------------------------------------------------------------------------

void CParticleTool::RealApplyParent()
{
    switch(m_EditMode){
    case emNone:
        {
            break;
        }
    case emAction:
        {
            R_ASSERT(false);
            break;
        }
    case emEffect:
        {
            m_EditPE->UpdateParent(m_Transform,m_Vel,m_Flags.is(flSetXFORM));
            break;
        }
    case emEffectSlot:
    case emGroup:
        {
            m_EditPG->UpdateParent(m_Transform,m_Vel,m_Flags.is(flSetXFORM));
            break;
        }
    case emAnimCurve:
        {
            break;
        }
    default:
        {
            THROW;
        }
    }
	m_Flags.set		(flApplyParent,false);
}

void CParticleTool::RealCompileEffect()
{
	if (m_LibPED)    m_LibPED->Compile(m_LibPED->m_EActionList);
	m_Flags.set		(flCompileEffect,false);
}

void CParticleTool::RealRemoveAction()
{
    if (m_LibPED)
    {
        xr_delete(m_LibPED->m_EActionList[remove_action_num]);
        m_LibPED->m_EActionList.erase(m_LibPED->m_EActionList.begin() + remove_action_num);

        RealCompileEffect();
    }
	m_Flags.set(flRemoveAction,false);
}

const char* CParticleTool::GetInfo()
{
	return 0;
}
//------------------------------------------------------------------------------

void CParticleTool::SelectListItem(const char* pref, const char* name, bool bVal, bool bLeaveSel, bool bExpand)
{
	xr_string nm = (name&&name[0])?PrepareKey(pref,name).c_str():pref;
    UIItemListForm* List = GetCurrentList();
    R_ASSERT(List);
	List->SelectItem(nm.c_str());
	if (pref){
    	List->SelectItem(pref);
    }
}
//------------------------------------------------------------------------------

PS::CPEDef* CParticleTool::AppendPE(PS::CPEDef* src, const char* path)
{
    VERIFY(m_bReady);
    PS::CPEDef* S = RImplementation.PSLibrary.AppendPED(src);
    S->m_Name = path;
    ExecCommand(COMMAND_UPDATE_PROPERTIES, true);
    SelectListItem(0, path, true, false, true);
    return S;
}

PS::CPGDef*	CParticleTool::AppendPG(PS::CPGDef* src, const char* path)
{
	VERIFY(m_bReady);
	PS::CPGDef* S 		= RImplementation.PSLibrary.AppendPGD(src);
    S->m_Name			= path;

    ExecCommand			(COMMAND_UPDATE_PROPERTIES,true);
    SelectListItem(0, path,true,false,true);
    return S;
}

#include "../xrECore/Editor/EditMesh.h"

bool CParticleTool::RayPick(const Fvector& start, const Fvector& dir, float& dist, Fvector* pt, Fvector* n)
{
    if (m_EditObject){
		SRayPickInfo pinf;
		if (m_EditObject->RayPick(dist,start,dir,Fidentity,&pinf)){
        	if (pt) pt->set(pinf.pt);
            if (n){
                const Fvector* PT[3];
                pinf.e_mesh->GetFacePT(pinf.inf.tris_id, PT);
            	n->mknormal(*PT[0],*PT[1],*PT[2]);
            }
            return true;
        }else return false;
    }else{
    	Fvector np; np.mad(start,dir,dist);
    	if ((start.y>0)&&(np.y<0.f)){
            if (pt) pt->set(start);
            if (n)	n->set(0.f,1.f,0.f);
            return true;
        }else return false;
    }
}

void CParticleTool::OnChangeMotion	(PropValue* sender)
{
	ChooseValue* V 			= dynamic_cast<ChooseValue*>(sender);
    if (V){
        m_ParentAnimator->Clear		();
        if (V->value->size())
            m_ParentAnimator->Load	(V->value->c_str());
    }
    if (m_Flags.is(flAnimatedParent))
		m_ParentAnimator->Play	(true);
    FillObjectPrefs();
}

void CParticleTool::OnChangeObject(PropValue* sender)
{
    ChooseValue* V = dynamic_cast<ChooseValue*>(sender);
    if (V)
    {
        Lib.RemoveEditObject(m_EditObject);
        m_EditObject = V->value->c_str() ? Lib.CreateEditObject(V->value->c_str()) : 0;
        //	ZoomObject(true); 

        UI->RedrawScene();
    }
    FillObjectPrefs();
}

void CParticleTool::FillObjectPrefs()
{
	PropItemVec		items;
    m_MotionName = m_ParentAnimator->Name();

    static xr_token PreviewTypesArr[] = {
        {"Object", (int)PreviewTypes::Object},
        {"Transform", (int)PreviewTypes::Transform},
        {0, 0}
    };
    
    PHelper().CreateToken8(items, "Preview Type", (u8*)&m_PreviewType, PreviewTypesArr)
        ->OnChangeEvent.bind(this, &CParticleTool::OnChangeObject);
    PHelper().CreateChoose(items, "Object\\Mesh", &m_ObjectName, smObject)
        ->OnChangeEvent.bind(this, &CParticleTool::OnChangeObject);
    PHelper().CreateFlag32(items, "Object\\Parent\\Allow Animated", &m_Flags, flAnimatedParent)
        ->OnChangeEvent.bind(this,&CParticleTool::OnChangeMotion);
    PHelper().CreateFlag32(items, "Object\\Parent\\Draw Path", &m_Flags, flAnimatedPath);
    PHelper().CreateChoose(items, "Object\\Parent\\Motion", &m_MotionName, smGameAnim)
        ->OnChangeEvent.bind(this,&CParticleTool::OnChangeMotion);
    PHelper().CreateFloat(items, "Object\\Parent\\Motion Speed", &m_ParentAnimator->Speed(), 0.f, 10000.f);

    PHelper().CreateVector(items, "Transform\\Position", &m_Position, flt_min, flt_max)
        ->OnChangeEvent.bind(this, &CParticleTool::OnChangeObject);
    PHelper().CreateAngle3(items, "Transform\\Rotation", &m_Rotation, -180, 180)
        ->OnChangeEvent.bind(this,&CParticleTool::OnChangeObject);
    
    m_ObjectProps->AssignItems(items);
}

bool CParticleTool::GetSelectionPosition	(Fmatrix& result)
{
	result = m_Transform;
	return true;
}


void CParticleTool::OnDrawUI()
{
    if (m_LibPED)m_LibPED->OnDrawUI();
    if (m_CreatingParticle)
    {
        bool change;
        shared_str result;
        if (UIChooseForm::GetResult(change, result))
        {
            if (change)
            {
                if (result == "Effect")
                {
                    AppendPE(0, m_CreatingParticlePath.c_str());
                }
                else if (result == "Group")
                {
                    AppendPG(0, m_CreatingParticlePath.c_str());
                }
                else if (result == "AnimCurve")
                {
                    AppendPAC(0, m_CreatingParticlePath.c_str());
                } else
                {
                    R_ASSERT(false, "Invalid choose result type!", result.c_str());
                }
            }
            m_CreatingParticle = false;
        }
        UIChooseForm::Update();
    }
    UIPACEditorForm::Update();
}

void CParticleTool::FillChooseParticleType(ChooseItemVec& items, void* param)
{
    items.push_back(SChooseItem("Effect", "Effect Particle"));
    items.push_back(SChooseItem("Group", "Group Particle"));
    items.push_back(SChooseItem("AnimCurve", "Particle Animation Curve"));
}

void CParticleTool::OnParticleCreateItem(const char* path)
{
    UIChooseForm::SelectItem(smCustom, 1, 0, TOnChooseFillItems(this, &CParticleTool::FillChooseParticleType), 0, 0, 0, 0);
    m_CreatingParticle = true;
    m_CreatingParticlePath = path;
}

void CParticleTool::OnParticleCloneItem(const char* parent_path, const char* new_full_name)
{
    PS::CPEDef* PE = FindPE(parent_path);
    if (PE) 
    {
        AppendPE(PE, new_full_name);
        Modified();
    }
    else
    {
        PS::CPGDef* PG = FindPG(parent_path);
        if (PG)
        {
            AppendPG(PG, new_full_name);
            Modified();
        }
    }
}

bool CParticleTool::VerifyParticleCreateItem(UIItemListForm::Node* Node)
{
    if (!Node)
    {
        return false;
    }
    return !Node->Object || !Node->Object->m_Object;
}

bool CParticleTool::VerifyParticleCreateFolder(UIItemListForm::Node* Node)
{
    if (!Node)
    {
        return false;
    }
    return !Node->Object || !Node->Object->m_Object;
}

bool CParticleTool::VerifyParticleRenameItem(UIItemListForm::Node* Node)
{
    if (!Node)
    {
        return false;
    }
    if (Node->Type == FolderHelper<ListItem, true>::FNT_Folder)
    {
        return true;
    }
    if (!Node->Object)
    {
        return false;
    }
    switch (Node->Object->Type())
    {
    case emEffectSlot:
        {
            return false;
        }
    default:
        {
            return true;
        }
    }
}

bool CParticleTool::VerifyParticleMoveItem(UIItemListForm::Node* Node)
{
    if (!Node || !Node->Object)
    {
        return false;
    }
    switch (Node->Object->Type())
    {
    case emEffectSlot:
        {
            return false;
        }
    default:
        {
            return true;
        }
    }
}

ENodeMoveActionSlot CParticleTool::GetItemMoveActionSlot(UIItemListForm::Node* Node)
{
    if (!Node || !Node->Object)
    {
        return ENodeMoveActionSlot::Default;
    }
    switch (Node->Object->Type())
    {
        case emAction:
            {
                return ENodeMoveActionSlot::First;
            }
        default:
            {
                return ENodeMoveActionSlot::Default;
            }
    }
}

bool CParticleTool::ActionItemMoveAction(UIItemListForm::Node* Node)
{
    bool IsProcessed = false;
    EParticleAction* Action = (EParticleAction*)Node->Object->m_Object;
    PS::CPEDef* Effect = Action->parent;
    R_ASSERT(Effect);
    if (ImGui::Button("Up"))
    {
        if (Effect->MoveUpAction(Action))
        {
            IsProcessed = true;
        }
    }
    if (ImGui::IsItemHovered())
    {
        ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
    }
    ImGui::SameLine();
    if (ImGui::Button("Down"))
    {
        if (Effect->MoveDownAction(Action))
        {
            IsProcessed = true;
        }
    }
    if (ImGui::IsItemHovered())
    {
        ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
    }
    if (IsProcessed)
    {
        ImGui::CloseCurrentPopup();
    }
    return IsProcessed;
}

void CParticleTool::OnParticleItemRename(UIItemListForm::Node& Node, const char* old_name, const char* new_name, EItemType type)
{
    Rename(Node, old_name, new_name);
    Modified();
}

bool CParticleTool::OnParticlePreItemRemove(UIItemListForm::Node& Node)
{
    
    if (!Node.Object)
    {
        return true;
    }
    switch (Node.Object->Type())
    {
    case emEffect:
    case emGroup:
    case emAnimCurve:
        {
            return true;
        }
    case emAction:
        {
            auto RealObj = (EParticleAction*)(Node.Object->m_Object);
            auto PE = RealObj->parent;
            return PE->RemoveAction(RealObj);
        }
    case emEffectSlot:
        {
            auto RealObj = (PS::CPGDef::SEffect*)(Node.Object->m_Object);
            auto PG = RealObj->parent;
            return PG->RemoveEffect(RealObj);
        }
    }
}

void CParticleTool::OnParticleItemRemove(UIItemListForm::Node& Node)
{
    Remove(Node);
    Modified();
}

void  CParticleTool::OnControlClick(ButtonValue* sender, bool& bDataModified, bool& bSafe)
{
    m_Transform.identity();
    bDataModified = false;
}

/*const char* CParticleTool::InsertBeforeLast(LPSTR buffer, u32 buf_size, const char* path, const char* insert_str)
{
    xr_string builder;
    auto ItemCount = _GetItemCount(path, '\\');
    for (int i = 0; i < ItemCount-1; ++i)
    {
        builder.append(_GetItem(path, i, buffer, buf_size, '\\'));
        builder.append("\\");
    }
    builder.append(insert_str);
    builder.append(_GetItem(path, ItemCount-1, buffer, buf_size, '\\'));
    xr_strcpy(buffer, buf_size, builder.c_str());
    return buffer;
}*/

/*EEditMode CParticleTool::GetAffectedItemType(const char* path)
{
    string_path buffer;
    xr_string Item = _GetItem(path, _GetItemCount(path, '\\')-1, buffer, sizeof(buffer), '\\');
    if (Item.StartWith("[PE"))
    {
        return emEffect;
    }
    if (Item.StartWith("[PG"))
    {
        return emGroup;
    }
    if (Item.StartWith("[ACTION"))
    {
        return emAction;
    }
    if (Item.StartWith("[EFFECT"))
    {
        return emEffectSlot;
    }
    return emNone;
}*/

void CParticleTool::OnParticleItemFocused(ListItem* items)
{
    PropItemVec props;
    m_EditMode = emEffect;

    ButtonValue* B;
    B = PHelper().CreateButton(props, "Transform\\Edit", "Reset", ButtonValue::flFirstOnly);
    B->OnBtnClickEvent = ButtonValue::TOnBtnClick(this, &CParticleTool::OnControlClick);

    // TODO: Is the difference is whether to apply full transform (if set) or only position (if not)?
    PHelper().CreateFlag32(props, "Transform\\Type", &m_Flags, flSetXFORM, "Update", "Set");

    // reset to default
    ResetCurrent();

    if (items) {

        ListItem* item = items;
        if (item) {
            m_EditMode = EEditMode(item->Type());
            switch (m_EditMode) {
            case emAction:
                {
                    auto PA = (EParticleAction*)item->m_Object;
                    PS::CPEDef* def = PA->parent;
                    R_ASSERT(def);
                    SetCurrentPE(def, PA);
                    PA->FillPropInit(props, "");
                    break;
                }
            case emEffect:
                {
                    PS::CPEDef* def = ((PS::CPEDef*)item->m_Object);
                    SetCurrentPE(def);
                    def->FillProp(EFFECT_PREFIX, props, item);
                    break;
                }
            case emEffectSlot:
                {
                    auto slot = (PS::CPGDef::SEffect*)item->m_Object;
                    auto def = slot->parent;
                    R_ASSERT(def);
                    SetCurrentPG(def, slot);
                    slot->FillPropInit(props, "");
                    break;
                }
            case emGroup:
                {
                    PS::CPGDef* def = ((PS::CPGDef*)item->m_Object);
                    SetCurrentPG(def);
                    def->FillProp(GROUP_PREFIX, props, item);
                    break;
                }
            case emAnimCurve:
                {
                    auto def = (PS::CPACDef*)item->m_Object;
                    def->FillProp(ANIM_CURVE_PREFIX, props, item);
                    SetCurrentPAC(def);
                    break;
                }
            default:
                {
                    THROW;
                }
            }
        }

    }

    m_ItemProps->ClearProperties();
    m_ItemProps->AssignItems(props);

    UI->RedrawScene();
}

bool CParticleTool::VerifyParticleCloneItem(UIItemListForm::Node* Node)
{
    if (!Node || !Node->Object)
    {
        return false;
    }
    auto type = Node->Object->Type();
    return type == emEffect || type == emGroup;
}

extern ECORE_API xr_string _item_to_select_after_edit;

void CParticleTool::RealUpdateProperties()
{
    static string1024 buffer;
    //static string256 buffer2;
    
    m_Flags.set(flRefreshProps, false);

    // Make path functions
    //auto MakePGPathFunc = [&](const char* OrigName)
    //{
    //    return InsertBeforeLast(buffer, sizeof(buffer), OrigName, "[PG] ");
    //};
    //auto MakePEPathFunc = [&](const char* OrigName)
    //{
    //    return InsertBeforeLast(buffer, sizeof(buffer), OrigName, "[PE] ");
    //};
    // Add functions
    auto AddAllPEFunc = [&](ListItemsVec& items) -> ListItemsVec&
    {
        PS::PEDIt Pe = RImplementation.PSLibrary.FirstPED();
        PS::PEDIt Ee = RImplementation.PSLibrary.LastPED();
        for (; Pe != Ee; Pe++) {
            ListItem* I = LHelper().CreateItem(items,
                *(*Pe)->m_Name,
                emEffect,
                0,
                *Pe);
            I->SetIcon(1);
            I->SetIconStr(ICON_FA_FIRE);
            //xr_string ModifiedPath = buffer;
            for (auto Action : (*Pe)->m_EActionList)
            {
                xr_sprintf(buffer, "%s\\%s", *(*Pe)->m_Name, Action->actionName.c_str());
                I = LHelper().CreateItem(items, buffer, emAction, 0, Action );
                I->SetIconStr(ICON_FA_BOLT);
            }
        }
        return items;
    };
    auto AddAllPGFunc = [&](ListItemsVec& items) -> ListItemsVec&
    {
        PS::PGDIt Pg = RImplementation.PSLibrary.FirstPGD();
        PS::PGDIt Eg = RImplementation.PSLibrary.LastPGD();
        for (; Pg != Eg; Pg++) {
            ListItem* I = LHelper().CreateItem(
                items,
                *(*Pg)->m_Name,
                emGroup,
                0,
                *Pg);
            I->SetIcon(2);
			I->SetIconStr(ICON_FA_LAYER_GROUP);
            for (auto Effect : (*Pg)->m_Effects)
            {
                const char* EffectName = nullptr;
                if (Effect->m_EffectName.c_str())
                {
                    EffectName = _GetItem(
                    Effect->m_EffectName.c_str(),
                    _GetItemCount(Effect->m_EffectName.c_str(), '\\') - 1,
                    buffer,
                    sizeof(buffer),
                    '\\');
                } else
                {
                    static shared_str InvalidEffectName = "invalid";
                    EffectName = InvalidEffectName.c_str();
                }
                xr_string EffectNameBuilder = *(*Pg)->m_Name;
                EffectNameBuilder.append("\\");
                EffectNameBuilder.append(EffectName);
                I = LHelper().CreateItem(items, EffectNameBuilder.c_str(), emEffectSlot, 0, Effect );
                I->SetIconStr(ICON_FA_FLASK);
            }
        }
        return items;
    };
    auto AddAllPACFunc = [&](ListItemsVec& items) -> ListItemsVec&
    {
        for (auto elem : RImplementation.PSLibrary.VecPACDs())
        {
            ListItem* I = LHelper().CreateItem(
                items,
                elem->getName(),
                emAnimCurve,
                0,
                elem);
            I->SetIcon(3);
            I->SetIconStr(ICON_FA_BEZIER_CURVE);
        }
        return items;
    };
    // Select selected functions
    auto SelectCurrentPEFunc = [&](UIItemListForm* List)
    {
        if (m_EditPE && m_EditPE->GetDefinition())
        {
            if (m_CurrentPA)
            {
                xr_sprintf(buffer, "%s\\%s", m_EditPE->Name().c_str(), m_CurrentPA->actionName.c_str());
                List->SelectItem(buffer);
            } else
            {
                List->SelectItem(m_EditPE->Name().c_str());
            }
        }
    };
    auto SelectCurrentPGFunc = [&](UIItemListForm* List)
    {
        if (m_EditPG && m_EditPG->GetDefinition())
        {
            if (m_CurrentEf)
            {
                auto PGDef = m_EditPG->GetDefinition();
                xr_string EffectNameBuilder = *PGDef->m_Name;
                EffectNameBuilder.append("\\");
                EffectNameBuilder.append(
                        _GetItem(
                            m_CurrentEf->m_EffectName.c_str(),
                            _GetItemCount(m_CurrentEf->m_EffectName.c_str(),'\\')-1,
                            buffer,
                            sizeof(buffer),
                            '\\'));
                List->SelectItem(EffectNameBuilder.c_str());
            } else
            {
                List->SelectItem(m_EditPG->Name().c_str());
            }
        }
    };

    {
        auto List = m_PList[PEd::ListTypeBase(PEd::LisType::All)];
        ListItemsVec items;
        List->AssignItems(
            AddAllPACFunc(AddAllPGFunc(AddAllPEFunc(items))),
            nullptr,
            true);
        if (_item_to_select_after_edit.size())
        {
            List->SelectItem(_item_to_select_after_edit.c_str());
            _item_to_select_after_edit = "";
        } else
        {
            SelectCurrentPEFunc(List);
            SelectCurrentPGFunc(List);
        }
    }
    {
        auto List = m_PList[PEd::ListTypeBase(PEd::LisType::Groups)];
        ListItemsVec items;
        List->AssignItems(
            AddAllPGFunc(items),
            nullptr,
            true);
        if (_item_to_select_after_edit.size())
        {
            List->SelectItem(_item_to_select_after_edit.c_str());
            _item_to_select_after_edit = "";
        } else
        {
            SelectCurrentPGFunc(List);
        }
    }
    {
        auto List = m_PList[PEd::ListTypeBase(PEd::LisType::Effects)];
        ListItemsVec items;
        List->AssignItems(
            AddAllPEFunc(items),
            nullptr,
            true);
        if (_item_to_select_after_edit.size())
        {
            List->SelectItem(_item_to_select_after_edit.c_str());
            _item_to_select_after_edit = "";
        } else
        {
            SelectCurrentPEFunc(List);
        }
    }
    {
        auto List = m_PList[PEd::ListTypeBase(PEd::LisType::AnimCurve)];
        ListItemsVec items;
        List->AssignItems(
            AddAllPACFunc(items),
            nullptr,
            true
            );
        if (_item_to_select_after_edit.size())
        {
            List->SelectItem(_item_to_select_after_edit.c_str());
            _item_to_select_after_edit = "";
        }
    }
}

