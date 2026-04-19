//---------------------------------------------------------------------------
#include "stdafx.h"


#include "../../Layers/xrRender/PSLibrary.h"
#include "../../Layers/xrRender/ParticleEffect.h"
#include "../../Layers/xrRender/ParticleGroup.h"
#include "../../Layers/xrRender/ParticleAnimCurve.h"

#include "ParticleEffectActions.h"
#include "../xrEProps/FolderLib.h"
#include "UI_ToolsCustom.h"

ECORE_API xr_token2* actions_token = nullptr;

bool PS::CPEDef::Equal(const CPEDef* pe)
{
    if (!m_Flags.equal(pe->m_Flags)) 						return false;
    if (!m_ShaderName.equal(pe->m_ShaderName)) 				return false;
    if (!m_TextureName.equal(pe->m_TextureName))			return false;
    if (0!=memcmp(&m_Frame,&pe->m_Frame,sizeof(m_Frame))) 	return false;
	if (!fsimilar(m_fTimeLimit,pe->m_fTimeLimit))			return false;
    if (m_MaxParticles!=pe->m_MaxParticles)					return false;
    if (m_Actions.size()!=pe->m_Actions.size())				return false;
    if (!m_VelocityScale.similar(pe->m_VelocityScale))		return false;
	if (!fsimilar(m_fCollideOneMinusFriction,pe->m_fCollideOneMinusFriction))	return false;
    if (!fsimilar(m_fCollideResilience,pe->m_fCollideResilience))				return false;
    if (!fsimilar(m_fCollideSqrCutoff,pe->m_fCollideSqrCutoff))					return false;
    return true;
}

void PS::CPEDef::Copy(const CPEDef& src)
{
    m_Name				= src.m_Name; 
    m_Flags				= src.m_Flags;
    m_ShaderName		= src.m_ShaderName;
    m_TextureName		= src.m_TextureName;
    m_Frame				= src.m_Frame;
	m_fTimeLimit		= src.m_fTimeLimit;
    m_MaxParticles		= src.m_MaxParticles;
	m_CachedShader		= src.m_CachedShader;
    m_VelocityScale.set			(src.m_VelocityScale);
	m_fCollideOneMinusFriction	= src.m_fCollideOneMinusFriction;
    m_fCollideResilience		= src.m_fCollideResilience;
    m_fCollideSqrCutoff			= src.m_fCollideSqrCutoff; 

    m_EActionList.resize(src.m_EActionList.size());
    for (u32 k=0; k<src.m_EActionList.size(); k++){
        PAPI::PActionEnum type 	= src.m_EActionList[k]->type;
        m_EActionList[k]		= pCreateEAction(type);
        *m_EActionList[k]		= *src.m_EActionList[k];
    	m_EActionList[k]->parent = this;
    }
	Compile				(m_EActionList);
}

void  PS::CPEDef::FindActionByName(const char* new_name, bool& res)
{
	res 				= false;
	for (EPAVecIt s_it=m_EActionList.begin(); s_it!=m_EActionList.end(); s_it++)
    	if (0==stricmp(new_name,*(*s_it)->actionName)){res=true; break;};
}

void PS::CPEDef::FillActionList(ChooseItemVec& items, void* param)
{
    for(int i=0; actions_token[i].name; i++)
        items.push_back(SChooseItem(actions_token[i].name,actions_token[i].info));
}

ECORE_API bool m_EditChoose = false;

void  PS::CPEDef::OnDrawUI()
{
    if (m_EditChoose)
    {
        bool change;
        shared_str result;
        if (UIChooseForm::GetResult(change, result))
        {
            if (change)
            {
                for (int i = 0; actions_token[i].name; i++)
                {
                    if (0 == strcmp(actions_token[i].name, result.c_str()))
                    {
                        EParticleAction* A = pCreateEAction((PAPI::PActionEnum)actions_token[i].id);
                        xr_string pref = xr_string(*A->actionName);
                        strlwr((char*)pref.data());
                        for (i = 0; true; i++)
                        {
                            bool result;
                            xr_string temp;
                            if (i == 0)
                            {
                                temp = pref;
                            }
                            else
                            {
                                string64 Buffer;
                                sprintf(Buffer, "%s_%02d", pref.c_str(), i - 1);
                                temp = Buffer;
                            }
                            FindActionByName(temp.c_str(), result);
                            if (!result)
                            {
                                pref = temp;
                                break;
                            }
                        }
                        A->actionName = pref.c_str();
                    	A->parent = this;
                        m_EActionList.push_back(A);
                        ExecCommand(COMMAND_UPDATE_PROPERTIES);

                        break;
                    }
                }
            }

            m_EditChoose = false;
        }
        UIChooseForm::Update();
    }
}
void  PS::CPEDef::OnActionsClick(ButtonValue* B, bool& bDataModified, bool& bSafe)
{
    switch (B->btn_num)
    {
        case 0:
        {
            m_EditChoose = true;
            UIChooseForm::SelectItem(smCustom, 1, nullptr, TOnChooseFillItems(this, &PS::CPEDef::FillActionList));
        }
        break;
    }
    bDataModified	= false;
}

void  PS::CPEDef::OnFlagChange(PropValue* sender)
{
    ExecCommand			(COMMAND_UPDATE_PROPERTIES);
}          

void  PS::CPEDef::OnShaderChange(PropValue* sender)
{
	m_CachedShader.destroy	();
	if (m_ShaderName.size()&&m_TextureName.size())
		m_CachedShader.create(m_ShaderName.c_str(),m_TextureName.c_str());
}          

void  PS::CPEDef::OnFrameResize(PropValue* sender)
{
	m_Frame.m_iFrameDimX	= iFloor(1.f/m_Frame.m_fTexSize.x);
}

void PS::CPEDef::CollisionFrictionOnBeforeEdit(PropValue* sender, float& edit_val)
{    edit_val = 1.f-edit_val;}
bool PS::CPEDef::CollisionFrictionOnAfterEdit(PropValue* sender, float& edit_val)
{    edit_val = 1.f-edit_val; return true;}
void PS::CPEDef::CollisionFrictionOnDraw(PropValue* sender, xr_string& draw_val)
{    
	FloatValue* V	= dynamic_cast<FloatValue*>(sender); VERIFY(V);
	draw_sprintf(draw_val,1.f-V->GetValue(),V->dec);
}
void PS::CPEDef::CollisionCutoffOnBeforeEdit(PropValue* sender, float& edit_val)
{    edit_val = _sqrt(edit_val);}
bool PS::CPEDef::CollisionCutoffOnAfterEdit(PropValue* sender, float& edit_val)
{    edit_val = (edit_val)*(edit_val); return true;}
void PS::CPEDef::CollisionCutoffOnDraw(PropValue* sender, xr_string& draw_val)
{    
	FloatValue* V	= dynamic_cast<FloatValue*>(sender); VERIFY(V);
	draw_sprintf(draw_val,_sqrt(V->GetValue()),V->dec);
}

bool PS::CPEDef::OnAfterActionNameEdit(PropValue* sender, shared_str& edit_val)
{
	bool found				= false;
    xr_string tmp(edit_val.c_str());
    strlwr((char*)tmp.data());
	edit_val				= tmp.c_str();
    FindActionByName		(edit_val.c_str(),found); 
    return 					!found;
}

void PS::CPEDef::FillProp(const char* pref, ::PropItemVec& items, void* owner)
{
	ButtonValue* B;
    PropValue* P = nullptr;

	B=PHelper().CreateButton				(items,PrepareKey(pref,"Control"),"Play(F5),Stop(F6),Stop...(F7)",ButtonValue::flFirstOnly);
    B->OnBtnClickEvent.bind					(this,&PS::CPEDef::OnControlClick);
	RTextValue* R = PHelper().CreateRText	(items,PrepareKey(pref,"Name"),&m_Name);
    R->OnAfterEditEvent.bind				(this,&PS::CPEDef::NameOnAfterEdit);


    // max particles
    PHelper().CreateS32		(items,PrepareKey				(pref,"Max Particles"),					&m_MaxParticles,  0, 100000);
//    P->OnChangeEvent		= OnFlagChange;
	// time limit
    P=PHelper().CreateFlag32(items,PrepareKey				(pref,"Time Limit"),		  			&m_Flags, dfTimeLimit);
    P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    if (m_Flags.is(dfTimeLimit))
    {
	    P = PHelper().CreateButton(items, PrepareKey(pref,"Time Limit\\Control"), "Disable", 0, xr_make_delegate(this, &PS::CPEDef::OnTimeLimitClicked));
    	P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    	PHelper().CreateFloat	(items,PrepareKey			(pref,"Time Limit\\Value (sec)"),		&m_fTimeLimit,  0, 10000.f);
    }
	// sprite
    P=PHelper().CreateFlag32(items,PrepareKey				(pref,"Sprite"),		 	   			&m_Flags, dfSprite);
    P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    if (m_Flags.is(dfSprite)){
	    P=PHelper().CreateButton(items, PrepareKey(pref,"Sprite\\Control"), "Disable", 0, xr_make_delegate(this, &PS::CPEDef::OnSpriteClicked));
    	P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    	P=PHelper().CreateChoose(items,PrepareKey			(pref,"Sprite\\Texture"), 	   			&m_TextureName, smTexture, nullptr,nullptr, 2);
        P->OnChangeEvent.bind	(this,&PS::CPEDef::OnShaderChange);
	    P=PHelper().CreateChoose(items,PrepareKey			(pref,"Sprite\\Shader"), 	   			&m_ShaderName,	smEShader);
        P->OnChangeEvent.bind	(this,&PS::CPEDef::OnShaderChange);
    	// frame
        P=PHelper().CreateFlag32(items,PrepareKey			(pref,"Sprite\\Culling"),			 	&m_Flags, dfCulling);
        P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
        if (m_Flags.is(CPEDef::dfCulling))
        {
	        P = PHelper().CreateButton(items, PrepareKey(pref,"Sprite\\Culling\\Control"), "Disable", 0, xr_make_delegate(this, &PS::CPEDef::OnSpriteCullingClicked));
        	P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
        	PHelper().CreateFlag32(items,PrepareKey			(pref,"Sprite\\Culling\\CCW"),			&m_Flags, dfCullCCW);
        }
        P=PHelper().CreateFlag32(items,PrepareKey			(pref,"Sprite\\Frame"),		 		 	&m_Flags, dfFramed);
        P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
        if (m_Flags.is(dfFramed)){
        	P = PHelper().CreateButton(items, PrepareKey(pref,"Sprite\\Frame\\Control"), "Disable", 0, xr_make_delegate(this, &PS::CPEDef::OnSpriteFrameClicked));
        	P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
        	PHelper().CreateFlag32(items,PrepareKey		(pref,"Sprite\\Frame\\Random Init"), 	&m_Flags, dfRandomFrame);
            PHelper().CreateS32	(items,PrepareKey			(pref,"Sprite\\Frame\\Count"),			&m_Frame.m_iFrameCount, 1,256);
            P=PHelper().CreateFloat(items,PrepareKey		(pref,"Sprite\\Frame\\Size U (0..1)"),	&m_Frame.m_fTexSize.x, EPS_S,1.f,0.001f,8);
            P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFrameResize);
            PHelper().CreateFloat	(items,PrepareKey	   	(pref,"Sprite\\Frame\\Size V (0..1)"),	&m_Frame.m_fTexSize.y, EPS_S,1.f,0.001f,8);
	        // animate
            P=PHelper().CreateFlag32(items,PrepareKey		(pref,"Sprite\\Animated"),				&m_Flags, dfAnimated);
            P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
            if (m_Flags.is(dfAnimated)){
                P = PHelper().CreateButton(items, PrepareKey(pref,"Sprite\\Animated\\Control"), "Disable", 0, xr_make_delegate(this, &PS::CPEDef::OnSpriteAnimatedClicked));
            	P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
            	PHelper().CreateFlag32(items,PrepareKey	(pref,"Sprite\\Animated\\Random Playback"),	&m_Flags, dfRandomPlayback);
		    	PHelper().CreateFloat(items,PrepareKey		(pref,"Sprite\\Animated\\Speed"),		&m_Frame.m_fSpeed, 0.f,1000.f);
            }
        }
    }
	// align to path
    P=PHelper().CreateFlag32(items,PrepareKey	(pref,"Movement\\Align To Path"), 					&m_Flags, dfAlignToPath);
    P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    if (m_Flags.is(dfAlignToPath)){
	    P = PHelper().CreateButton(items, PrepareKey(pref,"Movement\\Align To Path\\Control"), "Disable", 0, xr_make_delegate(this, &PS::CPEDef::OnMovementAlignClicked));
    	P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    	PHelper().CreateFlag32(items,PrepareKey	(pref,"Movement\\Align To Path\\Face Align"), 		&m_Flags, dfFaceAlign);
	    PHelper().CreateFlag32(items,PrepareKey	(pref,"Movement\\Align To Path\\Default World Align"), &m_Flags, dfWorldAlign);
    	PHelper().CreateAngle3(items,PrepareKey	(pref,"Movement\\Align To Path\\Default Rotate"),	&m_APDefaultRotation);
    }
	// velocity scale                                                           
    P=PHelper().CreateFlag32(items,PrepareKey	(pref,"Movement\\Velocity Scale"),					&m_Flags, dfVelocityScale);
    P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    if (m_Flags.is(dfVelocityScale))
    {
	    P = PHelper().CreateButton(items, PrepareKey(pref,"Movement\\Velocity Scale\\Control"), "Disable", 0, xr_make_delegate(this, &PS::CPEDef::OnMovementVelocityClicked));
    	P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    	PHelper().CreateVector(items,PrepareKey	(pref,"Movement\\Velocity Scale\\Value"),			&m_VelocityScale, -1000.f, 1000.f);
    }
	// collision
    P=PHelper().CreateFlag32(items,PrepareKey	(pref,"Movement\\Collision"),						&m_Flags, dfCollision);
    P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    FloatValue*	V 			= nullptr;
    if (m_Flags.is(dfCollision)){
    	P = PHelper().CreateButton(items, PrepareKey(pref,"Movement\\Collision\\Control"), "Disable", 0, xr_make_delegate(this, &PS::CPEDef::OnMovementCollisionClicked));
    	P->OnChangeEvent.bind	(this,&PS::CPEDef::OnFlagChange);
    	PHelper().CreateFlag32(items,PrepareKey(pref,"Movement\\Collision\\Collide With Dynamic"),	&m_Flags, dfCollisionDyn);
    	PHelper().CreateFlag32(items,PrepareKey(pref,"Movement\\Collision\\Destroy On Contact"),	&m_Flags, dfCollisionDel);
	    V=PHelper().CreateFloat	(items,PrepareKey	(pref,"Movement\\Collision\\Friction"),			&m_fCollideOneMinusFriction,0.f, 1.f);
        V->OnBeforeEditEvent.bind	(this,&PS::CPEDef::CollisionFrictionOnBeforeEdit);
        V->OnAfterEditEvent.bind	(this,&PS::CPEDef::CollisionFrictionOnAfterEdit);
        V->Owner()->OnDrawTextEvent.bind(this,&PS::CPEDef::CollisionFrictionOnDraw);
	    PHelper().CreateFloat		(items,PrepareKey	(pref,"Movement\\Collision\\Resilence"), 	&m_fCollideResilience, 		0.f, 1.f);
	    V=PHelper().CreateFloat		(items,PrepareKey	(pref,"Movement\\Collision\\Cutoff"),	 	&m_fCollideSqrCutoff, 		0.f, flt_max);
        V->OnBeforeEditEvent.bind	(this,&PS::CPEDef::CollisionCutoffOnBeforeEdit);
        V->OnAfterEditEvent.bind	(this,&PS::CPEDef::CollisionCutoffOnAfterEdit);
        V->Owner()->OnDrawTextEvent.bind(this,&PS::CPEDef::CollisionCutoffOnDraw);
    }
    // actions
	B=::PHelper().CreateButton(items,PrepareKey(pref,"Actions\\Edit"),"Append",ButtonValue::flFirstOnly);
    B->OnBtnClickEvent.bind	(this,&PS::CPEDef::OnActionsClick);
	for (EPAVecIt s_it=m_EActionList.begin(); s_it!=m_EActionList.end(); s_it++)
    {
    	u32 clr				= (*s_it)->flags.is(EParticleAction::flEnabled)?0xFF000000:0xFFC0C0C0;
        string128 buffer;
        sprintf(buffer, "%s (%s)", *(*s_it)->actionType, *(*s_it)->actionName);
    	shared_str a_pref		= PrepareKey(pref,"Actions", buffer);

        ButtonValue* B			= PHelper().CreateButton(items,PrepareKey(a_pref.c_str(), "Controls"),"Up,Down,Remove",ButtonValue::flFirstOnly); B->tag = (s_it-m_EActionList.begin());
        B->Owner()->prop_color	= clr;
        B->OnBtnClickEvent.bind	(this,&PS::CPEDef::OnActionEditClick);

        RTextValue* R;
		R=PHelper().CreateRText	(items,PrepareKey(a_pref.c_str(),"Name"),&(*s_it)->actionName);
        R->OnAfterEditEvent.bind(this,&PS::CPEDef::OnAfterActionNameEdit);
        R->Owner()->prop_color	= clr;
    	(*s_it)->FillProp	(items,a_pref.c_str(),clr);
    }
}

void PS::CPEDef::OnSpriteFrameClicked(ButtonValue* value, bool& bModif, bool& bSafe)
{
	m_Flags.set(dfFramed, false);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void PS::CPEDef::OnSpriteAnimatedClicked(ButtonValue* value, bool& bModif, bool& bSafe)
{
	m_Flags.set(dfAnimated, false);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void PS::CPEDef::OnTimeLimitClicked(ButtonValue* value, bool& bModif, bool& bSafe)
{
	m_Flags.set(dfTimeLimit, false);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void PS::CPEDef::OnSpriteClicked(ButtonValue* value, bool& bModif, bool& bSafe)
{
	m_Flags.set(dfSprite, false);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void PS::CPEDef::OnSpriteCullingClicked(ButtonValue* value, bool& bModif, bool& bSafe)
{
	m_Flags.set(dfCulling, false);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void PS::CPEDef::OnMovementAlignClicked(ButtonValue* value, bool& bModif, bool& bSafe)
{
	m_Flags.set(dfAlignToPath, false);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void PS::CPEDef::OnMovementVelocityClicked(ButtonValue* value, bool& bModif, bool& bSafe)
{
	m_Flags.set(dfVelocityScale, false);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void PS::CPEDef::OnMovementCollisionClicked(ButtonValue* value, bool& bModif, bool& bSafe)
{
	m_Flags.set(dfCollision, false);
    ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

bool PS::CPEDef::Validate(bool bMsg)
{
    bool have_kill_old  = false;

	u32 i = 0;    
    for (; i<m_EActionList.size(); ++i)
    	if(m_EActionList[i]->type==PAPI::PAKillOldID)
        { 
        	have_kill_old	= true;
            if(i != 0)
            {
            	std::swap(m_EActionList[i], m_EActionList[0]);
            }
            break; 
        }
        
    if (bMsg&&(false==have_kill_old))
    	Msg			("!.'%s': dosn't contains 'Kill Old' action.",*m_Name);
    return have_kill_old;
}

void  PS::CPEDef::OnControlClick(ButtonValue* B, bool& bDataModified, bool& bSafe)
{
	switch (B->btn_num)
	{
	case 0: Tools->PlayCurrent();		break;
	case 1: Tools->StopCurrent(false);	break;
	case 2: Tools->StopCurrent(true);	break;
	}
	bDataModified = false;
}

void  PS::CPEDef::OnActionEditClick(ButtonValue* B, bool& bDataModified, bool& bSafe)
{
	bDataModified = false;
	int idx = B->tag;
	switch (B->btn_num) {
	case 0:
		{
			// up
			bDataModified = MoveUpAction(idx);
			break;
		}
	case 1:
		{
			// down
			bDataModified = MoveDownAction(idx);
			break;
		}
	case 2:
		{
			// remove
			bDataModified = RemoveAction(idx);
			break;
		}
	}
}

bool PS::CPEDef::RemoveAction(int idx)
{
	if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Remove action?") == mrYes) {
		Tools->RemoveAction(idx);
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		return true;
	}
	return false;
}

bool PS::CPEDef::RemoveAction(EParticleAction* action)
{
	for (int i = 0; i < m_EActionList.size(); i++)
	{
		if (m_EActionList[i] == action)
		{
			return RemoveAction(i);
		}
	}
	R_ASSERT(false);
	return false;
}

bool PS::CPEDef::MoveUpAction(int idx)
{
	if (idx > 0) {
		EParticleAction* E = m_EActionList[idx - 1];
		m_EActionList[idx - 1] = m_EActionList[idx];
		m_EActionList[idx] = E;
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		return true;
	}
	return false;
}

bool PS::CPEDef::MoveUpAction(EParticleAction* action)
{
	for (int i = 0; i < m_EActionList.size(); i++)
	{
		if (m_EActionList[i] == action)
		{
			return MoveUpAction(i);
		}
	}
	R_ASSERT(false);
	return false;
}

bool PS::CPEDef::MoveDownAction(int idx)
{
	if (idx < (int(m_EActionList.size()) - 1)) {
		EParticleAction* E = m_EActionList[idx + 1];
		m_EActionList[idx + 1] = m_EActionList[idx];
		m_EActionList[idx] = E;
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		return true;
	}
	return false;
}

bool PS::CPEDef::MoveDownAction(EParticleAction* action)
{
	for (int i = 0; i < m_EActionList.size(); i++)
	{
		if (m_EActionList[i] == action)
		{
			return MoveDownAction(i);
		}
	}
	R_ASSERT(false);
	return false;
}

ECORE_API xr_string _item_to_select_after_edit;

bool PS::CPEDef::NameOnAfterEdit(PropValue* sender, shared_str& edit_val)
{
	for (PS::PGDIt g_it = RImplementation.PSLibrary.FirstPGD(); g_it != RImplementation.PSLibrary.LastPGD(); ++g_it)
	{
		PS::CPGDef* pg = (*g_it);
		xr_vector<PS::CPGDef::SEffect*>::const_iterator pe_it = pg->m_Effects.begin();
		xr_vector<PS::CPGDef::SEffect*>::const_iterator pe_it_e = pg->m_Effects.end();

		for (; pe_it != pe_it_e; ++pe_it)
		{
			PS::CPGDef::SEffect* Eff = (*pe_it);
			if (Eff->m_EffectName == this->m_Name)
				Eff->m_EffectName = edit_val;

			if (Eff->m_OnPlayChildName == this->m_Name)
				Eff->m_OnPlayChildName = edit_val;

			if (Eff->m_OnBirthChildName == this->m_Name)
				Eff->m_OnBirthChildName = edit_val;

			if (Eff->m_OnDeadChildName == this->m_Name)
				Eff->m_OnDeadChildName = edit_val;
		}
	}
	_item_to_select_after_edit = edit_val.c_str();
	return true;
}


bool PS::CPGDef::SEffect::Equal(const SEffect& src)
{
	if (!m_Flags.equal(src.m_Flags))	return false;
    if (!m_EffectName.equal(src.m_EffectName)) return false;
	if (!fsimilar(m_Time0,src.m_Time0))	return false;
	if (!fsimilar(m_Time1,src.m_Time1))	return false;
	return true;
}

void PS::CPGDef::SEffect::FillProp(const char* pref, PropItemVec& items, u32 clr)
{
	PropValue* V;
    V=PHelper().CreateChoose(items,PrepareKey(pref,"Name"),&m_EffectName,smPE);
    V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
    V->Owner()->prop_color	= clr;
    V=PHelper().CreateFloat	(items,PrepareKey(pref,"Start Time (s)"),&m_Time0,		0.f,1000.f);
    V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
    V->Owner()->prop_color	= clr;
    V=PHelper().CreateFloat	(items,PrepareKey(pref,"End Time (s)"),	&m_Time1,		0.f,1000.f);
    V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
    V->Owner()->prop_color	= clr;
    V=PHelper().CreateFlag32(items,PrepareKey(pref,"Deferred Stop"),&m_Flags,	SEffect::flDefferedStop);
    V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
    V->Owner()->prop_color	= clr;
    V=PHelper().CreateFlag32(items,PrepareKey(pref,"Enabled"),									&m_Flags, 	SEffect::flEnabled);
    V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
    V->Owner()->prop_color	= clr;
    V=PHelper().CreateFlag32(items,PrepareKey(pref,"Children\\On Birth"),						&m_Flags,	SEffect::flOnBirthChild);
    V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
    V->Owner()->prop_color	= clr;
    if (m_Flags.is(SEffect::flOnBirthChild)){
	    V=PHelper().CreateChoose(items,PrepareKey(pref,"Children\\On Birth\\Effect Name"),			&m_OnBirthChildName,smPE);
        V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
	    V->Owner()->prop_color	= clr;
    }
    V=PHelper().CreateFlag32(items,PrepareKey(pref,"Children\\On Play"),						&m_Flags,	SEffect::flOnPlayChild);
    V->OnChangeEvent.bind		(parent,&PS::CPGDef::OnParamsChange);
    V->Owner()->prop_color		= clr;
    if (m_Flags.is(SEffect::flOnPlayChild)){
	    V=PHelper().CreateChoose	(items,PrepareKey(pref,"Children\\On Play\\Effect Name"),			&m_OnPlayChildName,smPE);
        V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
	    V->Owner()->prop_color	= clr;
        V=PHelper().CreateFlag32(items,PrepareKey(pref,"Children\\On Play\\Play After Stop"),		&m_Flags,	SEffect::flOnPlayChildRewind);
        V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);
        V->Owner()->prop_color	= clr;
    }
    V=PHelper().CreateFlag32(items,PrepareKey(pref,"Children\\On Dead"),						&m_Flags,	SEffect::flOnDeadChild);
    V->OnChangeEvent.bind		(parent,&PS::CPGDef::OnParamsChange);
    V->Owner()->prop_color	= clr;
    if (m_Flags.is(SEffect::flOnDeadChild)){
	    V=PHelper().CreateChoose	(items,PrepareKey(pref,"Children\\On Dead\\Effect Name"),			&m_OnDeadChildName,smPE);
        V->OnChangeEvent.bind	(parent,&PS::CPGDef::OnParamsChange);    
	    V->Owner()->prop_color	= clr;
    }
}

void PS::CPGDef::SEffect::FillPropInit(PropItemVec& items, const char* pref)
{
	u32 clr					= m_Flags.is(CPGDef::SEffect::flEnabled)? 0xFF000000 :0xFFC0C0C0;
	xr_string nm;
	nm.resize(64);
	auto Index = parent->FindEffect(this);
	sprintf(nm.data(), "Effect #%d", Index + 1);

	auto FullPref = PrepareKey(pref,nm.c_str());
	auto B=PHelper().CreateButton(items,PrepareKey(FullPref.c_str(),"Controls"),"Preview,Select,Remove",ButtonValue::flFirstOnly);
	B->tag = Index;
	B->OnBtnClickEvent.bind	(parent,&PS::CPGDef::OnEffectEditClick);
	B->Owner()->prop_color	= clr;

	FillProp(FullPref.c_str(), items, clr);
}

bool PS::CPGDef::Equal(const CPGDef* pg)
{
	if (!m_Flags.equal(pg->m_Flags))				return false;
	if (!fsimilar(m_fTimeLimit,pg->m_fTimeLimit))	return false;
    if (m_Effects.size()!=pg->m_Effects.size())		return false;
    EffectIt s_it=m_Effects.begin(); 
    for (EffectIt d_it=m_Effects.begin(); d_it!=m_Effects.end(); s_it++,d_it++)
    	if (!(*s_it)->Equal(**d_it)) return false;
	return true;
}
int PS::CPGDef::FindEffect(SEffect* effect)
{
	for (int i = 0; i < m_Effects.size(); i++)
	{
		if (m_Effects[i] == effect)
		{
			return i;
		}
	}
	R_ASSERT(false);
	return -1;
}

bool PS::CPGDef::Validate(bool bMsg)
{
	bool failed = false;

    xr_vector<SEffect*>::const_iterator pe_it 		= m_Effects.begin();
    xr_vector<SEffect*>::const_iterator pe_it_e 	= m_Effects.end();

    for(;pe_it!=pe_it_e;++pe_it)
    {	
        PS::CPGDef::SEffect* Eff		= (*pe_it);
        PS::CPEDef* ped				= RImplementation.PSLibrary.FindPED(Eff->m_EffectName.c_str());
        if(!ped)
        {
            failed = failed||true;
            Msg("Validation FAILED (non-existent effect used) group[%s] effect[%s]", m_Name.c_str(), Eff->m_EffectName.c_str());
			break;
        }
    
        if(Eff->m_Flags.test(SEffect::flOnPlayChild) && Eff->m_OnPlayChildName.size()==0)
            failed = failed||true;
        if(Eff->m_Flags.test(SEffect::flOnBirthChild) && Eff->m_OnBirthChildName.size()==0)
            failed = failed||true;
        if(Eff->m_Flags.test(SEffect::flOnDeadChild) && Eff->m_OnDeadChildName.size()==0)
            failed = failed||true;

        if(failed && bMsg) 
            Msg("Validation FAILED (incorrect child event settings) group[%s] effect[%s]", m_Name.c_str(), Eff->m_EffectName.c_str());
        if(failed)
        	break;
    }
    return !failed;
}

void  PS::CPGDef::OnEffectsEditClick(ButtonValue* B, bool& bDataModified, bool& bSafe)
{
    switch (B->btn_num){
    case 0:
        m_Effects.push_back(new SEffect());
        m_Effects.back()->m_Flags.set(CPGDef::SEffect::flEnabled,false);
    	m_Effects.back()->parent = this;
        ExecCommand		(COMMAND_UPDATE_PROPERTIES);
        OnParamsChange	(B);
        bDataModified	= true;
    break;
    }
}

void  PS::CPGDef::OnEffectTypeChange(PropValue* sender)
{
    ExecCommand			(COMMAND_UPDATE_PROPERTIES);
    OnParamsChange		(sender);
}

void  PS::CPGDef::OnControlClick(ButtonValue* B, bool& bDataModified, bool& bSafe)
{
    switch (B->btn_num){
    case 0: Tools->PlayCurrent();		break;
    case 1: Tools->StopCurrent(false);	break;
    case 2: Tools->StopCurrent(true);	break;
    }
    bDataModified		= false;
}

void  PS::CPGDef::OnEffectEditClick(ButtonValue* B, bool& bDataModified, bool& bSafe)
{
    switch (B->btn_num){
    case 0:
	    {
		    Tools->PlayCurrent	(B->tag);    
    		bDataModified	= false;
	    }
    break;
    case 1:
    	{
    		CPGDef::SEffect* eff = *(m_Effects.begin()+B->tag); VERIFY(eff);
			Tools->SelectEffect(*eff->m_EffectName);
			bDataModified	= false;
			bSafe			= true;
    		break;
		}
    case 2:
	    {
		    bDataModified = RemoveEffect(B->tag, bSafe);
    		break;
	    }
    }
}

bool PS::CPGDef::RemoveEffect(int idx, bool& safe)
{
	if (ELog.DlgMsg(mtConfirmation, mbYes| mbNo,"Remove effect?") == mrYes){
		SEffect* eff	= *(m_Effects.begin()+idx);
		xr_delete		(eff);
		m_Effects.erase	(m_Effects.begin()+idx);
		ExecCommand		(COMMAND_UPDATE_PROPERTIES);
		OnParamsChangeInternal();
		safe = true;
		return true;
	}
	return false;
}

bool PS::CPGDef::RemoveEffect(SEffect* effect)
{
	bool dummy;
	for (int i = 0; i < m_Effects.size(); i++)
	{
		if (m_Effects[i] == effect)
		{
			return RemoveEffect(i, dummy);
		}
	}
	R_ASSERT(false);
	return false;
}

void PS::CPGDef::OnParamsChange(PropValue* sender)
{
	OnParamsChangeInternal();
}

void PS::CPGDef::OnParamsChangeInternal()
{
	//Tools->SetCurrentPG(0);
	//Tools->SetCurrentPG(this);
}

void PS::CPGDef::FillProp(const char* pref, PropItemVec& items, void* owner)
{                                   
    ButtonValue* B;
	B=PHelper().CreateButton	(items,PrepareKey(pref,"Control"),"Play,Stop,Stop...",ButtonValue::flFirstOnly);
    B->OnBtnClickEvent.bind		(this,&PS::CPGDef::OnControlClick);
    B=PHelper().CreateButton	(items,PrepareKey(pref,"Edit"),"Append Effect",ButtonValue::flFirstOnly);
    B->OnBtnClickEvent.bind		(this,&PS::CPGDef::OnEffectsEditClick);
    PropValue* V;
	PHelper().CreateName		(items,PrepareKey(pref,"Name"),&m_Name,(::ListItem*)owner);
    V=PHelper().CreateFloat		(items,PrepareKey(pref,"Time Limit (s)"),	&m_fTimeLimit,	-1.f,1000.f);
    V->OnChangeEvent.bind		(this,&PS::CPGDef::OnParamsChange);

    u32 i = 0;
    for (EffectIt it=m_Effects.begin(); it!=m_Effects.end(); ++it,++i)
    {
    	u32 clr					= (*it)->m_Flags.is(CPGDef::SEffect::flEnabled)? 0xFF000000 :0xFFC0C0C0;
        xr_string nm;
        nm.resize(64);
        sprintf(nm.data(), "Effect #%d", i + 1);

    	auto FullPref = PrepareKey(pref,nm.c_str());
        B=PHelper().CreateButton(items,PrepareKey(FullPref.c_str(),"Controls"),"Preview,Select,Remove",ButtonValue::flFirstOnly); B->tag = it-m_Effects.begin();
        B->OnBtnClickEvent.bind	(this,&PS::CPGDef::OnEffectEditClick);
        B->Owner()->prop_color	= clr;

    	(*it)->FillProp(FullPref.c_str(), items, clr);
    }
}

void PS::CPEDef::Render(const Fmatrix& parent)
{
    Fmatrix trans; trans.translate(parent.c);
    for (EPAVecIt it = m_EActionList.begin(); it != m_EActionList.end(); it++)
    {
        if ((*it)->flags.is(EParticleAction::flDraw | EParticleAction::flEnabled))
        {
            PBool* ar = (*it)->_bool_safe("Allow Rotate");
            (*it)->Render((ar && ar->val) ? parent : trans);
        }
    }
}

PS::CPEDef* CPSLibrary::AppendPED(PS::CPEDef* src)
{
    m_PEDs.push_back(new PS::CPEDef());
    if (src)
    {
	    m_PEDs.back()->Copy(*src);
    }

    return m_PEDs.back();
}

PS::CPGDef* CPSLibrary::AppendPGD(PS::CPGDef* src)
{
    m_PGDs.push_back(new PS::CPGDef());
    if (src)
    {
	    m_PGDs.back()->Clone(src);
    }
    return m_PGDs.back();
}

PS::CPACDef* CPSLibrary::AppendPACD(PS::CPACDef* src)
{
	m_PACDs.push_back(new PS::CPACDef());
	if (src)
	{
		m_PACDs.back()->Clone(src);
	}
	return m_PACDs.back();
}


//------------------------------------------------------------------------------
bool CPSLibrary::Save()
{
    xr_string temp_fn;
    if (EFS.GetSaveName("$game_data$", temp_fn))
    {
        return Save(temp_fn.c_str());
    }

    return false;
}
//------------------------------------------------------------------------------
bool CPSLibrary::Save2()
{
    string_path fn;
    SPBItem* pb = UI->ProgressStart(m_PEDs.size() + m_PGDs.size() + m_PACDs.size(), "Saving particles...");

    for (PS::PEDIt it = m_PEDs.begin(); it != m_PEDs.end(); ++it)
    {
        pb->Inc();
        PS::CPEDef* pe = (*it);

        if (!pe->Validate(false))
            continue;

        FS.update_path(fn, "$game_particles$", pe->m_Name.c_str());
        strcat(fn, ".pe");

        FS.file_delete(fn);

        CInifile ini(fn, false, false, false);
        pe->Save2(ini);
        ini.save_as(fn);
    }

    for (PS::PGDIt g_it = m_PGDs.begin(); g_it != m_PGDs.end(); ++g_it)
    {
        pb->Inc();
        PS::CPGDef* pg = (*g_it);

        if (!pg->Validate(false))
            continue;

        FS.update_path(fn, "$game_particles$", pg->m_Name.c_str());
        strcat(fn, ".pg");

        FS.file_delete(fn);

        CInifile ini(fn, false, false, false);
        pg->Save2(ini);
        ini.save_as(fn);
    }

	for (auto elem : m_PACDs)
	{
		pb->Inc();

		if (!elem->Validate(false))
		{
			continue;
		}

		FS.update_path(fn, "$game_particles$", elem->getName());
		strcat(fn, ".pac");

		FS.file_delete(fn);

		CInifile ini(fn, false, false, false);
		elem->Save2(ini);
		ini.save_as(fn);
	}
	
    UI->ProgressEnd(pb);
    return true;
}

bool CPSLibrary::Save(const char* nm)
{
    CMemoryWriter F;

    F.open_chunk(PS::Chunks::VERSION);
    F.w_enum(PS::Version::Latest);
    F.close_chunk();

    F.open_chunk(PS::Chunks::EXTENDED_PE);
    u32 chunk_id = 0;
    for (auto elem : m_PEDs)
    {
        F.open_chunk(chunk_id++);
        elem->Save(F);
        F.close_chunk();
    }
    F.close_chunk();

    F.open_chunk(PS::Chunks::EXTENDED_PG);
    chunk_id = 0;
    for (auto elem : m_PGDs)
    {
        F.open_chunk(chunk_id++);
        elem->Save(F);
        F.close_chunk();
    }
    F.close_chunk();

	F.open_chunk(PS::Chunks::EXTENDED_PAC);
    chunk_id = 0;
	for (auto elem : m_PACDs)
	{
		F.open_chunk(chunk_id++);
		elem->Save(F);
		F.close_chunk();
	}
	F.close_chunk();

    return F.save_to(nm);
}
//------------------------------------------------------------------------------

