#include "stdafx.h"
#include "CutsceneItem.h"

#include "ai_space.h"
//#include "Kinematics.h"
//#include "KinematicsAnimated.h"
#include "script_game_object.h"

SCutsceneObjectElement::SCutsceneObjectElement(LPCSTR ObjectName)
{
#ifndef MASTER_GOLD
    ObjName = ObjectName;
#endif
    HudModel = ::Render->model_Create(ObjectName);
    R_ASSERT3(HudModel, "Unable to find object", ObjectName);
    HudModelKinematics = smart_cast<IKinematics*>(HudModel);
    HudModelKinematicsAnimated = smart_cast<IKinematicsAnimated*>(HudModel);
}

void SCutsceneObjectElement::SetAllBonesVisibility(bool Visibility)
{
    R_ASSERT2(HudModelKinematics, "You need to create object first before set bones visibility");
	VisMask vis_mask;
    if (Visibility)
    {
        vis_mask.one();
    } else
    {
        vis_mask.zero();
    }
    HudModelKinematics->LL_SetBonesVisible(vis_mask);
}

void SCutsceneObjectElement::SetBoneVisibility(u16 BoneID, bool Visibility)
{
    R_ASSERT2(HudModelKinematics, "You need to create object first before set bones visibility");
    HudModelKinematics->LL_SetBoneVisible(BoneID, Visibility, true);
}

void SCutsceneObjectElement::SetParent(SCutsceneObjectElement* Parent, u16 BoneID)
{
    parent = Parent;
    AttachBoneID = BoneID;
#ifndef MASTER_GOLD
    parent->children.push_back(this);
#endif
}

void SCutsceneObjectElement::Activate()
{
    R_ASSERT2(HudModelKinematicsAnimated, "You need to create object first before activate!");
    MotionID M2 = HudModelKinematicsAnimated->ID_Cycle_Safe(AnimName.c_str());
    if (bDebug) {
        Msg("playing item animation [%s]", AnimName.c_str());
    }
	R_ASSERT4(M2.valid(), "model has no motion", HudModel->getDebugName().c_str(), AnimName.c_str());

    CBlend* B = HudModelKinematicsAnimated->PlayCycle(M2, true, [](CBlend* P)
    {
        auto Self = (SCutsceneObjectElement*)P->CallbackParam;
        Level().CreateDefferedScriptCallback([Self]()
        {
            luabind::functor<void> funct;
            if (ai().script_engine().functor(Self->OnFinishFuncName.c_str(), funct))
            {
                funct();
            }
        });
    }, this);
    //B->update_callback = false;
    //B->stop_at_end = true;
#ifndef MASTER_GOLD
    m_pBlends.push_back(B);
#endif
}

void SCutsceneObjectElement::Update(Fvector Deviation)
{
    Fmatrix m_transform;
    m_transform.identity();
    // TODO: Fix position for child objects
    //if (!parent)
    //{
        m_transform.c = Deviation;
    //} else
    //{
    //    auto trans = parent->HudModelKinematics->LL_GetTransform(AttachBoneID);
    //    trans.c.add(start_parent_transform.c);
    //    m_transform = trans;
    //}
    HudModelKinematics->CalculateBones(true);
    ::Render->set_Transform(&m_transform);
    ::Render->add_Visual(HudModel, false, true);
}

#ifndef MASTER_GOLD
void SCutsceneObjectElement::StopAnimation()
{
    for (auto& B : m_pBlends)
    {
        B->playing = false;
    }
}

void SCutsceneObjectElement::ForwardAnimation()
{
    for (auto& B : m_pBlends)
    {
        B->playing = true;
        if(B->speed < 0.f)
        {
            B->speed = -B->speed;
        }
    }
}

void SCutsceneObjectElement::BackwardAnimation()
{
    for (auto& B : m_pBlends)
    {
        B->playing = true;
        if(B->speed > 0.f)
        {
            B->speed = -B->speed;
        }
    }
}

SCutsceneObjectElement* SCutsceneObjectElement::DrawChildren(xr_set<SCutsceneObjectElement*>& Processed)
{
    SCutsceneObjectElement* Selected = nullptr;
    if (ImGui::TreeNodeEx(ObjName.c_str()))
    {
        Selected = this;
        for (auto& elem : children)
        {
            if (!Processed.contains(elem)){
                Processed.insert(elem);
                auto NewSelected = elem->DrawChildren(Processed);
                if (NewSelected)
                {
                    VERIFY(!Selected);
                    Selected = NewSelected;
                }
            }
        }
        ImGui::TreePop();
    }
    return Selected;
}
#endif

void SCutsceneObjectElement::SetAnimToPlay(LPCSTR AnimName)
{
    this->AnimName = AnimName;
}

void SCutsceneObjectElement::SetOnFinishFunc(LPCSTR Name)
{
    OnFinishFuncName = Name;
}

u16 SCutsceneObjectElement::GetBoneID(LPCSTR BoneName)
{
    R_ASSERT2(HudModelKinematics, "You need to create object first before get bone id!");
    return HudModelKinematics->LL_BoneID(BoneName);
}

void SCutsceneObjectElement::SetBonesWeapon(u16 BoneIDR, u16 BoneIDL)
{
    BoneL = BoneIDL;
    BoneR = BoneIDR;
}

CCutsceneItem::~CCutsceneItem()
{
    for(auto& elem : CutsceneElements)
    {
        xr_delete(elem);
    }
    CutsceneElements.clear();
}

void CCutsceneItem::Construct(LPCSTR Section)
{
    NameSect = Section;
    luabind::functor<void> funct;
    if (ai().script_engine().functor(pSettings->r_string(Section, "construct_func"), funct)) {
        funct(this);
    } else
    {
        R_ASSERT3(false, "Cutscene construct failed", Section);
    }
    
}

void CCutsceneItem::Activate()
{
    for(auto& elem : CutsceneElements)
    {
        elem->Activate();
    }
}

void CCutsceneItem::Update(Fmatrix matrix)
{
    auto pos = matrix.c;
    for (auto& elem : CutsceneElements)
    {
        elem->Update(pos);
    }
}

LPCSTR CCutsceneItem::GetName()
{
    return NameSect.c_str();
}

SCutsceneObjectElement* CCutsceneItem::CreateObjectElement(LPCSTR ObjectName)
{
    auto RetValue = new SCutsceneObjectElement(ObjectName);
    CutsceneElements.push_back(RetValue);
    return RetValue;
}

void CCutsceneItem::SetPivotObject(CScriptGameObject* PivotObject)
{
    this->PivotObject = &PivotObject->object();
}

#ifndef MASTER_GOLD
void CCutsceneItem::StopAnimation()
{
    for(auto& elem : CutsceneElements)
    {
        elem->StopAnimation();
    }
}

void CCutsceneItem::ForwardAnimation()
{
    for(auto& elem : CutsceneElements)
    {
        elem->ForwardAnimation();
    }
}

void CCutsceneItem::BackwardAnimation()
{
    for(auto& elem : CutsceneElements)
    {
        elem->BackwardAnimation();
    }
}

SCutsceneObjectElement* CCutsceneItem::Draw()
{
	xr_set<SCutsceneObjectElement*> Processed;
    SCutsceneObjectElement* Selected = nullptr;
    for (auto& elem : CutsceneElements)
    {
        if (!Processed.contains(elem))
        {
            Processed.insert(elem);
            auto NewSelected = elem->DrawChildren(Processed);
            if (NewSelected)
            {
                VERIFY(!Selected);
                Selected = NewSelected;
            }
        }
    }
    return Selected;
}
#endif

SCutsceneObjectElement::~SCutsceneObjectElement()
{
    ::Render->model_Delete(HudModel);
    HudModel = nullptr;
    HudModelKinematics = nullptr;
    HudModelKinematicsAnimated = nullptr;
}
