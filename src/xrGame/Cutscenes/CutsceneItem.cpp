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
    Offset.identity();
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

SCutsceneObjectElement* SCutsceneObjectElement::GetParent()
{
    return parent;
}

void SCutsceneObjectElement::SetParent(SCutsceneObjectElement* Parent, u16 BoneID)
{
    parent = Parent;
    AttachBoneID = BoneID;
//#ifndef MASTER_GOLD
    parent->children.push_back(this);
//#endif
}

void SCutsceneObjectElement::SetOffset(Fvector Offset)
{
    this->Offset.c.add(Offset);
}

void SCutsceneObjectElement::SetRotationDegrees(Fvector Rotation)
{
    Fmatrix TempMRot(Fmatrix::Identity);
    TempMRot.setHPB(
        deg2rad(Rotation.y),
        deg2rad(Rotation.x),
        deg2rad(Rotation.z)
    );
    Offset.mulB_43(TempMRot);
}

void SCutsceneObjectElement::Activate()
{
    R_ASSERT2(HudModelKinematicsAnimated, "You need to create object first before activate!");
    MotionID M2 = HudModelKinematicsAnimated->ID_Cycle_Safe(AnimName.c_str());
    if (bDebug) {
        Msg("playing item animation [%s]", AnimName.c_str());
    }
	R_ASSERT4(M2.valid(), "model has no motion", HudModel->getDebugName().c_str(), AnimName.c_str());

    // We cannot just call IKinematicsAnimated::PlayCycle for all bones because we need a CBlend for control
    for (u16 i = 0; i < HudModelKinematicsAnimated->partitions().count(); ++i)
    {
        CBlend* B = nullptr;
        if (!i)
        {
            B = HudModelKinematicsAnimated->PlayCycle(i, M2, true, [](CBlend* P)
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
            B->stop_at_end_callback = true;
        } else
        {
            B = HudModelKinematicsAnimated->PlayCycle(i, M2, true);
        }
        B->stop_at_end = true;
#ifndef MASTER_GOLD
        m_pBlends.push_back(B);
#endif
    }
}

void SCutsceneObjectElement::Update(Fmatrix Deviation)
{
    //Fmatrix m_transform;
    //m_transform.identity();
    // TODO: Fix position for child objects
    //if (!parent)
    //{
    //    m_transform.c = Deviation;
    //} else
    //{
    //    auto trans = parent->HudModelKinematics->LL_GetTransform(AttachBoneID);
    //    trans.c.add(start_parent_transform.c);
    //    m_transform = trans;
    //}
    {
        Fmatrix Trans = Deviation;
        Trans.mulB_44(Offset);
        //Trans.c.add(Offset);
        HudModelKinematics->CalculateBones(true);
        ::Render->set_Transform(Trans);
        ::Render->add_Visual(HudModel, false, true);
    }

    for (auto& Child : children)
    {
        //m_transform.identity();
        auto trans = HudModelKinematics->LL_GetTransform(Child->AttachBoneID);
        
        Child->Update(trans);
    }
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
    //auto pos = matrix.c;
    for (auto& elem : CutsceneElements)
    {
        if (!elem->GetParent())
        {
            elem->Update(matrix);
        }
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
