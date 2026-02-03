#pragma once

class IKinematicsAnimated;
class CBlend;
class CScriptGameObject;

struct SCutsceneObjectElement
{
    SCutsceneObjectElement(LPCSTR ObjectName);
    ~SCutsceneObjectElement();
    void SetAllBonesVisibility(bool Visibility);
    void SetBoneVisibility(u16 BoneID, bool Visibility);
    SCutsceneObjectElement* GetParent();
    void SetParent(SCutsceneObjectElement* Parent, u16 BoneID);
    void SetOffset(Fvector Offset);
    void SetRotationDegrees(Fvector Rotation); // set rotation in format {Pitch, Yaw, Roll}
    void SetAnimToPlay(LPCSTR AnimName);
    void SetOnFinishFunc(LPCSTR Name);
    u16 GetBoneID(LPCSTR BoneName);
    void SetBonesWeapon(u16 BoneIDR, u16 BoneIDL);

    void Activate();
    void Update(Fmatrix Deviation);
    
#ifndef MASTER_GOLD
    void StopAnimation();
    void ForwardAnimation();
    void BackwardAnimation();
    SCutsceneObjectElement* DrawChildren(xr_set<SCutsceneObjectElement*>& Processed);
#endif
    
private:
#ifndef MASTER_GOLD
    xr_vector<CBlend*> m_pBlends = {};
    shared_str ObjName;
#endif
    xr_vector<SCutsceneObjectElement*> children = {};
    Fmatrix Offset = {};
    IKinematicsAnimated* HudModelKinematicsAnimated = nullptr;
    IKinematics* HudModelKinematics = nullptr;
    IRenderVisual* HudModel = nullptr;
    
    SCutsceneObjectElement* parent = nullptr;
    u16 AttachBoneID = u16(-1);
    u16 BoneR = u16(-1), BoneL = u16(-1);
#ifndef MASTER_GOLD
public:
    Fmatrix start_parent_transform;
#endif
    bool start_parent_transform_set = false;

    shared_str AnimName;
    shared_str OnFinishFuncName = "";
};

class CCutsceneItem
{
    CObject* PivotObject = nullptr;
    shared_str NameSect;
    xr_vector<SCutsceneObjectElement*> CutsceneElements;
public:
    ~CCutsceneItem();
    
    void Construct(LPCSTR Section);
    void Activate();
    void Update(Fmatrix matrix);
    LPCSTR GetName();
    SCutsceneObjectElement* CreateObjectElement(LPCSTR ObjectName);
    void SetPivotObject(CScriptGameObject* PivotObject);
    CObject* GetPivotObject() const {return PivotObject;}
    
#ifndef MASTER_GOLD
    void StopAnimation();
    void ForwardAnimation();
    void BackwardAnimation();
    SCutsceneObjectElement* Draw();
#endif
};
