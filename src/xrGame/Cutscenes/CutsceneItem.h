#pragma once

class IKinematicsAnimated;
class CBlend;
class CScriptGameObject;

XREFLECT(LuaExport)
struct SCutsceneObjectElement
{
    SCutsceneObjectElement(LPCSTR ObjectName);
    ~SCutsceneObjectElement();
    XPROPERTY(LuaCallable, Name="set_all_bones_visibility")
    void SetAllBonesVisibility(bool Visibility);
    XPROPERTY(LuaCallable, Name="set_bone_visibility")
    void SetBoneVisibility(u16 BoneID, bool Visibility);
    XPROPERTY(LuaCallable, Name="set_parent")
    void SetParent(SCutsceneObjectElement* Parent, u16 BoneID);
    XPROPERTY(LuaCallable, Name="set_anim_to_play")
    void SetAnimToPlay(LPCSTR AnimName);
    XPROPERTY(LuaCallable, Name="set_on_finish_func")
    void SetOnFinishFunc(LPCSTR Name);
    XPROPERTY(LuaCallable, Name="get_bone_id")
    u16 GetBoneID(LPCSTR BoneName);
    XPROPERTY(LuaCallable, Name="set_bones_weapon")
    void SetBonesWeapon(u16 BoneIDR, u16 BoneIDL);

    void Activate();
    void Update(Fvector Deviation);
    
#ifndef MASTER_GOLD
    void StopAnimation();
    void ForwardAnimation();
    void BackwardAnimation();
    SCutsceneObjectElement* DrawChildren(xr_set<SCutsceneObjectElement*>& Processed);
#endif
    
private:
#ifndef MASTER_GOLD
    xr_vector<CBlend*> m_pBlends = {};
    xr_vector<SCutsceneObjectElement*> children = {};
    shared_str ObjName;
#endif
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

XREFLECT(LuaExport)
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
    XPROPERTY(LuaCallable, Name="create_object_element")
    SCutsceneObjectElement* CreateObjectElement(LPCSTR ObjectName);
    XPROPERTY(LuaCallable, Name="set_pivot_object")
    void SetPivotObject(CScriptGameObject* PivotObject);
    CObject* GetPivotObject() const {return PivotObject;}
    
#ifndef MASTER_GOLD
    void StopAnimation();
    void ForwardAnimation();
    void BackwardAnimation();
    SCutsceneObjectElement* Draw();
#endif
};
