// Originally by B.O.R.S.C.H.T. team
// see https://bitbucket.org/stalker/xray-csky_borscht_sdk

#pragma once
struct SAINode;

class IM_Manipulator
{
public:
    bool m_active;

    IM_Manipulator(): m_active(false) {}

    void Render(float canvasX, float canvasY, float canvasWidth, float canvasHeight);

    u32 MatrixMode = 0;

private:
    void CommandScale(ObjectList& lst, Fmatrix& ObjectMatrix, Fmatrix& DeltaMatrix, bool& retFlag);
    void CommandRotate(Fmatrix& ObjectMatrix, Fmatrix& DeltaMatrix, ObjectList& lst, const bool IsCSParent);
    void CommandMove(ObjectList& lst, Fmatrix& ObjectMatrix, Fmatrix& DeltaMatrix, SAINode* NodeObject);
};

extern IM_Manipulator imManipulator;
