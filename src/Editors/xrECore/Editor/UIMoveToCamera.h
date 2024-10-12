#pragma once

class UIMoveToCamera : 
    public IEditorWnd
{
public:
    UIMoveToCamera();
    virtual void Draw();

private:
    void Ok();
    void Close();
    void Reset();

private:
    Fvector NewCameraPosition;
    Fvector OldCameraPosition;
};