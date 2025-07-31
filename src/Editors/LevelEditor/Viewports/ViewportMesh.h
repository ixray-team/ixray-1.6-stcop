#pragma once
#include "IViewport.h"

class CViewportMesh :
    public IViewport
{
public:
    CViewportMesh();
    virtual ~CViewportMesh();

    virtual void Draw() override;
    virtual void Render() override;
    void OpenModel(const xr_path& File);

private:
    UIRenderForm View;
    CEditableObject* ViewMesh = nullptr;
};