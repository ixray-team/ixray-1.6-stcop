#pragma once

class IViewport:
    public IEditorWnd
{
public:
    IViewport();
    virtual ~IViewport();

    virtual void Render() = 0;

protected:
    UIRenderForm View;
    shared_str ViewName;
};