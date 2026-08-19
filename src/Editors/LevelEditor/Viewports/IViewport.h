#pragma once

class IViewport:
    public IEditorWnd
{
public:
    IViewport();
    virtual ~IViewport();

    virtual void Render() = 0;
	// Новый renderer вызывает только этот renderer-neutral hook. Legacy
	// Render() остаётся для пока не переведённых preview implementations.
	virtual void RenderTiramisu() {}

protected:
    UIRenderForm View;
    shared_str ViewName;
};
