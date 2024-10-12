#pragma once
class UILeftBarForm :
	public IEditorWnd
{
public:
	enum RenderMode
	{
		Render_Editor,
		Render_Engine
	};

	enum EAnimMode
	{
		e8bit,
		e16bit,
		e32bit
	};

	UILeftBarForm();
	virtual ~UILeftBarForm();
	virtual void Draw();
	IC RenderMode GetRenderMode() {return m_RenderMode;}
	void SetRenderMode(bool bEngineNode);
	IC int GetPickMode()const { return m_PickMode; }

private:
	int m_PickMode;
	RenderMode m_RenderMode;
	EAnimMode m_AnimMode;
};

