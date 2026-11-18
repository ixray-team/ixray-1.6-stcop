#pragma once
class ESceneTerrainTool;
class UITerrainTool : public UIToolCustom
{
public:
	UITerrainTool();
	virtual ~UITerrainTool();
	virtual void Draw() override;
	virtual void OnDrawUI() override {}
	ESceneTerrainTool* tool;
private:
	int		m_CreateRes;
	float	m_CreateHeight;
	float	m_FillHeight;
	int		m_SmoothPasses;
	int		m_ResizeRes;
	int		m_BakeSize;
};
