#pragma once
class EDetailManager;
class UIDOTool :public UIToolCustom
{
public:
	UIDOTool();
	virtual ~UIDOTool();
	virtual void Draw();
	virtual void OnDrawUI();
	EDetailManager* DM;

private:
	void HandleDragDrop();
	bool m_DOShuffle;
	bool IsChooseDraw = false;
};