#pragma once
class EDetailManager;

class UIDOTool :
	public UIToolCustom
{
public:
	UIDOTool() = default;
	virtual ~UIDOTool() = default;
	virtual void Draw();
	virtual void OnDrawUI();
	EDetailManager* DM = nullptr;

private:
	void HandleDragDrop();
	bool m_DOShuffle = false;
	bool IsChooseDraw = false;
};