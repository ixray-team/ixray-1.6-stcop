#pragma once
class UIKeyForm :public IEditorWnd
{
public:
	UIKeyForm();
	virtual ~UIKeyForm();
	virtual void Draw();
	IC bool AutoChange() const { return m_AutoChange; }
	IC float Position() const { return m_Position; };

private:
	void SetMark(int id, int action);
	void DrawMarkTrack(int id, bool enabled, float playhead, const ImVec2& size);
	void DrawNotifies(float ActionsWidth, float Playhead);
	void DrawNotifyEditor();
	void DrawAddBonePopup(editor_notify_data& NotifyData);
	void ResetNotifySelection();
	void SelectNotify(const shared_str& Bone, int Track, float Time);
	NotifyTrack* GetSelectedTrack();
	anim_notify* GetSelectedNotify();
	float m_Position;
	bool m_AutoChange;
	float m_TimeFactor;
	CSMotion* m_currentEditMotion;
	shared_str SelectedBone;
	int SelectedTrack = -1;
	float SelectedTime = 0.0f;
	bool DraggingKey = false;
	shared_str OpenBone;
	ImGuiTextFilter BoneFilter;
};

