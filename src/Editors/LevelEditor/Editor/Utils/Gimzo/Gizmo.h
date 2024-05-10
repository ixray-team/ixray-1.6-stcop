#pragma once
class Gizmo
{
public:
	enum class EStatus
	{
		None,
		SelectedX,
		SelectedY,
		SelectedZ,
		//only scale
		SelectedXYZ,
	};
	enum class EType
	{
		Move,
		Scale,
		Rotate,
		Count,
		None,
	};
	Gizmo();
	~Gizmo();
	void    Render();
	void OnFrame();
	void Clear();
	void Fixed();
	inline EStatus GetStatus()const { return m_CurrentStatus; }
	inline void SetType(EType Type)
	{
		m_Type = Type;
		m_bVisible = false;	
		m_CurrentStatus = EStatus::None; 
		m_bFixed = false;
		UI->RedrawScene();
	}
	inline EType GetType()const { return m_Type ; }
	inline bool IsFixed()const { return m_bFixed; }
	
	void SetStep(EType Type, float Step);
	float GetStep(EType Type) const;
	void SwitchStep(EType Type, bool Enable);
	bool IsStepEnable(EType Type) const;
	inline Fvector GetStartPosition()const {	return m_StartPosition;	}
private:
	bool m_bFixed;
	bool m_bVisible;
	Fvector m_ScreenPosition;
	EStatus m_CurrentStatus;
	EType m_Type;
	float m_Steps[static_cast<int>(EType::Count)];
	bool m_StepEnable[static_cast<int>(EType::Count)];
	Fvector m_Position, m_StartPosition;
	Fmatrix m_RotateMatrix;
};