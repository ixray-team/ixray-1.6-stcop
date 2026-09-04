#pragma once

class CPlanar : public CEditShape
{
	typedef CCustomObject inherited;

	xr_vector<Fvector>	m_ProjVerts;

	void			RebuildProjection	();
	void			OnStiffnessChange	(PropValue* prop);

public:
	float			m_Stiffness;

					CPlanar				(LPVOID data, const char* name);
	virtual			~CPlanar			();

	void			Construct			(LPVOID data);
	virtual bool	CanAttach			() { return false; }

	virtual bool	LoadStream			(IReader&);
	virtual bool	LoadLTX				(CInifile& ini, const char* sect_name);
	virtual void	SaveStream			(IWriter&);
	virtual void	SaveLTX				(CInifile& ini, const char* sect_name);

	virtual void	MoveTo				(const Fvector& pos, const Fvector& up) override;
	virtual void	OnFrame				() override;
	virtual void	OnUpdateTransform	() override;
	virtual void	Render				(int priority, bool strictB2F) override;
	virtual void	FillProp			(const char* pref, PropItemVec& items) override;
	virtual void	OnShowHint			(AStringVec& dest) override;
};
