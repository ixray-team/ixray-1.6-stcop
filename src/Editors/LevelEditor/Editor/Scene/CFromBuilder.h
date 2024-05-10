#pragma once
class CFromBuilder
{
public:
	CFromBuilder();
	virtual			~CFromBuilder();
	bool build();
	bool empty()const;
	void clear();
	void	Load(CObjectSpace* To, CDB::build_callback cb);
protected:
	xr_vector<CDB::TRI> m_Faces;
	xr_vector<Fvector> m_Vertex;
	Fbox m_Box;
};