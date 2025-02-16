///////////////////////////////////////////////////////////////
// encyclopedia_article.h
// ñòðóêòóðà, õðàíÿùàÿ è çàãðóæàþùàÿ ñòàòüè â ýíöèêëîïåäèþ
///////////////////////////////////////////////////////////////

#pragma once

#include "encyclopedia_article_defs.h"
#include "xml_str_id_loader.h"
#include "shared_data.h"

#include "../xrUI/Widgets/UIStatic.h"
//////////////////////////////////////////////////////////////////////////
// SInfoPortionData: äàííûå äëÿ InfoProtion
//////////////////////////////////////////////////////////////////////////
struct SArticleData : CSharedResource
{
	shared_str name;
	shared_str group;
	
	//êàðòèíêà
	CUIStatic image;

	//òåêñò ñòàòüè
	xr_string	text;
	
	//ñåêöèÿ ltx, îòêóäà ÷èòàòü äàííûå
//	shared_str ltx;

	// Òèï ñòàòüè
	ARTICLE_DATA::EArticleType	articleType;
	shared_str					ui_template_name;
};

class CEncyclopediaArticle;

class CEncyclopediaArticle : public CSharedClass<SArticleData, shared_str, false>
							,public CXML_IdToIndex<CEncyclopediaArticle>
{
private:
	typedef CSharedClass<SArticleData, shared_str, false>					inherited_shared;
	typedef CXML_IdToIndex<CEncyclopediaArticle>							id_to_index;

	friend id_to_index;
public:
						CEncyclopediaArticle		();
	virtual				~CEncyclopediaArticle		();

	virtual void Load	(shared_str str_id);

protected:
	shared_str			m_ArticleId;
	virtual	void		load_shared					(LPCSTR);
	static  void		InitXmlIdToIndex			();
public:
	const shared_str	Id							()						{return m_ArticleId;}
	SArticleData*		data						()						{ VERIFY(inherited_shared::get_sd()); return inherited_shared::get_sd();}
};