///////////////////////////////////////////////////////////////
// encyclopedia_article.cpp
// структура, хранящая и загружающая статьи в энциклопедию
///////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "encyclopedia_article.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "ui/UIInventoryUtilities.h"
#include "object_broker.h"
#include "../Include/xrRender/UIShader.h"

using namespace InventoryUtilities;

void ARTICLE_DATA::load (IReader& stream) 
{
	load_data(receive_time, stream); 
	load_data(article_id, stream); 
	load_data(readed, stream); 
	load_data(article_type, stream);
}

void ARTICLE_DATA::save (IWriter& stream)		
{
	save_data(receive_time, stream); 
	save_data(article_id, stream); 
	save_data(readed, stream); 
	save_data(article_type, stream);
}

ISaveObject& operator<<(ISaveObject& Object, ARTICLE_DATA& Data)
{
	BEGIN_CHUNK(Object, "ARTICLE_DATA")
	{
		Object << Data.receive_time << Data.article_id << Data.readed << Data.article_type;
	}
	return Object;
}

CEncyclopediaArticle::CEncyclopediaArticle()
{
}

CEncyclopediaArticle::~CEncyclopediaArticle()
{
	if (data()->image.GetParent())
		data()->image.GetParent()->DetachChild(&(data()->image));
	if (data()->model.GetParent())
		data()->model.GetParent()->DetachChild(&(data()->model));
}

/*
void CEncyclopediaArticle::Load	(ARTICLE_STR_ID str_id)
{
	Load	(id_to_index::IdToIndex(str_id));
}
*/
void CEncyclopediaArticle::Load	(shared_str  id)
{
	m_ArticleId = id;
	inherited_shared::load_shared(m_ArticleId, nullptr);
}


void CEncyclopediaArticle::load_shared	(const char*)
{
	const ITEM_DATA& item_data = *id_to_index::GetById(m_ArticleId);

	CUIXml*		pXML		= item_data._xml;
	pXML->SetLocalRoot		(pXML->GetRoot());

	//loading from XML
	XML_NODE* pNode = pXML->NavigateToNode(id_to_index::tag_name, item_data.pos_in_file);
	THROW3(pNode, "encyclopedia article id=", *item_data.id);

	//текст
	data()->text = pXML->Read(pNode, "text", 0, "");
	//имя
	data()->name = pXML->ReadAttrib(pNode, "name", "");
	//группа
	data()->group = pXML->ReadAttrib(pNode, "group", "");
	//секция ltx, откуда читать данные
	const char* ltx = pXML->Read(pNode, "ltx", 0, nullptr);
	const char* model = pXML->Read(pNode, "model", 0, nullptr);
	data()->model.SetVisual(nullptr);

	if (!model)
	{
		if (ltx && pSettings->section_exist(ltx))
		{
			InventoryIconParams icons_struct = GetInventoryIconParams(ltx);
			data()->image.SetShader(GetEquipmentIconsShader(icons_struct.icons_texture));

			Frect tex_rect;
			float scaleIcon = icons_struct.scaleIcon;
			tex_rect.x1 = icons_struct.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
			tex_rect.y1 = icons_struct.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
			tex_rect.x2 = icons_struct.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
			tex_rect.y2 = icons_struct.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);
			tex_rect.rb.add(tex_rect.lt);
			data()->image.GetUIStaticItem().SetTextureRect(tex_rect);
		}
		else
		{
			if (ltx)
				Msg("! Trying to read data from section [%s] for article [%s], but it doesn't exist!", ltx, m_ArticleId.c_str());

			if (pXML->NavigateToNode(pNode, "texture", 0))
			{
				pXML->SetLocalRoot(pNode);
				CUIXmlInit::InitTexture(*pXML, "", 0, &data()->image);
				pXML->SetLocalRoot(pXML->GetRoot());
			}
		}

		if (data()->image.GetShader() && data()->image.GetShader()->inited())
		{
			Frect r = data()->image.GetUIStaticItem().GetTextureRect();
			data()->image.SetAutoDelete(false);

			const int minSize = 65;

			// Сначала устанавливаем если надо минимально допустимые размеры иконки
			if (r.width() < minSize)
			{
				float dx = minSize - r.width();
				r.x2 += dx;
				data()->image.SetTextureOffset(dx / 2, data()->image.GetTextureOffeset()[1]);
			}

			if (r.height() < minSize)
			{
				float dy = minSize - r.height();
				r.y2 += dy;
				data()->image.SetTextureOffset(data()->image.GetTextureOffeset()[0], dy / 2);
			}

			data()->image.SetWndRect(Frect().set(0, 0, r.width(), r.height()));
		}
	}
	else if (model)
	{
		bool bUseModelLtx				= pXML->ReadAttribBool(pNode, "model", 0, "use_ltx_model", true);
		const char* base_visual				= bUseModelLtx ? pSettings->r_string(model, "visual") : "";
		IRenderVisual* iVis				= bUseModelLtx ? Render->model_Create(pSettings->read_if_exists<LPCSTR>(model,"3d_static_visual_name",base_visual)) : Render->model_Create(model);
		float rot_x						= deg2rad(pXML->ReadAttribFlt(pNode, "model", 0, "x", 0.f));
		float rot_y						= deg2rad(pXML->ReadAttribFlt(pNode, "model", 0, "y", 0.f));
		float rot_z						= deg2rad(pXML->ReadAttribFlt(pNode, "model", 0, "z", 0.f));
		float scale						= pXML->ReadAttribFlt(pNode, "model", 0, "scale", 1.f);
		data()->model.SetXYZ			(rot_x, rot_y, rot_z);
		data()->model.SetVisual			(iVis);
		data()->model.SetScaleFactor	(scale);
	}

	// Тип статьи
	xr_string atricle_type = pXML->ReadAttrib(pNode, "article_type", "encyclopedia");
	if (0 == _stricmp(atricle_type.c_str(), "encyclopedia"))
	{
		data()->articleType = ARTICLE_DATA::eEncyclopediaArticle;
	}
	else if (0 == _stricmp(atricle_type.c_str(), "journal"))
	{
		data()->articleType = ARTICLE_DATA::eJournalArticle;
	}
	else if (0 == _stricmp(atricle_type.c_str(), "task"))
	{
		data()->articleType = ARTICLE_DATA::eTaskArticle;
	}
	else if (0 == _stricmp(atricle_type.c_str(), "info"))
	{
		data()->articleType = ARTICLE_DATA::eInfoArticle;
	}
	else
	{
		Msg("incorrect article type definition for [%s]", *item_data.id);
	}

	data()->ui_template_name = pXML->ReadAttrib(pNode, "ui_template", "common");
}

void CEncyclopediaArticle::InitXmlIdToIndex()
{
	if(!id_to_index::tag_name)
		id_to_index::tag_name = "article";
	if(!id_to_index::file_str)
		id_to_index::file_str = pSettings->r_string("encyclopedia", "files");
}
