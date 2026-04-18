#include "StdAfx.h"

#include "MMSound.h"
#include "../../xrUI/xrUIXmlParser.h"

CMMSound::CMMSound()
{}

CMMSound::~CMMSound()
{
	all_Stop();
}

void CMMSound::Init(CUIXml& xml_doc, const char* path){
	string256 _path;	
	m_bRandom = xml_doc.ReadAttribInt(path, 0, "random")? true : false;

	int nodes_num	= xml_doc.GetNodesNum(path, 0, "menu_music");

	XML_NODE* tab_node = xml_doc.NavigateToNode(path,0);
	xml_doc.SetLocalRoot(tab_node);	
	for (int i = 0; i < nodes_num; ++i)
		m_play_list.push_back(xml_doc.Read("menu_music", i, ""));		
	xml_doc.SetLocalRoot(xml_doc.GetRoot());

   xr_strconcat(_path, path,":whell_sound");
	if (check_file(xml_doc.Read(_path, 0, "")))
        m_whell.create(xml_doc.Read(_path, 0, "") ,st_Effect,sg_SourceType);

	xr_strconcat(_path, path,":whell_click");
	if (check_file(xml_doc.Read(_path, 0, "")))
        m_whell_click.create(xml_doc.Read(_path, 0, ""),st_Effect,sg_SourceType );

}

bool CMMSound::check_file(const char* fname){
	string_path		_path;
	xr_strconcat(_path, fname, ".ogg");
	return FS.exist(_game_sounds_, _path) ? true : false;		
}

void CMMSound::whell_Play()
{
	if (m_whell.handle())
		m_whell.play(nullptr, sm_Looped | sm_Intro);
}

void CMMSound::whell_Stop(){
	if (m_whell.is_playing())
		m_whell.stop();
}

void CMMSound::whell_Click()
{
   	if (m_whell_click.handle())
		m_whell_click.play(nullptr, sm_Intro);
}

void CMMSound::whell_UpdateMoving(float frequency){
	m_whell.set_frequency(frequency);
}

void CMMSound::music_Play()
{
	if (m_play_list.empty())
		return;

	int i = Random.randI(m_play_list.size());

	string_path		_path;
	xr_strconcat(_path, m_play_list[i].c_str(), ".ogg");
	if (FS.exist(_game_sounds_, _path))
	{
		m_music_stereo.create(_path,st_Music,sg_SourceType);
   		m_music_stereo.play(nullptr, sm_Intro | sm_Looped);
	}
	else
	{
		string_path		_path_l;
		string_path		_path_r;
		xr_strconcat(_path_l, m_play_list[i].c_str(), "_l.ogg");
		xr_strconcat(_path_r, m_play_list[i].c_str(), "_r.ogg");
		VERIFY(FS.exist(_game_sounds_, _path_l));
		VERIFY(FS.exist(_game_sounds_, _path_r));

		m_music_l.create(_path_l, st_Music, sg_SourceType);
		m_music_r.create(_path_r, st_Music, sg_SourceType);

		m_music_l.play(nullptr, sm_Intro | sm_Looped);
		//m_music_l.set_panning(1.0f, 0.f);
		m_music_r.play(nullptr, sm_Intro | sm_Looped);
		//m_music_r.set_panning(0.f, 1.0f);
	}
}

void CMMSound::music_Update()
{
	if (Device.Paused() || !Device.b_is_Active) return;

	if ( (m_music_stereo.handle() && !m_music_stereo.is_playing()) || (m_music_l.handle() && !m_music_l.is_playing()) || (m_music_r.handle() && !m_music_r.is_playing()) )
		music_Play();
}

void CMMSound::music_Stop()
{
	m_music_l.stop();
	m_music_r.stop();
	m_music_stereo.stop();
}

void CMMSound::all_Stop(){
	music_Stop();
	m_whell.stop();
	m_whell_click.stop();
}