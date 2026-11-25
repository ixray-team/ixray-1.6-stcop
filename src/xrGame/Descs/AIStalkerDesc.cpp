#include "StdAfx.h"
#include "AIStalkerDesc.h"

void SAIStalker::Load(const shared_str& Section)
{
	XReflect::ReadIni(*this, Section);
	XReflect::ReadIni(Queues, FireQueueSection.size() && pSettings->section_exist(FireQueueSection) ? FireQueueSection : Section);
}
