#include "stdafx.h"
#include "PolterChem.h"
#include "../PhysicsShellHolder.h"
#include "../level.h"
#include "../actor.h"
#include "../xrPhysics/icolisiondamageinfo.h"

CPolterChem::CPolterChem(IPolterInterface* polter) : inherited(polter) {

}

CPolterChem::~CPolterChem() {

}

void CPolterChem::load(LPCSTR section){
	inherited::load(section);

}

void CPolterChem::update_schedule() {
	inherited::update_schedule();

}

void CPolterChem::update_frame() {
	inherited::update_frame();

}

void CPolterChem::on_hide() {
	inherited::on_hide();

}

void CPolterChem::on_show() {
	inherited::on_show();

}

void CPolterChem::on_destroy() {
	inherited::on_destroy();

}

void CPolterChem::on_die() {
	inherited::on_die();

}

void CPolterChem::on_hit(SHit* pHDS) {
	inherited::on_hit(pHDS);

}
