#ifndef UIPANELSCLASSFACTORY
#define UIPANELSCLASSFACTORY

#include "UITeamState.h"
#include "UIPlayerItem.h"

class UITeamPanels;

class UIPanelsClassFactory final
{
private:
public:
	UIPanelsClassFactory();
	~UIPanelsClassFactory();
	
	UITeamState* CreateTeamPanel(shared_str const & teamName, UITeamPanels *teamPanels);
};

#endif