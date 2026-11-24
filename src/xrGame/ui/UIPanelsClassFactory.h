#ifndef UIPANELSCLASSFACTORY
#define UIPANELSCLASSFACTORY

class UITeamPanels;
class UITeamState;

class UIPanelsClassFactory final
{
private:
public:
	UIPanelsClassFactory();
	~UIPanelsClassFactory();
	
	UITeamState* CreateTeamPanel(shared_str const & teamName, UITeamPanels *teamPanels);
};

#endif