#pragma once
#include "../control_combase.h"

class CBurerFastGravi : public CControl_ComCustom<> {
	typedef CControl_ComCustom<> inherited;

public:
	virtual bool	check_start_conditions	() override;
	virtual void	activate				() override;
	virtual void	deactivate				() override;
	virtual void	on_event				(ControlCom::EEventType, ControlCom::IEventData*) override;
	
private:	
			void	process_hit				();

};

