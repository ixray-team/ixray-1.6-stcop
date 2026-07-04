#include "Application.h"

int ENTRY_FUNCTION(ENTRY_ARGS)
{
	CApplication App(ENTRY_ARGS_PUSH);
	return App.Run();
}
