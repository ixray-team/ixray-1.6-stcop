#pragma once

#if 0
void maya_import_tools::start_progress(size_t amount, const char* status)
{
	MProgressWindow::reserve();
	MProgressWindow::setInterruptable(false);
	MProgressWindow::setTitle("X-Ray Import");
	MProgressWindow::setProgressStatus(status);
	MProgressWindow::setProgressRange(0, int(amount & INT_MAX));
	MProgressWindow::startProgress();
}

void maya_import_tools::advance_progress()
{
	MProgressWindow::setProgress(MProgressWindow::progress() + 1);
}

void maya_import_tools::end_progress()
{
	MProgressWindow::endProgress();
}
#endif

static inline void start_progress(size_t amount, const char* status) {}
static inline void advance_progress() {}
static inline void end_progress() {}
