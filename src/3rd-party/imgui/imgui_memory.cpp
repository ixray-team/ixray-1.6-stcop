#include "xrCore/xrCore.h"

extern void* (*const g_ImGuiEngineNew[])(size_t) = { &::operator new, &::operator new[] };
extern void (*const g_ImGuiEngineDelete[])(void*) = { &::operator delete, &::operator delete[] };
extern void (*const g_ImGuiEngineSizedDelete[])(void*, size_t) = { &::operator delete, &::operator delete[] };
extern void* (*const g_ImGuiEngineNothrowNew[])(size_t, const std::nothrow_t&) = { &::operator new, &::operator new[] };
extern void (*const g_ImGuiEngineNothrowDelete[])(void*, const std::nothrow_t&) = { &::operator delete, &::operator delete[] };
