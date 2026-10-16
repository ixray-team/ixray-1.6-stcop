#pragma once

// Called after selecting the level's cross table, before AI caches are built.
void LoadAIGridEditorPatch();
bool HasAIGridEditorPatch();
// Called before the render device/factory are destroyed; safe to call repeatedly.
void ShutdownAIGridEditor();
void RenderAIGridEditorWindow();
void RenderAIGridEditorWorld();
