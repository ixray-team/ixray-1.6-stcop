# Actor Animations

The **Actor Animations** panel controls the loaded actor's skeletal animation. Open it from the in-game tools menu in a configuration with `DEBUG_DRAW`; Alt toggles mouse capture.

## Controls and data

**Reload library** rebuilds the animation list from loaded level objects. Filter the list, select an animation, then use Play/Pause, Stop, Loop and the timeline step controls. The animation context menu adds a clip to the playlist or favorites; the star toggles a favorite. Favorites are stored in `$app_data_root$/actor_animations/favorites.ltx`.

**Override actor animation** blocks normal game animation selection while the panel controls the actor. Turn it off when returning to ordinary gameplay. The panel needs a loaded actor with an animated visual; a model or animation missing from the current level's library cannot be previewed.

## Post-build checks

In Debug, then RelWithDebInfo, load a level and open the panel. Reload the library, play an animation, pause, step and stop it. Repeat with override on and off. Add a favorite and confirm it after restarting the game. Check this on Windows and macOS after portability changes.
