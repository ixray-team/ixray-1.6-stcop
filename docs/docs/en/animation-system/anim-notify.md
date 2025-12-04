# Callbacks for Animation Frames (Anim Notify)
- Anim Notify is a callback system triggered when playing animations. It allows actions to be attached to specific bones of the model during certain animation frames. For example, if an NPC has a sneezing animation and does it several times, anim notify allows you to tie a separate callback to each sneezing frame so the mouth bone plays the sound of the NPC sneezing.
- You can attach the following actions to animation frames:
> - Give info portion
> - Remove info portion
> - Call Lua function
> - Play particle on bone
> - Play sound on bone
## Creating a callback
![image](https://github.com/user-attachments/assets/34aec8cb-30c5-4656-a563-ffc8cb9fad48)
The callback field appears as a tree with 3 levels of rows:
1. All callbacks in the animation
2. All callbacks on a bone
3. Group of callbacks on a bone
- Level 3 is for cases where two or more callbacks need to be attached to the same bone at the same or close frames. Only level 3 tags are interactive.
## Adding a callback
1. In AE, open the KeyForm window, expand the `Notifies` field, and press `Add`.
2. In the popup window, select the desired bone where callbacks are added. A row will appear with notifies corresponding to the selected bone.
3. Expand the field with the bone and press the `Add` button. This creates a row of notifies where callbacks can be placed.
- Alternatively, move the animation playback slider to the desired frame and press `Current keyframe` to the right of the target row.
## Removing a callback
- To remove a callback, choose one of two actions:
> - Hover over the Notify tag and right-click.
> - In the settings field, press the `Del` button.
- To remove an entire row, press the corresponding row`s `Del` button.
## Clearing a bone`s callbacks
- To clear all callbacks from a bone, press `Remove Bone` in the relevant bone field.
## Callback settings window
- Callback info is stored in a separate config. Hover on the desired callback and left-click to open a settings field below all rows. Enter the section name the callback will point to in the configs into `External ref`.
### Storing info in the config
- Callback names are used as identifiers in configs. Multiple callbacks can have the same name.
- Callback names are case-insensitive. For example, `Callback_Name` and `callback_name` are considered the same.
- In `gamedata/configs/misc` create a folder `anim_notify` and add a file with the following section:
```
[callback_name]
type = callback_type ; options: `give_info`, `disable_info`, `lua_functor`, `play_sound`, or `play_particle`
Info = info_portion_name ; only for `give_info` or `disable_info`
Func = script_filename.function_name ; only for `lua_functor`
particle = path\to\particle\particle_name ; only for `play_particle`
sound = path\to\sound\sound_name ; only for `play_sound`
```
