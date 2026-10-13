# Actor camera focus

Modder API: [English](../docs/main/en/scripting/camera-focus.md), [Russian](../docs/main/ru/scripting/camera-focus.md).

`CameraFocus.h` contains an actor-owned, allocation-free transition state with no engine dependencies. Phases are idle, entering, holding and leaving. Entry/exit use smoothstep; holding uses exponential angular tracking. Frame time is consumed across phase boundaries, so zero durations and large frame deltas terminate correctly. Invalid requests preserve the previous session. A replacement uses the current camera direction/FOV and retains the original return direction.

`ActorCameraFocus.cpp` owns the integration. The target is a `CGameObject*`, retained only for tracking, with a world-space offset. `CActor::net_Relcase` drops the reference and begins exit before deletion; `getDestroy()` also starts exit. Point focus and object snapshots retain only a position. Start requests validate single player, actor ownership, first-person mode, actor lifetime, vectors and parameters with explicit control flow in all configurations.

`CActor::cam_Update` updates focus after calculating the world camera position, before the base camera update. Direction is written into first-person yaw/pitch within camera limits. Focus FOV replaces the normal frame FOV locally; it never writes `g_base_fov` or `g_fov`. Camera collision, rig processing and regular effectors remain in the existing pipeline. Exit blends toward the live natural FOV, rather than a saved FOV setting. When direction restoration is disabled, leaving releases direction control.

Gameplay input uses `IsCameraFocusInputLocked()` in keyboard press/hold, mouse wheel/movement, gyro, touchpad, gamepad press/hold/stick and asynchronous gameplay key checks. Releases remain enabled. Starting a locked focus clears movement wishes, repeated actions, firing and weapon zoom. The lock belongs to the focus state; global script input flags are never acquired or released by focus.

Death, net destruction/spawn and save loading reset the state; focus is not serialized. Leaving first person and showing the talk window reset immediately. Absolute scripted camera APIs reset before adding their effector; camera updates also detect absolute positioning, demo cameras, vehicles and control-entity changes. Lifecycle resets deliberately skip the transition to hand control to the incoming camera system.

`camera_focus_params` is registered in `actor_script.cpp`. `ActorCameraFocus_script.cpp` validates the receiving game object and forwards the five actor methods registered in `script_game_object_script3.cpp`. No Lua callbacks run from camera processing; scripts poll active/state in their usual update.

## Verification

Build `xrGame` in the required configuration. Transition checks can include the production `CameraFocus.h` directly without engine dependencies. Cover phase boundaries, smooth entry, timed/indefinite hold, cancellation during entry, live FOV restoration, zero durations, input release, angle wrapping, tracking frame-rate independence, replacement, direction restoration and invalid input.

Runtime checks should include point/NPC targets, both tracking modes, deleting a tracked object, repeated start, stopping during entry/hold/exit, unlocked movement, held fire/aim/movement before locking, mouse/gamepad input, a pre-existing `level.disable_input()` restriction, camera changes, dialogs, save/load, actor death and absolute cutscenes. Verify first-person target alignment and restoration with a nondefault FOV on R1/R2/R4; no renderer-specific implementation is involved.
