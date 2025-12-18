> [!IMPORTANT]
> **Статус**: Supported / In Development <br>
> **Минимальная версия**: 1.4?

# Important Information
Particles in IX-Ray are an evolution of the particle system from the original X-Ray 1.6. The engine is guaranteed to be able to read particles created for the original engine. However, both unpacked and packed particles that were created (or even just saved) in the IX-Ray SDK may not work on other engines (including other SDKs), especially if features exclusive to IX-Ray were used during their creation.

If you plan to use the Particle Editor from the IX-Ray SDK for mod development on a different engine, engine adaptation will most likely be required.

# Support for Unpacked Particles
The game can read unpacked particles from the gamedata folder or from addons. Moreover, unpacked particles take priority over packed ones.

To make particles readable from gamedata/an addon, place them in a `particles` folder inside the gamedata or addon directory. Inside the `particles` folder, you can create your own hierarchy. However, to override data from a packed archive, the file you are overriding must have the exact same name as the particle being overridden and be located in the same subdirectory.

For example, if we want to override the `anomaly2/effects/anomaly_shift` particle, we need to create a file named `anomaly_shift.pe` and place it in the `particles/anomaly2/effects` folder.

# Supported Particle Types
Currently, IX-Ray supports only **sprite particles**: multiple sprites appear in space, which are created, modified, and destroyed according to specific rules. Support for new particle types may be added in the future.

# Particle Animation Curve
A new data type for particles - a pre-made 4-channel (RGBA) animation curve. Can be used when creating complex motion, size, or color change behaviors. In unpacked form, the curve data is stored in a `.pac` file.

A single animation curve with its corresponding modifier (`Color Animator`, `Size Animator`, `Velocity Animator`) can replace multiple standard modifiers (`Target Color`, `Target Velocity`, `Target Size`) with manually set `TimeFrom` and `TimeTo` parameters. Furthermore, a once-created curve can be reused in other particles without the need to reconfigure the animation.

![image](https://github.com/user-attachments/assets/7c11168e-b45d-4e49-a07f-9840af50aa22)
![image](https://github.com/user-attachments/assets/c6aa06ed-1635-4019-b5af-68320590e31d)

If you select a curve from the element list, the curve editor will open in the `RightBar`. The main area will display a visualization of the curves. The strip below the main area will contain the animation keys. The X-axis at the bottom shows the key time in milliseconds, and the Y-axis on the left shows the curve values depending on time.

Clicking on a key opens the settings for the curve values at that specific key. Holding `Ctrl` and clicking on an empty space in the key strip creates a new key. Its default values will be interpolated between the two neighboring keys. You can click and drag a key. It is even allowed to drag the outermost keys or drag a key past others - upon releasing, the keys will be automatically sorted, and the curve's total duration will be updated: the leftmost key will have time 0, and the rightmost key will update the total curve duration.

In the key settings, you can precisely adjust the time of the key: either by using the buttons on the right to move the key by a constant value or by entering a value in the field. Similarly, you can adjust the value of any of the 4 curves (RGBA) at this key. Additionally, there is a button to delete the key.

# New Modifiers
The particle system has been extended with new modifiers. The list may be expanded during development.

## Binders (In Development)
Modifiers created to allow direct modification of any modifier parameter from the game logic. Currently, these modifiers support parameter changes exclusively from C++. The system is under development; unexpected particle behavior may occur when changing its parameters through these modifiers.

Modifier types:
* `Bind Velocity` - create a binding to the parameter responsible for the particle's speed and direction of movement.
* `Bind Rotation` - create a binding to the parameter responsible for the particle's rotation.
* `Bind Size` - create a binding to the parameter responsible for the particle's size.
* `Bind Color (RGB)` - create a binding to the parameter responsible for the particle's color.
* `Bind Color (alpha)` - create a binding to the parameter responsible for the particle's transparency (alpha).

## Аниматоры
Modifiers created to animate a particle parameter according to an assigned curve:
* `Color Animator`- animate the particle's color. Uses all 4 curves (RGBA).
* `Size Animator` - animate the particle's size. Uses the first 3 curves (RGB = XYZ).
* `Velocity Animator` - animate the particle's velocity. Uses the first 3 curves (RGB = XYZ).
* `Velocity Rotation Animator` - animate the particle's rotation relative to its starting point. Uses the first 3 curves (RGB = XYZ).

# Changes in the Editor Interface
## Element Type in the Tree and Child Elements
![image](https://github.com/user-attachments/assets/82ce92ed-a4b0-4db0-84c6-966881ef0c94)
![image](https://github.com/user-attachments/assets/12a11e0c-11e3-4257-a842-1bafb3a20f84)
![image](https://github.com/user-attachments/assets/2e66cd9f-4b20-4ed2-8fcb-15e4b233adeb)

Each element now has an identifier for the type it belongs to:
* `PG` - Particle Group
* `PE` - Particle Effect
* `PAC` - Particle Animation Curve

Additionally, group and particle effects now have child elements:
* `EFFECT` - a reference to the settings of a particle effect that is part of a group.
* `ACTION` - a reference to the settings of an individual particle modifier.

A context menu can be opened by right-clicking (RMB) on any element type. For example, you can directly delete a single particle from a group particle list or rename a modifier of a single particle.

Furthermore, if you select a child element of a particle, you can open settings in the `RightBar` that are exclusive to the selected element. This is convenient when working with a particle that has a large number of emitters or modifiers.
