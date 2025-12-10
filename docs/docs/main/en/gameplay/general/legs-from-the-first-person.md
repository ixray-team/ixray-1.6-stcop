# First-Person Legs
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.1

## Config Setup

A first-person legs system has been implemented. To display legs, you need to add the following to the hand HUD section (`actor_hud_*`):

```ini
[actor_hud_01]
legs_visual = actor\stalker_legs\trenchcoat_outfit ; Visual of legs for this hand combination
legs_shift_delta = -0.67                           ; Legs offset relative to actor direction (optional, default value -0.55)
```

## Model Setup

For this system to work correctly, a special model is required that represents the character model without a head and hands, with the body attached to the `bip01_pelvis` bone

![image](https://github.com/user-attachments/assets/c8a52ae3-2f27-47bd-923e-43ae03e14283)

Animations for legs are required. The current implementation requires the same skeleton for the legs model and the character model.
