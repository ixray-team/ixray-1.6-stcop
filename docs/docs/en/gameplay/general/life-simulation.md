# Life Simulation

## Campfire Gatherings

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

### Overview

The `xr_kamp` behavior scheme from the original games "Shadow of Chernobyl" (SoC) and "Clear Sky" (CS) has been restored, allowing NPCs to gather at campfires for rest and socializing.

### Setup Requirements

For the system to work, you need to create two main elements on the level:

#### 1. Way Point
- **Name**: Should match the pattern `(SmartTerrainName)_kamp_(id)`
- **Purpose**: Defines the point where NPCs come to socialize

#### 2. Camp Zone  
- **Type**: Regular camp_zone in the level editor
- **Purpose**: Defines the area around the campfire where NPCs interact

### Setup Example

Let's say you have a smart terrain named `jup_a6`. The setup will look like this:

```
Way Point: jup_a6_kamp_1
Camp Zone: [any name, but logically named jup_a6_camp_1]
```

### Operation Features

- NPCs automatically find suitable gathering points by name pattern
- The system integrates into the general NPC life cycle
- Requires no additional scripting or configuration

![image](https://github.com/user-attachments/assets/73fb4fa9-c89b-4b47-ad04-ed951647ab50)
