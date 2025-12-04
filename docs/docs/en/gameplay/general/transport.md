# Transport
## General
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.1

* Basic vehicle functionality from SoC has been restored
* Fixed flying in the air on spawn
* Fixed level transitions 
* Added the **`SCRPTCAR`** class from 1.5.10
```ini
class = SCRPTCAR
```
* Support for `Story ID` (for **`SCRPTCAR`** class):
```ini
story_id = aiw_closed
```

## Position in Vehicle
Position is configured in the `car_definition` section. There are several modes:
1. Via bone (original)
```ini
driver_place = seat_left
```
2. Without specifying. Takes root bone (original)
3. Via direct position specification (Lost Alpha)
```ini
driver_position  = 10,10,10 ; Position
driver_direction = 1,0,1    ; Camera direction
```

## Dashboard
The car supports a dashboard. To do this, you need to create a `dashboard` section in the car model configuration

```ini
[dashboard]
rpm_bone = rpm_strelka                   ; Tachometer bone
fuel_bone = fuel_strelka                 ; Fuel gauge bone
speed_bone = speed_strelka               ; Speedometer bone
rpm_angle = 5.0, -180.0, -240.0, 5.0     ; [NET Online] zero_rpm, max_rpm, min_angle, max_angle
fuel_angle = 5.0, -180.0, -240.0, 5.     ; [NET Online] zero_fuel, max_fuel, min_angle, max_angle
speed_angle = 15.0, -133.0, -250.0, 15.0 ; [NET Online] 0 km/h, 100 km/h, min_angle, max_angle
```
For correct rotation or movement of bones, you need to configure `bind_rotation`

![image](https://github.com/user-attachments/assets/1b802575-632d-4364-b286-d7ac90eac133)

### Setup Examples:
Currently available for configuration: `rpm_*`, `fuel_*`, `speed_*`
```ini
*_bone = bone_name
*_angle = zero_value, max_value, min_angle, max_angle
```
For convenience, angles are also selected via `bind_rotation`

::: details Setup Instructions

#### Configuring `zero_value` and `max_value`

To configure `zero_value`, select a value when the gauge needle points to zero. To configure `max_value` (for tachometer and fuel gauge this value is set in the config, so you choose what the maximum value is, for the speedometer this value is fixed - 100 km/h), select the necessary value.

![image](https://github.com/user-attachments/assets/255a37dd-b615-4205-8f32-5916123184e5)

#### Configuring `min_angle` and `max_angle`
To configure the gauge limits, you need to select the extreme left and right position of the needles and write them in ascending order to the config.

![image](https://github.com/user-attachments/assets/c6b953c1-8db5-4079-8b6b-451d7fcb07f3)

:::

## Usable Bones 
This system allows you to set script callbacks on Use() by bone name. To do this, you need to register a list of bones and functions in `car_definition`
```ini
; Custom usable 
usable_bones = back_wheel, left_door
usable_bones_callback = test_car.fuel_test, test_car.block_left_door
```
Example script:
```lua
function fuel_test(obj_car)
    obj_car.fuel = 10
    return false -- Pass further processing to the engine 
end

function block_left_door(obj_car)
    return true -- Block further Use() event processing for the engine 
end
```
* Text hint 
```xml
<string id="car_use">
	<text>Use ($$ACTION_USE$$)</text>
</string>
```
> For more information on new script exports, see [here](/scripting/new-functions#ccar).
## UI Indicators
![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/d9b17009-669a-431b-953f-3b44d606f8b8)
### Files: 
* configs/ui/car_panel.xml <-- XML Description of the window 
* configs/ui/textures_descr/ui_car_panel.xml <-- Icon registration on texture 
* textures/ui/car_panel.dds <-- Texture with icons

### XML Window 

::: details Click me to toggle code

``` xml
<?xml version='1.0' encoding="UTF-8"?>
<w>
	<car_panel x="0" y="0" width="1024" height="768"> <!-- The window itself -->
	    <car_static x="840" y="525" width="175" height="100" stretch="1"> <!-- Background -->
			<texture>ui_car_panel_back</texture>
		</car_static>
	     <car_health_progress_bar x="45" y="12" width="114" height="12" horz="1" min="0" max="1" pos="0"> <!-- Health indicator -->
		    	    <progress stretch="1">
		        		<texture r="194" g="8" b="8" a="200">ui_inGame2_inventory_progress_bar</texture>
			        </progress>
		</car_health_progress_bar>
	     <car_fuel_progress_bar x="45" y="30" width="114" height="12" horz="1" min="0" max="100" pos="0"> <!-- Fuel indicator -->
		    	    <progress stretch="1">
		        		<texture r="8" g="122" b="122" a="200">ui_inGame2_inventory_progress_bar</texture>
			        </progress>
		</car_fuel_progress_bar>

		<car_engine_lamp> <!-- Engine indicator -->
			<on x="26" y="47" width="16" height="16" stretch="1">
				<texture r="122" g="122" b="8">ui_car_panel_engine</texture>
			</on>
			<off x="26" y="47" width="16" height="16" stretch="1">
				<texture a="120">ui_car_panel_engine</texture>
			</off>
		</car_engine_lamp>
		
		<car_light_lamp> <!-- Headlights indicator -->
			<on x="44" y="47" width="14" height="12" stretch="1">
				<texture r="122" g="122" b="8">ui_car_panel_light</texture>
			</on>
			<off x="44" y="47" width="14" height="12" stretch="1">
				<texture>ui_car_panel_light</texture>
			</off>
		</car_light_lamp>

		<car_speed_mode x="30" y="77" width="16" height="16" stretch="1"> <!-- Current gear indicator -->
			<text font="font_graffiti" />
		</car_speed_mode>
	</car_panel>
</w>
```
:::

## Trunk 
### Registration 
In the **userdata** config you need to specify the bone name in the `car_definition` section
```ini
trunk_bone = back_wheel
```
![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/86361179-a30f-432a-b8a8-b40acde72e62)
### Text Hint
```xml
<string id="car_trunk_use">
 <text>Open trunk ($$ACTION_USE$$)</text>
</string>
```
### Icon and Name 
This feature is optional. To activate it, add the following lines to the main car config: 
```ini
name = "niva" ; Car name (Supports text sections)
icon = ui_npc_monster_pseudodog ; car icon
```
