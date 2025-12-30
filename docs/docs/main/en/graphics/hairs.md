# Hair
> [!IMPORTANT]
> **Status**: WIP <br>
> **Minimum version**: 1.4

> Added a basic wind-driven hair shader animation
<Video url="https://youtu.be/mX3lMyu-MwE"/>

## Usage
* This algorithm has been tested on ribbon-style hair
* * Example: https://sketchfab.com/3d-models/messy-low-bun-female-hairstyle-db7a61ea9f2c45ddb0b7571972f9738f

* After that you need to generate a UV mask (`[texturename]_hairmask.dds`) containing information about the hair position on the model. All information is stored in the red channel. Data from other channels is ignored.
* * **0 - do not simulate this area, 255 - full simulation**.
![hair mask example](https://sun9-46.userapi.com/s/v1/ig2/LSjoofHrreQx7g9ZyIanT8RCIrtXUqdw7LzTzAMmaX2mbcQibv6ohvSOR__mVoWZlGAvuMjTjp5Qg7jNz-Dk0f6q.jpg?quality=95&as=32x31,48x46,72x69,108x104,160x154,240x230,360x345,480x461,540x518,640x614,720x691,963x924&from=bu&cs=963x0)
> Example of the simplest mask

* Check the result in-game
