# Волосы
> [!IMPORTANT]
> **Статус**: WIP <br>
> **Минимальная версия**: 1.4

> Добавлена простейшая шейдерная анимация волос от ветра
<Video url="https://youtu.be/mX3lMyu-MwE"/>

## Использование
* Данный алгоритм проверялся на волосах ленточного типа
* * Пример: https://sketchfab.com/3d-models/messy-low-bun-female-hairstyle-db7a61ea9f2c45ddb0b7571972f9738f

* После чего нужно сделать UV маску (`[texturename]_hairmask.dds`) с информацией о позиции волос на модели. Вся информациях хранится в красном канале. Данных из остальных не учитываются. 
* * __0 - не симулируем данный участок, 255 - полная симуляция__.
![image](https://sun9-46.userapi.com/s/v1/ig2/LSjoofHrreQx7g9ZyIanT8RCIrtXUqdw7LzTzAMmaX2mbcQibv6ohvSOR__mVoWZlGAvuMjTjp5Qg7jNz-Dk0f6q.jpg?quality=95&as=32x31,48x46,72x69,108x104,160x154,240x230,360x345,480x461,540x518,640x614,720x691,963x924&from=bu&cs=963x0)
> Пример самой простой маски

* Проверить результат в игре
