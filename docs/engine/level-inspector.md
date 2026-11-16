# Level Inspector

The guide to the tool's features, controls, data sources and limitations is on the website: [Level Inspector](../docs/main/ru/utilities/level-inspector.md) (in Russian).

## Post-build checks

- Load a level, open **Game → Level Inspector** and check that Alt toggles mouse capture.
- Turn on objects, zones and selection; point the camera at an object and a zone, check the details and the **Copy** button.
- Check the display of paths, graphs and patrol point filters.
- Check the display of the object skeleton and HUD, collision, HOM and SOM on a level that has this data.
- On the **Render** tab, toggle **Sound Zones** on a level with sound zones and check that cyan wireframes follow rotated zones. Switch to a level without zones and check that the wireframes disappear.
- Check both keys on the **Debug** tab in hold and toggle modes.

Repeat object and zone picking on Windows and macOS in Debug and RelWithDebInfo, including clicks on empty space. The selected object and reported hit should follow the cursor.
