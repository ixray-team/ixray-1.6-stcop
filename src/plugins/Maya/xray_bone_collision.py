from contextlib import contextmanager
from functools import partial

from maya import cmds, mel
from maya.api import OpenMaya as om

OWNER = "ixrayBoneCollision"
MENU = "ixrayBoneMenu"
WINDOW = "ixrayBoneWindow"
SHAPE_TYPES = ("None", "Box", "Sphere", "Cylinder")
JOINT_TYPES = ("Rigid", "Cloth", "Joint", "Wheel", "None", "Slider")
AXES = "XYZ"
_ae_columns = set()
_edit_depth = 0
_refresh_pending = False
_ui_generation = 0


def _bone_handle(joint):
	selection = om.MSelectionList()
	selection.add(joint)
	return om.MObjectHandle(selection.getDependNode(0))


def _bone_path(handle):
	if not handle.isValid() or not handle.isAlive():
		return None
	return om.MFnDagNode(handle.object()).fullPathName()


def _defer_action(action):
	generation = _ui_generation

	def run():
		if generation == _ui_generation:
			action()

	cmds.evalDeferred(run)


def _clear_property_panels():
	columns = list(_ae_columns) + [WINDOW + "|scroll|properties"]
	for column in columns:
		if not cmds.columnLayout(column, exists=True):
			_ae_columns.discard(column)
			continue
		for child in cmds.columnLayout(column, query=True, childArray=True) or []:
			cmds.deleteUI(child)


@contextmanager
def _scene_edit():
	global _edit_depth
	outermost = _edit_depth == 0
	_edit_depth += 1
	try:
		if outermost and not cmds.about(batch=True):
			_clear_property_panels()
		yield
	finally:
		_edit_depth -= 1
		if outermost:
			_refresh_templates()


@contextmanager
def undo_chunk():
	cmds.undoInfo(openChunk=True, chunkName="IX-Ray Bone")
	try:
		yield
	finally:
		cmds.undoInfo(closeChunk=True)


def _add(node, name, kind="double", default=0, minimum=None, hidden=False, enum=None):
	if cmds.attributeQuery(name, node=node, exists=True):
		return
	args = dict(longName=name, hidden=hidden, category="IX-Ray Bone")
	if kind == "string":
		args["dataType"] = "string"
	else:
		args.update(attributeType=kind, defaultValue=default)
		if minimum is not None:
			args["minValue"] = minimum
		if enum:
			args["enumName"] = ":".join(enum)
	cmds.addAttr(node, **args)
	if kind == "string":
		cmds.setAttr(node + "." + name, default, type="string")


def ensure_bone(joint):
	if cmds.nodeType(joint) != "joint":
		raise ValueError("Select a Maya joint")
	_add(joint, "xrayShapeType", "enum", enum=SHAPE_TYPES)
	_add(joint, "xrayJointType", "enum", enum=JOINT_TYPES)
	_add(joint, "xrayShapeFlags", "long", hidden=True)
	_add(joint, "xrayIKFlags", hidden=True)
	for name in ("NoPickable", "NoPhysics", "NoFogCollider", "RemoveAfterBreak", "Breakable"):
		_add(joint, "xray" + name, "bool")
	_add(joint, "xrayGameMaterial", "string", "default_object")
	for name, value in (("Mass", 10), ("Spring", 1), ("Damping", 1), ("Friction", 1),
						("BreakForce", 0), ("BreakTorque", 0)):
		_add(joint, "xray" + name, default=value, minimum=0)
	for axis in AXES:
		_add(joint, "xrayCenterOfMass" + axis, "doubleLinear")
		_add(joint, "xrayShapeOffset" + axis, "doubleLinear")
		_add(joint, "xrayShapeRotate" + axis, "doubleAngle")
		_add(joint, "xrayHalfSize" + axis, default=5, minimum=0)
		for suffix in ("Min", "Max"):
			_add(joint, "xrayLimit" + axis + suffix, "doubleAngle")
		for suffix in ("Spring", "Damping"):
			_add(joint, "xrayLimit" + axis + suffix, default=1, minimum=0)
	_add(joint, "xrayRadius", default=5, minimum=0)
	_add(joint, "xrayHalfHeight", default=5, minimum=0)
	_add(joint, "xraySlideMin", "doubleLinear")
	_add(joint, "xraySlideMax", "doubleLinear")
	_add(joint, "xrayBoneVersion", "long", 1, hidden=True)


def _is_helper(node):
	return (cmds.attributeQuery("xrayCollisionHelper", node=node, exists=True)
			and cmds.getAttr(node + ".xrayCollisionHelper"))


def helpers(joint):
	return [node for node in (cmds.listRelatives(joint, children=True, fullPath=True) or [])
			if _is_helper(node)]


def selected_bones():
	result = []
	for node in cmds.ls(selection=True, objectsOnly=True, long=True) or []:
		if cmds.nodeType(node) == "mesh":
			node = (cmds.listRelatives(node, parent=True, fullPath=True) or [node])[0]
		if _is_helper(node):
			node = (cmds.listRelatives(node, parent=True, fullPath=True) or [node])[0]
		if cmds.nodeType(node) == "joint" and node not in result:
			result.append(node)
	return result


def _disconnect_helper(joint, helper):
	for attr in cmds.listAttr(joint, userDefined=True) or []:
		dest = joint + "." + attr
		sources = cmds.listConnections(dest, source=True, destination=False, plugs=True) or []
		for source in sources:
			source_node = source.rsplit(".", 1)[0]
			if (cmds.ls(source_node, long=True) or [None])[0] == helper:
				value = cmds.getAttr(dest)
				cmds.disconnectAttr(source, dest)
				cmds.setAttr(dest, value)


def rebuild_shape(joint):
	with _scene_edit():
		return _rebuild_shape(joint)


def _rebuild_shape(joint):
	ensure_bone(joint)
	shape_type = cmds.getAttr(joint + ".xrayShapeType")
	if shape_type not in (0, 1, 2, 3):
		raise ValueError("Unsupported IX-Ray shape type")
	for old in helpers(joint):
		_disconnect_helper(joint, old)
		cmds.delete(old)
	if shape_type == 0:
		return None
	cm = om.MDistance(1, om.MDistance.kCentimeters).asUnits(om.MDistance.uiUnit())
	name = joint.rsplit("|", 1)[-1] + "_collision"
	if shape_type == 1:
		node = cmds.polyCube(name=name, width=2*cm, height=2*cm, depth=2*cm, constructionHistory=False)[0]
		sizes = [cmds.getAttr(joint + ".xrayHalfSize" + axis) for axis in AXES]
	elif shape_type == 2:
		node = cmds.polySphere(name=name, radius=cm, subdivisionsX=16, subdivisionsY=8, constructionHistory=False)[0]
		sizes = [cmds.getAttr(joint + ".xrayRadius")] * 3
	else:
		node = cmds.polyCylinder(name=name, radius=cm, height=2*cm, subdivisionsX=16, constructionHistory=False)[0]
		radius = cmds.getAttr(joint + ".xrayRadius")
		sizes = [radius, cmds.getAttr(joint + ".xrayHalfHeight"), radius]
	node = cmds.parent(node, joint, relative=True)[0]
	node = cmds.ls(node, long=True)[0]
	_add(node, "xrayCollisionHelper", "bool", 1, hidden=True)
	cmds.setAttr(node + ".xrayCollisionHelper", lock=True)
	for axis, size in zip(AXES, sizes):
		cmds.setAttr(node + ".translate" + axis, cmds.getAttr(joint + ".xrayShapeOffset" + axis))
		cmds.setAttr(node + ".rotate" + axis, cmds.getAttr(joint + ".xrayShapeRotate" + axis))
		cmds.setAttr(node + ".scale" + axis, size)
		cmds.connectAttr(node + ".translate" + axis, joint + ".xrayShapeOffset" + axis)
		cmds.connectAttr(node + ".rotate" + axis, joint + ".xrayShapeRotate" + axis)
	if shape_type == 1:
		for axis in AXES:
			cmds.connectAttr(node + ".scale" + axis, joint + ".xrayHalfSize" + axis)
	else:
		cmds.connectAttr(node + ".scaleX", joint + ".xrayRadius")
		cmds.connectAttr(node + ".scaleX", node + ".scaleZ")
		if shape_type == 2:
			cmds.connectAttr(node + ".scaleX", node + ".scaleY")
		else:
			cmds.connectAttr(node + ".scaleY", joint + ".xrayHalfHeight")
	for attr in ("shearXY", "shearXZ", "shearYZ", "rotateOrder", "inheritsTransform"):
		cmds.setAttr(node + "." + attr, lock=True)
	for base in ("rotatePivot", "scalePivot", "rotatePivotTranslate", "scalePivotTranslate", "rotateAxis"):
		for axis in AXES:
			cmds.setAttr(node + "." + base + axis, lock=True)
	cmds.setAttr(node + ".overrideEnabled", True)
	cmds.setAttr(node + ".overrideShading", False)
	cmds.setAttr(node + ".overrideColor", 17)
	for shape in cmds.listRelatives(node, shapes=True, fullPath=True) or []:
		for attr in ("castsShadows", "receiveShadows", "primaryVisibility", "visibleInReflections", "visibleInRefractions"):
			cmds.setAttr(shape + "." + attr, False)
	return node


def create_selected(shape_type, *_):
	joints = selected_bones()
	if not joints:
		cmds.warning("Select one or more joints (or their IX-Ray collision helpers)")
		return
	_create_for_bones(joints, shape_type)


def _create_for_bones(joints, shape_type):
	with _scene_edit(), undo_chunk():
		result = []
		for joint in joints:
			ensure_bone(joint)
			cmds.setAttr(joint + ".xrayShapeType", shape_type)
			node = rebuild_shape(joint)
			if node:
				result.append(node)
		cmds.select(result or joints, replace=True)
	_refresh_templates()


def _queue_create_selected(shape_type, *_):
	handles = [_bone_handle(joint) for joint in selected_bones()]
	if not handles:
		cmds.warning("Select one or more joints (or their IX-Ray collision helpers)")
		return

	def apply():
		joints = [path for path in (_bone_path(handle) for handle in handles) if path]
		if joints:
			_create_for_bones(joints, shape_type)

	_defer_action(apply)


def initialize_selected(*_):
	joints = selected_bones()
	if not joints:
		cmds.warning("Select a joint first")
		return
	with undo_chunk():
		for joint in joints:
			ensure_bone(joint)
	_refresh_templates()
	show_editor()


def set_visibility(visible, *_):
	with undo_chunk():
		for plug in cmds.ls("*.xrayCollisionHelper", recursive=True) or []:
			node = plug.rsplit(".", 1)[0]
			if _is_helper(node):
				cmds.setAttr(node + ".visibility", visible)


def _refresh_templates():
	global _refresh_pending
	if cmds.about(batch=True) or _refresh_pending:
		return
	_refresh_pending = True

	def refresh():
		global _refresh_pending
		_refresh_pending = False
		if _edit_depth:
			_refresh_templates()
			return
		mel.eval("refreshEditorTemplates;")
		if cmds.window(WINDOW, exists=True):
			_update_window()

	_defer_action(refresh)


def _field(joint, attr, label):
	plug = joint + "." + attr
	sources = cmds.listConnections(plug, source=True, destination=False, plugs=True) or []
	cmds.attrControlGrp(attribute=sources[0] if sources else plug, label=label)


def _section(label):
	cmds.frameLayout(label=label, collapsable=True, marginWidth=6, marginHeight=4)
	cmds.columnLayout(adjustableColumn=True)


def _end_section():
	cmds.setParent("..")
	cmds.setParent("..")


def _change_type(joint, attr, choices, value, *_):
	handle = _bone_handle(joint)
	index = choices.index(value)

	def apply():
		path = _bone_path(handle)
		if not path:
			return
		with _scene_edit(), undo_chunk():
			cmds.setAttr(path + "." + attr, index)
			if attr == "xrayShapeType":
				rebuild_shape(path)
			cmds.select(path, replace=True)

	_defer_action(apply)


def _fill(joint):
	if _edit_depth:
		cmds.text(label="Updating IX-Ray bone...")
		return
	if not cmds.attributeQuery("xrayBoneVersion", node=joint, exists=True):
		cmds.button(label="Add IX-Ray Bone Parameters", command=partial(_initialize_bone, joint))
		return
	shape_type = cmds.getAttr(joint + ".xrayShapeType")
	joint_type = cmds.getAttr(joint + ".xrayJointType")
	_section("Collision Shape")
	shape_menu = cmds.optionMenu(label="Type", changeCommand=partial(_change_type, joint, "xrayShapeType", SHAPE_TYPES))
	for label in SHAPE_TYPES:
		cmds.menuItem(label=label)
	cmds.optionMenu(shape_menu, edit=True, select=shape_type + 1)
	if shape_type:
		for axis in AXES:
			_field(joint, "xrayShapeOffset" + axis, "Position " + axis)
		if shape_type != 2:
			for axis in AXES:
				_field(joint, "xrayShapeRotate" + axis, "Rotation " + axis)
		if shape_type == 1:
			for axis in AXES:
				_field(joint, "xrayHalfSize" + axis, "Half size " + axis + " (cm)")
		else:
			_field(joint, "xrayRadius", "Radius (cm)")
			if shape_type == 3:
				_field(joint, "xrayHalfHeight", "Half height (cm)")
		cmds.button(label="Select Collision Primitive", command=lambda *_: cmds.select(helpers(joint), replace=True))
	for name, label in (("NoPickable", "No Pickable"), ("NoPhysics", "No Physics"),
						("RemoveAfterBreak", "Remove After Break"), ("NoFogCollider", "No Fog Collider")):
		_field(joint, "xray" + name, label)
	_end_section()
	_section("Physics")
	_field(joint, "xrayGameMaterial", "Game Material")
	_field(joint, "xrayMass", "Mass (kg)")
	for axis in AXES:
		_field(joint, "xrayCenterOfMass" + axis, "Center of Mass " + axis)
	_end_section()
	_section("Joint / IK (IX-Ray axes)")
	joint_menu = cmds.optionMenu(label="Type", changeCommand=partial(_change_type, joint, "xrayJointType", JOINT_TYPES))
	for label in JOINT_TYPES:
		cmds.menuItem(label=label)
	cmds.optionMenu(joint_menu, edit=True, select=joint_type + 1)
	if joint_type not in (0, 4):
		for attr in ("Spring", "Damping", "Friction"):
			_field(joint, "xray" + attr, attr)
	slots = {2: (0, 1, 2), 3: (0,), 5: (0, 1)}.get(joint_type, ())
	for slot in slots:
		axis = AXES[slot]
		slide = joint_type == 5 and slot == 0
		label = ("Slide Z" if slide else "Rotate Z") if joint_type == 5 else axis
		for bound in ("Min", "Max"):
			attr = "xraySlide" + bound if slide else "xrayLimit" + axis + bound
			_field(joint, attr, label + " " + bound)
		for attr in ("Spring", "Damping"):
			_field(joint, "xrayLimit" + axis + attr, label + " " + attr)
	_end_section()
	_section("Breakable")
	for name, label in (("Breakable", "Enabled"), ("BreakForce", "Break Force"), ("BreakTorque", "Break Torque")):
		_field(joint, "xray" + name, label)
	_end_section()


def ae_new(plug):
	parent = cmds.setParent(query=True)
	try:
		column = cmds.columnLayout("xrayBoneControls", adjustableColumn=True)
		_ae_columns.add(cmds.layout(column, query=True, fullPathName=True))
		_fill(plug.rsplit(".", 1)[0])
	finally:
		cmds.setParent(parent)


def ae_replace(plug):
	parent = cmds.setParent(query=True)
	column = parent + "|xrayBoneControls"
	if not cmds.columnLayout(column, exists=True):
		ae_new(plug)
		return
	try:
		cmds.setParent(column)
		for child in cmds.columnLayout(column, query=True, childArray=True) or []:
			cmds.deleteUI(child)
		_fill(plug.rsplit(".", 1)[0])
	finally:
		cmds.setParent(parent)


def ae_template(node):
	if cmds.nodeType(node) != "joint":
		return
	cmds.editorTemplate(forceRebuild=True)
	cmds.editorTemplate(beginLayout="IX-Ray Bone", collapse=False)
	mel.eval('editorTemplate -callCustom "ixrayBoneAENew" "ixrayBoneAEReplace" "message";')
	cmds.editorTemplate(endLayout=True)
	for attr in cmds.listAttr(node, userDefined=True) or []:
		if attr.startswith("xray"):
			cmds.editorTemplate(suppress=attr)


def _initialize_bone(joint, *_):
	with undo_chunk():
		ensure_bone(joint)
	_refresh_templates()


def _update_window(*_):
	if _edit_depth:
		_refresh_templates()
		return
	column = WINDOW + "|scroll|properties"
	if not cmds.columnLayout(column, exists=True):
		return
	for child in cmds.columnLayout(column, query=True, childArray=True) or []:
		cmds.deleteUI(child)
	parent = cmds.setParent(query=True)
	try:
		cmds.setParent(column)
		joints = selected_bones()
		if len(joints) != 1:
			cmds.text(label="Select one joint or its collision primitive")
		else:
			cmds.text(label=joints[0].rsplit("|", 1)[-1], align="left")
			_fill(joints[0])
	finally:
		cmds.setParent(parent)


def _queue_window_update(*_):
	_refresh_templates()


def show_editor(*_):
	if cmds.window(WINDOW, exists=True):
		cmds.deleteUI(WINDOW)
	cmds.window(WINDOW, title="IX-Ray Bone", widthHeight=(430, 700))
	cmds.scrollLayout("scroll", childResizable=True)
	cmds.columnLayout("properties", adjustableColumn=True)
	cmds.scriptJob(event=("SelectionChanged", _queue_window_update), parent=WINDOW)
	cmds.scriptJob(event=("Undo", _queue_window_update), parent=WINDOW)
	cmds.scriptJob(event=("Redo", _queue_window_update), parent=WINDOW)
	_update_window()
	cmds.showWindow(WINDOW)


def set_game_root(*_):
	result = cmds.fileDialog2(fileMode=3, caption="IX-Ray: select the game working directory",
							  okCaption="Select Game Root")
	if not result:
		return
	root = result[0].rstrip("\\/")
	if not root:
		return
	cmds.optionVar(sv=("ixrayGameRoot", root))
	cmds.warning("IX-Ray: game working directory saved. Restart Maya for it to take effect")


def install():
	if cmds.about(batch=True):
		return
	uninstall()
	mel.eval(r'''
global proc ixrayBoneAENew(string $plug)
{
    python("import xray_bone_collision; xray_bone_collision.ae_new(\"" + encodeString($plug) + "\")");
}
global proc ixrayBoneAEReplace(string $plug)
{
    python("import xray_bone_collision; xray_bone_collision.ae_replace(\"" + encodeString($plug) + "\")");
}
''')
	cmds.callbacks(addCallback=ae_template, hook="AETemplateCustomContent", owner=OWNER)
	main_window = mel.eval("$tmp = $gMainWindow")
	cmds.menu(MENU, label="IX-Ray", parent=main_window, tearOff=True)
	cmds.menuItem(label="Bone Collision", subMenu=True, tearOff=True)
	for index, label in enumerate(SHAPE_TYPES[1:], 1):
		cmds.menuItem(label="Create " + label, command=partial(_queue_create_selected, index))
	cmds.menuItem(divider=True)
	cmds.menuItem(label="Add Bone Parameters", command=initialize_selected)
	cmds.menuItem(label="Edit Selected", command=show_editor)
	cmds.menuItem(label="Remove from Selected Bones", command=partial(_queue_create_selected, 0))
	cmds.menuItem(divider=True)
	cmds.menuItem(label="Show All", command=partial(set_visibility, True))
	cmds.menuItem(label="Hide All", command=partial(set_visibility, False))
	cmds.menuItem(parent=MENU, divider=True)
	cmds.menuItem(parent=MENU, label="Set Game Root...", command=set_game_root)
	_refresh_templates()


def uninstall():
	global _ui_generation, _refresh_pending
	_ui_generation += 1
	_refresh_pending = False
	if cmds.about(batch=True):
		return
	cmds.callbacks(clearCallbacks=True, owner=OWNER)
	if cmds.window(WINDOW, exists=True):
		cmds.deleteUI(WINDOW)
	if cmds.menu(MENU, exists=True):
		cmds.deleteUI(MENU)
	_refresh_templates()
