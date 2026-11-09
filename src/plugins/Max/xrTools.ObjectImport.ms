rollout XRay_ImportModel_rol "x-ray *.object import v.20"
(
button LoadModelBtn "Import model" across:3 width:80 height:24 align:#left
checkbox LoadBatchChk "batch import" offset:[20,4]
checkbutton LoadSkinChk "skin" checked:false width:40 tooltip:"load skin" align:#right

label lbl_0 "scale object" align:#left across:2
spinner ScaleObject range:[0,100000,1] scale:.001 width:60 align:#right

label UseMatText "Use material" align:#left offset:[0,4] across:2
dropdownlist xrMtlType items:#("standard material","stalker material","standard + shaders")  selection:3 width:150 align:#right
label UseMatText2 "Smoothing" align:#left offset:[0,4] across:2
dropdownlist SGImportType items:#("Type #1", "Type #2") selection:1 width:150 align:#right

checkbox AttachChk "create single mesh" align:#left
checkbox LoadShapeChk "load bone collision" align:#left
checkbox MergeMaterials "use scene material" align:#left
checkbox PutToLayers "create layers" align:#left
checkbox LoadMaps "load textures" align:#left checked:true

label Lbl "textures path" align:#left
edittext TPathTxt width:200 align:#left across:2
button GetTPathBtn "get" align:#right width:24

label Lblb "objects path" align:#left
edittext ObjPathTxt width:200 align:#left across:2 --enabled:false
button GetObjPathBtn "get" align:#right width:24

/*
group "smoothing groups"
(
	checkbox LoadOrigSG "save original SG to file"
	edittext SGFilename width:200 align:#left across:2 readOnly:true
	button GetSGFilenameBtn "get" align:#right width:24
)
*/
------------------------------------------------------------------------------------------------------------
local LastPath = "C:\\Program Files\\X-Ray CoP SDK\\editors\\"

local SGa = #(), SGUsed = #{}

------------------------------------------------------------------------------------------------------------
struct cop_section 
(
	ID, Size = -1, 
	fn read f =
	(
		ID = readlong f
		Size = readlong f
	)
)

fn ReadPoint f = 
(
	local x = readfloat f
	local z = readfloat f
	local y = readfloat f
	return [x, y, z]
	--return -[x, z, y]
)


fn get_hierarchy obj =
(
	local i, res = #(obj) , chs = obj.children
	if chs.count == 0 then return #()
	for i in chs do join res (get_hierarchy i)
	return res
)


fn FinalSettings a fname =
(
	if a[1] != undefined then
		if a[1].count > 0 do
		(
			if a[1].count > 1 then
			(
					if AttachChk.checked then
					(
							a[1][1].name = (getfilenamefile fname)
							for k = 2 to a[1].count do attach a[1][1] a[1][k]
					)
					else
					(
							d = dummy name:(getfilenamefile fname) boxsize:[1,1,1]
							for obj in a[1] do obj.parent = d
							a[1][1] = d
					)
			)
			else a[1][1].name = (getfilenamefile fname)
			
			setuserprop a[1][1] "ref_name" ((substring (getfilenamepath fname) (ObjPathTxt.text.count+2) -1) + (getfilenamefile fname))
			return a[1][1]
		)
	return undefined
)

---------------------------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------------------------------------------
on GetSGFilenameBtn pressed do
(
	str = getsavefilename types:"smoothing groups|*.xsg|"
	if str!=undefined then
	(
		SGFilename.text = str
	)
)


---------------------------------------------------------------------------------------------------------------------------------------------
fn xray_check_scene_materials fname EngineMaterial CompileMaterial GameMaterial MaterialDoubleSide =
(
	local m, mat
	
	for mat in sceneMaterials do
	(
		if classof mat != multimaterial then mats = #(mat) else mats = mat
		for m in mats do
		(
			case (classof m) of
			(
				Standard:
					if (xrToolsUtil.HasShadersCA m) then
					(
						dm = m.diffusemap
						if dm!=undefined then
							if dm.filename != undefined then
								if (stricmp fname dm.filename) == 0 and m.EShader == EngineMaterial and 
									m.CShader == CompileMaterial and m.GameMaterial == GameMaterial and 
									(m.twoSided == (MaterialDoubleSide!=0)) then 
									(
										--format "material replaced %\n" fname
										return m
									)
					)
				S_T_A_L_K_E_R_Mtl:	
					(
						dm = m.diffusemap
						--format "check %, %\n" dm.filename fname
						if dm!=undefined then
							if dm.filename != undefined then
								if (stricmp fname dm.filename) == 0 and m.EShaderByName == EngineMaterial and 
									m.CShaderByName == CompileMaterial and m.GameMtlByName == GameMaterial and 
									(m.twoSided == (MaterialDoubleSide!=0)) then 
									(
										--format "material replaced %\n" fname
										return m
									)
					)
				default:	()
			)
		)
	)
	
	return undefined
)

---------------------------------------------------------------------------------------------------------------------------------------------
fn Load_single_bone f filesize LayerName =
(
		local Section = cop_section ()
		local i = 0, SectionEndOfs
		local BoneName, tm, BoneMaterial
		local BoneSettings
		
		Section.read f ; if Section.ID != 1 then return undefined --( format "\n### section 0x1 not found\n" ; return undefined )
		local UnkType = readshort f --; format "\t\t unk type = %\n" UnkType
		
		Section.read f ; if Section.ID != 2 then return undefined --( format "\n### section 0x1 not found (hierarchy)\n" ; return undefined )
		local CurBoneName = readstring f , ParentBoneName = readstring f , str = readstring f
		--format "\t\t bone:% => %\n" CurBoneName ParentBoneName
		
		Section.read f ; if Section.ID != 3 then return undefined --( format "\n### section 0x1 not found (transform)\n" ; return undefined )
		local BindPos = (ReadPoint f)*ScaleObject.value
		local BindRot = ReadPoint f
		local unkfloat = readfloat f --; format "\t\t +float = %\n" unkfloat
		local joint_type, SprFactor, DumpFactor, SpringFactor, DumpingFactor, AxisMin, AxisMax, Friction, tm
		local Breakable, BreakForce, BreakTorque
		local BoneLayerName = LayerName + "_bones", ShapeLayerName = LayerName + "_shapes"
		
		if PutToLayers.checked then
		(
			Lif = LayerManager.getLayerFromName BoneLayerName
			if Lif == undefined then 
			(
				Lif = LayerManager.newLayer()
				Lif.setname BoneLayerName
			)
		)
		
		while true do
		(
				CurPos = ftell f
				if CurPos >= filesize then 
				(
						obj = dummy name:CurBoneName boxsize:([1,1,1] * .0) pos:[0,0,0] showlinks:true wirecolor:(color 255 255 0)
						obj.rotation.controller.value = eulertoquat (eulerangles -(radtodeg BindRot.y) -(radtodeg BindRot.x) -(radtodeg BindRot.z)) order:4
						obj.position.controller.value = BindPos
						if PutToLayers.checked then	Lif.addnode obj
						obj.setSkinPose()
						
						setUserProp obj "Material" BoneMaterial
						setUserProp obj "Mass" BoneMass
						setUserProp obj "SpringFactor" SpringFactor
						setUserProp obj "DumpingFactor" SpringFactor
						setUserProp obj "joint_type" joint_type
						
						if joint_type > 0 then
								for i = 1 to 3 do
								(
										str = if i == 1 then "X" else if i == 2 then "Y" else "Z"
										setUserProp obj ("_SpringFactor_" + str) SprFactor[i]
										setUserProp obj ("_DumpingFactor_" + str) DumpFactor[i]
										setUserProp obj ("_AxisMin_" + str) AxisMin[i]
										setUserProp obj ("_AxisMax_" + str) AxisMax[i]
								)
						
						setUserProp obj "shape_type" shape_type
						setUserProp obj "BoxCenter" BoxCenter
						setUserProp obj "BoxHalfSize" BoxHalfSize
						setUserProp obj "SphereCenter" SphereCenter
						setUserProp obj "SphereRadius" SphereRadius
						setUserProp obj "CylCenter" CylCenter
						setUserProp obj "CylDir" CylDir
						setUserProp obj "CylHeight" CylHeight
						setUserProp obj "CylRadius" CylRadius
						setUserProp obj "Shape_tm" tm
						
						if XRayCollision != undefined and LoadShapeChk.checked and shape_type != 0 and shape_type != undefined then
						(
							BoxHalfSize *= ScaleObject.value
							SphereRadius *= ScaleObject.value
							CylRadius *= ScaleObject.value
							CylHeight *= ScaleObject.value
							
							b = XRayCollision 	stype:shape_type \
												width:BoxHalfSize.x length:BoxHalfSize.z height:BoxHalfSize.y \
												sradius:SphereRadius \
												cradius:CylRadius cheight:CylHeight \
												mass:BoneMass \
												name:("shape_" + obj.name) xray:true color:(color 80 80 80)
							swap tm.row1.y tm.row1.z
							swap tm.row2.y tm.row2.z
							swap tm.row3.y tm.row3.z
							swap tm.row2 tm.row3
							
						
							case shape_type of
							(
								1:
								(
									tm.row4 = [BoxCenter.x, BoxCenter.z, BoxCenter.y]*ScaleObject.value
									b.transform *= tm
									b.transform *= obj.transform
								)
								2:
								(
									tm.row4 = [SphereCenter.x, SphereCenter.z, SphereCenter.y]*ScaleObject.value
									b.transform *= tm
									b.transform *= obj.transform
								)
								3:
								(
									b.dir = normalize [CylDir.x, CylDir.z, CylDir.y]
									b.pos = [CylCenter.x, CylCenter.z, CylCenter.y]*ScaleObject.value
									b.transform *= obj.transform
								)
								default: () --format "\t\t\t - no shape\n"
							)
							
							b.parent = obj
							Lif = LayerManager.getLayerFromName ShapeLayerName
							if Lif == undefined then 
							(
								Lif = LayerManager.newLayer()
								Lif.setname ShapeLayerName
							)
							Lif.addnode b
						)
						
						return #(obj,ParentBoneName)
				)
				
				Section.read f
				if Section.ID == undefined then return undefined --( format "\n#eof\n" ; return undefined )
				SectionStartOfs = ftell f
				SectionEndOfs = SectionStartOfs + Section.Size
				--format "\tSection ID = 0x% (%) , size = % , (at %) " (bit.intashex Section.ID) Section.ID Section.Size (ftell f)
				
				
				
				case Section.ID of
				(
						0x2: -- string
						(
								BoneName = readstring f
								--format " | Bone name = %\n" BoneName 
						)
						0x4: -- material
						(
								BoneMaterial = readstring f
								--format " | Bone material = %\n" BoneMaterial 
						)
						0x5: --
						(
								shape_type = readshort f --; format "\t\tshape_type = %\n" shape_type
								shape_flags = readshort f --; format "\t\tshape_flags (%) = 0x%\n" shape_flags (bit.intashex shape_flags)
								
								tm = matrix3 1
								tm.row1 = [readfloat f, readfloat f, readfloat f]
								tm.row2 = [readfloat f, readfloat f, readfloat f]
								tm.row3 = [readfloat f, readfloat f, readfloat f]
								
								BoxCenter = [readfloat f, readfloat f, readfloat f]
								BoxHalfSize = [readfloat f, readfloat f, readfloat f]
								
								SphereCenter = [readfloat f, readfloat f, readfloat f]
								SphereRadius = readfloat f
								
								CylCenter = [readfloat f, readfloat f, readfloat f]
								CylDir = [readfloat f, readfloat f, readfloat f]
								CylHeight = readfloat f
								CylRadius = readfloat f
								
						)
						0x8: --
						(
								Breakable = readlong f
						)
						0x9: --
						(
								BreakForce = readfloat f
								BreakTorque = readfloat f
						)
						0x10: --
						(
								Friction = readfloat f
						)
						0x6: --
						(
								joint_type = readlong f --; format "\t\tjoint_type = %\n" joint_type
								
								AxisMin = [0,0,0]; AxisMax = [0,0,0]; SprFactor = [0,0,0]; DumpFactor = [0,0,0]
								
								for i = 1 to 3 do
								(
										AxisMin[i] = radtodeg (readfloat f)
										AxisMax[i] = radtodeg (readfloat f)
										SprFactor[i] = readfloat f
										DumpFactor[i] = readfloat f
								)
								
								SpringFactor = readfloat f
								DumpingFactor = readfloat f
								
								--fseek f SectionEndOfs #seek_set
						)
						0x7: -- Mass
						(
								BoneMass = readfloat f
								BoneCenterMass = (ReadPoint f)*ScaleObject.value
								
								--fseek f SectionEndOfs #seek_set
						)
					default:
						(
								--format "\tSection ID = 0x% (%) , size = % , (at %) " (bit.intashex Section.ID) Section.ID Section.Size SectionStartOfs 
								--format " | ......... skiped\n"
								fseek f Section.Size #seek_cur
						)
				) -- case
		)
		
) -- fn



------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

fn load_object f fname filesize LoadMesh LoadSkin = 
(
		sectionId = readlong f ; if sectionId != 0x7777 then ( format "\n### Unsupported file version\n" ; return #() )
		SectionSize = readlong f #unsigned
		--format "section [id = 0x%, size=%] at 0\t\t| EOBJ_CHUNK_MAIN\n" (bit.intashex sectionId) SectionSize
		
		local vmas = #(), fmas = #(), tvmas = #(), tfmas = #(), BugReport = #()
		local TexPath = TPathTxt.text
		local SectionEndOfs
		local Result = #() -- [1]: meshes | [2]: materials | [3]: bone array of #(bobj,parent_name) |
		local TexTabID1, TexTabID2
		local EOBJ_CHUNK_FLAGS, EOBJ_CHUNK_VERSION, EOBJ_CHUNK_USERDATA, EOBJ_CHUNK_LOD_REF
		local SkinAvailable = false
		local VTable, ITable, BoneNamesList, XTable = #()
	
		fn compareSG a b = 
		(
			local an = a.numberset, bn = b.numberset
			if an == bn then return 0
			if an > bn then return 1
			return -1
		)
		
		while true do
		(
				CurPos = ftell f
				if CurPos >= filesize then exit --( format "end reached at %\n" CurPos ; exit )
				sectionId = readlong f ; if sectionId == undefined then exit --( format "\n# eof\n" ; exit )
				SectionSize = readlong f #unsigned
				StartPos = ftell f
				SectionEndOfs = StartPos + SectionSize
				if SectionEndOfs > filesize then exit --(format "\n### NOT EXPECTED END\n" ; exit )
				
				--format "section [id = 0x%, size=%] at %\t\t|" (bit.intashex sectionId) SectionSize (StartPos-8)
				
				case sectionId of
				(
					0x7777:
						(
								--format " EOBJ_CHUNK_MAIN\n"
						)
					0x0900:
						(
								EOBJ_CHUNK_VERSION = readshort f --format " EOBJ_CHUNK_VERSION (16 supported) = %\n" EOBJ_CHUNK_VERSION
								if EOBJ_CHUNK_VERSION != 16 then return #() --( format "\n### Unsupported file version\n" ; return #() )
						)
					0x0912:
						(
								EOBJ_CHUNK_USERDATA = readstring f
								--format "EOBJ_CHUNK_USERDATA = %\n" EOBJ_CHUNK_USERDATA
						)
					0x0925:
						(
								EOBJ_CHUNK_LOD_REF = readstring f
								--format "EOBJ_CHUNK_LOD_REF = %\n" (trimright EOBJ_CHUNK_LOD_REF)
								fseek f SectionEndOfs #seek_set
						)
					0x0903:
						(
								EOBJ_CHUNK_FLAGS = readlong f
								--format "EOBJ_CHUNK_FLAGS = %\n" EOBJ_CHUNK_FLAGS
								if (bit.and EOBJ_CHUNK_FLAGS 1) > 0 then SkinAvailable = true
						)
					0x0910:
						(
								--if SectionSize > 0 then format " EOBJ_CHUNK_MESHES\n"
								MeshesEndOfs = SectionSize + (ftell f)
								Result[1] = #()
								Result[2] = #()
								if not LoadMesh then fseek f SectionSize #seek_cur
								
								while true do
								(
										if (ftell f) >= MeshesEndOfs then exit --( format "\n.......meshes loaded\n\n" ; exit)
										MeshIndex = readlong f --; format "\nLOADING MESH %.......\n" MeshIndex
										CurMeshSize = readlong f
										CurMeshSize += ftell f
										
										
										while true do
										(
												CurPos = ftell f
												if CurPos >= CurMeshSize then exit
												sectionId = readlong f ; if sectionId == undefined then break()
												SectionSize = readlong f #unsigned
												StartPos = ftell f
												SectionEndOfs = StartPos + SectionSize
												if SectionEndOfs > CurMeshSize then (exit) --break()
												
												--format "section [id = % (0x%), size=%] at %\t\t|" sectionId (bit.intashex sectionId) SectionSize (StartPos-8)
												
												case sectionId of
												(
													0x1000:
														(
																EMESH_CHUNK_VERSION = readshort f
																--format "\tEMESH_CHUNK_VERSION = %\n" EMESH_CHUNK_VERSION
														)
													0x1001:
														(
																EMESH_CHUNK_MESHNAME = readstring f
																--format "\tEMESH_CHUNK_MESHNAME = %\n" EMESH_CHUNK_MESHNAME
														)
													0x1004:
														(
																b = for i = 1 to 6 collect readfloat f --format " EMESH_CHUNK_BBOX %\n" b
														)
													0x1002:
														(
																EMESH_CHUNK_FLAGS = readbyte f --format " EMESH_CHUNK_FLAGS = %\n" EMESH_CHUNK_FLAGS
														)
													--0x1010:
													--	(
													--			EMESH_CHUNK_OPTIONS = readstring f --; format "EMESH_CHUNK_OPTIONS = %\n" EMESH_CHUNK_OPTIONS
													--			fseek f (SectionSize + StartPos) #seek_set
													--	)
													0x1005:
														(
																NumVerts = readlong f #unsigned --; format " EMESH_CHUNK_VERTS [%]" NumVerts
																vmas = for i = 1 to NumVerts collect ((ReadPoint f)*ScaleObject.value)
														)
													0x1006:
														(
																NumFaces = readlong f #unsigned --; format " EMESH_CHUNK_FACES [%]" NumFaces 
																fmas = #(); tfmas = #()
																
																for i = 1 to NumFaces do
																(
																		v1 = readlong f
																		tv1 = readlong f
																		v2 = readlong f
																		tv2 = readlong f
																		v3 = readlong f
																		tv3 = readlong f
																		append fmas [v1+1,v3+1,v2+1]
																		append tfmas [tv1+1,tv3+1,tv2+1]
																	
																		--format "%, %\n" i [v1+1,v3+1,v2+1]
																)
																
																
																m = mesh vertices:vmas faces:fmas
																m.name = EMESH_CHUNK_MESHNAME
																Result[1][MeshIndex+1] = m
																setuserprop m "user_data" EOBJ_CHUNK_USERDATA
																if PutToLayers.checked then
																(
																	Lif = LayerManager.getLayerFromName ((getfilenamefile fname) + "_mesh")
																	if Lif == undefined then 
																	(
																		Lif = LayerManager.newLayer()
																		Lif.setname ((getfilenamefile fname) + "_mesh")
																	)
																	if PutToLayers.checked then	Lif.addnode m
																)
														)
													0x1013:
														(
																--format " EMESH_CHUNK_SG\n"
															
															if SGImportType.selection == 1 then
															(
																local SGFaces = #(#()), var = #{}
																
																for i = 1 to NumFaces do
																(
																	v = (readlong f) + 3
																	if SGFaces[1][v] == undefined then SGFaces[1][v] = #{}
																	append SGFaces[1][v] i
																)
																
																-- remove undefined
																--for i = 1 to SGFaces[1].count do format "%, %\n" i SGFaces[1][i]
																for i = SGFaces[1].count to 1 by -1 do if SGFaces[1][i] == undefined then deleteitem SGFaces[1] i
																	
																-- sort by face count
																qsort SGFaces[1] compareSG
																
																for i = 1 to 32 where SGFaces[i]!=undefined and SGFaces[i].count>1 do
																(
																	var = meshop.getVertsUsingFace m SGFaces[i][1]
																	--format "i = %\n" i
																	
																	for j = 2 to SGFaces[i].count do
																	(
																		v = meshop.getVertsUsingFace m SGFaces[i][j]
																		if (var*v).numberset == 0 then
																		(
																			join var v
																			join SGFaces[i][1] SGFaces[i][j]
																			--format "\tjoin %\n" j
																		)
																		else
																		(
																			k = i+1
																			if SGFaces[k] == undefined then SGFaces[k] = #()
																			append SGFaces[k] SGFaces[i][j]
																			--format "remove sg from % to %, %\n" i k SGFaces[i][j]
																		)
																	)
																)
																
																for i = 1 to 33 where SGFaces[i] != undefined and SGFaces[i].count > 0 do
																(
																	if i == 33 then
																	(
																		for j in SGFaces[i] do
																		(
																			for k in j do
																				setFaceSmoothGroup m k (2^(i-1))
																			setfaceselection m  j keep:true
																		)
																		
																		setuserprop m "badsg" SGFaces[i]
																		if SGFaces[i].count > 1 then format "some smoothing groups are not correct\n"
																	)
																	else
																	(
																		--for j = SGFaces[i].count to 2 by -1 do deleteitem SGFaces[i][j]
																		for j in SGFaces[i][1] do setFaceSmoothGroup m j (2^(i-1))
																	)
																)
															)
															else
																if true then
																(
																	sig = #([3,1],[2,3],[1,2])
																	ste = #(1,2,4)
																	SGa = for i = 1 to NumFaces collect readlong f #unsigned
																	
																	local SGUsed = #{}
																	local SGroup = 0
																	local SGStack = #(), SGStackPos = 0
																	local SGFaces = #(#()), var = #{}
																	
																	for i = 1 to m.numfaces where not SGUsed[i] do
																	(
																		SGroup += 1
																		SGStackPos += 1; SGStack[SGStackPos] = i
																		SGFaces[1][SGroup] = #{}
																		
																		while true do
																		(
																			FaceInd = SGStack[SGStackPos]; SGStackPos -= 1
																			SGUsed[FaceInd] = true
																			--setFaceSmoothGroup m FaceInd (2^SGroup)
																			append SGFaces[1][SGroup] FaceInd
																			
																			for i = 1 to 3 where bit.and SGa[FaceInd] (2^(3-i)) == 0 do
																			(
																				ei = (FaceInd - 1)*3 + i
																				ope = meshop.getEdgesReverseEdge m ei
																				
																				for j in ope do 
																				(
																					k = (j-1)/3 + 1

																					if not SGUsed[k] then
																					(
																						SGStackPos += 1
																						SGStack[SGStackPos] = k
																						SGUsed[k] = true
																					)
																				)
																			)
																			
																			if SGStackPos < 1 then exit
																		)
																		
																	)
																	
																	--format "file sg groups count = %\n" SGroup
																	
																	-- remove undefined
																	--for i = 1 to SGFaces[1].count do format "%, %\n" i SGFaces[1][i]
																	for i = SGFaces[1].count to 1 by -1 do if SGFaces[1][i] == undefined then deleteitem SGFaces[1] i
																		
																	-- sort by face count
																	qsort SGFaces[1] compareSG
																	
																	for i = 1 to 32 where SGFaces[i]!=undefined and SGFaces[i].count>1 do
																	(
																		var = meshop.getVertsUsingFace m SGFaces[i][1]
																		--format "i = %\n" i
																		
																		for j = 2 to SGFaces[i].count do
																		(
																			v = meshop.getVertsUsingFace m SGFaces[i][j]
																			if (var*v).numberset == 0 then
																			(
																				join var v
																				join SGFaces[i][1] SGFaces[i][j]
																				--format "\tjoin %\n" j
																			)
																			else
																			(
																				k = i+1
																				if SGFaces[k] == undefined then SGFaces[k] = #()
																				append SGFaces[k] SGFaces[i][j]
																				--format "remove sg from % to %, %\n" i k SGFaces[i][j]
																			)
																		)
																	)
																	
																	for i = 1 to 33 where SGFaces[i] != undefined and SGFaces[i].count > 0 do
																	(
																		if i == 33 then
																		(
																			for j in SGFaces[i] do
																			(
																				for k in j do
																					setFaceSmoothGroup m k (2^(i-1))
																				setfaceselection m  j keep:true
																			)
																			
																			setuserprop m "badsg" SGFaces[i]
																			if SGFaces[i].count > 1 then format "some smoothing groups are not correct\n"
																		)
																		else
																			for j in SGFaces[i][1] do setFaceSmoothGroup m j (2^(i-1))
																	)
																)
																else
																(
																	sig = #([3,1],[2,3],[1,2])
																	ste = #(1,2,4)
																	SGa = for i = 1 to NumFaces collect readlong f #unsigned
																	
																	local SGUsed = #{}
																	local SGroup = 0, cnt = 0
																	local SGStack = #(), SGStackPos = 0
																	
																	for i = 1 to m.numfaces where not SGUsed[i] do
																	(
																		SGStackPos += 1; SGStack[SGStackPos] = i
																		
																		while true do
																		(
																			FaceInd = SGStack[SGStackPos]; SGStackPos -= 1
																			SGUsed[FaceInd] = true
																			setFaceSmoothGroup m FaceInd (2^SGroup)
																			
																			for i = 1 to 3 where bit.and SGa[FaceInd] (2^(3-i)) == 0 do
																			(
																				ei = (FaceInd - 1)*3 + i
																				ope = meshop.getEdgesReverseEdge m ei
																				
																				for j in ope do 
																				(
																					k = (j-1)/3 + 1

																					if not SGUsed[k] then
																					(
																						SGStackPos += 1
																						SGStack[SGStackPos] = k
																						SGUsed[k] = true
																					)
																				)
																			)
																			
																			if SGStackPos < 1 then exit
																		)
																		
																		SGroup += 1; if SGroup > 31 then SGroup = 0
																		cnt += 1
																	)
																	
																	--format "sg groups count = %\n" cnt
																)
																
															update m
														)
													0x1008:
														(
																if LoadMaps.checked then
																(
																	NumVMREFS = readlong f #unsigned --format " EMESH_CHUNK_VMREFS [%]\n" NumVMREFS
																	
																	for i = 1 to NumVMREFS do
																	(
																			Cnt = readbyte f
																			XTable[i] = #()
																			for j = 1 to Cnt do 
																			(
																				v1 = readlong f #unsigned
																				v2 = readlong f #unsigned
																				if v1 > 88 then (v1 = 0 ; v2 = 0)
																				XTable[i][j] = #(v1, v2)
																			)
																	)
																)
																fseek f (SectionSize + StartPos) #seek_set
														)
													0x1009:
														(
															if LoadMaps.checked then
															(
																NumSurfaces = readshort f --; format " EMESH_CHUNK_SFACE [%]\n" NumSurfaces
																Result[2][MeshIndex+1] = #()
																
																for i = 1 to NumSurfaces do
																(
																		UnkStr1 = readstring f --; format "\tMat name = %\n" UnkStr1 
																		append Result[2][MeshIndex+1] UnkStr1
																		SNumFaces = readlong f #unsigned --; format "\tnum faces = %\n" SNumFaces
																		for j = 1 to SNumFaces do setFaceMatID m ((readlong f #unsigned) + 1) i
																)
															)
															
															fseek f (SectionSize + StartPos) #seek_set
														)
													0x1012:
														(
															if LoadMaps.checked then
															(
																NumTextureTables = readlong f --; format " EMESH_CHUNK_VMAPS_2 [%]\n" NumTextureTables 
														
																VTable = #() -- vert values table
																ITable = #() -- indexes value tables
																BoneNamesList = #() -- bone names table list
																TabIndex = 1
																
																for i = 1 to NumTextureTables do
																(
																		UnkStr2 = readstring f --; format "\n\t\t(%)Table name = %\n" (i-1) UnkStr2
																		TableType = readbyte f --; format "\t\tTable Type = %\n" TableType 
																		TableID = readshort f --; format "\t\tTableID = %\n" TableID
																		NumTVerts = readlong f --; format "\t\tNumTVerts = %\n" NumTVerts
																		VTable[i] = #()
																		
																		case TableType of
																		(
																			2: -- Texture
																				(
																					for j = 1 to NumTVerts do
																					(
																							u = readfloat f ; v = readfloat f
																							append VTable[i] [ u , 1 - v , 0]
																					)
																					
																					ITable[i] = for j = 1 to NumTVerts collect (readlong f #unsigned)
																					
																					if TableID == 1 then (fseek f (NumTVerts*4) #seek_cur ; TexTabID2 = i)
																					else TexTabID1 = i
																				)
																				
																			1: -- bone
																				(
																						BoneNamesList[i] = UnkStr2
																						
																						for j = 1 to NumTVerts*2 do
																						(
																								u = readfloat f
																								append VTable[i] u
																						)
																				)
																				
																			default:( break() )
																		) -- case TableType 
																)
																
																
																meshop.setNumMaps m 1
																meshop.setMapSupport m 1 true
																meshop.defaultMapFaces m 1
																meshop.setNumMapVerts m 1 NumVMREFS
																
																for i = 1 to NumFaces do meshop.setmapface m 1 i tfmas[i]
																
																for i = 1 to NumVMREFS do
																(
																		j = XTable[i][1][1] + 1
																		k = XTable[i][1][2] + 1
																		meshop.setMapVert m 1 i VTable[j][k]
																)
																
															)																
															fseek f (SectionSize + StartPos) #seek_set
														)
													default:
														(
																fseek f SectionSize #seek_cur --format ".... skiped\n"
														)
												) -- case mesh
												
										) -- while loading mesh
										
										fseek f CurMeshSize #seek_set

										--format "...MESH % LOADED\n" MeshIndex 
								)
						) ------

					0x0907:
						(
							if LoadMaps.checked then
							(
								NUM_MATERIALS = readlong f --; format "\tNUM_MATERIALS = %\n" NUM_MATERIALS
								
								if LoadMesh then
								(
										local MatList = #()
										local MatNameList = #()
										
										for i = 1 to NUM_MATERIALS do
										(
												MaterialName = readstring f --; format "\t\tmaterial %: %\n" (i-1) MaterialName
												MatNameList[i] = MaterialName
												EngineMaterial = readstring f --; format "\t\t\tEngineMaterial: %\n" EngineMaterial
												CompileMaterial = readstring f --; format "\t\t\tCompileMaterial: %\n" CompileMaterial
												GameMaterial = readstring f --; format "\t\t\tGameMaterial: %\n" GameMaterial
												FileNameMaterial = readstring f --; format "\t\t\tFileNameMaterial: %\n" FileNameMaterial
												unkMatStr = readstring f --; format "\t\t\tunkMatStr: %\n" unkMatStr
												MaterialDoubleSide = readlong f --; format "\t\t\tDouble side : %\n" MaterialDoubleSide 
												MaterialUnkLong1 = readlong f --; format "\t\t\tMaterialUnkLong1 : 0x%\n" (bit.intashex MaterialUnkLong1)
												MaterialUnkLong2 = readlong f --; format "\t\t\tMaterialUnkLong2 : 0x%\n" (bit.intashex MaterialUnkLong2)
												
												chema = if not MergeMaterials.checked then undefined 
														else xray_check_scene_materials (TexPath + "\\" + FileNameMaterial + ".dds") EngineMaterial CompileMaterial GameMaterial MaterialDoubleSide
												
												if chema == undefined then
												(
													if 	(EngineMaterial == "def_shaders\\def_trans_v") or 
														(EngineMaterial == "def_shaders\\def_aref") or
														(EngineMaterial == "def_shaders\\def_aref_v") or
														(EngineMaterial == "def_shaders\\def_trans") or
														(EngineMaterial == "details\\blend") or
														(EngineMaterial == "def_shaders\\def_trans_v_tuchi") or
														(EngineMaterial == "models\\model_fur") or
														(EngineMaterial == "flora\\leaf_wave") then isOpacity = true
													else isOpacity = false
													
													if (xrMtlType.selection == 2) and (S_T_A_L_K_E_R_Mtl != undefined) then
													(
															MatList[i] = S_T_A_L_K_E_R_Mtl name:MaterialName
															MatList[i].EShaderByName = EngineMaterial
															MatList[i].CShaderByName = CompileMaterial
															MatList[i].GameMtlByName = GameMaterial
													)
													else
														if (xrMtlType.selection == 3) and (xrShadersDataCA != undefined) then
														(
																MatList[i] = standard name:MaterialName
																custAttributes.add MatList[i] xrShadersDataCA
																MatList[i].EShader = EngineMaterial
																MatList[i].CShader = CompileMaterial
																MatList[i].GameMaterial = GameMaterial
														)
														else
																MatList[i] = standard name:MaterialName
															
													if MaterialDoubleSide != 0 then MatList[i].twoSided = true
													MatList[i].DiffuseMap = bitmaptexture filename:(TexPath + "\\" + FileNameMaterial + ".dds")
													
													MatList[i].DiffuseMap.monoOutput = 0
													MatList[i].DiffuseMap.RGBOutput = 0
													MatList[i].DiffuseMap.alphasource = 2
													
													if isOpacity then
													(
															MatList[i].opacityMap = copy MatList[i].DiffuseMap
															MatList[i].opacityMap.monoOutput = 1
															MatList[i].opacityMap.RGBOutput = 1
															MatList[i].opacityMap.alphasource = 0
													)
												
													showTextureMap MatList[i] true
												)
												else MatList[i] = chema
										)
										
										for i = 1 to Result[1].count where Result[1][i]!=undefined do 
										(
												m = Result[1][i]
												names = Result[2][i]
												CurMat = multimaterial numsubs:names.count --name:(filenamefrompath fname)
												
												for j = 1 to names.count do
												(
														ind = findItem MatNameList names[j]
														if ind == 0 then () --format "############################################## cant find material %\n" names[j]
														else
														(
																CurMat[j] = MatList[ind]
														)
												)
												
												m.material = CurMat
										)
								)
							)
							fseek f (SectionSize + StartPos) #seek_set
						)
					0x921:
						(
								--format "\tBONES\n"
								if LoadSkin AND SkinAvailable then
								(
										local Section = cop_section ()
										Result[3] = #()
										i = 0
										
										while true do
										(
												if (ftell f) >= SectionEndOfs then exit
												Section.read f
												if Section.ID > 200 then ( format "+++ list\n" ; exit )
		
												i += 1
												BoneObject = Load_single_bone f (Section.size + (ftell f))  (getfilenamefile fname)
												Append Result[3] BoneObject
										)
										
										for bo in Result[3] do
										(
											pb = undefined
											for j in Result[3] do if bo[2] == j[1].name then (pb = j[1] ; exit)
											tm = if pb == undefined then matrix3 1 else pb.transform
											bo[1].parent = pb
											bo[1].transform *= tm
										)
								)
								
								fseek f (SectionSize + StartPos) #seek_set
						)
						
					default:
						(
								fseek f SectionSize #seek_cur --format ".... skiped\n"
						)
				) -- case
		) -- while
		
		

		
		
		if LoadSkin AND SkinAvailable then
				(
					smod = skin bone_Limit:10 --initialStaticEnvelope:true initialEnvelopeInner:0 initialEnvelopeOuter:0
					obj = result[1][1]
					
					max modify mode
					select obj
					addmodifier obj smod

					for ii = 1 to result[3].count do
					(
						tmp = skinOps.addbone smod result[3][ii][1] 0
						/*
						for jj = 1 to 2 do 
						(
							skinOps.SetInnerRadius smod ii jj 0
							skinOps.SetOuterRadius smod ii jj 0
						)
						*/
						--format "%, %, %\n" ii result[3][ii][1] tmp
					)
					
					for ii = 1 to result[3].count do
					(
						for jj = 1 to 2 do
						(
							skinOps.SetInnerRadius smod ii jj 0
							skinOps.SetOuterRadius smod ii jj 0
						)
					)
					
					
					SkinData = #(#(),#())
					
					for a in XTable do
					(
						isGood = true
						Bonar = #()
						War = #()
						skin_vid = -1
						skin_tid = -1
						
						for pa in a do
						(
							if pa[1] == TexTabID1-1 then (skin_tid = TexTabID1 ; skin_vid = pa[2] + 1 ; continue)
							if pa[1] == TexTabID2-1 then (skin_tid = TexTabID2 ; skin_vid = pa[2] + 1 ; continue)
							tid = pa[1] + 1
							iid = pa[2] + 1 
							
							if BoneNamesList[tid] != undefined then
							(
								w = VTable[tid][iid]
								bid = 1
								--for k = 1 to result[3].count do if (stricmp BoneNamesList[tid] result[3][k][1].name) == 0 then (bid = k ; exit)
								for k = 1 to result[3].count do if (stricmp BoneNamesList[tid] (skinOps.GetBoneName smod k 1)) == 0 then (bid = k ; exit)
								append Bonar Bid
								append War w
							)
						)
						
						if isGood then 
						(
							skin_index = ITable[skin_tid][skin_vid] + 1
							if SkinData[1][skin_index] == undefined then
							(
								SkinData[1][skin_index] = Bonar
								SkinData[2][skin_index] = War
							)
						)
					)
					
					for i = 1 to $.numverts do
					(
						--skinOps.SelectVertices smod i
						--skinOps.bakeSelectedVerts smod
						skinOps.setVertexWeights smod i SkinData[1][i] SkinData[2][i]
						--format "%, %, %\n" i SkinData[1][i] SkinData[2][i]
					)
					
					--gc()
				)
		
		--gc()
		return Result
		
) -- fn


----------------------------------------------------------------------------------------------------------------------------------------------------------

on GetTPathBtn pressed do
(
		p = getsavepath caption:"Select textures path"
		if p != undefined then
		(
				TPathTxt.text = p
				fname = scriptspath + "xr_scripts_settings.txt"
				setIniSetting fname "paths" "last_textures_path" p
		)
)

on GetObjPathBtn pressed do
(
		p = getsavepath caption:"Select X-Ray SDK objects path"
		if p != undefined then
		(
				ObjPathTxt.text = p
				fname = scriptspath + "xr_scripts_settings.txt"
				setIniSetting fname "paths" "last_objects_path" p
		)
)

----------------------------------------------------------------------------------------------------------------------------------------------------------
on XRay_ImportModel_rol open do
(
	fname = scriptspath + "xr_scripts_settings.txt"
	TPathTxt.text = getIniSetting fname "paths" "last_textures_path"
	ObjPathTxt.text = getIniSetting fname "paths" "last_objects_path"
	LastPath = ObjPathTxt.text
	LoadShapeChk.enabled = ( XRayCollision != undefined)
)
----------------------------------------------------------------------------------------------------------------------------------------------------------
on LoadModelBtn pressed do
(
		if not LoadBatchChk.checked then
		(
				fname = getopenfilename filename:LastPath \
										types:"x-ray object|*.object|"
				
				if fname != undefined then
				(
						f = fopen fname "rb"
						if f == undefined then format "cant open file\n"
						else
						(
								LastPath = getFileNamePath fname
								clearlistener ()
								ts = timestamp ()
								filesize = getfilesize fname
								format "start loading % |size = %\n\n" fname filesize
								undo off
								(
									with redraw off
									(
										--try (a = load_object f fname filesize true LoadSkinChk.checked)
										--catch ( a = #() ; format "### error: %\n\n" (getcurrentexception()) )
										a = load_object f fname filesize true LoadSkinChk.checked
									)
								)

								if not LoadSkinChk.checked then FinalSettings a fname
								fclose f
								--gc()
								format "done at % sec\n" ((timestamp() - ts)/1000.)
						)
				)
		)
		else
		(
				fdir = getSavePath initialDir:(LastPath)
				
				if fdir != undefined then
				(
						LastPath = fdir
						
						xrImportFilesPath = fdir
						xrImportFilesList = #()
						filein (scriptspath + "xray_tools\\xrTools.FilesSelector.ms")
						
						files = xrImportFilesList --getfiles (fdir + "\\*.object")
						ts = timestamp ()
						CurObjectIndex = 1
						clearlistener ()
						
						undo off
						(
							with redraw off
							(
								for fname in files do
								(
										f = fopen fname "rb"
										if f == undefined then format "\n### cant open file %\n\n" fname
										else
										(
												filesize = getfilesize fname
												format "% |size = %\n" fname filesize 
												pushPrompt ("loading " + (CurObjectIndex as string) + " of " + (files.count as string))
												--try (a = load_object f fname filesize true LoadSkinChk.checked)
												--catch ( a = #() ; format "### error: %\n\n" (getcurrentexception()) )
												a = load_object f fname filesize true LoadSkinChk.checked
												if not LoadSkinChk.checked then FinalSettings a fname
												fclose f
										)
										
										CurObjectIndex += 1
								)
							)
						)
						
						--gc()
						format "done at % sec\n" ((timestamp() - ts)/1000.)
				)
		)
)



-------------------------------------------------------------------------------------------------------------------------------------------------------------------

) -- roll

try (closeRolloutFloater XRay_ImportModel_flo) catch ()
XRay_ImportModel_flo = newRolloutFloater "x-ray tools" 270 350 40 130
addRollout  XRay_ImportModel_rol XRay_ImportModel_flo
--addRollout  XRay_Skeleton_rol XRay_Skeleton_flo



