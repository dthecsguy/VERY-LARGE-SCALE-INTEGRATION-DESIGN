;-------------------------------------------------
; Thu Jan 19 08:54:29 2023
; Custom Compiler Version: T-2022.06-2 
; Library Name: lab1 
; Export Condition: Local 
;-------------------------------------------------
controls(
;techArray(
;techArray( techReferenceNames )
;--------------------------------------
;) ;techArray

techParams(
;( parameter           value )
;( ----------             ----- )
( maskGrid          0.005 )
( MPPS              (list ( list nil ( quote cwW ) 0.09 ( quote cwL ) 0.09 ( quote cwSp ) 0.10 ( quote cwRowSp ) 0.15 ( quote Cw ) "CO" ( quote M1 ) "M1" ( quote M1CwEnc ) 0.005 ( quote breakLayer ) "PWELL" ) ( list nil ( quote name ) "NWELL" ( quote pathLayer ) "DIFF" ( quote pathWidth ) 0.09 ( quote pathCwEnc ) 0.05 ( quote enclosureLayers ) ( list ) ( quote spacings ) ( list nil ) ) ( list nil ( quote name ) "sub" ( quote pathLayer ) "DIFF" ( quote pathWidth ) 0.09 ( quote pathCwEnc ) 0.05 ( quote enclosureLayers ) ( list ) ( quote spacings ) ( list nil ) ) ) )
( scale             1 )
( mfgGrid           0.005 )
( drcGrid           0.005 )
( cadGrid           0.005 )
);techParams

;techPermissions(
;( class                (read-only users)  (read & write users) )
;( ----------          -----------------     --------------------   )
;) ;techPermissions

viewTypeUnits(
;( viewType                userUnit        DBUPerUU )
;( --------                     --------           -- --------   )
  ( maskLayout		 micron 	1000	)
  ( schematic		 inch 	160	)
  ( schematicSymbol	 inch 	160	)
  ( netlist		 inch 	160	)
) ;viewTypeUnits

  mfgGridResolution(
    ( 0.005 )
  ) ;mfgGridResolution

;colors(
;( colorName  colorName ...... colorName )
;) ;colors

leMPPControls (
; leMPPDefinition ( name           objList               spacings          masterIndex        [(exposedParameters)]) 
;                            ( -----             -------                ---------            ------------           ---------------------) 
leMPPDefinition ( psubGR	( ptypeGuardRing )      ( 1 ) 	0 )

;leMPPRingObject (
;		name  masterPathName
;		enclosedPathNames
;		offsetPathNames
;		subRectangleNames
;		encShapeNames
;		netName
;		[exposedParameters]
;		-------------------------- )
leMPPRingObject (
		ptypeGuardRing defaultPath
		( diffEncPath pplusEncPath )
		nil 
		( defaultContacts )
		nil 
		vss
		userParams( name netName )
);leMppRingObject

; leMPPDefinition ( name           objList               spacings          masterIndex        [(exposedParameters)]) 
;                            ( -----             -------                ---------            ------------           ---------------------) 
leMPPDefinition ( nwellGR	( ntypeGuardRing )      ( 1 ) 	0 )

;leMPPRingObject (
;		name  masterPathName
;		enclosedPathNames
;		offsetPathNames
;		subRectangleNames
;		encShapeNames
;		netName
;		[exposedParameters]
;		-------------------------- )
leMPPRingObject (
		ntypeGuardRing defaultPath
		( diffEncPath nplusEncPath nwellEncPath )
		nil 
		( defaultContacts )
		nil 
		vdd
		userParams( name netName )
);leMppRingObject

; leMPPMasterPath (
;		name
;		layer   purpose
;		width   pathStyle  conn   chop
;		[exposedParameters]
;		----------------------- )
leMPPMasterPath (
		defaultPath
		M1   drawing
		0.14   extend t t
		userParams( name width layer purpose conn chop )
);leMPPMasterPath

; leMPPEnclosedPath (
;		name
;		layer   purpose
;		enclosure   pathStyle  conn   chop
;		[exposedParameters]
;		[-eolEnclosure <integer | float>]
;		[-beginOffset float]
;		[-endOffset float]
;		----------------------- )
leMPPEnclosedPath (
		diffEncPath
		DIFF   drawing
		0.035  extend t nil
		userParams( name enclosure layer purpose conn chop )
);leMPPEnclosedPath

; leMPPEnclosedPath (
;		name
;		layer   purpose
;		enclosure   pathStyle  conn   chop
;		[exposedParameters]
;		[-eolEnclosure <integer | float>]
;		[-beginOffset float]
;		[-endOffset float]
;		----------------------- )
leMPPEnclosedPath (
		pplusEncPath
		PIMP   drawing
		0.175  extend t nil
		userParams( name enclosure layer purpose conn chop )
);leMPPEnclosedPath

; leMPPEnclosedPath (
;		name
;		layer   purpose
;		enclosure   pathStyle  conn   chop
;		[exposedParameters]
;		[-eolEnclosure <integer | float>]
;		[-beginOffset float]
;		[-endOffset float]
;		----------------------- )
leMPPEnclosedPath (
		nplusEncPath
		NIMP   drawing
		0.175  extend t nil
		userParams( name enclosure layer purpose conn chop )
);leMPPEnclosedPath

; leMPPEnclosedPath (
;		name
;		layer   purpose
;		enclosure   pathStyle  conn   chop
;		[exposedParameters]
;		[-eolEnclosure <integer | float>]
;		[-beginOffset float]
;		[-endOffset float]
;		----------------------- )
leMPPEnclosedPath (
		nwellEncPath
		NWELL   drawing
		0.28  extend t nil
		userParams( name enclosure layer purpose conn chop )
);leMPPEnclosedPath

; leMPPOffsetPath (
;		name
;		layer   purpose
;		width   offset  pathStyle  conn   chop
;		[exposedParameters]
;		[-beginOffset float]
;		[-endOffset float]
;		----------------------- )
; leMPPSubRect(
;		name
;		layer   purpose
;		width  height  conn   chop  enclosure  endOfLine   spacing   spacingType  numRows
;		[exposedParameters]
;		[-offset float]
;		[-beginOffset float]
;		[-endOffset float]
;		----------------------- )
leMPPSubRect (
		defaultContacts
		CO   drawing
		0.13   0.13 t t  0.005  0.005  0.13  fit  1
		userParams( name layer purpose width height conn chop minEnclosure eol spacing spaceType numRows )
);leMPPSubRect

) ;leMPPControls

;vdrc(
;( vdrcParamName vdrcParamValue )
;( ------------- -------------- )
;) ;vdrc

) ;controls

layerDefinitions(
 techPurposes(
;( purposeName  purposeNum   abbreviation )
;(   -----------        ----------        ----------     )
( dmy0             0          dmy0         )
( dmy1             1          dmy1         )
( dmy2             2          dmy2         )
( dmy3             3          dmy3         )
( dmy4             4          dmy4         )
( waterMark        63         waterMark    )
( dummy            127        dummy        )
( subnode          221        subnode      )
( extract          222        extract      )
 ) ;techPurposes

 techLayers(
;( layerName  layerNum   abbreviation )
;(   ---------      --------      ------------ )
( NWELL            1          NWELL        )
( DNW              2          DNW          )
( DIFF             3          DIFF         )
( PIMP             4          PIMP         )
( NIMP             5          NIMP         )
( DIFF_25          6          DIFF_25      )
( PAD              7          PAD          )
( ESD_25           8          ESD_25       )
( SBLK             9          SBLK         )
( PO               10         PO           )
( M1               11         M1           )
( VIA1             12         VIA1         )
( M2               13         M2           )
( VIA2             14         VIA2         )
( M3               15         M3           )
( VIA3             16         VIA3         )
( M4               17         M4           )
( VIA4             18         VIA4         )
( M5               19         M5           )
( VIA5             20         VIA5         )
( M6               21         M6           )
( VIA6             22         VIA6         )
( M7               23         M7           )
( M8               24         M8           )
( M9               25         M9           )
( CO               26         CO           )
( VIA7             27         VIA7         )
( VIA8             28         VIA8         )
( HVTIMP           29         HVTIMP       )
( LVTIMP           30         LVTIMP       )
( M1PIN            31         M1PIN        )
( M2PIN            32         M2PIN        )
( M3PIN            33         M3PIN        )
( M4PIN            34         M4PIN        )
( M5PIN            35         M5PIN        )
( M6PIN            36         M6PIN        )
( M7PIN            37         M7PIN        )
( M8PIN            38         M8PIN        )
( M9PIN            39         M9PIN        )
( HOTNWL           41         HOTNWL       )
( DIODMARK         43         DIODMARK     )
( BJTMARK          44         BJTMARK      )
( RNW              45         RNW          )
( RMARK            46         RMARK        )
( LOGO             48         LOGO         )
( IP               49         IP           )
( PrBoundary       50         PrBoundary   )
( RM1              51         RM1          )
( RM2              52         RM2          )
( RM3              53         RM3          )
( RM4              54         RM4          )
( RM5              55         RM5          )
( RM6              56         RM6          )
( RM7              57         RM7          )
( RM8              58         RM8          )
( RM9              59         RM9          )
( DM1EXCL          61         DM1EXCL      )
( DM2EXCL          62         DM2EXCL      )
( DM3EXCL          63         DM3EXCL      )
( DM4EXCL          64         DM4EXCL      )
( DM5EXCL          65         DM5EXCL      )
( DM6EXCL          66         DM6EXCL      )
( DM7EXCL          67         DM7EXCL      )
( DM8EXCL          68         DM8EXCL      )
( DM9EXCL          69         DM9EXCL      )
( VARMARK          70         VARMARK      )
( CBMMARK          71         CBMMARK      )
( INDMARK          72         INDMARK      )
( CTMMARK          73         CTMMARK      )
( METDMY           74         METDMY       )
( PWELL            75         PWELL        )
( DIFF_33          76         DIFF_33      )
( INDPINM          79         INDPINM      )
 ) ;techLayers

 techLayerPurposePriorities(
;(  layerName   purposeName  )
;(   ---------        ------------     )
( NWELL            drawing          )
( DNW              drawing          )
( DIFF             drawing          )
( PIMP             drawing          )
( NIMP             drawing          )
( DIFF_25          drawing          )
( PAD              drawing          )
( ESD_25           drawing          )
( SBLK             drawing          )
( PO               drawing          )
( M1               drawing          )
( M2               drawing          )
( M3               drawing          )
( M4               drawing          )
( M5               drawing          )
( M6               drawing          )
( M7               drawing          )
( M8               drawing          )
( M9               drawing          )
( CO               drawing          )
( VIA1             drawing          )
( VIA2             drawing          )
( VIA3             drawing          )
( VIA4             drawing          )
( VIA5             drawing          )
( VIA6             drawing          )
( VIA7             drawing          )
( VIA8             drawing          )
( HVTIMP           drawing          )
( LVTIMP           drawing          )
( M1PIN            drawing          )
( M2PIN            drawing          )
( M3PIN            drawing          )
( M4PIN            drawing          )
( M5PIN            drawing          )
( M6PIN            drawing          )
( M7PIN            drawing          )
( M8PIN            drawing          )
( M9PIN            drawing          )
( HOTNWL           drawing          )
( DIODMARK         drawing          )
( BJTMARK          drawing          )
( RNW              drawing          )
( RMARK            drawing          )
( LOGO             drawing          )
( IP               drawing          )
( PrBoundary       drawing          )
( RM1              drawing          )
( RM2              drawing          )
( RM3              drawing          )
( RM4              drawing          )
( RM5              drawing          )
( RM6              drawing          )
( RM7              drawing          )
( RM8              drawing          )
( RM9              drawing          )
( DM1EXCL          drawing          )
( DM2EXCL          drawing          )
( DM3EXCL          drawing          )
( DM4EXCL          drawing          )
( DM5EXCL          drawing          )
( DM6EXCL          drawing          )
( DM7EXCL          drawing          )
( DM8EXCL          drawing          )
( DM9EXCL          drawing          )
( VARMARK          drawing          )
( CBMMARK          drawing          )
( INDMARK          drawing          )
( CTMMARK          drawing          )
( METDMY           drawing          )
( PWELL            drawing          )
( DIFF_33          drawing          )
( INDPINM          drawing          )
) ;techLayerPurposePriorities

techDisplays(
;(lLayerName purposeName  packet  vis sel   con2ChgLy   drgEnbl  valid  [visibleOnly  [static]])
;(   ---------        -------           ------   --- ---    ------------     -------    -----  -- --)
( NWELL            drawing          NWELL           	t t nil t t  )
( DNW              drawing          DNW             	t t nil t t  )
( DIFF             drawing          DIFF            	t t nil t t  )
( PIMP             drawing          PIMP            	t t nil t t  )
( NIMP             drawing          NIMP            	t t nil t t  )
( DIFF_25          drawing          DIFF_25         	t t nil t t  )
( PAD              drawing          PAD             	t t nil t t  )
( ESD_25           drawing          ESD_25          	t t nil t t  )
( SBLK             drawing          SBLK            	t t nil t t  )
( PO               drawing          PO              	t t nil t t  )
( M1               drawing          M1              	t t nil t t  )
( M2               drawing          M2              	t t nil t t  )
( M3               drawing          M3              	t t nil t t  )
( M4               drawing          M4              	t t nil t t  )
( M5               drawing          M5              	t t nil t t  )
( M6               drawing          M6              	t t nil t t  )
( M7               drawing          M7              	t t nil t t  )
( M8               drawing          M8              	t t nil t t  )
( M9               drawing          M9              	t t nil t t  )
( CO               drawing          CO              	t t nil t t  )
( VIA1             drawing          VIA1            	t t nil t t  )
( VIA2             drawing          VIA2            	t t nil t t  )
( VIA3             drawing          VIA3            	t t nil t t  )
( VIA4             drawing          VIA4            	t t nil t t  )
( VIA5             drawing          VIA5            	t t nil t t  )
( VIA6             drawing          VIA6            	t t nil t t  )
( VIA7             drawing          VIA7            	t t nil t t  )
( VIA8             drawing          VIA8            	t t nil t t  )
( HVTIMP           drawing          HVTIMP          	t t nil t t  )
( LVTIMP           drawing          LVTIMP          	t t nil t t  )
( M1PIN            drawing          M1PIN           	t t nil t t  )
( M2PIN            drawing          M2PIN           	t t nil t t  )
( M3PIN            drawing          M3PIN           	t t nil t t  )
( M4PIN            drawing          M4PIN           	t t nil t t  )
( M5PIN            drawing          M5PIN           	t t nil t t  )
( M6PIN            drawing          M6PIN           	t t nil t t  )
( M7PIN            drawing          M7PIN           	t t nil t t  )
( M8PIN            drawing          M8PIN           	t t nil t t  )
( M9PIN            drawing          M9PIN           	t t nil t t  )
( HOTNWL           drawing          HOTNWL          	t t nil t t  )
( DIODMARK         drawing          DIODMARK        	t t nil t t  )
( BJTMARK          drawing          BJTMARK         	t t nil t t  )
( RNW              drawing          RNW             	t t nil t t  )
( RMARK            drawing          RMARK           	t t nil t t  )
( LOGO             drawing          LOGO            	t t nil t t  )
( IP               drawing          IP              	t t nil t t  )
( PrBoundary       drawing          PrBoundary      	t t nil t t  )
( RM1              drawing          RM1             	t t nil t t  )
( RM2              drawing          RM2             	t t nil t t  )
( RM3              drawing          RM3             	t t nil t t  )
( RM4              drawing          RM4             	t t nil t t  )
( RM5              drawing          RM5             	t t nil t t  )
( RM6              drawing          RM6             	t t nil t t  )
( RM7              drawing          RM7             	t t nil t t  )
( RM8              drawing          RM8             	t t nil t t  )
( RM9              drawing          RM9             	t t nil t t  )
( DM1EXCL          drawing          DM1EXCL         	t t nil t t  )
( DM2EXCL          drawing          DM2EXCL         	t t nil t t  )
( DM3EXCL          drawing          DM3EXCL         	t t nil t t  )
( DM4EXCL          drawing          DM4EXCL         	t t nil t t  )
( DM5EXCL          drawing          DM5EXCL         	t t nil t t  )
( DM6EXCL          drawing          DM6EXCL         	t t nil t t  )
( DM7EXCL          drawing          DM7EXCL         	t t nil t t  )
( DM8EXCL          drawing          DM8EXCL         	t t nil t t  )
( DM9EXCL          drawing          DM9EXCL         	t t nil t t  )
( VARMARK          drawing          VARMARK         	t t nil t t  )
( CBMMARK          drawing          CBMMARK         	t t nil t t  )
( INDMARK          drawing          INDMARK         	t t nil t t  )
( CTMMARK          drawing          CTMMARK         	t t nil t t  )
( METDMY           drawing          METDMY          	t t nil t t  )
( PWELL            drawing          PWELL           	t t nil t t  )
( DIFF_33          drawing          DIFF_33         	t t nil t t  )
( INDPINM          drawing          INDPINM         	t t nil t t  )
) ;techDisplays

 techLayerProperties(
;( propName     layer1/lpp1  [layer2/lpp2]      propValue )
;( --------            ------           ----------            ---------   )
( RESISTANCE       M1              	 0.009 )
( RESISTANCE       VIA1            	 0.0016 )
( RESISTANCE       M2              	 0.009 )
( RESISTANCE       VIA2            	 0.0016 )
( RESISTANCE       M3              	 0.009 )
( RESISTANCE       VIA3            	 0.0016 )
( RESISTANCE       M4              	 0.009 )
( RESISTANCE       VIA4            	 0.0016 )
( RESISTANCE       M5              	 0.009 )
( RESISTANCE       VIA5            	 0.0016 )
( RESISTANCE       M6              	 0.009 )
( RESISTANCE       VIA6            	 0.0016 )
( RESISTANCE       M7              	 0.009 )
( RESISTANCE       M8              	 0.009 )
( RESISTANCE       M9              	 0.028 )
( RESISTANCE       VIA7            	 0.0016 )
( RESISTANCE       VIA8            	 0.0016 )
 ) ;techLayerProperties

;techDerivedLayers(
;( derivedLayerName    layerNum   (operator layer1 [layer2] [params])  ) 
;(   -------------------       ---------     --------    ----     ------      -------     ) 
;) ;techDerivedLayers

) ;layerDefinitions

layerRules(
 functions(
;( layer     function     [maskNumber   [numberOfColorMasks [defaultColor]]]  [-fullColor <bool>  [-recognitionPurposes (purposeName ...)]])
;( -----     --------      ---------     --------------      -----------       ---------            -----------)
( NWELL             nWell          1            )
( DIFF              diffusion      2            )
( DIFF_25           diffusion      3            )
( PIMP              pImplant       4            )
( NIMP              nImplant       5            )
( M1                metal          6            )
( VIA1              cut            7            )
( M2                metal          8            )
( VIA2              cut            9            )
( M3                metal          10           )
( VIA3              cut            11           )
( M4                metal          12           )
( VIA4              cut            13           )
( M5                metal          14           )
( VIA5              cut            15           )
( M6                metal          16           )
( VIA6              cut            17           )
( M7                metal          18           )
( M8                metal          19           )
( M9                metal          20           )
( VIA7              cut            21           )
( VIA8              cut            22           )
( CO                cut            23           )
( BJTMARK           other          24           )
( DIODMARK          other          25           )
( DM1EXCL           other          26           )
( DM2EXCL           other          27           )
( DM3EXCL           other          28           )
( DM4EXCL           other          29           )
( DM5EXCL           other          30           )
( DM6EXCL           other          31           )
( DM7EXCL           other          32           )
( DM8EXCL           other          33           )
( DM9EXCL           other          34           )
( DNW               other          35           )
( ESD_25            other          36           )
( HOTNWL            other          37           )
( HVTIMP            other          38           )
( IP                other          39           )
( LOGO              other          40           )
( LVTIMP            other          41           )
( M1PIN             other          42           )
( M2PIN             other          43           )
( M3PIN             other          44           )
( M4PIN             other          45           )
( M5PIN             other          46           )
( M6PIN             other          47           )
( M7PIN             other          48           )
( M8PIN             other          49           )
( M9PIN             other          50           )
( PAD               other          52           )
( PO                other          53           )
( RM1               other          55           )
( RM2               other          56           )
( RM3               other          57           )
( RM4               other          58           )
( RM5               other          59           )
( RM6               other          60           )
( RM7               other          61           )
( RM8               other          62           )
( RM9               other          63           )
( RNW               other          64           )
( RMARK             other          65           )
( SBLK              other          66           )
( PrBoundary        other          67           )
( VARMARK           other          68           )
( CBMMARK           other          69           )
( INDMARK           other          70           )
( CTMMARK           other          71           )
( METDMY            other          72           )
( PWELL             other          73           )
( DIFF_33           diffusion      74           )
( INDPINM           other          79           )
 ) ;functions

 mfgResolutions(
;( layer         mfgResolution )
;( -----           -------------    )
( NWELL              0.005 )
( DNW                0.005 )
( DIFF               0.005 )
( PIMP               0.005 )
( NIMP               0.005 )
( DIFF_25            0.005 )
( PAD                0.005 )
( ESD_25             0.005 )
( SBLK               0.005 )
( PO                 0.005 )
( M1                 0.005 )
( VIA1               0.005 )
( M2                 0.005 )
( VIA2               0.005 )
( M3                 0.005 )
( VIA3               0.005 )
( M4                 0.005 )
( VIA4               0.005 )
( M5                 0.005 )
( VIA5               0.005 )
( M6                 0.005 )
( VIA6               0.005 )
( M7                 0.005 )
( M8                 0.005 )
( M9                 0.005 )
( CO                 0.005 )
( VIA7               0.005 )
( VIA8               0.005 )
( HVTIMP             0.005 )
( LVTIMP             0.005 )
( M1PIN              0.005 )
( M2PIN              0.005 )
( M3PIN              0.005 )
( M4PIN              0.005 )
( M5PIN              0.005 )
( M6PIN              0.005 )
( M7PIN              0.005 )
( M8PIN              0.005 )
( M9PIN              0.005 )
( HOTNWL             0.005 )
( DIODMARK           0.005 )
( BJTMARK            0.005 )
( RNW                0.005 )
( RMARK              0.005 )
( LOGO               0.005 )
( IP                 0.005 )
( PrBoundary         0.005 )
( RM1                0.005 )
( RM2                0.005 )
( RM3                0.005 )
( RM4                0.005 )
( RM5                0.005 )
( RM6                0.005 )
( RM7                0.005 )
( RM8                0.005 )
( RM9                0.005 )
( DM1EXCL            0.005 )
( DM2EXCL            0.005 )
( DM3EXCL            0.005 )
( DM4EXCL            0.005 )
( DM5EXCL            0.005 )
( DM6EXCL            0.005 )
( DM7EXCL            0.005 )
( DM8EXCL            0.005 )
( DM9EXCL            0.005 )
( VARMARK            0.005 )
( CBMMARK            0.005 )
( INDMARK            0.005 )
( CTMMARK            0.005 )
( METDMY             0.005 )
( PWELL              0.005 )
( DIFF_33            0.005 )
( INDPINM            0.005 )
 ) ;mfgResolutions

;viaLayers(
;) ;viaLayers

 routingDirections(
;( layer         direction  )
;( -----          --------    )
( M1 	horizontal )
( M2 	vertical )
( M3 	horizontal )
( M4 	vertical )
( M5 	horizontal )
( M6 	vertical )
( M7 	horizontal )
( M8 	vertical )
( M9 	horizontal )
 ) ;routingDirections

;currentDensities(
;( currentDensityName      layerName      value ) 
;( ---------------------          ---------         ----- ) 
;) ;currentDensities

;cutClasses(
;( layerName 
;  ( className [-numCuts numCuts] [-minWidth] [-minLength] (width length) [-comment ["rule_id"] "string"] )
;) 
;) ;cutClasses

;trackDefs(
;tracks(
;  (trackName -type <default | reserved | nonReserved> -offset <float> [-width <float>] [-color <string>]
;  [-sigType <all | power | ground | clock | signal | tieoff | tieHi | tieLo | analog | scan | reset>] [-visible <bool>] [-active <bool>]
;  [-gridOffsetLow <float>] [-gridOffsetHigh <float>] [-gridPointsLow (<float>...)] [-gridPointsHigh (<float>...)])
;);tracks

;groups(
;  (name layer (trackName ...) -direction <horizontal | vertical> -pitch (<float> <float>)
;  [-allowCrossBoundary <bool>])
;);groups

;regions(
;  (name (groupName ...) 
;  [-flip (<bool> <bool>)|-flipGroups (groupName...) [-flipGroupsFirst (groupName...)] [-groupFlipTarget tile | region]] 
;  [-minCount (<int> <int>)] [-maxCount (<int> <int>)] [-autoCreateGlobalRegion <bool>] 
;  [-snapGrid (<float> <float>)] [-snapOffset (<float> <float>)] 
;  [-defaultTracksVisible default | false | true] [-defaultTracksActive default | false | true] 
;  [-groupEnclosure (<float> <float>)] [-enclosedGroups (groupName...)] 
;  [-flipTilesX <bool> [-flipTilesXFirst <bool>]] [-flipTilesY <bool> [-flipTilesYFirst <bool>]] )
;);regions

;boundaryTrackData(
; (name -width <float> -space <float> -type <default | reserved | nonReserved> [-color <string>])
;);boundaryTrackData

;boundaryTracks(
; (name layer (boundaryTrackDataName...) -direction <horizontal | vertical>)
;);boundaryTracks

;boundaryTrackRegions(
; (name (boundaryTrackName...) -boundaryLPP (<string> <string>) -minEnclosure (<float> <float>)
;  [-tileWidth <float>] [-tileHeight <float>] [-minNumTiles (<int> <int>)])
;);boundaryTrackRegions

;) ;trackDefs

;trackPatternDefs(
;  trackPatternDef(name layer|(layer purpose) type spacingDirection trackPitch [width [(numTracks blockPitch) [offset [visible [active [autoGenerateTrack [color]]]]]]])
;) ;trackPatternDefs

;colorClasses(
;( layer [derived] 
;  ( color  colorLayer | (colorLayer colorPurpose ) )
;) 
;) ;colorClasses

;wideAlignments(
;( width pitch [distance [global]] [-groups <array> [-groupShapes ( grpLayer | (grpLayer grpPurpose) (spacing height) ... )]]
;  [-endShapes ( endLayer | (endLayer endPurpose) endExtension | (bottom-extension top-extension) )]
;  [-stepY step] [-colorDisplayPattern (colorPattern)] [-validFinSnapBottomPattern ( SnapPattern )]
;  wideTrackLayer | (wideTrackLayer wideTrackPurpose ) 
;  ( boundaryLayer | (boundaryLayer bounaryPurpose )  extension|(extension upperExtension) minNumTrack )
;  ( deviceLayer | (deviceLayer devicePurpose )  extension minNumTrack ) [-comment ["rule_id"] "description"]) 
;) ;wideAlignments

;pinTextLayers(
;( (shape lpp) (text lpp) (pin lpp))
;) ;pinTextLayers

;voltageTextLayers(
;( (object LPP) (minVoltage LPP) (maxVoltage LPP) )
;) ;voltageTextLayers

) ;layerRules

 viaDefs(
standardViaDefs(
;(viaDefName  layer1  layer2  (cutLayer cutWidth cutHeight [resistancePerCut])
;(cutRows   cutCol  (cutSpacing))
;(layer1Enc) (layer2Enc) (layer1Offset)  (layer2Offset)  (origOffset)
;[implant1   (implant1Enc)  [implant2  (implant2Enc) [wellsublayer]]])
;( -------------------------------------------------------------------------- )
(DIFFCON          DIFF         M1           ( CO          0.13 0.13 0 ) 
 (1 1 ( 0.13 0.13 )  )
 ( 0.05 0.04 )  ( 0.05 0.04 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(POLYCON          PO           M1           ( CO          0.13 0.13 0 ) 
 (1 1 ( 0.13 0.13 )  )
 ( 0.05 0.04 )  ( 0.05 0.04 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(VIA12            M1           M2           ( VIA1        0.14 0.14 0.0016 ) 
 (1 1 ( 0.16 0.16 )  )
 ( 0.05 0.005 )  ( 0.05 0.005 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(VIA23            M2           M3           ( VIA2        0.14 0.14 0.0016 ) 
 (1 1 ( 0.16 0.16 )  )
 ( 0.05 0.005 )  ( 0.05 0.005 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(VIA34            M3           M4           ( VIA3        0.14 0.14 0.0016 ) 
 (1 1 ( 0.16 0.16 )  )
 ( 0.05 0.005 )  ( 0.05 0.005 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(VIA45            M4           M5           ( VIA4        0.14 0.14 0.0016 ) 
 (1 1 ( 0.16 0.16 )  )
 ( 0.05 0.005 )  ( 0.05 0.005 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(VIA56            M5           M6           ( VIA5        0.14 0.14 0.0016 ) 
 (1 1 ( 0.16 0.16 )  )
 ( 0.05 0.005 )  ( 0.05 0.005 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(VIA67            M6           M7           ( VIA6        0.14 0.14 0.0016 ) 
 (1 1 ( 0.16 0.16 )  )
 ( 0.05 0.005 )  ( 0.05 0.005 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(VIA78            M7           M8           ( VIA7        0.14 0.14 0.0016 ) 
 (1 1 ( 0.16 0.16 )  )
 ( 0.05 0.005 )  ( 0.05 0.005 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
(VIA89            M8           M9           ( VIA8        0.36 0.36 0.0016 ) 
 (1 1 ( 0.34 0.34 )  )
 ( 0.08 0.03 )  ( 0.08 0.03 )  ( 0 0 )  ( 0 0 )  ( 0 0 )  )
) ;standardViaDefs

;standardViaVariants(
;(viaVariantName  standardViadefName [-cutLayer <string>] [-cutWidth <float>] [-cutHeight <float>]
;[-cutRows <int>] [-cutColumns <int>] [-cutSpacing (<float> <float>)]
;[-layer1Enc (<float> <float>)] [-layer2Enc (<float> <float>)] 
;[-layer1Offset (<float> <float>)] [-layer2Offset (<float> <float>)] [-originOffset (<float> <float>)]
;[-implant1Enc (<float> <float>)] [-implant2Enc (<float> <float>)] ) 
;( -------------------------------------------------------------------------- )
;) ;standardViaVariants 

;customViaDefs(
;( viaDefName libName cellName viewName layer1 layer2 resistancePerCut)
;(    ----------    -------     --------     -------       ------ ------     ---------------- )
;) ;customViaDefs

;customViaVariants(
;( viaVariantName customViaDefName ( (param value)...) )
;) ;customViaVariants

;bridgeVias(
;( viaDefName|viaVariantName -legWidth <float> -legPitch <float> -legDirection <vertical|horizontal> 
; -bridgeWidth <float|rangeValue> [-bridgeWidthParamName <string>] [-secondLegWidth <float>] 
; [-bridgePitch <float>] [-bridgePitchParamName <string>] [-cutSpacing (<float> < float>)] 
; [-cutRowsParamName <string>] [-cutColumnsParamName <string>] [-comment ["rule_id"] "string"] )
;) ;bridgeVias

 ) ;viaDefs

;siteDefs(
;scalarSiteDefs(
;( siteDefName    siteDefType    width     height    symInX    symInY    symInR90 )
;(   -----------         ---------          ----        -----       ------        -----          --------- )
;) ;scalarSiteDefs

;arraySiteDefs(
;( siteDefName    siteDefType   ( ( siteRefName  dx   dy    orientation ) ... )   [symInX]   [symInY]  [symInR90] )
;(   -----------        -----------          -------------     --    --      ----------              -------        --------      ----------   )
;) ;arraySiteDefs

;) ;siteDefs

constraintGroups (
;( groupName       [-operator op][-lppSpec lp][-override true|false] //default operator value is precedence, default override value is false;
;( constraintName [layers/lpps]   contraintValue [params] [-isHard true|false] [-comment] )
;( constraintGroup referenceGroupName);
;-------------------------------------------------------- )
( foundry  
( minWidth	NWELL 	0.65 )
( minSpacing	NWELL 	0.65 )
( minWidth	DIFF 	0.12 )
( minSpacing	DIFF 	0.14 )
( minArea	DIFF 	0.08 )
( minWidth	PO 	0.1 )
( minSpacing	PO 	0.18 )
( minWidth	PIMP 	0.24 )
( minSpacing	PIMP 	0.24 )
( minWidth	NIMP 	0.24 )
( minSpacing	NIMP 	0.24 )
( minWidth	CO 	0.13 )
( minSpacing	CO 	0.13 )
( minWidth	M1 	0.14 )
( minSpacing	M1 	0.14 )
( minArea	M1 	0.07 )
( minWidth	VIA1 	0.14 )
( minSpacing	VIA1 	0.16 )
( minWidth	M2 	0.16 )
( minSpacing	M2 	0.16 )
( minArea	M2 	0.07 )
( minWidth	VIA2 	0.14 )
( minSpacing	VIA2 	0.16 )
( minWidth	M3 	0.24 )
( minSpacing	M3 	0.16 )
( minArea	M3 	0.07 )
( minWidth	VIA3 	0.24 )
( minSpacing	VIA3 	0.16 )
( minWidth	M4 	0.16 )
( minSpacing	M4 	0.16 )
( minArea	M4 	0.07 )
( minClearance	NWELL DIFF 	0.62 )
( minClearance	PIMP DIFF 	0.14 )
( minClearance	NIMP DIFF 	0.14 )
( minClearance	CO PO 	0.12 )
( minClearance	CO DIFF 	0.12 )
( minClearance	DIFF PO 	0.05 )
( minClearance	RMARK CO 	0.24 )
( minClearance	NIMP PIMP 	0.24 )
( minEnclosure	PO DIFF 	0.18 )
( minClearance	NWELL DNW 	3.5 )
( minClearance	SBLK DIFF 	0.24 )
( minClearance	SBLK PO 	0.24 )
( minClearance	SBLK CO 	0.24 )
( minEnclosure	PWELL DIFF 	0.24 )
( minEnclosure	NWELL DIFF 	0.24 )
( minEnclosure	M1 VIA1 	0.005 )
( minEnclosure	M2 VIA1 	0.005 )
( minEnclosure	M2 VIA2 	0.005 )
( minEnclosure	M3 VIA2 	0.005 )
( minEnclosure	M3 VIA3 	0.005 )
( minEnclosure	M4 VIA3 	0.005 )
( minEnclosure	M4 VIA4 	0.005 )
( minEnclosure	M5 VIA4 	0.005 )
( minEnclosure	M5 VIA5 	0.005 )
( minEnclosure	M6 VIA5 	0.005 )
( minEnclosure	M6 VIA6 	0.005 )
( minEnclosure	M7 VIA6 	0.005 )
( minEnclosure	M7 VIA7 	0.005 )
( minEnclosure	M8 VIA7 	0.005 )
( minEnclosure	NWELL DNW 	0.5 )
( minEnclosure	M8 VIA8 	0.005 )
( minEnclosure	M9 VIA8 	0.005 )
( minEnclosure	PIMP DIFF 	0.14 )
( minEnclosure	NIMP DIFF 	0.14 )
( minEnclosure	DIFF_25 DIFF 	0.3 )
( minEnclosure	HVTIMP DIFF 	0.14 )
( minEnclosure	LVTIMP DIFF 	0.14 )
( minEnclosure	SBLK DIFF 	0.24 )
( minEnclosure	PO DIFF 	0.18 )
( minEnclosure	SBLK PO 	0.24 )
( minEnclosure	PIMP PO 	0.2 )
( minEnclosure	NIMP PO 	0.2 )
( minEnclosure	DIFF CO 	0.04 )
( minEnclosure	PO CO 	0.04 )
( minEnclosure	M1 CO 	0.005 )
( endOfLineEnclosure	M1 VIA1 	0.05 )
( endOfLineEnclosure	M2 VIA1 	0.05 )
( endOfLineEnclosure	M2 VIA2 	0.05 )
( endOfLineEnclosure	M3 VIA2 	0.05 )
( endOfLineEnclosure	M3 VIA3 	0.05 )
( endOfLineEnclosure	M4 VIA3 	0.05 )
( endOfLineEnclosure	M4 VIA4 	0.05 )
( endOfLineEnclosure	M5 VIA4 	0.05 )
( endOfLineEnclosure	M5 VIA5 	0.05 )
( endOfLineEnclosure	M6 VIA5 	0.05 )
( endOfLineEnclosure	M6 VIA6 	0.05 )
( endOfLineEnclosure	M7 VIA6 	0.05 )
( endOfLineEnclosure	M7 VIA7 	0.05 )
( endOfLineEnclosure	M8 VIA7 	0.05 )
( endOfLineEnclosure	M8 VIA8 	0.05 )
( endOfLineEnclosure	M9 VIA8 	0.05 )
( endOfLineEnclosure	DIFF CO 	0.05 )
( endOfLineEnclosure	PO CO 	0.05 )
( endOfLineEnclosure	M1 CO 	0.05 )
( defWidth	CO 	0.13 )
( defWidth	DIFF 	0.12 )
( defWidth	DIFF_25 	0.66 )
( defWidth	DM1EXCL 	0.45 )
( defWidth	DM2EXCL 	0.45 )
( defWidth	DM3EXCL 	0.45 )
( defWidth	DM4EXCL 	0.45 )
( defWidth	DM5EXCL 	0.45 )
( defWidth	DM6EXCL 	0.45 )
( defWidth	DM7EXCL 	0.45 )
( defWidth	DM8EXCL 	0.45 )
( defWidth	DM9EXCL 	0.45 )
( defWidth	DNW 	3.5 )
( defWidth	HVTIMP 	0.24 )
( defWidth	IP 	0.1 )
( defWidth	LOGO 	0.45 )
( defWidth	LVTIMP 	0.24 )
( defWidth	M1 	0.14 )
( defWidth	M2 	0.16 )
( defWidth	M3 	0.16 )
( defWidth	M4 	0.16 )
( defWidth	M5 	0.16 )
( defWidth	M6 	0.16 )
( defWidth	M7 	0.16 )
( defWidth	M8 	0.16 )
( defWidth	M9 	0.45 )
( defWidth	NIMP 	0.24 )
( defWidth	NWELL 	0.65 )
( defWidth	PAD 	55 )
( defWidth	PIMP 	0.24 )
( defWidth	PO 	0.1 )
( defWidth	SBLK 	0.44 )
( defWidth	VIA1 	0.14 )
( defWidth	VIA2 	0.14 )
( defWidth	VIA3 	0.14 )
( defWidth	VIA4 	0.14 )
( defWidth	VIA5 	0.14 )
( defWidth	VIA6 	0.14 )
( defWidth	VIA7 	0.14 )
( defWidth	VIA8 	0.36 )
( defWidth	PrBoundary 	0.45 )
( validRoutingLayers	( CO DIFF DIFF_25 DIFF_33 M1 M2 M3 M4 M5 M6 M7 M8 M9 PO VIA1 VIA2 VIA3 VIA4 VIA5 VIA6 VIA7 VIA8 ) )
 );foundry
( LEFDefaultRouteSpec  
( minWidth	CO 	0.13 )
( horizontalRouteGridPitch	M1 	0.2 )
( verticalRouteGridPitch	M1 	0.2 )
( horizontalRouteGridOffset	M1 	0.1 )
( verticalRouteGridOffset	M1 	0.1 )
( minWidth	M1 	0.14 )
( minWireExtension	M1 	0.105 )
( minWidth	VIA1 	0.13 )
( horizontalRouteGridPitch	M2 	0.3 )
( verticalRouteGridPitch	M2 	0.3 )
( horizontalRouteGridOffset	M2 	0.15 )
( verticalRouteGridOffset	M2 	0.15 )
( minWidth	M2 	0.15 )
( minWireExtension	M2 	0.105 )
( minWidth	VIA2 	0.13 )
( horizontalRouteGridPitch	M3 	0.3 )
( verticalRouteGridPitch	M3 	0.3 )
( horizontalRouteGridOffset	M3 	0.15 )
( verticalRouteGridOffset	M3 	0.15 )
( minWidth	M3 	0.15 )
( minWireExtension	M3 	0.105 )
( minWidth	VIA3 	0.13 )
( horizontalRouteGridPitch	M4 	0.3 )
( verticalRouteGridPitch	M4 	0.3 )
( horizontalRouteGridOffset	M4 	0.15 )
( verticalRouteGridOffset	M4 	0.15 )
( minWidth	M4 	0.15 )
( minWireExtension	M4 	0.105 )
( validRoutingLayers	( M1 M2 M3 M4 ) )
( validRoutingVias	( VIA12 VIA23 VIA34 POLYCON DIFFCON VIA45 VIA56 VIA67 VIA78 VIA89 ) )
 );LEFDefaultRouteSpec
) ;constraintGroups

