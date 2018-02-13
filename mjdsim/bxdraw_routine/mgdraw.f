*$ CREATE MGDRAW.FOR
*COPY MGDRAW
!ONLY BXDRAW in use
*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2013      by        Alfredo Ferrari           *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     MaGnetic field trajectory DRAWing: actually this entry manages   *
*                                        all trajectory dumping for    *
*                                        drawing                       *
*                                                                      *
*     Created on   01 March 1990   by        Alfredo Ferrari           *
*                                              INFN - Milan            *
*     Last change   12-Nov-13      by        Alfredo Ferrari           *
*                                              INFN - Milan            *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(CASLIM)'
      INCLUDE '(COMPUT)'
      INCLUDE '(SOURCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(GENSTK)'
      INCLUDE '(MGDDCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(QUEMGD)'
      INCLUDE '(SUMCOU)'
      INCLUDE '(TRACKR)'
*
      DIMENSION DTQUEN ( MXTRCK, MAXQMG )
*
      CHARACTER*20 FILNAM
      LOGICAL LFCOPE
      DOUBLE PRECISION :: INITKE = 0.0
      SAVE INITKE
      SAVE LFCOPE
      DATA LFCOPE / .FALSE. /
* Names of regions for boundary crossing check
* MRGNAM == initial region name; NRGNAM == new region name
      CHARACTER*8 MRGNAM, NRGNAM
      CHARACTER*8 RNAMCHK
*
*----------------------------------------------------------------------*
*                                                                      *
*     Icode = 1: call from Kaskad                                      *
*     Icode = 2: call from Emfsco                                      *
*     Icode = 3: call from Kasneu                                      *
*     Icode = 4: call from Kashea                                      *
*     Icode = 5: call from Kasoph                                      *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
* ! 77
         OPEN ( UNIT = 77, FILE = FILNAM, STATUS = 'UNKNOWN', FORM =
     &          'FORMATTED' )
      END IF

*      WRITE (IODRAW) NTRACK, MTRACK, JTRACK, SNGL (ETRACK),
*     &               SNGL (WTRACK)
*      WRITE (IODRAW) ( SNGL (XTRACK (I)), SNGL (YTRACK (I)),
*     &                 SNGL (ZTRACK (I)), I = 0, NTRACK ),
*     &               ( SNGL (DTRACK (I)), I = 1, MTRACK ),
*     &                 SNGL (CTRACK)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated
      IF ( LQEMGD ) THEN
         IF ( MTRACK .GT. 0 ) THEN
            RULLL  = ZERZER
            CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
*            WRITE (IODRAW) ( ( SNGL (DTQUEN (I,JBK)), I = 1, MTRACK ),
*     &                         JBK = 1, NQEMGD )
         END IF
      END IF
*  |  End of quenching
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     Boundary-(X)crossing DRAWing:                                    *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             19: boundary crossing                                    *
*     Icode = 2x: call from Emfsco                                     *
*             29: boundary crossing                                    *
*     Icode = 3x: call from Kasneu                                     *
*             39: boundary crossing                                    *
*     Icode = 4x: call from Kashea                                     *
*             49: boundary crossing                                    *
*     Icode = 5x: call from Kasoph                                     *
*             59: boundary crossing                                    *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY BXDRAW ( ICODE, MREG, NEWREG, XSCO, YSCO, ZSCO )
      !write in event file the # of track segments, # of edep events along track, particle type, energy, and weight 
      !	& of Neutrons and Muons that pass from Pb cube into NZone (pass completely through cube)

*	Open a file if this is our first time through
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
	 OPEN ( UNIT = 77, FILE = FILNAM, STATUS = 'UNKNOWN', FORM =
     &          'FORMATTED' )
	!This section uses the region names to gather/record the region numbers in IdUCX[0,1]
      END IF

*Look for mu- and neutrons passing through back of leadcube
*First translate region numbers into names
*     This name conversion thing is TEMPORARY ONLY!!!!!
*     Eventually, do not do this at runtime!!!!!
      CALL GEOR2N(MREG,   MRGNAM, IERR1)
      CALL GEOR2N(NEWREG, NRGNAM, IERR2)
      IF(IERR1 .NE. 0 .OR. IERR2 .NE. 0) STOP "Error in name conversion"
*      IF ((MRGNAM .EQ. "BH") .OR. (MRGNAM .EQ. "VA") .OR.
*     &          (MRGNAM .EQ. "leadcube") .OR. 
*     &          (MRGNAM .EQ. "nzone") .OR.
*     &          (MRGNAM .EQ. "VAS")) THEN      
*	WRITE(77,*) "REGION ", MREG, " == ", MRGNAM
*      END IF

*////begin checking if particle leaves veto panel

      IF ( ((MRGNAM .EQ. "vetop1") .OR. (MRGNAM .EQ. "vetop2") 
     & .OR. (MRGNAM .EQ. "vetop3") .OR. (MRGNAM .EQ. "vetop4") 
     & .OR. (MRGNAM .EQ. "vetop5") .OR. (MRGNAM .EQ. "vetop6") 
     & .OR. (MRGNAM .EQ. "vetop7") .OR. (MRGNAM .EQ. "vetop8") 
     & .OR. (MRGNAM .EQ. "vetop9") .OR. (MRGNAM .EQ. "vetop10") 
     & .OR. (MRGNAM .EQ. "vetop11") .OR. (MRGNAM .EQ. "vetop12"))
     & .AND. (NRGNAM .EQ. "overfl")) THEN
        WRITE(77,*) "Particle found leaving ", MRGNAM 
        WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ", 
     & DTRACK(1)
      END IF

*      IF ( (MRGNAM .EQ. "vetop2")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 2" 
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop3")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 3" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop4")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 4" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop5")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 5" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop6")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 6" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", 
*     & DTRACK(NPFLKA)
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop7")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 7" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop8")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 8" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop9")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 9" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop10")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 10" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop11")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 11" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF
*
*      IF ( (MRGNAM .EQ. "vetop12")
*     & .AND. (NRGNAM .EQ. "overfl")) THEN
*        WRITE(77,*) "Particle found leaving a veto panel 12" 
*	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
*     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
*     & ", # of edep events: ", MTRACK, ", latest edep: ", DTRACK
*      END IF

      IF ( (MRGNAM .EQ. "vetop13")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop14"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 13" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop14")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop13"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 14" 	
        WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop15")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop23"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 15" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop16")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop17"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 16" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop17")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop16"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 17" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop18")
     & .AND. ((NRGNAM .EQ. "pocket") .OR. (NRGNAM .EQ. "vetop19") 
     & .OR. (NRGNAM .EQ. "vetop21") .OR. (NRGNAM .EQ. "vetop22"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 18"
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1) 
      END IF

      IF ( (MRGNAM .EQ. "vetop19")
     & .AND. ((NRGNAM .EQ. "pocket") .OR. (NRGNAM .EQ. "vetop18") 
     & .OR. (NRGNAM .EQ. "vetop21") .OR. (NRGNAM .EQ. "vetop22"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 19" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop20")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop24"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 20" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop21")
     & .AND. ((NRGNAM .EQ. "pocket") .OR. (NRGNAM .EQ. "vetop18") 
     & .OR. (NRGNAM .EQ. "vetop19") .OR. (NRGNAM .EQ. "vetop22"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 21" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop22")
     & .AND. ((NRGNAM .EQ. "pocket") .OR. (NRGNAM .EQ. "vetop18") 
     & .OR. (NRGNAM .EQ. "vetop19") .OR. (NRGNAM .EQ. "vetop21"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 22" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop23")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop15"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 23" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop24")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop20"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 24" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop25")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop27"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 25" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop26")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop28"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 26" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop27")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop25"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 27" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop28")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop26"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 28" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop29")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop31"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 29" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop30")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop32"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 30" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop31")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop29"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 31" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( (MRGNAM .EQ. "vetop32")
     & .AND. ((NRGNAM .EQ. "overfl") .OR. (NRGNAM .EQ. "pocket") 
     & .OR. (NRGNAM .EQ. "vetop30"))) THEN
        WRITE(77,*) "Particle found leaving a veto panel 32" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

*////begin checking if particle leaves Ge detector

      IF ( ((MRGNAM .EQ. "rc1p1d1") .OR. (MRGNAM .EQ. "rc1p1d2") .OR.
     & (MRGNAM .EQ. "rc1p1d3") .OR. (MRGNAM .EQ. "rc1p1d4") .OR.
     & (MRGNAM .EQ. "rc1p2d1") .OR. (MRGNAM .EQ. "rc1p2d2") .OR. 
     & (MRGNAM .EQ. "rc1p2d3") .OR. (MRGNAM .EQ. "rc1p2d4") .OR.
     & (MRGNAM .EQ. "rc1p3d1") .OR. (MRGNAM .EQ. "rc1p3d2") .OR. 
     & (MRGNAM .EQ. "rc1p3d3") .OR. (MRGNAM .EQ. "rc1p3d4") .OR.
     & (MRGNAM .EQ. "rc1p4d1") .OR. (MRGNAM .EQ. "rc1p4d2") .OR. 
     & (MRGNAM .EQ. "rc1p4d3") .OR. (MRGNAM .EQ. "rc1p4d4") .OR.
     & (MRGNAM .EQ. "rc1p4d5") .OR.
     & (MRGNAM .EQ. "rc1p5d1") .OR. (MRGNAM .EQ. "rc1p5d2") .OR. 
     & (MRGNAM .EQ. "rc1p5d3") .OR. (MRGNAM .EQ. "rc1p5d4") .OR.
     & (MRGNAM .EQ. "rc1p6d1") .OR. (MRGNAM .EQ. "rc1p6d2") .OR. 
     & (MRGNAM .EQ. "rc1p6d3") .OR. (MRGNAM .EQ. "rc1p6d4") .OR.
     & (MRGNAM .EQ. "rc1p7d1") .OR. (MRGNAM .EQ. "rc1p7d2") .OR. 
     & (MRGNAM .EQ. "rc1p7d3") .OR. (MRGNAM .EQ. "rc1p7d4"))
     & .AND. (NRGNAM .EQ. "cryowSP")) THEN
        WRITE(77,*) "Particle found leaving a C1 Ge detector" 
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

      IF ( ((MRGNAM .EQ. "rc2p1d1") .OR. (MRGNAM .EQ. "rc2p1d2") .OR.
     & (MRGNAM .EQ. "rc2p1d3") .OR. (MRGNAM .EQ. "rc2p1d4") .OR.
     & (MRGNAM .EQ. "rc2p2d1") .OR. (MRGNAM .EQ. "rc2p2d2") .OR.
     & (MRGNAM .EQ. "rc2p2d3") .OR. (MRGNAM .EQ. "rc2p2d4") .OR.
     & (MRGNAM .EQ. "rc2p2d5") .OR.
     & (MRGNAM .EQ. "rc2p3d1") .OR. (MRGNAM .EQ. "rc2pdd2") .OR.
     & (MRGNAM .EQ. "rc2p3d3") .OR.
     & (MRGNAM .EQ. "rc2p4d1") .OR. (MRGNAM .EQ. "rc2p4d2") .OR.
     & (MRGNAM .EQ. "rc2p4d3") .OR. (MRGNAM .EQ. "rc2p4d4") .OR.
     & (MRGNAM .EQ. "rc2p4d5") .OR.
     & (MRGNAM .EQ. "rc2p5d1") .OR. (MRGNAM .EQ. "rc2p5d2") .OR.
     & (MRGNAM .EQ. "rc2p5d3") .OR. (MRGNAM .EQ. "rc2p5d4") .OR.
     & (MRGNAM .EQ. "rc2p6d1") .OR. (MRGNAM .EQ. "rc2p6d2") .OR.
     & (MRGNAM .EQ. "rc2p6d3") .OR. (MRGNAM .EQ. "rc2p6d4") .OR.
     & (MRGNAM .EQ. "rc2p7d1") .OR. (MRGNAM .EQ. "rc2p7d2") .OR.
     & (MRGNAM .EQ. "rc2p7d3") .OR. (MRGNAM .EQ. "rc2p7d4")) 
     & .AND. (NRGNAM .EQ. "cryoeSP")) THEN
        WRITE(77,*) "particle found leaving a C2 Ge detector"
	WRITE(77,*) MRGNAM, "| PID: ", JTRACK, ", Age: ", ATRACK, 
     & ", KE: ", ETRACK - AM(JTRACK), "Weight: ", WTRACK,
     & ", # of edep events: ", MTRACK, ", latest edep: ",
     & DTRACK(1)
      END IF

*////////old stuff
* INITIAL PARTICLE TKEFLK is fixed (No variation)
*      IF ((JTRACK .EQ. 11 .OR. JTRACK .EQ. 8) .AND. MRGNAM .EQ.
*     & "VAS") THEN	
*		INITKE = 0.0
*		WRITE(77,*) "***** Source MUON- or Neutron PARTICLE!!! *****"
*		WRITE(77,*) "JTRACK: ", JTRACK, ", WTRACK: ", WTRACK
*		WRITE(77,*) "ETRACK,KE: ", ETRACK, SNGL (TKEFLK (I))
*		WRITE(77,*) "***** END SOURCE PARTICLE *****"
*		WRITE(77,*) ""
*		WRITE(77,*) "INITKE: ", INITKE
*		INITKE = SNGL(TKEFLK(I))
*		WRITE(77,*) "INITKE: ", INITKE
*      END IF
*      IF((MRGNAM .EQ. "leadcube") .AND. (NRGNAM .EQ. "nzone")) THEN
*	IF ((JTRACK .EQ. 8) .OR. (JTRACK .EQ. 11))  THEN y
*	  WRITE (77,*) "----- Pb to nzone BX -----"
*	  WRITE (77,*) "BXDRAW", JTRACK, SNGL(ETRACK), SNGL(TKEFLK(I))
*	  WRITE (77,*) ""
*	END IF	
*	IF (JTRACK .EQ. 8) THEN
*	  WRITE(77,*) INITKE, SNGL(ETRACK), 
*     &         SNGL(ETRACK)-AM(JTRACK), NCASES
*	  WRITE (77,*) "neutron"
*	END IF
*      END IF

      RETURN
*
*======================================================================*
*                                                                      *
*     Event End DRAWing:                                               *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY EEDRAW ( ICODE )
      RETURN
*
*======================================================================*
*                                                                      *
*     ENergy deposition DRAWing:                                       *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             10: elastic interaction recoil                           *
*             11: inelastic interaction recoil                         *
*             12: stopping particle                                    *
*             13: pseudo-neutron deposition                            *
*             14: escape                                               *
*             15: time kill                                            *
*     Icode = 2x: call from Emfsco                                     *
*             20: local energy deposition (i.e. photoelectric)         *
*             21: below threshold, iarg=1                              *
*             22: below threshold, iarg=2                              *
*             23: escape                                               *
*             24: time kill                                            *
*     Icode = 3x: call from Kasneu                                     *
*             30: target recoil                                        *
*             31: below threshold                                      *
*             32: escape                                               *
*             33: time kill                                            *
*     Icode = 4x: call from Kashea                                     *
*             40: escape                                               *
*             41: time kill                                            *
*             42: delta ray stack overflow                             *
*     Icode = 5x: call from Kasoph                                     *
*             50: optical photon absorption                            *
*             51: escape                                               *
*             52: time kill                                            *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY ENDRAW ( ICODE, MREG, RULL, XSCO, YSCO, ZSCO )
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
       END IF
*      WRITE (IODRAW)  0, ICODE, JTRACK, SNGL (ETRACK), SNGL (WTRACK)
*      WRITE (IODRAW)  SNGL (XSCO), SNGL (YSCO), SNGL (ZSCO), SNGL (RULL)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated : calculate quenching factor
*  |  and store quenched energy in DTQUEN(1, jbk)
      IF ( LQEMGD ) THEN
         RULLL = RULL
         CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
*         WRITE (IODRAW) ( SNGL (DTQUEN(1, JBK)), JBK = 1, NQEMGD )
      END IF
*  |  end quenching
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     SOurce particle DRAWing:                                         *
*                                                                      *
*======================================================================*
*
      ENTRY SODRAW
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = 77, FILE = FILNAM, STATUS = 'UNKNOWN', FORM =
     &          'FORMATTED' )
      END IF
      WRITE(77,*) "SODRAW: ILOFLK = ", ILOFLK(NPFLKA), 
     & ", (X,Y,Z)(cm) = (", XFLK(NPFLKA), ",",
     & YFLK(NPFLKA), ",",ZFLK(NPFLKA), 
     & "), ENERGY(GeV) = ", TKEFLK(NPFLKA), 
     & ", WEIGHT = ", WTFLK(NPFLKA)


*      IF (JTRACK .EQ. 11) THEN
*	WRITE (77,*) "-------------------------------------------------"
*        WRITE (77,*) "SODRAW1", JTRACK, SNGL (TKEFLK (0)),
*     &          SNGL (TKEFLK (I)), NP
*        WRITE (77,*) "SODRAW2", NCASE, NEVENT (I)
*        WRITE (77,*) "SODRAW3", SNGL (WTFLK (I)), SNGL (WEIPRI) 
*        WRITE (77,*) "SODRAW4", " REGION # = ", NRGFLK (I)
*	WRITE (77,*) "-------------------------------------------------"
*	WRITE(77,*) ""
*      END IF
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope: it works only for 1 source particle on
*  |  the stack for the time being
      IF ( ILOFLK (NPFLKA) .GE. 100000 .AND. LRADDC (NPFLKA) ) THEN
         IARES  = MOD ( ILOFLK (NPFLKA), 100000  )  / 100
         IZRES  = MOD ( ILOFLK (NPFLKA), 10000000 ) / 100000
         IISRES = ILOFLK (NPFLKA) / 10000000
         IONID  = ILOFLK (NPFLKA)
*         WRITE (IODRAW) ( IONID,SNGL(-TKEFLK(I)),
*     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
*     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
*     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
*     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |  Patch for heavy ions: it works only for 1 source particle on
*  |  the stack for the time being
      ELSE IF ( ABS (ILOFLK (NPFLKA)) .GE. 10000 ) THEN
         IONID = ILOFLK (NPFLKA)
         CALL DCDION ( IONID )
*         WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-IONID)),
*     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
*     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
*     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
*     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |  Patch for heavy ions: ???
      ELSE IF ( ILOFLK (NPFLKA) .LT. -6 ) THEN
*         WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-ILOFLK(NPFLKA))),
*     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
*     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
*     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
*     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |
      ELSE
*         WRITE (IODRAW) ( ILOFLK(I), SNGL (TKEFLK(I)+AM(ILOFLK(I))),
*     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
*     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
*     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
*     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
      END IF
*  |
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     USer dependent DRAWing:                                          *
*                                                                      *
*     Icode = 10x: call from Kaskad                                    *
*             100: elastic   interaction secondaries                   *
*             101: inelastic interaction secondaries                   *
*             102: particle decay  secondaries                         *
*             103: delta ray  generation secondaries                   *
*             104: pair production secondaries                         *
*             105: bremsstrahlung  secondaries                         *
*             110: decay products                                      *
*     Icode = 20x: call from Emfsco                                    *
*             208: bremsstrahlung secondaries                          *
*             210: Moller secondaries                                  *
*             212: Bhabha secondaries                                  *
*             214: in-flight annihilation secondaries                  *
*             215: annihilation at rest   secondaries                  *
*             217: pair production        secondaries                  *
*             219: Compton scattering     secondaries                  *
*             221: photoelectric          secondaries                  *
*             225: Rayleigh scattering    secondaries                  *
*             237: mu pair     production secondaries                  *
*     Icode = 30x: call from Kasneu                                    *
*             300: interaction secondaries                             *
*     Icode = 40x: call from Kashea                                    *
*             400: delta ray  generation secondaries                   *
*  For all interactions secondaries are put on GENSTK common (kp=1,np) *
*  but for KASHEA delta ray generation where only the secondary elec-  *
*  tron is present and stacked on FLKSTK common for kp=npflka          *
*                                                                      *
*======================================================================*
*
      ENTRY USDRAW ( ICODE, MREG, XSCO, YSCO, ZSCO )
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
* No output by default:
      RETURN
*=== End of subrutine Mgdraw ==========================================*
      END

