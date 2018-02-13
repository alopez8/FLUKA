*THIS SOURCE REPRODUCES RALPHS SHOWERSFROMFILE MAGE GENERATOR
*$ CREATE SOURCE.FOR
*COPY SOURCE
*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2010      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     New source for FLUKA9x-FLUKA20xy:                                *
*                                                                      *
*     Created on 07 January 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on  17-Oct-10    by    Alfredo Ferrari               *
*                                                                      *
*  This is just an example of a possible user written source routine.  *
*  note that the beam card still has some meaning - in the scoring the *
*  maximum momentum used in deciding the binning is taken from the     *
*  beam momentum.  Other beam card parameters are obsolete.            *
*                                                                      *
*       Output variables:                                              *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(BEAMCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(IOIOCM)'
      INCLUDE '(LTCLCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(SOURCM)'
      INCLUDE '(SUMCOU)'
*
      LOGICAL LFIRST
*
* declare variables here 
*NUMBER_OF_INPUTS IS THE NUMBER OF PARTICLES IN THE INPUT FILE (# OF LINES -1)
	INTEGER :: NUMBER_OF_INPUTS = 0
	INTEGER :: K
	REAL, DIMENSION (:), ALLOCATABLE :: E_IS
	REAL, DIMENSION (:), ALLOCATABLE :: X
	REAL, DIMENSION (:), ALLOCATABLE :: Y
	REAL, DIMENSION (:), ALLOCATABLE :: Z
	REAL, DIMENSION (:), ALLOCATABLE :: PX
	REAL, DIMENSION (:), ALLOCATABLE :: PY
	REAL, DIMENSION (:), ALLOCATABLE :: PZ
	REAL, DIMENSION (:), ALLOCATABLE :: ENERGY
	INTEGER, DIMENSION (:), ALLOCATABLE :: PIDPDG
	INTEGER, DIMENSION (:), ALLOCATABLE :: PIDFLUKA
	REAL, DIMENSION (:), ALLOCATABLE :: COSTHETA
	REAL :: COSTHETASTAR
	REAL :: DISTRIBUTION
	REAL :: X_MJD
	REAL :: Y_MJD
	REAL :: Z_MJD
	REAL :: PX_MJD
	REAL :: PY_MJD
	REAL :: PZ_MJD
	REAL :: R
	INTEGER :: RAND

*	REAL, DIMENSION E_IS(:), X(:), Y(:), Z(:),
*     & PX(:), PY(:), PZ(:), ENERGY(:), PIDPDG(:), 
*     & PIDF(:), COSTHETA(:) 

	

      SAVE LFIRST
      DATA LFIRST / .TRUE. /

*Muonbackground.txt format
*1st line: NLINES
*E_IS     COSTHETA_I    E        PX        PY          PZ        X          Y         Z          PID(PDG)  PID(FLUKA)  
*150342   0.866684      148738   25849.3   -69542.7    -128913   -63.3917   84.2571   -1466.99   13        11
*TXFLK = VX/V
*TYFLK = VY/V
*TZFLK = VZ/V
*EX.
*|P|=148737.8 GeV
*TXFLK =  0.173791 = PX/SQRT(PX^2+PY^2+PZ^2)
*TYFLK = -0.467552 = PY/SQRT(PX^2+PY^2+PZ^2)
*TZFLK = -0.866713 = PZ/SQRT(PX^2+PY^2+PZ^2)
*TXFLK^2 + TYFLK^2 + TZFLK^2 ~ 1



*E_IS ->
*COSTHETA_I ->
*IN SHOWERSFROMFILE E_IS AND COSTHETA_I GIVE WEIGHT OF VERTEX WITH DISTRIBUTION FUNCTION IN FILE
*
*E -> TKEFLK (NPFLKA) (LINE 122) IN SHOWERSFROMFILE THIS IS UNUSED, PARTICLE IS DEFINED WITH PX,PY,PZ *AT VERTEX X,Y,Z
*PX ->PZ(NLINES) --> DIRECTION COSINE TZFLK
*PY ->PX(NLINES) --> DIRECTION COSINE TXFLK
*PZ ->PY(NLINES) --> DIRECTION COSINE TYFLK
*X -> ZFLK
*Y -> XFLK
*Z -> YFLK (LINE 137/138/139) ALSO CHANGE GEANT4 AXIS TO FLUKA AXIS (X->Z, Y->X, Z->Y)
*PID(PDG) -> NOT USED
*PID(FLUKA) -> IJBEAM (LINE 93)


*//////////////FORTRAN ARRAYS ARE INDEXED-1
*//////////////READ STATEMENT DOES NOT RESET READ LOCATION
*EX 	DO K=1,5 
*		READ(100) X(K) !READ LINES 1-5 OF FILE "100" INTO ARRAY X
*	CONTINUE
*	DO K=1,5 
*		READ(100) X(K) !READ LINES 6-10 OF FILE "100" INTO ARRAY X
*	CONTINUE
*//////////////FORTRAN LINE CONTINUATION, PUT AMPERSAND AT POSITION 6 OF NEW LINE
*EX
*READ (89,*) E_IS, COSTHETA, ENERGY, PX, PY, PZ, X, Y, Z, PIDPDG,
*     & PIDFLUKA






*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
*  |  *** The following 3 cards are mandatory ***
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.



*	*open muonbackground.txt here
*	*read in data to arrays with do loop
*	*do 10

*!	OPEN(UNIT=89, FILE="../test.txt", STATUS="OLD") !10 inputs takes  ~5 sec to read in (Pbsourcetest 01:02.5 total)
*!	OPEN(UNIT=89, FILE="../Muonbackground.txt", STATUS="OLD")    !takes ~78 sec to read in (Pbsourcetest 02:06.9 total)
	OPEN(UNIT=89, FILE="../Muonbackground_mu.txt", STATUS="OLD") !takes ~5 sec to read in (Pbsourcetest 00:56.8 total)
	OPEN(UNIT=99, FILE="SOURCEOUT.txt", STATUS="UNKNOWN") 
	OPEN ( UNIT = 77, FILE = "mjdud.txt", STATUS = 'UNKNOWN', 
     & FORM = 'FORMATTED' )
	READ (89,*) NUMBER_OF_INPUTS
	WRITE(99,*) "NUMBER_OF_INPUTS = ", NUMBER_OF_INPUTS
	

*Muonbackground.txt format
*1st line: NLINES
*E_IS     COSTHETA_I    E        PX        PY          PZ        X          Y         Z          PID(PDG)  PID(FLUKA)  

	ALLOCATE (E_IS(NUMBER_OF_INPUTS))
	ALLOCATE (COSTHETA(NUMBER_OF_INPUTS))
	ALLOCATE (ENERGY(NUMBER_OF_INPUTS))
	ALLOCATE (PX(NUMBER_OF_INPUTS))
	ALLOCATE (PY(NUMBER_OF_INPUTS))
	ALLOCATE (PZ(NUMBER_OF_INPUTS))
	ALLOCATE (X(NUMBER_OF_INPUTS))
	ALLOCATE (Y(NUMBER_OF_INPUTS))
	ALLOCATE (Z(NUMBER_OF_INPUTS))
	ALLOCATE (PIDPDG(NUMBER_OF_INPUTS))
	ALLOCATE (PIDFLUKA(NUMBER_OF_INPUTS))
	
	DO 10 K=1, NUMBER_OF_INPUTS
		READ (89,*) E_IS(K), COSTHETA(K), ENERGY(K), PX(K),
     & PY(K), PZ(K), X(K), Y(K), Z(K), PIDPDG(K), PIDFLUKA(K) 
   10   CONTINUE


	

*	WRITE(99,*) "FILLED ARRAYS. NOW CHECKING ACCURACY:"
*	DO 20 K=1, 5
*		WRITE(99,*) E_IS(K), COSTHETA(K), ENERGY(K), PX(K),
*     & PY(K), PZ(K), X(K), Y(K), Z(K), PIDPDG(K), PIDFLUKA(K)
*   20   CONTINUE

*///close Muonbackground_mu.txt and SOURCEOUT.txt
	CLOSE(UNIT=89, STATUS="KEEP")
	
	
	
*  |  *** User initialization ***
      END IF
*  |
*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
*  Npflka is the stack counter: of course any time source is called it
*  must be =0

*/////ALL BELOW IS CALLED FOR EVERY PRIMARY IN START CARD
*!	WRITE(99,*) "SOURCE.F CALLED"

*	randomly select particle to add to stack
	CALL RANDOM_NUMBER(R)
	RAND = INT(1 + (NUMBER_OF_INPUTS - 1)*R)
	COSTHETASTAR = 0
	DISTRIBUTION = 0
	X_MJD = 0
	Y_MJD = 0
	Z_MJD = 0
	PX_MJD = 0
	PY_MJD = 0
	PZ_MJD = 0


*//////////////RANDOMLY SELECTED INPUT, 
*/////////////NOW CALC WEIGHT, PRINT EVID OF NUMENTRIES SHOWER WITH N PARTICLES MUONS ONLY
*according to wolfram alpha these match Ralph's
	COSTHETASTAR = SQRT((COSTHETA(RAND)**(2.) + 0.102573**(2.) - 
     & 0.068287*COSTHETA(RAND)**(0.958633) + 0.0407253*
     & COSTHETA(RAND)**(0.817285))/(1 + 0.102573**(2.) 
     & - 0.068287 + 0.0407253))
	

	DISTRIBUTION = 0.14*((E_IS(RAND)*(1.+3.64/(
     & E_IS(RAND)*COSTHETASTAR**(1.29))))**(-2.7))*
     & ((1./(1.+(1.1*E_IS(RAND)*COSTHETASTAR)/115.))
     & +(0.054/(1.+(1.1*E_IS(RAND)*COSTHETASTAR)/850.
     & )))

*/////////////THEN SET PARTICLE TIME TO 0.0 SECONDS, SHIFT TO ZERO THEN TO MJD, FILL PARTICLE VARIABLES

	IJBEAM = PIDFLUKA(RAND)
*ROTATE POSITION 10 DEGREES AND SHIFT TO MJD DETECTOR ROOM
*AND GEANT4 AXIS IS CHANGED TO FLUKA AXIS (X->Z, Y->X, Z->Y)	
	Z_MJD = (X(RAND) + 62.6425)*0.984807753 
     & + (Y(RAND) - 96.5094)*0.173648177
	Z_MJD = Z_MJD + 4.6478

	X_MJD = -1.*(X(RAND) + 62.6425)*0.173648177 
     & + (Y(RAND) - 96.5094)*0.984807753
	X_MJD = X_MJD - 16.5611


*//// FORCE Y VALUE (VERTICAL) TO 2m ABOVE CAVERN (3.524 m or 352.4 cm)
*	Y_MJD = Z(RAND) + 1466.99 + 1.6764
	Y_MJD = 3.524

*Muonbackground position units m, need cm
*convert Muonbackground units from meters to cm
	X_MJD = X_MJD*100.
	Y_MJD = Y_MJD*100.
	Z_MJD = Z_MJD*100.

	
*ROTATE MOMENTUM 10 DEGREES
*AND GEANT4 AXIS IS CHANGED TO FLUKA AXIS (X->Z, Y->X, Z->Y)	
	PZ_MJD = PX(RAND)*0.984807753 + PY(RAND)*0.173648177

	PX_MJD = -1.*PX(RAND)*0.173648177 + PY(RAND)*0.984807753

	PY_MJD = PZ(RAND)
	

      NPFLKA = NPFLKA + 1
*  Wt is the weight of the particle
*calculate weight of the particle?
      WTFLK  (NPFLKA) = DISTRIBUTION
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
*  Particle type (1=proton.....). Ijbeam is the type set by the BEAM
*  card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron:
      ELSE
         IONID = IJBEAM
         ILOFLK (NPFLKA) = IJBEAM
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
      END IF
*  |
*  +-------------------------------------------------------------------*
*  From this point .....
*  Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
*  User dependent flag:
      LOUSE  (NPFLKA) = 0
*  No channeling:
      LCHFLK (NPFLKA) = .FALSE.
      DCHFLK (NPFLKA) = ZERZER
*  User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
*  User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
*  Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
*  ... to this point: don't change anything
*  Particle age (s)
      AGESTK (NPFLKA) = +ZERZER
      AKNSHR (NPFLKA) = -TWOTWO
*  Kinetic energy of the particle (GeV)

*	* change to appropriate array value
*      TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 ) - AM (IONID)
*/////////////////////////// Muonbackground energy units GeV, need GeV
      TKEFLK (NPFLKA) = ENERGY(RAND)
*  Particle momentum (GeV/c)
*//////////////////////////Muonbackground momentum units GeV/c, need GeV/c
      PMOFLK (NPFLKA) = SQRT(PX_MJD**(2.)+PY_MJD**(2.)+PZ_MJD**(2.))
*     PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
*    &                       + TWOTWO * AM (IONID) ) )
*  Cosines (tx,ty,tz)

      TXFLK  (NPFLKA) = PX_MJD/SQRT(PX_MJD**(2.)
     & + PY_MJD**(2.) + PZ_MJD**(2.))
      TYFLK  (NPFLKA) = PY_MJD/SQRT(PX_MJD**(2.)
     & + PY_MJD**(2.) + PZ_MJD**(2.))
      TZFLK  (NPFLKA) = PZ_MJD/SQRT(PX_MJD**(2.)
     & + PY_MJD**(2.) + PZ_MJD**(2.))
*     TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
*    &                       - TYFLK (NPFLKA)**2 )
*  Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates (cm)

      XFLK   (NPFLKA) = X_MJD
      YFLK   (NPFLKA) = Y_MJD
      ZFLK   (NPFLKA) = Z_MJD
*  Calculate the total kinetic energy of the primaries: don't change
      IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &   THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
      RADDLY (NPFLKA) = ZERZER


*///////////////WRITE out primary data to sourceout file,
	WRITE (99,*) "source.f 99 RAND = ", RAND,
     & ", ILOFLK = ", ILOFLK(NPFLKA), ", (X,Y,Z) = (", 
     & XFLK(NPFLKA), ",", YFLK(NPFLKA), "," ,ZFLK(NPFLKA), 
     & "), ENERGY = ", TKEFLK(NPFLKA), ", WEIGHT = ", WTFLK(NPFLKA),
     & "NUMPAR = ", NUMPAR(NPFLKA)

	CLOSE(UNIT=99, STATUS='KEEP')

*	WRITE (77,*) "source.f RAND = ", RAND,
*     & ", ILOFLK = ", ILOFLK(NPFLKA), ", (X,Y,Z) = (", 
*     & XFLK(NPFLKA), ",", YFLK(NPFLKA), ",",ZFLK(NPFLKA), 
*     & "), ENERGY = ", TKEFLK(NPFLKA), ", WEIGHT = ", WTFLK(NPFLKA),
*     & "NUMPAR = ", NUMPAR(NPFLKA)


*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV

*///////////////WRITE out primary data to sourceout file, close sourceout file


*	CLOSE(UNIT=77, STATUS='KEEP')

      RETURN
*=== End of subroutine Source =========================================*
      END

