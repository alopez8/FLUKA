*readEVDT.f is used to read binary output of EVENTDAT card
*full example in REEVDT.f
* ISCORE does not read correctly for the 1st selection in the SCORE card
      PROGRAM RDEVDT
      CHARACTER*80 RUNTIT, FILNAM
      CHARACTER*32 RUNTIM
      DIMENSION ISCORE(4), ENDIST(12), REGSCO(5000,4)
      INTEGER :: IB, NREGS, NSCO, NCASE
      INTEGER :: IISC = 1
      INTEGER :: IISC2
      REAL :: WEIPRU, ENETOT, A, B,C,D

      WRITE(*,*) 'Name of the EVENTDAT binary file?'
      READ(*,'(A)') FILNAM
      IB = INDEX(FILNAM,".")
      OPEN(UNIT = 7, FORM = 'UNFORMATTED', FILE = FILNAM,
     &     STATUS = 'OLD')
      OPEN(UNIT = 8, FORM = 'FORMATTED', FILE = 
     &FILNAM(1:IB-1)//'_slim.txt', STATUS = 'REPLACE')

*     Once, at the beginning of the run:
      READ(7)  RUNTIT, RUNTIM, NREGS, NSCO, (ISCORE(IS), IS = 1, NSCO)

*     Loop on each primary particle:
 100  CONTINUE
      READ(7,END=300) NCASE, WEIPRU, ENETOT
      READ(7) (ENDIST(IE), IE = 1, 12)
      DO 200 ISC = 1, NSCO
         READ(7) IISC2, ISCORE(ISC)
	IF (ISC .EQ. 1) THEN	
		READ(7) A
	END IF
         READ(7) (REGSCO(IR,ISC), IR = 1, NREGS)
         DO 500 IR = 1, NREGS
	    IF (ISCORE(ISC) .EQ. 208) THEN
*	    	IF (IR .EQ. ) THEN
	        	WRITE(8,'(A5,1X,I5,1X,I3,1X,I3,1X,
     &ES10.3)') "EVDT ",NCASE,NREGS,IR,REGSCO(IR,ISC)
*	    	END IF
	    END IF

 500     CONTINUE
 200  CONTINUE

      READ(7) NDUM, DUM1, DUM2
      IF (DUM1 .LT. 0.) THEN
*        DUM1 < 0 is used to signal that seeds follow
         READ(7) ISEED1, ISEED2, SEED1, SEED2, SOPP1, SOPP2
      ELSE
         BACKSPACE 7
      END IF

*     This event is finished, start again with the next one
      GO TO 100

 300  CONTINUE
      CLOSE (UNIT = 7)
      CLOSE (UNIT = 8)
      END
