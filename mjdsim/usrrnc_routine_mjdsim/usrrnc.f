*$ CREATE USRRNC.FOR
*COPY USRRNC
*
*=== Usrrnc ===========================================================*
*
      SUBROUTINE USRRNC ( IZ, IA, IS, X, Y, Z, MREG, WEE, ICALL )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 2005-2005      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR Residual NuClei:                                            *
*                                                                      *
*     Created on   06 april 2005   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on 06-apr-05     by    Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*
*

*IZ is Z (atomic number) of resnuc
*IA is A (mass number) of resnuc
*IS is isomeric state of the residual nucleus
*X/Y/Z are XYZ of resnuc
*MREG is region the particle is in
*WEE is particle weight
*ICALL is internal code calling flag (not for general use)

        CHARACTER*8 MRGNAM
        CALL GEOR2N(MREG,   MRGNAM, IERR1)

	OPEN ( UNIT = 77, FILE = "mjdud.txt", STATUS = 'UNKNOWN', 
     & FORM = 'FORMATTED' )
	WRITE(77,*) "Residual Nucleus| Region = ", MRGNAM, 
     & ", Z = ", IZ, ", A = ", IA, ", Isomeric State = ", IS, 
     & ", Weight = ", WEE, ", ICALL = ", ICALL
	
*	CLOSE(UNIT=77, STATUS='KEEP')

      RETURN
*=== End of subroutine Usrrnc =========================================*
      END
