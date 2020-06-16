C123456789012345678901234567890123456789012345678901234567890123456789012
      BLOCK DATA
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'
  
      DATA NVAR,INU,INE,INU2,INV2,INW2,INUV,INK/
     +      7,   1,  2,   3,   4,   5,   6,   7/
c       DATA NITERM,URF/1,0.3,0.1,0.3,0.3,0.3,0.3,0.1,7*0.3/ 
C       DATA NITERM,URF/1,0.3,0.5,0.3,0.3,0.3,0.3,0.1,7*0.3/   
c       DATA NITERM,URF/1,0.3,0.5,0.3,0.3,0.3,0.3,0.1,7*0.3/    !initial situation
c      DATA NITERM,URF/1,14*0.3/		!initially URF=0.3
      DATA NITERM,URF/1,14*0.1/		!initially URF=0.3
      DATA ISTEP,LASTEP/0,500000/
      DATA ICCOUNT/0/

C-----GRID-RELATED INDECES
      DATA INGEOM/2/			!1-channel, 2- pipe
      DATA NY/251/
      DATA DELTA,XLAST,ANGLE,RI/1.D0,40.,0.,0./	!delta=pipe radius or channel half-height
      DATA DYIDX,DYEDX,DDELDX/3*0.0/
      DATA XU,XD,DX/2*0.,1.0E30/

C-----PHYSICAL PROPERTIES
C      DATA VISCOS,DENSIT,PRLAM/1.82E-05,1.29,14*1.0/		!through input file
      DATA PRPTAU/1.0,13*0.0/
C      DATA H, CTE/1.0,1.0/
C-----TURBULENCE MODEL CONSTANTS
      DATA CD1,CD2,AK,EWALL,CMU/1.45,1.90,0.41,7.76,0.09/	!FTP-Newtonian (Nagano & Shimada) and FENE-P
c      DATA CD1,CD2,AK,CMU/1.45,1.90,0.4068,0.09/		!FTP-shear-thinning
      DATA CK,CE,C1,C2,C1P,C2P/0.22,0.180,1.8,0.6,0.50,0.3/

C-----VISCOELASTIC TURBULENCE MODEL PARAMETERS (ALFA AND BETA)
c   The next lines have the coefficients for the NLT closure of Resende
c      DATA CALFA0,CALFA1A,CALFA1B,CALFA2,CALFA3A,CALFA3B,CALFA4,CALFA5,		!Resende's closure of NLT
c     +  CGAMA1,CGAMA2,CEXP3,CALFA14						!Resende's closure of NLT
c     + /0.D0,0.85D-2,0.D0,1.9D-1,3.8D-2,0.8D0,0.D0,0.D0,3.8D-2,0.D0,0.0D0,		!Resende's closure of NLT
c     +  0.0D0/		!CALFA0=0 (epsilon outside NLT); CALFA0=1 (epsilon inside NLT)	!Resende's closure of NLT
Ccc     + /0.D0,0.85D-2,0.D0,1.9D-1,3.8D-2,0.8D0,0.D0,0.D0,3.8D-2,0.D0, 		!Resende's closure of NLT
Ccc     +  0.0D0,0.0D0/		!CALFA0=0 (epsilon outside NLT); CALFA0=1 (epsilon inside NLT)		!Resende's closure of NLT
c   End of coefficients for the NLT closure of Resende

c   The next lines have the coefficients for the NLT closure of Pinho
      DATA CALFA0,CALFA1A,CALFA1B,CALFA2,CALFA3A,CALFA3B,CALFA4,CALFA5,		!Pinho's closure of NLT
     +  CGAMA1,CGAMA2,CEXP3,CALFA14						!Pinho's closure of NLT
     + /0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D-1,0.D0,26.64D-5,		!Pinho's closure of NLT
     +  1.5D-4/		!CALFA0=0 (epsilon outside NLT); CALFA0=1 (epsilon inside NLT)	!Pinho's closure of NLT
Ccc     + /0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,3.D0,0.D0,4.0D-4,  or 29.14D-4 for Nagano&Shimada	!Pinho's closure of NLT
Ccc     +  1.5D-4/		!CALFA0=0 (epsilon outside NLT); CALFA0=1 (epsilon inside NLT)		!Pinho's closure of NLT
c   End of coefficients for the NLT closure of Pinho

      DATA CBETA0,CBETA1,CBETA2,CBETA3,CBETA4,CBETA5,CBETA6,CBETA7
     + /0.D0,0.65D0,0.D0,0.D0,0.D0,0.D0,0.D0,1.8D-1/		!CBETA0 = zero
c     + /1.D0,0.65D0,0.D0,0.D0,0.D0,0.D0,0.D0,1.8D-1/		!Optimized for Nagano & Hishida + Pinho
c     + /1.D0,1.3D0,0.D0,0.D0,0.D0,0.D0,0.D0,3.7D-1/		!Original values for DNS
 
C-----MODEL = 1 FOR K-EPSILON; = 2 FOR RSM
      DATA MODEL/1/

C-----GENERAL DATA
      DATA SMALL,GREAT,PI,SMALL1/1.E-30,1.D30,3.141592654D0,1.D-30/

      END

      PROGRAM MAIN
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'

C     This code originates from express2_081 code for the turbulence model
c     with the Toy constitutive model.
c     The constitutive equation used here is the FENE-P model and this 
c     turbulence model is based on the k-epsilon closure.
c     
c     In terms of  closure of viscoelastic terms, this is the first model proposed 
c     in the report dated 28th November 2005. 
c     
c     In terms of the Reynolds stresses, we implement here the low Reynolds turbulence model
c     of Nagano and Hishida (1987)
c
c     In this version of the code (3_031g), the turbulence model for the viscoelastic terms
c     uses as scales of velocity and space the friction velocity and
c     the zero shear rate kinematic viscosity combined with the frction 
c     velocity.
c
c     Also, in the equation of k, the viscoelastic stress power has been
c     put into the Sp term rather than in the Su term as in the version Su
c
c     IN THIS VERSION (No PD) we do not care about the limiting behaviour, 
c     both the polymer diffusion and the polymer dissipation were eliminated
c
c     Version 3_032 introduces modifications to the Newtonian model, namely the
c     improvements suggested by Nagano and Shimada (1993) of increasing Sigmak from 1.0
c     to 1.1, but also the variable Sigmak*= Sigmak/ft and SigmaEpsilon*=SigmaEpsilon/ft
c     with the function ft given by Park and Sung (1995) to provide a variable turbulent
c     diffusion
c
c     This version, express3_32SpNoPD_Res_NagHishPark implements as model of NLT the closure
c     proposed by Pedro Resende, as closure for CUijk that proposed by FPinho. The base model
c     is Nagano and Hishida with variable turbulent diffusion, as recommended by Nagano and
c     Shimada but with the ft function of Park and Sung. See above version 3_032 implementing
c     F Pinho's closure for NLT.
c     
c     F Pinho closure is in subroutine ELASTURB2 and ELASTURB, whereas the closure of 
c     Pedro Resende is in ELASTURB3. Note that a different line of data for the CAlfa
c     coefficients needs to be used (uncomment and comment my line)
     
      OPEN(5,FILE='INPUT300.DAT')
      REWIND(5)
      READ(5,*) DENSIT,ALAMB,AL2		!rho,lambda,L**2
      READ(5,*) PRLAM
      READ(5,*) VISS,VISP			!Visc solvent, Visc polymer
c      READ(5,*) UREF
      REWIND(5)
      CLOSE(5) 
c       The 3 Line Below were introduced to approximate the solution to  a newtonian Fluid      
      ALAMB= 0.01
      AL2= 0.01
      VISP= 0.000001
c       Input de UREF
        UREF= 0.035   ! set de reference velocity
        H= 0.001 ! average roughness height
        F= 4* 10**2       ! Solvent Viscosity -- Water 20C ----10**-3
        CTE= 2
        DENSIT= 0.99802 * 10**3  ! kg/m3 -- Water 20C
      Write(*,*) 'DENSIT , VISS , Calc',DENSIT, VISS, 
     +  EXP( -0.0001 * UREF)
      VISCOS=VISS+VISP			!FTP- zero shear rate viscosity
      
      OPEN(7,FILE='OUNoD.TXT')
      OPEN(8,FILE='OUnorNoD.TXT')
 
      IF(MODEL.EQ.1) THEN
       PRTUR(INU)=1.
c       PRTUR(INK)=1.0
       PRTUR(INK)=1.1		!Nagano & Shimada (1993)
       PRTUR(INE)=1.3
       CD1=1.45
       CD2=1.90
       CMU=0.09
       PRPTAU(INU)=1.
c       PRPTAU(INE)=1.
c       PRPTAU(INK)=1.		!If Cbeta=0 switch this on for CUijkto have only elastic contribution
      ELSE
       PRTUR(INK)=1./CK
       PRTUR(INU2)=1./CK
       PRTUR(INV2)=1./CK
       PRTUR(INW2)=1./CK
       PRTUR(INUV)=1./CK
       PRTUR(INE)=1./CE
       PRTUR(INU)=1.E30
      ENDIF

      TH=2./3.

      CALL GRID
      CALL INIT

C  **** START INTEGRATION LOOP ******

      DO 999 III=1,LASTEP

      CALL VARMOD

      ISTEP=ISTEP+1

C     PERFORM IN-STEP ITERATIONS
 
      DO 100 II=1,NITERM

      CALL WF
      CALL PRELIM

      CALL CALCU
      CALL PBCORR

      IF(MODEL.EQ.1) THEN
        CALL CALCK
        CALL CALCE
        GO TO 100
      ENDIF

      CALL CALCK
      CALL CALCU2
      CALL CALCW2
      CALL CALCUV
      CALL CALCV2
      CALL CALCE

  100 CONTINUE

      IF(ISTEP .LT. 10000) THEN
        IF(MOD(ISTEP,1000).EQ.0) THEN
          WRITE(*,*) 'ISTEP= ',ISTEP,'DPDXB= ',DPDXB,' ERR = ',
     1     (DPREF-DPDXB)/DPDXB,' XMUTP12 = ',XMUTP12(48),' VC22 =',
     1     VC22(48),'TLN11 =',TLN11(48),'TLN22 =',TLN22(48),
     1     'TLN33 =',TLN33(48),'TLN12 =',TLN12(48)
          WRITE(7,*) 'ISTEP= ',ISTEP,'DPDXB= ',DPDXB,' ERR = ',
     1     (DPREF-DPDXB)/DPDXB,' XMUTP12 = ',XMUTP12(48),' VC22 =',
     1     VC22(48),'TLN11 =',TLN11(48),'TLN22 =',TLN22(48),
     1     'TLN33 =',TLN33(48),'TLN12 =',TLN12(48)
          IF (DABS((DPREF-DPDXB)/DPDXB) .LT. 1.E-10) GOTO 1001	!used 5.0E-11 previously !DOAC- for controlling interative convergence
        ENDIF
      ELSE
        IF(MOD(ISTEP,500).EQ.0) THEN
          WRITE(*,*) 'ISTEP= ',ISTEP,'DPDXB= ',DPDXB,' ERR = ',
     1     (DPREF-DPDXB)/DPDXB,' XMUTP12 = ',XMUTP12(48),' VC22 =',
     1     VC22(48),'TLN11 =',TLN11(48),'TLN22 =',TLN22(48),
     1     'TLN33 =',TLN33(48),'TLN12 =',TLN12(48)
          WRITE(7,*) 'ISTEP= ',ISTEP,'DPDXB= ',DPDXB,' ERR = ',
     1     (DPREF-DPDXB)/DPDXB,' XMUTP12 = ',XMUTP12(48),' VC22 =',
     1     VC22(48),'TLN11 =',TLN11(48),'TLN22 =',TLN22(48),
     1     'TLN33 =',TLN33(48),'TLN12 =',TLN12(48)
          IF (DABS((DPREF-DPDXB)/DPDXB) .LT. 1.E-8) GOTO 1001	!used 5.0E-11 previously !DOAC- for controlling interative convergence
        ENDIF
      END IF
       DPREF=DPDXB							!DOAC- for controlling interative convergence
  999 CONTINUE

 1001 CALL OUTPUT
      STOP
      END

      SUBROUTINE  SETUP
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'
C
      ENTRY GRID
C********* SPECIFY ETA ******
      NYM1=NY-1
      NYM2=NY-2
      NYCL=(NY+1)/2
      NYUX=NYCL

C     SPECIFY ETA USING A GEOMETRIC EXPANSION SERIES AND REFLECT
C     ABOUT CHANNEL CENTRELINE TO OBTAIN SYMMETRIC GRID
c     THIS IS FOR A LOW REYNOLDS NUMBER MODEL, GOING STRAIGHT TO THE WALL
c        EPSY=1.15
c      EPSY=1.0724
c      EPSY=1.0356
c       EPSY=1.1
c       EPSY=1.049
       EPSY=1.13       !Era esse aqui ativado
      DY=DELTA*(EPSY-1.)/(EPSY**(NYCL-1-1)-1.)
      ETA(1)=0.0
      DO 5 J=2,NYCL
      ETA(J)=ETA(J-1)+DY
    5 DY=EPSY*DY


C    Now it reflects on the other side of the pipe
      DO 9 J=1,NYCL-1
      ETA(NYCL+J)=2.*ETA(NYCL)-ETA(NYCL-J)
    9 CONTINUE
c

c   Normalizes just in case ETA(NY) different from 2*DELTA. Avoids trouble.
      DD=ETA(NY)
      DO 7 J=1,NY
    7 ETA(J)=ETA(J)/DD
c

      DO 10 J=1,NY
   10 Y(J)=ETA(J)*2.*DELTA

C     CALCULATE SOME GEOMETRIC QUANTITIES

      DO 20 J=2,NY
   20 YDIF(J)=Y(J)-Y(J-1)
      DO 30 J=2,NYM1
   30 YS(J)=0.5*(Y(J+1)-Y(J-1))
      YS(2)=0.5*(Y(2)+Y(3))
      YS(NYM1)=Y(NY)-0.5*(Y(NYM1)+Y(NYM2))
      DO 40 J=2,NY
   40 YV(J)=0.5*(Y(J)+Y(J-1))
      YV(2)=Y(1)
      YV(NY)=Y(NY)
      DO 50 J=2,NYM1
   50 YSV(J)=YDIF(J)
      DO 60 J=1,NY
      IF(INGEOM.EQ.1) R(J)=1.0
      IF(INGEOM.EQ.2) R(J)=DABS(DELTA-Y(J))
c      IF(INGEOM.EQ.2) R(J)=DELTA-Y(J)         !In original code of Bassam (does not converge)
      RU(J)=R(J)
   60 CONTINUE
      DO 74 J=2,NY
   74 RV(J)=0.5*(R(J)+R(J-1))
      RV(2)=R(1)
      RV(NY)=R(NY)
   80 CONTINUE
      DO 120 J=2,NYM1
  120 YSR(J)=0.5*(RV(J)+RV(J+1))*YS(J)
      DO 130 J=2,NYM1
  130 YSVR(J)=RV(J)*YSV(J)
      RETURN

      ENTRY INIT

c   In this low Reynolds number code this initialises almost all quantities
c    even though some are not yet necessary. It just helps debugging. 
      
      DIA=2.*DELTA
      RE=UREF*DIA*DENSIT/VISCOS
      TAUW=DENSIT*UREF**2.*0.0225*(1./RE)**0.25	!Newtonian expression (Blasius)
c      TAUW= DENSIT*UREF**2.*0.0225*(1./RE)**0.25 + (0.07*10)
     

      TAUWB=TAUW
      TAUWT=TAUW
c      UT=DSQRT(TAUW/DENSIT)		!friction velocity
      UT= DSQRT(TAUW/DENSIT) 
      USB=UT		!FTP Needed in the first iteration
      UST=UT		!FTP Needed in the first iteration


      DO 36 J=1,NY  
       V(J)=0.
       P(J)=0.
       DPDX(J)=0.
       RHO(J)=DENSIT
       FMU(J)=1.0	!FTP-to avoid problems
       FMUNLT(J)=0.0
       FF1(J)=0.0
       FMUCU(J) =0.0
   36 CONTINUE
c       FMU(1)=0.0	!FTP-true value at wall 
c       FMU(NY)=0.0	!FTP-true value at wall 
c       FMUNLT(1)= 0.
c       FMUNLT(NY)= 0.
c       FF1(1)=0.0
c       FF1(NY)=0.0
c       FMUCU(1)=0.0
c       FMUCU(NY)=0.0
c       WRITE(*,*) 'Re=',RE,' UT=',UT	!For debugging
c       WRITE(*,*) 'L**2=', AL2		!For debugging
       

      DO 15 J=1,NYCL
       VISWAL=VISCOS		!FTP- Just initializing with zero shear rate viscosity
       XMUL(J)=VISS		!FTP- XMUL- solvent viscosity
       XMULM(J)=VISCOS		!FTP- XMULM total molecular viscosity (the polymer contribution in shear-thinning)
       YPLS(J)=DENSIT*UT*Y(J)/VISWAL			!FTP modified

       IF(YPLS(J) .LE. 11.63) U(J)=UT*YPLS(J)
       IF(YPLS(J) .GT. 11.63) U(J)=UT/AK*DLOG(EWALL*YPLS(J))
C       U(J)=1.5*UREF*Y(J)*(2*delta-Y(J))/delta/delta	!laminar profile
C       DUDY(J)= 3.*UREF/delta/delta*(delta-Y(J)) 	!we use also the laminar velocity profile
       IF(YPLS(J) .LE. 11.63) DUDY(J)=UT*UT*DENSIT/VISWAL
       IF(YPLS(J) .GT. 11.63) DUDY(J)=UT/AK/Y(J)

      AL=DMIN1(AK*Y(J),.085*DELTA)
      UV(J)=-(UT**2.)*(1-1.9999*ETA(J))
      IF(YPLS(J).LE.20)  UV(J)=-0.98*(UT**2.)*YPLS(J)/20.
      TK(J)=DABS(UV(J))/0.3
      RK=AL*DSQRT(TK(J))*DENSIT/VISWAL
      EMT=0.548*AL*DSQRT(TK(J))*(1.-DEXP(-0.029*RK))
c      TE(J) = UT**4. /(XMUT(J) * 0.0000000000003)
      TE(J)=(0.164/0.548*EMT+0.336*VISWAL/DENSIT)*TK(J)/(AL**2.+SMALL1)
      U2(J)=TK(J)
      V2(J)=0.4*TK(J)
      W2(J)=0.6*TK(J)
      DPDXB=0.0		!Or DPDXB below
c      DPDXB=-TAUW/DIA*2.D0**INGEOM
      XMUT(J)= !RHO(J)*CMU*TK(J)*TK(J)/(TE(J)+SMALL1) 
     + +UREF* (0.0166) * (4.2446* 10**(-0.68390827*LOG10(0.0166)))
c     Initialising quantities related to viscoelastic model
c     CONFORMATION tensor asssuming laminar conformation solution
       C22(J)=0.0
       C11(J)=0.0
       C33(J)=C22(J)
       fCmm(J)=1.0D0
       C12(J)=0.0
       C31(J)=0.0
       C32(J)=0.0
       fL=1.D0					!f(L)- formulation used by Suresh
C    END OF INITIALISATION OF CONFORMATION TENSOR

c    INITIALISING OTHER VISCOELASTIC QUANTITIES (NLTij, CUij AND TPij)
       TLN11(J)=0.0
       TLN22(J)=0.0
       TLN33(J)=0.0
       TLN12(J)=0.0
       CU112(J)=0.0				!initialized as 0, for the moment
       CU122(J)=0.0				!initialized as 0, for the moment
       CU222(J)=0.0				!initialized as 0, for the moment
       CU332(J)=0.0				!initialized as 0, for the moment
       VC22(J)=CU222(J)/2.				!by definition
       UC22(J)=0.0				!initialized as 0, for the moment
       TP12(J)=0.0
       TP11(J)=0.0
       TP22(J)=0.0
       TP33(J)=0.0
       XMUTP12(J)=0.0	!definition	
       TEEV(J)=0.0
C       CALL COUETTE(J)
c    END OF INITIALISATION OF OTHER VISCOELASTIC QUANTITIES (NLTij AND CUij)
C    END OF INITIALISATION OF VISCOELASTIC QUANTITIES

   15 CONTINUE


C     REFLECT PROFILES ABOUT CHANNEL CENTRELINE
c     SOME QUANTITIES WERE REFLECTED BECAUSE WE NEED THEM TO THE INITIAL OUTPUT (DEBUGGING)

      DO 25 J=1,NYCL-1
       YPLS(NYCL+J)=YPLS(NYCL-J)
       U(NYCL+J)=U(NYCL-J)
       DUDY(NYCL+J)=-DUDY(NYCL-J)
       UV(NYCL+J)=-UV(NYCL-J)		!Reynolds shear stress is anti-symmetric
       TK(NYCL+J)=TK(NYCL-J)
       TE(NYCL+J)=TE(NYCL-J)
       U2(NYCL+J)=U2(NYCL-J)
       V2(NYCL+J)=V2(NYCL-J)
       W2(NYCL+J)=W2(NYCL-J)
       XMUT(NYCL+J)=XMUT(NYCL-J)
c
       XMUL(NYCL+J)=XMUL(NYCL-J)
       XMULM(NYCL+J)=XMULM(NYCL-J)
       XMUTP12(NYCL+J)=XMUTP12(NYCL-J)
C
C    VISCOELASTIC QUANTITIES
       TP12(NYCL+J)=-TP12(NYCL-J)	!The polymer shear stress is  anti-symmetric
       TP11(NYCL+J)= TP11(NYCL-J)	!The polymer normal stresses are symmetric
       TP22(NYCL+J)= TP22(NYCL-J)
       TP33(NYCL+J)= TP33(NYCL-J)
       C11(NYCL+J)=C11(NYCL-J)
       C12(NYCL+J)=-C12(NYCL-J)		!The shear polymer configuration is anti-symmetric
       C22(NYCL+J)=C22(NYCL-J)
       C33(NYCL+J)=C33(NYCL-J)
       C31(NYCL+J)=C31(NYCL-J)		!Czx must not change sign
       C32(NYCL+J)=-C32(NYCL-J)		!Czy must change sign
       TLN11(NYCL+J)= TLN11(NYCL-J)
       TLN22(NYCL+J)= TLN22(NYCL-J)
       TLN33(NYCL+J)= TLN33(NYCL-J)
       TLN12(NYCL+J)=-TLN12(NYCL-J)	!NLTxy is also anti-symmetric
       CU112(NYCL+J)=-CU112(NYCL-J)	!CUxxy changes sign (anti-symmetric)
       CU122(NYCL+J)= CU122(NYCL-J)
       CU222(NYCL+J)=-CU222(NYCL-J)	!CUyyy changes sign (anti-symmetric)
       CU332(NYCL+J)=-CU332(NYCL-J)	!CUzzy changes sign (anti-symmetric)
        VC22(NYCL+J)= -VC22(NYCL-J)	!vcyy changes sign (anti-symmetric)
	UC22(NYCL+J)=  UC22(NYCL-J)
        TEEV(NYCL+J)= TEEV(NYCL-J)
C    END OF VISCOELASTIC QUANTITIES

   25 CONTINUE

c   CORRECTING SOME BOUNDARY CONDITIONS
      U(1)= 0.	!FTP-Wall value for k-eps & RSM (high and low Re)
      U2(1)= 0.
      V2(1)=0.
      W2(1)=0.
      UV(1)=0.		
      TK(1)= 0.		!FTP-Wall value for k-eps & RSM (high and low Re)
c      XMUT(1)= 0.      !era 0. RHO(J)*CMU*TK(J)*TK(J)/(TE(J)+SMALL1)  
      U(NY)=  0.		!FTP-Wall value for k-eps & RSM (high and low Re)
      U2(NY)= 0.
      V2(NY)=0.
      W2(NY)=0.
      UV(NY)=0.		!FTP-Wall value for k-eps & RSM (high and low Re)
      TK(NY)= 0.			!FTP-Wall value for k-eps & RSM (high and low Re)
c      XMUT(NY)= 0. !era 0. RHO(J)*CMU*TK(J)*TK(J)/(TE(J)+SMALL1) 
c      TE(1)=TE(2)	!FTP-wall condition of eps for high Re (standard model)
c      TE(NY)=TE(NYM1)	!FTP-wall condition of eps for high Re (standard model)
      TE(1)= 0.		!FTP-eps-tilde=0 at wall (low Re number model)
      TE(NY)= 0.		!FTP-eps-tilde=0 at wall (low Re number model)
      DO 122 J=1,NYM1
      TP12S(J)=0.5*(TP12(J)+TP12(J+1))	!FTP-elastic stress at north cell face
  122 UVS(J)=0.5*(UV(J)+UV(J+1))
      UVS(1)=0.
      UVS(NY)=0.
c   CHECK WHETHER TP12S IS NEEDED (I guess only in the context of a Reynolds stress model, not k-epsilon)
      TP12S(NY)=TP12(NY)	!FTP- inside the upper wall is equal to wall stress 

C    NOTE THAT ALL THE VISCOELASTIC TLNij and CUijk were initialised at the wall, above.
      
      DDELDX=DYEDX-DYIDX	!CORRECT-What the hell is this ??

C     CALCULATE TRUE MASS FLUX 
      SAREA=0.0
      SMASS=0.0
      SROAR=0.
      DO 100 J=1,NY		!FTP-low Re. It was J=2,NYM1 for high Reynolds
      SAREA=SAREA+YSR(J)
      ROAR=RHO(J)*YSR(J)
      SROAR=SROAR+ROAR
  100 SMASS=SMASS+RHO(J)*YSR(J)*U(J)

      UBI=SMASS/SROAR
      FAC=UREF/UBI
      DO 9876 J=1,NY		!FTP-low Re. It was J=2,NYM1 for high Reynolds
 9876 U(J)=U(J)*FAC
      SMASSI=SMASS*FAC		!True mass flux acounting for effect of Uwall=0 mass flux

c  Here we output the initial values for control
      WRITE(7,*) 'Output of initial values'
      WRITE(7,*)'ISTEP=',ISTEP,'DPDXB=',DPDXB,'DPREF=',DPREF
      WRITE(7,*) 'FAC=',FAC
      WRITE(7,213)
      WRITE(7,214)(J,ETA(J),U(J),DUDY(J),TK(J),TP12(J),TP11(J),
     +   TP22(J),TP33(J),YPLS(J),UPLS(J),XMUL(J),XMULM(J),J=1,NY)
      WRITE(7,*)
      WRITE(7,215)
      WRITE(7,214)(J,C11(J),C22(J),C33(J),C12(J),C31(J),fCmm(J),
     +   CU112(J),CU122(J),CU222(J),CU332(J),UC22(J),VC22(J),J=1,NY)
      WRITE(7,*)
      WRITE(7,216)
      WRITE(7,217)(J,TLN11(J),TLN22(J),TLN33(J),TLN12(J),J=1,NY)
      WRITE(7,*)			
c  End of initial value output
C      PAUSE

  213 FORMAT(//6X,10H     Y/Y.5,10H      U   ,10H    DUDY  ,
     +12H        TK  ,12H        TP12,12H        TP11,12H        TP22,
     +10H      TP33,11H         Y+,11H         U+,
     +11H       XMUL,10H     XMULM)

  215 FORMAT(//6X,10H       C11,10H       C22,10H      C33 ,
     +12H         C12,12H         C31,12H        fCmm,11H      CU112,
     +10H     CU122,11H      CU222,11H      CU332,
     +11H       UC22,10H      VC22)

  216 FORMAT(//6X,10H     TLN11,10H     TLN22,10H    TLN33 ,
     +12H       TLN12)

  214  FORMAT(1X,I3,2X,1P12E11.3)
  217  FORMAT(1X,I3,2X,1P4E11.3)

      RETURN
      END

      SUBROUTINE UPDATE 
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'

      ENTRY VARMOD

C     SAVE UPSTREAM QUANTITIES
      DO 10 J=1,NY
       UU(J)=U(J)
       TKU(J)=TK(J)
       TEU(J)=TE(J)
       UVSU(J)=UVS(J)	!only quantities calculated in the Solver require this
       TP12U(J)=TP12(J)	!FTP-new elastic stress. Required in a Reynolds stress formulation with a transport equation on TP12
       U2U(J)=U2(J)
       V2U(J)=V2(J)
       W2U(J)=W2(J)
       UVU(J)=UV(J)
   10 CONTINUE

      DO 20 J=1,NY
       RU(J)=R(J)
       YSRU(J)=YSR(J)
       YSVRU(J)=YSVR(J)
       GY(J)=0.0
   20  GX(J)=RHO(J)*U(J)
       DELTAU=DELTA
       XU=XD
      RETURN

      ENTRY PBCORR
C     CORRECT DPDX AND U FOR CONFINED FLOW
 
      SAREA=0.0
      SROAR=0.0
      SMASS=0.0
      SDURA=0.0
 
      DO 300 J=1,NY			!FTP-low Re. It was J=2,NYM1
       ROAR=RHO(J)*YSR(J)
       SAREA=SAREA+YSR(J)
       SROAR=SROAR+ROAR
       SMASS=SMASS+RHO(J)*YSR(J)*U(J)
  300  SDURA=SDURA+DU(J)*RHO(J)*YSR(J)
 
      UB=SMASS/SROAR
      DPDXP=(SMASSI-SMASS)/SDURA
        DPDXB=DPDXB+DPDXP

      DO 310 J=1,NY			!FTP-low Re. It was J=2,NYM1	
      DPDX(J)=DPDXB
      U(J)=U(J)+DU(J)*DPDXP
  310 U(J)=DMAX1(U(J),0.0D0)
      U(1)= 0.	!FTP. Yes, because it just set U(1)­0 which it can't
      U(NY)= 0.!FTP. Yes, because it just set U(NY)­0 which it can't
      RETURN

      END

      SUBROUTINE PREP
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'

      ENTRY WF

      IF(ISTEP.NE.1) THEN
c       TAUW=DPDX(2)*DIA/(2.**INGEOM)		!DPDX is constant so any point is OK
       IF (ISTEP .LE. 1000) THEN
         TAUWB=(VISCOS*DUDY(1)) !+  (DENSIT * 0.07**(1/3)) 
	 TAUWT=(VISCOS*DUDY(NY)) !+ (DENSIT * 0.07**(1/3))
c         TAUWB=(VISCOS*DUDY(1)) +  (DENSIT*0.1 * 0.07**(1/3)) 
c	 TAUWT=(VISCOS*DUDY(NY)) + (DENSIT * 0.1* 0.07**(1/3))

       ELSE
        TAUWB=(XMULM(1)*DUDY(1)) !+ (DENSIT * 0.07**(1/3)) 
	 TAUWT=(XMULM(NY)*DUDY(NY)) !+ (DENSIT * 0.07**(1/3)) 
c        TAUWB=(XMULM(1)*DUDY(1)) + (DENSIT * 0.1* 0.07**(1/3)) 
c	 TAUWT=(XMULM(NY)*DUDY(NY)) + (DENSIT * 0.1* 0.07**(1/3)) 
     
       END IF
C       TAUWB=TAUW
C       TAUWT=TAUW
        USB=DSQRT(DABS(TAUWB)/DENSIT)
        UST=DSQRT(DABS(TAUWT)/DENSIT)
c        WRITE(*,*) 'tauwb', TAWB  

      END IF 
      VISWALB=XMUTP12(1)+VISS     !FTP- True wall viscosity (OR TAUW/DUDY)
      VISWALT=XMUTP12(NY)+VISS    !FTP- True wall viscosity (OR TAUW/DUDY)
c      VISWALB=DABS(TAUWB/DUDY(1))     !FTP- True wall viscosity 
c      VISWALT=DABS(TAUWT/DUDY(NY))    !FTP- True wall viscosity
      
      DO 203 J=2,NYCL
       YPLS(J)=RHO(J)*Y(J)*USB/VISWALB
  203 CONTINUE
       YPLS(1)=0.
       
      DO 204 J=NYCL+1,NYM1
       YPLS(J)=RHO(J)*(Y(NY)-Y(J))*UST/VISWALT
  204 CONTINUE
       YPLS(NY)=0.

      RETURN

      ENTRY PRELIM

      DO 122 J=2,NYM1
C   calculating ¶U/¶y, the first derivative of velocity
       GN=(U(J+1)-U(J))/(Y(J+1)-Y(J))
       GS=(U(J)-U(J-1))/(Y(J)-Y(J-1))
       RATIO=(Y(J)-(5.D-1*(Y(J-1)+Y(J))))/(5.D-1*(Y(J+1)-Y(J-1)))
       DUDY(J)=GS+RATIO*(GN-GS)
C

C   Now we calculate the first derivate of DSQRT(TK(J)). Same philosophy
C   as for the first derivative of velocity
       GNSQRTK=(DSQRT(TK(J+1))-DSQRT(TK(J)))/(Y(J+1)-Y(J))
       GSSQRTK=(DSQRT(TK(J))-DSQRT(TK(J-1)))/(Y(J)-Y(J-1))
       DSQTKDY(J)=GSSQRTK+RATIO*(GNSQRTK-GSSQRTK)

C       EPSN=(TE(J+1)-TE(J))/(Y(J+1)-Y(J))	!FTP: depsilontilde/dy
C       EPSS=(TE(J)-TE(J-1))/(Y(J)-Y(J-1))	!FTP: depsilontilde/dy
C       DEPSDY(J)=EPSS+RATIO*(EPSN-EPSS)		!FTP: depsilontilde/dy

C   THE QUANTITIES BELOW ARE REQUIRED BY THE NEW VISCOELASTIC MODEL
       DC11N=(C11(J+1)-C11(J))/(Y(J+1)-Y(J))	!FTP: dC11/dy
       DC11S=(C11(J)-C11(J-1))/(Y(J)-Y(J-1))	!FTP: dC11/dy
       DC11DY(J)=DC11S+RATIO*(DC11N-DC11S)	!FTP: dC11/dy

       DC12N=(C12(J+1)-C12(J))/(Y(J+1)-Y(J))	!FTP: dC12/dy
       DC12S=(C12(J)-C12(J-1))/(Y(J)-Y(J-1))	!FTP: dC12/dy
       DC12DY(J)=DC12S+RATIO*(DC12N-DC12S)	!FTP: dC12/dy

       DC22N=(C22(J+1)-C22(J))/(Y(J+1)-Y(J))	!FTP: dC22/dy
       DC22S=(C22(J)-C22(J-1))/(Y(J)-Y(J-1))	!FTP: dC22/dy
       DC22DY(J)=DC22S+RATIO*(DC22N-DC22S)	!FTP: dC22/dy

       DC33N=(C33(J+1)-C33(J))/(Y(J+1)-Y(J))	!FTP: dC33/dy
       DC33S=(C33(J)-C33(J-1))/(Y(J)-Y(J-1))	!FTP: dC33/dy
       DC33DY(J)=DC33S+RATIO*(DC33N-DC33S)	!FTP: dC33/dy

       DU2N=(U2(J+1)-U2(J))/(Y(J+1)-Y(J))	!FTP: du2/dy
       DU2S=(U2(J)-U2(J-1))/(Y(J)-Y(J-1))	!FTP: du2/dy
       DU2DY(J)=DU2S+RATIO*(DU2N-DU2S)		!FTP: du2/dy
       
       DV2N=(V2(J+1)-V2(J))/(Y(J+1)-Y(J))	!FTP: dv2/dy
       DV2S=(V2(J)-V2(J-1))/(Y(J)-Y(J-1))	!FTP: dv2/dy
       DV2DY(J)=DV2S+RATIO*(DV2N-DV2S)		!FTP: dv2/dy
       
       DW2N=(W2(J+1)-W2(J))/(Y(J+1)-Y(J))	!FTP: dw2/dy
       DW2S=(W2(J)-W2(J-1))/(Y(J)-Y(J-1))	!FTP: dw2/dy
       DW2DY(J)=DW2S+RATIO*(DW2N-DW2S)		!FTP: dw2/dy

       DUVN=(UV(J+1)-UV(J))/(Y(J+1)-Y(J))	!FTP: duv/dy
       DUVS=(UV(J)-UV(J-1))/(Y(J)-Y(J-1))	!FTP: duv/dy
       DUVDY(J)=DUVS+RATIO*(DUVN-DUVS)		!FTP: duv/dy

C   FCUii=fCmm*(CUxxy+CUyyy+CUzzy), so below its derivative is calculated

       FCUiiN=(fCmm(J+1)*(CU112(J+1)+CU222(J+1)+CU332(J+1))-
     +         fCmm(J)*(CU112(J)+CU222(J)+CU332(J)))/(Y(J+1)-Y(J))
       FCUiiS=(fCmm(J)*(CU112(J)+CU222(J)+CU332(J))-
     +         fCmm(J-1)*(CU112(J-1)+CU222(J-1)+CU332(J-1)))/
     +        (Y(J)-Y(J-1))					!FTP: d(f(Cmm)*CUiiy)/dy
       DFCUDY(J)=FCUiiS+RATIO*(FCUiiN-FCUiiS)			!FTP: d(f(Cmm)*CUiiy)/dy
C    END OF QUANTITIES REQUIRED BY VISCOELASTIC MODEL      
  122 CONTINUE

      DUDY(1)=(U(2)-U(1))/(Y(2)-Y(1))
      DUDY(NY)=(U(NY)-U(NYM1))/(Y(NY)-Y(NYM1))     

      DSQTKDY(1)=(DSQRT(TK(2))-DSQRT(TK(1)))/(Y(2)-Y(1))		!FTP
      DSQTKDY(NY)=(DSQRT(TK(NY))-DSQRT(TK(NYM1)))/(Y(NY)-Y(NYM1))	!FTP

C      DEPSDY(1)=(TE(2)-TE(1))/(Y(2)-Y(1))			
C      DEPSDY(NY)=(TE(NY)-TE(NYM1))/(Y(NY)-Y(NYM1))		
     
      DC11DY(1)=(C11(2)-C11(1))/(Y(2)-Y(1))
      DC11DY(NY)=(C11(NY)-C11(NYM1))/(Y(NY)-Y(NYM1))

      DC12DY(1)=(C12(2)-C12(1))/(Y(2)-Y(1))
      DC12DY(NY)=(C12(NY)-C12(NYM1))/(Y(NY)-Y(NYM1))

      DC22DY(1)=(C22(2)-C22(1))/(Y(2)-Y(1))
      DC22DY(NY)=(C22(NY)-C22(NYM1))/(Y(NY)-Y(NYM1))
      
      DC33DY(1)=(C33(2)-C33(1))/(Y(2)-Y(1))
      DC33DY(NY)=(C33(NY)-C33(NYM1))/(Y(NY)-Y(NYM1))

      DU2DY(1)=(U2(2)-U2(1))/(Y(2)-Y(1))
      DU2DY(NY)=(U2(NY)-U2(NYM1))/(Y(NY)-Y(NYM1))

      DV2DY(1)=(V2(2)-V2(1))/(Y(2)-Y(1))
      DV2DY(NY)=(V2(NY)-V2(NYM1))/(Y(NY)-Y(NYM1))

      DW2DY(1)=(W2(2)-W2(1))/(Y(2)-Y(1))
      DW2DY(NY)=(W2(NY)-W2(NYM1))/(Y(NY)-Y(NYM1))

      DUVDY(1)=(UV(2)-UV(1))/(Y(2)-Y(1))
      DUVDY(NY)=(UV(NY)-UV(NYM1))/(Y(NY)-Y(NYM1))

      DFCUDY(1)=(fCmm(2)*(CU112(2)+CU222(2)+CU332(2))-
     +           fCmm(1)*(CU112(1)+CU222(1)+CU332(1)))/(Y(2)-Y(1))
      DFCUDY(NY)=(fCmm(NY)*(CU112(NY)+CU222(NY)+CU332(NY))-
     +            fCmm(NYM1)*(CU112(NYM1)+CU222(NYM1)+CU332(NYM1)))/
     +           (Y(NY)-Y(NYM1))

c   The second derivative is calculated as the first derivative, using the
c   first derivative as the input. It may need to be changed in the future.

      DO 125 J=2,NYM1
       G2N=(DUDY(J+1)-DUDY(J))/(Y(J+1)-Y(J))	!FTP
       G2S=(DUDY(J)-DUDY(J-1))/(Y(J)-Y(J-1))	!FTP
       RATIO=(Y(J)-(0.5*(Y(J-1)+Y(J))))/(0.5*(Y(J+1)-Y(J-1)))       
       D2UDY2(J)=G2S+RATIO*(G2N-G2S)		!FTP
  125 CONTINUE
  
C   End of calculation of second derivative.
      
      D2UDY2(1)=(DUDY(2)-DUDY(1))/(Y(2)-Y(1))			!FTP
      D2UDY2(NY)=(DUDY(NY)-DUDY(NYM1))/(Y(NY)-Y(NYM1))		!FTP
      

      DO 121 J=2,NYM1
       TKS(J)=.5*(U2(J)+V2(J)+W2(J))
       XMUT(J)= !RHO(J)*TKS(J)*V2(J)/(TE(J)+SMALL1)
     + + UREF* (0.0166) * (4.2446* 10**(-0.68390827*LOG10(0.0166))) 
  121 CONTINUE

       TKS(1)= 0.
       TKS(NY)= 0.
       XMUT(1)=0.
       XMUT(NY)=0.

      IF(MODEL.EQ.1) THEN


       TAUWB=DENSIT*USB*USB
c        VISWALB=VISS
        VISWALB=DABS(TAUWB/(DUDY(1)+SMALL1))
       TAUWT=DENSIT*UST*UST
c	 VISWALT=VISS
        VISWALT=DABS(TAUWT/(DUDY(NY)+SMALL1))
      TEE(1)=2.D0*XMUL(1)/RHO(1)*DSQTKDY(1)*DSQTKDY(1)		!epsilon=epsilontilde + D   (for low Reynolds)
      TEE(NY)=2.D0*XMUL(NY)/RHO(NY)*DSQTKDY(NY)*DSQTKDY(NY)	!epsilon=epsilontilde + D   (for low Reynolds)

c      YPLSCR=0.D0
c      IF(ISTEP .GE. 12000) YPLSCR=4.

c     Calculation of y+ and FMU
      DO 124 J=2,NYM1
       YPLS(J)=RHO(J)*USB*Y(J)/VISWALB				!FTP based on wall viscosity
       IF(J.GT.NYUX) YPLS(J)=RHO(J)*UST*(Y(NY)-Y(J))/VISWALT 	!FTP based on wall vis and at upper half
       FMU(J)=(1.D0-DEXP(-YPLS(J)/2.65D1))*(1.D0-DEXP(-YPLS(J)/2.65D1)) !Nagano and Hishida's (1987) FMU for low Reynolds (based on Van Driest)
c       FMU(J)=(1.D0-DEXP(-(YPLS(J)-YPLSCR)/2.65D1))*
c     +        (1.D0-DEXP(-(YPLS(J)-YPLSCR)/2.65D1)) !Nagano and Hishida's (1987) FMU for low Reynolds (based on Van Driest)
       AUXRT=RHO(J)*TK(J)*TK(J)/(TE(J)+SMALL1)/VISS
       FT(J)=1.D0+3.5D0*DEXP(-AUXRT*AUXRT/1.5D2/1.5D2)	!Nagano & Shimada (1993) and Park and Sung
c       FT(J)=1.D0
c     End of calculation of y+

      XMUT(J)= !RHO(J)*FMU(J)*CMU*TK(J)*TK(J)/(TE(J)+TEEV(J)+SMALL1)		!FTP FMU for low Reynolds
     + +UREF* (0.0166) * (4.2446* 10**(-0.68390827*LOG10(0.0166))) !   XMUT(J)=RHO(J)*FMU(J)*CMU*TK(J)*TK(J)/(TE(J)+SMALL1)		!FTP FMU for low Reynolds
      TEE(J)=TE(J)+2.D0*XMUL(J)/RHO(J)*DSQTKDY(J)*DSQTKDY(J)	!epsilon=epsilontilde + D   (for low Reynolds)

      UV(J)=-XMUT(J)/RHO(J)*DUDY(J)
      TKS(J)=TK(J)

      XMUL(J)=VISS  			!FTP-Solvent viscosity 
      XMULM(J)=XMUL(J)+XMUTP12(J)

       IF (ISTEP .GE. 10000 .AND. ISTEP .LT. 12000) CALL COUETTE(J)
       IF (ISTEP .GE. 12000 .AND. ALAMB .GT. 1D-9) THEN
c   Here we calculate now the polymer stress and all other viscoelastic
c   quantities using their definitions. 
       XMUTP12OLD=XMUTP12(J)       
c       CALL ELASTURB(J)	!Pinho closure (cubic equation)
      CALL ELASTURB2(J)		!Pinho closure (iterative)
c      CALL ELASTURB3(J)	!Resende closure of NLT (iterative) + Pinho closure of CUijk
c       CALL COUETTE(J) 	!Laminar. No turbulence.
c
cc       IF (XMUTP12(J) .GT. VISP) THEN
cc         XMUTP12(J)=VISP
cc	 TP12(J)=XMUTP12(J)*DUDY(J)
cc       END IF   					!refers to IF (XMUTP12(J) .GT. VISP)    

cc       IF (J.EQ.NYUX) XMUTP12(J)=VISP 	!on axis ¶u¶y=0 hence XMUTP122=VISP
       XMUTP12(J)=5.D-1*XMUTP12OLD+5.D-1*XMUTP12(J)		!Relaxation in polymer viscosity if necessary

       TP12S(J)=5.D-1*(TP12(J)+TP12(J+1))          	!I think this is only relevant for a Reynolds stress model, not k-epsilon
       ELSE						!Refers to first IF (ISTEP .GT. 10000)
        XMUTP12(J)=0.D0
        IF (ALAMB .EQ. 0.) XMUTP12(J)=VISP  		!FTP-Solvent viscosity 
        TP12(J)=XMUTP12(J)*DUDY(J)
       END IF						!Refers to first IF (ISTEP .GT. 10000)
C   End of calculation of elastic stress and other viscoelastic quantities

 124  CONTINUE

      TKS(1)= 0.	!FTP- perhaps irrelevant
      TKS(NY)= 0.	!FTP- perhaps irrelevant
      XMUT(1)=0.D0		!FTP- perhaps irrelevant
      XMUT(NY)=0.D0		!FTP- perhaps irrelevant
      XMUL(1)=VISS		!FTP- IMPORTANT
      XMUL(NY)=VISS		!FTP- IMPORTANT
      UV(1)=0.			!FTP- perhaps irrelevant
      UV(NY)=0.			!FTP- perhaps irrelevant
      FMU(1)=0.D0			!FTP- perhaps irrelevant
      FMU(NY)=0.D0		!FTP- perhaps irrelevant

       IF (ISTEP .GE. 10000 .AND. ISTEP .LT. 12000) CALL COUETTE(1)
       IF (ISTEP .GE. 10000 .AND. ISTEP .LT. 12000) CALL COUETTE(NY)
      IF (ISTEP .GE. 12000 .AND. ALAMB .GT. 1D-9) THEN
C    SETTING THE POLYMER STRESS AT WALL USING LAMINAR COUETTE SOLUTION
c    BOTTOM WALL
c        CALL COUETTE(1)	!Laminar. No turbulence
c        CALL ELASTURB(1)	!Pinho closure (cubic equation)
        CALL ELASTURB2(1)	!Pinho closure (iterative)
c        CALL ELASTURB3(1)	!Resende closure of NLT (iterative) + Pinho closure of CUijk
      
c    TOP WALL
c        CALL COUETTE(NY)	!Laminar. No turbulence
c        CALL ELASTURB(NY)	!Pinho closure (cubic equation)
        CALL ELASTURB2(NY)	!Pinho closure (iterative)
c        CALL ELASTURB3(NY)	!Resende closure of NLT (iterative) + Pinho closure of CUijk
       ELSE		!Refers to second IF (ISTEP .GT. 12000)
        XMUTP12(1)=0.D0
        XMUTP12(NY)=0.D0
         IF (ALAMB .EQ. 0.0) THEN
            XMUTP12(1)=VISP		!FTP- IMPORTANT
            XMUTP12(NY)=VISP		!FTP- IMPORTANT
         ENDIF
         TP12(1)=XMUTP12(1)*DUDY(1)
         TP12(NY)=XMUTP12(NY)*DUDY(NY)
       END IF   	!Refers to second IF (ISTEP .GT. 12000)
	 XMULM(1)=XMUL(1)+XMUTP12(1)		!Required for TAUWB	
	 XMULM(NY)=XMUL(NY)+XMUTP12(NY)		!Required for TAUWT
       
        DO J=1,NY
          U2(J)=TK(J)*2.D0/3.D0
          V2(J)=TK(J)*2.D0/3.D0
          W2(J)=TK(J)*2.D0/3.D0
        END DO
       
      ENDIF		!Refers to model IF (MODEL .EQ. 1)

      DO 123 J=1,NY
      P11(J)=-2.D0*UV(J)*DUDY(J)
      G11(J)=4.D0*UV(J)*OMEGA
      P22(J)=0.D0
      G22(J)=-4.D0*UV(J)*OMEGA
      P12(J)=-V2(J)*DUDY(J)
      G12(J)=-2.D0*(U2(J)-V2(J))*OMEGA
      PK(J)=5.D-1*P11(J)
      EDK(J)=TE(J)/(TKS(J)+SMALL1)
  123 CONTINUE

      IF(MODEL.EQ.1) RETURN

      DO 9876 J=2,NYM1
 9876 F(J)=((DABS(TKS(J)))**1.5/TE(J))*(1./Y(J)+1./(Y(NY)-Y(J)))
      F2=F(2)
      FNYM1=F(NYM1)
      FF=DMAX1(F(2),F(NYM1))
      DO 9877 J=2,NYM1
      F(J)=F(J)/FF
 9877 F(J)=DMIN1(F(J),1.D0)
      RETURN

      END


      SUBROUTINE COUETTE (I)
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'

C    SETTING THE POLYMER STRESS AT WALL USING LAMINAR COUETTE SOLUTION
c    BOTTOM WALL

       fL=1.D0					!Suresh formulation
       BB=2.D0*ALAMB*ALAMB*DUDY(I)*DUDY(I)/AL2	!Suresh formulation
       BALFA=27.D0*BB+4.D0
       BBETA=DSQRT(27.D0*BB)
       BDELTA=BBETA+DSQRT(BALFA)
       C22(I)=432.D0**(1.D0/6.D0)*(BDELTA**(2.D0/3.D0)-2.D0**
     1        (2.D0/3.D0))/(6.D0*DSQRT(BB)*BDELTA**(1.D0/3.D0)+
     1         SMALL1)              
       C11(I)=AL2-C22(I)*(AL2-1.D0)    
       C33(I)=C22(I)		     
C       C12(I)=(AL2-3.D0)/(AL2+SMALL1)*ALAMB*DUDY(I)*C22(I)*C22(I)  !Bird formulation
       C12(I)=ALAMB*DUDY(I)*C22(I)*C22(I)		!Suresh formulation
       
       IF (I. EQ. NYUX) THEN
        C11(I)= 1.D0
        C12(I)= 0.D0
	C22(I)= 1.D0
	C33(I)= 1.D0	
       END IF
       fCmm(I)=(AL2-3.D0)/(AL2-C11(I)-C22(I)-C33(I)+SMALL1)	!Suresh formulation


       TP12(I)=VISP/(ALAMB+SMALL1)*fCmm(I)*C12(I)
       TP11(I)=VISP/(ALAMB+SMALL1)*(fCmm(I)*C11(I)-fL)
       TP22(I)=VISP/(ALAMB+SMALL1)*(fCmm(I)*C22(I)-fL)
       TP33(I)=VISP/(ALAMB+SMALL1)*(fCmm(I)*C33(I)-fL)
       XMUTP12(I)=DABS(TP12(I)/(DUDY(I)+SMALL1))	
       IF (I .EQ. NYUX) XMUTP12(I)=VISP
      RETURN
 
      END

 
      SUBROUTINE ELASTIC
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'

             
      ENTRY ELASTURB(J)		!This uses the analytical solution of the cubic equation
c   Here we calculate now the polymer stress and all other viscoelastic
c   quantities using their definitions. 
C
C   FIrst, the turbulence parameters

         UR2=USB*USB+TK(J)
	 UTAU=USB
         IF(J.GT.NYUX) UR2=UST*UST+TK(J)
         IF(J.GT.NYUX) UTAU=UST	 

	   ALFA0=CALFA0*XMUTP12(J)/(VISP+SMALL1)*DENSIT/
     1           (VISS+XMUTP12(J)+SMALL1)	!XMULM is  viscosity (VISS+VISP)
           ALFA1A=CALFA1A/(ALAMB+SMALL1) 		!to be modified
           ALFA1B=CALFA1B/(ALAMB+SMALL1) 		!to be modified
           ALFA2=CALFA2
           ALFA3A=CALFA3A/(ALAMB*ALAMB*UTAU*UTAU+SMALL1)	!to be modified
           ALFA3B=CALFA3B/(ALAMB*ALAMB*UTAU*UTAU+SMALL1)	!to be modified
           GAMA1=CGAMA1
	   EXPR3=CEXP3*UTAU*UTAU*DENSIT*DENSIT/(VISS+VISP+SMALL1)/
     1      (VISS+VISP+SMALL1)
	   CKK=C11(J)+C22(J)+C33(J)
	   ALFA14=CALFA14*DENSIT/(VISS+VISP+SMALL1)

           BETA0=CBETA0*XMUTP12(J)/(VISP+SMALL1)
           BETA1=CBETA1
           BETA2=CBETA2
           BETA3=CBETA3
           BETA4=CBETA4
           BETA5=CBETA5
           BETA6=CBETA6
           BETA7=CBETA7/(ALAMB+SMALL1)		!Unrelated to normalization
           
           FMUNLT(J)=(1.D0-DEXP(-YPLS(J)/2.65D+1))*
     +               (1.D0-DEXP(-YPLS(J)/2.65D+1)) !
           FMUCU(J)=(1.D0-DEXP(-YPLS(J)/2.65D+1)) !
c
c	 END IF
c
C       fL=AL2/(AL2-3.)				!f(L)
         fL=1.D0				!f(L) formulation of Suresh
         AUX=ALAMB/(fCmm(J)+SMALL1)	!LAMBDA/f(Cmm)

         TLN11OLD=TLN11(J)
         TLN12OLD=TLN12(J)
         TLN22OLD=TLN22(J)
         TLN33OLD=TLN33(J)

         TLN11(J)=AUX*(ALFA0*TH*TE(J))+FMUNLT(J)*AUX*((2.D0*ALFA1A+4.D0*
     +    ALFA1B*C11(J))*C12(J)*DUDY(J)+2.D0*ALFA3A*(U2(J)*C11(J)+
     +    UV(J)*C12(J))+2.D0*(CALFA4+CALFA5)*DUVDY(J)*DC12DY(J)+
     +    GAMA1*DV2DY(J)*DC11DY(J)+EXPR3*CKK*U2(J)+ALFA14*DUDY(J)*
     +    (3.D0*U2(J)*C12(J)+UV(J)*(C22(J)-2.D0*C11(J))))	

         TLN12(J)=FMUNLT(J)*AUX*((ALFA1A*C22(J)+2.D0*ALFA1B*
     +    (2.D0*C12(J)*C12(J)+C11(J)*C22(J)+C11(J)*C11(J)))*DUDY(J)+
     +    ALFA3A*((U2(J)+V2(J))*C12(J)+UV(J)*(C11(J)+C22(J)))+
     +    (CALFA4+CALFA5)*(DV2DY(J)*DC12DY(J)+DUVDY(J)*DC22DY(J))+
     +    GAMA1*DV2DY(J)*DC12DY(J)+EXPR3*CKK*UV(J)+ALFA14*DUDY(J)*
     +    (U2(J)*(C22(J)-C11(J))-UV(J)*C12(J)-V2(J)*C11(J)))
     
         TLN22(J)=AUX*(ALFA0*TH*TE(J))+AUX*FMUNLT(J)*(ALFA1B*4.D0*
     +    C12(J)*(C11(J)+2.D0*C22(J))*DUDY(J)+2.D0*ALFA3A*(UV(J)*C12(J)+
     +    V2(J)*C22(J))+2.D0*(CALFA4+CALFA5)*DC22DY(J)*DV2DY(J)+
     +    GAMA1*DV2DY(J)*DC22DY(J)+EXPR3*CKK*V2(J)+ALFA14*DUDY(J)*
     +    (UV(J)*(2.D0*C22(J)-C11(J))-3.D0*V2(J)*C12(J)))			

         TLN33(J)=AUX*(ALFA0*TH*TE(J))+AUX*FMUNLT(J)*(ALFA1B*
     +    DUDY(J)*C31(J)*C32(J)+2.D0*ALFA3A*W2(J)*C33(J)+GAMA1*
     +    DV2DY(J)*DC33DY(J)+EXPR3*CKK*W2(J))	

         TLN11MIN=-fL/(ALAMB+SMALL1)-2.D0*C12(J)*DUDY(J)

         IF (TLN11(J) .LT. TLN11MIN) TLN11(J)=TLN11MIN     !              
         IF (TLN11(J) .GT. fCmm(J)*C11(J)/(ALAMB+SMALL1)) TLN11(J)=	!probably unnecessary and should be used instead to limit C22
     +       fCmm(J)*C11(J)/(ALAMB+SMALL1)


         IF (TLN22(J) .LT. 0.D0) TLN22(J)=0.D0		!Inspection of DNS data
         IF (TLN22(J) .GT. fCmm(J)*C22(J)/(ALAMB+SMALL1)) TLN22(J)=	!probably unnecessary and should be used instead to limit C22
     +       fCmm(J)*C22(J)/(ALAMB+SMALL1)

         IF (TLN33(J) .LT. 0.D0) TLN33(J)=0.D0		!Inspection of DNS data
         IF (TLN33(J) .GT. fCmm(J)*C33(J)/(ALAMB+SMALL1)) TLN33(J)=	!probably unnecessary and should be used instead to limit C33
     +       fCmm(J)*C33(J)/(ALAMB+SMALL1)

          TLN11(J)=5.D-2*TLN11(J)+9.5D-1*TLN11OLD
          TLN12(J)=5.D-2*TLN12(J)+9.5D-1*TLN12OLD
          TLN22(J)=5.D-2*TLN22(J)+9.5D-1*TLN22OLD
          TLN33(J)=5.D-2*TLN33(J)+9.5D-1*TLN33OLD
          
         C22OLD=C22(J)
	 C33OLD=C33(J)
	 C11OLD=C11(J)
	 C12OLD=C12(J)

c      Here we solve the cubic equation on C22 
         A1=TLN12(J)/(DUDY(J)+SMALL1)
	 A2=(AL2-ALAMB*(TLN11(J)+TLN22(J)+TLN33(J)))*(ALAMB*TLN22(J)+
     +      1.D0)/(2.D0*ALAMB*ALAMB*DUDY(J)*DUDY(J)+SMALL1)
	 A3=-AL2*(ALAMB*TLN22(J)+1.D0)*(ALAMB*TLN22(J)+1.D0)/
     +      (2.D0*ALAMB*ALAMB*DUDY(J)*DUDY(J)+SMALL1)
         AALFA=A2-A1*A1/3.D0
	 BBETA=A3-A1*A2/3.D0+2.D0*A1*A1*A1/27.D0
	 BDELTA=BBETA*BBETA/4.D0+AALFA*AALFA*AALFA/27.D0
	 IF (BDELTA .LT. 0.) THEN
	   RO=DSQRT(-AALFA*AALFA*AALFA/27.D0)
	   OMEGA1=DACOS(-BBETA/(2.D0*RO))
	   C2SOL1=2.D0*DSQRT(-AALFA/3.D0)*DCOS(OMEGA1/3.D0)-A1/3.D0
	   C2SOL2=2.D0*DSQRT(-AALFA/3.D0)*DCOS((OMEGA1+2.D0*PI)/3.D0)-
     +            A1/3.D0
	   C2SOL3=2.D0*DSQRT(-AALFA/3.D0)*DCOS((OMEGA1+4.D0*PI)/3.D0)-
     +            A1/3.D0

	   C1SOL1=AL2-C2SOL1*(AL2-1.D0+ALAMB*(TLN22(J)+TLN33(J)))/
     +         (ALAMB*TLN22(J)+1.D0)
	   C1SOL2=AL2-C2SOL2*(AL2-1.D0+ALAMB*(TLN22(J)+TLN33(J)))/
     +         (ALAMB*TLN22(J)+1.D0)
	   C1SOL3=AL2-C2SOL3*(AL2-1.D0+ALAMB*(TLN22(J)+TLN33(J)))/
     +         (ALAMB*TLN22(J)+1.D0)

           CLIMIT1=(AL2-C1SOL1)/2.D0	!Limit on C22 for Ckk< L**2
	   CLIMIT2=(AL2-C1SOL2)/2.D0	!Limit on C22 for Ckk< L**2
	   CLIMIT3=(AL2-C1SOL3)/2.D0	!Limit on C22 for Ckk< L**2

	   IF (C2SOL1 .GT. 0. .AND. C2SOL1 .LT. CLIMIT1 .AND. 
     +         C1SOL1 .GT. 1. .AND. C1SOL1 .LT. AL2) THEN
	       C11(J)=C1SOL1
	       C22(J)=C2SOL1
	       ICCOUNT=ICCOUNT+1
	   ELSE IF(C2SOL2 .GT. 0. .AND. C2SOL2 .LT. CLIMIT2 .AND. 
     +              C1SOL2 .GT. 1. .AND. C1SOL2 .LT. AL2) THEN
	       C11(J)=C1SOL2
	       C22(J)=C2SOL2
	       ICCOUNT=ICCOUNT+1
	   ELSE IF(C2SOL3 .GT. 0. .AND. C2SOL3 .LT. CLIMIT3 .AND. 
     +              C1SOL3 .GT. 1. .AND. C1SOL3 .LT. AL2) THEN 
	       C11(J)=C1SOL3
	       C22(J)=C2SOL3
	       ICCOUNT=ICCOUNT+1
	    ELSE 			!refers to C2SOL1,C2SOL2,C2SOL3
c	       WRITE(*,*)'C11(',J,')=',C11(J),'C22(',J,')=',C22(J)
c             WRITE(7,*)'ISTEP=',ISTEP,'ALFA1=',ALFA1
c	     WRITE(7,*)'C22(',J,')=',C22(J),' C2SOL1=',C2SOL1,
c     +           ' C2SOL2=',C2SOL2,' C2SOL3=',C2SOL3
c	     WRITE(7,*)'C11(',J,')=',C11(J),' C1SOL1=',C1SOL1,
c     +                 ' C1SOL2=',C1SOL2,' C1SOL3=',C1SOL3
CCCC	       WRITE(*,*)'Problems in C22'
CCCC	       PAUSE

	       C11(J)=C11OLD
	       C22(J)=C22OLD
	    ENDIF			!refers to C2SOL1,C2SOL2,C2SOL3
	 ELSE				!refers to BDELTA positive
	   PP=-BBETA/2.D0+DSQRT(BDELTA)
	   QQ=-BBETA/2.D0-DSQRT(BDELTA)
	   C2SOL=PP/(DABS(PP)+SMALL1)*(DABS(PP))**(1.D0/3.D0)+
     +           QQ/(DABS(QQ)+SMALL1)*(DABS(QQ))**(1.D0/3.D0)-A1/3.D0
           C1SOL=AL2-C22(J)*(AL2-1.D0+ALAMB*(TLN22(J)+TLN33(J)))/
     +            (ALAMB*TLN22(J)+1.D0)

           CLIMIT=(AL2-C1SOL)/2.D0		!Limit for C22 so that Ckk<L**2
           IF (C2SOL .GT. 0. .AND. C2SOL .LT. CLIMIT .AND. 
     +          C1SOL .GT. 1. .AND. C1SOL .LT. AL2) THEN
	       C11(J)=C1SOL     
	       C22(J)=C2SOL
	       ICCOUNT=ICCOUNT+1
           ELSE		    !C22 and C11
C	       WRITE(*,*)'1R','C11(',J,')=',C11(J),'C22(',J,')=',C22(J)
C               WRITE(7,*)'ISTEP=',ISTEP,J,'single root'
C	       WRITE(7,*)'C22(',J,')=',C22(J),' C2SOL1=',C2SOL1
C	       WRITE(7,*)'C11(',J,')=',C11(J),' C1SOL1=',C1SOL1

c               IF (C1SOL .LT. 1.) C11(J)=1.
c	       C11(J)=AL2
c               IF (C2SOL .LT. 0.0) C22(J)=0.0
c	       C22(J)=CLIMIT

	       C11(J)=C11OLD
	       C22(J)=C22OLD

	   ENDIF		    !C22 and C11
	 ENDIF				!refers to BDELTA

c          C22(J)=1.D-1*C22(J)+9.D-1*C22OLD
c          C11(J)=1.D-1*C11(J)+9.D-1*C11OLD

	  C3SOL=C22(J)*(ALAMB*TLN33(J)+1.D0)/(ALAMB*TLN22(J)+1.D0)
          CLIMIT=(AL2-C11(J))/2.D0	  
          IF (C3SOL .GT. 0.0 .AND. C3SOL .LT. CLIMIT) THEN
	    C33(J)=C3SOL
	  ELSE IF (C3SOL .LT. 0.0) THEN
	    C33(J)=0.0
	  ELSE			!C3SOL > CLIMIT
	    C33(J)=CLIMIT	  
	  END IF

c          C33(J)=1.D-1*C33(J)+9.D-1*C33OLD
	 C12(J)=C22(J)*ALAMB*(C22(J)*DUDY(J)+TLN12(J))/
     +          (ALAMB*TLN22(J)+1.D0)
c          C12(J)=1.D-1*C12(J)+9.D-1*C12OLD


c          C22(J)=1.D-1*C22(J)+9.D-1*C22OLD
c          C11(J)=1.D-1*C11(J)+9.D-1*C11OLD
c          C33(J)=1.D-1*C33(J)+9.D-1*C33OLD
c          C12(J)=1.D-1*C12(J)+9.D-1*C12OLD



c	  IF (J .EQ. NYUX) THEN	  
c	    C11(J)=(ALAMB*TLN11(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
c	    C12(J)=(ALAMB*TLN12(J))/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
c	    C22(J)=(ALAMB*TLN22(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
c	    C33(J)=(ALAMB*TLN33(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
c          END IF


         C31(J)=0.0		!Assumed zero (zero is definitely a solution)
         C32(J)=0.0		!Assumed zero (zero is definitely a solution)
       
c       fCmm(J)=AL2/(AL2-C11(J)-C22(J)-C33(J)+SMALL1)		!f(Cmm)
         fCmm(J)=(AL2-3.)/(AL2-C11(J)-C22(J)-C33(J)+SMALL1)	!f(Cmm) of Suresh
         IF (fCmm(J) .LT. 1.D0) fCmm(J)=1.D0	!Lower limit
         IF (fCmm(J) .LT. 0.D0) fCmm(J)=GREAT	!Lower limit
cc         IF (fCmm(J) .GT. GREAT) fCmm(J)=GREAT	!Upper limit


         CU222OLD=CU222(J)
         CU332OLD=CU332(J)
         CU112OLD=CU112(J)
         CU122OLD=CU122(J)

         CU222(J)=FMUCU(J)*AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+
     +     (BETA4+BETA5+BETA6)*C22(J)*DV2DY(J)-2.D0*BETA7*fCmm(J)*
     +      DSQRT(V2(J))*C22(J))				

         IF(J.EQ.NYUX) CU222(J)=FMUCU(J)*
     +     AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+(BETA4+BETA5+BETA6)*
     +     C22(J)*DV2DY(J)+2.D0*BETA7*0.D0)

         IF(J.LT.NYUX) CU222(J)=FMUCU(J)*
     +     AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+(BETA4+BETA5+BETA6)*
     +     C22(J)*DV2DY(J)+2.D0*BETA7*fCmm(J)*DSQRT(V2(J))*C22(J))
         
         VC22(J)=CU222(J)/2.D0				!by definition
         CU222(J)=CU222(J)+AUX*BETA0*DV2DY(J)				

         CU332(J)=AUX*(BETA0*DW2DY(J))+FMUCU(J)*AUX*(BETA4*C22(J)*
     +       DW2DY(J)-2.D0*BETA7*fCmm(J)*DSQRT(W2(J))*C32(J))				

         UC22(J)=AUX*(-BETA1*UV(J)*DC22DY(J)+(BETA4/2.D0+BETA5)*C22(J)*
     +       DUVDY(J)-BETA7*fCmm(J)*DSQRT(U2(J))*C22(J))		

         CU112(J)=AUX*(BETA0*DU2DY(J))+FMUCU(J)*AUX*(-2.D0*BETA1*UV(J)*
     +     DC12DY(J)+2.D0*BETA3*UC22(J)*DUDY(J)+BETA4*C22(J)*DU2DY(J)+
     +     (BETA5+BETA6)*C12(J)*DUVDY(J)-2.D0*BETA7*fCmm(J)*
     +     DSQRT(U2(J))*C12(J))				

         CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*(-BETA1*(UV(J)*
     +     DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*DUDY(J)+
     +     (BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(DSQRT(V2(J))*C12(J)+DSQRT(U2(J))*C22(J)))

         IF(J.EQ.NYUX) CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*
     +     (-BETA1*(UV(J)*DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*
     +     DUDY(J)+(BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(0.D0+DSQRT(U2(J))*C22(J)))

         IF(J.LT.NYUX) CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*
     +     (-BETA1*(UV(J)*DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*
     +     DUDY(J)+(BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(-DSQRT(V2(J))*C12(J)+DSQRT(U2(J))*C22(J)))

         CU112(J)=1.D-2*CU112(J)+9.9D-1*CU112OLD
         CU122(J)=1.D-2*CU122(J)+9.9D-1*CU122OLD
         CU222(J)=1.D-2*CU222(J)+9.9D-1*CU222OLD
         CU332(J)=1.D-2*CU332(J)+9.9D-1*CU332OLD

       TP12(J)=VISP/(ALAMB+SMALL1)*fCmm(J)*C12(J)
       TP11(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C11(J)-fL)
       TP22(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C22(J)-fL)
       TP33(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C33(J)-fL)
       XMUTP12(J)=DABS(TP12(J)/(DUDY(J)+SMALL1))	!We can introduce TP12 into momentum as a viscosity
       XMUTP12(J)=DABS(TP12(J)/(DUDY(J)+SMALL1))	!We can introduce TP12 into momentum as a viscosity
       IF (J .EQ. NYUX) XMUTP12(J)=VISP

      RETURN

 
      ENTRY ELASTURB2(J)	!This uses the original equations which are unstable
c   Here we calculate the polymer stress and all other viscoelastic
c   quantities using their definitions. 
C
C   FIrst, the turbulence parameters
       LAST=1
       UTAU=USB
       
       DO 500 ISTEP1=1,LAST
         
	   ALFA0=CALFA0*XMUTP12(J)/(VISP+SMALL1)*DENSIT/
     1           (VISS+XMUTP12(J)+SMALL1)	!XMULM is  viscosity (VISS+VISP)
           ALFA1A=CALFA1A/(ALAMB+SMALL1) 		!
           ALFA1B=CALFA1B/(ALAMB+SMALL1) 		!
           ALFA2=CALFA2
           ALFA3A=CALFA3A/(ALAMB*ALAMB*UTAU*UTAU+SMALL1)
           ALFA3B=CALFA3B/(ALAMB*ALAMB*UTAU*UTAU+SMALL1)
           GAMA1=CGAMA1
	   EXPR3=CEXP3*UTAU*UTAU*DENSIT*DENSIT/(VISS+VISP+SMALL1)/
     1           (VISS+VISP+SMALL1)
	   CKK=C11(J)+C22(J)+C33(J)
	   ALFA14=CALFA14*DENSIT/(VISS+VISP+SMALL1)

           BETA0=CBETA0*XMUTP12(J)/(VISP+SMALL1)
           BETA1=CBETA1
           BETA2=CBETA2
           BETA3=CBETA3
           BETA4=CBETA4
           BETA5=CBETA5
           BETA6=CBETA6
           BETA7=CBETA7/(ALAMB+SMALL1)		!Check this definition
           
           FMUNLT(J)=(1.D0-DEXP(-YPLS(J)/2.65D+1))*
     +               (1.D0-DEXP(-YPLS(J)/2.65D+1)) !
           FMUCU(J)=(1.D0-DEXP(-YPLS(J)/2.65D+1)) !
c
C       fL=AL2/(AL2-3.)				!f(L)
         fL=1.D0				!f(L) formulation of Suresh

         AUX=ALAMB/(fCmm(J)+SMALL1)	!LAMBDA/f(Cmm)

         TLN11OLD=TLN11(J)
         TLN12OLD=TLN12(J)
         TLN22OLD=TLN22(J)
         TLN33OLD=TLN33(J)

         TLN11(J)=AUX*(ALFA0*TH*TE(J))+FMUNLT(J)*AUX*((2.D0*ALFA1A+4.D0*
     +    ALFA1B*C11(J))*C12(J)*DUDY(J)+2.D0*ALFA3A*(U2(J)*C11(J)+
     +    UV(J)*C12(J))+2.D0*(CALFA4+CALFA5)*DUVDY(J)*DC12DY(J)+
     +    GAMA1*DV2DY(J)*DC11DY(J)+EXPR3*CKK*U2(J)+ALFA14*DUDY(J)*
     +    (3.D0*U2(J)*C12(J)+UV(J)*(C22(J)-2.D0*C11(J))))	

         TLN12(J)=FMUNLT(J)*AUX*((ALFA1A*C22(J)+2.D0*ALFA1B*
     +    (2.D0*C12(J)*C12(J)+C11(J)*C22(J)+C11(J)*C11(J)))*DUDY(J)+
     +    ALFA3A*((U2(J)+V2(J))*C12(J)+UV(J)*(C11(J)+C22(J)))+
     +    (CALFA4+CALFA5)*(DV2DY(J)*DC12DY(J)+DUVDY(J)*DC22DY(J))+
     +    GAMA1*DV2DY(J)*DC12DY(J)+EXPR3*CKK*UV(J)+ALFA14*DUDY(J)*
     +    (U2(J)*(C22(J)-C11(J))-UV(J)*C12(J)-V2(J)*C11(J)))
     
         TLN22(J)=AUX*(ALFA0*TH*TE(J))+AUX*FMUNLT(J)*(ALFA1B*4.D0*
     +    C12(J)*(C11(J)+2.D0*C22(J))*DUDY(J)+2.D0*ALFA3A*(UV(J)*C12(J)+
     +    V2(J)*C22(J))+2.D0*(CALFA4+CALFA5)*DC22DY(J)*DV2DY(J)+
     +    GAMA1*DV2DY(J)*DC22DY(J)+EXPR3*CKK*V2(J)+ALFA14*DUDY(J)*
     +    (UV(J)*(2.D0*C22(J)-C11(J))-3.D0*V2(J)*C12(J)))			

         TLN33(J)=AUX*(ALFA0*TH*TE(J))+AUX*FMUNLT(J)*(ALFA1B*
     +    DUDY(J)*C31(J)*C32(J)+2.D0*ALFA3A*W2(J)*C33(J)+GAMA1*
     +    DV2DY(J)*DC33DY(J)+EXPR3*CKK*W2(J))	


         TLN11MIN=-fL/(ALAMB+SMALL1)-2.D0*C12(J)*DUDY(J)

         IF (TLN11(J) .LT. TLN11MIN) TLN11(J)=TLN11MIN     !              
         IF (TLN11(J) .GT. fCmm(J)*C11(J)/(ALAMB+SMALL1)) TLN11(J)=	!probably unnecessary and should be used instead to limit C22
     +       fCmm(J)*C11(J)/(ALAMB+SMALL1)


         IF (TLN22(J) .LT. 0.D0) TLN22(J)=0.D0		!Inspection of DNS data
         IF (TLN22(J) .GT. fCmm(J)*C22(J)/(ALAMB+SMALL1)) TLN22(J)=	!probably unnecessary and should be used instead to limit C22
     +       fCmm(J)*C22(J)/(ALAMB+SMALL1)

         IF (TLN33(J) .LT. 0.D0) TLN33(J)=0.D0		!Inspection of DNS data
         IF (TLN33(J) .GT. fCmm(J)*C33(J)/(ALAMB+SMALL1)) TLN33(J)=	!probably unnecessary and should be used instead to limit C33
     +       fCmm(J)*C33(J)/(ALAMB+SMALL1)

          TLN11(J)=1.D-1*TLN11(J)+9.D-1*TLN11OLD
          TLN12(J)=1.D-1*TLN12(J)+9.D-1*TLN12OLD
          TLN22(J)=1.D-1*TLN22(J)+9.D-1*TLN22OLD
          TLN33(J)=1.D-1*TLN33(J)+9.D-1*TLN33OLD

          
         C22OLD=C22(J)
	 C33OLD=C33(J)
	 C11OLD=C11(J)
	 C12OLD=C12(J)

c      Here we use the iterative process to determine the conformation tensor
c      we don't use the cubic equation here
c
c     subrelaxa‹o. Se lambda = 0.1 Ž conveniente usar subrela com coeficientes 0.1 e 0.9
c     se lambda da ordem de 1 s‹o necess‡rios coeficientes de subrelaxa‹o de 0.05 e 0.95
c     se lambda da ordem de 10 s‹o necess‡rios coeficientes de subrelaxa‹o de 0.01 e 0.99
c     I also iterated more times (4) at high lambda, whereas at small I iterated 2
c     USING LAST=4 and subrelax coefficients of 0.01 and 0.99 it works well for all Lambdas
c
         C31(J)=0.D0		!Assumed zero (zero is definitely a solution)
         C32(J)=0.D0		!Assumed zero (zero is definitely a solution)

         C2SOL=(ALAMB*TLN22(J)+fL)/(fCmm(J)+SMALL1)       
	   C22(J)=C2SOL
         IF (C22(J) .LT. 0.0) C22(J) = 0.D0
         IF (C22(J) .GT. AL2) C22(J) = AL2
         C22(J)=1.D-2*C22(J)+9.9D-1*C22OLD

         C3SOL=(ALAMB*TLN33(J)+fL)/(fCmm(J)+SMALL1)
	    C33(J)=C3SOL
         IF (C33(J) .LT. 0.0) C33(J) = 0.D0
         IF (C33(J) .GT. AL2) C33(J) = AL2
         C33(J)=1.D-2*C33(J)+9.9D-1*C33OLD

         C12(J)=ALAMB*(C22(J)*DUDY(J)+TLN12(J))/(fCmm(J)+SMALL1)
         C12(J)=1.D-2*C12(J)+9.9D-1*C12OLD

         C1SOL=(ALAMB*(2.*C12(J)*DUDY(J)+TLN11(J))+fL)/(fCmm(J)+SMALL1)
	 C11(J)=C1SOL
         IF (C11(J) .LT. 1.0) C11(J) = 1.D0
         IF (C11(J) .GT. AL2) C11(J) = AL2
         C11(J)=1.D-2*C11(J)+9.9D-1*C11OLD

	  IF (J .EQ. NYUX) THEN	  
	    C11(J)=(ALAMB*TLN11(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
	    C12(J)=(ALAMB*TLN12(J))/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
	    C22(J)=(ALAMB*TLN22(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
	    C33(J)=(ALAMB*TLN33(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
          END IF

c       fCmm(J)=AL2/(AL2-C11(J)-C22(J)-C33(J)+SMALL1)		!f(Cmm) of Bird
         fCmm(J)=(AL2-3.D0)/(AL2-C11(J)-C22(J)-C33(J)+SMALL1)	!f(Cmm) of Suresh
         IF (fCmm(J) .LT. 1.D0) fCmm(J)=1.D0	!Lower limit
         IF (fCmm(J) .LT. 0.D0) fCmm(J)=GREAT	!Lower limit
cc         IF (fCmm(J) .GT. GREAT) fCmm(J)=GREAT	!Upper limit


 500   CONTINUE

         CU222OLD=CU222(J)
         CU332OLD=CU332(J)
         CU112OLD=CU112(J)
         CU122OLD=CU122(J)

         CU222(J)=FMUCU(J)*AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+
     +     (BETA4+BETA5+BETA6)*C22(J)*DV2DY(J)-2.D0*BETA7*fCmm(J)*
     +      DSQRT(V2(J))*C22(J))				

         IF(J.EQ.NYUX) CU222(J)=FMUCU(J)*
     +     AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+(BETA4+BETA5+BETA6)*
     +     C22(J)*DV2DY(J)+2.D0*BETA7*0.D0)

         IF(J.LT.NYUX) CU222(J)=FMUCU(J)*
     +     AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+(BETA4+BETA5+BETA6)*
     +     C22(J)*DV2DY(J)+2.D0*BETA7*fCmm(J)*DSQRT(V2(J))*C22(J))

         VC22(J)=CU222(J)/2.D0				!by definition
         CU222(J)=CU222(J)+AUX*BETA0*DV2DY(J)				

         CU332(J)=AUX*(BETA0*DW2DY(J))+FMUCU(J)*AUX*(BETA4*C22(J)*
     +       DW2DY(J)-2.D0*BETA7*fCmm(J)*DSQRT(W2(J))*C32(J))				

         UC22(J)=AUX*(-BETA1*UV(J)*DC22DY(J)+(BETA4/2.D0+BETA5)*C22(J)*
     +       DUVDY(J)-BETA7*fCmm(J)*DSQRT(U2(J))*C22(J))		

         CU112(J)=AUX*(BETA0*DU2DY(J))+FMUCU(J)*AUX*(-2.D0*BETA1*UV(J)*
     +     DC12DY(J)+2.D0*BETA3*UC22(J)*DUDY(J)+BETA4*C22(J)*DU2DY(J)+
     +     (BETA5+BETA6)*C12(J)*DUVDY(J)-2.D0*BETA7*fCmm(J)*
     +     DSQRT(U2(J))*C12(J))				

         CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*(-BETA1*(UV(J)*
     +     DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*DUDY(J)+
     +     (BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(DSQRT(V2(J))*C12(J)+DSQRT(U2(J))*C22(J)))

         IF(J.EQ.NYUX) CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*
     +     (-BETA1*(UV(J)*DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*
     +     DUDY(J)+(BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(0.D0+DSQRT(U2(J))*C22(J)))

         IF(J.LT.NYUX) CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*
     +     (-BETA1*(UV(J)*DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*
     +     DUDY(J)+(BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(-DSQRT(V2(J))*C12(J)+DSQRT(U2(J))*C22(J)))


         CU112(J)=1.D-2*CU112(J)+9.9D-1*CU112OLD
         CU122(J)=1.D-2*CU122(J)+9.9D-1*CU122OLD
         CU222(J)=1.D-2*CU222(J)+9.9D-1*CU222OLD
         CU332(J)=1.D-2*CU332(J)+9.9D-1*CU332OLD

       TP12(J)=VISP/(ALAMB+SMALL1)*fCmm(J)*C12(J)
       TP11(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C11(J)-fL)
       TP22(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C22(J)-fL)
       TP33(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C33(J)-fL)
       XMUTP12(J)=DABS(TP12(J)/(DUDY(J)+SMALL1))	!We can introduce TP12 into momentum as a viscosity
c       XMUTP12(J)=DABS(TP12(J)/(DUDY(J)+SMALL1))	!We can introduce TP12 into momentum as a viscosity
       IF (J .EQ. NYUX) XMUTP12(J)=VISP

      RETURN
  
      ENTRY ELASTURB3(J)	!Resende's closure
c   Here we calculate now the polymer stress and all other viscoelastic
c   quantities using their definitions. 
C
C   FIrst, the turbulence parameters
       LAST=1
       UTAU=USB
       
       DO 501 ISTEP1=1,LAST
         
	   CKK=C11(J)+C22(J)+C33(J)

c     Coefficients for closure of NLTij of Resende
           ALFA1A=CALFA1A*UTAU*UTAU*DENSIT/(VISS+VISP+SMALL1) 	!CALFA1A= CF1 of Resende
           ALFA2=CALFA2						!CALFA2 = CF2 of Resende
           ALFA3A=CALFA3A*DENSIT/(VISS+VISP+SMALL1)		!CALFA3A= CF3 of Resende
           ALFA3B=CALFA3B					!CALFA3B= CF4 of Resende
           GAMA1=CGAMA1*4.D0/15.D0*DENSIT/VISS			!CGAMA1 =CepsF of Resende

           FMUNLT(J)=(1.D0-DEXP(-YPLS(J)/5.D+1))*
     +               (1.D0-DEXP(-YPLS(J)/5.D+1)) 		!FMUNLT=fF2 of Resende
           FF1(J)=(1.D0-0.8D0*DEXP(-YPLS(J)/3.0D+1))*
     +               (1.D0-0.8D0*DEXP(-YPLS(J)/3.0D+1)) 	!FF1=fF1 of Resende
           
c     End of coefficients for closure of NLTij of Resende

c     Coefficients for closure of CUijk
           BETA0=CBETA0*XMUTP12(J)/(VISP+SMALL1)
           BETA1=CBETA1
           BETA2=CBETA2
           BETA3=CBETA3
           BETA4=CBETA4
           BETA5=CBETA5
           BETA6=CBETA6
           BETA7=CBETA7/(ALAMB+SMALL1)		!Check this definition

           FMUCU(J)=(1.D0-DEXP(-YPLS(J)/2.65D+1)) !

c    End of coefficients for closure of CUijk   
        

c
C       fL=AL2/(AL2-3.)				!f(L)
         fL=1.D0				!f(L) formulation of Suresh

         AUX=ALAMB/(fCmm(J)+SMALL1)	!LAMBDA/f(Cmm)

         TLN11OLD=TLN11(J)
         TLN12OLD=TLN12(J)
         TLN22OLD=TLN22(J)
         TLN33OLD=TLN33(J)

         TLN11(J)=ALFA1A*C11(J)*fCmm(J)-ALFA2*2.D0*C12(J)*DUDY(J)+
     +      AUX*ALFA3A*2.D0*DABS(DUDY(J))*C22(J)*U2(J)-AUX*FF1(J)*
     +      ALFA3B*2.D0*C22(J)*DUDY(J)*DUDY(J)+AUX*GAMA1*Ckk*
     +      TEE(J)*FMUNLT(J)

         TLN12(J)=ALFA1A*C12(J)*fCmm(J)-ALFA2*C22(J)*DUDY(J)+
     +      AUX*ALFA3A*DABS(DUDY(J))*C22(J)*UV(J)
     
         TLN22(J)=ALFA1A*C22(J)*fCmm(J)+AUX*GAMA1*Ckk*TEE(J)*FMUNLT(J)		

         TLN33(J)=ALFA1A*C33(J)*fCmm(J)+AUX*GAMA1*Ckk*TEE(J)*FMUNLT(J)

         TLN11MIN=-fL/(ALAMB+SMALL1)-2.D0*C12(J)*DUDY(J)

         IF (TLN11(J) .LT. TLN11MIN) TLN11(J)=TLN11MIN     !              
         IF (TLN11(J) .GT. fCmm(J)*C11(J)/(ALAMB+SMALL1)) TLN11(J)=	!probably unnecessary and should be used instead to limit C22
     +       fCmm(J)*C11(J)/(ALAMB+SMALL1)


         IF (TLN22(J) .LT. 0.D0) TLN22(J)=0.D0		!Inspection of DNS data
         IF (TLN22(J) .GT. fCmm(J)*C22(J)/(ALAMB+SMALL1)) TLN22(J)=	!probably unnecessary and should be used instead to limit C22
     +       fCmm(J)*C22(J)/(ALAMB+SMALL1)

         IF (TLN33(J) .LT. 0.D0) TLN33(J)=0.D0		!Inspection of DNS data
         IF (TLN33(J) .GT. fCmm(J)*C33(J)/(ALAMB+SMALL1)) TLN33(J)=	!probably unnecessary and should be used instead to limit C33
     +       fCmm(J)*C33(J)/(ALAMB+SMALL1)

          TLN11(J)=1.D-1*TLN11(J)+9.D-1*TLN11OLD
          TLN12(J)=1.D-1*TLN12(J)+9.D-1*TLN12OLD
          TLN22(J)=1.D-1*TLN22(J)+9.D-1*TLN22OLD
          TLN33(J)=1.D-1*TLN33(J)+9.D-1*TLN33OLD

          
         C22OLD=C22(J)
	 C33OLD=C33(J)
	 C11OLD=C11(J)
	 C12OLD=C12(J)

c      Here we use the iterative process to determine the conformation tensor
c      we don't use the cubic equation here
c
c     subrelaxa‹o. Se lambda = 0.1 Ž conveniente usar subrela com coeficientes 0.1 e 0.9
c     se lambda da ordem de 1 s‹o necess‡rios coeficientes de subrelaxa‹o de 0.05 e 0.95
c     se lambda da ordem de 10 s‹o necess‡rios coeficientes de subrelaxa‹o de 0.01 e 0.99
c     I also iterated more times (4) at high lambda, whereas at small I iterated 2
c     USING LAST=4 and subrelax coefficients of 0.01 and 0.99 it works well for all Lambdas
c
         C31(J)=0.D0		!Assumed zero (zero is definitely a solution)
         C32(J)=0.D0		!Assumed zero (zero is definitely a solution)

         C2SOL=(ALAMB*TLN22(J)+fL)/(fCmm(J)+SMALL1)       
	   C22(J)=C2SOL
         IF (C22(J) .LT. 0.0) C22(J) = 0.D0
         IF (C22(J) .GT. AL2) C22(J) = AL2
         C22(J)=1.D-2*C22(J)+9.9D-1*C22OLD

         C3SOL=(ALAMB*TLN33(J)+fL)/(fCmm(J)+SMALL1)
	    C33(J)=C3SOL
         IF (C33(J) .LT. 0.0) C33(J) = 0.D0
         IF (C33(J) .GT. AL2) C33(J) = AL2
         C33(J)=1.D-2*C33(J)+9.9D-1*C33OLD

         C12(J)=ALAMB*(C22(J)*DUDY(J)+TLN12(J))/(fCmm(J)+SMALL1)
         C12(J)=1.D-2*C12(J)+9.9D-1*C12OLD

         C1SOL=(ALAMB*(2.*C12(J)*DUDY(J)+TLN11(J))+fL)/(fCmm(J)+SMALL1)
	 C11(J)=C1SOL
         IF (C11(J) .LT. 1.0) C11(J) = 1.D0
         IF (C11(J) .GT. AL2) C11(J) = AL2
         C11(J)=1.D-2*C11(J)+9.9D-1*C11OLD

	  IF (J .EQ. NYUX) THEN	  
	    C11(J)=(ALAMB*TLN11(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
	    C12(J)=(ALAMB*TLN12(J))/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
	    C22(J)=(ALAMB*TLN22(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
	    C33(J)=(ALAMB*TLN33(J)+fL)/(fCmm(J)+SMALL1)		!on axis no need for cubic equation
          END IF

c       fCmm(J)=AL2/(AL2-C11(J)-C22(J)-C33(J)+SMALL1)		!f(Cmm) of Bird
         fCmm(J)=(AL2-3.D0)/(AL2-C11(J)-C22(J)-C33(J)+SMALL1)	!f(Cmm) of Suresh
         IF (fCmm(J) .LT. 1.D0) fCmm(J)=1.D0	!Lower limit
         IF (fCmm(J) .LT. 0.D0) fCmm(J)=GREAT	!Lower limit
cc         IF (fCmm(J) .GT. GREAT) fCmm(J)=GREAT	!Upper limit


 501   CONTINUE

        IF (ISTEP .GT. 18000) THEN

         CU222OLD=CU222(J)
         CU332OLD=CU332(J)
         CU112OLD=CU112(J)
         CU122OLD=CU122(J)

         CU222(J)=FMUCU(J)*AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+
     +     (BETA4+BETA5+BETA6)*C22(J)*DV2DY(J)-2.D0*BETA7*fCmm(J)*
     +      DSQRT(V2(J))*C22(J))				

         IF(J.EQ.NYUX) CU222(J)=FMUCU(J)*
     +     AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+(BETA4+BETA5+BETA6)*
     +     C22(J)*DV2DY(J)+2.D0*BETA7*0.D0)

         IF(J.LT.NYUX) CU222(J)=FMUCU(J)*
     +     AUX*(-2.D0*BETA1*V2(J)*DC22DY(J)+(BETA4+BETA5+BETA6)*
     +     C22(J)*DV2DY(J)+2.D0*BETA7*fCmm(J)*DSQRT(V2(J))*C22(J))

         VC22(J)=CU222(J)/2.D0				!by definition
         CU222(J)=CU222(J)+AUX*BETA0*DV2DY(J)				

         CU332(J)=AUX*(BETA0*DW2DY(J))+FMUCU(J)*AUX*(BETA4*C22(J)*
     +       DW2DY(J)-2.D0*BETA7*fCmm(J)*DSQRT(W2(J))*C32(J))				

         UC22(J)=AUX*(-BETA1*UV(J)*DC22DY(J)+(BETA4/2.D0+BETA5)*C22(J)*
     +       DUVDY(J)-BETA7*fCmm(J)*DSQRT(U2(J))*C22(J))		

         CU112(J)=AUX*(BETA0*DU2DY(J))+FMUCU(J)*AUX*(-2.D0*BETA1*UV(J)*
     +     DC12DY(J)+2.D0*BETA3*UC22(J)*DUDY(J)+BETA4*C22(J)*DU2DY(J)+
     +     (BETA5+BETA6)*C12(J)*DUVDY(J)-2.D0*BETA7*fCmm(J)*
     +     DSQRT(U2(J))*C12(J))				

         CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*(-BETA1*(UV(J)*
     +     DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*DUDY(J)+
     +     (BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(DSQRT(V2(J))*C12(J)+DSQRT(U2(J))*C22(J)))

         IF(J.EQ.NYUX) CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*
     +     (-BETA1*(UV(J)*DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*
     +     DUDY(J)+(BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(0.D0+DSQRT(U2(J))*C22(J)))

         IF(J.LT.NYUX) CU122(J)=AUX*(BETA0*DUVDY(J))+FMUCU(J)*AUX*
     +     (-BETA1*(UV(J)*DC22DY(J)+V2(J)*DC12DY(J))+BETA3*VC22(J)*
     +     DUDY(J)+(BETA4+BETA5)*C22(J)*DUVDY(J)+BETA6*C12(J)*DV2DY(J)
     +     -BETA7*fCmm(J)*(-DSQRT(V2(J))*C12(J)+DSQRT(U2(J))*C22(J)))


         CU112(J)=1.D-2*CU112(J)+9.9D-1*CU112OLD
         CU122(J)=1.D-2*CU122(J)+9.9D-1*CU122OLD
         CU222(J)=1.D-2*CU222(J)+9.9D-1*CU222OLD
         CU332(J)=1.D-2*CU332(J)+9.9D-1*CU332OLD

        END IF
 
       TP12(J)=VISP/(ALAMB+SMALL1)*fCmm(J)*C12(J)
       TP11(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C11(J)-fL)
       TP22(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C22(J)-fL)
       TP33(J)=VISP/(ALAMB+SMALL1)*(fCmm(J)*C33(J)-fL)
       XMUTP12(J)=DABS(TP12(J)/(DUDY(J)+SMALL1))	!We can introduce TP12 into momentum as a viscosity
c       XMUTP12(J)=DABS(TP12(J)/(DUDY(J)+SMALL1))	!We can introduce TP12 into momentum as a viscosity
       IF (J .EQ. NYUX) XMUTP12(J)=VISP

      RETURN
      
      END



      SUBROUTINE CENTRAL
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'

      ENTRY CALCU
      JF=2
      JL=NYM1
c      JF=1
c      JL=NY
      IVAR=INU
      UVM=0.

      DO 110 J=JF,JL
c      DO 110 J=2,NY
      SU(J)=-DPDX(J)/RHO(J)

c      IF (ISTEP .GT. 25000) THEN	!FROM OLD MODEL (THESE 4 INSTRUCTIONS
c       SIJMIP=2.*SIJMIS(J)*RV(J+1)	!new viscoelastic stress term in north face
c       SU(J)=SU(J)+(SIJMIP-SIJMIM)/YSR(J)/RHO(J)		!north - south term and compensation for areas. In SOLVE term is multiplied by RHO*VOL
c       SIJMIM=SIJMIP			!north term becomes south of next cell
c      ENDIF

      IF(MODEL.EQ.1) GO TO 110
      UVP=UVS(J)*RV(J+1)		!for RSM: the Reynolds shear stress must go to the source
      SU(J)=SU(J)-(UVP-UVM)/YSR(J)	!for RSM: the Reynolds shear stress must go to the source
      UVM=UVP
  110 SP(J)=0.D0

      CALL SOLVE(1,PRTUR(INU),PRLAM(INU),PRPTAU(INU),U,UU)

c   Boundary condition
       U(1)= 0.
       U(NY)= 0.
c

      RETURN

      ENTRY CALCU2
      JF=2
      JL=NYM1
      IVAR=INU2

      DO 600 J=JF,JL
      SU(J)=P11(J)*(1.D0-C2)+TH*C1*TE(J)+TH*C2*PK(J)+C1P*F(J)*EDK(J)*
     +     V2(J)+TH*C2*C2P*F(J)*PK(J)-C2*C2P*F(J)*P22(J)
      SU(J)=SU(J)+G11(J)
C     SU(J)=SU(J)+(1.0-0.5*C2)*G11(J)

      SP(J)=-C1*EDK(J)*U2(J)-TH*TE(J)
  600 CONTINUE

      CALL SOLVE(1,PRTUR(INU2),PRLAM(INU2),PRPTAU(INU2),U2,U2U)
      RETURN

      ENTRY CALCV2
      JF=2
      JL=NYM1
      IVAR=INV2

      DO 610 J=JF,JL
      SU(J)=P22(J)*(1.-C2)+TH*C1*TE(J)+TH*C2*PK(J)
     +      +2.*C2*C2P*F(J)*P22(J)
      SU(J)=SU(J)+G22(J)
C     SU(J)=SU(J)+(1.-0.5*C2)*G22(J)

      SP(J)=-C1*EDK(J)*V2(J)-2.*C1P*F(J)*EDK(J)*V2(J)-TH*TE(J)
     +-2.*TH*C2*C2P*F(J)*PK(J)
  610 CONTINUE

      CALL SOLVE(1,PRTUR(INV2),PRLAM(INV2),PRPTAU(INV2),V2,V2U)
      RETURN

      ENTRY CALCW2
      JF=2
      JL=NYM1
      IVAR=INW2

      DO 620 J=JF,JL
      SU(J)=TH*C1*TE(J)+TH*C2*PK(J)+C1P*F(J)*EDK(J)*V2(J)
     +   +TH*C2*C2P*F(J)*PK(J)-C2*C2P*F(J)*P22(J)
      SP(J)=-C1*EDK(J)*W2(J)-TH*TE(J)
  620 CONTINUE

      CALL SOLVE(1,PRTUR(INW2),PRLAM(INW2),PRPTAU(INW2),W2,W2U)
      RETURN

      ENTRY CALCUV
c      JF=2
c      JL=NYM1
      JF=1
      JL=NY
      IVAR=INUV

      DO 630 J=JF,JL
      SU(J)=P12(J)*(1.-C2)+1./TH*C2*C2P*F(J)*P12(J)
      SU(J)=SU(J)+G12(J)
C     SU(J)=SU(J)+(1.-0.5*C2)*G12(J)

      SP(J)=-C1*EDK(J)*UV(J)-1./TH*C1P*F(J)*EDK(J)*UV(J)
  630 CONTINUE

      CALL SOLVE(1,PRTUR(INUV),PRLAM(INUV),PRPTAU(INUV),UV,UVU)
      DO 631 J=2,NYM1
  631 UVS(J)=0.5*(UV(J)+UV(J+1))
      UVS(NYM1)=0.
      RETURN

      ENTRY CALCK
      JF=2
      JL=NYM1
c      JF=1
c      JL=NY
      IVAR=INK
c      CCOEF=1.7D0*USB*ALAMB*(VISS+VISP)/DELTA/(VISP+SMALL1)		!1.7*We/(1-Beta)/Re
      CCOEF=1.075D0		!1.7*We/(1-Beta)/Re - for Re=395, We=25 and Beta=0.9

      DO 480 J=JF,JL
c      SU(J)=PK(J) - 2.*XMUL(J)/RHO(J)*DSQTKDY(J)*DSQTKDY(J)	!FTP- Su=PK()-D
       SU(J)=PK(J) - 2.D0*(VISS)/RHO(J)*DSQTKDY(J)*
     1      DSQTKDY(J)+VISP/(ALAMB+SMALL1)*DFCUDY(J)/2.D0/RHO(J)     
       TEEV(J)=CCOEF*VISP/(ALAMB+SMALL1)/RHO(J)/2.D0*fCmm(J)*
     1      (TLN11(J)+TLN22(J)+TLN33(J))	                                        !FTP- Su=PK()-D-(d/dy(f*CV22)-f*NLTnn/2)

c       SU(J)=PK(J) - 2.D0*(VISS+XMUTP12(J))/RHO(J)*DSQTKDY(J)*
c     1      DSQTKDY(J)+VISP/(ALAMB+SMALL1)*(DFCUDY(J)-fCmm(J)*
c     1       (TLN11(J)+TLN22(J)+TLN33(J)))/RHO(J)/2.D0	                                        !FTP- Su=PK()-D-(d/dy(f*CV22)-f*NLTnn/2)
C      SP(J)=-TE(J)*XMUL(J)/(VISS+XMUTP12(J))	!TE is EpsilontildeNP
      SP(J)=-TE(J)-TEEV(J)	!TE is EpsilontildeNP
c      SP(J)=-TE(J)
  480 CONTINUE

      CALL SOLVE(1,PRTUR(INK),PRLAM(INK),PRPTAU(INK),TK,TKU)

C     BOUNDARY CONDITIONS 
      IF(MODEL.EQ.1) THEN
       TK(1)=0.		!low Re (may be unnecessary, but just in case)
       TK(NY)=0.			!low Re (may be unnecessary, but just in case)
      ENDIF

      RETURN

      ENTRY CALCE
      JF=2
      JL=NYM1
c      JF=1
c      JL=NY
      IVAR=INE

      DO 510 J=JF,JL
c       IF (J .LE. NYUX) REYT=RHO(J)*TK(J)*TK(J)/(TE(J)+SMALL1)/VISWALB	!FTP- turbulent Reynolds =k**2/eps/niuwall
c       IF (J .GT. NYUX) REYT=RHO(J)*TK(J)*TK(J)/(TE(J)+SMALL1)/VISWALT	!FTP- turbulent Reynolds =k**2/eps/niuwall       
       REYT=RHO(J)*TK(J)*TK(J)/(TE(J)+SMALL1)/XMUL(J)	!FTP- turbulent Reynolds =k**2/eps/niu
       FU2=1.D0-3.D-1*DEXP(-REYT*REYT)	!FTP- according to model of Nagano and Hishida (1987)
       FU1=1.D0	!FTP- according to model of Nagano and Hishida (1987)

       SU(J)=FU1*CD1*PK(J)*EDK(J)+(VISS)/RHO(J)*
     1       XMUT(J)/RHO(J)*(1.D0-FMU(J))*D2UDY2(J)*D2UDY2(J)		!FTP SU=SUNEWT + SUNEWTlowRe 

CCC  FROM OLD MODEL THE NEXT 8 INSTRUCTIONS
c      SU(J)=FU1*CD1*PK(J)*EDK(J)+XMUT(J)/RHO(J)/XMUL(J)/PRTUR(INE)*
c     1  DVISDY(J)*DEPSDY(J)+XMULLAM(J)/RHO(J)*XMUT(J)/RHO(J)*(1-FMU(J))*
c     1  D2UDY2(J)*D2UDY2(J)			!FTP SU=SUNEWT + NEW NN TERM + SUNEWTlowRe 

c      AAA= 0.0
c      IF (ISTEP .GT. 10000) AAA= 1.0	!switches on the new NN term
c      SU(J)=FU1*CD1*PK(J)*EDK(J)+AAA*XMUT(J)/RHO(J)/XMUL(J)/PRTUR(INE)*
c     1  DVISDY(J)*DEPSDY(J)+XMUL(J)/RHO(J)*XMUT(J)/RHO(J)*(1.-FMU(J))*
c     1  D2UDY2(J)*D2UDY2(J)			!FTP SU=SUNEWT + NEW NN TERM + SUNEWTlowRe 

      SP(J)=-FU2*CD2*EDK(J)*TE(J)
  510 CONTINUE

      CALL SOLVE(1,PRTUR(INE),PRLAM(INE),PRPTAU(INE),TE,TEU)

C     BOUNDARY CONDITIONS 

      IF(MODEL.EQ.1) THEN
       TE(1)= 0.			!low Re (may be unnecessary, but just in case)
       TE(NY)= 0.		!low Re (may be unnecessary, but just in case)
      ENDIF

      IF(MODEL.EQ.2) THEN
      CC=PK(2)*CD1/CD2
      TE(2)=.5*(CC+DSQRT(CC**2+4.*CE*TKS(2)**2*V2(2)/(CD2*Y(2)**2)))
      CC=PK(NYM1)*CD1/CD2
      TE(NYM1)=0.5*(CC+DSQRT(CC**2+4.*CE*TKS(NYM1)**2*V2(NYM1)/(CD2*
     +(Y(NY)-Y(NYM1))**2)))
      ENDIF

      RETURN
      END

C
      SUBROUTINE SOLVE(ISTG,PRNOT,PRNOL,PRNOSIJ,PHI,PHIU)
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'
      DIMENSION A(NCMAXY),B(NCMAXY),CC(NCMAXY),D(NCMAXY),AP(NCMAXY)
      DIMENSION PHI(NCMAXY),PHIU(NCMAXY)

      IF(ISTG.EQ.1) THEN

C     COEFFICIENTS FOR CELL NODES
      DO 62 J=1,NY
      GAM(J)=XMUT(J)/PRTUR(IVAR)+XMUL(J)/PRLAM(IVAR)+XMUTP12(J)*
     1  PRPTAU(IVAR)
          IF (IVAR .EQ. INK .OR. IVAR .EQ. INE) THEN	!variable turbulent diffusion - Nagano and Shimada (1993) + Park and Sung (1995)
             GAM(J)=XMUT(J)/PRTUR(IVAR)*FT(J)+XMUL(J)/PRLAM(IVAR)+
     1         XMUTP12(J)*PRPTAU(IVAR)
          END IF
   62 CONTINUE

      GAMS=0.
      GAMN=0.
c      IF(IVAR.EQ.INU) GAMS=DENSIT*USB*USB*YDIF(2)/U(2)			!Use at high Re. Almost true at low, but not true
c      IF(IVAR.EQ.INU) GAMN=DENSIT*UST*UST*(Y(NY)-Y(NYM1))/U(NYM1)	!Use at high Re. Almost true at low, but not true
      IF(IVAR.EQ.INU) GAMS=VISWALB	!True at low Re
      IF(IVAR.EQ.INU) GAMN=VISWALT	!True at low Re
C      IF(IVAR.EQ.INU) GAMS=VISCOS	!
C      IF(IVAR.EQ.INU) GAMN=VISCOS	!

      DO 600 J=JF,JL
  600 AX(J)=GX(J)*YSRU(J)

      AREA=DX*R(1)
      ALY=GY(2)*AREA
      TY=GAMS*AREA/YDIF(2)
      TY=DMAX1(TY,-ALY)
      CY=TY+ALY
      DO 605 J=JF,JL
      AYM(J)=CY
      VOL=DX*YSR(J)
      DU(J)=-VOL
      SU(J)=SU(J)*VOL*RHO(J)
      SP(J)=SP(J)*VOL*RHO(J)/(PHI(J)+SMALL1)
      AREA=RV(J+1)*DX
      ALY=5.D-1*GY(J+1)*AREA
      GAMM=5.D-1*(GAM(J)+GAM(J+1))
      IF(J.EQ.JL) GAMM=GAMN
      TY=GAMM*AREA/YDIF(J+1)
      TY=DMAX1(TY,DABS(ALY))
  607 AYP(J)=TY-ALY
      CY=TY+ALY
  605 CONTINUE

      ELSE
C     COEFFICIENTS FOR STAGGERED VARIABLES

      ENDIF

C*************** SOLUTION OF EQUATIONS **************
      URFVAR=URF(IVAR)
      DO 10 J=JF,JL
      SU(J)=SU(J)+AX(J)*PHIU(J)
      ASUM=AYM(J)+AYP(J)
      AP(J)=ASUM+AX(J)-SP(J)
      AP(J)=AP(J)/URFVAR
      SU(J)=SU(J)+(1.D0-URFVAR)*PHI(J)*AP(J)
      IF(IVAR.EQ.INU) DU(J)=DU(J)/AP(J)
   10 CONTINUE
      JST=JF-1
      A(JST)=0.D0
      CC(JST)=PHI(JST)
      DO 100 J=JF,JL
      A(J)=AYP(J)
      B(J)=AYM(J)
      CC(J)=SU(J)
      D(J)=AP(J)
      TERM=1.D0/(D(J)-B(J)*A(J-1)+SMALL1)
      A(J)=A(J)*TERM
  100 CC(J)=(CC(J)+B(J)*CC(J-1))*TERM
      DO 202 JJ=JF,JL
      J=JL+JST-JJ+1
  202 PHI(J)=A(J)*PHI(J+1)+CC(J)
      RETURN
      END

C
      SUBROUTINE OUTPUT
      INCLUDE 'PARAM2.INC'
      INCLUDE 'COMON3RES.ROT'
      DIMENSION UVM(NCMAXY),PDE(NCMAXY),USTAR(NCMAXY),UPB(NCMAXY),
     + VPB(NCMAXY),WPB(NCMAXY),U2T(NCMAXY),V2T(NCMAXY),W2T(NCMAXY)

C     LOCATE POSITION OF MAXIMUM VELOCITY

      NYUX=1
      DO 181 J=1,NY
  181 IF(U(J).GE.U(NYUX)) NYUX=J
      UMAX=U(NYUX)

      TAUWB=DENSIT*USB*USB
      TAUWT=DENSIT*UST*UST

      XND=XD/DIA
      YPL2=YPLS(2)
c      REB=DENSIT*UB*DIA/VISCOS			!for Newtonian
      VISWALB=DABS(TAUWB/DUDY(1))     !FTP- True wall viscosity 
      VISWALT=DABS(TAUWT/DUDY(NY))    !FTP- True wall viscosity
      REB=DENSIT*UB*DIA/VISWALB			!based on wall viscosity
c      REC=DENSIT*U(NYUX)*DELTA/VISCOS		!based on radius and maximum velocity (centreline)
      REC=DENSIT*U(NYUX)*DELTA/VISWALB			!based on radius, maximum vel (centreline) & wall viscosity
      FDARCY=DABS(DPDX(2))*DIA/DENSIT/UB/UB*2**(3-INGEOM)
      RO=OMEGA*DIA/UB
      USBC=USB/U(NYUX)
      USTC=UST/U(NYUX)
      CFB=2.*TAUWB/(DENSIT*U(NYUX)*U(NYUX))	!Cf based on maximum velocity
      CFT=2.*TAUWT/(DENSIT*U(NYUX)*U(NYUX))
      RETAU=DENSIT*USB*DELTA/VISWALB		!Retau based on wall viscosity
      RETAU0=DENSIT*USB*DELTA/(VISS+VISP+SMALL1)	!Retau based on zero-shear viscosity
      REMED=DENSIT*UB*DELTA/(VISS+VISP+SMALL1)		!Re based on bulk velocity and channel half-height
      WETAU=DENSIT*ALAMB*USB*USB/VISWALB
      WETAU0=DENSIT*ALAMB*USB*USB/(VISS+VISP+SMALL1)
      WEMED=ALAMB*UB/DELTA			!We based on bulk velocity and channel half-height
      DO 105 J=1,NY
        YPLS(J)=DENSIT*USB*Y(J)/VISWALB			!based on wall viscosity
        IF(J.GT.NYUX) YPLS(J)=DENSIT*UST*(Y(NY)-Y(J))/VISWALT
  105 TKS(J)=0.5*(U2(J)+V2(J)+W2(J))

c#################################################################
 
      WRITE(7,90)
      WRITE(7,91)CALFA0,CALFA1A,CALFA1B,CALFA2,CALFA3A,CALFA3B,CALFA4
      WRITE(7,93)CALFA5,CGAMA1,CGAMA2,CEXP3,CALFA14
      WRITE(7,92)CBETA0,CBETA1,CBETA2,CBETA3,CBETA4,CBETA5,CBETA6,CBETA7
      WRITE(7,*)
      WRITE(7,94)ALAMB,AL2,VISP,VISS,UREF,ICCOUNT
      WRITE(7,*)
      WRITE(7,101)ISTEP,XU,XD,DX,NYUX
      WRITE(7,107) DELTA,DIA,SMASS,REB,DPDX(2),U(NYUX),RO,REC,
     +      TAUWB,USB,CFB,USBC,TAUWT,UST,CFT,USTC,FDARCY,VISWALB,VISWALT
C      WRITE(7,*) 'FDM1=',FDM1,'  REG=',REG
      WRITE(7,95)RETAU,RETAU0,REMED,WETAU,WETAU0,WEMED

      WRITE(8,91)CALFA0,CALFA1A,CALFA1B,CALFA2,CALFA3A,CALFA3B,CALFA4
      WRITE(8,93)CALFA5,CGAMA1,CGAMA2,CEXP3,CALFA14
      WRITE(8,92)CBETA0,CBETA1,CBETA2,CBETA3,CBETA4,CBETA5,CBETA6,CBETA7
      WRITE(8,*)
      WRITE(8,94)ALAMB,AL2,VISP,VISS,UREF,ICCOUNT
      WRITE(8,*)
      WRITE(8,101)ISTEP,XU,XD,DX,NYUX
      WRITE(8,107) DELTA,DIA,SMASS,REB,DPDX(2),U(NYUX),RO,REC,
     +      TAUWB,USB,CFB,USBC,TAUWT,UST,CFT,USTC,FDARCY,VISWALB,VISWALT
C      WRITE(8,*) 'FDM1=',FDM1,'  REG=',REG
      WRITE(8,95)RETAU,RETAU0,REMED,WETAU,WETAU0,WEMED


      PK(1)=0.
      PK(NY)=0.
      RUM=1./U(NYUX)
      RUM2=RUM**2.
      RUT2=1./USB**2.

      DO 85 J=1,NY
      U2T(J)=U2(J)/UB**2.
      V2T(J)=V2(J)/UB**2.
      W2T(J)=W2(J)/UB**2.
c      UVM(J)=UV(J)*RUM2		!FTP normalised with centerline velocity
      UVM(J)=UV(J)*RUT2			!FTP normalised with friction velocity
      PDE(J)=PK(J)/(TE(J) + SMALL1)
      UUM(J)=U(J)/UB
      UPB(J)=DSQRT(U2(J))/UB
      VPB(J)=DSQRT(V2(J))/UB
      WPB(J)=DSQRT(W2(J))/UB

      UPLS(J)=U(J)/USB
      IF(J.GT.NYUX) UPLS(J)=U(J)/UST
      ARG=DMAX1(EWALL*YPLS(J),1.D-5)
      USTAR(J)=DLOG(ARG)/AK
      IF(YPLS(J).LT.11.5) USTAR(J)=YPLS(J)
      TEEV(J)=VISP/RHO(J)/(ALAMB+SMALL1)*fCmm(J)/2.*
     +        (TLN11(J)+TLN22(J)+TLN33(J))
      QV(J)=VISP/(ALAMB+SMALL1)*DFCUDY(J)/2.
      AM11(J)=2.D0*C12(J)*DUDY(J)
      AM12(J)=C22(J)*DUDY(J)
      TS12(J)=VISS*DUDY(J)
      SUMSTR(J)=(TS12(J)+TP12(J)-RHO(J)*UV(J))/(RHO(J)*USB*USB)
      TLNii(J)=(TLN11(J)+TLN22(J)+TLN33(J))*DELTA/USB
      CUii2(J)=(CU112(J)+CU222(J)+CU332(J))/USB
  85  CONTINUE

      IPRINT=1

      IF (IPRINT .EQ. 1) THEN
       WRITE(7,119)
       WRITE(7,220)(J,Y(J),U(J),TK(J),TE(J),TEE(J),DUDY(J),U2(J),
     +  V2(J),W2(J),UV(J),FMUNLT(J),ETA(J),UUM(J),TEEV(J),TK(J),TP12(J),
     +  TP11(J),TP22(J),TP33(J),YPLS(J),UPLS(J),XMUL(J),XMULM(J),
     +  C11(J),C22(J),C33(J),C12(J),C31(J),fCmm(J),CU112(J),CU122(J),
     +  CU222(J),CU332(J),UC22(J),VC22(J),QV(J),TLN11(J),TLN22(J),
     +  TLN33(J),TLN12(J),XMUTP12(J),AM11(J),AM12(J),TS12(J),
     +  SUMSTR(J),DU2DY(J),DV2DY(J),DW2DY(J),DUVDY(J),DC11DY(J),
     +  DC22DY(J),DC33DY(J),DC12DY(J),J=1,NY)

       WRITE(8,121)
        WRITE(8,221)(J,YPLS(J),UPLS(J),TK(J)/USB/USB,TE(J)*(VISS+VISP)/
     +  RHO(J)/USB/USB/USB/USB,TEE(J)*(VISS+VISP)/RHO(J)/USB/USB/
     +  USB/USB,DUDY(J)/USB*DELTA,UV(J)/USB/USB,FMUNLT(J),ETA(J),
     +  UUM(J),TEEV(J)*(VISS+VISP)/RHO(J)/USB/USB/USB/USB,TP12(J)/
     +  RHO(J)/USB/USB,TP11(J)/RHO(J)/USB/USB,TP22(J)/RHO(J)/USB/USB,
     +  TP33(J)/RHO(J)/USB/USB,
     +  XMUL(J),XMULM(J),C11(J),C22(J),C33(J),C12(J),C31(J),fCmm(J),
     +  CU112(J),CU122(J),CU222(J),CU332(J),CUii2(J),
     +  UC22(J),VC22(J),QV(J),TLN11(J)*DELTA/USB,TLN22(J)*DELTA/USB,
     +  TLN33(J)*DELTA/USB,TLNii(J),TLN12(J)*DELTA/USB,XMUTP12(J),
     +  AM11(J),AM12(J),TS12(J)/RHO(J)/USB/USB,SUMSTR(J),J=1,NY)


      ELSE
       WRITE(7,110)
       WRITE(7,111)(J,Y(J),U(J),TK(J),TE(J),TEE(J),DUDY(J),U2(J),
     +   V2(J),W2(J),UV(J),F(J),J=1,NY)
       WRITE(7,113)
       WRITE(7,114)(J,ETA(J),UUM(J),TEEV(J),TK(J),TP12(J),TP11(J),
     +   TP22(J),TP33(J),YPLS(J),UPLS(J),XMUL(J),XMULM(J),J=1,NY)
       WRITE(7,*)
       WRITE(7,115)
       WRITE(7,114)(J,C11(J),C22(J),C33(J),C12(J),C31(J),fCmm(J),
     +   CU112(J),CU122(J),CU222(J),CU332(J),UC22(J),VC22(J),J=1,NY)
       WRITE(7,*)
       WRITE(7,116)
       WRITE(7,117)(J,QV(J),TLN11(J),TLN22(J),TLN33(J),TLN12(J),
     +   XMUTP12(J),AM11(J),AM12(J),TS12(J),SUMSTR(J),J=1,NY)      
      END IF
      
      WRITE(7,*)			
      CLOSE(7)
      CLOSE(8)			

C----------------------------------FORMATS
   90 FORMAT(1H0,110(1H-))
   91 FORMAT(8HCALFA0 =,F7.3,3X,9HCALFA1A =,F7.3,3X,9HCALFA1B =,
     +   F7.3,3X,8HCALFA2 =,F7.3,3X,9HCALFA3A =,F7.3,3X,9HCALFA3B =,
     +   F7.3,3X,8HCALFA4 =,F9.7)
   92 FORMAT(8HCBETA0 =,F7.3,3x,8HCBETA1 =,F7.3,3X,8HCBETA2 =,F7.3,3X,
     +   8HCBETA3 =,F7.3,3X,8HCBETA4 =,F7.3,3X,8HCBETA5 =,F7.3,3X,
     +   8HCBETA6 =,F7.3,3X,8HCBETA7 =,F7.3)
   93 FORMAT(8HCALFA5 =,F7.3,3X,8HCGAMA1 =,F7.3,3X,8HCGAMA2 =,F7.3,3X,
     +   7HCEXP3 =,F9.7,3X,9HCALFA14 =,F9.7)
   94 FORMAT(8HLAMBDA =,F12.8,3X,6HL**2 =,F9.3,3X,6HVISP =,F9.6,3X,
     +   6HVISS =,F9.6,3X,6HUREF =,F8.5,3X,9HICCOUNT =,I9)
   95 FORMAT(7HReTau =,F10.2,3X,8HReTau0 =,F10.2,3X,7HRemed =,F10.2,3X,
     +   7HWeTau =,F10.2,3X,8HWetau0 =,F10.2,3X,7HWemed =,F10.2)
  101 FORMAT(8X,6HITER =,I6,8X,3HXU=,1PE11.3,8X,3HXD=,1PE11.3,
     +           8X,3HDX=,1PE11.3,7X,5HNYUX=,I3/)
  107 FORMAT(10X,7HDELTA =,1PE11.3,10X,7HDIAM  =,1PE11.3,10X,
     +  7HMASS  =,1PE11.3,10X,7HREBULK=,1PE11.3/
     + 10X,7HDPDX  =,1PE11.3,10X,7HUMAX  =,1PE11.3,10X,7HRO    =,
     + 1PE11.3,10X,7HREMX  =,1PE11.3/10X,7HTAUWB =,1PE11.3,10X,
     + 7HUSB   =,1PE11.3,10X,7HCFB   =,1PE11.3,9X,8HUSB/UMX=,1PE11.3/
     + 10X,7HTAUWT =,1PE11.3,10X,7HUST   =,1PE11.3,10X,7HCFT   =,
     + 1PE11.3,9X,8HUST/UMX=,1PE11.3/10X,7HFDARCY=,1PE11.3,9X,8HVISWALB=
     + ,1PE11.3,9X,8HVISWALT=,1PE11.3)
  110 FORMAT(//6X,10H         Y,10H         U,
     +10H         K,
     +      11H    EDtilde,10H        ED,10H      DUDY,
     +10H        U2,10H        V2,10H        W2,11H         UV,
     +10H         F)
  111 FORMAT(1X,I3,2X,1P11E11.3)

  113 FORMAT(//6X,10H     Y/Y.5,10H      U/UM,10H     TEEV ,
     +12H          TK,12H        TP12,12H        TP11,12H        TP22,
     +10H      TP33,11H         Y+,11H         U+,
     +11H       XMUL,10H     XMULM)

  115 FORMAT(//6X,10H       C11,10H       C22,10H      C33 ,
     +12H         C12,12H         C31,12H        fCmm,11H      CU112,
     +10H     CU122,11H      CU222,11H      CU332,
     +11H       UC22,10H      VC22)

  116 FORMAT(//6X,9H       QV,10H     TLN11,10H     TLN22,10H     TLN33,
     +12H       TLN12,12H     XMUTP12,11H        M11,11H        M12,
     +12H        TS12,12H  SUM STRESS)
  
  119 FORMAT(//3X,3H  I,10H         Y,10H         U,10H         K,
     + 13H      EDtilde,10H        ED,10H      DUDY,
     + 10H        U2,10H        V2,10H        W2,11H         UV,
     + 13H       FMUNLT,13H        Y/Y.5,10H      U/UM,12H       TEEV ,
     + 12H          TK,12H        TP12,12H        TP11,12H        TP22,
     + 10H      TP33,11H         Y+,11H         U+,
     + 11H       XMUL,10H     XMULM,10H       C11,12H         C22,
     + 10H      C33 ,12H         C12,12H         C31,12H        fCmm,
     + 11H      CU112,10H     CU122,11H      CU222,11H      CU332,
     + 11H       UC22,10H      VC22,11H         QV,13H        TLN11,
     + 10H     TLN22,12H       TLN33,12H       TLN12,12H     XMUTP12,
     + 11H        M11,11H        M12,12H        TS12,12H   SUMSTRESS,
     + 10H     DU2DY,11H      DV2DY,11H      DW2DY,11H      DUVDY,
     + 11H     DC11DY,12H      DC22DY,11H     DC33DY,11H     DC12DY)

  121 FORMAT(//3X,3H  I,10H        Y+,10H        U+,10H        K+,
     + 13H     EDtilde+,10H       ED+,10H    DU+DY*,
     + 11H        UV+,
     + 13H       FMUNLT,13H        Y/Y.5,10H      U/UM,12H      TEEV+ ,
     + 12H       TP12+,12H       TP11+,12H       TP22+,
     + 10H     TP33+,
     + 11H       XMUL,13H        XMULM,10H       C11,12H         C22,
     + 13H         C33 ,12H         C12,12H         C31,12H        fCmm,
     + 15H          CU112,11H      CU122,12H       CU222,11H      CU332,
     + 13H       CUii2+,13H         UC22,12H        VC22,
     + 11H         QV,13H       TLN11*,13H       TLN22*,
     + 12H      TLN33*,13H       TLNii*,12H      TLN12*,12H     XMUTP12,
     + 11H        M11,11H        M12,12H       TS12+,15H     SUMSTRESS+)


  114  FORMAT(1X,I3,2X,1P12E11.3)
  117  FORMAT(1X,I3,2X,1P10E11.3)
  220  FORMAT(1X,I3,2X,1P11E11.3,2X,1P12E11.3,2X,1P12E11.3,2X,1P18E11.3)
  221  FORMAT(1X,I3,2X,1P11E11.3,2X,1P12E12.3,2X,1P14E12.3,2X,1P4E11.3)
  878  FORMAT(1X,5H RO= ,F7.4,5X,5HREB= ,F8.1,5X,5HREC= ,F8.1)
       RETURN
       END


