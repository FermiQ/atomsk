MODULE duplicate
!
!**********************************************************************************
!*  DUPLICATE                                                                     *
!**********************************************************************************
!* This module reads atomic coordinates from an array P, and                      *
!* expands it along the directions of the cell vectors.                           *
!**********************************************************************************
!* (C) March 2010 - Pierre Hirel                                                  *
!*     Université de Lille, Sciences et Technologies                              *
!*     UMR CNRS 8207, UMET - C6, F-59655 Villeneuve D'Ascq, France                *
!*     pierre.hirel@univ-lille.fr                                                 *
!* Last modification: P. Hirel - 26 Feb. 2025                                     *
!**********************************************************************************
!* This program is free software: you can redistribute it and/or modify           *
!* it under the terms of the GNU General Public License as published by           *
!* the Free Software Foundation, either version 3 of the License, or              *
!* (at your option) any later version.                                            *
!*                                                                                *
!* This program is distributed in the hope that it will be useful,                *
!* but WITHOUT ANY WARRANTY; without even the implied warranty of                 *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                  *
!* GNU General Public License for more details.                                   *
!*                                                                                *
!* You should have received a copy of the GNU General Public License              *
!* along with this program.  If not, see <http://www.gnu.org/licenses/>.          *
!**********************************************************************************
!
USE comv
USE constants
USE messages
USE files
USE subroutines
!
CONTAINS
!
!
SUBROUTINE DUPLICATECELL(H,P,S,dupmatrix,SELECT,AUX)
!
!
IMPLICIT NONE
CHARACTER(LEN=128):: msg
LOGICAL:: doshells
LOGICAL,DIMENSION(:),ALLOCATABLE:: SELECT, newSELECT  !mask for atom list
INTEGER(il):: i, newNP
INTEGER:: m, n, o, qi
INTEGER:: Nmem  !total number of atoms loaded in memory
INTEGER, DIMENSION(3):: dupmatrix
REAL(dp):: tempreal
REAL(dp),DIMENSION(3,3),INTENT(INOUT):: H   !Base vectors of the supercell
REAL(dp),DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT):: P, S
REAL(dp),DIMENSION(:,:),ALLOCATABLE:: Q, T
REAL(dp),DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT):: AUX !auxiliary properties
REAL(dp),DIMENSION(:,:),ALLOCATABLE:: newAUX !auxiliary properties
!
!Initialize variables
i = 0
qi = 0
Nmem = SIZE(P,1)
IF(ALLOCATED(Q)) DEALLOCATE(Q)
IF(ALLOCATED(T)) DEALLOCATE(T)
IF(ALLOCATED(newAUX)) DEALLOCATE(newAUX)
IF( ALLOCATED(S) .AND. SIZE(S,1).NE.0 ) THEN
  doshells=.TRUE.
  Nmem = Nmem+SIZE(S,1)
ELSE
  doshells=.FALSE.
ENDIF
!
CALL ATOMSK_MSG(2066,(/''/),(/ DBLE(dupmatrix(1)), &
     & DBLE(dupmatrix(2)), DBLE(dupmatrix(3)) /))
!
!If expansion is zero along a direction, correct it
DO i=1,3
  IF( dupmatrix(i)==0) THEN
    dupmatrix(i)=1
  ENDIF
ENDDO

!If all dimensions are set to 1 then the system stays the same
IF( dupmatrix(1)==1 .AND. dupmatrix(2)==1 .AND.                 &
  & dupmatrix(3)==1                                ) THEN
  nwarn=nwarn+1
  CALL ATOMSK_MSG(2728,(/''/),(/0.d0/))
  GOTO 1000
ENDIF
!
!
!
200 CONTINUE
IF( .NOT.ALLOCATED(P) .OR. SIZE(P)<=0 ) THEN
  !No atom in system: can not apply option
  newNP = 0
  GOTO 400
ENDIF
!
IF( .NOT.ALLOCATED(SELECT) ) THEN
  !All atoms must be duplicated
  tempreal = DBLE(SIZE(P,1))*DABS(DBLE(dupmatrix(1))*DBLE(dupmatrix(2))*DBLE(dupmatrix(3)))
ELSE
  !Only selected atoms will be duplicated
  !Count how many atoms are selected in the original system
  qi=0
  DO i=1,SIZE(SELECT)
    IF(IS_SELECTED(SELECT,i)) qi=qi+1
  ENDDO
  tempreal = DBLE(SIZE(P,1)) + DBLE(qi)*( DABS(DBLE(dupmatrix(1))*DBLE(dupmatrix(2))*DBLE(dupmatrix(3))) - 1.d0)
ENDIF
WRITE(msg,*) "new NP = ", tempreal
CALL ATOMSK_MSG(999,(/msg/),(/0.d0/))
!
IF( tempreal<=0 .OR. tempreal>NATOMS_MAX ) THEN
  !Exceeds number of atoms that can be treated
  nerr=nerr+1
  CALL ATOMSK_MSG(821,(/''/),(/tempreal/))
  GOTO 1000
ENDIF
!
newNP = NINT(tempreal)
Nmem = Nmem+newNP
ALLOCATE(Q(newNP,4),STAT=i)
IF( i>0 ) THEN
  ! Allocation failed (not enough memory)
  nerr = nerr+1
  CALL ATOMSK_MSG(819,(/''/),(/DBLE(Nmem)/))
  GOTO 1000
ENDIF
IF(ALLOCATED(AUX)) THEN
  Nmem = Nmem + CEILING( DBLE(newNP*SIZE(AUX,2))/4.d0 )
  ALLOCATE( newAUX(newNP,SIZE(AUX,2) ) , STAT=i )
  IF( i>0 ) THEN
    ! Allocation failed (not enough memory)
    nerr = nerr+1
    CALL ATOMSK_MSG(819,(/''/),(/DBLE(Nmem)/))
    GOTO 1000
  ENDIF
ENDIF
!CALL ATOMSK_MSG(2067,(/''/),(/DBLE(newNP)/))
!
IF( doshells ) THEN
  Nmem = Nmem+newNP
  ALLOCATE(T(newNP,4) , STAT=i)
  IF( i>0 ) THEN
    ! Allocation failed (not enough memory)
    nerr = nerr+1
    CALL ATOMSK_MSG(819,(/''/),(/DBLE(Nmem)/))
    GOTO 1000
  ENDIF
ENDIF
!
!
qi = 0
DO o = 0 , dupmatrix(3)-SIGN(1,dupmatrix(3)) , SIGN(1,dupmatrix(3))
  DO n = 0 , dupmatrix(2)-SIGN(1,dupmatrix(2)) , SIGN(1,dupmatrix(2))
    DO m = 0 , dupmatrix(1)-SIGN(1,dupmatrix(1)) , SIGN(1,dupmatrix(1))
      DO i=1,SIZE(P,1)
        IF( IS_SELECTED(SELECT,i) .OR. (o==0.AND.n==0.AND.m==0) ) THEN
          qi = qi+1
          IF( qi>SIZE(Q,1) ) THEN
            nerr = nerr+1
            CALL ATOMSK_MSG(4821,(/''/),(/DBLE(qi),DBLE(SIZE(Q,1))/))
            GOTO 1000
          ELSE
            Q(qi,1) = P(i,1)*SIGN(1,dupmatrix(1)) + DBLE(m)*H(1,1) + DBLE(n)*H(2,1) + DBLE(o)*H(3,1)
            Q(qi,2) = P(i,2)*SIGN(1,dupmatrix(2)) + DBLE(m)*H(1,2) + DBLE(n)*H(2,2) + DBLE(o)*H(3,2)
            Q(qi,3) = P(i,3)*SIGN(1,dupmatrix(3)) + DBLE(m)*H(1,3) + DBLE(n)*H(2,3) + DBLE(o)*H(3,3)
            Q(qi,4) = P(i,4)
            !Duplicated particles will have same auxiliary properties as the originals
            IF(ALLOCATED(newAUX)) newAUX(qi,:) = AUX(i,:)
            !Also duplicate shells if any
            IF( doshells ) THEN
              T(qi,1) = S(i,1)*SIGN(1,dupmatrix(1)) + DBLE(m)*H(1,1) + DBLE(n)*H(2,1) + DBLE(o)*H(3,1)
              T(qi,2) = S(i,2)*SIGN(1,dupmatrix(2)) + DBLE(m)*H(1,2) + DBLE(n)*H(2,2) + DBLE(o)*H(3,2)
              T(qi,3) = S(i,3)*SIGN(1,dupmatrix(3)) + DBLE(m)*H(1,3) + DBLE(n)*H(2,3) + DBLE(o)*H(3,3)
              T(qi,4) = S(i,4)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO
!
!Replace old P with the new Q
DEALLOCATE(P)
ALLOCATE(P(SIZE(Q,1),4) , STAT=i)
IF( i>0 ) THEN
  ! Allocation failed (not enough memory)
  nerr = nerr+1
  CALL ATOMSK_MSG(819,(/''/),(/DBLE(Nmem)/))
  GOTO 1000
ENDIF
P = Q
DEALLOCATE(Q)
!Replace old AUX by newAUX
IF(ALLOCATED(AUX)) THEN
  DEALLOCATE(AUX)
  ALLOCATE( AUX( SIZE(newAUX,1), SIZE(newAUX,2) ) , STAT=i )
  IF( i>0 ) THEN
    ! Allocation failed (not enough memory)
    nerr = nerr+1
    CALL ATOMSK_MSG(819,(/''/),(/DBLE(Nmem)/))
    GOTO 1000
  ENDIF
  AUX = newAUX
  IF(ALLOCATED(newAUX)) DEALLOCATE(newAUX)
ENDIF
!
!Replace old S with the new T
IF( doshells) THEN
  DEALLOCATE(S)
  ALLOCATE(S(SIZE(T(:,1)),4) , STAT=i)
  IF( i>0 ) THEN
    ! Allocation failed (not enough memory)
    nerr = nerr+1
    CALL ATOMSK_MSG(819,(/''/),(/DBLE(Nmem)/))
    GOTO 1000
  ENDIF
  S(:,:) = T(:,:)
  DEALLOCATE(T)
ENDIF
!
!Increase the size of SELECT array
!NOTE: atoms that were previously selected will still be selected after that,
!     but their replicas WILL NOT be selected
IF( ALLOCATED(SELECT) ) THEN
  ALLOCATE( newSELECT(SIZE(P,1))  , STAT=i)
  IF( i>0 ) THEN
    ! Allocation failed (not enough memory)
    nerr = nerr+1
    CALL ATOMSK_MSG(819,(/''/),(/DBLE(Nmem)/))
    GOTO 1000
  ENDIF
  newSELECT(:) = .FALSE.
  DO i=1,SIZE(SELECT)
    newSELECT(i) = SELECT(i)
  ENDDO
  DEALLOCATE(SELECT)
  ALLOCATE(SELECT(SIZE(P,1)))
  SELECT(:) = newSELECT(:)
  DEALLOCATE(SELECT)
ENDIF
!
!
!
400 CONTINUE
!Resize the cell dimensions
H(1,:) = dupmatrix(1)*H(1,:)
H(2,:) = dupmatrix(2)*H(2,:)
H(3,:) = dupmatrix(3)*H(3,:)
!
!
!
500 CONTINUE
CALL ATOMSK_MSG(2068,(/''/),(/DBLE(newNP)/))
GOTO 1000
!
!
!
800 CONTINUE
CALL ATOMSK_MSG(802,(/''/),(/DBLE(i)/))
nerr = nerr+1
!
!
!
1000 CONTINUE
IF(ALLOCATED(Q)) DEALLOCATE(Q)
IF(ALLOCATED(T)) DEALLOCATE(T)
IF(ALLOCATED(newAUX)) DEALLOCATE(newAUX)
!
!
!
END SUBROUTINE DUPLICATECELL
!
!
END MODULE duplicate
