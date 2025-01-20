MODULE math
!
!**********************************************************************************
!*  MATH                                                                          *
!**********************************************************************************
!* This module contains functions and subroutines performing mathematical         *
!* operations.                                                                    *
!**********************************************************************************
!* (C) March 2018 - Pierre Hirel                                                  *
!*     Université de Lille, Sciences et Technologies                              *
!*     UMR CNRS 8207, UMET - C6, F-59655 Villeneuve D'Ascq, France                *
!*     pierre.hirel@univ-lille.fr                                                 *
!* Last modification: P. Hirel - 12 Dec. 2024                                     *
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
!* List of functions in this file:                                                *
!* IS_INTEGER          determines if a real number is an integer                  *
!* VECLENGTH           calculates the length of a vector                          *
!* VEC_PLANE           determines if a point is above or below a plane            *
!* VEC_ANGLE           computes angle between 2 vectors                           *
!* VECMAT              computes the product of a row and column vectors           *
!* GCD                 calculates the greatest common divisor of two integers     *
!* ANGVEC              calculates angle between 2 vectors                         *
!* DEG2RAD             converts angles from degrees to radians                    *
!* RAD2DEG             converts angles from radians to degrees                    *
!* ORTHOVEC            checks if two vectors are normal to each other             *
!* CROSS_PRODUCT       calculates the cross product of two vectors                *
!* SCALAR_TRIPLE_PRODUCT computes scalar triple product ez (ex ^ ey).             *
!* ROTMAT_AXIS         provides the matrix for rotation of angle around axis      *
!* ROTMAT_VECTORS      provides the matrix for rotation between 2 vectors         *
!* EPS_LEVI_CIVITA     calculates the Levi-Civita symbol, given i,j,k             *
!* MATTRACE            computes the trace of a NxN matrix                         *
!* MATDET              computes the determinant of a 3x3 matrix                   *
!* List of subroutines in this file:                                              *
!* INVMAT              inverts a NxN matrix                                       *
!* CONVMAT             converts conventional vectors into a matrix                *
!* MATCONV             converts matrix into conventional vectors                  *
!* VOLUME_PARA         computes the volume of a parallelepiped                    *
!* DERIVATIVE          calculate the derivative of a function                     *
!* EULER2MAT           converts Euler angles into a rotation matrix               *
!* MAT2EULER           converts rotation matrix into Euler angles                 *
!* IS_ROTMAT           returns TRUE if a rotation matrix, FALSE otherwise         *
!**********************************************************************************
!
!
USE comv
USE constants
!
!
CONTAINS
!
!
!
!********************************************************
!  IS_INTEGER
!  This function determines if a real number can be
!  interpreted as an integer. For instance 1.d0, 2.d0
!  will return TRUE, but 1.0001d0 will return FALSE.
!********************************************************
LOGICAL FUNCTION IS_INTEGER(number,th)
!
IMPLICIT NONE
REAL(dp),INTENT(IN):: number
REAL(dp),INTENT(IN):: th     !threshold to decide if it is an integer
!
IS_INTEGER = .FALSE.
!
IF( DABS( DBLE(NINT(number)) - number ) < th ) THEN
  IS_INTEGER = .TRUE.
ENDIF
!
END FUNCTION IS_INTEGER
!
!
!********************************************************
!  VECLENGTH
!  This function calculates the length of a vector.
!********************************************************
FUNCTION VECLENGTH(V) RESULT(Vlength)
!
IMPLICIT NONE
REAL(dp),DIMENSION(3),INTENT(IN):: V
REAL(dp):: Vlength
!
Vlength = DSQRT(DABS( V(1)*V(1) + V(2)*V(2) + V(3)*V(3) ))
!
END FUNCTION VECLENGTH
!
!
!********************************************************
!  VEC_PLANE
!  This function determines if a point of coordinates P
!  is above or below a given plane. The plane is defined
!  by its normal N and its distance to the cartesian
!  origin (0,0,0). The result "position" is positive if
!  the point is above the plane, negative if it is below,
!  and zero if P is exactly in the plane. The expressions
!  "above" and "below" mean that the point P is at a
!  greater and smaller distance from the origin than
!  the plane, respectively.
!********************************************************
FUNCTION VEC_PLANE(N,d0,P) RESULT(position)
!
IMPLICIT NONE
REAL(dp), DIMENSION(3),INTENT(IN):: N  !normal to the plane (may not be unit vector)
REAL(dp), DIMENSION(3),INTENT(IN):: P  !position of the point
REAL(dp),INTENT(IN):: d0  !distance between the plane and the origin
REAL(dp):: position  !>0 if P is above plane, <0 if below, =0 if in plane
REAL(dp),DIMENSION(3):: normal  !unit vector normal to the plane
!
IF( d0==0.d0 .OR. VECLENGTH(N)==0.d0 ) THEN
  !atom has to be in plane
  position = 0.d0
ELSE
  normal(:) = N(:) / VECLENGTH(N)
  position = DOT_PRODUCT( normal(:) , P - d0*normal(:) )
ENDIF
!
END FUNCTION VEC_PLANE
!
!
!********************************************************
!  GCD
!  This function calculates the Greatest Common Divisor
!  of two integers.
!********************************************************
RECURSIVE FUNCTION GCD(n,m) RESULT(o)
!
IMPLICIT NONE
INTEGER, INTENT(IN) :: n,m
INTEGER:: o
INTEGER:: r
!
IF(m*n.NE.0) THEN
  r = MODULO(n,m)
  IF(r == 0) THEN
    o = m
  ELSE
    o = GCD(m,r)
  ENDIF
ELSE
  IF(m==0 .AND. n==0) THEN
    o=1
  ELSE
    o = MAX(n,m)
  ENDIF
ENDIF
!
END FUNCTION GCD
!
!
!********************************************************
!  ANGVEC
!  This function calculates the angle between two vectors
!  V1 and V2. Angle theta is returned in radians.
!********************************************************
FUNCTION ANGVEC(V1,V2) RESULT(theta)
!
IMPLICIT NONE
REAL(dp),DIMENSION(3),INTENT(IN):: V1, V2
REAL(dp):: theta
!
theta = DOT_PRODUCT(V1,V2) /                                    &
      & ( DSQRT(DOT_PRODUCT(V1,V1))*DSQRT(DOT_PRODUCT(V2,V2)) )
!
IF(theta>1.d0) THEN
  theta = 1.d0
ELSEIF(theta<-1.d0) THEN
  theta = -1.d0
ENDIF
!
theta = DACOS(theta)
!
RETURN
!
END FUNCTION ANGVEC
!
!
!********************************************************
! VEC_ANGLE
! Angles between vectors u1 and u2, oriented
! according to normal n
!********************************************************
FUNCTION VEC_ANGLE(u1, u2, n) RESULT(phi)

IMPLICIT NONE
REAL(dp),DIMENSION(3),INTENT(IN):: u1, u2, n
REAL(dp):: phi
!
phi = SIGN( ACOS( DOT_PRODUCT(u1, u2)/ &
    &  ( VECLENGTH(u1)*VECLENGTH(u2) ) ), SCALAR_TRIPLE_PRODUCT(u1,u2,n) )
!
END FUNCTION VEC_ANGLE
!
!
!********************************************************
! VECMAT
! This function takes in two vectors u1 and u2,
! assuming that u1 is a row vector and u2 a column vector,
! multiplies them and returns the resulting matrix m.
!********************************************************
FUNCTION VECMAT(u1,u2) RESULT(m)

IMPLICIT NONE
INTEGER:: i, j
REAL(dp),DIMENSION(3),INTENT(IN):: u1, u2
REAL(dp),DIMENSION(3,3):: m
!
DO i=1,3
  DO j=1,3
    m(i,j) = u1(i)*u2(j)
  ENDDO
ENDDO
!
END FUNCTION VECMAT
!
!
!********************************************************
!  DEG2RAD
!  This function converts angles from degrees to radians
!********************************************************
FUNCTION DEG2RAD(angdeg) RESULT(angrad)
!
IMPLICIT NONE
REAL(dp):: angdeg, angrad
!
angrad = angdeg*pi/180.d0
!
RETURN
!
END FUNCTION DEG2RAD
!
!
!********************************************************
!  RAD2DEG
!  This function converts angles from radians to degrees
!********************************************************
FUNCTION RAD2DEG(angrad) RESULT(angdeg)
!
IMPLICIT NONE
REAL(dp):: angdeg, angrad
!
angdeg = angrad*180.d0 / pi
!
RETURN
!
END FUNCTION RAD2DEG
!
!
!********************************************************
! ORTHOVEC
! This function returns .TRUE. if two vectors are
! orthogonal, and .FALSE. otherwise.
!********************************************************
LOGICAL FUNCTION ORTHOVEC(V1,V2)
!
IMPLICIT NONE
REAL(dp),DIMENSION(3),INTENT(IN):: V1, V2
!
ORTHOVEC = .FALSE.
IF(DOT_PRODUCT(V1,V2)==0.d0) THEN
  ORTHOVEC = .TRUE.
ENDIF
!
END FUNCTION ORTHOVEC
!
!
!********************************************************
!  CROSS_PRODUCT
!  This function calculates the cross product
!  of two vectors.
!********************************************************
FUNCTION CROSS_PRODUCT(V1,V2) RESULT(V3)
!
IMPLICIT NONE
REAL(dp), DIMENSION(3),INTENT(IN):: V1, V2
REAL(dp), DIMENSION(3):: V3
!
V3(1) = V1(2)*V2(3)-V1(3)*V2(2)
V3(2) = V1(3)*V2(1)-V1(1)*V2(3)
V3(3) = V1(1)*V2(2)-V1(2)*V2(1)
!
RETURN
!
END FUNCTION CROSS_PRODUCT
!
!
!********************************************************
!  SCALAR_TRIPLE_PRODUCT
!  This function computes the scalar triple product
!  of three vectors: ez (ex ^ ey).
!********************************************************

FUNCTION SCALAR_TRIPLE_PRODUCT(ex,ey,ez) RESULT(stp)

IMPLICIT NONE
REAL(dp),DIMENSION(3),INTENT(IN):: ex, ey, ez
REAL(dp):: stp

stp = ez(1) *( ex(2)*ey(3) - ex(3)*ey(2) ) &
    + ez(2) *( ex(3)*ey(1) - ex(1)*ey(3) ) &
    + ez(3) *( ex(1)*ey(2) - ex(2)*ey(1) )

END FUNCTION SCALAR_TRIPLE_PRODUCT
!
!
!********************************************************
!  ROTMAT_AXIS
!  This function provides the rotation matrix
!  corresponding to a rotation of a given angle
!  around the given axis.
!********************************************************
!
FUNCTION ROTMAT_AXIS(axis,angle) RESULT(rot_matrix)
!
IMPLICIT NONE
REAL(dp),INTENT(IN):: angle  !angle of rotation (degrees)
REAL(dp),DIMENSION(3),INTENT(IN):: axis !coordinates (x,y,z) of rotation axis
REAL(dp),DIMENSION(3):: axis_u !unit vector directed along the axis
REAL(dp),DIMENSION(3,3):: rot_matrix !final rotation matrix
REAL(dp):: c, s
!
 c=DCOS(DEG2RAD(angle))
 s=DSIN(DEG2RAD(angle))
!
axis_u(:) = axis(:) / VECLENGTH(axis)
!
rot_matrix(:,:) = 0.d0
!
!ux²*(1-c) + c
rot_matrix(1,1) = (1.d0-c)*axis_u(1)**2 + c
!ux*uy*(1-c) - uz*s
rot_matrix(1,2) = (1.d0-c)*axis_u(1)*axis_u(2) - s*axis_u(3)
!ux*uz*(1-c) + uy*s
rot_matrix(1,3) = (1.d0-c)*axis_u(1)*axis_u(3) + s*axis_u(2)
!
!ux*uy*(1-c) + uz*s
rot_matrix(2,1) = (1.d0-c)*axis_u(1)*axis_u(2) + s*axis_u(3)
!uy²*(1-c) + c
rot_matrix(2,2) = (1.d0-c)*axis_u(2)**2 + c
!uy*uz*(1-c) - ux*s
rot_matrix(2,3) = (1.d0-c)*axis_u(2)*axis_u(3) - s*axis_u(1)
!
!ux*uz*(1-c) - uy*s
rot_matrix(3,1) = (1.d0-c)*axis_u(1)*axis_u(3) - s*axis_u(2)
!uy*uz*(1-c) + ux*s
rot_matrix(3,2) = (1.d0-c)*axis_u(2)*axis_u(3) + s*axis_u(1)
!uz²*(1-c) + c
rot_matrix(3,3) = (1.d0-c)*axis_u(3)**2 + c
!
!
END FUNCTION ROTMAT_AXIS
!
!
!********************************************************
!  ROTMAT_VECTORS
!  This function provides the rotation matrix
!  corresponding to a rotation from a vector a
!  to a vector b.
!********************************************************
!
FUNCTION ROTMAT_VECTORS(a,b) RESULT(rot_matrix)
!
IMPLICIT NONE
INTEGER:: i
REAL(dp),DIMENSION(3),INTENT(IN):: a, b  !input vectors
REAL(dp),DIMENSION(3):: an, bn  !normalized vectors
REAL(dp),DIMENSION(3):: v
REAL(dp),DIMENSION(3,3):: vx, vx2
REAL(dp),DIMENSION(3,3):: rot_matrix !final rotation matrix
REAL(dp):: c    !cosinus
!
!
rot_matrix(:,:) = 0.d0
DO i=1,3
  rot_matrix(i,i) = 1.d0
ENDDO
!
!Normalize vectors
an(:) = a(:) / VECLENGTH(a)
bn(:) = b(:) / VECLENGTH(b)
 c = DOT_PRODUCT(an,bn)
 !c=DCOS(ANGVEC(a,b))
!cosinus must be different from -1 to proceed
!Otherwise just return identity matrix
IF( DABS(c+1.d0) > 1.d-12 ) THEN
  !Compute cross-product of vectors a and b
  v(:) = CROSS_PRODUCT(an,bn)
  !Compute skew-symmetric matrix
  vx(:,:) = 0.d0
  vx(1,2) = -v(3)
  vx(1,3) = v(2)
  vx(2,1) = v(3)
  vx(2,3) = -v(1)
  vx(3,1) = -v(2)
  vx(3,2) = v(1)
  !Compute square of vx
  vx2 = MATMUL(vx,vx)
  !Compute final rotation matrix
  rot_matrix(:,:) = rot_matrix(:,:) + vx(:,:) + vx2(:,:)/(1.d0+c)
ENDIF
!
!
END FUNCTION ROTMAT_VECTORS
!
!
!********************************************************
! EPS_LEVI_CIVITA
! This function computes the Levi-Civita symbol.
!********************************************************
FUNCTION EPS_LEVI_CIVITA(i,j,k) RESULT(eps_ijk)
!
IMPLICIT NONE
INTEGER:: i, j, k, eps_ijk
!
eps_ijk=0
!
IF (i==1 .AND. j==2 .AND. k==3) eps_ijk=1
IF (i==2 .AND. j==3 .AND. k==1) eps_ijk=1
IF (i==3 .AND. j==1 .AND. k==2) eps_ijk=1
!
IF (i==3 .AND. j==2 .AND. k==1) eps_ijk=-1
IF (i==1 .AND. j==3 .AND. k==2) eps_ijk=-1
IF (i==2 .AND. j==1 .AND. k==3) eps_ijk=-1
!
END FUNCTION EPS_LEVI_CIVITA
!
!
!********************************************************
! MATTRACE
! This function computes the trace of a NxN matrix.
!********************************************************
FUNCTION MATTRACE(A) RESULT(Tr)
!
!
REAL(dp),DIMENSION(:,:),INTENT(IN):: A(:,:)
REAL(dp):: Tr
!
Tr = 0.d0
DO i=1,MIN(SIZE(A,1),SIZE(A,2))
  Tr = Tr + A(i,i)
ENDDO
!
END FUNCTION MATTRACE
!
!
!********************************************************
! MATDET
! This function computes the determinant of a 3x3 matrix.
!********************************************************
FUNCTION MATDET(A) RESULT(DET)
!
!
REAL(dp),DIMENSION(3,3),INTENT(IN):: A(:,:)
REAL(dp):: DET
!
DET =   A(1,1)*A(2,2)*A(3,3) &
    & + A(1,2)*A(2,3)*A(3,1) &
    & + A(1,3)*A(2,1)*A(3,2) &
    & - A(3,1)*A(2,2)*A(1,3) &
    & - A(3,2)*A(2,3)*A(1,1) &
    & - A(3,3)*A(2,1)*A(1,2)
!
END FUNCTION MATDET
!
!
!********************************************************
! INVMAT
! This subroutine inverts a NxN matrix M
! and outputs the result into the matrix G.
!********************************************************
SUBROUTINE INVMAT(M,G,status)
!
IMPLICIT NONE
REAL(dp),DIMENSION(:,:),INTENT(IN):: M
REAL(dp),DIMENSION(:,:),INTENT(OUT):: G
INTEGER,INTENT(OUT),OPTIONAL:: status
INTEGER:: i
INTEGER,DIMENSION(SIZE(M,1)):: IPIV !for LAPACK routine DGETRI
REAL(dp):: det
REAL(dp),DIMENSION(SIZE(M,1)):: WORK !for LAPACK routine DGETRI
!
i=0
!
IF( SIZE(M,1).NE.SIZE(M,2) ) THEN
  !Non-square matrix: cancel
  i=1
  !
ELSE
  IF( SIZE(M,1)==3 .AND. SIZE(M,2)==3 ) THEN
    !3x3 matrix: simple enough, let's do it by hand
    det =   M(1,1)*M(2,2)*M(3,3) - M(1,1)*M(3,2)*M(2,3) &
        & - M(2,1)*M(1,2)*M(3,3) + M(2,1)*M(3,2)*M(1,3) &
        & + M(3,1)*M(1,2)*M(2,3) - M(3,1)*M(2,2)*M(1,3)
    !
    G(1,1) = (M(2,2)*M(3,3) - M(2,3)*M(3,2))/det
    G(2,1) = (M(2,3)*M(3,1) - M(2,1)*M(3,3))/det
    G(3,1) = (M(2,1)*M(3,2) - M(2,2)*M(3,1))/det
    !
    G(1,2) = (M(3,2)*M(1,3) - M(3,3)*M(1,2))/det
    G(2,2) = (M(3,3)*M(1,1) - M(3,1)*M(1,3))/det
    G(3,2) = (M(3,1)*M(1,2) - M(3,2)*M(1,1))/det
    !
    G(1,3) = (M(1,2)*M(2,3) - M(1,3)*M(2,2))/det
    G(2,3) = (M(1,3)*M(2,1) - M(1,1)*M(2,3))/det
    G(3,3) = (M(1,1)*M(2,2) - M(1,2)*M(2,1))/det
    !
  ELSEIF( SIZE(G,1)==SIZE(M,1) .AND. SIZE(G,2)==SIZE(M,2) ) THEN
    !general NxN matrix: call LAPACK routines DGETRF and DGETRI
    G(:,:) = M(:,:)
    CALL DGETRF( SIZE(M,1), SIZE(M,2), G, SIZE(M,1), IPIV, i)
    IF( i==0 ) THEN
      CALL DGETRI( SIZE(M,1), G, SIZE(M,1), IPIV, WORK, SIZE(M,1), i )
    ENDIF
    !
  ELSE
    !non-consistent array sizes: some programmer
    !made a mistake when calling this routine
    i=1
  ENDIF
  !
ENDIF
!
IF(PRESENT(status)) status=i
!
END SUBROUTINE INVMAT
!
!
!********************************************************
!  CONVMAT
!  This subroutine converts conventional vectors
!  defined by a b c alpha beta gamma,
!  into a lower triangular matrix:
!        |  H(1,1)    0       0     |
!   H =  |  H(2,1)  H(2,2)    0     |
!        |  H(3,1)  H(3,2)  H(3,3)  |
!********************************************************
!
SUBROUTINE CONVMAT(a,b,c,alpha,beta,gamma,H)
!
IMPLICIT NONE
REAL(dp),INTENT(IN):: a, b, c, alpha, beta, gamma
REAL(dp),DIMENSION(3,3):: H
!
H(:,:) = 0.d0
H(1,1) = a
H(2,1) = b*DCOS(gamma)
H(2,2) = b*DSIN(gamma)
H(3,1) = c*DCOS(beta)
H(3,2) = c*( DSIN(beta)*( DCOS(alpha)-DCOS(beta)*DCOS(gamma) )/ &
       &      (DSIN(beta)*DSIN(gamma))             )
H(3,3) = c*(DSIN(beta)*                                         &
       &     DSQRT(                                             &
       &           ( DSIN(gamma)**2                             &
       &             -DCOS(beta)**2 - DCOS(alpha)**2            &
       &             +2.d0*DCOS(alpha)*DCOS(beta)*DCOS(gamma)   &
       &           )                                            &
       &          )/(DSIN(beta)*DSIN(gamma))                    &
       &   )
!
!
END SUBROUTINE CONVMAT
!
!
!********************************************************
!  MATCONV
!  This subroutine converts a matrix into conventional
!  vectors defined by a b c alpha beta gamma.
!********************************************************
!
SUBROUTINE MATCONV(H,a,b,c,alpha,beta,gamma)
!
IMPLICIT NONE
REAL(dp):: a, b, c, alpha, beta, gamma
REAL(dp),DIMENSION(3,3),INTENT(IN):: H
!
 a = VECLENGTH(H(1,:))
 b = VECLENGTH(H(2,:))
 c = VECLENGTH(H(3,:))
 alpha = ANGVEC( H(2,:),H(3,:) )
  beta = ANGVEC( H(3,:),H(1,:) )
 gamma = ANGVEC( H(1,:),H(2,:) )
!
!
END SUBROUTINE MATCONV
!
!
!********************************************************
!  VOLUME_PARA
!  This subroutine computes the volume of a parallelepiped
!  defined by three vectors stored in a 3x3 matrix.
!********************************************************
!
SUBROUTINE VOLUME_PARA(Pvec,Volume)
!
IMPLICIT NONE
REAL(dp):: a, b, c, alpha, beta, gamma
REAL(dp):: Volume
REAL(dp), DIMENSION(3,3),INTENT(IN):: Pvec
!
CALL MATCONV(Pvec, a, b, c, alpha, beta, gamma)
!
Volume = a*b*c*                                            &
       & (1.d0 + 2.d0*DCOS(alpha)*DCOS(beta)*DCOS(gamma)   &
       &  -DCOS(alpha)**2 -DCOS(beta)**2 -DCOS(gamma)**2  )
!
!
END SUBROUTINE VOLUME_PARA
!
!
!********************************************************
!  DERIVATIVE
!  This subroutine calculates the centered derivative
!  of an array.
!********************************************************
!
SUBROUTINE DERIVATIVE(func,dfunc)
!
IMPLICIT NONE
INTEGER:: i, funcsize
REAL(dp):: step
REAL(dp),DIMENSION(:,:),INTENT(IN):: func
REAL(dp),DIMENSION(:,:),ALLOCATABLE:: dfunc
!
funcsize = SIZE(func(:,1))
IF(.NOT.ALLOCATED(dfunc)) ALLOCATE(dfunc(funcsize-2,2))
dfunc(:,:)=0.d0
!
DO i=2,funcsize-1
  step = func(i+1,1)-func(i-1,1)
  dfunc(i,1) = func(i,1)
  dfunc(i,2) = (func(i+1,2)-func(i-1,2))/step
ENDDO
!
END SUBROUTINE DERIVATIVE
!
!
!********************************************************
!  EULER2MAT_ZYX
!  This subroutine converts Euler angles (a,b,c) (radians)
!  into a 3x3 rotation matrix, using ZYX convention
!  i.e. first rotation around X Cartesian axis,
!  then Y, and finally Z.
!********************************************************
!
SUBROUTINE EULER2MAT_ZYX(a,b,c,rotmat)
!
IMPLICIT NONE
REAL(dp),INTENT(IN):: a, b, c
REAL(dp):: c1, c2, c3, s1, s2, s3
REAL(dp),DIMENSION(3,3),INTENT(OUT):: rotmat
!
c1 = DCOS(c)
c2 = DCOS(b)
c3 = DCOS(a)
s1 = DSIN(c)
s2 = DSIN(b)
s3 = DSIN(a)
!
rotmat(:,:) = 0.d0
rotmat(1,1) = c1*c2
rotmat(1,2) = c1*s2*s3 - c3*s1
rotmat(1,3) = s1*s3 + c1*c3*s2
rotmat(2,1) = c2*s1
rotmat(2,2) = c1*c3 + s1*s2*s3
rotmat(2,3) = c3*s1*s2 - c1*s3
rotmat(3,1) = -1.d0*s2
rotmat(3,2) = c2*s3
rotmat(3,3) = c2*c3
!
END SUBROUTINE EULER2MAT_ZYX
!
!
!********************************************************
!  MAT2EULER_ZYX
!  This subroutine converts a 3x3 rotation matrix into
!  Euler angles (a,b,c) (in radians), using ZYX convention
!  i.e. first rotation around X Cartesian axis,
!  then Y, and finally Z.
!********************************************************
!
SUBROUTINE MAT2EULER_ZYX(rotmat,a,b,c)
!
IMPLICIT NONE
REAL(dp),INTENT(OUT):: a, b, c
REAL(dp),DIMENSION(3,3),INTENT(IN):: rotmat
!
c = DATAN2( rotmat(2,1) , rotmat(1,1) )
b = DATAN2( -1.d0*rotmat(3,1) , DSQRT(1.d0 - rotmat(3,1)**2) )
a = DATAN2( rotmat(3,2) , rotmat(3,3) )
!
END SUBROUTINE MAT2EULER_ZYX
!
!
!********************************************************
!  IS_ROTMAT
!  This function checks if a 3x3 matrix M is a rotation
!  matrix, by computing M.Mt where Mt is the transpose.
!  If M.Mt yields the identity matrix then M is a
!  rotation matrix, otherwise it is not.
!********************************************************
!
FUNCTION IS_ROTMAT(matrix) RESULT(isrotmat)
!
IMPLICIT NONE
INTEGER:: i, j
LOGICAL:: isrotmat
REAL(dp),DIMENSION(3,3):: IdMat
REAL(dp),DIMENSION(3,3),INTENT(IN):: matrix
!
isrotmat = .TRUE.
IdMat(:,:) = 0.d0
!
IdMat = MATMUL( matrix , TRANSPOSE(matrix) )
!
DO i=1,3
  DO j=1,3
    IF( i==j .AND. DABS(1.d0-IdMat(i,j))>1.d-6 ) THEN
      !This non-diagonal element is not equal to 1
      isrotmat = .FALSE.
    ENDIF
    IF( i.NE.j .AND. DABS(IdMat(i,j))>1.d-6 ) THEN
      !This non-diagonal element is non-zero
      isrotmat = .FALSE.
    ENDIF
  ENDDO
ENDDO
!
END FUNCTION IS_ROTMAT
!
!
!
END MODULE math
