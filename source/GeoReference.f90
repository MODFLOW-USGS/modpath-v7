module GeoReferenceModule
  use UTL7MODULE,only:ustop  
  implicit none
  
! Set default access status to private
  private
    
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: GeoReferenceType
    double precision :: OriginX = 0.0
    double precision :: OriginY = 0.0
    double precision, private :: AngRot = 0.0
    double precision, private :: RadAngle = 0.0
    double precision, private :: CosAngle = 1.0
    double precision, private :: SinAngle = 0.0
    double precision, private :: RadPerDeg
 contains
    procedure :: GetAngRot
    procedure :: SetData
    procedure :: ConvertModelToGlobal
    !procedure :: ConvertGlobalToModel
 end type

    contains
!----------------------------------------------------------
    subroutine SetData(this, originX, originY, angRot)
    implicit none
    class (GeoReferenceType) :: this
    doubleprecision, intent(in) :: originX, originY, angRot
    
    this%AngRot = angRot
    this%OriginX = originX
    this%OriginY = originY
    this%RadPerDeg = atan(1.0) / 45.0
    
    
    if((angRot .gt. 180.0) .or. (angRot .lt. -180.0)) then
        call ustop('The rotation angle must be in the range -180 to 180 degrees. Stop')
    end if
    
    this%RadAngle = this%AngRot * this%RadPerDeg
    this%CosAngle = cos(this%RadAngle)
    this%SinAngle = sin(this%RadAngle)
    
    end subroutine SetData
!----------------------------------------------------------
    function GetAngRot(this) result(angle)
    implicit none
    class (GeoReferenceType) :: this
    double precision :: angle
    
    angle = this%AngRot
    
    end function GetAngRot
!----------------------------------------------------------
    subroutine ConvertModelToGlobal(this, modelX, modelY, globalX, globalY)
    implicit none
    class (GeoReferenceType) :: this
    doubleprecision, intent(in) :: modelX, modelY
    doubleprecision, intent(inout) :: globalX, globalY
    
    ! Rotate coordinates around the default origin (0, 0) if necessar.
    ! If no rotation is necessary, just set global x and y equal to model x and y
    if(this%AngRot .eq. 0.0) then
        globalX = modelX
        globalY = modelY
    else
        globalX = (modelX * this%CosAngle) - (modelY * this%SinAngle)
        globalY = (modelX * this%SinAngle) + (modelY * this%CosAngle)        
    end if
    
    ! Now add origin offsets
    globalX = globalX + this%OriginX
    globalY = globalY + this%OriginY
    
    end subroutine ConvertModelToGlobal
!!----------------------------------------------------------
!    subroutine ConvertGlobalToModel(this, globalX, globalY, modelX, modelY)
!    implicit none
!    class (GeoReferenceType) :: this
!    doubleprecision, intent(in) :: globalX, globalY
!    doubleprecision, intent(inout) :: modelX, modelY
!    
!    if((this%OriginX .eq. 0.0) .and. (this%OriginY .eq. 0.0) .and. (this%AngRot .eq. 0.0)) then
!        modelX = globalX
!        modelY = globalY
!    end if
!    
!    end subroutine ConvertGlobalToModel
    
 end module GeoReferenceModule

    
    