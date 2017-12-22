module ModpathUnstructuredBasicDataModule
  implicit none
  
! Set default access status to private
  private

  type,public :: ModpathUnstructuredBasicDataType
    integer,allocatable,dimension(:) :: IBound
    doubleprecision,allocatable,dimension(:) :: Porosity
  end type
  
contains

end module ModpathUnstructuredBasicDataModule