module ParticleModule
  implicit none
  
! Set default access status to private
  private

type,public :: ParticleType
  integer :: CellNumber, Layer, Face, Group, ID, Status, SequenceNumber
  doubleprecision :: LocalX, LocalY, LocalZ, GlobalZ, TrackingTime
  integer :: InitialCellNumber, InitialFace, Drape, InitialLayer
  doubleprecision :: InitialLocalX, InitialLocalY, InitialLocalZ, InitialGlobalZ, InitialTrackingTime
  doubleprecision,dimension(:),allocatable :: ExitVelocity
end type








contains




end module ParticleModule