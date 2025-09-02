module utils
  implicit none

  contains

    subroutine percentile_clip(arr, limits)
      real(8), intent(inout) :: arr(:)
      integer, intent(in) :: limits(2)
      integer :: n, i
      real(8), allocatable :: sorted(:)
      real(8) :: low_val, high_val
      integer :: idx_low, idx_high

      n = size(arr)
      allocate(sorted(n))
      sorted = arr
      call sort_array(sorted)

      idx_low  = max(1, int(n * limits(1) / 100.0d0) + 1)
      idx_high = min(n, int(n * limits(2) / 100.0d0))

      low_val  = sorted(idx_low)
      high_val = sorted(idx_high)

      ! replace out-of-bounds values
      do i = 1, n
        if (arr(i) < low_val) arr(i) = low_val
        if (arr(i) > high_val) arr(i) = high_val
      end do

      deallocate(sorted)
  end subroutine percentile_clip

  subroutine sort_array(a)
    real(8), intent(inout) :: a(:)
    integer :: i, j
    real(8) :: key
    integer :: n

    n = size(a)
    do i = 2, n
      key = a(i)
      j = i - 1
      do while (j >= 1 .and. a(j) > key)
        a(j+1) = a(j)
        j = j - 1
      end do
      a(j+1) = key
    end do
  end subroutine sort_array

end module utils

module statools
  implicit none

  contains

    real(8) function arithmetic_mean(array)
      real(8), intent(in) :: array(:)
      arithmetic_mean = sum(array) / size(array)
    end function arithmetic_mean

    real(8) function w_arithmetic_mean(array, weights)
      real(8), intent(in) :: array(:), weights(:)
      real(8) :: sum_aw
      sum_aw = sum(array * weights)  ! element-wise multiply and sum
      w_arithmetic_mean = sum_aw / sum(weights)
    end function w_arithmetic_mean

    real(8) function geometric_mean(array)
      real(8), intent(in) :: array(:)
      geometric_mean = exp(sum(log(array)) / size(array))
    end function geometric_mean

    real(8) function harmonic_mean(array)
      real(8), intent(in) :: array(:)
      harmonic_mean = size(array) / sum(1/array)
    end function harmonic_mean

    real(8) function winsorized_mean(array, limits)
      use utils, only: percentile_clip

      real(8), intent(inout) :: array(:)
      integer, intent(in) :: limits(2)

      call percentile_clip(array, limits)
      winsorized_mean = sum(array) / size(array)
    end function winsorized_mean

end module statools

