module pdf
    contains

    function arrpdf(arr, len) result(f)
        implicit none
    
        integer, intent(in) :: len
        real, dimension(len), intent(in) :: arr
        real :: datavari, arrvari, datamean, arrmean
        real, parameter :: pi=acos(-1.0)
        integer i
        real, dimension(2, 101) :: f
    
        datamean = arrmean(arr, len)
        datavari = arrvari(arr, len)
    
        do i = 1, 101
            f(1, i) = -2 + real(i-1)*(2.0+5.0)/100.0
        end do
        do i = 1, 101
            f(2, i) = exp(-(f(1, i)-datamean)**2/(2*datavari**2))/(2*datavari**2*pi)
        end do
        
    end function arrpdf
end module pdf

program DatAnalysis
    use pdf
    implicit none
    
    real :: arrmean, arrvari, arrdevi
    real, dimension(2, 101) :: datapdf
    
    real, dimension(:), allocatable :: lead
    integer :: rows, i, io
    real :: datamean, datavari, datadevi
    open(1, file='./05_data.txt', status='old', action='read')

    rows = 0
    do
        read(1,*,iostat=io)
        if ( io/=0 ) exit
        rows = rows + 1
    end do
    if ( rows<2 ) then
        print *, "Too few data to analyze"
        call exit
    end if

    rewind(1)

    allocate(lead(rows))
    do i = 1, rows
        read(1,*) lead(i)
    end do

    datamean=arrmean(lead, rows)
    print *, "The mean of this data set is:  ", datamean

    datavari=arrvari(lead, rows)
    print *, "The variance of this data set is:  ", datavari

    datadevi=arrdevi(lead, rows)
    print *, "The standard deviation is:  ", datadevi
    print *, "The number of data points is:  ", rows

    open(2, file='05_pdf.txt', status='replace', action='write')
    datapdf=arrpdf(lead, rows)
    do i = 1, 101
        write(2,*) datapdf(1, i), datapdf(2, i)
    end do

end program DatAnalysis

real function arrmean(arr, len) result(datamean)
    implicit none
    
    integer, intent(in) :: len
    real,dimension(len), intent(in) :: arr
    integer :: i

    datamean = 0.0
    do i = 1, len
        datamean = datamean+arr(i)
    end do

    datamean = datamean/real(len)
    return
end function arrmean

real function arrvari(arr, len) result(datavari)
    implicit none

    integer, intent(in) :: len
    real,dimension(len), intent(in) :: arr
    real :: datamean, arrmean
    integer i

    datamean = arrmean(arr, len)
    datavari = 0.0

    do i = 1, len
        datavari = datavari + (arr(i)-datamean)**2
    end do

    datavari = datavari/real(len-1)
    
end function arrvari

real function arrdevi(arr, len) result(datadevi)
    implicit none

    integer, intent(in) :: len
    real,dimension(len), intent(in) :: arr
    real :: datavari, arrvari
    integer i

    datavari = arrvari(arr, len)
    datadevi = sqrt(datavari)
    
end function arrdevi
