module module1
   integer n00,n12,n1
   character(len=38):: record00(1000),record12(1000)
   character(len=11),allocatable :: IDlist(:)
   character(len=8),allocatable :: lat1(:)
   character(len=9),allocatable :: lon1(:)
   character(len=8):: lat
   character(len=9):: lon
   character(len=11) ID0,ID1,ID2
   character(len=1) block1
end module module1
    
program Pground1
use module1
  implicit none
  integer i,j,k,year,month,day,hour,level,vp
  real Pground
  character(len=103) record2,record3

call sub1()
do i=1,n1

  open(unit=21,file='D:\IAP\IGRA\derived\'//IDlist(i)//'-drvd.txt')
  open(unit=22,file='D:\IAP\derived\derived\00z\'//IDlist(i)//'.txt')
  open(unit=23,file='D:\IAP\derived\derived\12z\'//IDlist(i)//'.txt')
  lat=lat1(i)
  lon=lon1(i)

  do while(.not.eof(21))
  record2=''
  record3=''
      read(21,'(a103)') record2
      read(record2(2:12),'(a11)') ID0
      if(ID0==IDlist(i))then  
        n00=0
        n12=0
        record00=''
        record12=''
          read(record2(25:26),'(i2)') hour
          if(hour<=6)then
		  do 
            if(eof(21)) exit 
            read(21,'(a103)') record3
            if(record3(1:1)=='#') exit
			read(record3(1:7),'(i7)') level
			read(record3(73:79),'(i7)') vp
            if(level>30000.AND.vp>0)then
            n00=n00+1
            record00(n00)(1:23)=record2(2:24)
            write(record00(n00)(24:30),'(i7)') level
			write(record00(n00)(32:38),'(i7)') vp
            !write(22,'(a38)') record00(n00)
            else if(level==30000.and.vp>0)then
                n00=n00+1
                record00(n00)(1:23)=record2(2:24)
                write(record00(n00)(24:30),'(i7)') level
			    write(record00(n00)(32:38),'(i7)') vp
                do j=1,n00
                    write(22,'(a38)') record00(j)
                end do 
            else
                exit
			end if
			end do
          else
 		  do 
            if(eof(21)) exit
            read(21,'(a103)') record3
            if(record3(1:1)=='#') exit
			read(record3(1:7),'(i7)') level
			read(record3(73:79),'(i7)') vp
            if(level>30000.AND.vp>0)then
            n12=n12+1
            record12(n12)(1:23)=record2(2:24)
            write(record12(n12)(24:30),'(i7)') level
			write(record12(n12)(32:38),'(i7)') vp
            !write(22,'(a38)') record00(n00)
            else if(level==30000.and.vp>0)then
                n12=n12+1
                record12(n12)(1:23)=record2(2:24)
                write(record12(n12)(24:30),'(i7)') level
			    write(record12(n12)(32:38),'(i7)') vp
                do j=1,n12
                    write(23,'(a38)') record12(j)
                end do 
            else
                exit
			end if
			end do
          end if
      end if
  end do
  close(22)
  close(23)
  close(21)
  !call sub12()
  end do
end

subroutine sub1()
use module1
implicit none
integer i
  open(unit=10, file='D:\IAP\IGRA\igra2-station-list.txt')
  n1=0
  do while(.not.eof(10))
    read(unit=10, fmt="(a11)") ID0
	n1=n1+1!计算了ID总数，可用来传递定义可变数组
  enddo
  allocate(IDlist(n1))
  allocate(lat1(n1))
  allocate(lon1(n1))
  close(10)
  open(unit=10, file='D:\IAP\IGRA\igra2-station-list.txt')
  do i=1,n1
     read(unit=10, fmt="(a11,a1,a8,a1,a9)") IDlist(i),block1,lat1(i),block1,lon1(i)
  end do
  close(10)
end subroutine sub1
 
subroutine sub00()
use module1
implicit none
integer i,j,k,temp1,temp2,pg
real pground
pg=0
j=0

do i=1,n00
    read(record00(i)(24:30),'(i7)') temp1
    read(record00(i+1)(24:30),'(i7)') temp2
    if(record00(i)(14:20)==record00(i+1)(14:20))then
    pg=pg+temp1
    j=j+1
else
    pground=(pg+temp1)*0.01/((j+1)*1.0)
    open(31,file='D:\IAP\Pground\Pground\00\new\'//record00(i)(13:16)//record00(i)(18:19)//'_pg.txt',position='Append') 
    write(31,'(a11,a1,a8,a1,a9,a1,a7,f8.2)') record00(i)(1:11),block1,lat,block1,lon,block1,record00(i)(13:19),pground
    j=0
    pg=0
    close(31)
    end if
end do

end subroutine sub00

subroutine sub12()
use module1
implicit none
integer i,j,k,temp1,temp2,pg
real pground
pg=0
j=0

do i=1,n12
    !print*,record12(i)
    !read(*,*)
    read(record12(i)(24:30),'(i7)') temp1
    read(record12(i+1)(24:30),'(i7)') temp2
    if(record12(i)(14:20)==record12(i+1)(14:20))then
    pg=pg+temp1
    j=j+1
else
    pground=(pg+temp1)*0.01/((j+1)*1.0)
    open(31,file='D:\IAP\Pground\Pground\12\new\'//record12(i)(13:16)//record12(i)(18:19)//'_pg.txt',position='Append') 
    write(31,'(a11,a1,a8,a1,a9,a1,a7,f8.2)') record12(i)(1:11),block1,lat,block1,lon,block1,record12(i)(13:19),pground
    j=0
    pg=0
    close(31)
    end if
end do
end subroutine sub12
