module module1
integer :: i,j,k,s,n1
character(len=11),allocatable :: IDlist(:)
character(len=11) ID0,ID1,ID2
character(len=1) block1
integer,allocatable :: IDlength(:)
character(len=34) record1
integer year1,month1,level1,mark
real Tw,w1,w2
character(len=8) lat
character(len=9) lon
character(len=35),allocatable :: record0(:) 
character(len=8),allocatable :: lat1(:)
character(len=9),allocatable :: lon1(:)
end module module1

module module2
!use module1
    contains
recursive subroutine sub4(l)
use module1
implicit none
integer year2,month2,level2,l
l=l-1
		block1=''
        read(record0(l)(13:16),'(i4)') year2
		read(record0(l)(18:19),'(i2)') month2
		read(record0(l)(21:24),'(i4)') level2
        read(record0(l)(35:35),'(i1)') mark
        read(record0(l)(26:33),'(f8.4)') w1
        read(record0(l+1)(26:33),'(f8.4)') w2
        Tw=Tw+0.5*(w2+w1)*real(level2-level1)
        !print*,tw,year2,month2,level2
        !read(*,*)
		if(mark==0.and.month2==month1.and.year2==year1)then
            call sub4(l)
        else
            if(mark==1.and.month2==month1.and.year2==year1)then
                Tw=Tw/980.0
          write(31,'(a24,f8.2,a1,a8,a9)') record0(l)(1:24),Tw,block1,lat1(l),lon1(l)
          !exit
          
          end if
        end if
end subroutine sub4 
    end module module2


Program Split
   use module1
  implicit none
  integer n2
call sub1() !算ID数量
  
  !open(unit=10, file='/nuist/p/work/st6099139/IGRA/igra2-station-list2.txt')
!call Splittemp()
!call Splitghgt()
!call IGRA()
!call Pground12()
call vapr2()
call qcal()  
call sub2()
call sub3()
call sub5()

end

    
subroutine sub1()
use module1
implicit none
  open(unit=10, file='D:\IAP\IGRA\igra2-station-list.txt')
  n1=0
  do while(.not.eof(10))
    read(unit=10, fmt="(a11)") ID0
	n1=n1+1!计算了ID总数，可用来传递定义可变数组
  enddo
  allocate(IDlist(n1))
  allocate(IDlength(n1))
  close(10)
  open(unit=10, file='D:\IAP\IGRA\igra2-station-list.txt')
  do i=1,n1
     read(unit=10, fmt="(a11)") IDlist(i)
  end do
  close(10)
end subroutine sub1


subroutine sub2()
use module1
implicit none
real q
IDlength=0
open(unit=21, file='D:\IAP\testin1\testin1\q12.txt')
i=1
    do while(.not.eof(21))
    read(21,'(a25,f8.4,a1)') record1(1:25),q,block1
	read(record1(1:11),'(a11)') ID1
    if(ID1==IDlist(i))then
        IDlength(i)=IDlength(i)+1
    else
        i=i+1
        print*,i,ID1
       do while(ID1/=IDlist(i))
           i=i+1
       end do   
        IDlength(i)=IDlength(i)+1
    end if
    end do
    !open(unit=22,file='D:\IAP\testin1\testin1\IDlength.txt')
    !write(22,*) 
    s=sum(IDlength)
    allocate(record0(s))
    allocate(lat1(s))
    allocate(lon1(s))
    close(21)
    close(22)
open(unit=21, file='D:\IAP\testin1\testin1\q12.txt')
  do i=1,s
      read(21,'(a35,a8,a9)') record0(i),lat1(i),lon1(i)
  end do
close(21)
end subroutine sub2

subroutine sub3()
use module1
use module2
implicit none
!integer year1,month1,level1
integer s1,s2
  open(unit=31,file='D:\IAP\testin1\testin1\result12.txt')
do i=1,n1
		s2=sum(IDlength(1:i))
        s1=s2-IDlength(i)+1
        !print*,i,s1,s2
        !read(*,*)
        do j=s2,s1,-1
        read(record0(j)(13:16),'(i4)') year1
		read(record0(j)(18:19),'(i2)') month1
		read(record0(j)(21:24),'(i4)') level1
        if(level1==300)then
            Tw=0.0
            !print*,'ok1'
			call sub4(j)
   !         print*,j,IDlist(j)
        end if
        end do
end do
close(31)
    end subroutine sub3

    subroutine sub5()
    use module1
    implicit none
    character(len=24) record2
    open(unit=31,file='D:\IAP\testin1\testin1\result12.txt')
    do while(.not.eof(31))
        !read(31,'(a24,a1,f10.2)')record2,block1,Tw
        read(31,'(a24,f8.2,a1,a8,a9)') record2,Tw,block1,lat,lon
        read(record2(1:11),'(a11)') ID1
        read(record2(18:19),'(i2)') month1
	    read(record2(13:16),'(i4)') year1
        if(month1<10)then
            write(record2(18:18),'(a1)') '0'
        end if
    open(21,file='D:\IAP\testin1\testin1\12z\w\'//record2(13:16)//record2(18:19)//'_w.txt',position='append')
    write(21,'(a24,a1,f8.2,a8,a9)') record2,block1,Tw,lat,lon
    close(21)
    end do
    close(31)
    end subroutine sub5
    



Subroutine Splittemp
use module1
  implicit none
  integer s1,s2
  character(len=34),allocatable :: temp(:)
  integer,allocatable :: templength(:)
  allocate(templength(n1))
  templength=0
  open(unit=10, file='D:\IAP\testin1\testin1\igra2-station-list2.txt')
  open(unit=11, file='D:\IAP\testin1\testin1\temp_12z-mly.txt')
  i=1
    do while(.not.eof(11))
    read(11,'(a34)') record1
	read(record1(1:11),'(a11)') ID1
    if(ID1==IDlist(i))then
        templength(i)=templength(i)+1
    else
        i=i+1
       do while(ID1/=IDlist(i))
           i=i+1
       end do   
        templength(i)=templength(i)+1
    end if
    end do
    close(11)
    open(unit=11, file='D:\IAP\testin1\testin1\temp_12z-mly.txt')
    s=sum(templength)
    allocate(temp(s))
    do i=1,s
        read(11,'(a34)') temp(i)
    end do
    
  do i=1,n1
   !open(21,file='/nuist/p/work/st6099139/IGRA/temp/'//IDlist(i)//'_temp.txt',position='append')
    open(21,file='D:\IAP\testin1\testin1\12z\temp\'//IDlist(i)//'_temp.txt',position='append') 
   		s2=sum(templength(1:i))
        s1=s2-templength(i)+1
    do j=s1,s2
        write(21,'(a34)')temp(j)
    end do
    close(21)
  enddo
end subroutine splittemp

Subroutine Splitghgt
use module1
  implicit none
  integer s1,s2
  character(len=34),allocatable :: ghgt(:)
  integer,allocatable :: ghgtlength(:)
  allocate(ghgtlength(n1))
  ghgtlength=0
  open(unit=10, file='D:\IAP\testin1\testin1\igra2-station-list2.txt')
  open(unit=11, file='D:\IAP\testin1\testin1\ghgt_12z-mly.txt')
  i=1
    do while(.not.eof(11))
    read(11,'(a34)') record1
	read(record1(1:11),'(a11)') ID1
    if(ID1==IDlist(i))then
        ghgtlength(i)=ghgtlength(i)+1
    else
        i=i+1
       do while(ID1/=IDlist(i))
           i=i+1
       end do   
        ghgtlength(i)=ghgtlength(i)+1
    end if
    end do
    close(11)
    open(unit=11, file='D:\IAP\testin1\testin1\ghgt_12z-mly.txt')
    s=sum(ghgtlength)
    allocate(ghgt(s))
    do i=1,s
        read(11,'(a34)') ghgt(i)
    end do
    
  do i=1,n1
   !open(21,file='/nuist/p/work/st6099139/IGRA/ghgt/'//IDlist(i)//'_ghgt.txt',position='append')
    open(21,file='D:\IAP\testin1\testin1\12z\ghgt\'//IDlist(i)//'_ghgt.txt',position='append') 
   		s2=sum(ghgtlength(1:i))
        s1=s2-ghgtlength(i)+1
    do j=s1,s2
        write(21,'(a34)')ghgt(j)
    end do
    close(21)
  enddo
    end subroutine splitghgt

Subroutine IGRA
use module1
  implicit none
  integer year2,month2,month3,month4,level2,level3,level4,temp1,temp2,temp4,ghgt
  real Pground
  character(len=34):: record00(14000)
  character(len=34) record4,record3,record2,record5
  !open(unit=23,file='/nuist/p/work/st6099139/IGRA/12z/result12.txt')
  !open(unit=10, file='/nuist/p/work/st6099139/IGRA/igra2-station-list2.txt')
open(unit=23,file='D:\IAP\testin1\testin1\result.txt')
open(unit=10, file='D:\IAP\testin1\testin1\igra2-station-list.txt')

  do while(.not.eof(10))
    read(unit=10, fmt="(a11,a1,a8,a1,a9)") ID0,block1,lat,block1,lon
    !print*,ID0
    !read(*,*)
    !open(unit=21,file='/nuist/p/work/st6099139/IGRA/12z/temp/'//ID0//'_temp.txt')
    !open(unit=22,file='/nuist/p/work/st6099139/IGRA/12z/ghgt/'//ID0//'_ghgt.txt')
    open(unit=21,file='D:\IAP\testin1\testin1\12z\temp\'//ID0//'_temp.txt')
    open(unit=22,file='D:\IAP\testin1\testin1\12z\ghgt\'//ID0//'_ghgt.txt')
    i=0
    do while(.not.eof(21))
      i=i+1
      read(21,'(a34)') record2
      record00(i)=record2      
    end do
    do j=1,i-2
        record5=record00(j)
      read(record5(13:16),'(i4)') year1
      read(record5(18:19),'(i2)') month1
      read(record5(21:24),'(i4)') level1
      read(record5(26:31),'(i6)') temp1
      !print*,level1
      if(level1/=9999) then
        cycle
      else
          record2=record00(j+1)
          record4=record00(j+2)
        read(record2(21:24),'(i4)') level2
        read(record2(26:31),'(i6)') temp2
		read(record2(18:19),'(i2)') month2
        read(record4(21:24),'(i4)') level4
        read(record4(26:31),'(i6)') temp4
		read(record4(18:19),'(i2)') month4
        if(month2/=month1)then !避免一个月只有一条记录
			exit
		end if
        !print*,level2,level4
        rewind(22)
        do while(.not.eof(22))
          read(22,'(a34)') record3
          read(record3(13:16),'(i4)') year2
          read(record3(18:19),'(i2)') month3
          read(record3(21:24),'(i4)') level3
          read(record3(26:31),'(i6)') ghgt
          !print*,year1,year2,month1,month3,level2,level3
        if(year1/=year2)then
            !print*,"ok1"
                cycle
            else
                if(month1/=month3)then
                    !print*,"ok2"
                    cycle
                else
					if(level2==level3) then
                      Pground=level3*1.0*exp(-9.80665*ghgt*2.0/(287*(temp1*0.1+temp2*0.1+546.3)))
					  if((Pground+50)<level2)then
						exit
					  else
                      write(23,'(a11,a1,a8,a1,a9,a1,i4,a1,i2,a1,f6.1)') ID0,block1,lat,&
                      block1,lon,block1,year1,block1,month1,block1,Pground
                      !read(*,*)
                      exit
					  end if
                    else
						if(month4/=month1)then
							exit
						endif	
                      if(level4==level3) then
                        Pground=level3*1.0*exp(-9.80665*ghgt*2.0/(287*(temp1*0.1+temp4*0.1+546.3)))
						if((Pground+50)<level2)then
							exit
						else	
                        write(23,'(a11,a1,a8,a1,a9,a1,i4,a1,i2,a1,f6.1)') ID0,block1,lat,&
                        block1,lon,block1,year1,block1,month1,block1,Pground
                        exit
						endif
                      else
                        cycle
                      end if
                    end if
                end if
            end if
        end do
      end if

    !call temp(ID0)
    !call ghgt(ID0)
      end do
close(21)
close(22)
  enddo
  close(23)
  close(10)
end subroutine IGRA
subroutine Pground12()
use module1
implicit none
  real Pground
  character(len=45) record2
  ! open(unit=10,file='/nuist/p/work/st6099139/IGRA/12z/result12.txt')
  open(unit=10,file='D:\IAP\testin1\testin1\result.txt')
  do while (.not.eof(10))
    read(10,'(a45)') record2
    !read(record1(32:35),'(i4)') year
    read(record2(37:38),'(i2)') month1
    if(month1<10)then
      write(record2(37:37),'(a1)') '0'
    end if
    !open(unit=21,file='/nuist/p/work/st6099139/IGRA/12z/pg/'//record1(32:35)//record1(37:38)//'_pg.txt',position='append')
    open(unit=21,file='D:\IAP\testin1\testin1\12z\pg\'//record2(32:35)//record2(37:38)//'_pg.txt',position='append')
    write(21,'(a45)') record2
    close(21)
  end do
  close(10)
end subroutine Pground12
    
subroutine vapr2()
use module1
implicit none
real pground
integer vapr,level2
character(len=45) record2
 open(unit=10,file='D:\IAP\testin1\testin1\vapr_12z-mly.txt')
 open(unit=21,file='D:\IAP\testin1\testin1\vapr2_12z-mly.txt',position='append',STATUS='REPLACE')
 do while(.not.eof(10))
	read(unit=10, fmt='(a34)') record1
    !print*,record1
	read(record1(1:11),'(a11)') ID1
    read(record1(18:19),'(i2)') month1
	read(record1(21:24),'(i4)') level1
	read(record1(26:31),'(i6)') vapr
    !print*,ID1,month1,level1,vapr
	if(level1==9999)then
        !print*,'ok1'
		if(month1<10)then
            !print*,'ok2'
           open(unit=11,file='D:\IAP\testin1\testin1\12z\pg\'//record1(13:16)//'0'//record1(19:19)//'_pg.txt')
           !print*,'ok3'
			do while(.not.eof(11))
				read(unit=11, fmt="(a45)") record2
				read(record2(1:11),'(a11)') ID2
				read(record2(13:20),'(a8)') lat
				read(record2(22:30),'(a9)') lon
				read(record2(40:45),'(f6.1)') pground
                !print*,record2,ID1,ID2
				level2=nint(pground)
				if(ID1==ID2)then
					write(21,'(a34,i4,a8,a9)') record1,level2,lat,lon
					close(11)
					exit
				endif
			end do
       else
           open(unit=11,file='D:\IAP\testin1\testin1\12z\pg\'//record1(13:16)//record1(18:19)//'_pg.txt')
           do while(.not.eof(11))
				read(unit=11, fmt='(a45)') record2
				read(record2(1:11),'(a11)') ID2
				read(record2(13:20),'(a8)') lat
				read(record2(22:30),'(a9)') lon
				read(record2(40:45),'(f6.1)') pground
				level2=nint(pground)
				if(ID1==ID2)then
					write(21,'(a34,i4,a8,a9)') record1,level2,lat,lon
					close(11)
					exit
				endif
			end do
		end if
	else
		write(21,'(a34,a4,a8,a9)') record1,'0000',lat,lon
	end if
end do
 close(10)
 close(21)
    end subroutine vapr2
    subroutine qcal()
  use module1
  implicit none
  integer pground,vapr
  real q
    block1=''
  open(unit=21,file='D:\IAP\testin1\testin1\q12.txt')
  open(unit=11,file='D:\IAP\testin1\testin1\vapr2_12z-mly.txt')
  do while(.not.eof(11))
	read(unit=11, fmt="(a34,i4,a8,a9)") record1,pground,lat,lon
    !print*,'ok1',pgroundlat,lon
    !read(*,*)
	!read(record1(1:11),'(a11)') ID1
    !read(record1(18:19),'(i2)') month1
	read(record1(21:24),'(i4)') level1
	read(record1(26:31),'(i6)') vapr
    if(level1==9999)then
		q=622.0*vapr*0.01/(pground*1.0-0.378*vapr*0.01)
        mark=1
		write(21,'(a20,i4,a1,f8.4,a1,i1,a8,a9)') record1(1:20),pground,block1,q,block1,mark,lat,lon
	else
		q=622.0*vapr*0.01/(level1*1.0-0.378*vapr*0.01)
        mark=0
		write(21,'(a25,f8.4,a1,i1,a8,a9)') record1(1:25),q,block1,mark,lat,lon
	endif	
end do
close(11)
close(21)
end subroutine qcal