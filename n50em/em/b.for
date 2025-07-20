      character*100 a100,b100,name1,name2
      character*5 name
      character*2 aa
      
      open(unit=20,file='br.dat')
      open(unit=21,file='bt.dat')         
c      open(unit=20,file='qr.dat')
c      open(unit=21,file='qt.dat')  
 
c      open(unit=30,file='mr.dat')
c      open(unit=31,file='mt.dat')             
      do 300 k = 1,10
      
      if(k.eq.1) name='zn80y'
      if(k.eq.2) name='ge82y'  
      if(k.eq.3) name='se84y'
      if(k.eq.4) name='kr86y'    
      if(k.eq.5) name='sr88y'
      if(k.eq.6) name='zr90y'  
      if(k.eq.7) name='mo92y'
      if(k.eq.8) name='ru94y'         
      if(k.eq.9) name='pd96y'
      if(k.eq.10) name='cd98y'  
      aa = name(3:4)
      read(aa,330) ia
330   format(i2) 
        
      
      name1 = 'c:\rsh-nushellx\tf2\'//name//'.deo'  
      name2 = 'c:\rsh-nushellx\r35c\'//name//'.deo'        
      
      open(unit=10,file=name1,status='old')
      open(unit=11,file=name2,status='old')  
    
          
      do 100 i = 1,19
      read(10,*)
100   continue
      read(10,101) a100
101   format(a100)
      b100 = ' '
      b100 = a100(29:100)
      print *,b100
      read(b100,*) x1,x2,x3,x4,x5t
      close(10)
      
      
      do 110 i = 1,19
      read(11,*)
110   continue
      read(11,111) a100
111   format(a100)
      b100 = ' '
      b100 = a100(29:100)
      print *,b100
      read(b100,*) x1,x2,x3,x4,x5r
      close(11)
            
      
      print *,name,ia,x5r,x5t
      xa = ia
      write(20,120) xa,x5r
      write(21,120) xa+0.2,x5t        
c      write(20,120) ia,x4r
c      write(21,120) ia,x4t         
c      write(30,120) ia,x3r
c      write(31,120) ia,x3t  
               
120   format(2f8.2)
      
300   continue
      
      end 
      
