      character*100 a100,b100,name1,name2
      character*5 name
      character*2 aa
      open(unit=20,file='qr.dat')
      open(unit=21,file='qt.dat')   
      open(unit=30,file='mr.dat')
      open(unit=31,file='mt.dat')             
      do 300 k = 1,11
      
      if(k.eq.1) name='cu79y'
      if(k.eq.2) name='ga81y'  
      if(k.eq.3) name='as83y'
      if(k.eq.4) name='br85y'    
      if(k.eq.5) name='rb87y'
      if(k.eq.6) name='y_89y'  
      if(k.eq.7) name='nb91y'
      if(k.eq.8) name='tc93y'         
      if(k.eq.9) name='rh95y'
      if(k.eq.10) name='ag97y'  
      if(k.eq.11) name='in99y'
      aa = name(3:4)
      read(aa,330) ia
330   format(i2) 
        
      
      name1 = 'c:\rsh-nushellx\t35-deo\'//name//'.deo'  
      name2 = 'c:\rsh-nushellx\r35-deo\'//name//'.deo'        
      
      open(unit=10,file=name1,status='old')
      open(unit=11,file=name2,status='old')  
    
          
      do 100 i = 1,16
      read(10,*)
100   continue
      read(10,101) a100
101   format(a100)
      b100 = ' '
      b100 = a100(25:100)
      read(b100,*) x1,x2,x3t,x4t
      close(10)
      
      
      do 110 i = 1,16
      read(11,*)
110   continue
      read(11,111) a100
111   format(a100)
      b100 = ' '
      b100 = a100(25:100)
      read(b100,*) x1,x2,x3r,x4r
      close(11)
            
      xa = ia
      print *,name,ia,x3r,x4r,x3t,x4t
      write(20,120) xa-0.2,x4r
      write(21,120) xa+0.2,x4t   
      
      write(30,120) xa-0.2,x3r
      write(31,120) xa+0.2,x3t  
               
120   format(2f8.2)
      
300   continue
      
      end 
      
