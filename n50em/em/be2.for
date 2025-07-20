      character*100 a100,b100,name1,name2
      character*5 name
      character*2 aa,as
      character*1 ap
      character*10 fileina,fileinb
      character*20 filema,filemb,fileqa,fileqb
      character*20 fileq8a,fileq8b,filem8a,filem8b
      character*40 fileinaf,fileinbf
      character*20 filebe2,filebm1
      
      fileina = 'p35i3'
      fileinb = 'p35i2'     
      
      filebe2 = 'be2p35.dat' 
      filebm1 = 'bm1p35.dat' 
            
      open(unit=20,file=filebe2)  
      open(unit=21,file=filebm1) 
      
      filema = fileina//'.m'
      filemb = fileinb//'.m'  
      print *,filema,filemb 
      call fch(filema)
      call fch(filemb)
      open(unit=30,file=filema)  
      open(unit=31,file=filemb)    
      
      fileqa = fileina//'.q'
      fileqb = fileinb//'.q'   
      call fch(fileqa)
      call fch(fileqb)
      open(unit=40,file=fileqa)  
      open(unit=41,file=fileqb)  
      
      filem8a = fileina//'.m8'
      filem8b = fileinb//'.m8'   
      call fch(filem8a)
      call fch(filem8b)
      open(unit=50,file=filem8a)  
      open(unit=51,file=filem8b)  
      
      fileq8a = fileina//'.q8'
      fileq8b = fileinb//'.q8'   
      call fch(fileq8a)
      call fch(fileq8b)
      open(unit=60,file=fileq8a)  
      open(unit=61,file=fileq8b)                             
         
              
      
      kkk = 0
      do 300 k = 1,21
      
      if(k.eq.1) name='cu79y'
      if(k.eq.2) name='zn80y'      
      if(k.eq.3) name='ga81y'  
      if(k.eq.4) name='ge82y'
      if(k.eq.5) name='as83y'    
      if(k.eq.6) name='se84y'
      if(k.eq.7) name='br85y'  
      if(k.eq.8) name='kr86y'
      if(k.eq.9) name='rb87y'         
      if(k.eq.10) name='sr88y'
      if(k.eq.11) name='y_89y'  
      if(k.eq.12) name='zr90y'
      if(k.eq.13) name='nb91y'  
      if(k.eq.14) name='mo92y'
      if(k.eq.15) name='tc93y'    
      if(k.eq.16) name='ru94y'
      if(k.eq.17) name='rh95y'  
      if(k.eq.18) name='pd96y'
      if(k.eq.19) name='ag97y'         
      if(k.eq.20) name='cd98y'
      if(k.eq.21) name='in99y'      
           
      aa = name(3:4)
      read(aa,330) ia
330   format(i2) 

      fileinaf = 'c:\rsh-nushellx\n50\'//fileina//'\'//name//'.deo'       
      fileinbf = 'c:\rsh-nushellx\n50\'//fileinb//'\'//name//'.deo'  
      call fch(fileinaf)       
      call fch(fileinbf)      
      print *,fileinaf,fileinbf
      open(unit=10,file=fileinaf,status='old')
      open(unit=11,file=fileinbf,status='old')  
               
        
cbab --------- read top of file
      do 100 i = 1,16
      read(10,*)
      read(11,*)      
100   continue

cbab --------- read until initial state is found 

106   continue
      read(10,101,end=108) a100
      read(11,101,end=108) b100 
101   format(a100)
    
      if(a100(6:6).eq.'.') go to 200 
      go to 106
200   continue 


cbab ------------ if initial states do not match do to the next one
      if(a100(13:26).ne.b100(13:26)) go to 106
      
cbab ---------- read properties of the initial state
      if(a100(16:16).eq.'+') ipi = 0
      if(a100(16:16).eq.'-') ipi = 1  
      read(a100(12:13),*) ji   
      read(a100(20:21),*) ni        
      
      read(a100(3:11),*) eia  
      read(b100(3:11),*) eib             
         
      read(a100(55:76),*) xma,xqa
      read(b100(55:76),*) xmb,xqb   
    
      
cbab write out ground state moments
      if(eia.eq.0..and.xma.ne.0.) 
     1 write(30,175) ia,xma,ji,ni,ipi
      if(eib.eq.0..and.xmb.ne.0.) 
     1 write(31,175) ia,xmb,ji,ni,ipi  
      if(eia.eq.0..and.xqa.ne.0.) 
     1 write(40,175) ia,xqa,ji,ni,ipi
      if(eib.eq.0..and.xqb.ne.0.) 
     1 write(41,175) ia,xqb,ji,ni,ipi   
175   format(i4,1x,f8.3,1x,3i3)        

      if(ia.gt.88.and.ji.eq.8.and.ni.eq.1.and.ipi.eq.0) 
     1 write(50,175) ia,xma,ji,ni,ipi
      if(ia.gt.88.and.ji.eq.8.and.ni.eq.1.and.ipi.eq.0)     
     1 write(51,175) ia,xmb,ji,ni,ipi  
      if(ia.gt.88.and.ji.eq.8.and.ni.eq.1.and.ipi.eq.0)      
     1 write(60,175) ia,xqa,ji,ni,ipi
      if(ia.gt.88.and.ji.eq.8.and.ni.eq.1.and.ipi.eq.0)     
     1 write(61,175) ia,xqb,ji,ni,ipi   

      
cbab ----------- find first final state
171   continue
      read(10,101,end=108) a100
      read(11,101,end=108) b100
c      print *,'a100 ',a100 
c      print *,'b100 ',b100             
      if(a100(13:13).eq.'.') go to 141
      go to 171

cbab -------------- read high final states      
140   continue  
      read(10,101,end=108) a100
      read(11,101,end=108) b100 
141   continue

cbab --------- blank means we have finished with final state 
cbab --------- go back and read another initial state
      if(a100.eq.' ') go to 106
      if(b100.eq.' ') go to 106      
cbab --------- be sure (j,n,pi) of final states match   
      if(a100(18:30).ne.b100(18:30)) go to 140   
          
       
   
cbab --------- read properties of final states
      read(a100(23:23),*,end=108) ap        
      if(ap.eq.'+') ipf = 0
      if(ap.eq.'-') ipf = 1              
      read(a100(19:20),*,end=108) jf
      read(a100(39:48),*,end=108) eg         
      read(a100(12:18),*,end=108) efa    
      read(b100(12:18),*,end=108) efb            
      read(a100(27:28),*,end=108) nf    
cbab ----------- read b(m1) and b(e2)
      read(a100(55:81),*,end=108) bm1a,be2a       
      read(b100(55:81),*,end=108) bm1b,be2b
c      print 160,ap,x2a,eg,ia,ji,jf,ni,nf,ipi,ipf
c160   format(a1,1x,2f8.3,1x,i3,1x,2i3,1x,2i3,1x,2i3)
       
      if(eg.le.0.) go to 140   
      if(ni.gt.1) go to 140
      if(nf.gt.1) go to 140      
      if(ipi.ne.ipf) go to 140
      xji = ji
          
      as = ' '
      kkk = kkk + 1
cbab --------- mark final states with large b
      if(b2a.gt.200) as = '*'
      
      if(be2a.ne.0.) write(20,151) 
     1 be2a,be2b,be2a-be2b,as,kkk,ia,eia,efa,ji,jf,ipi,ipf,ni,nf
c      if(be2a.ne.0.) print 151,be2a,be2b,be2a-be2b,as,kkk,ia,eia,efa,
c     1 ji,jf,ipi,ipf,ni,nf  
     
      if(bm1a.ne.0.) write(21,151) 
     1 bm1a,bm1b,bm1a-bm1b,as,kkk,ia,eia,efa,ji,jf,ipi,ipf,ni,nf
      if(bm1a.ne.0.) print 151,bm1a,bm1b,bm1a-bm1b,as,kkk,ia,eia,efa,
     1 ji,jf,ipi,ipf,ni,nf       
         
151   format(3f9.3,1x,a1,1x,2i3,1x,2f8.3,1x,2i3,1x,2i3,1x,2i3,1x,a2)
    
cbab ------------- go back and read another final state    
      go to 140
      
108   continue

  
      
      close(10)
      

      
300   continue
      
      end 
      
c      open(unit=20,file='m8r.dat')
c      open(unit=21,file='m8t.dat')         
c      open(unit=30,file='q8r.dat')
c      open(unit=31,file='q8t.dat')  
 
c      open(unit=30,file='mr.dat')
c      open(unit=31,file='mt.dat')  

  

c      name1 = 'c:\rsh-nushellx\n50\a30\'//name//'.deo'  
c      name1 = 'c:\rsh-nushellx\n50\p35i2\'//name//'.deo'   

c      open(unit=20,file='a30a35be2.dat')        
      
      
      subroutine fch(file)
cbab removes up to 2 spaces 
      character(len=*) file
      k = len(file)
      if(k.ge.200) print *,'length of file is more that 100',
     1' in subrou',k
      if(k.ge.200) return            
      do 20 kk = 1,2
      do 10 i = 1,k
      if(file(i:i).eq.' ') go to 11
10    continue
      return
11    ia = i
      do 12 i = ia+1,k
      if(file(i:i).ne.' ') go to 13
12    continue
      return
13    ib = i
      il = k-ib+1
      do 14 i = 1,il
      i1 = ia+i-1
      i2 = ib+i-1
      file(i1:i1) = file(i2:i2)
14    continue
      do 15 i = ia+il,k
15    file(i:i) = ' '
20    continue
      return
      end                     
      
