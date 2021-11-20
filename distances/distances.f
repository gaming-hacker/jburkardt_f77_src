       PROGRAM main

c*********************************************************************72
c
cc MAIN is the main program for DISTANCES.
c
C------------------------------------------------------------------------
C DISTANCES, Des Higgins, EMBL Data Library, April 1992                 I
C------------------------------------------------------------------------
C Read a multiple alignment file in FASTA or PIR format and output a 
C distance matrix for input to spacer.  The sequences must be aligned
C first.  The gaps in the alignment are read as hyphen characters: "-".
c
       IMPLICIT INTEGER(A-Z)
C***************Maximum problem size*********************
C dimensioned for 5000 residues each in 650 sequences.
C reset these in ALL subroutines where they occur if you change them!!
       PARAMETER (MAXL=5000,MAXN=650)
C********************************************************
       REAL PCSIMM,ABSIMM,DIVIS,KAA
       CHARACTER LINE*120,NAMES(MAXN)*16,SEQS(MAXN,MAXL),DATATYPE*20
       CHARACTER FNAME*50,FNAME2*50,ALPH(MAXN),ANS,SEQTYPE*20
       CHARACTER aacids*20
       INTEGER*2 LENS(MAXN),SEQINT(MAXN,MAXL)
       REAL distance(MAXN,MAXN),aamat(0:20,0:20)
       LOGICAL DNA,OPENOK,DIST
       data aacids /'DEKRHNQSTILVFWYCMAGP'/

       LIN   = 66
       LOUT  = 67

       print*
       print*,'Read a multiple alignment in PIR '
       print*,'or FASTA format and output a matrix of'
       print*,'Euclidean distances for SPACER.'
       print*

4      FNAME = '        '
       PRINT*
       PRINT*,' The sequences must be aligned already!'
       PRINT*,' Use PIR or FASTA format with hyphens for gaps.'
       PRINT*
       CALL GETSTR('Multiple alignment file ?        >> ',FNAME)
       OPEN(UNIT=LIN,FILE=FNAME,STATUS='OLD',ERR=4) 

2      PRINT*
       FNAME2 = FNAME
C***       CALL CHFEXT(FNAME2,'DIST',NMLEN)
       CALL CHFEXT(FNAME2,'dist',NMLEN)

       CALL GETSTR('Name for output (matrix) file ?   [  '
     &   //FNAME2(1:NMLEN)//' ] >> ',FNAME2)

       OPEN(UNIT=Lout,FILE=FNAME2,ERR=2,STATUS='UNKNOWN')
       CALL READSEQS(LIN,NSEQS,SEQS,NAMES,LENS,CONLEN)

       PRINT*
       PRINT*,'Number of sequences = ',NSEQS
       PRINT*
       PRINT*,'Consensus length    = ',CONLEN
       PRINT*

       PRINT*
       DO 5 I = 1,NSEQS
       WRITE(*,'(A,I4,A,A,I5)') ' Sequence # ',I,NAMES(I),
     *                          ' length = ',LENS(I)
5      CONTINUE
       PRINT*

       IT = 1
       CALL GETINT
     &  (' Type 1 to toss all gaps; 0 not to [1] >> ',0,1,IT)
       IF(IT.EQ.1) CALL TOSSGAPS(SEQS,CONLEN,NSEQS)
       IT = 0
       CALL GETINT
     &  (' Type 1 for DNA; 0 for protein     [0] >> ',0,1,IT)
       IF(IT.EQ.1) DNA = .TRUE.
       IF(DNA) AACIDS = 'ACGTU'

         do 10 i = 0,20
         do 11 j = 0,20
11       aamat(i,j) = 1.0
         aamat(i,i) = 0.0
10       continue
         aamat(0,0) = 1.0
         AAMAT(4,5) = 0.0
         AAMAT(5,4) = 0.0

       IF(.NOT.DNA) THEN
         PRINT*
         it = 1
         CALL GETINT(
     & ' Type 1 for ID distances; 0 for Smith aa matrix [1] >> '
     & ,0,1,IT)
         IF(IT.EQ.0) CALL SETAAMAT(AAMAT,AACIDS)
       END IF

       DO 16 I = 1,NSEQS
       DO 15 J = 1,CONLEN
       SEQINT(I,J) = INDEX(AACIDS,SEQS(I,J))
15     CONTINUE
16     CONTINUE

       call distmat(nseqs,conlen,distance,seqINT,aamat)

         DO 7 I = 1,NSEQS
7        distance(I,I) = 0.0

       write(lout,*) nseqs
       do 6 i = 1,nseqs
       write(lout,'(5f12.4)') (distance(i,j),j=1,i)
6      continue

       STOP
       END
       SUBROUTINE SETAAMAT(AAMAT,AACIDS)

c*********************************************************************72
c
cc SETAAMAT
c
       IMPLICIT INTEGER(A-Z)
       REAL AAMAT(0:20,0:20),idmatch,match1,match2,nomatch
       CHARACTER AACIDS*(*)

       IDMATCH = 0.0
       MATCH1  = 1.0
       MATCH2  = 2.0
       NOMATCH = 3.0

       DO 1 I = 0,20
       DO 1 J = 0,20
1      AAMAT(I,J) = NOMATCH

       CALL SETAASCORE('DEKRHNQST',MATCH2,AAMAT,AACIDS)
       CALL SETAASCORE('ILVFWYCM',MATCH2,AAMAT,AACIDS)

       CALL SETAASCORE('DE',MATCH1,AAMAT,AACIDS)
       CALL SETAASCORE('KRH',MATCH1,AAMAT,AACIDS)
       CALL SETAASCORE('NQ',MATCH1,AAMAT,AACIDS)
       CALL SETAASCORE('ST',MATCH1,AAMAT,AACIDS)
       CALL SETAASCORE('ILV',MATCH1,AAMAT,AACIDS)
       CALL SETAASCORE('FWY',MATCH1,AAMAT,AACIDS)
       CALL SETAASCORE('AG',MATCH1,AAMAT,AACIDS)
       
       DO 2 I = 1,20
2      AAMAT(I,I) = IDMATCH

       RETURN
       END
       SUBROUTINE SETAASCORE(GROUP,MATCH,AAMAT,AACIDS)

c*********************************************************************72
c
cc SETAASCORE
c
       IMPLICIT INTEGER(A-Z)
       REAL AAMAT(0:20,0:20),MATCH
       CHARACTER GROUP*(*),AACIDS*(*)

       GRLEN = LEN(GROUP)
 
       DO 10 I = 1,GRLEN
       DO 10 J = 1,GRLEN
       COL = INDEX(AACIDS,GROUP(I:I))
       ROW = INDEX(AACIDS,GROUP(J:J))
       AAMAT(COL,ROW) = MATCH
       AAMAT(ROW,COL) = MATCH
10     CONTINUE
      
       RETURN
       END
       SUBROUTINE TOSSGAPS(SEQS,CONLEN,NSEQS)

c*********************************************************************72
c
cc TOSSGAPS
c
       IMPLICIT INTEGER (A-Z)
       PARAMETER (MAXL=5000,MAXN=650)
       CHARACTER SEQS(MAXN,MAXL)

           DO 3 I = 1,CONLEN
             DO 1 J = 1,NSEQS
               IF(SEQS(J,I).EQ.'-') THEN
                 DO 2 K = 1,NSEQS
2                SEQS(K,I) = '-'
                 GO TO 3
               END IF
1            CONTINUE
3          CONTINUE

           RETURN
       END
       SUBROUTINE CHFEXT(FNAME,EXT,NEWLEN)

c*********************************************************************72
c
cc CHFEXT changes the extension of a filename.
c
       IMPLICIT INTEGER(A-Z)
       CHARACTER FNAME*(*),EXT*(*)

       STRLEN = LEN(FNAME)
       DO 10 I = STRLEN,1,-1
       IF(FNAME(I:I).NE.' ') THEN      
         NAMLEN = I
         GO TO 11
       END IF
10     CONTINUE
11     CONTINUE

       STRLEN = LEN(EXT)
       DO 20 I = STRLEN,1,-1
       IF(EXT(I:I).NE.' ') THEN      
         EXTLEN = I
         GO TO 21
       END IF
20     CONTINUE
21     CONTINUE

       BRPOS  = INDEX(FNAME,']')
       NAMBEG = BRPOS + 1
       DOTPOS = INDEX(FNAME(NAMBEG:NAMLEN),'.')
       DOTPOS = DOTPOS + BRPOS
       IF(DOTPOS.EQ.0) THEN
         FNAME  = FNAME(1:NAMLEN)//'.'//EXT(1:EXTLEN)
         NEWLEN = NAMLEN + EXTLEN + 1
       ELSE
         FNAME  = FNAME(1:DOTPOS)//EXT(1:EXTLEN)
         NEWLEN = DOTPOS + EXTLEN
       END IF
   
       RETURN
       END
       SUBROUTINE DISTMAT(NSEQS,CONLEN,DISTANCE,SEQINT,aamat)

c*********************************************************************72
c
cc DISTMAT calculates a similarity matrix between pairs os sequences.
c
       IMPLICIT INTEGER(A-Z)
       PARAMETER (MAXL=5000,MAXN=650)

       REAL PCSIMM,ABSIMM,DIVIS,KAA,aamat(0:20,0:20)
       INTEGER*2 SEQINT(MAXN,MAXL)
       INTEGER*2 LENS(MAXN)
       REAL DISTANCE(MAXN,MAXN),DIST
       LOGICAL DNA

       DO 26 I = 1,NSEQS
       DO 26 J = I,NSEQS
       DIST   = 0.0       
       DO 25 K = 1,CONLEN
       IF(SEQINT(I,K).NE.0.AND.SEQINT(J,K).NE.0) 
     &      DIST = DIST + AAMAT(SEQINT(I,K),SEQINT(J,K))         
25     CONTINUE
       IF(NSEQS.LE.10) 
     &    WRITE(*,'(I4,A,I4,A,F6.0)') I,' vs. ',J,' DIST = ',DIST
       IF(DIST.NE.0.0) DIST = SQRT(DIST)
       DISTANCE(I,J) = DIST
       DISTANCE(J,I) = DIST
26     CONTINUE

       RETURN
       END
       SUBROUTINE READSEQS(LIN,NSEQS,SEQS,NAMES,LENS,CONLEN)

c*********************************************************************72
c
cc READSEQS reads a FASTA or PIR format multiple aligment file.
c
       IMPLICIT INTEGER(A-Z)
       PARAMETER (MAXL=5000,MAXN=650,INLEN=250)

       LOGICAL PIRFORM
       CHARACTER LINE*(inlen),NAMES(MAXN)*16,SEQS(MAXN,MAXL)
       CHARACTER ch*1,digits*40
       INTEGER*2 LENS(MAXN)

C characters to be ignored
       digits = '0123456789.,;:\|/?[]{}_=+)(&^%$#@!`~'

       READ(LIN,'(A)') LINE
C check for a > as first character
       if(line(1:1).ne.'>') then
          print*,char(7)
          print*,'Sequences must be in FASTA or PIR format'
          print*,'Each sequence name must start with a > character'
          print*,'EXIT'
          stop
       end if
       pirform = .FALSE.
C check whether FASTA or PIR format
       if(line(4:4).eq.';') pirform = .TRUE.
       backspace(lin)

       nseqs   = 0

1      read(lin,'(a)',end=90) line
       nseqs = nseqs + 1
       if(nseqs.gt.maxn) then
         print*,char(7),'Maximum number of seqs. exceeded'
         print*,'Maximum number is',MAXN
         stop
       end if
       lens(nseqs) = 0
       if(pirform) then
         names(nseqs) = line(5:20)
       else
         names(nseqs) = line(2:17)
       end if
C skip an extra line if PIR
       if(pirform) read(lin,'(a)') line

2      read(lin,'(a)',end=90) line
C look for the start of the next sequence
       if(line(1:1).eq.'>') then
          backspace(lin)
          go to 1
       end if
       do 10 i = 1,inlen
       ch = line(i:i)
C skip blanks
       if(ch.eq.' ') go to 10
C skip any non alphabetic characters
       if(index(digits,ch).ne.0) go to 10
C look for a * if PIR format is used (= end of sequence)
       if(pirform.and.ch.eq.'*') then
3        read(lin,'(a)',end=90) line
C look for the start of the next sequence
         if(line(1:1).ne.'>') go to 3
         backspace(lin)
         go to 1
       end if
C convert to upper case
C ASCII ONLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       if(ichar(ch).ge.97) ch = char(ichar(ch)-32)
       lens(nseqs) = lens(nseqs) + 1
       if(lens(nseqs).gt.MAXL) then
         print*,char(7),'Maximum seq. length exceeded'
         print*,'Maximum length is',MAXL
         stop
       end if
       seqs(nseqs,lens(nseqs)) = ch       
10     continue
C go back to read more sequence
       go to 2

90     continue

       conlen = lens(1)
       do 20 i = 1,nseqs
       if(lens(i).ne.conlen) then
         print*,char(7),'Sequences MUST be aligned already!!'
         print*,'Sequence number:',i,' Length = ',lens(i)
       end if
       if(lens(i).gt.conlen) conlen = lens(i)
20     continue

       DO 24 I = 1,NSEQS
       LENS(I) = 0
       DO 24 J = 1,CONLEN
C count the number of non-blanks in each sequence
       IF(SEQS(I,J).NE.'-') LENS(I) = LENS(I) + 1
24     CONTINUE

       RETURN
       END
       SUBROUTINE GETSTR(PROMPT,ANSTR)

c*********************************************************************72
c
cc GETSTR prompts for a string input.
c
        IMPLICIT INTEGER(A-Z)
        CHARACTER*(*) PROMPT,ANSTR
        CHARACTER*70 TEMPSTR
        ANLEN= LEN(ANSTR)
        PLEN = LEN(PROMPT)
        DO 10 I = PLEN,1,-1
        IF(PROMPT(I:I).NE.' ') THEN
           PLEN = I
           GO TO 11
        END IF
10      CONTINUE

11      continue
        WRITE(*,'(1X,A,A$)') PROMPT(1:PLEN),' '
        TEMPSTR = ' '
        READ(*,'(A)') TEMPSTR
        IF(TEMPSTR.EQ.' ') then
          RETURN
        end if
        ANSTR = TEMPSTR(1:ANLEN)
        RETURN
        END
        SUBROUTINE GETINT(PROMPT,MINI,MAXI,ANSI)

c*********************************************************************72
c
cc GETINT prompts for an integer input.
c
        IMPLICIT INTEGER(A-Z)
        CHARACTER*(*) PROMPT

        PLEN = LEN(PROMPT)
        DO 10 I = PLEN,1,-1
        IF(PROMPT(I:I).NE.' ') THEN
           PLEN = I
           GO TO 11
        END IF
10      CONTINUE

11      WRITE(*,'(1X,A,A$)') PROMPT(1:PLEN),' '
        TEMP = 0
        READ(*,'(BN,I8)',ERR=11) TEMP
        IF(TEMP.EQ.0) RETURN
        IF(TEMP.GT.MAXI) THEN
          PRINT*,CHAR(7)
          PRINT*,'ERROR: too big ....... max = ',MAXI
          PRINT*
          GO TO 11
        ELSE IF(TEMP.LT.MINI) THEN
          PRINT*,CHAR(7)
          PRINT*,'ERROR: too small ..... min = ',MINI
          PRINT*
          GO TO 11 
        ELSE
          ANSI = TEMP
        END IF
        RETURN
        END



