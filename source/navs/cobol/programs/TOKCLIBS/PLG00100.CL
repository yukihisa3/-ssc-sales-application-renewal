/.******************************************************************./
/.*                                                                *./
/.*      MEMBER  NAME       PLG00100                               *./
/.*                                                                *./
/.******************************************************************./

    PGM  (P1-?KUBUN)
    PARA  ?KUBUN  ,STRING*1,IN,VALUE-' '
    VAR   ?P      ,STRING*99
    VAR   ?C      ,STRING*30,VALUE-'PLG00100       JOB  END'
    VAR   ?D      ,STRING*3,VALUE-' - '
    VAR   ?PGMEC  ,INTEGER
    VAR   ?ENTNAME,NAME
    VAR   ?LIBNAME,NAME
    VAR   ?LABEL,STRING*8
    VAR   ?NAME,STRING*8

    DEFLIBL   LIBL-TOKKLIB/TOKELIBO/TOKCLIBO/TOKFLIB/TOKELIB
    SNDMSG    MSG-'PLG00100       JOB  START',TO-XCTL.@ORGPROF
    ?P := '?KUBUN = ' && ?KUBUN
    SNDMSG ?P,TO-XCTL
 RUN1:
    OVRPRTF FILE-XLIST,TOFILE-COMP1.XUCL
    OVRPRTF FILE-XELIST,TOFILE-COMP2.XUCL
    CALL PGM-SLG0010I.TOKELIBO,PARA-(?KUBUN)
      IF  @PGMEC   ^=   0    THEN
         ?C  :=  '** PLG00100-RUN1     ABEND **'  GOTO  OWARIXX  END

 OWARIXX:
    ?PGMEC    :=  @PGMEC
    ?C        :=  ?C   &&   ?D
    IF        ?PGMEC   ^=   0   THEN
              ?P    :=   ?C   &&   @PGMEM
    ELSE
              ?P    :=   ?C                           END
    CHGCMVAR  CMVAR-'@PGMEM',VALUE-?P
    SNDMSG    MSG-?P,TO-XCTL.@ORGPROF
    CHGCMVAR '@OUTQN',XSYSLSTQ
    RETURN    PGMEC-?PGMEC
