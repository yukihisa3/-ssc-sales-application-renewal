/.*****************************************************************./
/.*      儘僌弌椡惂屼張棟                                         *./
/.*      MEMBER  NAME       PLG00200                               *./
/.*                                                               *./
/.*****************************************************************./

    PGM   (P1-?NAME1,P2-?AREA,P3-?YMD)
    PARA  ?NAME1  ,STRING*08,IN,VALUE-'        '
    PARA  ?AREA   ,STRING*06,IN,VALUE-'      '
    PARA  ?YMD    ,STRING*06,IN,VALUE-'000000'
    VAR   ?P      ,STRING*99
    VAR   ?C      ,STRING*30,VALUE-'PLG00200        JOB  END'
    VAR   ?D      ,STRING*3,VALUE-' - '
    VAR   ?PGMEC  ,INTEGER
    VAR   ?WKSTN  ,NAME
    VAR   ?PFROM  ,NAME
    VAR   ?PFROMN ,STRING*7,VALUE-'       '
    VAR   ?LNAME  ,NAME
    VAR   ?LNAME0 ,STRING*08,VALUE-'WSLOGDMP'

    ACTMSGQ   MAXRMSG-50,UC-@YES
    DEFLIBL   LIBL-TOKKLIB/TOKELIBO/TOKCLIBO/TOKELIB/TOKFLIB
 RUN1:
    SNDMSG    MSG-?NAME1,TO-XCTL.@ORGPROF
    SNDMSG    MSG-?AREA,TO-XCTL.@ORGPROF
    SNDMSG    MSG-?YMD,TO-XCTL.@ORGPROF

    ?WKSTN    :=  %NAME(?NAME1)
    ?PFROMN   :=  '@' && ?AREA
    ?PFROM    :=  %NAME(?PFROMN)

    ?LNAME    :=  %NAME(?LNAME0)
    CHGCMVAR '@OUTQN',XXLOGQ
    CHGCMVAR '@OUTDSN',?LNAME
    IF   ?AREA = '@ACTWS'  THEN
         DMPWSLOG WKSTN-?WKSTN,FROM-@ACTWS,DATE-?YMD
         IF  @PGMEC   ^=   0    THEN
             ?C := '** DMPWSLOG 合菽薮装 ABEND **' GOTO OWARIXX END
    ELSE
         DMPWSLOG WKSTN-?WKSTN,FROM-@TOP,DATE-?YMD
         IF  @PGMEC   ^=   0    THEN
             ?C := '** DMPWSLOG 合菽薮装 ABEND **' GOTO OWARIXX END
    END
    SNDMSG    MSG-'劅劅劅劅劅劅劅仺庢摼廔椆',TO-XCTL.@ORGPROF

 OWARIXX:
    ?PGMEC    :=  @PGMEC
    ?C        :=  ?C   &&   ?D
    IF        ?PGMEC   ^=   0   THEN
              ?P    :=   ?C   &&   @PGMEM
    ELSE
              ?P    :=   ?C                           END
    CHGCMVAR  CMVAR-'@PGMEM',VALUE-?P
    SNDMSG    MSG-?P,TO-XCTL.@ORGPROF
    CHGCMVAR '@OUTDSN',XSYSLIST
    RETURN    PGMEC-?PGMEC

