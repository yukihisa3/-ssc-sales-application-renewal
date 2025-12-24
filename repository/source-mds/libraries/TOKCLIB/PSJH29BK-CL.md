# PSJH29BK

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSJH29BK.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成                   *  ./
/. *   JOB-ID      :    PSJH2901                             *  ./
/. *   JOB-NAME    :    ジャスコ                             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSJH2901'
    VAR       ?STEP     ,STRING*8
    VAR       ?PARA     ,STRING*14,VALUE-'199909091200A1'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.データ種別毎振り分け                                     ./
SJH2900B:

    ?STEP :=   'SJH2900B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CVCS256,TOFILE-JUSCO.ONLBLIB
    OVRF      FILE-DATAF1,TOFILE-JUSDATA1.TOKFLIB
    OVRF      FILE-DATAF2,TOFILE-JUSCO.TOKFLIB
    OVRF      FILE-DATAF3,TOFILE-JUSDATA3.TOKFLIB
    OVRF      FILE-DATAF4,TOFILE-JUSDATA4.TOKFLIB
    OVRF      FILE-DATAF5,TOFILE-JUSDATA5.TOKFLIB
    CALL      PGM-SJH2900B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.  データ変換                                                  ./
SJH2901B:

    ?STEP :=   'SJH2901B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB

    OVRF      FILE-CVCSG001,TOFILE-JUSCO.TOKFLIB
    OVRF      FILE-JHMKENL1,TOFILE-JHMKENL1.TOKFLIB
    OVRF      FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRF      FILE-FURIMS1,TOFILE-FURIMS1.TOKFLIB
    CALL      PGM-SJH2901B.TOKELIB,PARA-(?PARA)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END




RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
