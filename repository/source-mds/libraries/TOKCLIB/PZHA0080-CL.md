# PZHA0080

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PZHA0080.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理                             *  ./
/. *   JOB-ID      :    ZHA0080B                             *  ./
/. *   JOB-NAME    :    発注書                               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZHA0080'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


/.  発注書                                                      ./
ZHA0080B:
    DEFLIBL    TOKFLIB/TOKELIB/TOKFLIB
    ?STEP :=   'ZHA0080B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB

    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL,SPOOL-@YES,
            OUTQ-XOUTQ2,MEDLIB-TOKELIB
    OVRF      FILE-ZHACHDT2,TOFILE-ZHACHDT2.TOKFLIB
    OVRF      FILE-ZHACHD1,TOFILE-ZHACHD1.TOKFLIB
    OVRF      FILE-ZSOMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-HTOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-HTENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-HMEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-ZHA0080B.TOKELIB
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
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'


    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
