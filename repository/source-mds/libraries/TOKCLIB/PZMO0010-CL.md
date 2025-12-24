# PZMO0010

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PZMO0010.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ殿　            　　　　　　　          *  ./
/. *   SYSTEM-NAME :     在庫システム                        *  ./
/. *   JOB-ID      :    PZMO0010                             *  ./
/. *   JOB-NAME    :    在庫管理表　　　　　                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZMO0010'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


/.  在庫抽出　　　　　　                                        ./
ZMO0010B:

    ?STEP :=   'ZMO0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CRTFILE   FILE-ZZAIWK.@TEMP,SIZE-4000,ESIZE-100
    CHGFILE   FILE-ZZAIWK,ORG-@SF
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-ZZAIWK,TOFILE-ZZAIWK.@TEMP
    CALL      PGM-ZMO0010B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.  在庫管理表　　　　　                                        ./
ZMO0020B:

    ?STEP :=   'ZMO0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-HMEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZZAIWK,TOFILE-ZZAIWK.@TEMP
    CALL      PGM-ZMO0020B.TOKELIB
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
