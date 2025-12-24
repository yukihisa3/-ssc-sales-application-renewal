# PZHA0040

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PZHA0040.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理                             *  ./
/. *   JOB-ID      :    ZHA0040O                             *  ./
/. *   JOB-NAME    :    発注入力　　　　                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZHA0040'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN    :=       @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    CRTFILE FILE-ZHACHWK.@TEMP,SIZE-300
    CHGFILE FILE-ZHACHWK.@TEMP


    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


/.  手書伝票入力                                                ./
ZHA0040O:
    DEFLIBL            TOKELIB/TOKFLIB
    ?STEP :=   'ZHA0040O'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL4,TOFILE-SHOTBL4.TOKFLIB
    OVRF      FILE-ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB
    OVRF      FILE-ZHACHDT1,TOFILE-ZHACHDT1.TOKFLIB
    OVRF      FILE-ZNYUKDT1,TOFILE-ZNYUKDT1.TOKFLIB
    SNDMSG    ?WKSTN,TO-XCTL
    OVRF      FILE-ZHACHWK,TOFILE-ZHACHWK.@TEMP
    CALL      PGM-ZHA0040O.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    CNVFILE FILE-ZHACHWK.@TEMP,TOFILE-ZHACHWK.TOKFLIB
    DLTFILE FILE-ZHACHWK.@TEMP

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
