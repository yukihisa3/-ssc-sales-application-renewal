# PSSY0004

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSSY0004.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSSY0004                             *  ./
/. *   JOB-NAME    :    出荷明細表データ作表処理             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0004'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*1,VALUE-'2'
    VAR       ?P2       ,STRING*8,VALUE-'19990908'
    VAR       ?P3       ,STRING*4,VALUE-'1111'
    VAR       ?P4       ,STRING*8,VALUE-'00000173'
    VAR       ?P5       ,STRING*2,VALUE-'11'
    VAR       ?P6       ,STRING*5,VALUE-'00014'
    VAR       ?P7       ,STRING*5,VALUE-'00016'
    VAR       ?P8       ,STRING*8,VALUE-'00000000'
    VAR       ?P9       ,STRING*2,VALUE-'00'
    VAR       ?P10      ,STRING*2,VALUE-'99'
    VAR       ?P11      ,STRING*6,VALUE-'000000'
    VAR       ?P12      ,STRING*6,VALUE-'999999'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  出荷明細表作表                                              ./
SSY0004L:

    ?STEP :=   'SSY0004L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-SHWSYUKF,TOFILE-SHWSYUKF.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB

/.  OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-SKTELIB  ./
    CALL      PGM-SSY0004L.TOKELIB,PARA-(?P1)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN2:

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
