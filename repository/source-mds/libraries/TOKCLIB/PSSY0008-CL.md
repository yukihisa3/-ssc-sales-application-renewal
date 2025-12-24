# PSSY0008

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSSY0008.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSSY0008                             *  ./
/. *   JOB-NAME    :    発注集計表作表                       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0008'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*8,VALUE-'19990908'
    VAR       ?P2       ,STRING*4,VALUE-'1111'
    VAR       ?P3       ,STRING*8,VALUE-'00000173'
    VAR       ?P4       ,STRING*2,VALUE-'11'
    VAR       ?P5       ,STRING*1,VALUE-'1'
    VAR       ?P6       ,STRING*1,VALUE-'2'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

SORT2:

    ?STEP :=  'SORT2   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWHACF.TOKFLIB,INRL-200,INBF-5,
              OUTFILE-SHWHACF.TOKFLIB,OUTBF-5,
              KEY-28!4!CA,KEY1-26!2!CA,KEY2-40!8!CA,KEY3-48!13!CA,
              KEY4-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.  発注集計表作表                                              ./
SSY0008L:

    ?STEP :=   'SSY0008L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'


    DEFLIBL   TOKELIB

    OVRF      FILE-SHWHACF,TOFILE-SHWHACF.TOKFLIB

/.  OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB  ./
    CALL      PGM-SSY0008L.TOKELIB,PARA-(?P5,?P6)
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
