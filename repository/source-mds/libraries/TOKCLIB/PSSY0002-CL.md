# PSSY0002

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSSY0002.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSSY0002                             *  ./
/. *   JOB-NAME    :    出荷明細表データ抽出処理             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0002'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*1,VALUE-'1'
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

/.  出荷明細データ抽出                                          ./
SSY0002B:

    ?STEP :=   'SSY0002B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SHWSYUKF,TOFILE-SHWSYUKF.TOKFLIB
    CALL      PGM-SSY0002B.TOKELIB,PARA-(?P1,?P2,?P3,
                    ?P4,?P5,?P6,?P7,?P8,?P9,?P10,?P11,?P12)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    IF        ?P1        =  '2'   THEN
              GOTO TANAKEI
    END


/.  ＳＯＲＴ　                                                  ./
SORT1:

    ?STEP :=  'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWSYUKF.TOKFLIB,INRL-200,INBF-5,
              OUTFILE-SHWSYUKF.TOKFLIB,OUTBF-5,
              KEY-1!20!CA,KEY1-21!2!CA,KEY2-126!8!CA,KEY3-28!4!CA,
              KEY4-23!5!CA,KEY5-32!6!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    ELSE
              GOTO SSY0004L
    END


RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC



/.  部門_番集計                                                ./
TANAKEI:

/.  ＳＯＲＴ　                                                  ./
SORT2:

    ?STEP :=  'SORT2   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWSYUKF.TOKFLIB,INRL-200,INBF-5,
              OUTFILE-SHWSYUKF.TOKFLIB,OUTBF-5,
              KEY-1!20!CA,KEY1-21!2!CA,KEY2-28!4!CA,KEY3-32!6!CA,
              KEY4-54!13!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.  出荷明細データ_番集計                                      ./
SSY0003B:


    ?STEP :=   'SSY0003B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB/TOKFLIB

    OVRF      FILE-SHWSYUKF,TOFILE-SHWSYUKF.TOKFLIB
    OVRF      FILE-SHWSYUTF,TOFILE-SHWSYUTF.TOKFLIB
    CALL      PGM-SSY0003B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    CNVFILE   FILE-SHWSYUTF.TOKFLIB,TOFILE-SHWSYUKF.TOKFLIB,
                            ADD-@NO

/.  出荷明細表作表                                              ./
SSY0004L:

    ?STEP :=   'SSY0004L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-SHWSYUKF,TOFILE-SHWSYUKF.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB

/.  OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB  ./
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
