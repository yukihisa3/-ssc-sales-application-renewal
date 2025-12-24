/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSSY0006                             *  ./
/. *   JOB-NAME    :    発注集計表データ抽出処理             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0006'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*8,VALUE-'19990908'
    VAR       ?P2       ,STRING*4,VALUE-'1111'
    VAR       ?P3       ,STRING*8,VALUE-'00000173'
    VAR       ?P4       ,STRING*2,VALUE-'00'
    VAR       ?P5       ,STRING*1,VALUE-'1'
    VAR       ?P6       ,STRING*1,VALUE-'1'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  出荷明細データ抽出                                          ./
SSY0006B:

    ?STEP :=   'SSY0006B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-SHWHACF,TOFILE-SHWHACF.TOKFLIB
    CALL      PGM-SSY0006B.TOKELIB,PARA-(?P1,?P2,?P3,?P4,?P5,?P6)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    IF        ?P6        =  '2'   THEN
              GOTO AITESCD
    END


/.  ＳＯＲＴ　サカタ商品コード順                                ./
SORT1:

    ?STEP :=  'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT       INFILE-SHWHACF.TOKFLIB,INRL-200,INBF-5,
              OUTFILE-SHWHACF.TOKFLIB,OUTBF-5,
              KEY-28!4!CA,KEY1-26!2!CA,KEY2-40!8!CA,KEY3-61!16!CA,
              KEY4-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    ELSE
              GOTO SSY0008L
    END

RTN1:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


/.                                                        ./
AITESCD:

/.  ＳＯＲＴ　相手商品コード                                    ./
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
