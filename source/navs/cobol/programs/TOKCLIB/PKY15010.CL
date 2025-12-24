/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    　　　　　　　　　　　　　　         *  ./
/. *   JOB-ID      :    PKY15010                             *  ./
/. *   JOB-NAME    :    発注集計表データ抽出　               *  ./
/. *               :    　　　　　　　                       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PKY15010'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB
/.  発注集計表ファイル作成                                      ./
SKY1501B:

    ?STEP :=  'SKY1501B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HDENJNL,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-UHACHUD,TOFILE-UHACHUD.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRDSPF   FILE-DSPFILE,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SKY1501B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END

/.  ＳＯＲＴ（部門コード，ルート，納品日，商品コード，店舗コード）./
SORT:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-UHACHUD.TOKFLIB,INRL-200,INBF-5,
              OUTFILE-UHACHUD.TOKFLIB,OUTBF-5,
              KEY-12!4!CA,KEY1-167!2!CA,KEY2-23!6!CA,KEY3-37!13!CA,
              KEY4-7!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.  発注集計表（共通）                                         ./
SKY1502B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG    '***  発注集計表作成　***',TO-XCTL

    OVRF      FILE-UHACHUD,TOFILE-UHACHUD.TOKFLIB
    OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL
    CALL      PGM-SKY1502B.TOKELIB
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
