/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    セキチュー支払照合　植物　           *  ./
/. *   JOB-ID      :    PSI15200                             *  ./
/. *   JOB-NAME    :    不照合リスト作成                     *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSI15200'
    VAR       ?STEP     ,STRING*8
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名ｾｯﾄ##./
    ?PGNM :=  'セキチュー植物：不照合リスト発行'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##支払データ編集##./
SSI0112B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SSIHARAD,TOFILE-SEKICHUS.TOKKLIB
    OVRF      FILE-SEKSSIF,TOFILE-SEKSSIF.TOKKLIB
    CALL      PGM-SSI1540B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【支払データ編集】'
              GOTO ABEND END

/.##ＳＯＲＴ（取引先＋伝票番号順）##./
SORT1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SEKSSIF.TOKKLIB,INRL-100,INBF-40,
              OUTFILE-SEKSSIF.TOKKLIB,OUTBF-40,
              KEY-5!9!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【支払データＳＯＲＴ】'
              GOTO ABEND END

/.##（セキチュー）不照合リスト##./
SSI1520L:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SEKSSIF,TOFILE-SEKSSIF.TOKKLIB
    OVRF      FILE-SEIGKF21,TOFILE-SETGK151.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI1520L.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【不照合リスト発行】'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIB
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '不照合リスト発行が異常終了しました。'
    ?KEKA2 :=  '再度、実行して頂き同様な場合はＮＡＶへ'
    ?KEKA3 :=  '連絡して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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