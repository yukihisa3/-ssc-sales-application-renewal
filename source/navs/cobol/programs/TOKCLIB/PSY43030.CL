/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    欠品案内データ作成（アークランド）   *  ./
/. *   JOB-ID      :    PSY43030                             *  ./
/. *   JOB-NAME    :    欠品案内データ作成                   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY43030'
    VAR       ?STEP     ,STRING*8
    VAR       ?KENFLG   ,STRING*1,VALUE-' '         /.件数ＦＬＧ ./
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
/.-----------------------------------------------------------./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
/.-----------------------------------------------------------./
    VAR       ?JDATE    ,STRING*8,VALUE-'        '  /.ﾊﾞｯﾁ日付   ./
    VAR       ?JTIME    ,STRING*4,VALUE-'    '      /.ﾊﾞｯﾁ時間   ./
    VAR       ?JTOKCD   ,STRING*8,VALUE-'        '  /.ﾊﾞｯﾁ取引先 ./
    VAR       ?JKBN     ,STRING*1,VALUE-'1'         /.処理区分   ./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '欠品案内データ作成'
/.アークランドサカモト　欠品案内データ作成処理./
STEP00:
    ?JBID    :=   'PSY43030'
    ?CLID    :=   'PSY43030'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

/.##欠品案内データ作成指示##./
SSY4307I:

    ?STEP :=   'SSY4307I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SSY4307I.TOKELIB,
              PARA-(?JDATE,?JTIME,?JTOKCD,?JKBN)
    IF        @PGMEC    ^=   0    THEN
             IF  @PGMEC = 4010        THEN
                 SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
                 RETURN PGMEC-@PGMEC
             ELSE
                 ?KEKA4 :=  '（抽出ＤＴ指示）'
                 GOTO ABEND
             END
    END

/.##欠品案内データ作成##./
SSY4303B:

    ?STEP :=   'SSY4303B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-ARKKEPL1,TOFILE-ARKKEPL1.TOKFLIB
    OVRF      FILE-ARKERRF,TOFILE-ARKERRF.TOKFLIB
    CALL      PGM-SSY4303B.TOKELIB,
              PARA-(?JDATE,?JTIME,?JTOKCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '（抽出ＤＴ作成）'
              GOTO ABEND END

/.##欠品案内データエラーリスト##./
SSY4308L:

    ?STEP :=   'SSY4308L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ARKERRF,TOFILE-ARKERRF.TOKFLIB
    CALL      PGM-SSY4308L.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '（エラーリスト発行）'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '確認後、アークランドサカモトへデータの'
    ?KEKA3 :=  '送信を行なって下さい。'
    ?KEKA4 :=  '確認して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
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