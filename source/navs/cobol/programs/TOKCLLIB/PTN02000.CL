/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理システム                     *  ./
/. *   JOB-ID      :    PTN02000                             *  ./
/. *   JOB-NAME    :    実_更新（Ｄ３６５用）               *  ./
/. *                                     オフコンクラウド    *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PTN02000'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '実_更新（Ｄ３６５用）'

/.  在庫管理システム                                           ./
SAVFILE:

    ?OPR1  :=  '　＃＃　_卸確定更新処理（Ｄ３６５用）　＃＃'
    ?OPR2  :=  '　_卸確定処理処理を行います。'
    ?OPR3  :=  '　宜しいですか？'
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?OPR1  :=  '　＃＃＃＃＃＃＃　_卸確定更新処理　＃＃＃＃＃＃　'
    ?OPR2  :=  '　在庫ファイルのバックアップを行います。'
    ?OPR3  :=  ''
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    SAVFILE FILE-YTTANAF.TOKDTLIB/SKTANAF.TOKDTLIB/ZAMZAIF.TOKFLIB/
            KKTANAF.TOKDTLIB/KJTANAF.TOKDTLIB,TODEV-@NONE,
            MODE-@USED,TOPATH-'/HGNAS/PTN02000',
            REP-@YES,COMPRESS-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '関係Ｆ／在庫Ｍバックアップ'
              GOTO ABEND END

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKDTLIB

/.##在庫Ｍ　現在庫数クリア##./
STA0070B:

    ?STEP :=   'STA0070B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZAMZAIF,TOFILE-ZAMZAIF.TOKFLIB
    CALL      PGM-STA0070B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '在庫Ｍ　現在庫数クリア'
              GOTO ABEND END

/.##_卸確定Ｆ　集計##./
STN0200B:

    ?STEP :=   'STN0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KKTANAL2,TOFILE-KKTANAL2.TOKDTLIB
    OVRF      FILE-ZTANAWK,TOFILE-ZTANAWK.TOKFLIB
    CALL      PGM-STN0200B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸確定Ｆ　集計'
              GOTO ABEND END

/.##実_更新##./
STA0080B:

    ?STEP :=   'STA0080B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZTANADT,TOFILE-ZTANAWK.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-STA0080B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '実_更新'
              GOTO ABEND END

/.##現在庫数再計算##./
STA0090B:

    ?STEP :=   'STA0090B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZAMZAIF,TOFILE-ZAMZAIF.TOKFLIB
    CALL      PGM-STA0090B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '現在庫数再計算'
              GOTO ABEND END

/.##再在庫引当処理##./
PTA01000:

    DLTOVRF   FILE-*

    CALL      PTA01000.TOKELIB

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '実_更新処理が正常終了しました。'
    ?KEKA2 :=  '_卸予定データの作成を開始して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '実_更新処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
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