/. ***********************************************************  ./
/. *     サカタのタネ　営業第２部ＮＡＶＳ基幹システム        *  ./
/. *   SYSTEM-NAME :    営利種子ストックＮＯ管理             *  ./
/. *   JOB-ID      :    PST00300                             *  ./
/. *   JOB-NAME    :    営利種子ストックＮＯ累積ＤＴ削除　　 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PST00300'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./

/.PG名称ｾｯﾄ./

    ?PGNM  :=  '営利種子ストックＮＯ累積ＤＴ削除'

/.ﾗｲﾌﾞﾗﾘﾘｽﾄ登録./

    DEFLIBL TOKELIB/TOKFLIB/TOKDTLIB

/.  ストックＮＯ管理累積ＤＴ削除メッセージ                       ./
STEP00:
    ?OPR1  :=  '【営利種子ストックＮＯ累積ＤＴ削除】'
    ?OPR2  :=  ''
    ?OPR3  :=  '営利種子ストックＮＯ累積ＤＴの削除を'
    ?OPR4  :=  '行ないます。確認お願い致します。'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  ストックＮＯ管理累積ＤＴ削除メッセージ                       ./
STEP01:
    ?OPR1  :=  '！注意！　！注意！　！注意！　！注意！'
    ?OPR2  :=  '！注意！　データが削除されます！注意！'
    ?OPR3  :=  '！注意！　確認して下さい！　　！注意！'
    ?OPR4  :=  '！注意！　　キャンセル→Ｆ９　！注意！'
    ?OPR5  :=  '！注意！　！注意！　！注意！　！注意！'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  ストックＮＯ管理累積ＤＴ削除メッセージ                       ./
STEP02:
    ?OPR1  :=  '！注意！　！注意！　！注意！　！注意！'
    ?OPR2  :=  '！注意！　本当に削除して良い　！注意！'
    ?OPR3  :=  '！注意！　ですか？責任者様に　！注意！'
    ?OPR4  :=  '！注意！　確認して下さい！！　！注意！'
    ?OPR5  :=  '！注意！　！注意！　！注意！　！注意！'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##バックアップ##./
PBACKUP:

    ?STEP :=   'PBACKUP '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SAVFILE FILE-STNOKRF.TOKDTLIB,TODEV-@NONE,MODE-@USED,
            TOPATH-'/HGNAS/STNOKR',REP-@YES,COMPRESS-@YES
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【削除前バックアップ】'
              GOTO ABEND END

/.##ストックＮＯ管理Ｆ削除処理##./
SST0030B:

    ?STEP :=   'SST0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SST0030B.TOKSOLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【ストックＮＯ管理Ｆ削除処理】'
              GOTO ABEND END


RTN:

    ?KEKA1 :=  'ＳＴＮＯ累積ＤＴの削除が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  'ＳＴＮＯ累積ＤＴの削除が異常終了しました。'
    ?KEKA2 :=  'この画面より結果リストを出力して下さい。'
    ?KEKA3 :=  '（ログリストを採取しＮＡＶへ連絡）'
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