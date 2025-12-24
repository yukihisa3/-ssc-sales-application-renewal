/.***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    種子販売実績システム                 *  ./
/. *   JOB-ID      :    REPLJ020                             *  ./
/. *   JOB-NAME    :    ＰＣ側実績更新完了チェック処理　　   *  ./
/. ***********************************************************  ./
/.  PGM (P1-?PGKEKA)    ./
    PGM
/.###ﾊﾟﾗﾒﾀ定義####./
    VAR       ?PGKEKA ,STRING*1,VALUE-' '     /.PG実行結果./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'REPLJ020'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?SDATE  ,STRING*8,VALUE-'        '
    VAR       ?STIME  ,STRING*6,VALUE-'      '
    VAR       ?SCNT   ,STRING*9,VALUE-'000000000'
    VAR       ?PGNM   ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                    /.      2    ./
    VAR       ?KEKA2  ,STRING*40                    /.      3    ./
    VAR       ?KEKA3  ,STRING*40                    /.      4    ./
    VAR       ?KEKA4  ,STRING*40                    /.      5    ./
    VAR       ?WKKEKA ,STRING*1
    VAR       ?KEKA   ,STRING*2,VALUE-'  '
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM     :=  'ＰＣ側実績更新完了チェック処理'
    ?SYORINM1 :=  'ＰＣ側実績更新完了チェック'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP010:
    ?MSGX :=  '*** (REPLJ020) '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##サーバー側　更新結果Ｆ削除##./
STEP011:

    ?STEP :=  'STEP011 '
    ?MSGX :=  '*** (REPLJ020) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-JISSVRF.TOKWLIB
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【サーバー側　更新結果Ｆ削除】'
         GOTO ABEND
    END

/.##サーバー側　更新結果取得##./
STEP012:

    ?STEP :=  'STEP012 '
    ?MSGX :=  '*** (REPLJ020) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CNVDF FILE-JISSVRF.TOKWLIB,
    PATH-'/SAKATA/DATA/PRKANRI/JISPRSVR.CSV',MODE-@IMP,ADD-@NO,
    ACTCHK-@NO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【サーバー側　更新結果取得】'
         GOTO ABEND
    END

/.##サーバー側　更新結果判定##./
STEP013:

    ?STEP :=  'STEP013 '
    ?MSGX :=  '*** (REPLJ020) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-JISPCCL2,TOFILE-JISPCCL2.TOKWLIB
    OVRF FILE-JISSVRL2,TOFILE-JISSVRL2.TOKWLIB
    CALL SPD9992B.TOKELIBO,PARA-(?KEKA)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【サーバー側　更新結果判定】'
         GOTO ABEND
    END
/.##　結果判定　##./
    IF   ?KEKA  =  'NG'  THEN
    DEFLIBL TOKELIBO/TOKELIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'ＰＣ側の実績更新処理は、チェック異常終了です。'
    ?SYORINM3 := 'ＰＣ側の実績内容を確認して下さい。　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    END
    IF   ?KEKA  =  'OK'  THEN
    DEFLIBL TOKELIBO/TOKELIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'ＰＣ側の実績更新処理は、正常終了です。　　　　　'
    ?SYORINM3 := 'ＰＣ側で実績内容を確認して下さい。　　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
    /.##正常終了　更新##./
    ?PGKEKA := '1'
    SNDMSG MSG-'＃＃実績連携（レプリカ）チェック正常終了＃＃'
           ,TO-XCTL,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
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
               SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
    END
    /.##異常終了　更新##./
    ?PGKEKA := '2'
    SNDMSG MSG-'＃＃実績連携（レプリカ）チェック異常終了＃＃'
           ,TO-XCTL,SLOG-@YES,JLOG-@YES
    ?MSGX :=   '＃＃異常場所：' && ?KEKA4 && ' ＃＃'
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC
