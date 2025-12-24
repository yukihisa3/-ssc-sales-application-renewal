/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    種子販売実績システム                 *  ./
/. *   JOB-ID      :    REPLJ010                             *  ./
/. *   JOB-NAME    :    実績連携ファイルレプリカ送信処理　   *  ./
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
    VAR       ?PGMID  ,STRING*8,VALUE-'REPLJ010'
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

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '実績連携データ抽出処理→送信'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP010:
    ?MSGX :=  '*** (REPLJ010) '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##サーバー側　更新結果Ｆ削除##./
STEP011:

    ?STEP :=  'STEP011 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-JISSVRF.TOKWLIB
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【サーバー側　更新結果Ｆ削除】'
         GOTO ABEND
    END

/.##サーバー側　更新結果取得##./
STEP012:

    ?STEP :=  'STEP012 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
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
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
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
          GOTO   STEP020
    END


/.##レプリカ用　実績連携Ｆ削除##./
STEP014:

    ?STEP :=  'STEP014 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-PCJISWF.TOKWLIB
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【レプリカ用　実績連携Ｆ削除】'
         GOTO ABEND
    END

/.##実績連携データ抽出##./
STEP020:

    ?STEP :=  'STEP020 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    ?SCNT := '000000000'
    OVRF FILE-PCJISSL3,TOFILE-PCJISSL3.TOKKLIB
    OVRF FILE-PCJISWL1,TOFILE-PCJISWL1.TOKWLIB
    CALL SJS0231B.TOKELIBO,PARA-(?SCNT)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【送信実績データカウント】'
         GOTO ABEND
    END

/.##送信実績データカウント##./
STEP025:

    ?STEP :=  'STEP025 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-PCJISWL1,TOFILE-PCJISWL1.TOKWLIB
    CALL SJS0235B.TOKELIBO,PARA-(?SCNT)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【実績連携データ抽出】'
         GOTO ABEND
    END


/.##制御ファイル作成##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=   '*** (REPLJ010) ' && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-JISPCCL1,TOFILE-JISPCCL1.TOKWLIB
    CALL SPD9990B.TOKELIBO,PARA-(?SDATE,?STIME)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【制御ファイル作成】'
         GOTO ABEND
    END
    ?MSGX :=   '## DATE:' && ?SDATE && ' TIME:' && ?STIME
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##多階層へ制御Ｆ出力##./
STEP040:

    ?STEP :=  'STEP040 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CNVDF FILE-JISPCCF.TOKWLIB,
    PATH-'/SAKATA/DATA/PRKANRI/JISPRFIL.CSV',MODE-@EXP,ADD-@NO,
    ACTCHK-@NO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【実績制御Ｆ→多階層へ出力（実行前）】'
         GOTO ABEND
    END

/.##実績連携データ送信（レプリケーション）##./
STEP050:

    ?STEP :=  'STEP050 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.  SYNCREPL GRP-'DENPYO-FILE-GROUP2',MODE-@ALL,OUTTYPE-@CRT,
             WAIT-@YES
./  IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【実績連携データ送信】'
              GOTO ABEND END

/.##制御ファイル更新（正常終了）##./
STEP060:

    ?STEP :=  'STEP060 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-JISPCCL1,TOFILE-JISPCCL1.TOKWLIB
    CALL SPD9991B.TOKELIBO,PARA-(?SDATE,?STIME,?SCNT)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【制御ファイル開始出力】'
         GOTO ABEND
    END

/.##連携データ抽出##./
STEP070:

    ?STEP :=  'STEP070 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CALL SJS8000B.TOKSOLIB
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【連携データ抽出（ＴＸＴ）】'
         GOTO ABEND
    END

/.##連携データ出力##./
STEP071:

    ?STEP :=  'STEP071 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-PCJISWWK.TOKDTLIB,PARA-PCJISWWK,UNIT-1
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【連携データ出力（ＦＩＭＰＯＲＴ）】'
         GOTO ABEND
    END

/.##多階層へ制御Ｆ出力##./
STEP080:

    ?STEP :=  'STEP080 '
    ?MSGX :=  '*** (REPLJ010) ' && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CNVDF FILE-JISPCCF.TOKWLIB,
    PATH-'/SAKATA/DATA/PRKANRI/JISPRFIL.CSV',MODE-@EXP,ADD-@NO,
    ACTCHK-@NO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【実績制御Ｆ→多階層へ出力（実行後）】'
         GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
    /.##正常終了　更新##./
    ?PGKEKA := '1'
    SNDMSG MSG-'＃＃実績連携（レプリカ）送信　正常終了＃＃'
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
    SNDMSG MSG-'＃＃実績連携（レプリカ）送信　異常終了＃＃'
           ,TO-XCTL,SLOG-@YES,JLOG-@YES
    ?MSGX :=   '＃＃異常場所：' && ?KEKA4 && ' ＃＃'
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC
