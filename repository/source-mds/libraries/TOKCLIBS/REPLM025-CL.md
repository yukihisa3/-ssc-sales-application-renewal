# REPLM025

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/REPLM025.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    種子販売実績システム                 *  ./
/. *   JOB-ID      :    REPLM020                             *  ./
/. *   JOB-NAME    :    マスタレプリカ送信処理（一括）　　   *  ./
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
    VAR       ?PGMID  ,STRING*8,VALUE-'REPLM020'
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

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'マスタレプリカ処理【一括】'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP010:
    ?MSGX :=  '*** (REPLM020) '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##制御ファイル作成##./
STEP020:

    ?STEP :=   'STEP020 '
    ?MSGX :=  '*** (REPLM020) ' && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-JISPCCL1,TOFILE-MSTPCCL1.TOKWLIB
    CALL SPD9990B.TOKELIBO,PARA-(?SDATE,?STIME)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【制御ファイル作成】'
         GOTO ABEND
    END
    ?MSGX :=  '## DATE:' && ?SDATE && ' TIME:' && ?STIME
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##多階層へ制御Ｆ出力##./
STEP030:

    ?STEP :=  'STEP030 '
    ?MSGX :=  '*** (REPLM020) ' && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CNVDF FILE-MSTPCCF.TOKWLIB,
    PATH-'/ﾍｱﾁｱﾎｱ/ｴｱﾎｱ/PRKANRI/MSTPRFIL.CSV',MODE-@EXP,ADD-@NO,
    ACTCHK-@NO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【実績制御Ｆ→多階層へ出力（実行前）】'
         GOTO ABEND
    END

/.##マスタ送信（レプリケーション）##./
STEP040:

    ?STEP :=  'STEP040 '
    ?MSGX :=  '*** (REPLM020) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    SYNCREPL GRP-'KIHON-MASTER-GROUP',MODE-@ALL,WAIT-@YES
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【マスタデータ送信】'
              GOTO ABEND END

/.##制御ファイル更新（正常終了）##./
STEP050:

    ?STEP :=  'STEP050 '
    ?MSGX :=  '*** (REPLM020) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-JISPCCL1,TOFILE-MSTPCCL1.TOKWLIB
    CALL SPD9991B.TOKELIBO,PARA-(?SDATE,?STIME,?SCNT)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【制御ファイル開始出力】'
         GOTO ABEND
    END

/.##多階層へ制御Ｆ出力##./
STEP060:

    ?STEP :=  'STEP060 '
    ?MSGX :=  '*** (REPLM020) ' && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CNVDF FILE-MSTPCCF.TOKWLIB,
    PATH-'/ﾍｱﾁｱﾎｱ/ｴｱﾎｱ/PRKANRI/MSTPRFIL.CSV',MODE-@EXP,ADD-@NO,
    ACTCHK-@NO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【マスタ制御Ｆ→多階層へ出力（実行後）】'
         GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
    /.##正常終了　更新##./
    ?PGKEKA := '1'
    SNDMSG MSG-'＃＃マスタ（レプリカ）送信　正常終了＃＃'
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
    SNDMSG MSG-'＃＃マスタ（レプリカ）送信　異常終了＃＃'
           ,TO-XCTL,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '＃＃異常場所：' && ?KEKA4 && ' ＃＃'
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

```
