# REPLT010

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/REPLT010.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    種子販売実績システム                 *  ./
/. *   JOB-ID      :    REPLT010                             *  ./
/. *   JOB-NAME    :    手書データ連携　レプリカ受信処理　   *  ./
/. ***********************************************************  ./
    PGM (P1-?PGKEKA)
/.  PGM                 ./
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA      ?PGKEKA ,STRING*1,OUT,VALUE-' '     /.PG実行結果./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'REPLT010'
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
    VAR       ?KENSU  ,STRING*9,VALUE-'         '

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '手書連携データ抽出処理→受信'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP010:
    ?MSGX :=  '*** (REPLT010) '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##サーバー側　更新結果Ｆ削除##./
STEP011:

    ?STEP :=  'STEP011 '
    ?MSGX :=  '*** (REPLT010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-TEGSVRF.TOKWLIB
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【サーバー側　更新結果Ｆ削除】'
         GOTO ABEND
    END

/.##サーバー側　更新結果取得##./
STEP012:

    ?STEP :=  'STEP012 '
    ?MSGX :=  '*** (REPLT010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CNVDF FILE-TEGSVRF.TOKWLIB,
    PATH-'/ﾍｱﾁｱﾎｱ/ｴｱﾎｱ/PRKANRI/TEGPRSVR.CSV',MODE-@IMP,ADD-@NO,
    ACTCHK-@NO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【サーバー側　更新結果取得】'
         GOTO ABEND
    END

/.##サーバー側　更新結果判定##./
STEP013:

    ?STEP :=  'STEP013 '
    ?MSGX :=  '*** (REPLT010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-JISSVRL2,TOFILE-TEGSVRL2.TOKWLIB
    CALL SPD9994B.TOKELIBO,PARA-(?KEKA,?KENSU,?SDATE,?STIME)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【サーバー側　更新結果判定】'
         GOTO ABEND
    END
/.##　結果判定　##./
    IF   ?KEKA  =  'NG'  THEN
          ?MSGX :=  '#ＰＣ側　制御Ｆの異常です#'
          SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
          GOTO   ABEND
    END
/.##　結果判定　##./
    IF   ?KENSU  =  '000000000'  THEN
          ?MSGX :=  '#ＰＣ側の件数が上がっていません#'
          SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
          GOTO   ABEND
    END


/.##制御ファイル作成##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=   '*** (REPLT010) ' && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-JISPCCL1,TOFILE-TEGPCCL1.TOKWLIB
    CALL SPD9995B.TOKELIBO,PARA-(?SDATE,?STIME,?KENSU)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【制御ファイル作成】'
         GOTO ABEND
    END
    ?MSGX :=   '## DATE:' && ?SDATE && ' TIME:' && ?STIME
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##多階層へ制御Ｆ出力##./
STEP040:

    ?STEP :=  'STEP040 '
    ?MSGX :=  '*** (REPLT010) ' && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CNVDF FILE-TEGPCCF.TOKWLIB,
    PATH-'/ﾍｱﾁｱﾎｱ/ｴｱﾎｱ/PRKANRI/TEGPRFIL.CSV',MODE-@EXP,ADD-@NO,
    ACTCHK-@NO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【手書制御Ｆ→多階層へ出力（実行前）】'
         GOTO ABEND
    END

/.##実績連携データ送信（レプリケーション）##./
STEP050:

    ?STEP :=  'STEP050 '
    ?MSGX :=  '*** (REPLT010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    SYNCREPL GRP-'TEGAKI-DENPYO-REPLICA',MODE-@ALL,OUTTYPE-@CRT,
             WAIT-@YES
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【実績連携データ送信】'
              GOTO ABEND END

/.##制御ファイル更新（正常終了）##./
STEP060:

    ?STEP :=  'STEP060 '
    ?MSGX :=  '*** (REPLT010) ' && ?STEP && '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF FILE-JISPCCL1,TOFILE-TEGPCCL1.TOKWLIB
    CALL SPD9991B.TOKELIBO,PARA-(?SDATE,?STIME,?KENSU)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【制御ファイル開始出力】'
         GOTO ABEND
    END

/.##多階層へ制御Ｆ出力##./
STEP080:

    ?STEP :=  'STEP080 '
    ?MSGX :=  '*** (REPLT010) ' && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    CNVDF FILE-TEGPCCF.TOKWLIB,
    PATH-'/ﾍｱﾁｱﾎｱ/ｴｱﾎｱ/PRKANRI/TEGPRFIL.CSV',MODE-@EXP,ADD-@NO,
    ACTCHK-@NO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【手書制御Ｆ→多階層へ出力（実行後）】'
         GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
    /.##正常終了　更新##./
    ?PGKEKA := '1'
    SNDMSG MSG-'＃＃手書連携（レプリカ）送信　正常終了＃＃'
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
    SNDMSG MSG-'＃＃手書連携（レプリカ）送信　異常終了＃＃'
           ,TO-XCTL,SLOG-@YES,JLOG-@YES
    ?MSGX :=   '＃＃異常場所：' && ?KEKA4 && ' ＃＃'
    SNDMSG ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

```
