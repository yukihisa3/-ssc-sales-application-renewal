# PNDL0010

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PNDL0010.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ＮＡＶＳ基幹システム　　　　          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　                     *  ./
/. *   JOB-ID      :    PNDL0010                             *  ./
/. *   JOB-NAME    :    Ｄ３６５連携データ削除               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PNDL0010'
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

    ?PGNM  :=  'Ｄ３６５連携データ削除（月次）'

/.ﾗｲﾌﾞﾗﾘﾘｽﾄ登録./

    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKDTLIB

/.  出荷検品処理　実行確認メッセージ                             ./
STEP00:
    ?OPR1  :=  '　【Ｄ３６５連携データ削除（月次）】'
    ?OPR2  :=  ''
    ?OPR3  :=  '　　Ｄ３６５連携データの削除を行ないます。'
    ?OPR4  :=  '　　確認して下さい。'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##Ｄ３６５連携データバックアップ##./
PSAVFILE:

    ?STEP :=   'PSAVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    SNDMSG MSG-'#＃Ｄ３６５連携Ｆ　バックアップ中＃＃',
           TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    SAVFILE FILE-URIKEJF.D365DLIB,TODEV-@NONE,MODE-@USED,
            TOPATH-'/HGNAS/PNDL0010',REP-@YES,COMPRESS-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'Ｄ３６５連携Ｆ　バックアップ'
              GOTO ABEND
    END

/.##Ｄ３６５連携データ削除##./
NDL0010B:

    ?STEP :=   'NDL0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    SNDMSG MSG-'#＃Ｄ３６５連携ＤＴ　削除中＃',
           TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-URIKEJL4,TOFILE-URIKEJL4.D365DLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF FILE-URIKEJBB,TOFILE-URIKEJBB.D365WORK
    CALL  NDL0010B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'Ｄ３６５連携ＤＴ　削除'
              GOTO ABEND
    END

/.##バックアップＦ退避##./
URIKEJBB:

    ?STEP :=   'URIKEJBB'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    SNDMSG MSG-'＃＃バックアップＦ退避＃＃',
           TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    SAVFILE FILE-URIKEJBB.D365WORK,TODEV-@NONE,MODE-@USED,
            TOPATH-'/HGNAS/PNDL0010',REP-@YES,COMPRESS-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'Ｄ３６５連携Ｆ　バックアップ'
              GOTO ABEND
    END

RTN:

    ?KEKA1 :=  'Ｄ３６５連携データ削除（月次）が正常'
    ?KEKA2 :=  '終了しました。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  'Ｄ３６５連携データ削除（月次）が異常終了'
    ?KEKA2 :=  'しました。処理結果リストを出力して下さい'
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

```
