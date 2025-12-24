/. ***********************************************************  ./
/. *   サカタのタネ　基幹システム（本社システム）            *  ./
/. *   SYSTEM-NAME :    グッデイＥＤＩ　　　　　　　　　　   *  ./
/. *   JOB-NAME    :    基本／出荷情報データ削除             *  ./
/. *   JOB-ID      :    PSY57980                             *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY57980'
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

    ?PGNM  :=  'グッデイ基本／出荷情報データ削除'

/.ﾗｲﾌﾞﾗﾘﾘｽﾄ登録./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.  実行確認メッセージ                             ./
STEP00:
    ?OPR1  :=  '【グッデイ基本／出荷情報データ削除】'
    ?OPR2  :=  ''
  /.?OPR3  :=  'グッデイ基本／出荷情報データの'      ./
    ?OPR3  :=  '基本情報／出荷情報データの削除を行います'
  /.?OPR4  :=  '削除を行います。確認してください。'  ./
    ?OPR4  :=  '確認してください。'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##グッデイ基本情報ファイル削除##./
SSY5798B:

    ?STEP :=   'SSY5798B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃基本情報ファイル削除　削除中＃',TO-XCTL

  /.OVRF FILE-GDJOHOL1,TOFILE-GDJOHOL1.TOKKLIB./
  /.OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB  ./
    CALL  SSY5798B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【グッデイ基本情報ファイル削除】'
              GOTO ABEND
    END

/.##グッデイ出荷情報ファイル削除##./
SSY5799B:

    ?STEP :=   'SSY5799B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃基本情報　削除中＃',TO-XCTL

  /.OVRF FILE-GDSYUKL1,TOFILE-GDSYUKL1.TOKKLIB./
  /.OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB  ./
    CALL  SSY5799B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【グッデイ出荷情報ファイル削除】'
              GOTO ABEND
    END

RTN:

    ?KEKA1 :=  'グッデイ基本／出荷情報データ削除処理が'
    ?KEKA2 :=  '正常終了しました。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-?PGMEC

ABEND:

    ?KEKA1 :=  'データ削除処理が異常終了しました。'
    ?KEKA2 :=  'この画面より結果リストを出力して下さい。'
    ?KEKA3 :=  '（ログリストを採取しＮＡＶへ連絡）'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
  /.?PGMEC    :=    @PGMEC ./
  /.?PGMEM    :=    @PGMEM ./
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

    RETURN    PGMEC-?PGMEC
