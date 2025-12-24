# PNSY9000

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PNSY9000.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭ仕入先統合                     *  ./
/. *   JOB-ID      :    PNSY9000                             *  ./
/. *   JOB-NAME    :    出荷検品月次更新処理                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PNSY9000'
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

    ?PGNM  :=  'ＤＣＭ出荷検品－月次更新処理'

/.ﾗｲﾌﾞﾗﾘﾘｽﾄ登録./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB

/.  出荷検品処理　実行確認メッセージ                             ./
STEP00:
    ?OPR1  :=  '　【ＤＣＭ出荷検品処理　月次更新】'
    ?OPR2  :=  ''
    ?OPR3  :=  '　　ＤＣＭ出荷検品処理の月次更新を行ないます。'
    ?OPR4  :=  '　　確認して下さい。'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##基本情報ＤＴ削除##./
NSY9001B:

    ?STEP :=   'NSY9001B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃基本ＤＴ　削除中＃',TO-XCTL

    OVRF FILE-DNJOHOL1,TOFILE-DNJOHOL1.TOKDTLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  NSY9001B.TOKSOLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷ＤＴ　削除処理】'
              GOTO ABEND END

/.##出荷情報ＤＴ削除##./
NSY9002B:

    ?STEP :=   'NSY9002B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃出荷情報　削除中＃',TO-XCTL

    OVRF FILE-DNSYUKL1,TOFILE-DNSYUKL1.TOKDTLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
/.  CALL  NSY9002B.TOKSOLIB
./  IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　削除処理】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  'ＤＣＭ出荷月次更新が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  'ＤＣＭ出荷月次更新が異常終了しました。'
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

```
