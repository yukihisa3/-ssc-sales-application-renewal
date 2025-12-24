# PMT991

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PMT991.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    マスタメンテ                         *  ./
/. *   JOB-ID      :    PMT991                               *  ./
/. *   JOB-NAME    :    商品変換テーブル一括登録処理         *  ./
/. *               :    2005/11/08 - 11/XX                   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PMT991  '
    VAR       ?STEP     ,STRING*8
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL    TOKELIB/TOKFLIB/TOKELIBO

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '商品変換テーブル一括登録'

    ?OPR1  :=  '　処理名称：商品変換テーブル一括登録　　　　　　　'
    ?OPR2  :=  '　＜注意＞データ退避を行ないます。他端末未使用必須'
    ?OPR3  :=  '　商品変換テーブル一括入力にて作成したデータの　　'
    ?OPR4  :=  '　商品変換テーブルへの登録を行ないます。　　　　　'
    ?OPR5  :=  '　更新完了後、正しく登録されているか確認！！'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##データバックアップ##./
DATABAK:
    ?STEP :=   'DATABAK'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  SAVFILE FILE-HSHOTBL.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【商品変換データバックアップ】'
              GOTO ABEND END
 ./
/.##一括登録##./
ZMT991B:
    ?STEP :=   'ZMT991B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTBLM1,TOFILE-SHTBLM1.TOKWLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-ERRF,TOFILE-ZMT991ER.TOKWLIB
    CALL      PGM-ZMT991B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【商品変換登録更新】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '商品変換テーブルの一括登録が終了しまし'
    ?KEKA2 :=  'た。登録内容を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '商品変換テーブル一括登録が異常終了しま'
    ?KEKA2 :=  'した。ログリストを採取してＮＡＶまで連'
    ?KEKA3 :=  '絡をお願い致します。'
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
