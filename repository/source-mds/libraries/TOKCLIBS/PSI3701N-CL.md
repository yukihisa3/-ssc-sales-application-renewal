# PSI3701N

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSI3701N.CL`

## ソースコード

```jcl
/. *********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　     *  ./
/. *   SYSTEM-NAME :    ナフコ新ＥＤＩ　支払照合　　　　   *  ./
/. *   JOB-ID      :    PSI3701N                           *  ./
/. *   JOB-NAME    :    ＡＣＯＳデータ取込　　　　　　    *  ./
/. *********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSI3701N'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?HIDUKE ,STRING*8,VALUE-'        '  /.バッチ日付./
    VAR       ?JIKAN  ,STRING*4,VALUE-'    '      /.バッチ時間./
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
    VAR       ?PGCHK1 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK2 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK3 ,STRING*1,VALUE-' '         /.      5    ./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ナフコＡＣＯＳデータ取込'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ナフコＡＣＯＳデータ取込】'
    ?OPR2  :=  'ＡＣＯＳディスカバラーより抽出したデータを'
    ?OPR3  :=  '編集後、指定のフォルダーにセットしましたか？'
    ?OPR4  :=  ''
    ?OPR5  :=  '確認し問題ない場合は、実行して下さい。'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##データ取込##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃　データ取込　　開始　＃＃',TO-XCTL

    CNVDF FILE-NAFUKOSN.TOKWLIB,PATH-'/HGNAS/NAFUKOSN.CSV',
          MODE-@IMP,ADD-@NO,ACTCHK-@NO,EXTCNV-@YES
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【データ取込】'
              GOTO ABEND END
    SNDMSG MSG-'＃＃　データ取込　　終了　＃＃',TO-XCTL

/.##データ初期化##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃　編集前初期化　開始　＃＃',TO-XCTL
    CLRFILE SETGK90.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【編集前初期化】'
              GOTO ABEND END
    SNDMSG MSG-'＃＃　編集前初期化　終了　＃＃',TO-XCTL

/.##データ編集##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃　データ編集　　開始　＃＃',TO-XCTL
    OVRF FILE-NAFUKOSL,TOFILE-NAFUKOSL.TOKWLIB
    OVRF FILE-SETGK903,TOFILE-SETGK903.TOKKLIB
    CALL SSI3706N.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【データ編集】'
              GOTO ABEND END
    SNDMSG MSG-'＃＃　データ編集　　終了　＃＃',TO-XCTL

/.##プログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコＡＣＯＳデータ取込が正常終了しました　'
    ?KEKA2 :=  '支払明細書、不照合リストを出力して下さい'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコＡＣＯＳデータ取込が異常終了しました'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
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

```
