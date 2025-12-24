# PNYUKCHK

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNYUKCHK.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫未入庫数再計算処理    　　　     *  ./
/. *   JOB-ID      :    PNYUKCHK                             *  ./
/. *   JOB-NAME    :    １．在庫マスタバックアップ　　       *  ./
/. *               :    ２．在庫マスタ未入庫数初期化　　　   *  ./
/. *               :    ３．未入庫数再計算　　　　　　　　   *  ./
/. *               :    ４．未入庫数再計算（ＡＣＯＳ振替分） *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PNYUKCHK'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?SYSDATE  ,STRING*13                    /.ｼｽﾃﾑ日付   ./
    VAR ?YOUBI    ,STRING*1                     /.曜日       ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL LIBL-TOKELIB/TOKFLIB/TOKSOLIB/TOKDTLIB/TOKDLIB

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  '在庫未入庫数再計算処理'

/.##ｼｽﾃﾑ日付取得##./
    ?SYSDATE  :=    @SCDATED
/.##曜日取得##./
    ?YOUBI    :=    %SBSTR(?SYSDATE,13,1)

/.##在庫マスタバックアップ##./

STEP010:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

   /.##月曜日定義##./
    IF     ?YOUBI  =  '1'  THEN
           ?MSGX :=  '【月曜日用－バックアップ】'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
           SETPF FILE-ZAMZAIF.TOKFLIB,
                 TOFILE-ZAMZAIF1.ZAIKOBK,ADD-@NO,ACTCHK-@NO
           IF   @PGMEC    ^=   0    THEN
                ?KEKA4 := '＜ﾊﾞｯｸｱｯﾌﾟ月曜日異常＞'
                GOTO ABEND
           END
    END
   /.##火曜日定義##./
    IF     ?YOUBI  =  '2'  THEN
           ?MSGX :=  '【火曜日用－バックアップ】'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
           SETPF FILE-ZAMZAIF.TOKFLIB,
                 TOFILE-ZAMZAIF2.ZAIKOBK,ADD-@NO,ACTCHK-@NO
           IF   @PGMEC    ^=   0    THEN
                ?KEKA4 := '＜ﾊﾞｯｸｱｯﾌﾟ火曜日異常＞'
                GOTO ABEND
           END
    END
   /.##水曜日定義##./
    IF     ?YOUBI  =  '3'  THEN
           ?MSGX :=  '【水曜日用－バックアップ】'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
           SETPF FILE-ZAMZAIF.TOKFLIB,
                 TOFILE-ZAMZAIF3.ZAIKOBK,ADD-@NO,ACTCHK-@NO
           IF   @PGMEC    ^=   0    THEN
                ?KEKA4 := '＜ﾊﾞｯｸｱｯﾌﾟ水曜日異常＞'
                GOTO ABEND
           END
    END
   /.##木曜日定義##./
    IF     ?YOUBI  =  '4'  THEN
           ?MSGX :=  '【木曜日用－バックアップ】'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
           SETPF FILE-ZAMZAIF.TOKFLIB,
                 TOFILE-ZAMZAIF4.ZAIKOBK,ADD-@NO,ACTCHK-@NO
           IF   @PGMEC    ^=   0    THEN
                ?KEKA4 := '＜ﾊﾞｯｸｱｯﾌﾟ木曜日異常＞'
                GOTO ABEND
           END
    END
   /.##金曜日定義##./
    IF     ?YOUBI  =  '5'  THEN
           ?MSGX :=  '【金曜日用－バックアップ】'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
           SETPF FILE-ZAMZAIF.TOKFLIB,
                 TOFILE-ZAMZAIF5.ZAIKOBK,ADD-@NO,ACTCHK-@NO
           IF   @PGMEC    ^=   0    THEN
                ?KEKA4 := '＜ﾊﾞｯｸｱｯﾌﾟ金曜日異常＞'
                GOTO ABEND
           END
    END

/.## 未入庫数初期化処理 ##./
STEP020:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-ZAMZAIF,TOFILE-ZAMZAIF.TOKFLIB
    CALL SZA9999B.TOKELIBO
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '＜在庫Ｍ未入庫数初期化＞'
           GOTO  ABEND
    END

/.## 未入庫数再計算 ##./
STEP030:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-HACHEDL1,TOFILE-HACHEDL1.TOKFLIB
    OVRF FILE-HACMEIL1,TOFILE-HACMEIL1.TOKFLIB
    OVRF FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    CALL SZA9998B.TOKELIBO
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '＜未入庫数再計算＞'
           GOTO  ABEND
    END

/.## 未入庫数再計算（ＡＣＯＳ振替分） ##./
STEP040:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL SZA9997B.TOKSOLIB
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '＜未入庫数再計算（ＡＣＯＳ振替分）＞'
           GOTO  ABEND
    END

/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:
    DEFLIBL TOKELIB/TOKELIBO
/.##正常セット##./
    ?KEKA1 :=  '＜在庫Ｍ　未入庫数　再計算処理＞'
    ?KEKA2 :=  ''
    ?KEKA3 :=  '再計算処理が正常終了しました。'
    ?KEKA4 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
    DEFLIBL TOKELIB/TOKELIBO
/.##異常セット##./
    ?KEKA1 :=  '＜在庫Ｍ　未入庫数　再計算処理＞'
    ?KEKA2 :=  '再計算処理が異常終了しました。'
    ?KEKA3 :=  '早急にＮＡＶまで連絡して下さい。'
    ?KEKA4 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO ?MSGX :=   ?MSG(?I)
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC

```
