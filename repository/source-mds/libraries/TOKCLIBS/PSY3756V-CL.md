# PSY3756V

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY3756V.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3756V                             *  ./
/. *   JOB-NAME    :    出荷以依頼データ：ＣＳＶ出力　       *  ./
/. *               :                                        *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'SSY3756V'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1./
    VAR ?KEKA1    ,STRING*40                    /.      2./
    VAR ?KEKA2    ,STRING*40                    /.      3./
    VAR ?KEKA3    ,STRING*40                    /.      4./
    VAR ?KEKA4    ,STRING*40                    /.      5./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?TAB      ,STRING*1,VALUE-'2'        /. レコード種別 ./
    VAR ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                  /.      2    ./
    VAR ?OPR3     ,STRING*50                  /.      3    ./
    VAR ?OPR4     ,STRING*50                  /.      4    ./
    VAR ?OPR5     ,STRING*50                  /.      5    ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  '出荷依頼データＣＳＶ出力'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得 ##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ログインユーザー情報取得 ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
/.##確認画面##./

STEP010:

    ?OPR1  :=  '【ナフコ出荷依頼データＣＳＶ出力】　処理】'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  'この処理で、出荷依頼データがＣＳＶに出力　'
    ?OPR4  :=  'されます。　　　　　　　　　　　　　　　　'
    ?OPR5  :=  'ファイルの確認を行ってください。　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ＣＳＶファイルクリア##./
STEP030:
    CLRFILE  NFSYKDT.TOKKLIB

/.## 出荷依頼データＣＳＶ出力 ﾍｯﾀﾞﾃﾞｰﾀ ##./
SSY3756V:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?TAB     レコード種別     ./
    /.             ＨＤ：ヘッダ     ./
    ?TAB      :=   'HD'
    OVRF FILE-NFSYKDT,TOFILE-NFSYKDT.TOKKLIB
    CALL PGM-SSY3756V.TOKELIBO,PARA-(?TAB)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '出荷依頼データＣＳＶ出力ＨＤ'
           GOTO  ABEND
        END
    END

/.## 出荷依頼データＣＳＶ出力 ﾒｲｻｲﾃﾞｰﾀ ##./
SSY3756Z:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?TAB     レコード種別     ./
    /.             ＤＴ：明細　　　 ./
    ?TAB      :=   'DT'
    CALL PGM-SSY3756V.TOKELIBO,PARA-(?TAB)
    OVRF FILE-NFSYKDT,TOFILE-NFSYKDT.TOKKLIB
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '出荷依頼データＣＳＶ出力ＤＴ'
           GOTO  ABEND
        END
    END

/.##ファイル統合##./
STEP040:
/.  順編成ファイルの統合（ヘッダ、明細）を行う。 ./
/.  DEFLIBL NFSYKDTM.TOKKLIB ./

/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '出荷依頼データＣＳＶ出力成異常終了。　'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
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
        DO ?MSGX :=   ?MSG(?I)
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC

```
