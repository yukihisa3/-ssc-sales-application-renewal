# PSY3719V

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY3719V.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3719V                             *  ./
/. *   JOB-NAME    :    ナフコ新ＥＤＩ　受領アンマッチリスト *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY3719I'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
    VAR ?SDATES   ,STRING*8,VALUE-'        ' /. 開始出荷日 ./
    VAR ?SDATEE   ,STRING*8,VALUE-'        ' /. 終了出荷日 ./
    /.↓20120530./
    VAR ?JDATES   ,STRING*8,VALUE-'        ' /. 開始受領日 ./
    VAR ?JDATEE   ,STRING*8,VALUE-'        ' /. 終了受領日 ./
    /.↑20120530./
    VAR ?KBN      ,STRING*1,VALUE-'1'        /. 出力制御区分 ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  'ナフコ受領アンマッチリスト発行指示'

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

/.## 受領アンマッチリスト発行指示 ##./
SSY3719I:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL PGM-SSY3719I.TOKELIBO
     /.↓20120530./
       /.  ,PARA-(?SDATES,?SDATEE) ./
           ,PARA-(?SDATES,?SDATEE,?JDATES,?JDATEE)
     /.↑20120530./

    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO  RTN
        ELSE
           ?KEKA4 := '受領アンマッチリスト発行指示'
           GOTO  ABEND
        END
    END

    ?MSGX := '## 納品日:' && ?SDATES && ' - ' && ?SDATEE && ' ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '## 受領日:' && ?JDATES && ' - ' && ?JDATEE && ' ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## 抽出Ｆ初期化 ##./
CLRFILE1:

    ?STEP :=   'CLRFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-NFJISKF.TOKKLIB
    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := '出荷実績データ初期化'
         GOTO ABEND
    END

/.## 抽出Ｆ初期化 ##./
CLRFILE2:

    ?STEP :=   'CLRFILE2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-NFJUJKF.TOKKLIB
    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := '受領実績データ初期化'
         GOTO ABEND
    END

/.## 出荷実績データ抽出 ##./
SSY3710B:

    ?STEP :=   'SSY3710B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.↓20120530./
  /.OVRF FILE-NFJOHOLF,TOFILE-NFJOHOLF.TOKKLIB./
  /.OVRF FILE-SHTDENLK,TOFILE-SHTDENLN.TOKFLIB./
    OVRF FILE-SHTDENLK,TOFILE-SHTDENLT.TOKKLIB
  /.↑20120530./
    OVRF FILE-NFJISKL1,TOFILE-NFJISKL1.TOKKLIB
    CALL SSY3710B.TOKELIBO,PARA-(?SDATES,?SDATEE)

    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := '出荷実績データ抽出'
         GOTO ABEND
    END

/.## 受領実績データ抽出 ##./
SSY3716B:

    ?STEP :=   'SSY3716B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-NFJYURL6,TOFILE-NFJYURL6.TOKKLIB
    OVRF FILE-NFJUJKL1,TOFILE-NFJUJKL1.TOKKLIB
 /.↓20120530./
 /. CALL SSY3716B.TOKELIBO,PARA-(?SDATES,?SDATEE) ./
    CALL SSY3716B.TOKELIBO,PARA-(?JDATES,?JDATEE)
 /.↑20120530./

    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := '受領実績データ抽出'
         GOTO ABEND
    END

/.## アンマッチデータ作成 ##./
SSY3717B:

    ?STEP :=   'SSY3717B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-NFJISKL2,TOFILE-NFJISKL2.TOKKLIB
    OVRF FILE-NFJUJKL2,TOFILE-NFJUJKL2.TOKKLIB
    CALL SSY3717B.TOKELIBO,PARA-(?KBN)

    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := 'アンマッチデータ作成'
         GOTO ABEND
    END

/.## アンマッチＣＳＶ出力 ##./
SSY3714V:

    ?STEP :=   'SSY3714V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-NFNUNML1,TOFILE-NFNUNML1.TOKKLIB
/.##2016/10/05 NAV ST TOKDLIBへ変更##./
/.  OVRF FILE-NFSHOMS1,TOFILE-NFSHOMS1.TOKKLIB./
    OVRF FILE-NFSHOMS1,TOFILE-NFSHOMS1.TOKDLIB
    OVRF FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF FILE-SAKUBAL1,TOFILE-SAKUBAL1.TOKKLIB
    OVRF FILE-NFNUNMWK,TOFILE-NFNUNMWK.TOKKLIB
    CALL SSY3714V.TOKELIBO

    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := 'アンマッチＣＳＶ出力'
         GOTO ABEND
    END

/.## ＰＣへ転送 ##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-NFNUNMWK.TOKKLIB,PARA-NFNUNMWK,UNIT-1

    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := 'ＰＣ転送処理'
         GOTO ABEND
    END

/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB
    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '受領アンマッチリスト発行にて異常発生！！'
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
