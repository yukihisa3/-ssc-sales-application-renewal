# PKE0302N

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PKE0302N.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　出荷検品システム（本社システム）      *  ./
/. *   SYSTEM-NAME :    出荷検品システム（０１７３）　　　　 *  ./
/. *   JOB-ID      :    PKE0302N                             *  ./
/. *   JOB-NAME    :    ケーヨー送信ＤＴ作成処理／新ＥＤＩ用 *  ./
/. ***********************************************************  ./
    PGM

    VAR   ?WS    ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR   ?WKSTN ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR   ?PGMEC ,INTEGER
    VAR   ?PGMECX,STRING*11
    VAR   ?PGMEM ,STRING*99
    VAR   ?MSG   ,STRING*99(6)
    VAR   ?MSGX  ,STRING*99
    VAR   ?PGMID ,STRING*8,VALUE-'PKE0302N'
    VAR   ?STEP  ,STRING*8
    VAR   ?PGNM    ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?KEKA1   ,STRING*40                  /.      2    ./
    VAR   ?KEKA2   ,STRING*40                  /.      3    ./
    VAR   ?KEKA3   ,STRING*40                  /.      4    ./
    VAR   ?KEKA4   ,STRING*40                  /.      5    ./
    VAR   ?OPR1    ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?OPR2    ,STRING*50                  /.      2    ./
    VAR   ?OPR3    ,STRING*50                  /.      3    ./
    VAR   ?OPR4    ,STRING*50                  /.      4    ./
    VAR   ?OPR5    ,STRING*50                  /.      5    ./
    VAR   ?CHK     ,STRING*1,VALUE-' '         /.送信ﾁｪｯｸFｸﾘｱ./
    VAR   ?KENSU   ,STRING*7,VALUE-'0000000'   /.送信件数./
    VAR   ?KAKUNIN ,STRING*2,VALUE-'  '        /.送信件数ﾁｪｯｸ./
    VAR   ?SYORINM1,STRING*40
    VAR   ?SYORINM2,STRING*50
    VAR   ?SYORINM3,STRING*50
    VAR   ?SYORIKN1,STRING*7,VALUE-'0000000'
    VAR   ?SYORIKN2,STRING*7,VALUE-'0000000'
    VAR   ?SYORIKN3,STRING*7,VALUE-'0000000'
    VAR   ?SYORIKN4,STRING*7,VALUE-'0000000'
    VAR   ?SYORIKN5,STRING*7,VALUE-'0000000'

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '出荷検品－ケーヨー送信ＤＴ作成'
    ?SYORINM1 :=  'ケーヨー送信ＤＴ作成（０１７３）'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.## ｼｮﾘｶｲｼﾒｯｾｰｼﾞ ##./
    ?OPR1  :=  '（処理名称）ケーヨー送信データ作成処理'
    ?OPR2  :=  '　　出荷検品済計上データの作成を行ないます。'
    ?OPR3  :=  '　　確認して下さい。（０１７３）'
    ?OPR4  :=  '※処理開始に伴い、対象ＤＴのＢＫＵＰを行ないます。'
    ?OPR5  :=  '　ＢＫＵＰ用のＭＯをセットして下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
/.##MO=>BACKUP##./
LTBACKUP:

    ?STEP :=   'LTBACKUP'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SAVFILE FILE-RUISYUF.TOKKLIB/RUIKONF.TOKKLIB/
            SNDKENF.TOKKLIB/KENSOKF.TOKKLIB/
            SYJOHO62.TOKBLIB/
            SYLABH62.TOKBLIB/
            SYLABM62.TOKBLIB,
            TODEV-LTO,
            MODE-@USED
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【ＭＯ待避】'
              GOTO ABEND
    END

/.##送信ﾁｪｯｸﾌｧｲﾙ初期化##./
SKE0840B:

    ?STEP :=   'SKE0840B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KENCHKF,TOFILE-KENCHKF.TOKFLIB
    CALL      PGM-SKE0840B.TOKELIB,PARA-(?CHK)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【送信Ｆ初期化チェック】'
              GOTO ABEND
    END

    /.##送信ﾁｪｯｸﾌｧｲﾙ初期化##./
    IF        ?CHK  =  '1'   THEN
               ?MSGX :=  '【送信F初期化　開始】'
               SNDMSG    ?MSGX,TO-XCTL
               CLRFILE FILE-KENCHKF.TOKFLIB
               IF        @PGMEC    ^=   0   THEN
                         ?KEKA4  :=  '【送信チェックＦ初期化】'
                         GOTO ABEND
               END
               CLRFILE FILE-SNDKENDT.ONLBLIB
               IF        @PGMEC    ^=   0   THEN
                         ?KEKA4  :=  '【送信Ｆ初期化】'
                         GOTO ABEND
               END
    ELSE
               ?MSGX :=  '【送信F初期化　無し】'
               SNDMSG    ?MSGX,TO-XCTL
    END

/.##送信ＤＴ作成処理##./
SKE0320B:

    ?STEP :=   'SKE0320B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RUISYUL2,TOFILE-RUISYUL2.TOKKLIB
    OVRF      FILE-RUIKONL2,TOFILE-RUIKONL2.TOKKLIB
    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-KENSOKL1,TOFILE-KENSOKL1.TOKKLIB
    OVRF      FILE-SNDKENF,TOFILE-SNDKENF.TOKKLIB
    CALL      PGM-SKE0320B.TOKELIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【送信ＤＴ作成】'
              GOTO ABEND
    END

/.##SNDKENF=>SNDKENDT ﾍ COPY##./
SNDCOPY:

    ?STEP :=   'SNDCOPY '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-SNDKENF.TOKKLIB,TOFILE-SNDKENDT.ONLBLIB,
            BF-31
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【送信用ファイルヘＣＯＰＹ】'
              GOTO ABEND
    END

/.##SNDKENF=>BACKUP##./
SNDBAKUP:

    ?STEP :=   'SNDBAKUP'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SAVFILE FILE-SNDKENDT.ONLBLIB,TODEV-LTO,MODE-@USED
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【作成済データバックアップ】'
              GOTO ABEND
    END

/.##フバサミクレー送信キーファイル抽出##./
KEYDATA1:

    ?STEP :=   'KEYDATA1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SNDFINF,TOFILE-SNDFINDT.TOKKLIB
    OVRF      FILE-KENSOKF,TOFILE-KENSOKL2.TOKKLIB
    CALL      PGM-SKE0850B.TOKELIB,PARA-('63')
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【フバサミ送信キー抽出】'
              GOTO ABEND
    ELSE
              CNVFILE FILE-SNDFINDT.TOKKLIB,
                      TOFILE-SNDFIN63.TOKWLIB,BF-81
              IF   @PGMEC    ^=   0   THEN
                   ?KEKA4  :=  '【フバサミ送信キーＣＰ】'
                   GOTO ABEND
              END
    END

/.##片岡送信キーファイル抽出##./
KEYDATA2:

    ?STEP :=   'KEYDATA2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SNDFINF,TOFILE-SNDFINDT.TOKKLIB
    OVRF      FILE-KENSOKF,TOFILE-KENSOKL2.TOKKLIB
    CALL      PGM-SKE0850B.TOKELIB,PARA-('6A')
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【片岡送信キー抽出】'
              GOTO ABEND
    ELSE
              CNVFILE FILE-SNDFINDT.TOKKLIB,
                      TOFILE-SNDFIN6A.TOKWLIB,BF-81
              IF   @PGMEC    ^=   0   THEN
                   ?KEKA4  :=  '【片岡送信キーＣＰ】'
                   GOTO ABEND
              END
    END

/.##検品管理ファイル更新##./
SKE0860B:

/.  ?STEP :=   'SKE0860B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KENKANL2,TOFILE-KENKANL2.TOKKLIB
    CALL      PGM-SKE0860B.TOKELIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【検品管理Ｆ更新処理】'
              GOTO ABEND
    END
   ./
/.##送信件数チェック##./
SKE8020B:

    ?STEP :=   'SKE8020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SNDKENDT,TOFILE-SNDKENDT.ONLBLIB
    CALL      PGM-SKE8020B.TOKELIBO,PARA-(?KENSU,?KAKUNIN)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【送信件数カウント】'
              GOTO ABEND
    END

    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    ?SYORIKN1 := ?KENSU
    /.## 送信件数チェック ##./
    IF  ?KAKUNIN  =  'OK'
    THEN
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '今回の処理で送信するデータが作成されました。　　'
    ?SYORINM3 := '手動配信にて、ケーヨーへデータを送信して下さい。'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ELSE
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'ケーヨーに送信するデータはありません。　　　　　'
    ?SYORINM3 := '【送信データ無！！】　　　　　　　　　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    END

RTN: /.##ﾌﾟﾛｸﾞﾗﾑ正常時、終了ﾒｯｾｰｼﾞ##./

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
/.  ?KEKA3 :=  ''
    ?KEKA4 :=  '確認後、ケーヨーＤＴ送信開始！！'
./  IF  ?KAKUNIN = 'OK'
        THEN
          ?KEKA3 :=  '【送信データ　あり】'
          ?KEKA4 :=  '配信画面より、データを送信して下さい。'
        ELSE
          ?KEKA3 :=  '【送信データ　なし】'
          ?KEKA4 :=  '送信しなくてもＯＫです。'
    END
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:/.##ﾌﾟﾛｸﾞﾗﾑ異常終了時、終了ﾒｯｾｰｼﾞ##./

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
