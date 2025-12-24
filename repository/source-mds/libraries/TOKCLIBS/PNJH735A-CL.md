# PNJH735A

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH735A.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ダイユーエイトＷｅｂＥＤＩ対応　　   *  ./
/. *   JOB-ID      :    PNJH735A                             *  ./
/. *   JOB-NAME    :    発注受信データ変換　　　　　　　　   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PNJH735A'
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
    ?PGNM :=  'ダイユーエイトＷｅｂデータ取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ダイユーエイトＷｅｂデータ取込変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  '（ＭＡＸ事業部対応／日敷対応）　　　　　　　'
    ?OPR4  :=  'この処理で、各事業所のデータが作成されます。'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##受信ＤＴ初期化##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DAIYU8.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ初期化##./
STEP031:

    ?STEP :=   'STEP031 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DAIYU8M.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ初期化##./
STEP035:

    ?STEP :=   'STEP035 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DAIYU8F.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ初期化##  日敷 2013.09.09 ./
STEP036:

    ?STEP :=   'STEP036 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DAIYU8N.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理　ダイユーエイト##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DAIYU8.ONLBLIB,MODE-@ADD,PARA-DAIYU8,
            UNIT-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ダイユーエイト受信データ取込処理】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理　エイトファーム##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DAIYU8F.ONLBLIB,MODE-@ADD,PARA-DAIYU8,
            UNIT-2
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【エイトファーム受信データ取込処理】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理　ダイユーエイトＭＡＸ##./
STEP051:

    ?STEP :=   'STEP051 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DAIYU8M.ONLBLIB,MODE-@ADD,PARA-DAIYUMAX,
            UNIT-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ダイユーＭＡＸ受信データ取込処理】'
                    GOTO ABEND
    END

/.##ダイユーエイトＭＡＸデータをダイユーエイトに追加コピー##./
STEP052:

    ?STEP :=   'STEP052 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-DAIYU8M.ONLBLIB,TOFILE-DAIYU8.ONLBLIB,BF-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ＭＡＸ＝＞ダイユーコピー（追加）】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理　日敷## 2013.09.09 ./
STEP055:

    ?STEP :=   'STEP055 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DAIYU8N.ONLBLIB,MODE-@ADD,PARA-DAIYU8N,
            UNIT-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【日敷受信データ取込処理】'
                    GOTO ABEND
    END

/.##バッチ_採番処理##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SJH8999B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ_採番処理】'
                    GOTO ABEND
    END

/.##ダイユーエイト変換##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## ダイユーエイト　　　　　変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    OVRF      FILE-CVCSG001,TOFILE-DAIYU8.ONLBLIB
    CALL PNJH7351.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
    IF   ?PGCHK1  = '1'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイユーエイト　　　　変換エラー ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【ダイユーエイト受信データ変換処理】'
         GOTO ABEND
    END

/.##受信ＤＴクリア処理　ダイユーエイト##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8,UNIT-13
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ダイユーエイト受信データクリア処理】'
                    GOTO ABEND
    END
    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYUMAX,UNIT-2
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ダイユーエイト受信データクリア処理】'
                    GOTO ABEND
    END


/.##エイトファーム変換##./
STEP090:

    ?STEP :=   'STEP090 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## エイトファーム　　　　　変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    OVRF      FILE-CVCSG001,TOFILE-DAIYU8F.ONLBLIB
    CALL PNJH735E.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
    IF   ?PGCHK1  = '1'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## エイトファーム　　　　変換エラー ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【エイトファーム受信データ変換処理】'
         GOTO ABEND
    END

/.##受信ＤＴクリア処理##./
STEP100:

    ?STEP :=   'STEP100 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8,UNIT-14
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【エイトファーム受信データクリア処理】'
                    GOTO ABEND
    END

/.##日敷 変換## 2013.09.09 ./
STEP110:

    ?STEP :=   'STEP110 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## 日敷　　　　　　　　　　変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    OVRF      FILE-CVCSG001,TOFILE-DAIYU8N.ONLBLIB
    CALL PNJH735N.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
    IF   ?PGCHK1  = '1'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## 日敷　　　　　　　　　変換エラー ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【日敷受信データ変換処理】'
         GOTO ABEND
    END

/.##受信ＤＴクリア処理 日敷## 2013.09.09 ./
STEP115:

    ?STEP :=   'STEP115 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8N,UNIT-2
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【日敷受信データクリア処理】'
                    GOTO ABEND
    END


/.##プログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ダイユーエイト変換処理が正常終了しました'
    ?KEKA2 :=  '件数リスト等を確認して下さい。'
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
    ?KEKA1 :=  'ダイユーエイト変換処理が異常終了しました'
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
