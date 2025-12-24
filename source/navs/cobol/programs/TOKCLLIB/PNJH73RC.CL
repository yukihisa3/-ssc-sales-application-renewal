/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    リックＷｅｂＥＤＩ対応－ダイユー追加 *  ./
/. *   JOB-ID      :    PNJH73RC                             *  ./
/. *   JOB-NAME    :    発注受信データ変換　　　　　　　　   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PNJH73RC'
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
    ?PGNM :=  'リックＷｅｂデータ取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【リックＷｅｂデータ取込変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  'この処理で、各事業所のデータが作成されます。'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.----------------------------------------------------------------./

/.##受信ＤＴ初期化##  リック資材 2018/01/31 ./
STEP037:

    ?STEP :=  'STEP037 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DAIYRZ.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'リック資材【受信データ初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ初期化##  リック植物 2018/01/31 ./
STEP038:

    ?STEP :=  'STEP038 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DAIYRS.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'リック植物【受信データ初期化】'
                    GOTO ABEND
    END

/.----------------------------------------------------------------./

/.##受信ＤＴ取込処理　リック資材## 2017/01/31 ./
STEP056:

    ?STEP :=  'STEP056 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DAIYRZ.ONLBLIB,MODE-@ADD,PARA-DAIYLIC,UNIT-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【リック資材受信データ取込処理】'
            GOTO ABEND
    END

/.##受信ＤＴ取込処理　リック植物## 2017/01/31 ./
STEP057:

    ?STEP :=  'STEP057 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DAIYRS.ONLBLIB,MODE-@ADD,PARA-DAIYLIC,UNIT-2
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【リック植物受信データ取込処理】'
            GOTO ABEND
    END

/.----------------------------------------------------------------./

/.##バッチ_採番処理##./
STEP060:

    ?STEP :=  'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SJH8999B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ_採番処理】'
            GOTO ABEND
    END

/.----------------------------------------------------------------./

/.##リック資材 変換## 2018/01/31 ./
STEP120:

    ?STEP :=  'STEP120 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## リック資材　　　　　　　変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    CALL PNJH73RZ.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
    IF   ?PGCHK1  = '1'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## リック資材　　　　　　変換エラー ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【リック資材受信データ変換処理】'
         GOTO ABEND
    END

/.##受信ＤＴクリア処理 リック資材## 2018/01/31 ./
STEP125:

    ?STEP :=  'STEP125 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYLIC,UNIT-3
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【リック資材受信データクリア処理】'
            GOTO ABEND
    END

/.----------------------------------------------------------------./

/.##リック植物 変換## 2018/01/31 ./
STEP130:

    ?STEP :=  'STEP130 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## リック植物　　　　　　　変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    CALL PNJH73RS.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
    IF   ?PGCHK1  = '1'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## リック植物　　　　　　変換エラー ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【リック資材受信データ変換処理】'
         GOTO ABEND
    END

/.##受信ＤＴクリア処理 リック植物## 2018/01/31 ./
STEP135:

    ?STEP :=  'STEP135 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYLIC,UNIT-4
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【リック植物受信データクリア処理】'
            GOTO ABEND
    END

/.----------------------------------------------------------------./

/.##プログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'リック変換処理が正常終了しました'
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
    ?KEKA1 :=  'リック変換処理が異常終了しました'
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
