# PNJH3750

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH3750.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ナフコ新ＥＤＩシステム対応　　　　   *  ./
/. *   JOB-ID      :    PNJH3750                             *  ./
/. *   JOB-NAME    :    発注受信データ変換　　　　　　　　   *  ./
/. *   UPDATE      :    2019/11/26 S2245053                  *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PNJH3750'
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
    ?PGNM :=  'ナフコ新ＥＤＩ　発注データ取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ナフコ新ＥＤＩ　発注データ取込変換処理】'
    ?OPR2  :=  '受信処理は完了していますか？'
    ?OPR3  :=  '完了している場合、変換処理を実行して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/TOKDLIB/TOKDTLIB/ONLBLIB

/.##受信データ取込##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /. ↓S2245053 ./
  /.FEXPORT FILE-NAFUKOTS.ONLBLIB,MODE-@REP,PARA-NAFUKO,
            UNIT-1
    FEXPORT FILE-NAFUKOTS.ONLBLIB,MODE-@REP,PARA-NAFUKOTS,
            UNIT-1   ./
  /. ↑S2245053 ./

  /. ↓S2245053 ./
    SNDMSG  'ASSIGN NFHACSF.ONLBLIB',TO-XCTL
    ASSIGN  FILE-NFHACSF.ONLBLIB!@XCL
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【排他制御ファイルロック】'
                    GOTO ABEND
    END

    SNDMSG  'DLTFILE NFHACBK.ONLBLIB',TO-XCTL
    DLTFILE NFHACBK.ONLBLIB

    SNDMSG  'CPYFILE NFHACSF-NFHACBK',TO-XCTL
    CPYFILE  FILE-NFHACSF.ONLBLIB,TOFILE-NFHACBK.ONLBLIB,CRTFILE-@YES
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【発注データ退避】'
                    GOTO ABEND
    END
  /. ↑S2245053 ./

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

/.##ナフコ変換##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## ナフコ　　　　　　　　　変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    CALL PNJH3751.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
    IF   ?PGCHK1  = '1'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ナフコ　　　　　　　　変換エラー ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【ナフコ受信データ変換処理】'
         GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:
   /. ↓S2245053./
    RELEASE FILE-NFHACSF.ONLBLIB!@XCL
   /. ↑S2245053./

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ変換処理が正常終了しました'
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

   /. ↓S2245053./
    RELEASE FILE-NFHACSF.ONLBLIB!@XCL
   /. ↑S2245053./
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ変換処理が異常終了しました'
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
