# PNJH9400

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PNJH9400.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   サカタのタネ　　　　　　　　　　　　　　　　　        *  ./
/. *   SYSTEM-NAME :    ヨドバシ　　　ＥＤＩ　　　　　　　   *  ./
/. *   JOB-ID      :    PNJH9400                             *  ./
/. *   JOB-NAME    :    ヨドバシ発注データ取込チェック　　   *  ./
/. *   UPDATE      :    2021/07/26 S22461250                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PNJH9400'
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

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKSOLIB/
            TOKDLIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ヨドバシ　受注データ取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ヨドバシ　受注データ取込変換処理】　'
    ?OPR2  :=  'データ準備は完了していますか？'
    ?OPR3  :=  '完了している場合、変換処理を実行して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  '処理後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/TOKDLIB/TOKDTLIB/
            TOKJLIB/ONLBLIB/TOKSOLIB

/.##受信データ取込##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG  'ASSIGN YODJYUSF.TOKDTLIB',TO-XCTL
    ASSIGN  FILE-YODJYUSF.TOKDTLIB!@XCL
  /.IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【排他制御ファイルロック】'
                    GOTO ABEND
    END./

    SNDMSG  'DLTFILE AMZJYUBK.TOKDTLIB',TO-XCTL
    DLTFILE YODJYUBK.TOKDTLIB

    SNDMSG  'CPYFILE YODJYUSF-YODJYUBK',TO-XCTL
    CPYFILE  FILE-YODJYUSF.TOKDTLIB,TOFILE-YODJYUBK.TOKDTLIB,
             CRTFILE-@YES
  /.IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受注データ退避】'
                    GOTO ABEND
    END    ./

    SNDMSG  'FEXPORT YODJYUSF.TOKDTLIB',TO-XCTL

    FEXPORT FILE-YODJYUSF.TOKDTLIB,
            TYPE-@FILE,
            MODE-@REP,
            PARA-YODOBASI,
            UNIT-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受注データ転送】'
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

/.##変換ジョブ実行##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## ヨドバシ　受注　　　変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    CALL PNJH9420.TOKCOLIB,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
    IF   ?PGCHK1  = '0'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ヨドバシ　受注　　データ０件 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【ヨドバシ受注変換処理】'
         GOTO ZEROKEN
    END
    IF   ?PGCHK1  = '1'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ヨドバシ　受注　　変換ＯＫ　 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【ヨドバシ受注変換処理】'

         FIMPORT FILE-YODZEROF.TOKDTLIB,PARA-YODOBASI,UNIT-4

         GOTO RTN
    END
    IF   ?PGCHK1  = '2'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ヨドバシ　受注　データエラー ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【ヨドバシ受注変換処理】'
         GOTO ABEND
    END
    IF   ?PGCHK1  = '3'    THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ヨドバシ　受注　処理異常　　 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         ?KEKA4 := '【ヨドバシ受注変換処理】'
         GOTO ABEND
    END

/.##データ０件（資源の開放）##./
ZEROKEN:

    RELEASE FILE-YODJYUSF.TOKDTLIB!@XCL

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '発注データはゼロ件です'
    ?KEKA2 :=  '確認して下さい'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム正常終了（資源の開放）##./
RTN:

    RELEASE FILE-YODJYUSF.TOKDTLIB!@XCL

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ヨドバシ変換処理が正常終了しました'
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

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    RELEASE FILE-YODJYUSF.TOKDTLIB!@XCL

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    IF      ?PGCHK1  =  '3'  THEN
            ?KEKA1 := 'ヨドバシ変換処理が異常終了しました'
            ?KEKA2 := 'ログ採取し，ＮＡＶへ連絡して下さい。'
            ?KEKA3 := ''
    END
    IF      ?PGCHK1  =  '2'  THEN
            ?KEKA1 := '受注データにエラーがあります。'
            ?KEKA2 := 'データ、マスタを確認・修正し'
            ?KEKA3 := '再度実行してください。'
    END
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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

    RETURN    PGMEC-?PGMEC

```
