/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    請求　　　　　                       *  ./
/. *   JOB-ID      :    PSV00200                             *  ./
/. *   JOB-NAME    :    DCMJAPAN支払明細CSVデータ出力        *  ./
/. *   UPDATE      :    2019/03/05                           *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSV00200'
    VAR       ?STEP     ,STRING*8

    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?SIMEBI   ,STRING*8,VALUE-'00000000' /.締日       ./
    VAR       ?TORICD   ,STRING*8,VALUE-'00000000' /.取引先ＣＤ ./

    VAR       ?P1       ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?BTYMD    ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?P2       ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?BTTIME   ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?P3       ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?BTTOKC   ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?P4       ,STRING*2,VALUE-'0'        /.倉庫ＣＤ  ./
    VAR       ?SAKUCD   ,STRING*40,
                VALUE-'                                        '
  /.VAR       ?SAKUCDS  ,STRING*2,VALUE-'00'./     /.作場(開始)./
  /.VAR       ?SAKUCDE  ,STRING*2,VALUE-'00'./     /.作場(終了)./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./
    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./

/.-----------------------------------------------------------./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
/.----------------------------------------------------------------./
/.  ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES      ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＤＣＭ支払明細ＣＳＶデータ出力'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

    DEFLIBL TOKELIB/TOKELIBO/TOKDTLIB/TOKDLIB/TOKFLIB/TOKKLIB/TOKWLIB

FILECHK:

    ASSIGN FILE-CSVME87.TOKDTLIB!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得１'
           SNDMSG 'ファイル排他獲得１',TOWS-@ORGWS
           GOTO   ERR
    END

/.----------------------------------------------------------------./
/.##ＣＳＶデータ出力指示##./

SSV0021I:

    ?STEP :=   'SSV0021I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      PGM-SSV0021I.TOKSOLIB,PARA-(?SIMEBI,?TORICD)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＤＣＭ支払明細ＣＳＶ出力指示'
              GOTO   ABEND
    END

/.----------------------------------------------------------------./
/.##ＣＳＶデータ作成##./

SSV0022V:

    ?STEP :=   'SSV0022V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-SITME872,TOFILE-SITME872.TOKKLIB
    OVRF      FILE-CSVME87,TOFILE-CSVME87.TOKDTLIB
    CALL      PGM-SSV0022V.TOKSOLIB,PARA-(?SIMEBI,?TORICD)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＤＣＭ支払明細ＣＳＶ作成'
              GOTO   ABEND
    END

FIMPORT1:   /.ＣＳＶ転送./

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-CSVME87.TOKDTLIB,TYPE-@FILE,
              PARA-SEIKSIHA,UNIT-2,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＤＣＭ支払明細ＣＳＶ転送'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ＤＣＭ支払明細ＣＳＶ出力が異常終了しました。'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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

    RETURN    PGMEC-?PGMEC

ERR:  /.資源の解放./

    ?OPR1  :=  '　　＃＃＃＃＃＃＃　資源使用中　＃＃＃＃＃＃＃　　'
    ?OPR2  :=  '　　現在、ＣＳＶファイルを　他で使用中です。　　　'
    ?OPR3  :=  '　　少し時間をおいて再度、実行して下さい。　　　　'
    ?OPR4  :=  ''
    ?OPR5  :=  '　　ＥＮＴＥＲ＝再実行，ＰＦ９＝プログラム終了　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    GOTO   FILECHK
