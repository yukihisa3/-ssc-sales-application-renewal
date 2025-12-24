/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    受領返品　　　                       *  ./
/. *   JOB-ID      :    PJR02400                             *  ./
/. *   JOB-NAME    :    共通受領書発行（帳票／ＣＳＶ）　     *  ./
/. *   UPDATE      :                                         *  ./
/. *                                                         *  ./
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
    VAR       ?PGMID    ,STRING*8,VALUE-'PJR02400'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./

    VAR       ?SOKO     ,STRING*2,VALUE-'  '       /.倉庫ＣＤ    ./
    VAR       ?DSOKO    ,STRING*2,VALUE-'  '       /.代表倉庫    ./

    VAR       ?TOKCD    ,STRING*8,VALUE-'00000000' /.取引先ＣＤ  ./
    VAR       ?DTKBN    ,STRING*1,VALUE-' '        /.日付区分　　./
    VAR       ?DATEF    ,STRING*8,VALUE-'00000000' /.開始日付./
    VAR       ?DATET    ,STRING*8,VALUE-'00000000' /.終了日付./
    VAR       ?HENKBN   ,STRING*1,VALUE-' '        /.返品区分　　./
    VAR       ?OUTKBN   ,STRING*1,VALUE-' '        /.発行区分　　./
    VAR       ?PRTKBN   ,STRING*1,VALUE-' '        /.出力区分　　./

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
/.-----------------------------------------------------------./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '共通受領書　発行'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKO,?DSOKO)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO ABEND
    END

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

  /.GOTO SJR0260L./

/.----------------------------------------------------------------./
/.##発行指示入力##./
SJR0240I:

    ?STEP :=   'SJR0240I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKELIBO/TOKDTLIB/TOKFLIB/TOKKLIB/
              TOKSOLIB/TOKWLIB/TOKDLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      PGM-SJR0240I.TOKSOLIB,PARA-(?TOKCD,
                                          ?DTKBN,?DATEF,?DATET,
                                          ?HENKBN,
                                          ?OUTKBN,?PRTKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '共通受領書　指示'
              GOTO   ABEND
    END

    ?MSGX :=  '取引先ＣＤ　＝'  && ?TOKCD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '日付区分　　＝'  && '？？？'
    IF        ?DTKBN  = ' '  THEN
              ?MSGX :=  '日付区分　　＝'  && '受信日'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?DTKBN  = '1'  THEN
              ?MSGX :=  '日付区分　　＝'  && '計上日'
              SNDMSG    ?MSGX,TO-XCTL
    END

    ?MSGX :=  '　日付（開）＝'  && ?DATEF
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '　日付（終）＝'  && ?DATET
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '返品区分　　＝'  && '？？？'
    IF        ?HENKBN = ' '  THEN
              ?MSGX :=  '返品区分　　＝'  && '全て'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?HENKBN  = '1'  THEN
              ?MSGX :=  '返品区分　　＝'  && '返品のみ'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?HENKBN  = '2'  THEN
              ?MSGX :=  '返品区分　　＝'  && '受領のみ'
              SNDMSG    ?MSGX,TO-XCTL
    END

    ?MSGX :=  '発行区分　　＝'  && '？？？'
    IF        ?OUTKBN = ' '  THEN
              ?MSGX :=  '発行区分　　＝'  && '帳票出力'
              SNDMSG    ?MSGX,TO-XCTL

              ?MSGX :=  '出力区分　　＝'  && '？？？'
              IF        ?PRTKBN = ' '  THEN
                        ?MSGX :=  '出力区分　　＝'  && 'レーザー'
              END
              IF        ?PRTKBN = '1'  THEN
                        ?MSGX :=  '出力区分　　＝'  && '連帳'
              END
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?OUTKBN = '1'  THEN
              ?MSGX :=  '発行区分　　＝'  && 'ＣＳＶ出力'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.----------------------------------------------------------------./
/.##ファイル排他##./
FILECHK:

    ?STEP :=   'FILECHK'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF     ?OUTKBN  =   '1'   THEN     /.CSV出力指定時のみ./
           ASSIGN FILE-COMJRCSV.TOKWLIB!@XCL
           ?PGMEC := @PGMEC
           ?PGMES := @PGMES
           IF     ?PGMEC  ^=    0    THEN
                  ?KEKA4 := 'ファイル排他獲得'
                  GOTO   ERR
           END
    END

/.##共通受領書データ抽出##./
SJR0250B:

    ?STEP :=   'SJR0250B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    CALL      PGM-SJR0250B.TOKSOLIB,PARA-(?TOKCD,?DTKBN,?DATEF,?DATET,
                                          ?HENKBN)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4001 THEN
              GOTO OUTCHK END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '共通受領書データ抽出'
              GOTO ABEND END

/.----------------------------------------------------------------./
OUTCHK:
    IF     ?OUTKBN  =   ' '   THEN     /.帳票出力指定時./
           GOTO  SJR0260L
    END
    IF     ?OUTKBN  =   '1'   THEN     /.CSV出力指定時./
           GOTO  SJR0270V
    END
/.----------------------------------------------------------------./

/.##共通受領書発行##./
SJR0260L:

    ?STEP :=   'SJR0260L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

  /.↓./
  /.OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL  ./
    CASE      ?PRTKBN   OF
    /.レーザー./                                 /.↓社内にない./
    /.現地./   #' '#  OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    /.社内./ /.#' '#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL./

    /.連帳./
       #'1'#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    END
  /.↑./

    CALL      PGM-SJR0260L.TOKSOLIB,PARA-(?DTKBN,?DATEF,?DATET,
                                          ?HENKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '共通受領書発行'
              GOTO ABEND
    END
    GOTO      RTN

/.----------------------------------------------------------------./
/.##共通受領書発行_ＣＳＶ作成##./
SJR0270V:

    ?STEP :=   'SJR0270V'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    CALL      PGM-SJR0270V.TOKSOLIB,PARA-(?DTKBN,?DATEF,?DATET,
                                          ?HENKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '共通受領書ＣＳＶ作成'
              GOTO ABEND
    END

FIMPORT1:   /.共通受領書ＣＳＶ転送./

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-COMJRCSV.TOKWLIB,TYPE-@FILE,
              PARA-COMJURYO,UNIT-1,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '共通受領書ＣＳＶ転送'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '共通受領書発行が異常しました。　　　'
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
    ?OPR2  :=  '　　現在、共通受領書ＣＳＶファイルを他で使用中　　'
    ?OPR3  :=  '　　です。少し時間をおいて再度、実行して下さい。　'
    ?OPR4  :=  ''
    ?OPR5  :=  '　　ＥＮＴＥＲ＝再実行，ＰＦ９＝プログラム終了　　'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    GOTO   FILECHK
