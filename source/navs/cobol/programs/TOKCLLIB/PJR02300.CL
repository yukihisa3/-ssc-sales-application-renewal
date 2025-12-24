/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    受領返品　　　                       *  ./
/. *   JOB-ID      :    PJR02300                             *  ./
/. *   JOB-NAME    :    受信エラーリスト再発行　　　　　     *  ./
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
    VAR       ?PGMID    ,STRING*8,VALUE-'PJR02300'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?SOKO     ,STRING*2,VALUE-'  '       /.倉庫ＣＤ    ./
    VAR       ?DSOKO    ,STRING*2,VALUE-'  '       /.代表倉庫    ./

    VAR       ?JDATE    ,STRING*8,VALUE-'00000000' /.受信日./
    VAR       ?JTIME    ,STRING*4,VALUE-'0000'     /.受信時刻./
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
    ?PGNM :=  '受信エラーリスト再発行'

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

/.----------------------------------------------------------------./
/.##発行指示入力##./
SJR0230I:

    ?STEP :=   'SJR0230I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKSOLIB/TOKELIB/TOKELIBO/TOKDTLIB/TOKFLIB/TOKKLIB/
              TOKDLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      PGM-SJR0230I.TOKSOLIB,PARA-(?JDATE,?JTIME,?PRTKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '受信エラーリスト再発行　　　指示'
              GOTO   ABEND
    END

    ?MSGX :=  '受信日　　　＝'  && ?JDATE
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '受信時刻　　＝'  && ?JTIME
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '出力区分　　＝'  && '？？？'
    IF        ?PRTKBN = ' '  THEN
              ?MSGX :=  '出力区分　　＝'  && 'レーザー'
    END
    IF        ?PRTKBN = '1'  THEN
              ?MSGX :=  '出力区分　　＝'  && '連帳'
    END
    SNDMSG    ?MSGX,TO-XCTL

/.----------------------------------------------------------------./
/.##受信エラーリスト_出力##./
SJR0220L:

    ?STEP :=   'SJR0220L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

  /.社内実行用./
  /.OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL  ./
    CASE      ?PRTKBN   OF
    /.レーザー./                                 /.↓社内にない./
    /.現地./ /.#' '#  OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL ./
    /.社内./   #' '#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL

    /.連帳./
       #'1'#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    END

    CALL      PGM-SJR0220L.TOKSOLIB,PARA-(?JDATE,?JTIME)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '受信エラーリスト　　　　　出力'
              GOTO ABEND
    END
    GOTO      RTN

/.----------------------------------------------------------------./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '受信エラーリスト　出力が異常しました。'
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
