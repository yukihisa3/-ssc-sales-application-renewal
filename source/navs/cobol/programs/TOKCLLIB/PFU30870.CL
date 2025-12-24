/. ***TEST****************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    社内振替　　　                       *  ./
/. *   JOB-ID      :    PFU30870                             *  ./
/. *   JOB-NAME    :    社内振替情報チェックリスト（入力順） *  ./
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
    VAR       ?PGMID    ,STRING*8,VALUE-'PFU30870'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./

    VAR       ?SOKO     ,STRING*2,VALUE-'  '       /.倉庫ＣＤ    ./
    VAR       ?DSOKO    ,STRING*2,VALUE-'  '       /.代表倉庫    ./

    VAR       ?OUTSOKO  ,STRING*2,VALUE-'  '       /.出力倉庫　  ./
    VAR       ?DATEF    ,STRING*8,VALUE-'00000000' /.入力日（開）./
    VAR       ?DATET    ,STRING*8,VALUE-'00000000' /.入力日（終）./
    VAR       ?DTKBN    ,STRING*1,VALUE-' '        /.ＤＴ区分　  ./
    VAR       ?INKBN    ,STRING*1,VALUE-' '        /.入力区分　  ./
    VAR       ?KANKBN   ,STRING*1,VALUE-' '        /.完納区分　　./
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
    ?PGNM :=  '社内振替情報チェックリスト出力'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDTLIB/TOKDLIB/TOKFLIB/TOKKLIB
             /TOKMDLIB

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

  /.GOTO SFU3091L./

/.----------------------------------------------------------------./
/.##発行指示入力##./
SFU3080I:

    ?STEP :=   'SFU3080I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      PGM-SFU3080I.TOKSOLIB,PARA-(?SOKO,?DSOKO,
                                          ?OUTSOKO,?DATEF,?DATET,
                                          ?DTKBN,?INKBN,
                                          ?KANKBN,
                                          ?OUTKBN,?PRTKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '社内振替情報チェックリスト　指示'
              GOTO   ABEND
    END

    ?MSGX :=  '出力倉庫　　＝'  && ?OUTSOKO
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '入力日（開）＝'  && ?DATEF
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '入力日（終）＝'  && ?DATET
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  'ＤＴ区分　　＝'  && '？？？'
    IF        ?DTKBN  = ' '  THEN
              ?MSGX :=  'ＤＴ区分　　＝'  && '全て'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?DTKBN  = '1'  THEN
              ?MSGX :=  'ＤＴ区分　　＝'  && '発注のみ'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?DTKBN  = '2'  THEN
              ?MSGX :=  'ＤＴ区分　　＝'  && '入荷のみ'
              SNDMSG    ?MSGX,TO-XCTL
    END

    ?MSGX :=  '入力区分　　＝'  && '？？？'
    IF        ?INKBN  = ' '  THEN
              ?MSGX :=  '入力区分　　＝'  && '全て'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?INKBN  = '1'  THEN
              ?MSGX :=  '入力区分　　＝'  && '入力分'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?INKBN  = '2'  THEN
              ?MSGX :=  '入力区分　　＝'  && 'ＥＸＣＥＬ分'
              SNDMSG    ?MSGX,TO-XCTL
    END

    ?MSGX :=  '完納区分　　＝'  && '？？？'
    IF        ?KANKBN = ' '  THEN
              ?MSGX :=  '完納区分　　＝'  && '全て'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?KANKBN = '1'  THEN
              ?MSGX :=  '完納区分　　＝'  && '未完納分'
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?KANKBN = '2'  THEN
              ?MSGX :=  '完納区分　　＝'  && '完納分'
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
           ASSIGN FILE-SFRCHK.TOKDTLIB!@XCL
           ?PGMEC := @PGMEC
           ?PGMES := @PGMES
           IF     ?PGMEC  ^=    0    THEN
                  ?KEKA4 := 'ファイル排他獲得'
                  GOTO   ERR
           END
    END

/.##社内振替情報リスト出力ワーク作成１##./
SFU3087B:

    ?STEP :=   'SFU3087B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    CALL      PGM-SFU3087B.TOKSOLIB,PARA-(?OUTSOKO,?DATEF,?DATET,
                                          ?DTKBN,?INKBN,
                                          ?KANKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'リスト出力ワーク作成１'
              GOTO ABEND END

/.----------------------------------------------------------------./
OUTCHK:
    IF     ?OUTKBN  =   ' '   THEN     /.帳票出力指定時./
           GOTO  SFU3091L
    END
    IF     ?OUTKBN  =   '1'   THEN     /.CSV出力指定時./
           GOTO  SFU3100V
    END
/.----------------------------------------------------------------./

/.##社内振替情報チェックリスト_出力##./
SFU3091L:

    ?STEP :=   'SFU3091L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    IF ?PRTKBN   = '1'  THEN
       OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    ELSE
      CASE  ?SOKO  OF
       # 'TU' #
       ?MSGX :=  '##　ＡＷ札幌配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTU.TOKELIBO
       # '6A' #
       ?MSGX :=  '##　片岡配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT6A.TOKELIBO
       # '63' #
       ?MSGX :=  '##　フバサミ配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT63.TOKELIBO
       # '84' #
       ?MSGX :=  '##　鴻巣配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT84.TOKELIBO
       # 'TV' #
       ?MSGX :=  '##　西尾配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTV.TOKELIBO
       # 'T5' #
       ?MSGX :=  '##　ＫＡ岡山配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTT5.TOKELIBO
       # 'E1' #
       ?MSGX :=  '##　甘木配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTE1.TOKELIBO
       # '01' #
       ?MSGX :=  '##　本社　　　　　　　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO
      ELSE
       ?MSGX :=  '##　出力判定不能　本社へ出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO
      END
    END

    CALL      PGM-SFU3091L.TOKSOLIB,PARA-(?OUTSOKO,?DATEF,?DATET,
                                          ?KANKBN,?DTKBN,?INKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '社内振替情報チェックリスト出力'
              GOTO ABEND
    END
    GOTO      RTN

/.----------------------------------------------------------------./
/.##社内振替情報チェックリスト_ＣＳＶ作成##./
SFU3100V:

    ?STEP :=   'SFU3100V'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    CALL      PGM-SFU3100V.TOKSOLIB,PARA-(?OUTSOKO,?DATEF,?DATET,
                                          ?KANKBN,?DTKBN,?INKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '社内振替情報ＣＳＶ作成'
              GOTO ABEND
    END

FIMPORT1:   /.社内振替情報チェックリストＣＳＶ転送./

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-SFRCHK.TOKDTLIB,TYPE-@FILE,
              PARA-SFURIKAE,UNIT-2,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '社内振替情報リストＣＳＶ転送'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '社内振替情報リスト出力が異常しました。'
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
    ?OPR2  :=  '　　現在、社内振替ＣＳＶファイルを他で使用中　　　'
    ?OPR3  :=  '　　です。少し時間をおいて再度、実行して下さい。　'
    ?OPR4  :=  ''
    ?OPR5  :=  '　　ＥＮＴＥＲ＝再実行，ＰＦ９＝プログラム終了　　'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    GOTO   FILECHK
