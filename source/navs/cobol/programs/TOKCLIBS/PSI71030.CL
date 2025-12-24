/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSI71030                             *  ./
/. *   JOB-NAME    :    支払明細書発行　　　　　　　　　     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSI71030'
    VAR       ?STEP     ,STRING*8
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./
    VAR       ?BUMON    ,STRING*4,VALUE-'    '
    VAR       ?TANCD    ,STRING*8,VALUE-'        '
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
     DEFLIBL   TOKFLIB/TOKELIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'Ｊ本田　　　支払明細書発行'

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
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##確認画面##./
KAKUNIN:

    ?STEP :=   'KAKUNIN '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '処理名：ジョイフル本田　支払明細書発行（Ａ４）'
    ?OPR2  :=  ''
    ?OPR3  :=  '　上記、帳票を発行します。確認して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##支払データ編集##./
CVCS256:

    ?STEP :=   'CVCS256 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOFUL,TOFILE-SHIHONDA.ONLBLIB
    OVRF      FILE-CVCS256,TOFILE-SHIHONDA.ONLBLIB
    OVRF      FILE-CVCS128,TOFILE-HONDASHI.TOKWLIB
    CALL      PGM-CVCS256.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【２５６→１２８分割】'
              GOTO ABEND END

/.##支払データ編集##./
SSI7101B:

    ?STEP :=   'SSI7101B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOFUL,TOFILE-HONDASHI.TOKWLIB
    OVRF      FILE-SITGK711,TOFILE-SITGK711.TOKWLIB
    OVRF      FILE-SITGG711,TOFILE-SITGG711.TOKWLIB
    CALL      PGM-SSI7103B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【支払データ分割】'
              GOTO ABEND END

/.##支払明細書##./
SSI7104L:

    ?STEP :=   'SSI7104L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##本社ﾚｰｻﾞｰへ出力##./
    IF ?BUMON = '2920' THEN
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    ELSE
       OVRPRTF FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
    END
    OVRF     FILE-SITGK711,TOFILE-SITGK711.TOKWLIB
    OVRF     FILE-SITGG711,TOFILE-SITGG711.TOKWLIB
    OVRF     FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL     PGM-SSI7104L.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【支払明細書発行】'
              GOTO ABEND END


RTN:

    ?KEKA1 :=  '支払明細書の発行が正常終了しました。'
    ?KEKA2 :=  '確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '支払明細書発行が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
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