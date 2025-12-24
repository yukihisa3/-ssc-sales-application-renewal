/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ケーヨー伝票レス　　      　　　     *  ./
/. *   JOB-ID      :    PSK00200                             *  ./
/. *   JOB-NAME    :    ケーヨー受領書発行                   *  ./
/. *               :    受領書ＣＳＶ出力                     *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM     (P1-?PRTKBN)
    PARA ?PRTKBN  ,STRING*1,IN,VALUE-' '        /.用紙区分 ./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSK00200'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1./
    VAR ?KEKA1    ,STRING*40                    /.      2./
    VAR ?KEKA2    ,STRING*40                    /.      3./
    VAR ?KEKA3    ,STRING*40                    /.      4./
    VAR ?KEKA4    ,STRING*40                    /.      5./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
/. 共通パラメタ./
    VAR ?SKBN     ,STRING*1,VALUE-' '          /. 出力区分 ./
    VAR ?DKBN     ,STRING*1,VALUE-' '          /. 日付区分 ./
    VAR ?DFROM    ,STRING*8,VALUE-'        '   /. 日付Ｆ   ./
    VAR ?DTO      ,STRING*8,VALUE-'        '   /. 日付Ｔ   ./
    VAR ?KKBN     ,STRING*1,VALUE-' '          /. 計上区分 ./
    VAR ?TANF     ,STRING*2,VALUE-'  '         /. 担当者Ｆ ./
    VAR ?TANT     ,STRING*2,VALUE-'  '         /. 担当者Ｔ ./
    VAR ?DENK1    ,STRING*2,VALUE-'  '         /. 伝区１   ./
    VAR ?DENK2    ,STRING*2,VALUE-'  '         /. 伝区２   ./
    VAR ?DENK3    ,STRING*2,VALUE-'  '         /. 伝区３   ./
    VAR ?DENK4    ,STRING*2,VALUE-'  '         /. 伝区４   ./
    VAR ?DENK5    ,STRING*2,VALUE-'  '         /. 伝区５   ./
    VAR ?TENF     ,STRING*5,VALUE-'     '      /. 店舗Ｆ   ./
    VAR ?TENT     ,STRING*5,VALUE-'     '      /. 店舗Ｔ   ./
    VAR ?DENNF    ,STRING*9,VALUE-'         '  /. 伝番Ｆ　 ./
    VAR ?DENNT    ,STRING*9,VALUE-'         '  /. 伝番Ｔ　 ./
    VAR ?SKBF     ,STRING*2,VALUE-'  '         /. 出場Ｆ　 ./
    VAR ?SKBT     ,STRING*2,VALUE-'  '         /. 出場Ｔ   ./
    VAR ?DENKF    ,STRING*2,VALUE-'  '         /. 伝区Ｆ　 ./
    VAR ?DENKT    ,STRING*2,VALUE-'  '         /. 伝区Ｔ   ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB/TOKELIBO

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  'ケーヨー　受領書発行'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得 ##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ログインユーザー情報取得 ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END

/.## 受領書発行指示 ##./
SSK0020I:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSK0020I.TOKELIBO,PARA-(?BUMON,?TANCD,
                                     ?SKBN,?DKBN,?DFROM,?DTO,
                                     ?KKBN,?TANF,?TANT,?DENK1,
                                     ?DENK2,?DENK3,?DENK4,?DENK5,
                                     ?TENF,?TENT,?DENNF,?DENNT,
                                     ?SKBF,?SKBT,?DENKF,?DENKT)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        IF ?PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO  RTN
        ELSE
           ?KEKA4 := '受領書発行指示'
           GOTO  ABEND
        END
    END

    ?MSGX :=  '# 出力区分 = ' && ?SKBN && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 日付区分 = ' && ?DKBN && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 日付Ｆ = ' && ?DFROM && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 日付Ｔ = ' && ?DTO && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 計上区分 = ' && ?KKBN && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 担当者Ｆ = ' && ?TANF && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 担当者Ｔ = ' && ?TANT && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝区１　 = ' && ?DENK1 && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝区２　 = ' && ?DENK2 && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝区３　 = ' && ?DENK3 && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝区４　 = ' && ?DENK4 && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝区５　 = ' && ?DENK5 && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 店舗Ｆ　 = ' && ?TENF && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 店舗Ｔ　 = ' && ?TENT && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝番Ｆ　 = ' && ?DENNF && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝番Ｔ　 = ' && ?DENNT && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 出場Ｆ　 = ' && ?SKBF && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 出場Ｔ　 = ' && ?SKBT && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝区Ｆ　 = ' && ?DENKF && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 伝区Ｔ　 = ' && ?DENKT && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## 受領データ抽出  ##./
SSK0021B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSK0021B.TOKELIBO,PARA-(
                                     ?SKBN,?DKBN,?DFROM,?DTO,
                                     ?KKBN,?TANF,?TANT,?DENK1,
                                     ?DENK2,?DENK3,?DENK4,?DENK5,
                                     ?TENF,?TENT,?DENNF,?DENNT,
                                     ?SKBF,?SKBT,?DENKF,?DENKT)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        IF ?PGMEC = 4001  THEN
           SNDMSG MSG-'対象データ無し',TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO  RTN
        ELSE
           ?KEKA4 := '受領データ抽出'
           GOTO  ABEND
        END
    ELSE
        GOTO  FURIWAKE
    END

/.## 処理振り分け ##./
FURIWAKE:

    IF  ?SKBN = '1'  THEN
         GOTO  SSK0022L
    ELSE
         GOTO  SSK0023V
    END

/.## 受領書発行 ##./
SSK0022L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. プリンタ出力先制御　１：レーザー　２：連帳 ./
    IF    ?PRTKBN  =    '1'   THEN
          OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    ELSE
          OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    END

    CALL PGM-SSK0022L.TOKELIBO,PARA-(
                                     ?SKBN,?DKBN,?DFROM,?DTO,
                                     ?KKBN,?TANF,?TANT,?DENK1,
                                     ?DENK2,?DENK3,?DENK4,?DENK5,
                                     ?TENF,?TENT,?DENNF,?DENNT,
                                     ?SKBF,?SKBT,?DENKF,?DENKT)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?KEKA4 := '受領書発行'
        GOTO  ABEND
    ELSE
        GOTO  RTN
    END

/.## 受領書ＣＳＶ ##./
SSK0023V:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSK0023V.TOKELIBO,PARA-(
                                     ?SKBN,?DKBN,?DFROM,?DTO,
                                     ?KKBN,?TANF,?TANT,?DENK1,
                                     ?DENK2,?DENK3,?DENK4,?DENK5,
                                     ?TENF,?TENT,?DENNF,?DENNT,
                                     ?SKBF,?SKBT,?DENKF,?DENKT)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?KEKA4 := '受領書ＣＳＶ'
        GOTO  ABEND
    END

/.## 受領データＣＳＶ出力 ##./
FIMPORT:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-KEICSVJR.TOKKLIB,PARA-PARAKEI,UNIT-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
           ?KEKA4 := '受領データＣＳＶ出力'
           GOTO  ABEND
    ELSE
        GOTO  RTN
    END


/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO ?MSGX :=   ?MSG(?I)
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
