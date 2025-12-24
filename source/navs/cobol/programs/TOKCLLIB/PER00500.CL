/. ***********************************************************  ./
/. *   サカタのタネ　営業第二部　　　　　　　　　　  　      *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PER00500                             *  ./
/. *   JOB-NAME    :    Ｄ３６５エラーリカバリデータ更新　   *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PER00500'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,NAME!MOD
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK3   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?OK1    ,STRING*2,VALUE-'  '        /.実行確認1./
    VAR       ?OK2    ,STRING*2,VALUE-'  '        /.実行確認2./
    VAR       ?OK3    ,STRING*2,VALUE-'  '        /.実行確認3./
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
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR       ?BUMON    ,STRING*4,VALUE-'    '   /.部門名./
    VAR       ?TANCD8   ,STRING*8,VALUE-'        '  /.担当者CD./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '        /.担当者CD./
    VAR       ?WS ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?SOKCD  ,STRING*2,VALUE-'  '         /.自倉庫ｺｰﾄﾞ./
    VAR       ?DSOKCD ,STRING*2,VALUE-'  '         /.代表倉庫ｺｰﾄﾞ./
    VAR       ?BDATE  ,STRING*8,VALUE-'00000000'   /.受信日付./
    VAR       ?BTIME  ,STRING*6,VALUE-'000000'     /.受信時間./
    VAR       ?TORICD ,STRING*8,VALUE-'00000000'   /.取引先./
    VAR       ?SOKO   ,STRING*2,VALUE-'00'         /.出荷場所./
    VAR       ?NOUDT  ,STRING*8,VALUE-'00000000'   /.納品日./
    VAR       ?BMNCD  ,STRING*4,VALUE-'0000'       /.部門./
    VAR       ?TNCD   ,STRING*2,VALUE-'00'         /.担当者　./
    VAR       ?SITEI  ,STRING*1,VALUE-'2'          /.非定型　./
    VAR       ?SYURUI ,STRING*1,VALUE-'1'          /.種類　　./
    VAR       ?KANRINO,STRING*8,VALUE-'00000000'   /.管理番号./
    VAR       ?ERR1   ,STRING*1,VALUE-' '          /.対象１./
    VAR       ?ERR2   ,STRING*1,VALUE-' '          /.対象２./
    VAR       ?ERR3   ,STRING*1,VALUE-' '          /.対象３./
    VAR       ?ERR4   ,STRING*1,VALUE-' '          /.対象４./
    VAR       ?SEL    ,STRING*1,VALUE-' '          /.選択　./
    VAR       ?OUTPUT ,STRING*1,VALUE-' '          /.出力区分./

    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'Ｄ３６５エラーリカバリデータ更新'
    ?SYORINM1 :=  'Ｄ３６５エラーリカバリデータ更新'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB/TOKSOLIB/
            TOKMDLIB
/.##ログインユーザー情報取得##./
STEP030:

    ?STEP :=   'STEP030'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)


/.##倉庫ｺｰﾄﾞ取得##./
STEP040:

    ?STEP :=   'STEP040'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##エラーリカバリ更新指示##./
SER0050I:

    ?STEP :=   'SER0050I '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL SER0050I.TOKSOLIB,PARA-(?BUMON,?TANCD,
                                 ?ERR1,?ERR2,?ERR3,?ERR4,?SEL,
                                 ?KANRINO,?BDATE,?BTIME)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
       IF      ?PGMEC    =  4010    THEN
               SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
               GOTO RTN
       ELSE
               ?KEKA4 := '【エラーリカバリ更新指示】'
               GOTO ABEND
       END
    END

    ?MSGX    :=  '## 対象１　　 = ' && ?ERR1
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX    :=  '## 対象２　　 = ' && ?ERR2
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX    :=  '## 対象３　　 = ' && ?ERR3
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX    :=  '## 対象４　　 = ' && ?ERR4
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX    :=  '## 選択　　　 = ' && ?SEL
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX    :=  '## 管理番号　 = ' && ?KANRINO
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX    :=  '## バッチ日付 = ' && ?BDATE
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX    :=  '## バッチ時刻 = ' && ?BTIME
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##リカバリ対象判定(受注売上)##./
EER1:
    IF       ?ERR1  ^=   '1'  THEN
             GOTO      ERR2
    END

/.##キャンセルファイルクリア##./
CLR1:

    ?STEP :=   'CLR1     '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE  FILE-CANURIKJ.D365DLIB

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '【キャンセルファイルクリア】'
              GOTO ABEND
    END

/.##受注売上リカバリーキーデータ作成##./
SER0060B:

    ?STEP :=   'SER0060B '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL SER0060B.TOKSOLIB,PARA-(?BUMON,?TANCD,
                                 ?KANRINO,?BDATE,?BTIME)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              IF     ?PGMEC    =   4001    THEN
                     GOTO      ERR2
              ELSE
                     ?KEKA4 := '【受注売上リカバリーキーデータ作成】'
                     GOTO ABEND
              END
    END

/.##受注売上データリカバリ処理##./
SER0070B:

    ?STEP :=   'SER0070B '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL SER0070B.TOKSOLIB

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              IF        ?PGMEC    =   4001    THEN
                        GOTO      ERR2
              ELSE
                        ?KEKA4 := '【受注売上データリカバリ処理】'
                        GOTO ABEND
              END
    END

/.##リカバリ対象判定(入荷実績)##./
 ERR2:
    IF       ?ERR2  ^=   '1'  THEN
             GOTO      ERR3
    END

/.##入荷実績リカバリ処理##./
SER0080B:

    ?STEP :=   'SER0080B '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL SER0080B.TOKSOLIB,PARA-(?BUMON,?TANCD,
                                 ?KANRINO,?BDATE,?BTIME)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              IF        ?PGMEC    =   4001    THEN
                        GOTO      ERR3
              ELSE
                        ?KEKA4 := '【入荷実績リカバリ処理】'
                        GOTO ABEND
              END
    END

/.##リカバリ対象判定(移動実績)##./
 ERR3:
    IF       ?ERR3  ^=   '1'  THEN
             GOTO      ERR4
    END

/.##移動実績リカバリ処理##./
SER0090B:

    ?STEP :=   'SER0090B '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL SER0090B.TOKSOLIB,PARA-(?BUMON,?TANCD,
                                 ?KANRINO,?BDATE,?BTIME)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              IF        ?PGMEC    =   4001    THEN
                        GOTO      ERR4
              ELSE
                        ?KEKA4 := '【移動実績リカバリ処理】'
                        GOTO ABEND
              END
    END

/.##リカバリ対象判定(作業実績)##./
 ERR4:
    IF       ?ERR4  ^=   '1'  THEN
             GOTO      SER0110L
    END

/.##作業実績リカバリ処理##./
SER0100B:

    ?STEP :=   'SER0100B '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL SER0100B.TOKSOLIB,PARA-(?BUMON,?TANCD,
                                 ?KANRINO,?BDATE,?BTIME)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              IF        ?PGMEC    =   4001    THEN
                        GOTO      SER0110L
              ELSE
                        ?KEKA4 := '【作業実績リカバリ処理】'
                        GOTO ABEND
              END
    END

/.##連携エラーリカバリリスト##./
SER0110L:

    ?STEP :=   'SER0110L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /. プリンタ出力先制御　空白：連帳　１：レーザー ./
    ?OUTPUT  :=    '1'
    IF    ?OUTPUT  =    ' '   THEN
          OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    ELSE
          OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    END

    CALL SER0110L.TOKSOLIB,PARA-(?BUMON,?TANCD,
                                 ?ERR1,?ERR2,?ERR3,?ERR4,
                                 ?KANRINO,?BDATE,?BTIME)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '【連携エラーリカバリリスト】'
              GOTO ABEND
    END
    GOTO RTN

/.##プログラム正常終了##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'Ｄ３６５エラーリカバリデータ更新完了。'
    ?KEKA2 :=  'リスト等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    IF      ?PGMEC    =  0       THEN
            CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    END
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB/BMSFLIB
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

/.##プログラム異常終了##./
ABEND:

 /. ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM  ./

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'Ｄ３６５エラーリカバリデータ更新が　　'
    ?KEKA2 :=  '異常終了しました。　　　　　　　　　'
    ?KEKA3 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
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
