/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   *  ./
/. *   JOB-ID      :    PNA09400                             *  ./
/. *   JOB-NAME    :    小売連携発注データ再累積　　　　　　 *  ./
/. *                    （手書）　　　　　　　　　　　　　　 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS      ,STRING*08,VALUE-'        '      /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?WKSTN   ,NAME                            /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGMEC   ,INTEGER                         /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ ./
    VAR ?PGMECX  ,STRING*11                       /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ    ./
    VAR ?PGMEM   ,STRING*99                       /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ  ./
    VAR ?MSG     ,STRING*99(6)                    /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX    ,STRING*99                         /.SNDMSG表示用 ./
    VAR ?CLID    ,STRING*08,VALUE-'PNA09400'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./

   /.##パラメタ（個別）##./
    VAR ?BUMON   ,STRING*04,VALUE-'    '            /.部門名       ./
    VAR ?TANCD6  ,STRING*06,VALUE-'      '          /.担当者CDWK   ./
    VAR ?TANCD   ,STRING*02,VALUE-'  '              /.担当者CD     ./
    VAR ?FILEID  ,STRING*08                         /.依頼ﾃﾞｰﾀID   ./
    VAR ?LIBID   ,STRING*08,VALUE-'TOKKLIB '        /.依頼ﾃﾞｰﾀLIB  ./
    VAR ?FILE    ,NAME!MOD!COM                      /.CRTFILE用変数./
    VAR ?FILEN   ,NAME                              /.CRTFILE用変数./
    VAR ?LIBN    ,NAME                              /.CRTFILE用変数./
    VAR ?SNA0050B,STRING*05,VALUE-'     '           /.PGMES        ./
    VAR ?FILETXT  ,STRING*17
    VAR ?MSG1    ,STRING*74,VALUE-''            /.連携_指定ﾒｯｾｰｼﾞ ./
    VAR ?MSG2    ,STRING*74,VALUE-''
    VAR ?MSG3    ,STRING*74,VALUE-''
    VAR ?MSG4    ,STRING*74,VALUE-''
    VAR ?MSG5    ,STRING*74,VALUE-''
    VAR ?MSG1A,STRING*38,VALUE-'・小売連携発注データ（手書き）の再累'
    VAR ?MSG1B,STRING*38,VALUE-'積を行います。　　　　　　　　　　　'

    VAR ?MSG2A,STRING*38,VALUE-'　このログインユーザでの、直前に累積'
    VAR ?MSG2B,STRING*38,VALUE-'失敗したデータを累積し直します。　　'

    VAR ?MSG3A,STRING*38,VALUE-'・ログインしているユーザに間違いない'
    VAR ?MSG3B,STRING*38,VALUE-'か確認して下さい。　　　　　　　　　'

    VAR ?MSG4A,STRING*38,VALUE-'　間違いなければ、再累積する連携_を'
    VAR ?MSG4B,STRING*38,VALUE-'指定して下さい。　　　　　　　　　　'

    VAR ?MSG5A,STRING*38,VALUE-'注）開始・終了は同一_を指定して下さ'
    VAR ?MSG5B,STRING*38,VALUE-'い！　（終了：空打ち）　　　　　　　'

   /.##パラメタ（Ａ）##./

   /.##パラメタ（Ｂ）##./
    VAR ?PB01    ,STRING*06,VALUE-'000000'          /.部門・担当者 ./
    VAR ?PB02    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ手書種別./
    VAR ?PB03    ,STRING*09,VALUE-'000000000'       /.連携_(FROM) ./
    VAR ?PB04    ,STRING*09,VALUE-'000000000'       /.連携_(TO)   ./
    VAR ?PB05    ,STRING*15,VALUE-'000.000.000.000' /.起動元ＩＰ   ./

/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := '小売連携発注データ再累積（手書き）'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
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
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD6)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD6,1,2)

/.##送信ファイルＩＤ特定##./
EXXXXXXA:

               /.↓E:ｵﾝﾗｲﾝ､T:手書 ./
    ?FILEID  :=  'T' && ?BUMON && ?TANCD && 'A'
    ?FILEN   :=  %NAME(?FILEID)
    ?LIBN    :=  %NAME(?LIBID)
    ?FILE    :=  %NCAT(?FILEN,?LIBN)
    ?FILETXT :=  %STRING(?FILE)
    ?MSGX :=  '* FILE-ID = ' && ?FILETXT && ' *'
    SNDMSG    ?MSGX,TO-XCTL

/.##連携_指定（共通）##./
SNA0090I:

    ?STEP :=   'SNA0090I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSG1  := %SBSTR(?MSG1A,2,36) && %SBSTR(?MSG1B,2,36)
    ?MSG2  := %SBSTR(?MSG2A,2,36) && %SBSTR(?MSG2B,2,36)
    ?MSG3  := %SBSTR(?MSG3A,2,36) && %SBSTR(?MSG3B,2,36)
    ?MSG4  := %SBSTR(?MSG4A,2,36) && %SBSTR(?MSG4B,2,36)
    ?MSG5  := %SBSTR(?MSG5A,2,36) && %SBSTR(?MSG5B,2,36)

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIBO
    OVRF      FILE-NARKANL1,TOFILE-NARKANL1.TOKKLIB
    CALL      PGM-SNA0090I.TOKELIBO,
              PARA-(?BUMON,?TANCD,
                    ?MSG1,?MSG2,?MSG3,?MSG4,?MSG5,
                    ?PB01,?PB02,?PB03,?PB04,?PB05)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
              SNDMSG    ?MSGX,TO-XCTL
              RETURN
         ELSE
              ?KEKA4 :=  '連携_指定（共通）'
              GOTO ABEND
         END
    END
    ?MSGX := '# 1:手書/2:ｵﾝﾗｲﾝ　 = ' && ?PB02 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 指定連携_(開始) = ' && ?PB03 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 指定連携_(終了) = ' && ?PB04 && ' #'
    SNDMSG    ?MSGX,TO-XCTL

/.##連携_存在チェック##./
SNA0200B:

    ?STEP :=   'SNA0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-EXXFIL,TOFILE-?FILE
    CALL      PGM-SNA0200B.TOKELIBO
             ,PARA-(?PB01,?PB02,?PB03,?PB04,?PB05)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC     =  4001  THEN
              GOTO  SNA0050B
    END
    IF        ?PGMEC     =  4002  THEN
              GOTO  ABEND
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '連携_存在チェック'
              GOTO ABEND
    END

/.##連携データ累積処理##./
SNA0050B:

    ?STEP :=   'SNA0050B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-EXXFIL,TOFILE-?FILE
    OVRF      FILE-NARRUIL2,TOFILE-NARRUIL2.TOKKLIB
    CALL      PGM-SNA0050B.TOKELIBO
    ?SNA0050B :=    @PGMES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?SNA0050B ^=   'S0000' THEN
         ?KEKA4 :=  '連携データ累積処理'
         GOTO ABEND
    ELSE
         SNDMSG MSG-'連携データ累積処理　　　　　完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##正常終了処理##./
RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC
/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?CLID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
