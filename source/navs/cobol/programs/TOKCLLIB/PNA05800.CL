/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   *  ./
/. *   JOB-ID      :    PNA05800                             *  ./
/. *   JOB-NAME    :    小売連携発注データ抽出・送信　　　　 *  ./
/. *                    （手書き）ＨＵＬＦＴ非連携　　　　　 *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾊﾟﾗﾒﾀ定義####./
    VAR  ?RENKBN ,STRING*01,VALUE-' '             /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS      ,STRING*08,VALUE-'        '      /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?WKSTN   ,NAME                            /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGMEC   ,INTEGER                         /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ ./
    VAR ?PGMECX  ,STRING*11                       /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ    ./
    VAR ?PGMEM   ,STRING*99                       /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ  ./
    VAR ?MSG     ,STRING*99(6)                    /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX    ,STRING*99                         /.SNDMSG表示用 ./
    VAR ?CLID    ,STRING*08,VALUE-'PNA05800'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./
   /. ↓2012.4.13 排他制御用 ./
    VAR ?OPR1    ,STRING*50                         /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2    ,STRING*50                         /.      2    ./
    VAR ?OPR3    ,STRING*50                         /.      3    ./
    VAR ?OPR4    ,STRING*50                         /.      4    ./
    VAR ?OPR5    ,STRING*50                         /.      5    ./
    VAR ?UMU     ,STRING*1,VALUE-' '                /.有無区分用 ./
    VAR ?PBUMON  ,STRING*04,VALUE-'    '            /.部門ＣＤ　 ./
    VAR ?PTANCD  ,STRING*02,VALUE-'  '              /.担当者ＣＤ ./
    VAR ?PWS     ,STRING*08,VALUE-'        '        /.WKSTN名    ./
    VAR ?PREN    ,STRING*09,VALUE-'000000000'       /.連携ＮＯ   ./
    VAR ?PONL    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ/手書 ./
   /. ↑2012.4.13 ./

   /. ↓2012.4.13 疎通確認用 ./
    VAR ?OKNG    ,STRING*2,VALUE-'  '               /.疎通結果　 ./
   /. ↑2012.4.13 ./

   /.##パラメタ（個別）##./
    VAR ?BUMON   ,STRING*04,VALUE-'    '            /.部門名       ./
    VAR ?TANCD6  ,STRING*06,VALUE-'      '          /.担当者CDWK   ./
    VAR ?TANCD   ,STRING*02,VALUE-'  '              /.担当者CD     ./
    VAR ?FILEID  ,STRING*08                         /.依頼ﾃﾞｰﾀID   ./
    VAR ?LIBID   ,STRING*08,VALUE-'TOKKLIB '        /.依頼ﾃﾞｰﾀLIB  ./
    VAR ?SIZE    ,STRING*05,VALUE-'05000'           /.依頼ﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?ESIZE   ,STRING*05,VALUE-'01000'           /.拡張ﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?FILE    ,NAME!MOD!COM                      /.CRTFILE用変数./
    VAR ?FILEN   ,NAME                              /.CRTFILE用変数./
    VAR ?LIBN    ,NAME                              /.CRTFILE用変数./
    VAR ?CRTSIZE ,INTEGER                           /.CRTFILE用変数./
    VAR ?EXSIZE  ,INTEGER                           /.CRTFILE用変数./

    VAR ?FILEIDS ,STRING*08                         /.SORTﾃﾞｰﾀID   ./
    VAR ?LIBIDS  ,STRING*08,VALUE-'TOKWLIB '        /.SORTﾃﾞｰﾀLIB  ./
    VAR ?SIZES   ,STRING*05,VALUE-'05000'           /.SORTﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?ESIZES  ,STRING*05,VALUE-'01000'           /.SORTﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?FILES   ,NAME!MOD!COM                      /.CRTFILE用変数./
    VAR ?FILENS  ,NAME                              /.CRTFILE用変数./
    VAR ?LIBNS   ,NAME                              /.CRTFILE用変数./
    VAR ?CRTSIZES,INTEGER                           /.CRTFILE用変数./
    VAR ?EXSIZES ,INTEGER                           /.CRTFILE用変数./

    VAR ?FILEIDR ,STRING*08                         /.SORTﾃﾞｰﾀID   ./
    VAR ?LIBIDR  ,STRING*08,VALUE-'TOKWLIB '        /.SORTﾃﾞｰﾀLIB  ./
    VAR ?SIZER   ,STRING*05,VALUE-'05000'           /.SORTﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?ESIZER  ,STRING*05,VALUE-'01000'           /.SORTﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?FILER   ,NAME!MOD!COM                      /.CRTFILE用変数./
    VAR ?FILENR  ,NAME                              /.CRTFILE用変数./
    VAR ?LIBNR   ,NAME                              /.CRTFILE用変数./
    VAR ?CRTSIZER,INTEGER                           /.CRTFILE用変数./
    VAR ?EXSIZER ,INTEGER                           /.CRTFILE用変数./

    VAR ?SNDHLFT1,STRING*05,VALUE-'     '           /.PGMES        ./
    VAR ?SNA0191B,STRING*05,VALUE-'     '           /.PGMES        ./
    VAR ?SNA0050B,STRING*05,VALUE-'     '           /.PGMES        ./
    VAR ?FILETXT ,STRING*17
    VAR ?IP      ,STRING*15,VALUE-'000.000.000.000' /.起動元ＩＰ   ./

   /.##パラメタ（Ａ）##./
    VAR ?PA01    ,STRING*06,VALUE-'000000'          /.部門・担当者 ./
    VAR ?PA02    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ手書種別./
    VAR ?PA03    ,STRING*20,VALUE-'00000000000000000000' /.ﾊﾞｯﾁ_ ./
    VAR ?PA04    ,STRING*02,VALUE-'00'              /.出荷場所     ./
    VAR ?PA05    ,STRING*08,VALUE-'00000000'        /.納品日       ./
    VAR ?PA06    ,STRING*08,VALUE-'00000000'        /.実納品日     ./
    VAR ?PA07    ,STRING*08,VALUE-'00000000'        /.取引先       ./
    VAR ?PA08    ,STRING*09,VALUE-'000000000'       /.伝票_1(FROM)./
    VAR ?PA09    ,STRING*09,VALUE-'000000000'       /.伝票_1(TO)  ./
    VAR ?PA10    ,STRING*09,VALUE-'000000000'       /.伝票_2(FROM)./
    VAR ?PA11    ,STRING*09,VALUE-'000000000'       /.伝票_2(TO)  ./
    VAR ?PA12    ,STRING*09,VALUE-'000000000'       /.伝票_3(FROM)./
    VAR ?PA13    ,STRING*09,VALUE-'000000000'       /.伝票_3(TO)  ./
    VAR ?PA14    ,STRING*09,VALUE-'000000000'       /.伝票_4(FROM)./
    VAR ?PA15    ,STRING*09,VALUE-'000000000'       /.伝票_4(TO)  ./
    VAR ?PA16    ,STRING*09,VALUE-'000000000'       /.伝票_5(FROM)./
    VAR ?PA17    ,STRING*09,VALUE-'000000000'       /.伝票_5(TO)  ./
    VAR ?PA18    ,STRING*01,VALUE-'0'               /.状態         ./
    VAR ?PA19    ,STRING*09,VALUE-'000000000'       /.連携_       ./
    VAR ?PA99    ,STRING*06,VALUE-'000000'          /.部門・担当者 ./

   /.##パラメタ（Ｂ）##./
    VAR ?PB01    ,STRING*06,VALUE-'000000'          /.部門・担当者 ./
    VAR ?PB02    ,STRING*01,VALUE-'1'               /.ｵﾝﾗｲﾝ手書種別./
    VAR ?PB03    ,STRING*09,VALUE-'000000000'       /.連携_(FROM) ./
    VAR ?PB04    ,STRING*09,VALUE-'000000000'       /.連携_(TO)   ./
    VAR ?PB05    ,STRING*15,VALUE-'000.000.000.000' /.起動元ＩＰ   ./

    VAR ?PA20    ,STRING*08,VALUE-'00000000'        /.管理番号　　 ./

    VAR ?STRENKBN,STRING*01,VALUE-' '               /.変換連携区分 ./

/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := '小売連携ＤＴ抽出・送信（手　書）非連携'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/HULOLIB/HULFLIB/HULWORK1
           /TOKDTLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.↓2012.4.13 ./
/.##実行確認##./
    ?OPR1  :=  '　＃　確認してください！　＃　'
    ?OPR2  :=  ''
    ?OPR3  :=  '　同じ取引先の伝票を、伝票入力等で開いていないか'
    ?OPR4  :=  '　確認してから、続行してください。'
    ?OPR5  :=  '　続行しますか？'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
/.↑2012.4.13 ./

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

/.##連携データ抽出指示（手書）##./
SNA0580I:

    ?STEP :=   'SNA0580I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKMDLIB
    OVRF      FILE-SHTDENLC,TOFILE-SHTDENLC.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TOKMS1,TOFILE-TOKMS1.TOKFLIB
    CALL      PGM-SNA0580I.TOKSOLIB,
              PARA-(?BUMON,?TANCD,
                    ?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
    /.##2013/02/07  ?PA17,?PA18,?PA19) ST ##./
    /.##2020/05/01  ?PA17,?PA18,?PA19,?PA20) ST ##./
                    ?PA17,?PA18,?PA19,?PA20,?RENKBN)
    /.##2013/02/07                     ED ##./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
              SNDMSG    ?MSGX,TO-XCTL
              RETURN
         ELSE
              ?KEKA4 :=  '抽出指示（手書）'
              GOTO ABEND
         END
    END
    ?MSGX := '# 部門＋担当者 = ' && ?PA01 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 1:手書/2:ｵﾝﾗｲﾝ= ' && ?PA02 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# バッチ_　　  = ' && ?PA03 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 出荷場所　　 = ' && ?PA04 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 納品日　　　 = ' && ?PA05 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 実納品日　　 = ' && ?PA06 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 取引先コード = ' && ?PA07 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票１(開始) = ' && ?PA08 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票１(終了) = ' && ?PA09 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票２(開始) = ' && ?PA10 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票２(終了) = ' && ?PA11 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票３(開始) = ' && ?PA12 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票３(終了) = ' && ?PA13 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票４(開始) = ' && ?PA14 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票４(終了) = ' && ?PA15 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票５(開始) = ' && ?PA16 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票５(終了) = ' && ?PA17 && ' #'
    SNDMSG    ?MSGX,TO-XCTL

/.##抽出ファイルＩＤ編集##./
EXXXXXXA:

               /.↓E:ｵﾝﾗｲﾝ､T:手書 ./
    ?FILEID  :=  'T' && ?BUMON && ?TANCD && 'A'
    ?FILEN   :=  %NAME(?FILEID)
    ?LIBN    :=  %NAME(?LIBID)
    ?CRTSIZE :=  %INTEGER(?SIZE)
    ?EXSIZE  :=  %INTEGER(?ESIZE)
    ?FILE    :=  %NCAT(?FILEN,?LIBN)
    ?FILETXT :=  %STRING(?FILE)
    ?MSGX :=  '* FILE-ID = ' && ?FILETXT && ' *'
    SNDMSG    ?MSGX,TO-XCTL

/.##抽出ファイル非存在時は作成##./
CRTFILE1:

    ?STEP :=   'CRTFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CRTFILE FILE-?FILE,SIZE-?CRTSIZE,ESIZE-?EXSIZE
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   0542 THEN
              SNDMSG MSG-'抽出先ファイル存在確認完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO CHGFILE1
         ELSE
              ?KEKA4 :=  '抽出先ファイル作成'
              GOTO ABEND
         END
    END
    SNDMSG MSG-'抽出先ファイル作成完了',TO-XCTL.@ORGPROF,JLOG-@YES

/.##抽出ファイル初期化##./
CHGFILE1:

    ?STEP :=   'CHGFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CHGFILE FILE-?FILE
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '抽出先ファイル初期化'
              GOTO ABEND
    ELSE
              SNDMSG MSG-'抽出先ファイル初期化完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##SORTファイルＩＤ編集##./
SXXXXXXA:

               /.↓S:SORT ./
    ?FILEIDS :=  'S' && ?BUMON && ?TANCD && 'A'
    ?FILENS  :=  %NAME(?FILEIDS)
    ?LIBNS   :=  %NAME(?LIBIDS)
    ?CRTSIZES:=  %INTEGER(?SIZES)
    ?EXSIZES :=  %INTEGER(?ESIZES)
    ?FILES   :=  %NCAT(?FILENS,?LIBNS)
    ?FILETXT :=  %STRING(?FILES)
    ?MSGX :=  '* FILE-ID = ' && ?FILETXT && ' *'
    SNDMSG    ?MSGX,TO-XCTL

/.##抽出ファイル非存在時は作成##./
CRTFILE2:

    ?STEP :=   'CRTFILE2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CRTFILE FILE-?FILES,SIZE-?CRTSIZES,ESIZE-?EXSIZES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   0542 THEN
              SNDMSG MSG-'ワークファイル存在確認完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO CHGFILE2
         ELSE
              ?KEKA4 :=  'ＳＯＲＴワークファイル作成'
              GOTO ABEND
         END
    END
    SNDMSG MSG-'ワークファイル作成完了',TO-XCTL.@ORGPROF,JLOG-@YES

/.##SORTファイル初期化##./
CHGFILE2:

    ?STEP :=   'CHGFILE2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CHGFILE FILE-?FILES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ワークファイル初期化'
              GOTO ABEND
    ELSE
              SNDMSG MSG-'ワークファイル初期化完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##抽出ファイルＩＤ編集##./
RXXXXXXA:

    ?FILEIDR :=  'R' && ?BUMON && ?TANCD && 'A'
    ?FILENR  :=  %NAME(?FILEIDR)
    ?LIBNR   :=  %NAME(?LIBIDR)
    ?CRTSIZER:=  %INTEGER(?SIZER)
    ?EXSIZER :=  %INTEGER(?ESIZER)
    ?FILER   :=  %NCAT(?FILENR,?LIBNR)
    ?FILETXT :=  %STRING(?FILER)
    ?MSGX :=  '* FILE-ID = ' && ?FILETXT && ' *'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##抽出ファイル非存在時は作成##./
CRTFILE3:

    ?STEP :=   'CRTFILE3'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CRTFILE FILE-?FILER,SIZE-?CRTSIZER,ESIZE-?EXSIZER
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   0542 THEN
              SNDMSG MSG-'連携ファイル存在確認完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO CHGFILE4
         ELSE
              ?KEKA4 :=  '連携ファイル作成'
              GOTO ABEND
         END
    END
    SNDMSG MSG-'ワークファイル作成完了',TO-XCTL.@ORGPROF,JLOG-@YES

/.##抽出ファイル初期化##./
CHGFILE4:

    ?STEP :=   'CHGFILE4'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CHGFILE FILE-?FILER
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '抽出ファイル初期化'
              GOTO ABEND
    ELSE
              SNDMSG MSG-'抽出ファイル初期化完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##連携データ抽出（手書）##./
SNA0600B:

    ?STEP :=   'SNA0600B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLC,TOFILE-SHTDENLC.TOKFLIB
    OVRF      FILE-NARKANL1,TOFILE-NARKANL1.TOKKLIB
    OVRF      FILE-NARRNOL1,TOFILE-NARRNOL1.TOKKLIB
    OVRF      FILE-TXXXXXXA,TOFILE-?FILE
    CALL      PGM-SNA0600B.TOKSOLIB
             ,PARA-(?IP,
                    ?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
    /.##2013/02/07  ?PA17,?PA18,?PA19, ST ##./
                    ?PA17,?PA18,?PA19,?PA20,
    /.##2013/02/07                     ED ##./
    /.##2020/05/01  ?PB01,?PB02,?PB03,?PB04,?PB05)  ST ##./
                    ?PB01,?PB02,?PB03,?PB04,?PB05,?RENKBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC     =  4001  THEN
              SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
              SNDMSG '＊　この指定条件では　＊',TO-XCTL
              SNDMSG '＊　抽出対象データが　＊',TO-XCTL
              SNDMSG '＊　ありません！　　　＊',TO-XCTL
              SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
              RETURN    PGMEC-@PGMEC
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '連携データ抽出（手書）'
              GOTO ABEND
    END

SORT0001:

    ?STEP :=   'SORT0001'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTD  INFILE-?FILE,
           INRL-300,INBF-1,
           OUTFILE-?FILER,
           OUTRL-300,OUTBF-1,
           WORKFILE-?FILES,
           SEQ-@YES,
           KEY-1!9!CA,         /. 連携_         ./
           KEY1-52!8!DA,       /. 注文日         ./
           KEY2-84!9!DA,       /. 自社伝票_     ./
           KEY3-128!2!DA,      /. 伝票行_       ./
           RCDL-@DSP
        /.*SELECT-'269!2!D!NE!L-1,181!1!C!NE!L-5'*./

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=  0 THEN
              ?KEKA4 :=  '小売連携発注データＳＯＲＴ'
              GOTO ABEND
    END

/.##連携データ出力##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  FIMPORT FILE-?FILER,PARA-NAS,UNIT-2
./  ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=  0 THEN
              ?KEKA4 :=  '連携データ出力'
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

/.##(ONL手書共通)先行受信連携_連携ＤＴ＝＞受注ＤＴ作成##./
SNA0500B:

    ?STEP :=   'SNA0500B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##IF  ?RENKBN = '1'  THEN
        ?STRENKBN :=  '2'
    ELSE
        IF  ?RENKBN = '2'  THEN
            ?STRENKBN :=  '3'
        END
    END   ##./
/.  ?STRENKBN :=  ?RENKBN
./
    OVRF      FILE-EXXFIL,TOFILE-?FILE
    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-SUBTBLL1,TOFILE-SUBTBLL1.TOKDTLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-SNDNJYTF,TOFILE-SNDNJYTF.D365DLIB
    CALL      PGM-SNA0500B.TOKSOLIB,PARA-(?STRENKBN)
    ?SNA0050B :=    @PGMES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?SNA0050B ^=   'S0000' THEN
         IF      ?SNA0050B  =   'S4010' THEN
                ?MSGX :=  '#####################################'
                SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                ?MSGX :=  '# 顧客需要家ＩＤマスタ未登録が発生] #'
                SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                ?MSGX :=  '# マスタ登録後、再処理が必要です  ] #'
                SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                ?MSGX :=  '# 確認してください]]]]]]]]]]]]]]]]] #'
                SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                ?MSGX :=  '#####################################'
                SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                GOTO  ABEND

         ELSE
                GOTO  ABEND
         END
    ELSE
         SNDMSG MSG-'連携データ累積処理　　　　　完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
    END
    OVRF      FILE-SNDXJYTF,TOFILE-SNDXJYTF.D365DLIB
    CALL      PGM-CNVJYTP1.TOKSOLIB
    CNVDF FILE-SNDXJYTF.D365DLIB,PATH-'/NAVPC/SNDJTKF.CSV',
          MODE-@EXP,ADD-@NO,ACTCHK-@NO,EXTCNV-@YES

/.##連携データ送信 完了処理##./
SNA0191B:

    ?STEP :=   'SNA0191B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-PRTFILE,TOFILE-PRTF.TOKELIB
    OVRF      FILE-NARKANL1,TOFILE-NARKANL1.TOKKLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SNA0191B.TOKSOLIB
             ,PARA-(?PB01,?PB02,?PB03,?PB04,?PB05)
    ?SNA0191B :=    @PGMES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?SNA0191B ^=   'S0000' THEN
         GOTO ABEND
    ELSE
         SNDMSG MSG-'連携データ送信 完了処理　　完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##正常終了処理##./
RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '連携発注データの抽出処理を行いました。'
    ?KEKA2 :=  'ＮＡＳシステムを起動し、抽出したデータ'
    ?KEKA3 :=  'の取込処理を実施して下さい。　　　　　'
    ?KEKA4 :=  '※忘れず実施願います！！　　　　　　　'
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
