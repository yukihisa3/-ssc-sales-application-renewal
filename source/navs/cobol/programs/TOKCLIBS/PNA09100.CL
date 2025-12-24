/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   *  ./
/. *   JOB-ID      :    PNA09100                             *  ./
/. *   JOB-NAME    :    小売連携発注データ再送信　　　　　　 *  ./
/. *                    （手書）　　　　　　　　　　　　　　 *  ./
/. *   UPDATE      :    HULFT/排他制御 2012.4.13             *  ./
/. *   UPDATE      :    HULFT/疎通確認 2012.4.13             *  ./
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
    VAR ?CLID    ,STRING*08,VALUE-'PNA09100'        /.CLID         ./
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
  /.VAR ?SIZE    ,STRING*05,VALUE-'05000' ./        /.依頼ﾃﾞｰﾀｻｲｽﾞ ./
  /.VAR ?ESIZE   ,STRING*05,VALUE-'01000' ./        /.拡張ﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?FILE    ,NAME!MOD!COM                      /.CRTFILE用変数./
    VAR ?FILEN   ,NAME                              /.CRTFILE用変数./
    VAR ?LIBN    ,NAME                              /.CRTFILE用変数./
  /.VAR ?CRTSIZE ,INTEGER ./                        /.CRTFILE用変数./
  /.VAR ?EXSIZE  ,INTEGER ./                        /.CRTFILE用変数./

    VAR ?FILEIDS ,STRING*08                         /.SORTﾃﾞｰﾀID   ./
    VAR ?LIBIDS  ,STRING*08,VALUE-'TOKWLIB '        /.SORTﾃﾞｰﾀLIB  ./
    VAR ?SIZES   ,STRING*05,VALUE-'05000'           /.SORTﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?ESIZES  ,STRING*05,VALUE-'01000'           /.SORTﾃﾞｰﾀｻｲｽﾞ ./
    VAR ?FILES   ,NAME!MOD!COM                      /.CRTFILE用変数./
    VAR ?FILENS  ,NAME                              /.CRTFILE用変数./
    VAR ?LIBNS   ,NAME                              /.CRTFILE用変数./
    VAR ?CRTSIZES,INTEGER                           /.CRTFILE用変数./
    VAR ?EXSIZES ,INTEGER                           /.CRTFILE用変数./

    VAR ?SNDHLFT1,STRING*05,VALUE-'     '           /.PGMES        ./
    VAR ?SNA0190B,STRING*05,VALUE-'     '           /.PGMES        ./
    VAR ?SNA0050B,STRING*05,VALUE-'     '           /.PGMES        ./
    VAR ?FILETXT  ,STRING*17
    VAR ?IP      ,STRING*15,VALUE-'000.000.000.000' /.起動元ＩＰ   ./
    VAR ?MSG1    ,STRING*74,VALUE-''            /.連携_指定ﾒｯｾｰｼﾞ ./
    VAR ?MSG2    ,STRING*74,VALUE-''
    VAR ?MSG3    ,STRING*74,VALUE-''
    VAR ?MSG4    ,STRING*74,VALUE-''
    VAR ?MSG5    ,STRING*74,VALUE-''
    VAR ?MSG1A,STRING*38,VALUE-'・小売連携発注データ（手書き）の再送'
    VAR ?MSG1B,STRING*38,VALUE-'信を行います。　　　　　　　　　　　'

    VAR ?MSG2A,STRING*38,VALUE-'　このログインユーザでの、直前に送信'
    VAR ?MSG2B,STRING*38,VALUE-'失敗したデータを送信し直します。　　'

    VAR ?MSG3A,STRING*38,VALUE-'・ログインしているユーザに間違いない'
    VAR ?MSG3B,STRING*38,VALUE-'か確認して下さい。　　　　　　　　　'

    VAR ?MSG4A,STRING*38,VALUE-'　間違いなければ、再送信する連携_を'
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
    ?CLNM := '小売連携発注データ再送信（手書）'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/HULOLIB/HULFLIB/HULWORK1
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

/. ↓2012.4.13 ./
/.##排他制御チェック##./
SNA0260B:

    ?STEP :=   'SNA0260B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NARLOCF,TOFILE-NARLOCF.TOKKLIB
    CALL      PGM-SNA0260B.TOKELIBO,
                  PARA-(?UMU,?PBUMON,?PTANCD,?PWS,?PREN,?PONL)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '排他制御チェック'
              GOTO ABEND
    END
  /.SNDMSG    'UMU=',TO-XCTL
    SNDMSG    ?UMU,TO-XCTL
    SNDMSG    'BUMON=',TO-XCTL
    SNDMSG    ?BUMON,TO-XCTL
    SNDMSG    'TANCD=',TO-XCTL
    SNDMSG    ?TANCD,TO-XCTL
    SNDMSG    'WS=',TO-XCTL
    SNDMSG    ?WS,TO-XCTL
    SNDMSG    'PB03=',TO-XCTL
    SNDMSG    ?PB03,TO-XCTL
    SNDMSG    'PB02=',TO-XCTL
    SNDMSG    ?PB02,TO-XCTL ./

    IF        ?UMU       =   '0'    THEN
              GOTO ASSIGN
    ELSE
              SNDMSG '！！！！！！！！！！！！！！',TO-XCTL
              SNDMSG '！　　　　　　　　　　　　！',TO-XCTL
              SNDMSG '！　他で連携処理中です　　！',TO-XCTL
              SNDMSG '！　しばらくお待ち下さい　！',TO-XCTL
              SNDMSG '！　　　　　　　　　　　　！',TO-XCTL
              SNDMSG '！！！！！！！！！！！！！！',TO-XCTL
              RETURN
    END
/. ↑2012.4.13 ./

/. ↓2012.4.13 ./
/.##排他制御ファイルロック##./
ASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-NARLOCF.TOKKLIB!@XCLR
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '排他制御ファイルロック'
              GOTO ABEND
    END
/. ↑2012.4.13 ./

/. ↓2012.4.13 ./
/.##排他制御処理##./
SNA0270B:

    ?STEP :=   'SNA0270B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NARLOCF,TOFILE-NARLOCF.TOKKLIB
    CALL      PGM-SNA0270B.TOKELIBO,
                  PARA-(?BUMON,?TANCD,?WS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '排他制御処理'
              GOTO ABEND
    END

    SETPF FILE-NARLOCF.TOKKLIB,TOFILE-NARLOCF2.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '排他制御処理ＳＥＴＰＦ'
              GOTO ABEND
    END

    RELEASE   NARLOCF.TOKKLIB!@XCLR

/. ↑2012.4.13 ./

/. ↓2012.4.13 ./
/.##ダミーデータ送信要求##./
SNDDUMMY:

    ?STEP :=   'SNDDUMMY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-STRSND.HULOLIB,PARA-(FILE-HGNDUMYH)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ダミーデータ送信要求'
              GOTO ABEND
    ELSE
              SNDMSG MSG-'ダミーデータ送信要求完了',
                                        TO-XCTL,JLOG-@YES
    END
/. ↑2012.4.13 ./

/. ↓2012.4.13 ./
/.##疎通監視処理##./
SNA0300B:

    ?STEP :=   'SNA0300B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

 /. TEST
     ?OKNG := 'OK' ./

    CALL      PGM-SNA0300B.TOKELIBO,PARA-(?OKNG)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '疎通監視処理'
              GOTO ABEND
    END

    IF        ?OKNG     =  'OK'  THEN
              SNDMSG MSG-'疎通確認ＯＫ',TO-XCTL,JLOG-@YES
              GOTO TXXXXXXA
    ELSE
              SNDMSG MSG-'疎通確認ＮＧ',TO-XCTL,JLOG-@YES
              SNDMSG MSG-'！！！！！！！！！！！！！！！！',
                                        TO-XCTL,JLOG-@YES
              SNDMSG MSG-'！　苗業務システムとの伝送が　！',
                                        TO-XCTL,JLOG-@YES
              SNDMSG MSG-'！　できない状態です　　　　　！',
                                        TO-XCTL,JLOG-@YES
              SNDMSG MSG-'！　確認してください　　　　　！',
                                        TO-XCTL,JLOG-@YES
              SNDMSG MSG-'！！！！！！！！！！！！！！！！',
                                        TO-XCTL,JLOG-@YES
              ?KEKA4 :=  '疎通監視処理　ＮＧ'
              GOTO ABEND
    END
/. ↑2012.4.13 ./

/.##送信ファイルＩＤ特定##./
TXXXXXXA:

               /.↓E:ｵﾝﾗｲﾝ､T:手書 ./
    ?FILEID  :=  'T' && ?BUMON && ?TANCD && 'A'
    ?FILEN   :=  %NAME(?FILEID)
    ?LIBN    :=  %NAME(?LIBID)
  /.?CRTSIZE :=  %INTEGER(?SIZE)
    ?EXSIZE  :=  %INTEGER(?ESIZE)  ./
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
/. ↓2012.4.12 ./
              CLRFILE NARLOCF.TOKKLIB
              CLRFILE NARLOCF2.TOKKLIB
/. ↑2012.4.12 ./
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
              GOTO  SXXXXXXA
    END
    IF        ?PGMEC     =  4002  THEN
              GOTO  ABEND
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '連携_存在チェック'
              GOTO ABEND
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
/.##SORT##./
SORT0001:

    ?STEP :=   'SORT0001'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTD  INFILE-?FILE,
           INRL-300,INBF-1,
           OUTFILE-HGN001H.TOKKLIB,
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

/. ↓2012.4.12 ./
/.##排他制御ファイル更新##./
SNA0280B:

    ?STEP :=   'SNA0280B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NARLOCF,TOFILE-NARLOCF.TOKKLIB
    CALL      PGM-SNA0280B.TOKELIBO,PARA-(?PB03,?PB02)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '排他制御ファイル更新'
              GOTO ABEND
    END
/. ↑2012.4.12 ./

/.##連携データ送信要求（手書き）##./
SNDHLFT1:

    ?STEP :=   'SNDHLFT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /. ↓2012.4.13 ./
    CALL      PGM-STRSND.HULOLIB,PARA-(FILE-HGN001H)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '連携データ送信要求（手書き）'
              GOTO ABEND
    ELSE
              SNDMSG MSG-'連携データ送信要求（手書き）完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              SNDMSG MSG-'結果が通知されるまでお待ちください。',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              SNDMSG MSG-'通知されるまでは次の連携はできません',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
    END
    /. ↑2012.4.13 ./

    /. ↓2012.4.13 ---->
    ?SNDHLFT1 :=    @PGMES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?SNDHLFT1 ^=  'S0000' THEN
              SNDMSG MSG-'連携データ送信（手書）失敗！！',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              ?KEKA4 :=  '連携データ送信（手書）'
              GOTO ABEND
    ELSE
              SNDMSG MSG-'連携データ送信（手書）完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO SNA0190B
    END
      <--------- ↑2012.4.13 ./

/. ↓2012.4.13 以下処理は、送信後ジョブとして別ＣＬにする./
/.--------------------->
  ##連携データ送信 完了処理##
SNA0190B:

    ?STEP :=   'SNA0190B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRPRTF   FILE-PRTFILE,TOFILE-PRTF.TOKELIB
    OVRF      FILE-NARKANL1,TOFILE-NARKANL1.TOKKLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SNA0190B.TOKELIBO
             ,PARA-(?PB01,?PB02,?PB03,?PB04,?PB05)
    ?SNA0190B :=    @PGMES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?SNA0190B ^=  'S0000' THEN
              SNDMSG MSG-'連携データ送信　完了処理　　失敗！！',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              ?KEKA4 :=  '連携データ送信 完了処理'
              GOTO ABEND
    ELSE
              SNDMSG MSG-'連携データ送信完了処理　　　完了',
                                        TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
    END
   <------------------------------- ./

/.##正常終了処理##./
RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
  /. ↓ 2012.4.13 ./
  /.?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  '' ./
    ?KEKA1 :=  '連携発注データの送信依頼を行いました。'
    ?KEKA2 :=  '結果が通知されるまでお待ちください。　'
    ?KEKA3 :=  ''
    ?KEKA4 :=  '通知されるまでは、次の連携はできません'
  /. ↑ 2012.4.13 ./
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC
/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

    /. ↓2012.4.13 送信完了前の異常終了時は排他解除./
    CLRFILE   FILE-NARLOCF.TOKKLIB
    CLRFILE   FILE-NARLOCF2.TOKKLIB
    RELEASE   NARLOCF.TOKKLIB!@XCLR
    /. ↑2012.4.13 ./

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