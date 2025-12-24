/. ***********************************************************  ./
/. *   サカタのタネ殿　本社システム　　　  　　　　　        *  ./
/. *   SYSTEM-NAME :   ＨＧ基幹システム　　　　　　　　　    *  ./
/. *   JOB-ID      :   PBT03000                              *  ./
/. *   JOB-NAME    :   ＬＩＮＫＳ出荷連携データ抽出VER2　　  *  ./
/. *                    （オンライン／手書）　　　　　　　　 *  ./
/. *                    ※栃木倉庫　　　　　　　　　　　　　 *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS      ,STRING*08,VALUE-'        '      /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?BUMON   ,STRING*04,VALUE-'    '          /.部門名       ./
    VAR ?TANCD6  ,STRING*06,VALUE-'      '        /.担当者CDWK   ./
    VAR ?TANCD   ,STRING*02,VALUE-'  '            /.担当者CD     ./
    VAR ?SOUKO   ,STRING*02,VALUE-'  '            /.倉庫コード     ./
    VAR ?DSOUKO  ,STRING*02,VALUE-'  '            /.代表倉庫コード ./
    VAR ?WKSTN   ,NAME                            /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGMEC   ,INTEGER                         /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ ./
    VAR ?PGMECX  ,STRING*11                       /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ    ./
    VAR ?PGMEM   ,STRING*99                       /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ  ./
    VAR ?MSG     ,STRING*99(6)                    /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX    ,STRING*99                         /.SNDMSG表示用 ./
    VAR ?CLID    ,STRING*08,VALUE-'PBT03000'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./
    VAR ?OPR1    ,STRING*50                         /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2    ,STRING*50                         /.      2    ./
    VAR ?OPR3    ,STRING*50                         /.      3    ./
    VAR ?OPR4    ,STRING*50                         /.      4    ./
    VAR ?OPR5    ,STRING*50                         /.      5    ./
    VAR ?ERRCD   ,STRING*1                          /.ｴﾗｰCD      ./

   /.##パラメタ（個別）##./
    /.抽出件数情報./
    VAR ?SYURUIA ,STRING*01,VALUE-' '         /.抽出種　出A(ONL)./
    VAR ?KENSUUA ,STRING*08,VALUE-'00000000'  /.　件数　出A(ONL)./
    VAR ?SYURUIB ,STRING*01,VALUE-' '         /.抽出種　出B(ｼﾞｭﾝ)./
    VAR ?KENSUUB ,STRING*08,VALUE-'00000000'  /.　件数　出B(ｼﾞｭﾝ)./
    VAR ?SYURUIC ,STRING*01,VALUE-' '         /.抽出種　出C(手書)./
    VAR ?KENSUUC ,STRING*08,VALUE-'00000000'  /.　件数　出C(手書)./
    VAR ?SYURUID ,STRING*01,VALUE-' '         /.抽出種　出D(入出)./
    VAR ?KENSUUD ,STRING*08,VALUE-'00000000'  /.　件数　出D(入出)./
    VAR ?SYURUIE ,STRING*01,VALUE-' '         /.抽出種　出E(作業)./
    VAR ?KENSUUE ,STRING*08,VALUE-'00000000'  /.　件数　出E(作業)./
    VAR ?SYURUIF ,STRING*01,VALUE-' '         /.抽出種　出F(振替)./
    VAR ?KENSUUF ,STRING*08,VALUE-'00000000'  /.　件数　出F(振替)./
    VAR ?SYURUIG ,STRING*01,VALUE-' '         /.抽出種　入G(入荷)./
    VAR ?KENSUUG ,STRING*08,VALUE-'00000000'  /.　件数　入G(入荷)./
    VAR ?SYURUIH ,STRING*01,VALUE-' '         /.抽出種　入H(入出)./
    VAR ?KENSUUH ,STRING*08,VALUE-'00000000'  /.　件数　入H(入出)./
    VAR ?SYURUII ,STRING*01,VALUE-' '         /.抽出種　入I(作業)./
    VAR ?KENSUUI ,STRING*08,VALUE-'00000000'  /.　件数　入I(作業)./
    VAR ?SYURUIJ ,STRING*01,VALUE-' '         /.抽出種　入J(振替)./
    VAR ?KENSUUJ ,STRING*08,VALUE-'00000000'  /.　件数　入J(振替)./
    VAR ?SYURUIK ,STRING*01,VALUE-' '         /.抽出種　出K(カＯ)./
    VAR ?KENSUUK ,STRING*08,VALUE-'00000000'  /.　件数　出K(カＯ)./
    VAR ?SYURUIL ,STRING*01,VALUE-' '         /.抽出種　出L(カ手)./
    VAR ?KENSUUL ,STRING*08,VALUE-'00000000'  /.　件数　出L(カ手)./
    VAR ?SYURUIZ ,STRING*01,VALUE-' '         /.抽出種　DUMMY./
    VAR ?KENSUUZ ,STRING*08,VALUE-'00000000'  /.　件数　DUMMY./

    VAR ?CYOUHYOU,STRING*01,VALUE-'S'         /.帳票区分　　　　 ./

   /.##パラメタ（出荷）##./
    VAR ?PA01    ,STRING*04,VALUE-'0000'       /.部門ＣＤ ./
    VAR ?PA02    ,STRING*02,VALUE-'00'         /.担当者ＣＤ ./
    VAR ?PA03    ,STRING*01,VALUE-' '          /.送信区分 ./
    VAR ?PA04    ,STRING*01,VALUE-' '          /.抽出区分-ONL ./
    VAR ?PA05    ,STRING*01,VALUE-' '          /.抽出区分-手書./
    VAR ?PA06    ,STRING*01,VALUE-' '          /.抽出区分-横持./
    VAR ?PA07    ,STRING*02,VALUE-'  '         /.抽出倉庫ＣＤ ./
    VAR ?PA08    ,STRING*08,VALUE-'00000000'   /.バ-受信日-ONL./
    VAR ?PA09    ,STRING*04,VALUE-'0000'       /.バ-受信時-ONL./
    VAR ?PA10    ,STRING*08,VALUE-'00000000'   /.バ-取引先-ONL./
    VAR ?PA11    ,STRING*08,VALUE-'00000000'   /.納品日-FROM-ONL./
    VAR ?PA12    ,STRING*08,VALUE-'00000000'   /.納品日-TO-ONL./
    VAR ?PA13    ,STRING*05,VALUE-'00000'      /.店舗-FROM-ONL./
    VAR ?PA14    ,STRING*05,VALUE-'00000'      /.店舗-TO-ONL./
    VAR ?PA15    ,STRING*08,VALUE-'00000000'   /.取引先-手書./
    VAR ?PA16    ,STRING*09,VALUE-'000000000'  /.伝票-FROM-手書./
    VAR ?PA17    ,STRING*09,VALUE-'000000000'  /.伝票-TO-手書./
    VAR ?PA18    ,STRING*08,VALUE-'00000000'   /.納品日-FROM-手書./
    VAR ?PA19    ,STRING*08,VALUE-'00000000'   /.納品日-TO-手書./
    VAR ?PA20    ,STRING*08,VALUE-'00000000'   /.横持日-横持ち./

   /.##パラメタ（入荷）##./
    VAR ?PB01    ,STRING*04,VALUE-'0000'       /.部門ＣＤ ./
    VAR ?PB02    ,STRING*02,VALUE-'00'         /.担当者ＣＤ ./
    VAR ?PB03    ,STRING*01,VALUE-' '          /.送信区分 ./
    VAR ?PB04    ,STRING*01,VALUE-' '          /.抽出区分-入荷./
    VAR ?PB05    ,STRING*01,VALUE-' '          /.抽出区分-横持./
    VAR ?PB06    ,STRING*02,VALUE-'  '         /.抽出倉庫ＣＤ ./
    VAR ?PB07    ,STRING*08,VALUE-'00000000'   /.連携日　　　 ./

   /.##パラメタ（エラー判定用）##./
    VAR ?ERR     ,STRING*01,VALUE-' '

   /.##倉庫別ファイルＩＤ編集用##./
    VAR ?FILCNZ   ,STRING*6,VALUE-'CNZSYK'     /.カインズ./
    VAR ?FILETC   ,STRING*6,VALUE-'LNKSYK'     /.カインズ以外./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?LIBNMC   ,STRING*8,VALUE-'BMSFLIB '
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKKLIB '
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?LNKSYKF  ,NAME!MOD
    VAR ?LNKSYKX  ,STRING*17

    VAR ?FILCNZW  ,STRING*6,VALUE-'CNZSYW'     /.カインズワーク./
    VAR ?FILNMW   ,STRING*8,VALUE-'        '
    VAR ?LIBNMW   ,STRING*8,VALUE-'BMSFLIB '
    VAR ?FILIDW   ,NAME
    VAR ?LIBIDW   ,NAME
    VAR ?CNZSYWF  ,NAME!MOD
    VAR ?CNZSYWX  ,STRING*17

/.==================================================================./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := 'ＬＩＮＫＳ出荷連携データ抽出　　　　　'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/BMSFLIB/LINKSLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.==================================================================./

/.##実行確認##./
/.  ?OPR1  :=  ''
    ?OPR2  :=  '　ＬＩＮＫＳシステムへの'
    ?OPR3  :=  '　出荷連携データ抽出を行います。'
    ?OPR4  :=  ''
    ?OPR5  :=  '　続行しますか？'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)./

/.==================================================================./

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

/.==================================================================./

/.##倉庫コード取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,
                  PARA-(?WS,?SOUKO,?DSOUKO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '倉庫コード取得'
              GOTO ABEND
    END
  /.SNDMSG    'WS=',TO-XCTL
    SNDMSG    ?WS,TO-XCTL
    SNDMSG    'SOUKO=',TO-XCTL
    SNDMSG    ?SOUKO,TO-XCTL
    SNDMSG    'DSOUKO=',TO-XCTL
    SNDMSG    ?DSOUKO,TO-XCTL   ./

/.==================================================================./

/.##確認画面##./
SHOM0900:

    ?STEP :=   'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '＃＃＃Ｕ　Ｓ　Ｂ　接　続　確　認＃＃＃'
    ?OPR2  :=  '　ＵＳＢを接続して下さい。　　　　　　'
    ?OPR3  :=  '　ＵＳＢ接続後、ＥＮＴＥＲを押して　　'
    ?OPR4  :=  '　下さい。　'
    ?OPR5  :=  '※必ず、確認実施お願い致します。'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '確認画面'
              GOTO ABEND
    END

/.##出荷連携データ抽出指示##./
SBT0010I:

    ?STEP :=   'SBT0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIBO
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBT0010I.TOKELIBO,
              PARA-(?BUMON,?TANCD,?SOUKO,?DSOUKO,
                    ?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
              SNDMSG    ?MSGX,TO-XCTL
              RETURN
         ELSE
              ?KEKA4 :=  '出荷連携データ抽出指示'
              GOTO ABEND
         END
    END
    ?MSGX := '# 部門ＣＤ     = ' && ?PA01 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 担当者ＣＤ    = ' && ?PA02 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 送信区分　　  = ' && ?PA03 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分(オ) = ' && ?PA04 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分(手) = ' && ?PA05 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分(横) = ' && ?PA06 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出倉庫ＣＤ = ' && ?PA07 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 受信日（オ） = ' && ?PA08 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 受信時刻（オ）= ' && ?PA09 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 取引先（オ） = ' && ?PA10 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 納品日FROM(オ)= ' && ?PA11 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 納品日TO  (オ)= ' && ?PA12 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 店舗FROM(オ)　= ' && ?PA13 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 店舗TO  (オ)　= ' && ?PA14 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 取引先  (手)　= ' && ?PA15 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票FROM(手)　= ' && ?PA16 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 伝票TO  (手)　= ' && ?PA17 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 納品日FROM(手)= ' && ?PA18 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 納品日TO  (手)= ' && ?PA19 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 横持ち日  (横)= ' && ?PA20 && ' #'
    SNDMSG    ?MSGX,TO-XCTL

/.==================================================================./

/.##ASSIGNファイルID取得##./
FILNMCHG:

    ?STEP :=   'FILNMCHG'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CASE  ?PA04  OF

          #'Y'#  /.オンライン./

              CASE  ?PA10  OF

                    #'00921084'#  /.カインズ./

                        /.連携データ./
                        ?FILNM    :=    ?FILCNZ && ?PA07
                        ?FILID    :=    %NAME(?FILNM)
                        ?LIBID    :=    %NAME(?LIBNMC)
                        ?LNKSYKF  :=    %NCAT(?FILID,?LIBID)
                        ?LNKSYKX  :=    %STRING(?LNKSYKF)
                        ?MSGX :=  '## 連携ﾌｧｲﾙ名 = '
                                     &&  ?LNKSYKX  &&  ' ##'
                        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

                        /.連携ワーク./
                        ?FILNMW   :=    ?FILCNZW && ?PA07
                        ?FILIDW   :=    %NAME(?FILNMW)
                        ?LIBIDW   :=    %NAME(?LIBNMW)
                        ?CNZSYWF  :=    %NCAT(?FILIDW,?LIBIDW)
                        ?CNZSYWX  :=    %STRING(?CNZSYWF)
                        ?MSGX :=  '## 連携ﾌｧｲﾙ名 = '
                                     &&  ?CNZSYWX  &&  ' ##'
                        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

              ELSE      /.カインズ以外./

                        /.連携データ./
                        ?FILNM    :=    ?FILETC && ?PA07
                        ?FILID    :=    %NAME(?FILNM)
                        ?LIBID    :=    %NAME(?LIBNM)
                        ?LNKSYKF  :=    %NCAT(?FILID,?LIBID)
                        ?LNKSYKX  :=    %STRING(?LNKSYKF)
                        ?MSGX :=  '## 連携ﾌｧｲﾙ名 = '
                                     &&  ?LNKSYKX  &&  ' ##'
                        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

              END

    ELSE   /.オンライン以外./

              CASE  ?PA15  OF

                    #'00921084'#  /.カインズ./

                        /.連携データ./
                        ?FILNM    :=    ?FILCNZ && ?PA07
                        ?FILID    :=    %NAME(?FILNM)
                        ?LIBID    :=    %NAME(?LIBNMC)
                        ?LNKSYKF  :=    %NCAT(?FILID,?LIBID)
                        ?LNKSYKX  :=    %STRING(?LNKSYKF)
                        ?MSGX :=  '## 連携ﾌｧｲﾙ名 = '
                                     &&  ?LNKSYKX  &&  ' ##'
                        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

                        /.連携ワーク./
                        ?FILNMW   :=    ?FILCNZW && ?PA07
                        ?FILIDW   :=    %NAME(?FILNMW)
                        ?LIBIDW   :=    %NAME(?LIBNMW)
                        ?CNZSYWF  :=    %NCAT(?FILIDW,?LIBIDW)
                        ?CNZSYWX  :=    %STRING(?CNZSYWF)
                        ?MSGX :=  '## 連携ﾌｧｲﾙ名 = '
                                     &&  ?CNZSYWX  &&  ' ##'
                        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

              ELSE      /.カインズ以外./

                        /.連携データ./
                        ?FILNM    :=    ?FILETC && ?PA07
                        ?FILID    :=    %NAME(?FILNM)
                        ?LIBID    :=    %NAME(?LIBNM)
                        ?LNKSYKF  :=    %NCAT(?FILID,?LIBID)
                        ?LNKSYKX  :=    %STRING(?LNKSYKF)
                        ?MSGX :=  '## 連携ﾌｧｲﾙ名 = '
                                     &&  ?LNKSYKX  &&  ' ##'
                        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

              END
    END

  /.?MSGX     :=  '## 連携ﾌｧｲﾙ名 = ' && ?LNKSYKX && ' ##'./
  /.SNDMSG    MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES       ./

/.##出荷連携ファイルロック##./
ASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-?LNKSYKF!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  '出荷連携ファイルロック'
              GOTO ABEND
    END

    IF        ?PA10     =  '00921084'  THEN   /.ｶｲﾝｽﾞｵﾝﾗｲﾝ./
              ASSIGN    FILE-?CNZSYWF!@XCL    /.連携ワーク./
              ?PGMEC    :=    @PGMEC
              ?PGMEM    :=    @PGMEM
              IF        ?PGMEC    ^=   0    THEN
                        SNDMSG '他で連携処理が行われています'
                                                   ,TOWS-@ORGWS
                        ?KEKA4 :=  '出荷連携ワークロック'
                        GOTO ABEND
              END
    END

    IF        ?PA15      =  '00921084'  THEN   /.ｶｲﾝｽﾞ手書き./
              ASSIGN    FILE-?CNZSYWF!@XCL     /.連携ワーク./
              ?PGMEC    :=    @PGMEC
              ?PGMEM    :=    @PGMEM
              IF        ?PGMEC    ^=   0    THEN
                        SNDMSG '他で連携処理が行われています'
                                                   ,TOWS-@ORGWS
                        ?KEKA4 :=  '出荷連携ワークロック'
                        GOTO ABEND
              END
    END

/.==================================================================./

 /. 取引先=カインズの場合は、カインズ専用処理へジャンプ ./

    IF        ?PA10    =   '00921084'   THEN   GOTO   SBT0340B  END
    IF        ?PA15    =   '00921084'   THEN   GOTO   SBT0340B  END

/.==================================================================./

/.##出荷連携データ抽出（オンライン）VER2##./
SBT0300B:

    /. 抽出区分(オ)＝Y　のみ実行./
    IF        ?PA04   ^=   'Y'          THEN   GOTO   SBT0030B  END

    ?STEP :=   'SBT0300B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-LNKSPML1,TOFILE-LNKSPML1.TOKKLIB
    OVRF      FILE-LNKSYKF,TOFILE-?LNKSYKF
    CALL      PGM-SBT0300B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIA,?KENSUUA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ抽出（オンライン）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##オンライン連携状況ファイル更新##./

    ?STEP :=   'SBT0100B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLJ,TOFILE-SHTDENLJ.TOKKLIB
    OVRF      FILE-LNKONLL2,TOFILE-LNKONLL2.TOKKLIB
    CALL      PGM-SBT0100B.TOKELIBO,
              PARA-(?BUMON,?TANCD,?PA07
                   ,?PA08,?PA09,?PA10,?PA11,?PA12)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'オンライン連携状況ファイル作成'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷連携データ抽出（オンライン－ジュンテンドーTC）##./
SBT0030B:

    /. 抽出区分(オ)＝Y　且つ　取引先=ｼﾞｭﾝﾃﾝﾄﾞｰ　のみ実行./
    IF        ?PA04   ^=   'Y'          THEN   GOTO   SBT0320B  END
    IF        ?PA10   ^=   '00005116'   THEN   GOTO   SBT0320B  END

    ?STEP :=   'SBT0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JTCDENL1,TOFILE-JTCDENL1.TOKKLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-LNKSYKF,TOFILE-?LNKSYKF
    CALL      PGM-SBT0030B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIB,?KENSUUB)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ抽出（ジュンテンドー）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##オンライン連携状況ファイル更新##./

    ?STEP :=   'SBT0100B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLJ,TOFILE-JTCDENL2.TOKKLIB
    OVRF      FILE-LNKONLL2,TOFILE-LNKONLL2.TOKKLIB
    CALL      PGM-SBT0100B.TOKELIBO,
              PARA-(?BUMON,?TANCD,?PA07,
                    ?PA08,?PA09,?PA10,?PA11,?PA12)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'オンライン連携状況ファイル更新'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷連携データ抽出（手書き）VER2##./
SBT0320B:

    /. 抽出区分(手)＝Y　のみ実行./
    IF        ?PA05   ^=   'Y'          THEN   GOTO   SBT0050B  END

    ?STEP :=   'SBT0320B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLF,TOFILE-SHTDENLF.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-LNKSPML1,TOFILE-LNKSPML1.TOKKLIB
    OVRF      FILE-LNKSYKF,TOFILE-?LNKSYKF
    CALL      PGM-SBT0320B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIC,?KENSUUC)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ抽出（手書き）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷連携データ抽出（横持：入出庫）##./
SBT0050B:
    /. ######################## ./
    /. ###  現状非使用機能  ### ./
    /. ######################## ./
    /. 抽出区分(横)＝Y　のみ実行./
    IF        ?PA06   ^=   'Y'          THEN   GOTO   SBT0330L  END

    ?STEP :=   'SBT0050B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-NYSFILL3,TOFILE-NYSFILL3.TOKFLIB
    OVRF      FILE-NYSFILL5,TOFILE-NYSFILL5.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB./
    OVRF      FILE-LNKSYKF,TOFILE-?LNKSYKF
/.##CALL      PGM-SBT0050B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUID,?KENSUUD)
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ抽出（横持：入出庫）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷連携データ抽出（横持：作業）##./
SBT0060B:
    /. ######################## ./
    /. ###  現状非使用機能  ### ./
    /. ######################## ./
    /. 抽出区分(横)＝Y　のみ実行./
    IF        ?PA06   ^=   'Y'          THEN   GOTO   SBT0330L  END

    ?STEP :=   'SBT0060B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-SYGFILL3,TOFILE-SYGFILL3.TOKFLIB
    OVRF      FILE-SYGFILL4,TOFILE-SYGFILL4.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB./
    OVRF      FILE-LNKSYKF,TOFILE-?LNKSYKF
/.##CALL      PGM-SBT0060B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIE,?KENSUUE)
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ抽出（横持：作業）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷連携データ抽出（横持：振替）##./
SBT0070B:
    /. ######################## ./
    /. ###  現状非使用機能  ### ./
    /. ######################## ./
    /. 抽出区分(横)＝Y　のみ実行./
    IF        ?PA06   ^=   'Y'          THEN   GOTO   SBT0330L  END

    ?STEP :=   'SBT0070B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-FURRUIL2,TOFILE-FURRUIL2.TOKKLIB
    OVRF      FILE-FURRUIL3,TOFILE-FURRUIL3.TOKKLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB./
    OVRF      FILE-LNKSYKF,TOFILE-?LNKSYKF
/.##CALL      PGM-SBT0070B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIF,?KENSUUF)
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ抽出（横持：振替）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷連携データ抽出（カインズオンライン）##./
SBT0340B:

    /. 抽出区分(オ)＝Y　且つ　取引先=カインズ 　のみ実行./
    IF        ?PA04   ^=   'Y'          THEN   GOTO   SBT0350B  END
    IF        ?PA10   ^=   '00921084'   THEN   GOTO   SBT0350B  END

    ?STEP :=   'SBT0340B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-BMSHACL1,TOFILE-BMSHACL1.BMSFLIB
    OVRF      FILE-CNZSYKF,TOFILE-?LNKSYKF
    CALL      PGM-SBT0340B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIK,?KENSUUK)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ抽出（カインズＯＮＬ）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##オンライン連携状況ファイル作成##./

    ?STEP :=   'SBT0100B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLJ,TOFILE-SHTDENLJ.TOKKLIB
    OVRF      FILE-LNKONLL2,TOFILE-LNKONLL2.TOKKLIB
    CALL      PGM-SBT0100B.TOKELIBO,
              PARA-(?BUMON,?TANCD,?PA07
                   ,?PA08,?PA09,?PA10,?PA11,?PA12)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'オンライン連携状況ファイル作成'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##出荷連携データ抽出（カインズオンライン）累積用##./

  /. 抽出件数0件以外時のみデータ累積を行う./
  /.IF    ?PA03   ^=   ' '        THEN   GOTO   SBT0350B  END./
    IF    ?KENSUUK =   '00000000' THEN   GOTO   SBT0350B  END

    ?STEP :=   'SBT0340B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   FILE-?CNZSYWF
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携ワーククリア'
              ?ERR   :=  'E'
              GOTO ABEND
    END

    OVRF      FILE-CNZSYKF,TOFILE-?CNZSYWF
    CALL      PGM-SBT0340B.TOKELIBO   /.再送モードで抽出./
             ,PARA-(?PA01,?PA02,'1',?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIZ,?KENSUUZ)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '累積用　データ抽出（カインズＯＮＬ）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##出荷連携累積データ削除（カインズオンライン）##./

    ?STEP :=   'SBT0360B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CNKSYRL1,TOFILE-CNZSYRL1.BMSFLIB
    CALL      PGM-SBT0360B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIZ,?KENSUUZ)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携累積データ削除（カインズＯＮＬ）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##出荷連携データ累積（カインズオンライン）##./

    ?STEP :=   'SBT0370B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CNZSYRF,TOFILE-CNZSYRF.BMSFLIB
    OVRF      FILE-CNZSYWF,TOFILE-?CNZSYWF
    CALL      PGM-SBT0370B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ累積（カインズＯＮＬ）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷連携データ抽出（カインズ手書き）##./
SBT0350B:

    /. 抽出区分(手)＝Y　且つ　取引先=カインズ 　のみ実行./
    IF        ?PA05   ^=   'Y'          THEN   GOTO   SBT0330L  END
    IF        ?PA15   ^=   '00921084'   THEN   GOTO   SBT0330L  END

    ?STEP :=   'SBT0350B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLF,TOFILE-SHTDENLF.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-CNZSYKF,TOFILE-?LNKSYKF
    CALL      PGM-SBT0350B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIL,?KENSUUL)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ抽出（カインズ手書）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##出荷連携データ抽出（カインズ手書）累積用##./

  /. 抽出件数0件以外時のみデータ累積を行う./
  /.IF    ?PA03   ^=   ' '        THEN   GOTO   SBT0330L  END./
    IF    ?KENSUUL =   '00000000' THEN   GOTO   SBT0330L  END

    ?STEP :=   'SBT0350B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   FILE-?CNZSYWF
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携ワーククリア'
              ?ERR   :=  'E'
              GOTO ABEND
    END

    OVRF      FILE-CNZSYKF,TOFILE-?CNZSYWF
    CALL      PGM-SBT0350B.TOKELIBO   /.再送モードで抽出./
             ,PARA-(?PA01,?PA02,'1',?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIZ,?KENSUUZ)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '累積用　データ抽出（カインズ手書）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##出荷連携累積データ削除（カインズ手書）##./

    ?STEP :=   'SBT0360B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CNKSYRL2,TOFILE-CNZSYRL2.BMSFLIB
    CALL      PGM-SBT0365B.TOKELIBO
             ,PARA-(?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,?PA07,?PA08,
                    ?PA09,?PA10,?PA11,?PA12,?PA13,?PA14,?PA15,?PA16,
                    ?PA17,?PA18,?PA19,?PA20,
                    ?SYURUIZ,?KENSUUZ)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携累積データ削除（カインズ手書）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##出荷連携データ累積（カインズ手書）##./

    ?STEP :=   'SBT0370B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CNZSYRF,TOFILE-CNZSYRF.BMSFLIB
    OVRF      FILE-CNZSYWF,TOFILE-?CNZSYWF
    CALL      PGM-SBT0370B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データ累積（カインズ手書）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.==================================================================./

/.##入出荷連携結果リスト出力VER2##./
SBT0330L:

    ?STEP :=   'SBT0330L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '# 帳票区分     = ' && ?CYOUHYOU && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 部門ＣＤ     = ' && ?PA01 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 担当者ＣＤ    = ' && ?PA02 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 送信区分　　  = ' && ?PA03 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分(オ) = ' && ?PA04 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分(手) = ' && ?PA05 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分(横) = ' && ?PA06 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分：入荷= ' && ?PB04 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分：入横= ' && ?PB05 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出倉庫ＣＤ = ' && ?PA07 && ' #'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '# 種類Ａ　　　 = ' && ?SYURUIA && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ａ　件数 = ' && ?KENSUUA && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｂ　　　 = ' && ?SYURUIB && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｂ　件数 = ' && ?KENSUUB && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｃ　　　 = ' && ?SYURUIC && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｃ　件数 = ' && ?KENSUUC && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｄ　　　 = ' && ?SYURUID && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｄ　件数 = ' && ?KENSUUD && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｅ　　　 = ' && ?SYURUIE && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｅ　件数 = ' && ?KENSUUE && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｆ　　　 = ' && ?SYURUIF && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｆ　件数 = ' && ?KENSUUF && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｇ　　　 = ' && ?SYURUIG && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｇ　件数 = ' && ?KENSUUG && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｈ　　　 = ' && ?SYURUIH && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｈ　件数 = ' && ?KENSUUH && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｉ　　　 = ' && ?SYURUII && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｉ　件数 = ' && ?KENSUUI && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｊ　　　 = ' && ?SYURUIJ && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｊ　件数 = ' && ?KENSUUJ && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｋ　　　 = ' && ?SYURUIK && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｋ　件数 = ' && ?KENSUUK && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｌ　　　 = ' && ?SYURUIL && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 種類Ｌ　件数 = ' && ?KENSUUL && ' #'
    SNDMSG    ?MSGX,TO-XCTL

/.  OVRF      FILE-PRTF,TOFILE-PRTF.TOKELIB
./  CASE  ?SOUKO  OF
       # 'TU' #
       ?MSGX :=  '##　ＡＷ札幌配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTU.TOKELIBO,MEDLIB-TOKELIBO
       # '6A' #
       ?MSGX :=  '##　片岡配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT6A.TOKELIBO,MEDLIB-TOKELIBO
       # 'TK' #
       ?MSGX :=  '##　さくら配送センター　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTK.TOKELIBO,MEDLIB-TOKELIBO
       # '63' #
       ?MSGX :=  '##　フバサミ配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT63.TOKELIBO,MEDLIB-TOKELIBO
       # '84' #
       ?MSGX :=  '##　鴻巣配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT84.TOKELIBO,MEDLIB-TOKELIBO
       # 'TV' #
       ?MSGX :=  '##　西尾配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTV.TOKELIBO,MEDLIB-TOKELIBO
       # 'T5' #
       ?MSGX :=  '##　ＫＡ岡山配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTT5.TOKELIBO,MEDLIB-TOKELIBO
       # 'E1' #
       ?MSGX :=  '##　甘木配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTE1.TOKELIBO,MEDLIB-TOKELIBO
       # '01' #
       ?MSGX :=  '##　本社　　　　　　　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO,MEDLIB-TOKELIBO
    ELSE
       ?MSGX :=  '##　出力判定不能　本社へ出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO,MEDLIB-TOKELIBO
    END
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SBT0330L.TOKELIBO
             ,PARA-(?CYOUHYOU,
                    ?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,
                    ?PB04,?PB05,
                    ?PA07,
                    ?SYURUIA,?KENSUUA,
                    ?SYURUIB,?KENSUUB,
                    ?SYURUIC,?KENSUUC,
                    ?SYURUID,?KENSUUD,
                    ?SYURUIE,?KENSUUE,
                    ?SYURUIF,?KENSUUF,
                    ?SYURUIG,?KENSUUG,
                    ?SYURUIH,?KENSUUH,
                    ?SYURUII,?KENSUUI,
                    ?SYURUIJ,?KENSUUJ,
                    ?SYURUIK,?KENSUUK,
                    ?SYURUIL,?KENSUUL)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入出荷連携結果リスト出力'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷連携データ転送##./
FIMPORT1:

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CASE  ?PA04  OF

          #'Y'#  /.オンライン./

              CASE  ?PA10  OF

                    #'00921084'#  /.カインズ./

                        FIMPORT   FILE-?LNKSYKF,TYPE-@FILE,
                                  PARA-LINKSTK,UNIT-3,OPR-@NO
                        ?PGMEC    :=    @PGMEC
                        ?PGMEM    :=    @PGMEM
                        IF        ?PGMEC    ^=   0    THEN
                                  ?KEKA4 :=  '出荷連携データ転送'
                                  GOTO ABEND
                        END

              ELSE      /.カインズ以外./

                        FIMPORT   FILE-?LNKSYKF,TYPE-@FILE,
                                  PARA-LINKSTK,UNIT-1,OPR-@NO
                        ?PGMEC    :=    @PGMEC
                        ?PGMEM    :=    @PGMEM
                        IF        ?PGMEC    ^=   0    THEN
                                  ?KEKA4 :=  '出荷連携データ転送'
                                  GOTO ABEND
                        END

              END

    ELSE   /.オンライン以外./

              CASE  ?PA15  OF

                    #'00921084'#  /.カインズ./

                        FIMPORT   FILE-?LNKSYKF,TYPE-@FILE,
                                  PARA-LINKSTK,UNIT-3,OPR-@NO
                        ?PGMEC    :=    @PGMEC
                        ?PGMEM    :=    @PGMEM
                        IF        ?PGMEC    ^=   0    THEN
                                  ?KEKA4 :=  '出荷連携データ転送'
                                  GOTO ABEND
                        END

              ELSE      /.カインズ以外./

                        FIMPORT   FILE-?LNKSYKF,TYPE-@FILE,
                                  PARA-LINKSTK,UNIT-1,OPR-@NO
                        ?PGMEC    :=    @PGMEC
                        ?PGMEM    :=    @PGMEM
                        IF        ?PGMEC    ^=   0    THEN
                                  ?KEKA4 :=  '出荷連携データ転送'
                                  GOTO ABEND
                        END

              END
    END

/.##出荷連携データクリア##./
CLRFILE1:

    ?STEP :=   'CLRFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   FILE-?LNKSYKF
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷連携データクリア'
              GOTO ABEND
    END

/.==================================================================./

/.##正常終了処理##./
RTN:

    RELEASE   ?LNKSYKF!@XCL
    RELEASE   ?CNZSYWF!@XCL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  '　【出荷連携データ抽出】'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC
/.==================================================================./

/.##異常終了処理##./
ABEND:

    RELEASE   ?LNKSYKF!@XCL
    RELEASE   ?CNZSYWF!@XCL

    IF        ?ERR   =  'E'  THEN
              OVRF      FILE-PRTF,TOFILE-PRTF.TOKELIB
              OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
              OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
              CALL      PGM-SBT0330L.TOKELIBO
                       ,PARA-(?CYOUHYOU,
                              ?PA01,?PA02,?PA03,?PA04,?PA05,?PA06,
                              ?PB04,?PB05,
                              ?PA07,
                              ?SYURUIA,?KENSUUA,
                              ?SYURUIB,?KENSUUB,
                              ?SYURUIC,?KENSUUC,
                              ?SYURUID,?KENSUUD,
                              ?SYURUIE,?KENSUUE,
                              ?SYURUIF,?KENSUUF,
                              ?SYURUIG,?KENSUUG,
                              ?SYURUIH,?KENSUUH,
                              ?SYURUII,?KENSUUI,
                              ?SYURUIJ,?KENSUUJ,
                              ?SYURUIK,?KENSUUK,
                              ?SYURUIL,?KENSUUL)
    END

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