/. ***TEST****************************************************  ./
/. *     サカタのタネ　本社システム　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   *  ./
/. *   JOB-ID      :    PBT01600                             *  ./
/. *   JOB-NAME    :    ＬＩＮＫＳ入荷連携データ抽出　　　　 *  ./
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
    VAR ?CLID    ,STRING*08,VALUE-'PBT01600'        /.CLID         ./
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

    VAR ?CYOUHYOU,STRING*01,VALUE-'N'         /.帳票区分　　　　 ./

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

/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := 'ＬＩＮＫＳ入荷連携データ抽出　　　　　'
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

/.##実行確認##./
/.  ?OPR1  :=  ''
    ?OPR2  :=  '　ＬＩＮＫＳシステムへの'
    ?OPR3  :=  '　入荷連携データ抽出を行います。'
    ?OPR4  :=  ''
    ?OPR5  :=  '　続行しますか？'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)./

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

/.##入荷連携ファイルロック##./
ASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-LNKNYKF.TOKKLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  '入荷連携ファイルロック'
              GOTO ABEND
    END

/.##入荷連携データ抽出指示##./
SBT0160I:

    ?STEP :=   'SBT0160I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIBO
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SBT0160I.TOKELIBO,
              PARA-(?BUMON,?TANCD,?SOUKO,?DSOUKO,
                    ?PB01,?PB02,?PB03,?PB04,?PB05,?PB06,?PB07)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   4010 THEN
              RELEASE   LNKNYKF.TOKKLIB!@XCL
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
              SNDMSG    ?MSGX,TO-XCTL
              RETURN
         ELSE
              ?KEKA4 :=  '入荷連携データ抽出指示'
              GOTO ABEND
         END
    END
    ?MSGX := '# 部門ＣＤ     = ' && ?PB01 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 担当者ＣＤ    = ' && ?PB02 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 送信区分　　  = ' && ?PB03 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分(入) = ' && ?PB04 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出区分(横) = ' && ?PB05 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 抽出倉庫　　　= ' && ?PB06 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 連携日　  (横)= ' && ?PB07 && ' #'

/.##入荷連携データ抽出（入荷）##./
SBT0170B:

    /. 抽出区分(入)＝Y　のみ実行./
    IF        ?PB04   ^=   'Y'          THEN   GOTO   SBT0180B  END

    ?STEP :=   'SBT0170B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  OVRF      FILE-NYKFILL3,TOFILE-NYKFILL3.TOKFLIB
./  OVRF      FILE-NYKFILL3,TOFILE-NYKFILL3.D365DLIB
    OVRF      FILE-LNKNYKF,TOFILE-LNKNYKF.TOKKLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL2,TOFILE-SHOTBL2.TOKFLIB
    CALL      PGM-SBT0170B.TOKELIBO
             ,PARA-(?PB01,?PB02,?PB03,?PB04,?PB05,?PB06,?PB07,
                    ?SYURUIG,?KENSUUG)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
  /.IF        ?PGMEC     =  4001  THEN
              SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
              SNDMSG '＊　この指定条件では　＊',TO-XCTL
              SNDMSG '＊　抽出対象データが　＊',TO-XCTL
              SNDMSG '＊　ありません！　　　＊',TO-XCTL
              SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
              RETURN    PGMEC-@PGMEC
    END                               ./
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷連携データ抽出（入荷）'
              ?ERR   :=  'E'
              GOTO ABEND
    END
/. TEST
    ?ERR   :=  'E'
    GOTO ABEND
   TEST ./
/.##入荷連携データ抽出（横持：入出庫）##./
SBT0180B:

    /. 抽出区分(横)＝Y　のみ実行./
    IF        ?PB05   ^=   'Y'          THEN   GOTO   SBT0080L  END

    ?STEP :=   'SBT0180B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYSFILL4,TOFILE-NYSFILL4.TOKFLIB
    OVRF      FILE-NYSFILL6,TOFILE-NYSFILL6.TOKFLIB
    OVRF      FILE-LNKNYKF,TOFILE-LNKNYKF.TOKKLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
/.##CALL      PGM-SBT0180B.TOKELIBO
             ,PARA-(?PB01,?PB02,?PB03,?PB04,?PB05,?PB06,?PB07,
                    ?SYURUIH,?KENSUUH)
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
  /.IF        ?PGMEC     =  4001  THEN
              SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
              SNDMSG '＊　この指定条件では　＊',TO-XCTL
              SNDMSG '＊　抽出対象データが　＊',TO-XCTL
              SNDMSG '＊　ありません！　　　＊',TO-XCTL
              SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
              RETURN    PGMEC-@PGMEC
    END ./
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷連携データ抽出（横持：入出庫）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##入荷連携データ抽出（横持：作業）##./
SBT0190B:

    /. 抽出区分(横)＝Y　のみ実行./
    IF        ?PB05   ^=   'Y'          THEN   GOTO   SBT0080L  END

    ?STEP :=   'SBT0190B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SYGFILL3,TOFILE-SYGFILL3.TOKFLIB
    OVRF      FILE-SYGFILL4,TOFILE-SYGFILL4.TOKFLIB
    OVRF      FILE-LNKNYKF,TOFILE-LNKNYKF.TOKKLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
/.##CALL      PGM-SBT0190B.TOKELIBO
             ,PARA-(?PB01,?PB02,?PB03,?PB04,?PB05,?PB06,?PB07,
                    ?SYURUII,?KENSUUI)
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
  /.IF        ?PGMEC     =  4001  THEN
              SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
              SNDMSG '＊　この指定条件では　＊',TO-XCTL
              SNDMSG '＊　抽出対象データが　＊',TO-XCTL
              SNDMSG '＊　ありません！　　　＊',TO-XCTL
              SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
              RETURN    PGMEC-@PGMEC
    END ./
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷連携データ抽出（横持：作業）'
              ?ERR   :=  'E'
              GOTO ABEND
    END

/.##入荷連携データ抽出（横持：振替）##./
SBT0200B:
 /. --------------------------------------------------- ./
 /. 不要処理となったため、SBT0200Bは迂回する 2012/10/10 ./
 /. --------------------------------------------------- ./
    GOTO   SBT0080L

    /. 抽出区分(横)＝Y　のみ実行./
    IF        ?PB05   ^=   'Y'          THEN   GOTO   SBT0080L  END
    ?STEP :=   'SBT0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURRUIL2,TOFILE-FURRUIL2.TOKKLIB
    OVRF      FILE-FURRUIL3,TOFILE-FURRUIL3.TOKKLIB
    OVRF      FILE-LNKNYKF,TOFILE-LNKNYKF.TOKKLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-SBT0200B.TOKELIBO
             ,PARA-(?PB01,?PB02,?PB03,?PB04,?PB05,?PB06,?PB07,
                    ?SYURUIJ,?KENSUUJ)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷連携データ抽出（横持：振替）'
              GOTO ABEND
    END

/.##入出荷連携結果リスト出力##./
SBT0080L:

    ?STEP :=   'SBT0080L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '# 帳票区分     = ' && ?CYOUHYOU && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 部門ＣＤ     = ' && ?PB01 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 担当者ＣＤ    = ' && ?PB02 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 送信区分　　  = ' && ?PB03 && ' #'
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
    ?MSGX := '# 抽出倉庫ＣＤ = ' && ?PB06 && ' #'
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

    OVRF      FILE-PRTF,TOFILE-PRTF.TOKELIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SBT0080L.TOKELIBO
             ,PARA-(?CYOUHYOU,
                    ?PB01,?PB02,?PB03,?PA04,?PA05,?PA06,
                    ?PB04,?PB05,
                    ?PB06,
                    ?SYURUIA,?KENSUUA,
                    ?SYURUIB,?KENSUUB,
                    ?SYURUIC,?KENSUUC,
                    ?SYURUID,?KENSUUD,
                    ?SYURUIE,?KENSUUE,
                    ?SYURUIF,?KENSUUF,
                    ?SYURUIG,?KENSUUG,
                    ?SYURUIH,?KENSUUH,
                    ?SYURUII,?KENSUUI,
                    ?SYURUIJ,?KENSUUJ)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入出荷連携結果リスト出力'
              GOTO ABEND
    END

/.##入荷連携データ転送##./
FIMPORT1:

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-LNKNYKF.TOKKLIB,TYPE-@FILE,
              PARA-LINKS,UNIT-2,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷連携データ転送'
              GOTO ABEND
    END

/.##入荷連携データクリア##./
CLRFILE1:

    ?STEP :=   'CLRFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   FILE-LNKNYKF.TOKKLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷連携データクリア'
              GOTO ABEND
    END

/.##正常終了処理##./
RTN:

    RELEASE   LNKNYKF.TOKKLIB!@XCL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  '　【入荷連携データ抽出】'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC
/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

    RELEASE   LNKNYKF.TOKKLIB!@XCL

    IF        ?ERR   =  'E'  THEN
              OVRF      FILE-PRTF,TOFILE-PRTF.TOKELIB
              OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
              OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
              CALL      PGM-SBT0080L.TOKELIBO
                       ,PARA-(?CYOUHYOU,
                              ?PB01,?PB02,?PB03,?PA04,?PA05,?PA06,
                              ?PB04,?PB05,
                              ?PB06,
                              ?SYURUIA,?KENSUUA,
                              ?SYURUIB,?KENSUUB,
                              ?SYURUIC,?KENSUUC,
                              ?SYURUID,?KENSUUD,
                              ?SYURUIE,?KENSUUE,
                              ?SYURUIF,?KENSUUF,
                              ?SYURUIG,?KENSUUG,
                              ?SYURUIH,?KENSUUH,
                              ?SYURUII,?KENSUUI,
                              ?SYURUIJ,?KENSUUJ)
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