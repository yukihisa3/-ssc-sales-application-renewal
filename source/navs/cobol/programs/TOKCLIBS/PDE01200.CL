/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    伝票ＥＸＣＥＬ連携　　　　　　       *  ./
/. *   JOB-NAME    :    伝票EXCEL計上処理　　　　　　　      *  ./
/. *   JOB-ID      :    PDE01200                             *  ./
/. *   DATE        :    2016/10/17                           *  ./
/. ***********************************************************  ./
    PGM  (P1-?JIKKOU)

/.##INパラメタ##./
    PARA  ?JIKKOU  ,STRING*1,IN,VALUE-' '
            /.実行区分                      ./
            /.  '3'    :   計上処理         ./
/.--------------------------------------------------------------./
    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PDE01200'
    VAR  ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR  ?BUMON   ,STRING*4,VALUE-'    '     /.部門./
    VAR  ?TANCD8  ,STRING*8,VALUE-'        ' /.部門+担当者./
    VAR  ?TANCD   ,STRING*2,VALUE-'  '       /.担当者     ./
    VAR  ?SOKCD   ,STRING*2,VALUE-'00'       /.実行倉庫   ./
    VAR  ?DSOKCD  ,STRING*2,VALUE-'00'       /.代表倉庫   ./
/.--------------------------------------------------------------./
    VAR  ?LIST1   ,STRING*1,VALUE-' '        /.帳票区分１ ./
    VAR  ?LIST2   ,STRING*1,VALUE-' '        /.帳票区分２ ./
    VAR  ?LIST3   ,STRING*1,VALUE-' '        /.帳票区分３ ./
    VAR  ?OUTPUT  ,STRING*1,VALUE-' '        /.出力先　　 ./
    VAR  ?TORICD  ,STRING*8,VALUE-'00000000' /.取引先ＣＤ ./
    VAR  ?TENPOF  ,STRING*5,VALUE-'00000'    /.店舗（開始 ./
    VAR  ?TENPOT  ,STRING*5,VALUE-'99999'    /.店舗（終了 ./
/.--------------------------------------------------------------./
    VAR  ?DTEXCEL ,STRING*1,VALUE-' '        /.EXCEL種別(ﾃﾞｰﾀ)./
    VAR  ?CNT1    ,STRING*7,VALUE-'0000000'  /.件数(取込) ./
    VAR  ?CNT2    ,STRING*7,VALUE-'0000000'  /.件数(売上）./
    VAR  ?CNT3    ,STRING*7,VALUE-'0000000'  /.件数(返品）./
    VAR  ?CNT4    ,STRING*7,VALUE-'0000000'  /.件数(値引）./
    VAR  ?CNT5    ,STRING*7,VALUE-'0000000'  /.件数(ＥＲ）./
/.--------------------------------------------------------------./
    VAR  ?OPR1    ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?OPR2    ,STRING*50                 /.      2    ./
    VAR  ?OPR3    ,STRING*50                 /.      3    ./
    VAR  ?OPR4    ,STRING*50                 /.      4    ./
    VAR  ?OPR5    ,STRING*50                 /.      5    ./
/.--------------------------------------------------------------./
    VAR  ?MSG1    ,STRING*80                 /.開始終了MSG./
    VAR  ?PGNM    ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1   ,STRING*40                 /.      2    ./
    VAR  ?KEKA2   ,STRING*40                 /.      3    ./
    VAR  ?KEKA3   ,STRING*40                 /.      4    ./
    VAR  ?KEKA4   ,STRING*40                 /.      5    ./
/.---------------------------------------------------------------./
  /.##端末別ファイルＩＤ編集用##./

    /.共通_ライブラリ名./
    VAR  ?LIBNM   ,STRING*8,VALUE-'ZAIEXCEL'
    /.共通_ファイルＩＤワーク./
    VAR   ?FILID   ,NAME
    /.共通_ＬＩＢＩＤワーク./
    VAR   ?LIBID   ,NAME
    /.共通_端末番号./
    VAR  ?TANNO    ,STRING*03,VALUE-'   '

    /.伝票EXCELデータ_共通部_ファイルＩＤ先頭./
    VAR  ?DCSV     ,STRING*4,VALUE-'DCSV'
    /.伝票EXCELデータ_個別部_ファイルＩＤ末尾./
    VAR  ?DCSV1    ,STRING*1,VALUE-'1'
    VAR  ?DCSV2    ,STRING*1,VALUE-'2'
    VAR  ?DCSV3    ,STRING*1,VALUE-'3'
    /.伝票EXCELデータ_OVRF_ファイルＩＤ./
    VAR  ?DCSVXXX1 ,NAME!MOD
    VAR  ?DCSVXXX2 ,NAME!MOD
    VAR  ?DCSVXXX3 ,NAME!MOD

    /.伝票EXCEL取込Ｆ_共通部_ファイルＩＤ先頭./
    VAR  ?DEX      ,STRING*3,VALUE-'DEX'
    /.伝票EXCEL取込Ｆ_個別部_ファイルＩＤ末尾./
    VAR  ?DEXF     ,STRING*1,VALUE-'F'
    VAR  ?DEXL1    ,STRING*2,VALUE-'L1'
    VAR  ?DEXL2    ,STRING*2,VALUE-'L2'
    /.伝票EXCEL取込Ｆ_OVRF_ファイルＩＤ./
    VAR  ?DEXXXXF  ,NAME!MOD
    VAR  ?DEXXXXL1 ,NAME!MOD
    VAR  ?DEXXXXL2 ,NAME!MOD

    /.基本売上伝票Ｆ_共通部_ファイルＩＤ先頭./
    VAR  ?URI      ,STRING*3,VALUE-'URI'
    /.基本売上伝票Ｆ_個別部_ファイルＩＤ末尾./
    VAR  ?URIF     ,STRING*1,VALUE-'F'
    VAR  ?URIL1    ,STRING*2,VALUE-'L1'
    VAR  ?URIL2    ,STRING*2,VALUE-'L2'
    VAR  ?URIL3    ,STRING*2,VALUE-'L3'
    /.基本売上伝票Ｆ_OVRF_ファイルＩＤ./
    VAR  ?URIXXXF  ,NAME!MOD
    VAR  ?URIXXXL1 ,NAME!MOD
    VAR  ?URIXXXL2 ,NAME!MOD
    VAR  ?URIXXXL3 ,NAME!MOD

    /.伝票番号採番リストデータ_共通部_ファイルＩＤ先頭./
    VAR  ?LST      ,STRING*3,VALUE-'LST'
    /.伝票番号採番リストデータ_個別部_ファイルＩＤ末尾./
    VAR  ?LSTF     ,STRING*1,VALUE-'F'
    VAR  ?LSTL1    ,STRING*2,VALUE-'L1'
    /.伝票番号採番リストデータ_OVRF_ファイルＩＤ./
    VAR  ?LSTXXXF  ,NAME!MOD
    VAR  ?LSTXXXL1 ,NAME!MOD

    /.発注集計ワーク_共通部_ファイルＩＤ先頭./
    VAR  ?HAC      ,STRING*3,VALUE-'HAC'
    /.発注集計ワーク_個別部_ファイルＩＤ末尾./
    VAR  ?HACF     ,STRING*1,VALUE-'F'
    /.発注集計ワーク_OVRF_ファイルＩＤ./
    VAR  ?HACXXXF  ,NAME!MOD

    /.ファイルＩＤ表示用ワーク./
    VAR  ?DSPFILID,STRING*17

    VAR   ?FILNM7  ,STRING*7,VALUE-'       '
    VAR   ?FILNM8  ,STRING*8,VALUE-'        '

/.---------------------------------------------------------------./
  /.##実行状況チェック・更新用##./
    /.実行状況./
    VAR   ?PHASE   ,STRING*1,VALUE-'0'
    /.取込日付./
    VAR   ?DATE1   ,STRING*8,VALUE-'00000000'
    /.取込時刻./
    VAR   ?TIME1   ,STRING*6,VALUE-'000000'
    /.変換日付./
    VAR   ?DATE2   ,STRING*8,VALUE-'00000000'
    /.変換時刻./
    VAR   ?TIME2   ,STRING*6,VALUE-'000000'
    /.計上日付./
    VAR   ?DATE3   ,STRING*8,VALUE-'00000000'
    /.計上時刻./
    VAR   ?TIME3   ,STRING*6,VALUE-'000000'
/.-----------------------------------------------------------./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB/ZAIEXCEL

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    CASE  ?JIKKOU     OF
          #'3'# ?PGNM := '伝票ＥＸＣＥＬ取込_計上処理'
          ELSE  SNDMSG   '！！！！！！！！！！！！！！！！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                SNDMSG   '！実行区分3を指定して下さい   ！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                SNDMSG   '！！！！！！！！！！！！！！！！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                RETURN
    END

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
GETWKSTN:
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=  'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              GOTO   ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO   ABEND
    END

/.##端末番号を取得##./
SBT0620B:

    ?STEP :=   'SBT0620B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SBT0620B.TOKELIBO,PARA-(?WS,?TANNO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX := '## 端末NO = ' && ?TANNO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##端末別ファイルＩＤ編集##./
FILNMCHG:

    ?STEP :=  'FILNMCHG'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.伝票EXCEL取込Ｆ_ＰＦ ./
    ?FILNM7   :=    ?DEX  && ?TANNO && ?DEXF
    ?FILID    :=    %NAME(?FILNM7)
    ?LIBID    :=    %NAME(?LIBNM)
    ?DEXXXXF  :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?DEXXXXF)
    ?MSGX     :=    '##EXCEL取込Ｆ PF=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.伝票EXCEL取込Ｆ_Ｌ１ ./
    ?FILNM8   :=    ?DEX  && ?TANNO && ?DEXL1
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?DEXXXXL1 :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?DEXXXXL1)
    ?MSGX     :=    '##EXCEL取込Ｆ L1=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.基本売上伝票Ｆ_ＰＦ ./
    ?FILNM7   :=    ?URI  && ?TANNO && ?URIF
    ?FILID    :=    %NAME(?FILNM7)
    ?LIBID    :=    %NAME(?LIBNM)
    ?URIXXXF  :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?URIXXXF)
    ?MSGX     :=    '##基本売伝Ｆ  PF=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.基本売上伝票Ｆ_Ｌ１ ./
    ?FILNM8   :=    ?URI  && ?TANNO && ?URIL1
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?URIXXXL1 :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?URIXXXL1)
    ?MSGX     :=    '##基本売伝Ｆ  L1=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.基本売上伝票Ｆ_Ｌ２ ./
    ?FILNM8   :=    ?URI  && ?TANNO && ?URIL2
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?URIXXXL2 :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?URIXXXL2)
    ?MSGX     :=    '##基本売伝Ｆ  L2=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.基本売上伝票Ｆ_Ｌ３ ./
    ?FILNM8   :=    ?URI  && ?TANNO && ?URIL3
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?URIXXXL3 :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?URIXXXL3)
    ?MSGX     :=    '##基本売伝Ｆ  L3=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.伝票番号採番リストデータ_ＰＦ ./
    ?FILNM7   :=    ?LST  && ?TANNO && ?LSTF
    ?FILID    :=    %NAME(?FILNM7)
    ?LIBID    :=    %NAME(?LIBNM)
    ?LSTXXXF  :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?LSTXXXF)
    ?MSGX     :=    '##採番ﾘｽﾄﾃﾞｰﾀ PF=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.伝票番号採番リストデータ_Ｌ１ ./
    ?FILNM8   :=    ?LST  && ?TANNO && ?LSTL1
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?LSTXXXL1 :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?LSTXXXL1)
    ?MSGX     :=    '##採番ﾘｽﾄﾃﾞｰﾀ L1=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.発注集計ワーク_ＳＦ ./
    ?FILNM7   :=    ?HAC  && ?TANNO && ?HACF
    ?FILID    :=    %NAME(?FILNM7)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HACXXXF  :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?HACXXXF)
    ?MSGX     :=    '##発注集計ﾜｰｸ SF=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##使用ファイルロック##./
ASSIGN:

    ?STEP :=  'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ASSIGN FILE-?DEXXXXF!@XCL  /.伝票EXCEL取込Ｆ./
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   0    THEN
           SNDMSG '連携処理が行えません',
                  TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  'ＥＸＣＥＬ取込Ｆ　ロック'
           GOTO   ABEND1
    END

    ASSIGN FILE-?URIXXXF!@XCL  /.基本売上伝票データ./
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   0    THEN
           SNDMSG '連携処理が行えません',
                  TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  '基本売上データ　　ロック'
           GOTO   ABEND1
    END

    ASSIGN FILE-?LSTXXXF!@XCL  /.採番リストデータ./
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   0    THEN
           SNDMSG '連携処理が行えません',
                  TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  '採番リストデータ　ロック'
                       GOTO   ABEND1
    END

    ASSIGN FILE-?HACXXXF!@XCL  /.発注集計ワーク./
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   0    THEN
           SNDMSG '連携処理が行えません',
                  TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  '発注集計ワーク　　ロック'
           GOTO   ABEND1
    END

/.##実行状況チェック##./
SDE0160B:

    ?STEP :=  'SDE0160B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL      PGM-SDE0160B.TOKELIBO,
              PARA-(?WS,?PHASE,
                    ?DATE1,?TIME1,
                    ?DATE2,?TIME2,
                    ?DATE3,?TIME3)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '実行状況チェック'
              GOTO   ABEND
    END

    ?MSGX := 'LINK-IN-WKSTN =' && ?WS
    SNDMSG ?MSGX,TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
    ?MSGX := 'PHASE         =' && ?PHASE
    SNDMSG ?MSGX,TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
    ?MSGX := '取込日付＝' && ?DATE1
    SNDMSG ?MSGX,TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
    ?MSGX := '取込時刻＝' && ?TIME1
    SNDMSG ?MSGX,TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
    ?MSGX := '変換日付＝' && ?DATE2
    SNDMSG ?MSGX,TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
    ?MSGX := '変換時刻＝' && ?TIME2
    SNDMSG ?MSGX,TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
    ?MSGX := '計上日付＝' && ?DATE3
    SNDMSG ?MSGX,TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
    ?MSGX := '計上時刻＝' && ?TIME3
    SNDMSG ?MSGX,TOWS-@ORGWS,JLOG-@YES,SLOG-@YES

    IF  ?PHASE =  '2'   THEN   /.変換済で未計上の場合##./
        GOTO  SDE0010I
    END

/.##取込確認画面##./
SHOM0900:

    ?STEP :=  'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE      ?PHASE     OF

     #'1'#
             /.１２３４５６７８９０１２３４５６７８９０１２３４５./
     ?OPR1 := '＜　エラー！　　　前回分データ変換確認　＞'
     ?OPR2 := '前回分の伝票ＥＸＣＥＬデータの変換が未実施です！'
     ?OPR3 := '計上出来るデータがありません。確認して下さい。'
     ?OPR4 := '続行・打ち切り　どちらを選択しても処理は打ち切り'
     ?OPR5 := 'ます。'

     #'3'#
             /.１２３４５６７８９０１２３４５６７８９０１２３４５./
     ?OPR1 := '＜　エラー！　　　前回分データ計上確認　＞'
     ?OPR2 := '前回分の伝票ＥＸＣＥＬデータの変換が未実施です！'
     ?OPR3 := '計上出来るデータがありません。確認して下さい。'
     ?OPR4 := '続行・打ち切り　どちらを選択しても処理は打ち切り'
     ?OPR5 := 'ます。'

     #'0'#
             /.１２３４５６７８９０１２３４５６７８９０１２３４５./
     ?OPR1 := '＜　エラー！　　　ＥＸＣＥＬデータ未取込確認　＞'
     ?OPR2 := '伝票ＥＸＣＥＬデータの取込が未実施です！'
     ?OPR3 := '計上出来るデータがありません。確認して下さい。'
     ?OPR4 := '続行・打ち切り　どちらを選択しても処理は打ち切り'
     ?OPR5 := 'ます。'

    ELSE
             /.１２３４５６７８９０１２３４５６７８９０１２３４５./
     ?OPR1 := '＜　エラー！　　　想定外の実行状況　　　　　　＞'
     ?OPR2 := '想定外の実行状況です！'
     ?OPR3 := '実行状況を確認して下さい。'
     ?OPR4 := '続行・打ち切り　どちらを選択しても処理は打ち切り'
     ?OPR5 := 'ます。'

    END

    CALL   OHOM0900.TOKELIB,
           PARA-(?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '処理確認画面'
              GOTO   ABEND
    END

    ?MSGX :=  '## 処理を打ち切ります 実行状況ＮＧ ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN  /.処理を終了します./

/.##伝票EXCELデータ取込指示##./
SDE0010I:

    ?STEP :=  'SDE0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL      SDE0010I.TOKELIBO,
              PARA-(?JIKKOU,
                    ?WS,?TANNO,
                    ?BUMON,?TANCD,
                    ?PHASE,
                    ?DATE1,?TIME1,?DATE2,?TIME2,?DATE3,?TIME3,
                    ?LIST1,?LIST2,?LIST3,
                    ?OUTPUT)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC =  4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN    PGMEC-?PGMEC
    END
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '伝票ＥＸＣＥＬ計上指示'
              GOTO      ABEND
    END

/.##リスト出力先制御##./
CHGSPLQ:

    ?STEP :=  'CHGSPLQ '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE      ?OUTPUT   OF
    /.レーザー./
    /.現地./
       #' '#  IF  ?BUMON  =  '2940'  THEN
                  SNDMSG '＃九州営業課レーザーへ出力＃',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                  OVRPRTF FILE-XU04LP,TOFILE-AMAGILBP.XUCL
              ELSE
                  SNDMSG '＃本社レーザーへ出力　　　＃',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                  OVRPRTF FILE-XU04LP,TOFILE-TOSTMPRT.XUCL
              END
    /.社内./
    /. #' '#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL   ./

    /.連帳./                           /.↓現地はXU04LPでOK？./
       #'1'#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    END

/.##伝票ＥＸＣＥＬ計上処理##./
 /.##伝票番号採番リストデータ初期化##./
PCLRFILE:

    ?STEP  :=  'PCLRFILE'
    ?MSGX  :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CLRFILE FILE-?LSTXXXF
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '伝票番号採番リストデータ初期化'
              GOTO   ABEND
    END

 /.##伝票番号採番リストデータ作成##./
SDE0080B:

    ?STEP  :=  'SDE0080B'
    ?MSGX  :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF FILE-URIXXXL1,TOFILE-?URIXXXL1
    OVRF FILE-LSTXXXL1,TOFILE-?LSTXXXL1
    CALL      PGM-SDE0080B.TOKELIBO,
              PARA-(?BUMON,?TANCD)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '伝票番号採番リストデータ作成'
              GOTO   ABEND
    END

 /.##伝票ＥＸＣＥＬ計上処理##./
SDE0120B:

    ?STEP :=  'SDE0120B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF FILE-URIXXXL1,TOFILE-?URIXXXL1
    CALL      PGM-SDE0120B.TOKELIBO,
              PARA-(?BUMON,?TANCD)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '伝票ＥＸＣＥＬ計上処理'
              GOTO   ABEND
    END

 /.##実行状況更新##./
SDE0150B:

    ?STEP  :=  'SDE0150B'
    ?MSGX  :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ?PHASE := '3'
    CALL      PGM-SDE0150B.TOKELIBO,
              PARA-(?WS,?PHASE,?DATE1,?TIME1)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '実行状況更新'
              GOTO   ABEND
    END

/.##伝票番号採番リスト##./
SDE0090L:

    IF     ?LIST1  ^=  'Y'  THEN     /.帳票出力を選択した場合のみ./
           GOTO     SDE0140L
    END

    ?STEP  :=  'SDE0090L'
    ?MSGX  :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.伝票番号採番リスト./
    OVRF      FILE-LSTXXXL1,TOFILE-?LSTXXXL1
    CALL      PGM-SDE0090L.TOKELIBO
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '伝票番号採番リスト'
              GOTO ABEND
    END

/.##発注集計表##./
SDE0140L:

    IF     ?LIST2  ^=  'Y'  THEN     /.帳票出力を選択した場合のみ./
           GOTO     SDE0110L
    END

    ?STEP  :=  'SDE0140L'
    ?MSGX  :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.発注集計表データ抽出./
    OVRF      FILE-URIXXXL2,TOFILE-?URIXXXL2
    OVRF      FILE-HACXXXF,TOFILE-?HACXXXF
    ?TORICD := '00000000'
    ?TENPOF := '00000'
    ?TENPOT := '99999'
    CALL      PGM-SDE0130B.TOKELIBO,
              PARA-(?TORICD,?TENPOF,?TENPOT)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '発注集計表データ抽出'
              GOTO   ABEND
    END

    /.SORT./
    /.SORT順：_取引先ＣＤ
              _倉庫ＣＤ
              _部門ＣＤ
              _納品日
  　　　　　　_相手商品ＣＤ
  　　　　　　_店舗ＣＤ./

    SORT      INFILE-?HACXXXF,INRL-256,INBF-15,
              OUTFILE-?HACXXXF,OUTBF-15,
              KEY-13!8!DA,
              KEY1-26!2!CA,
              KEY2-28!4!CA,
              KEY3-40!8!DA,
              KEY4-48!13!CA,
              KEY5-21!5!CA,
              RCDL-@DSP

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注集計表データＳＯＲＴ'
              GOTO ABEND
    END

    /.発注集計表./
    OVRF      FILE-HACXXXF,TOFILE-?HACXXXF
    CALL      PGM-SDE0140L.TOKELIBO,
              PARA-(?BUMON,?TANCD)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注集計表発行'
              GOTO ABEND
    END

/.##伝票変換リスト##./
SDE0110L:

    IF     ?LIST3  ^=  'Y'  THEN     /.帳票出力を選択した場合のみ./
           GOTO     RTN
    END

    ?STEP  :=  'SDE0110L'
    ?MSGX  :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.伝票変換リスト./
    OVRF      FILE-URIXXXL3,TOFILE-?URIXXXL3
    CALL      PGM-SDE0110L.TOKELIBO,
              PARA-(?TORICD,?TENPOF,?TENPOT,?TANCD,?BUMON)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '伝票変換リスト'
              GOTO ABEND
    END

/.--------------------------------------------------------------./

RTN:

    RELEASE FILE-?DCSVXXX2!@XCL
    RELEASE FILE-?DCSVXXX3!@XCL
    RELEASE FILE-?DEXXXXF!@XCL
    RELEASE FILE-?URIXXXF!@XCL
    RELEASE FILE-?LSTXXXF!@XCL
    RELEASE FILE-?HACXXXF!@XCL

    ?KEKA1 :=  '伝票ＥＸＣＥＬデータ計上処理'
    ?KEKA2 :=  '　　　正常終了しました。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    RELEASE FILE-?DEXXXXF!@XCL
    RELEASE FILE-?URIXXXF!@XCL
    RELEASE FILE-?LSTXXXF!@XCL
    RELEASE FILE-?HACXXXF!@XCL

    ?KEKA1 :=  '伝票ＥＸＣＥＬデータ計上処理が'
    ?KEKA2 :=  '異常終了しました。'
    ?KEKA3 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  := ?MSG(?I)
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    END

    RETURN    PGMEC-?PGMEC

ABEND1:

    RELEASE FILE-?DEXXXXF!@XCL
    RELEASE FILE-?URIXXXF!@XCL
    RELEASE FILE-?LSTXXXF!@XCL
    RELEASE FILE-?HACXXXF!@XCL

    ?KEKA1 :=  '伝票ＥＸＣＥＬデータ計上処理関係ファイ'
    ?KEKA2 :=  'ルが、他で使用されています。'
    ?KEKA3 :=  '他端末で使用中でないか確認して下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  := ?MSG(?I)
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
