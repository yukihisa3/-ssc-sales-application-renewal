/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    イオン流通ＢＭＳ変換処理             *  ./
/. *   JOB-ID      :    PNJH2910                             *  ./
/. *   JOB-NAME    :    イオン流通ＢＭＳ変換処理             *  ./
/. *   UPDATE      :    2017.10.06 S11270540                 *  ./
/. *   UPDATE      :    2021.11.08 S22470210 返品自動計上    *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
/.       P6-?LIBNM,P7-?FILNM) ./
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA      ?TOKCD    ,STRING*8,IN,VALUE-'        ' /.受信取引先./
    PARA      ?LINE     ,STRING*1,IN,VALUE-' '        /.回線./
    PARA      ?YUSEN    ,STRING*1,IN,VALUE-' '        /.回線優先./
    PARA      ?LIBNM    ,STRING*8,IN,VALUE-'        ' /.集信LIB./
    PARA      ?FILNM    ,STRING*8,IN,VALUE-'        ' /.集信FILE./
    PARA      ?JKEKA    ,STRING*4,OUT,VALUE-'    '    /.集信FILE./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH2910'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##返品自動計上パラメタ##./
    VAR       ?JKBN     ,STRING*1,VALUE-' '         /.実行区分./
    VAR       ?BUMCD    ,STRING*4,VALUE-'2920'      /.部門ＣＤ./
    VAR       ?TANCD    ,STRING*2,VALUE-'98'        /.担当者ＣＤ./
    VAR       ?HTOKCD   ,STRING*8,VALUE-'00000000'  /.取引先ＣＤ./
    VAR       ?NDATE    ,STRING*8,VALUE-'        '  /.入力日./
    VAR       ?UPTIME   ,STRING*6,VALUE-'      '    /.更新日./
    VAR       ?DENCNT   ,STRING*7,VALUE-'0000000'   /.売伝追加件数./
    /. リスト出力用パラメタ ./
    VAR       ?KEIKBN   ,STRING*1,VALUE-'3'         /. 計上区分 ./
    VAR       ?TANST    ,STRING*2,VALUE-'98'        /. 担当者開始 ./
    VAR       ?TANED    ,STRING*2,VALUE-'98'        /. 担当者終了 ./
    VAR       ?DKBN     ,STRING*1,VALUE-'2'         /. 日付区分　 ./
    VAR       ?AFROM    ,STRING*8,VALUE-'00000000'  /. 検収日開始 ./
    VAR       ?ATO      ,STRING*8,VALUE-'00000000'  /. 検収日終了 ./
    VAR       ?NFROM    ,STRING*8,VALUE-'00000000'  /. 入力日開始 ./
    VAR       ?NTO      ,STRING*8,VALUE-'00000000'  /. 入力日終了 ./
    VAR       ?KFROM    ,STRING*8,VALUE-'00000000'  /. 計上日開始 ./
    VAR       ?KTO      ,STRING*8,VALUE-'00000000'  /. 計上日終了 ./
    VAR       ?SFROM    ,STRING*8,VALUE-'00000000'  /. 作成日開始 ./
    VAR       ?STO      ,STRING*8,VALUE-'00000000'  /. 作成日終了 ./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./
    /.エラーメール発行用ワーク./
    VAR ?ERKBN    ,STRING*03,VALUE-'   '       /.区分./
    VAR ?ERDATE   ,STRING*08,VALUE-'00000000'  /.バッチ日付./
    VAR ?ERTIME   ,STRING*04,VALUE-'0000'      /.バッチ時刻./
    VAR ?ERTKCD   ,STRING*08,VALUE-'00000000'  /.バッチ取引先./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO/
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB/TOKMDLIB

/.##イオン受信取引先マスタ初期化##./
PCLRFIL1:

    ?STEP :=   'PCLRFIL1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE FILE-IONTWKF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K608'
              CALL PGM-SNJ0730B.TOKELIB,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先付替エラーワーク初期化##./
PCLRFIL2:

    ?STEP :=   'PCLRFIL2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE FILE-IONTERWK.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K609'
              CALL PGM-SNJ0730B.TOKELIB,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先ＣＤ付替え処理（発注）##./
SBM1000B:

    ?STEP :=   'SBM1000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-BMSHACL1,TOFILE-BMSHACL1.BMSFLIB
    OVRF      FILE-IONFURL1,TOFILE-IONFURL1.BMSFLIB
    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-IONTERWK,TOFILE-IONTERWK.BMSFLIB
    CALL      PGM-SBM1000B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K604'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##変換連続起動##./
SBM1040B:

    ?STEP :=   'SBM1040B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    CALL      PGM-SBM1040B.TOKELIBO,PARA-('1',?LINE,?YUSEN)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K605'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先ＣＤ付替え処理（受領）##./
SBM1001B:

    ?STEP :=   'SBM1001B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-BMSJYRL1,TOFILE-BMSJYRL1.BMSFLIB
    OVRF      FILE-IONFURL1,TOFILE-IONFURL1.BMSFLIB
    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-IONTERWK,TOFILE-IONTERWK.BMSFLIB
    CALL      PGM-SBM1001B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K604'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先ＣＤ付替え処理（受領訂正）##./
SBM1002B:

    ?STEP :=   'SBM1002B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-BMSJYTL1,TOFILE-BMSJYTL1.BMSFLIB
    OVRF      FILE-IONFURL1,TOFILE-IONFURL1.BMSFLIB
    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-IONTERWK,TOFILE-IONTERWK.BMSFLIB
    CALL      PGM-SBM1002B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K604'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先ＣＤ付替え処理（返品）##./
SBM1003B:

    ?STEP :=   'SBM1003B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-BMSHEPL1,TOFILE-BMSHEPL1.BMSFLIB
    OVRF      FILE-IONFURL1,TOFILE-IONFURL1.BMSFLIB
    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-IONTERWK,TOFILE-IONTERWK.BMSFLIB
    CALL      PGM-SBM1003B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K604'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先ＣＤ付替え処理（支払）##./
SBM1004B:

    ?STEP :=   'SBM1004B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-BMSSIHL1,TOFILE-BMSSIHL1.BMSFLIB
    OVRF      FILE-IONFURL1,TOFILE-IONFURL1.BMSFLIB
    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-IONTERWK,TOFILE-IONTERWK.BMSFLIB
    CALL      PGM-SBM1004B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K604'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先別受信件数リスト発行##./
SBM1030L:

    ?STEP :=   'SBM1030L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SBMJOB JOB-TOKJYULT,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-PNJH291L.TOKCLIBO,LOG-@YES!1024,PTY-5,
           PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKELIBO/TOKFLIB/
           TOKKLIB/TOKJLIB/BMSFLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
/.  OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM1030L.TOKELIBO,
              PARA-('1',?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
                ##ABENDｺｰﾄﾞｾｯﾄ##
              ?KEKA := 'K606'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END
./

/.##受領データ共通変換##./
SJR0070B:

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO/
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB/TOKMDLIB
    ?STEP :=   'SJR0070B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-DENHENL1,TOFILE-DENHENL1.TOKDTLIB
    CALL      PGM-SJR0070B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信エラーリスト（受領・返品）##./
SJR0220L:

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO/
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB/TOKMDLIB
    ?STEP :=   'SJR0220L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0220L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信状況リスト（受領・返品）##./
SJR0210L:

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO/
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB/TOKMDLIB
    ?STEP :=   'SJR0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0210L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品データ抽出##./
SJR0280B:

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO/
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB/TOKMDLIB
    ?STEP :=   'SJR0280B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0280B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.↓ 以下追加 2021.11.08 ./
/.##返品自動計上判定処理（通常）##./
SJR0430B:

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO/
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB/TOKMDLIB
    ?STEP :=   'SJR0430B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?JKBN  =  '1'  THEN
              OVRF  FILE-COMRHEL2,TOFILE-COMRMHL7.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHEL2,TOFILE-COMRHEL2.TOKDTLIB
    END

/.##CALL      PGM-SJR0430B.TOKSOLIB,
              PARA-(?JKBN,?BUMCD,?TANCD,?HIDUKE,?JIKAN,?HTOKCD,
                    ?NDATE) ##./    /.計上対象の入力日をOUT./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品自動計上処理（通常）##./
SJR0440B:

    ?STEP :=   'SJR0440B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?JKBN = ' '  THEN
              OVRF  FILE-COMRHELA,TOFILE-COMRHELA.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHELA,TOFILE-COMRMHLA.TOKDTLIB
    END
/.##CALL      PGM-SJR0440B.TOKSOLIB,
              PARA-(?BUMCD,?TANCD,?HIDUKE,?JIKAN,?HTOKCD,?NDATE,
                    ?UPTIME,?DENCNT)   計上時刻・件数をOUT##./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    IF        ?DENCNT    =   '0000000'    THEN
              GOTO RTN
    END

/.## 返品自動計上確認データ抽出 ##./
SJR0360C:

    ?STEP :=   'SJR0360C'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?JKBN = ' '  THEN
        OVRF FILE-COMRHEL4,TOFILE-COMRHEL4.TOKDTLIB
        OVRF FILE-COMRHELB,TOFILE-COMRHELB.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRHEL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRHEL9.TOKDTLIB
    ELSE
        OVRF FILE-COMRHEL4,TOFILE-COMRMHL4.TOKDTLIB
        OVRF FILE-COMRHELB,TOFILE-COMRMHLB.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRMHL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRMHL9.TOKDTLIB
    END
    OVRF FILE-COMWHEL1,TOFILE-AUTWHEL1.TOKDTLIB

    ?NFROM  :=  ?NDATE   /.判定処理からの計上対象入力日./
    ?NTO    :=  ?NDATE   /.判定処理からの計上対象入力日./

/.##CALL PGM-SJR0360C.TOKSOLIB,PARA-(?KEIKBN,?HTOKCD,?TANST,?TANED,
                                     ?DKBN,?AFROM,?ATO,?NFROM,?NTO,
                                     ?KFROM,?KTO,?SFROM,?STO,
                                     ?UPTIME)   計上更新日も条件##./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
        IF ?PGMEC = 4001  THEN
           GOTO  RTN
        ELSE
           GOTO  ABEND
        END
    END

/.## 返品自動計上確認リスト発行 ##./
SJR0460L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL

/.##CALL PGM-SJR0460L.TOKSOLIB,PARA-(?JKBN,?KEIKBN,?HTOKCD,?TANST,
             ?TANED,?DKBN,?AFROM,?ATO,?NFROM,?NTO,?KFROM,?KTO,
             ?SFROM,?STO)
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        GOTO  ABEND
    ELSE
        GOTO  RTN
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?JKEKA := ?KEKA
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC
/.##異常終了時##./
ABEND:

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO/
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB/TOKMDLIB
  /.## エラーメール用データ出力　セット 開始  ##./
    ?ERKBN   :=  '002'
    ?ERDATE  :=  ?HIDUKE
    ?ERTIME  :=  ?JIKAN
    ?ERTKCD  :=  ?TOKCD
    CALL ERR0010B.TOKSOLIB,PARA-(?ERKBN,?ERDATE,?ERTIME,?ERTKCD)
  /.## エラーメール用データ出力　セット 終了  ##./

    ?JKEKA    :=    ?KEKA
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC
