****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　                        *
*    サブシステム　　　　：　コーナン　ＥＤＩ　　　　　　　　　*
*    モジュール名　　　　：　ＰＯＲケース用ピッキングリスト    *
*    作成日／作成者　　　：　2021/01/18 INOUE                  *
*    処理概要　　　　　　：　抽出データよりピッキングリスト　  *
*                            を発行する。                      *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SSY1215L.
*               流用:SSY1212L
 AUTHOR.             NAV.
 DATE-WRITTEN.       2021/01/18.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*リスト発行ワーク
     SELECT   KNPKLCW1       ASSIGN        TO  01-VI-KNPKLCW1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  PKL-F001
                                               PKL-F002
                                               PKL-F003
                                               PKL-F004
                                               PKL-FA02
                                               PKL-FA07
                                               PKL-FA08
                                               PKL-FA03
                                               PKL-FA04
                                               PKL-FA05
                                               PKL-FC01
                                               PKL-FA06
                                               PKL-FB05
                             FILE STATUS   IS  PKL-STA.

*店舗Ｍ
     SELECT   TENMS1         ASSIGN        TO  01-VI-TENMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  TEN-F52  TEN-F011
                             FILE STATUS   IS  TEN-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-STA.

*商品名称Ｍ
     SELECT   MEIMS1         ASSIGN        TO  01-VI-MEIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SHO-F011
                                               SHO-F012
                             FILE STATUS   IS  TEN-STA.

*取引先Ｍ
     SELECT   TOKMS2         ASSIGN        TO  01-VI-TOKMS2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  TOK-F01
                             FILE STATUS   IS  TOK-STA.

*プリンタＦ
     SELECT   PRTF           ASSIGN      TO  GS-XU04LP
                             ORGANIZATION         IS   SEQUENTIAL
                             ACCESS MODE          IS   SEQUENTIAL
                             SYMBOLIC DESTINATION IS  "PRT"
                             PROCESSING MODE      IS   PRT-PRO
                             GROUP                IS   PRT-GRP
                             FORMAT               IS   PRT-FMT
                             SELECTED FUNCTION    IS   PRT-FNC
                             UNIT     CONTROL     IS   PRT-CTL
                             FILE STATUS          IS   PRT-STA
                             DESTINATION-1        IS   PRT-DES.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*リスト発行ワーク
 FD  KNPKLCW1.
     COPY     KNPKLCW1 OF  XFDLIB
     JOINING  PKL      AS  PREFIX.
*店舗Ｍ
 FD  TENMS1.
     COPY     HTENMS   OF  XFDLIB
     JOINING  TEN      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*商品名称Ｍ
 FD  MEIMS1.
     COPY     HMEIMS   OF  XFDLIB
     JOINING  SHO      AS  PREFIX.
*取引先Ｍ
 FD  TOKMS2.
     COPY     HTOKMS   OF  XFDLIB
     JOINING  TOK      AS  PREFIX.
*プリンタＦ
 FD  PRTF.
     COPY     FSY1215L OF  XMDLIB
     JOINING  PRT      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*プリンタＦ制御用
 01  PRT-CONTROL.
     03  PRT-PRO           PIC  X(02).
     03  PRT-GRP           PIC  X(08).
     03  PRT-FMT           PIC  X(08).
     03  PRT-DES           PIC  X(08).
     03  PRT-CTL           PIC  X(06).
     03  PRT-FNC           PIC  X(04).
*ステータス
 01  STA-AREA.
     03  PKL-STA             PIC  X(02).
     03  TEN-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  SHO-STA             PIC  X(02).
     03  TOK-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  WORK-AREA.
     03  WK-SYSDT            PIC  9(06)  VALUE  ZERO.
     03  WK-SYSDTW           PIC  9(08)  VALUE  ZERO.
     03  WK-SYSDTW-R         REDEFINES   WK-SYSDTW.
       05  WK-SYSDT-YY       PIC  9(04).
       05  WK-SYSDT-MM       PIC  9(02).
       05  WK-SYSDT-DD       PIC  9(02).
     03  FLG-TOK             PIC  9(01)  VALUE  ZERO.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  PKL-FLG             PIC  9(01)  VALUE  ZERO.
     03  PAGE-FLG            PIC  9(01)  VALUE  ZERO.
*
*    03  WK-BDATE            PIC  9(08)  VALUE  ZERO.
*    03  WK-BTIME            PIC  9(04)  VALUE  ZERO.
*    03  WK-BTORCD           PIC  9(08)  VALUE  ZERO.
     03  WK-SOKOCD           PIC  X(02)  VALUE  SPACE.
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
     03  WK-TORCD            PIC  9(08)  VALUE  ZERO.
     03  WK-ROUTE            PIC  9(02)  VALUE  ZERO.
     03  WK-BUMON            PIC  9(02)  VALUE  ZERO.
     03  WK-HACYU            PIC  9(06)  VALUE  ZERO.
*    03  WK-HACYU-R          REDEFINES   WK-HACYU.
*      05  WK-HACYU-YY       PIC  9(02).
*      05  WK-HACYU-MM       PIC  9(02).
*      05  WK-HACYU-DD       PIC  9(02).
     03  WK-HACYU-R.
       05  WK-HACYU-YY       PIC  9(04).
       05  WK-HACYU-MM       PIC  9(02).
       05  WK-HACYU-DD       PIC  9(02).
     03  WK-NOUHIN           PIC  9(06)  VALUE  ZERO.
*    03  WK-NOUHIN-R         REDEFINES   WK-NOUHIN.
*      05  WK-NOUHIN-YY      PIC  9(02).
*      05  WK-NOUHIN-MM      PIC  9(02).
*      05  WK-NOUHIN-DD      PIC  9(02).
     03  WK-NOUHIN-R.
       05  WK-NOUHIN-YY      PIC  9(04).
       05  WK-NOUHIN-MM      PIC  9(02).
       05  WK-NOUHIN-DD      PIC  9(02).
     03  WK-TANA             PIC  X(06)  VALUE  SPACE.
*
     03  WK-TENKB            PIC  X(01)  VALUE  SPACE.
*    03  WK-BUMON            PIC  X(04)  VALUE  SPACE.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
*ﾊﾞｯﾁNO.
 01  WK-BATCH.
     03  WK-JDATE            PIC  9(08)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "-".
     03  WK-JTIME            PIC  9(04)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "-".
     03  WK-TORI             PIC  9(08)  VALUE  ZERO.
*メッセージ情報
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE  " *** ".
         05  FILLER          PIC  X(07)  VALUE  " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SSY1215L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
     03  MSG-EXIT.
         05  FILLER          PIC  X(12)  VALUE  "*** SSY1215L".
         05  FILLER          PIC  X(11)  VALUE  "    END ***".
     03  MSG-INPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** KNPKLCW1 ".
         05  FILLER          PIC  X(10)  VALUE  "  INPUT = ".
         05  CNT-READ        PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
     03  MSG-PRINT.
         05  FILLER          PIC  X(13)  VALUE  "*** PRTF     ".
         05  FILLER          PIC  X(10)  VALUE  "  PAGE  = ".
         05  CNT-WRITE       PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
*
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
*=============================================================
*LINKAGE             SECTION.
*=============================================================
*01  PARA-OUTNO              PIC  9(01).
******************************************************************
 PROCEDURE               DIVISION.
******************************************************************
 DECLARATIVES.
*プリントＦ
 PRT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       PRTF.
     MOVE    "PRTF"        TO    ERR-FL-ID.
     MOVE     PRT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*リスト発行ワーク
 OUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       KNPKLCW1.
     MOVE    "KNPKLCW1"    TO    ERR-FL-ID.
     MOVE     PKL-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*店舗Ｍ
 TEN-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       TENMS1.
     MOVE    "TENMS1"      TO    ERR-FL-ID.
     MOVE     TEN-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*商品名称Ｍ
 PKL-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       MEIMS1.
     MOVE    "MEIMS1"      TO    ERR-FL-ID.
     MOVE     SHO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*取引先Ｍ
 TOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       TOKMS2.
     MOVE    "TOKMS2"      TO    ERR-FL-ID.
     MOVE     TOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SSY1215L   START  **"   UPON  CONS.
*
     MOVE     "CONTROL-SEC"       TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SSY1215L   END    **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
     MOVE    "INIT-SEC"   TO      S-NAME.
*ファイル ＯＰＥＮ
     OPEN     INPUT       KNPKLCW1.
     OPEN     INPUT       TENMS1.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       MEIMS1.
     OPEN     INPUT       TOKMS2.
     OPEN     OUTPUT      PRTF.
*システム日付取得
     ACCEPT   WK-SYSDT    FROM  DATE.
     MOVE    "3"          TO         LINK-IN-KBN.
     MOVE     WK-SYSDT    TO         LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING      LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   WK-SYSDTW
     ELSE
         MOVE    ZERO           TO   WK-SYSDTW
     END-IF.
*ＭＡＸ行設定
     MOVE     62          TO    MAX-LINE.
     MOVE     SPACE       TO    PRT-CONTROL.
*リスト発行ワークリード
     PERFORM  PKL-RD-SEC.
     IF  PKL-FLG   =  9
         MOVE  99         TO    MAIN-FLG
     END-IF.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     MOVE    "MAIN-SEC"       TO   S-NAME.
*倉庫ＣＤ ブレイク判定
     IF  PKL-F004 NOT  =  WK-SOKOCD
         MOVE     PKL-F004    TO   WK-SOKOCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*センターＣＤブレイク判定
     IF  PKL-FA02     NOT  =  WK-TENCD
         MOVE     PKL-FA02    TO   WK-TENCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*取引先ＣＤブレイク判定
     IF  PKL-FA07     NOT  =  WK-TORCD
         MOVE     PKL-FA07    TO   WK-TORCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*ルートＣＤブレイク判定
     IF  PKL-FA08     NOT  =  WK-ROUTE
         MOVE     PKL-FA08    TO   WK-ROUTE
         MOVE     1           TO   PAGE-FLG
     END-IF.
*部門ブレイク判定
     IF  PKL-FA03    NOT  =  WK-BUMON
         MOVE     PKL-FA03    TO   WK-BUMON
         MOVE     1           TO   PAGE-FLG
     END-IF.
*発注日ブレイク判定
     IF  PKL-FA04 NOT  =  WK-HACYU
         MOVE     PKL-FA04    TO   WK-HACYU
         MOVE     1           TO   PAGE-FLG
     END-IF.
*納品日ブレイク判定
     IF  PKL-FA05 NOT  =  WK-NOUHIN
         MOVE     PKL-FA05    TO   WK-NOUHIN
         MOVE     1           TO   PAGE-FLG
     END-IF.
*_番（上１桁）ブレイク判定
     IF  PKL-FC01(1:1)    NOT   =   WK-TANA(1:1)
         MOVE     PKL-FC01     TO   WK-TANA
         MOVE     1            TO   PAGE-FLG
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*リスト発行ワークリード
     PERFORM  PKL-RD-SEC.
     IF  PKL-FLG   =  9
         MOVE  99                    TO  MAIN-FLG
     END-IF.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     MOVE    "HEAD-WT-SEC"       TO  S-NAME.
     MOVE    SPACE               TO  PRT-HEADX.
*日付
     MOVE    WK-SYSDT-YY         TO  PRT-SYSYY.
     MOVE    WK-SYSDT-MM         TO  PRT-SYSMM.
     MOVE    WK-SYSDT-DD         TO  PRT-SYSDD.
*頁
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PRT-PAGE.
*ﾊﾞｯﾁNO.
     MOVE    PKL-F001            TO  WK-JDATE.
     MOVE    PKL-F002            TO  WK-JTIME.
     MOVE    PKL-F003            TO  WK-TORI.
     MOVE    WK-BATCH            TO  PRT-BATCH.
*取引先名
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    PKL-F003      TO    TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE     TO    TOK-REC
               INITIALIZE            TOK-REC
         END-READ
         MOVE    TOK-F02   TO        PRT-TORINM
         MOVE    1         TO        FLG-TOK
     ELSE
         MOVE    TOK-F02   TO        PRT-TORINM
     END-IF.
*ルート
     MOVE    PKL-FA08            TO  PRT-ROUTE.
*発注日
     MOVE    "3"          TO         LINK-IN-KBN.
     MOVE     WK-HACYU    TO         LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING      LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   WK-HACYU-R
     ELSE
         MOVE    ZERO           TO   WK-HACYU-R
     END-IF.
     MOVE    WK-HACYU-YY         TO  PRT-HACYY.
     MOVE    WK-HACYU-MM         TO  PRT-HACMM.
     MOVE    WK-HACYU-DD         TO  PRT-HACDD.
*納品日
     MOVE    "3"          TO         LINK-IN-KBN.
     MOVE     WK-NOUHIN   TO         LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING      LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   WK-NOUHIN-R
     ELSE
         MOVE    ZERO           TO   WK-NOUHIN-R
     END-IF.
     MOVE    WK-NOUHIN-YY        TO  PRT-NOUYY.
     MOVE    WK-NOUHIN-MM        TO  PRT-NOUMM.
     MOVE    WK-NOUHIN-DD        TO  PRT-NOUDD.
*倉庫名
     MOVE  WK-SOKOCD             TO  SOK-F01.
     READ  ZSOKMS1
           INVALID  KEY
              MOVE  SPACE        TO  PRT-SOKONM
           NOT INVALID  KEY
              MOVE  SOK-F02      TO  PRT-SOKONM
     END-READ.
*部門
     MOVE  PKL-FA03               TO  PRT-BUMON.
*センター
     MOVE  PKL-F003               TO  TEN-F52.
     MOVE  PKL-FA02               TO  PRT-TENCD  TEN-F011.
     READ  TENMS1
           INVALID  KEY
                 MOVE  SPACE      TO  PRT-TENNM
           NOT INVALID  KEY
                 MOVE  TEN-F02    TO  PRT-TENNM
     END-READ.
*_番（１：１）の設定
     MOVE  WK-TANA(1:1)         TO   PRT-TANA1.
*箱数記入欄
*    IF  ( PAGE-CNT   =  0 ) OR
*        ( PAGE-FLG   =  1 )
*          MOVE  SPACE          TO   PRT-HAKOSU
*    ELSE
*          MOVE  ALL NC"＊"     TO   PRT-HAKOSU
*    END-IF.
*印刷
     MOVE "FSY1215L"             TO  PRT-FMT.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "A000"                 TO  PRT-CTL.
     MOVE "HEAD"                 TO  PRT-GRP.
     WRITE PRT-FSY1215L.
*
     MOVE  9                     TO  LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
     MOVE    "BODY-WT-SEC"     TO  S-NAME.
*改ページ
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-CNT   =  ZERO
     OR  PAGE-FLG   =  1
         PERFORM  HEAD-WT-SEC
         MOVE     ZERO         TO  PAGE-FLG
     END-IF.
*明細部のクリア
     MOVE  SPACE               TO  PRT-BODYX.
*_番
     MOVE  PKL-FC01(2:3)       TO  PRT-TANA2.
     MOVE  PKL-FC01(5:2)       TO  PRT-TANA3.
*商品名
     MOVE  PKL-FC02            TO  SHO-F011.
     MOVE  PKL-FC03            TO  SHO-F012.
     READ  MEIMS1
           INVALID
              MOVE  ALL NC"＊" TO  PRT-SHONM
*             MOVE  SPACE      TO  PRT-SHONM
*             MOVE  PKL-F081   TO  PRT-KANANM(1:15)
*             MOVE  PKL-F082   TO  PRT-KANANM(16:15)
           NOT INVALID  KEY
              MOVE  SHO-F02    TO  PRT-SHONM
*             MOVE  SPACE      TO  PRT-KANANM
     END-READ.
*
*数量
     MOVE  PKL-FB08            TO  PRT-SURYO.
*ケース数
     MOVE  PKL-FB07            TO  PRT-CASE.
*訂正数量
*    IF    PKL-FB08  NOT  =  PKL-F10
*          MOVE  PKL-F10       TO  PRT-TSURYO
*    ELSE
*          MOVE  ZERO          TO  PRT-TSURYO
*    END-IF.
*売単価
     MOVE  PKL-FB10            TO  PRT-URITAN.
*
*引当済み印
     IF    PKL-FC12    =   "1"
           MOVE   SPACE        TO  PRT-HOSHI
     ELSE
           MOVE   NC"★"       TO  PRT-HOSHI
     END-IF.
*伝票_
     MOVE  PKL-FA06            TO  PRT-DENNO.
*ＪＡＮＣＤ
     MOVE  PKL-FB11            TO  PRT-JANCD.
*
*印刷
     MOVE "FSY1215L"             TO  PRT-FMT.
     MOVE "A001"                 TO  PRT-CTL.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "BODY"                 TO  PRT-GRP.
     WRITE PRT-FSY1215L.
     ADD   2                     TO  LINE-CNT.
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                リスト発行ワークリード処理
*=============================================================
 PKL-RD-SEC      SECTION.
     MOVE    "PKL-RD-SEC"  TO  S-NAME.
*リード
     READ  KNPKLCW1      NEXT
         AT   END
           MOVE   9        TO  PKL-FLG
           GO              TO  PKL-RD-EXIT
     END-READ.
     ADD   1               TO         CNT-READ.
 PKL-RD-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
     MOVE      "END-SEC"     TO     S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE      PRTF.
     CLOSE      KNPKLCW1.
     CLOSE      TENMS1.
     CLOSE      ZSOKMS1.
     CLOSE      MEIMS1.
     CLOSE      TOKMS2.
*ＭＳＧ出力
     DISPLAY    MSG-INPUT    UPON   CONS.
     MOVE       PAGE-CNT     TO     CNT-WRITE.
     DISPLAY    MSG-PRINT    UPON   CONS.
 END-EXIT.
     EXIT.
