# ZHA0125B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZHA0125B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　在庫管理システム                      *
*    プログラム　　名：　発注残リスト　出力                    *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　1993.05.13      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         ZHA0125B.
 AUTHOR.             NAV.
 DATE-WRITTEN.       93.05.13.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*抽出Ｆ
     SELECT   ZOUTF          ASSIGN        TO  01-S-ZOUTF
                             ORGANIZATION  IS  SEQUENTIAL
                             ACCESS MODE   IS  SEQUENTIAL
                             FILE STATUS   IS  OUT-STA.

*仕入先Ｍ
     SELECT   ZSHIMS1        ASSIGN        TO  01-VI-ZSHIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SHI-F01
                             FILE STATUS   IS  SHI-STA.

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
                             RECORD KEY    IS  MEI-F011
                                               MEI-F012
                             FILE STATUS   IS  SHI-STA.

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
*抽出Ｆ
 FD  ZOUTF
                     BLOCK  CONTAINS  5    RECORDS.
     COPY   ZHACHDT.
*仕入先Ｍ
 FD  ZSHIMS1.
     COPY     ZSHIMS   OF  XFDLIB
     JOINING  SHI      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*商品名称Ｍ
 FD  MEIMS1.
     COPY     HMEIMS   OF  XFDLIB
     JOINING  MEI      AS  PREFIX.
*プリンタＦ
 FD  PRTF.
     COPY     ZHA0125X OF  XMDLIB
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
     03  OUT-STA             PIC  X(02).
     03  SHI-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  MEI-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  WORK-AREA.
     03  SIR-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-SYSDT            PIC  9(06)  VALUE  ZERO.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  HAC-FLG             PIC  9(01)  VALUE  ZERO.
     03  SOKO-FLG            PIC  9(01)  VALUE  ZERO.
     03  DEN-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-SIRCD            PIC  X(08)  VALUE  SPACE.
     03  WK-SOKOCD           PIC  X(02)  VALUE  SPACE.
     03  WK-DENNO            PIC  9(07)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### ZHA0125B".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
     03  MSG-EXIT.
         05  FILLER          PIC  X(12)  VALUE  "*** ZHA0125B".
         05  FILLER          PIC  X(11)  VALUE  "    END ***".
     03  MSG-INPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** ZHACHDT1 ".
         05  FILLER          PIC  X(10)  VALUE  "  INPUT = ".
         05  CNT-READ        PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
     03  MSG-PRINT.
         05  FILLER          PIC  X(13)  VALUE  "*** PRTF     ".
         05  FILLER          PIC  X(10)  VALUE  " PAGE   = ".
         05  CNT-WRITE       PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-WS               PIC  X(08).
******************************************************************
 PROCEDURE               DIVISION      USING    LINK-WS.
******************************************************************
 DECLARATIVES.
*プリントＦ
 PRT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       PRTF.
     MOVE    "PRTF"        TO    ERR-FL-ID.
     MOVE     PRT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*抽出Ｆ
 OUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZOUTF.
     MOVE    "ZOUTF"       TO    ERR-FL-ID.
     MOVE     OUT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*仕入先Ｍ
 SHI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSHIMS1.
     MOVE    "ZSHIMS1"     TO    ERR-FL-ID.
     MOVE     SHI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*商品名称Ｍ
 MEI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       MEIMS1.
     MOVE    "MEIMS1"      TO    ERR-FL-ID.
     MOVE     MEI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  ZHA0125B   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  ZHA0125B     END  **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     INPUT       ZOUTF.
     OPEN     INPUT       ZSHIMS1.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       MEIMS1.
     OPEN     OUTPUT      PRTF.
*システム日付取得
     ACCEPT   WK-SYSDT    FROM  DATE.
*ＭＡＸ行設定
     MOVE     62          TO    MAX-LINE.
     MOVE     SPACE       TO    PRT-CONTROL.
*抽出Ｆリード
     PERFORM  OUT-RD-SEC.
     IF  HAC-FLG   =  9
         MOVE  99         TO    MAIN-FLG
     END-IF.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*仕入先ＣＤ ブレイク判定
*93.10.16 STRAT
     IF  HAC-F08  NOT  =  WK-SIRCD
         MOVE     HAC-F08     TO   WK-SIRCD
         MOVE     HAC-F31     TO   WK-SOKOCD
         MOVE     HAC-F02     TO   WK-DENNO
*********PERFORM  HEAD-WT-SEC
         PERFORM  SPC-WT-SEC
         MOVE     1           TO   SOKO-FLG
         MOVE     1           TO   DEN-FLG
     END-IF.
*93.10.16 END
*倉庫ＣＤ ブレイク判定
     IF  HAC-F31  NOT  =  WK-SOKOCD
         MOVE     HAC-F31     TO   WK-SOKOCD
         MOVE     HAC-F02     TO   WK-DENNO
         MOVE     1           TO   SOKO-FLG
         MOVE     1           TO   DEN-FLG
     END-IF.
*伝票_ ブレイク判定
     IF  HAC-F02     NOT  =  WK-DENNO
         MOVE     HAC-F02     TO   WK-DENNO
         MOVE     1           TO   DEN-FLG
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*抽出Ｆリード
     PERFORM  OUT-RD-SEC.
     IF  HAC-FLG   =  9
         MOVE  99                    TO  MAIN-FLG
     END-IF.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     MOVE  SPACE       TO  PRT-HEADX.
*頁
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PRT-PAGE.
*日付
     MOVE    WK-SYSDT            TO  PRT-SYSDT.
*仕入先の設定
*93.10.16 START
*
*****MOVE  WK-SIRCD              TO  PRT-SIRCD.
*****MOVE  WK-SIRCD              TO  SHI-F01.
*****READ  ZSHIMS1
*****      INVALID  KEY
*****         MOVE  SPACE                 TO  PRT-SIRNM
*****      NOT INVALID  KEY
*****         MOVE  SHI-F02               TO  PRT-SIRNM
*****END-READ.
*印刷
     MOVE "ZHA0125X"             TO  PRT-FMT.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "A000"                 TO  PRT-CTL.
     MOVE "HEAD"                 TO  PRT-GRP.
     WRITE PRT-ZHA0125X.
*
     MOVE  8                     TO  LINE-CNT.
     MOVE  1                     TO  SIR-FLG.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 SPC-WT-SEC                 SECTION.
*改ページ
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-CNT   =  ZERO
         PERFORM  HEAD-WT-SEC
         MOVE     1           TO   SOKO-FLG
         MOVE     1           TO   DEN-FLG
     END-IF.
*明細部のクリア
     MOVE  SPACE       TO  PRT-BODYX.
*印刷
     MOVE "ZHA0125X"             TO  PRT-FMT.
     MOVE "A001"                 TO  PRT-CTL.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "BODY"                 TO  PRT-GRP.
     WRITE PRT-ZHA0125X.
     ADD   2                     TO  LINE-CNT.
     MOVE  1                     TO  SIR-FLG.
 SPC-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
*改ページ
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-CNT   =  ZERO
         PERFORM  HEAD-WT-SEC
         MOVE     1           TO   SOKO-FLG
         MOVE     1           TO   DEN-FLG
     END-IF.
*明細部のクリア
     MOVE  SPACE       TO  PRT-BODYX.
*仕入先
     IF    SIR-FLG               =   1
           MOVE  HAC-F08        TO   PRT-BSIRCD
           MOVE  HAC-F08        TO   SHI-F01
           READ  ZSHIMS1
                 INVALID  KEY
                    MOVE  SPACE      TO  PRT-BSIRNM
                 NOT INVALID  KEY
                    MOVE  SHI-F02    TO  PRT-BSIRNM
           END-READ
           MOVE  ZERO       TO       SIR-FLG
     END-IF.
*
*倉庫ＣＤ
     IF    SOKO-FLG    =   1
       MOVE  HAC-F31     TO  PRT-SOKOCD
*倉庫名
       MOVE  HAC-F31     TO  SOK-F01
       READ  ZSOKMS1
             INVALID  KEY
                MOVE  SPACE       TO  PRT-SOKONM
             NOT INVALID  KEY
                MOVE  SOK-F02     TO  PRT-SOKONM
       END-READ
       MOVE  ZERO        TO  SOKO-FLG
     END-IF.
*伝票_
     IF    DEN-FLG    =   1
       MOVE  HAC-F02     TO  PRT-DENNO
       MOVE  ZERO        TO  DEN-FLG
     END-IF.
*行_
     MOVE  HAC-F05           TO  PRT-GYONO.
*入荷予定日
     MOVE  HAC-F13           TO  PRT-NYUDT.
*商品ＣＤ
     MOVE  HAC-F20           TO  PRT-SHOCD.
*商品名
     MOVE  HAC-F20           TO  MEI-F011.
     MOVE  HAC-F21           TO  MEI-F012.
     READ  MEIMS1
           INVALID  KEY
              MOVE  SPACE                 TO  PRT-SHONM
           NOT INVALID  KEY
              MOVE  MEI-F021              TO  WK-SHONM1
              MOVE  MEI-F022              TO  WK-SHONM2
              MOVE  WK-SHONM              TO  PRT-SHONM
     END-READ.
*_番
     IF    HAC-F30  NOT = SPACE
       MOVE  HAC-F30(1:1)      TO  PRT-TANABN(1:1)
       MOVE  "-"               TO  PRT-TANABN(2:1)
       MOVE  HAC-F30(2:3)      TO  PRT-TANABN(3:3)
       MOVE  "-"               TO  PRT-TANABN(6:1)
       MOVE  HAC-F30(5:2)      TO  PRT-TANABN(7:2)
     END-IF.
*発注数
     MOVE  HAC-F22           TO  PRT-HACSU.
*入荷数
     MOVE  HAC-F23           TO  PRT-NYUSU.
*発注残
     COMPUTE  PRT-ZANSU  =   HAC-F22   -  HAC-F23.
*備考
     MOVE  HAC-F29           TO  PRT-BIKOU.
*印刷
     MOVE "ZHA0125X"             TO  PRT-FMT.
     MOVE "A001"                 TO  PRT-CTL.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "BODY"                 TO  PRT-GRP.
     WRITE PRT-ZHA0125X.
     ADD   2                     TO  LINE-CNT.
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                抽出Ｆリード処理
*=============================================================
 OUT-RD-SEC      SECTION.
*リード
     READ  ZOUTF      NEXT
         AT   END
           MOVE   9        TO  HAC-FLG
           GO              TO  OUT-RD-EXIT
     END-READ.
*カウント
     ADD   1               TO         CNT-READ.
 OUT-RD-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      PRTF.
     CLOSE      ZOUTF.
     CLOSE      ZSHIMS1.
     CLOSE      ZSOKMS1.
     CLOSE      MEIMS1.
*ＭＳＧ出力
     DISPLAY    MSG-INPUT    UPON   CONS.
     MOVE       PAGE-CNT     TO     CNT-WRITE.
     DISPLAY    MSG-PRINT    UPON   CONS.
 END-EXIT.
     EXIT.

```
