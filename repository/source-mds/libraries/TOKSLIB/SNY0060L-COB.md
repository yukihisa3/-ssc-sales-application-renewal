# SNY0060L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SNY0060L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　仕入管理システム                      *
*    プログラム　　名：　入荷実績確定リスト　出力              *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　2000.05.01      UPDATE: 2012.10.12    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SNY0060L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       00.05.01.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YA-21     IS   YA-21
         YB        IS   YB
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*抽出Ｆ
     SELECT   NYJISEKF       ASSIGN        TO  01-S-NYJISEKF
                             ORGANIZATION  IS  SEQUENTIAL
                             ACCESS MODE   IS  SEQUENTIAL
                             FILE STATUS   IS  NYJ-STA.

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
*条件ファイル
     SELECT   JYOKEN1        ASSIGN        TO  01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.

*プリンタＦ
     SELECT   PRINTF    ASSIGN  TO   LP-04.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*抽出Ｆ
 FD  NYJISEKF
                     BLOCK  CONTAINS  6    RECORDS.
     COPY     NYJISEKF OF  XFDLIB
     JOINING  NYJ      AS  PREFIX.
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
*条件ファイル
 FD  JYOKEN1.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*プリンタＦ
 FD  PRINTF.
 01    P-REC                 PIC  X(200).
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  NYJ-STA             PIC  X(02).
     03  SHI-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  MEI-STA             PIC  X(02).
     03  JYO-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  CNT-AREA.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER              PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN          PIC  N(06)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"）".
 01  WORK-AREA.
     03  WK-SYSDT            PIC  9(06)  VALUE  ZERO.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  HAC-FLG             PIC  9(01)  VALUE  ZERO.
     03  SIR-FLG             PIC  9(01)  VALUE  ZERO.
     03  DEN-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-SIRCD            PIC  X(08)  VALUE  SPACE.
     03  WK-SOKOCD           PIC  X(02)  VALUE  SPACE.
     03  WK-DENNO            PIC  9(07)  VALUE  ZERO.
     03  WK-EDA              PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
     03  WK-MEMO             PIC  X(07)  VALUE  SPACE.
****  見出し行０             ****
 01  MIDASI0        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SNY0060L".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(23)  VALUE
         NC"※※　入　荷　実　績　確　定　リ　ス　ト　※※".
     02  H-TOKHAN            PIC  N(08)  VALUE  SPACE.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZZ9.
****  見出し行１             ****
 01  MIDASI1        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"倉庫：".
     02  H1-SOKCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-SOKNM            PIC  N(18).
****  見出し行２             ****
 01  MIDASI2        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"仕入先".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"仕入先名".
     02  FILLER              PIC  X(31)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"備　考".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"メ　モ".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"社外ＭＳＧ".
****  見出し行３             ****
 01  MIDASI3        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(10)  VALUE SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注_".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"枝".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"行".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"入荷予定日".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"商品".
     02  FILLER              PIC  X(05)  VALUE  "ｺｰﾄﾞ".
     02  FILLER              PIC  N(03)  VALUE
         NC"_　番".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"商品名".
     02  FILLER              PIC  X(41)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"入荷予定数".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"納入日".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"入庫数".
*2012/10/12↓
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"連携".
*2012/10/12↑
*
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
*2012/10/12↓
***  02  FILLER              PIC  N(67)  VALUE  ALL NC"─".
     02  FILLER              PIC  N(69)  VALUE  ALL NC"─".
*2012/10/12↑
****  明細行１               ****
 01  MEISAI1        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT01               PIC  X(08)B.
     02  PRT02               PIC  N(18)B(08).
     02  PRT03               PIC  X(10)B(02).
     02  PRT04               PIC  X(07)B(20).
     02  PRT05               PIC  X(02)B.
     02  PRT06               PIC  N(20).
****  明細行２               ****
 01  MEISAI2        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  PRT11               PIC  9(07)B.
     02  PRT12               PIC  9(02)BB.
     02  PRT13               PIC  9(02)BB.
     02  PRT14               PIC  X(08)BB.
     02  PRT15               PIC  X(08)B.
     02  PRT16               PIC  X(08)B.
     02  PRT171              PIC  N(15).
     02  PRT172              PIC  N(15).
     02  PRT18               PIC  Z,ZZZ,ZZ9.99BB.
     02  PRT19               PIC  X(08).
     02  PRT20               PIC  Z,ZZZ,ZZ9.99.
*2012/10/12↓
     02  FILLER              PIC  X(03) VALUE  SPACE.
     02  PRT21               PIC  N(01).
*2012/10/12↑
*
****  明細行３               ****
 01  MEISAI3.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
*2012/10/12↓
***  02  FILLER              PIC  X(134) VALUE  ALL "-".
     02  FILLER              PIC  X(136) VALUE  ALL "-".
*2012/10/12↑
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SNY0060L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 PROCEDURE               DIVISION.
******************************************************************
 DECLARATIVES.
*抽出Ｆ
 OUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       NYJISEKF.
     MOVE    "NYJISEKF"    TO    ERR-FL-ID.
     MOVE     NYJ-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*仕入先Ｍ
 SHI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSHIMS1.
     MOVE    "ZSHIMS1"     TO    ERR-FL-ID.
     MOVE     SHI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*商品名称Ｍ
 MEI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       MEIMS1.
     MOVE    "MEIMS1"      TO    ERR-FL-ID.
     MOVE     MEI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*条件ファイル
 JYO-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JYOKEN1.
     MOVE    "JYOKEN1"     TO    ERR-FL-ID.
     MOVE     JYO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SNY0060L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SNY0060L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     INPUT       NYJISEKF.
     OPEN     INPUT       ZSHIMS1.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       MEIMS1.
     OPEN     INPUT       JYOKEN1.
     OPEN     OUTPUT      PRINTF.
*システム日付取得
     ACCEPT   WK-SYSDT    FROM  DATE.
*ＭＡＸ行設定
     MOVE     62          TO    MAX-LINE.
*システム日付・時刻の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-SYSDT            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD(1:4)  TO   H-YY.
     MOVE      LINK-OUT-YMD(5:2)  TO   H-MM.
     MOVE      LINK-OUT-YMD(7:2)  TO   H-DD.
*特販部名称編集
     MOVE      SPACE              TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE     "99"                TO   JYO-F01.
     MOVE     "BUMON"             TO   JYO-F02.
     READ      JYOKEN1
       INVALID KEY
               MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
       NOT INVALID KEY
               MOVE JYO-F03            TO   HEN-TOKHAN
     END-READ.
     MOVE      HEN-TOKHAN-AREA    TO   H-TOKHAN.
*抽出Ｆリード
     PERFORM  NYJ-RD-SEC.
     IF  HAC-FLG   =  9
         MOVE  99         TO    MAIN-FLG
     END-IF.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*改ページ
*倉庫ＣＤ ブレイク判定
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-CNT   =  ZERO
     OR  NYJ-F01  NOT  =  H1-SOKCD
         MOVE     NYJ-F01     TO   WK-SOKOCD
         MOVE     NYJ-F02     TO   WK-SIRCD
         MOVE     NYJ-F03     TO   WK-DENNO
         MOVE     NYJ-F05     TO   WK-EDA
         PERFORM  HEAD-WT-SEC
         MOVE     1           TO   SIR-FLG
         MOVE     1           TO   DEN-FLG
     END-IF.
*仕入先ＣＤ ブレイク判定
     IF  NYJ-F02  NOT  =  WK-SIRCD
         MOVE     NYJ-F02     TO   WK-SIRCD
         MOVE     NYJ-F03     TO   WK-DENNO
         MOVE     NYJ-F05     TO   WK-EDA
         MOVE     1           TO   SIR-FLG
         MOVE     1           TO   DEN-FLG
     END-IF.
*伝票_ ブレイク判定
     IF  NYJ-F03     NOT  =  WK-DENNO
     OR  NYJ-F05     NOT  =  WK-EDA
         MOVE     NYJ-F03     TO   WK-DENNO
         MOVE     NYJ-F05     TO   WK-EDA
         MOVE     1           TO   DEN-FLG
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*抽出Ｆリード
     PERFORM  NYJ-RD-SEC.
     IF  HAC-FLG   =  9
     OR  NYJ-F03  NOT =  WK-DENNO
     OR  NYJ-F05  NOT =  WK-EDA
           WRITE    P-REC   FROM    MEISAI3    AFTER  1
           ADD      1               TO  LINE-CNT
     END-IF.
     IF  HAC-FLG   =  9
         MOVE  99                    TO  MAIN-FLG
     END-IF.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PAGE-SUU.
*倉庫
     MOVE  WK-SOKOCD             TO  H1-SOKCD.
     MOVE  WK-SOKOCD             TO  SOK-F01.
     READ  ZSOKMS1
           INVALID  KEY
              MOVE  SPACE            TO  H1-SOKNM
           NOT INVALID  KEY
              MOVE  SOK-F02          TO  H1-SOKNM
     END-READ.
*印刷
     IF      PAGE-CNT    NOT =   1
             MOVE     SPACE   TO      P-REC
             WRITE    P-REC   AFTER   PAGE
     END-IF.
     WRITE   P-REC     FROM    MIDASI0     AFTER  2.
     WRITE   P-REC     FROM    MIDASI1     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
     WRITE   P-REC     FROM    MIDASI2     AFTER  1.
     WRITE   P-REC     FROM    MIDASI3     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
*
     MOVE  6                     TO  LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
*明細部のクリア
     MOVE  SPACE         TO  MEISAI1  MEISAI2.
*
     IF    SIR-FLG    =   1
      OR   DEN-FLG    =   1
*仕入先
       MOVE  NYJ-F02     TO  PRT01
       MOVE  NYJ-F02     TO  SHI-F01
       READ  ZSHIMS1
             INVALID  KEY
                MOVE  SPACE       TO  PRT02
             NOT INVALID  KEY
                MOVE  SHI-F02     TO  PRT02
       END-READ
*備考
       MOVE  NYJ-F13         TO  PRT03
*メモ
       MOVE  NYJ-F14         TO  PRT04
*社外ＭＳＧ
       MOVE  NYJ-F151        TO  PRT05
       MOVE  NYJ-F152        TO  PRT06
*伝票ＮＯ
       MOVE  NYJ-F03         TO  PRT11
       MOVE  NYJ-F05         TO  PRT12
*入荷予定日
       MOVE  NYJ-F06(3:2)    TO  PRT14(1:2)
       MOVE  "/"             TO  PRT14(3:1)
       MOVE  NYJ-F06(5:2)    TO  PRT14(4:2)
       MOVE  "/"             TO  PRT14(6:1)
       MOVE  NYJ-F06(7:2)    TO  PRT14(7:2)
     END-IF.
*行_
     MOVE  NYJ-F04           TO  PRT13.
*商品ＣＤ
     MOVE  NYJ-F08           TO  PRT15.
*商品名
     MOVE  NYJ-F08           TO  MEI-F011.
     MOVE  NYJ-F09           TO  MEI-F012.
     READ  MEIMS1
           INVALID  KEY
              MOVE  SPACE        TO  PRT171   PRT172
           NOT INVALID  KEY
              MOVE  MEI-F021     TO  PRT171
              MOVE  MEI-F022     TO  PRT172
     END-READ.
*_番
     IF    NYJ-F10  NOT = SPACE
       MOVE  NYJ-F10(1:1)        TO  PRT16(1:1)
       MOVE  "-"                 TO  PRT16(2:1)
       MOVE  NYJ-F10(2:3)        TO  PRT16(3:3)
       MOVE  "-"                 TO  PRT16(6:1)
       MOVE  NYJ-F10(5:2)        TO  PRT16(7:2)
     END-IF.
*入荷予定数
     MOVE  NYJ-F11           TO  PRT18.
*納入数
     MOVE  NYJ-F07(3:2)      TO  PRT19(1:2).
     MOVE  "/"               TO  PRT19(3:1).
     MOVE  NYJ-F07(5:2)      TO  PRT19(4:2).
     MOVE  "/"               TO  PRT19(6:1).
     MOVE  NYJ-F07(7:2)      TO  PRT19(7:2).
*入庫数
     MOVE  NYJ-F12           TO  PRT20.
*2012/10/12↓
*物流未連携
     IF    NYJ-F16  =  "1"
           MOVE  SPACE       TO  PRT21
     ELSE
           MOVE  NC"未"      TO  PRT21
     END-IF.
*2012/10/12↑
*印刷
     IF    SIR-FLG  =   1
     OR    DEN-FLG  =   1
           WRITE    P-REC   FROM    MEISAI1    AFTER  1
           ADD      1               TO  LINE-CNT
     END-IF.
     WRITE    P-REC         FROM    MEISAI2    AFTER  1.
     ADD   1                     TO  LINE-CNT.
*
     MOVE  ZERO                  TO  SIR-FLG.
     MOVE  ZERO                  TO  DEN-FLG.
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                抽出Ｆリード処理
*=============================================================
 NYJ-RD-SEC      SECTION.
*リード
     READ  NYJISEKF      NEXT
         AT   END
           MOVE   9        TO  HAC-FLG
           GO              TO  NYJ-RD-EXIT
     END-READ.
*カウント
     ADD   1               TO         READ-CNT.
 NYJ-RD-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      PRINTF.
     CLOSE      NYJISEKF.
     CLOSE      ZSHIMS1.
     CLOSE      ZSOKMS1.
     CLOSE      MEIMS1.
     CLOSE      JYOKEN1.
*ＭＳＧ出力
     DISPLAY "* NYJISEKF(IN)=" READ-CNT " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
 END-EXIT.
     EXIT.

```
